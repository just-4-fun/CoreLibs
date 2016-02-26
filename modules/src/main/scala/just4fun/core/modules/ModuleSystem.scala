package just4fun.core.modules

import java.util.concurrent.locks.ReentrantLock
import scala.collection.mutable
import just4fun.core.async.{AsyncContextKey, DefaultAsyncContext, AsyncContext, AsyncContextOwner}
import just4fun.core.debug.DebugUtils._

object ModuleSystem {
	private[modules] val STOPPED = 0
	private[modules] val STARTED = 1
	private[modules] val STOPPING = 2

	private[this] val systems = new mutable.WeakHashMap[ModuleSystem, Unit]()
	private[modules] var initialized: Boolean = false
	private[modules] val lock = new Object
	private[modules] var currentSystem: ModuleSystem = null

	//TODO are those methods needed anymore ?
	def default: Option[ModuleSystem] = byId(defaultIdentifier)
	def defaultIdentifier: String = "_default$ystem_"
	def byId(id: String): Option[ModuleSystem] = id match {
		case null ⇒ None
		case _ ⇒ systems.collectFirst { case (s, _) if s.systemId == id ⇒ s }
	}
	private[modules] def init(s: ModuleSystem): Unit = {
		initialized = true
		systems.put(s, ())
	}

	private[modules] def attach(m: Module): ModuleSystem = lock.synchronized {
		if (currentSystem != null) currentSystem.attach(m)
		currentSystem
	}
}


trait ModuleSystem extends AsyncContextOwner {
//todo private[this] val _dc = disableDebugCode()
	import ModuleSystem.{STOPPED, STARTED, STOPPING}
	val systemId: String = getClass.getName
	protected[this] implicit final val thisSystem: this.type = this
	implicit val asyncContext: AsyncContext = new DefaultAsyncContext
	protected[this] val internal: InternalUtils = new InternalUtils
	protected[this] val restoreAgent: ModuleRestoreAgent = null
	private[this] val modules = mutable.ListBuffer[Module]()
	private[this] val detached = mutable.ListBuffer[Module]()
	private[this] val root: SystemModule = new SystemModule(this)
	private[this] val lock = new Object
	private[this] var state = STOPPED
	private[this] var currentArgs: List[ConstructArgs] = Nil
	//
	ModuleSystem.init(this)


	/* system api */
	// todo final def contentSize: Int
// todo	final def isEmpty: Boolean
	final def isSystemStarted: Boolean = lock.synchronized (state > STOPPED)
	final def isSystemStopping: Boolean = lock.synchronized (state == STOPPING)
	final def isSystemStopped: Boolean = lock.synchronized (state == STOPPED)
	/* callbacks */
	protected[this] def onSystemStart(): Unit = ()
	protected[this] def onSystemStop(): Unit = ()
	/* internal */
	private[this] def startSystem(): Unit = {
		state = STARTED
		logV(s"*************  SYSTEM START  **************")
		onSystemStart()
	}
	private[this] def canStop: Boolean = modules.isEmpty && detached.isEmpty
	private[this] def stopSystem(): Unit = lock.synchronized {
		if (canStop && isSystemStarted) {
			asyncContext.stop(true)
			onSystemStop()
			asyncContext.stop(true)
			logV(s"*************  SYSTEM  STOP  **************")
			state = STOPPED
		}
	}
	private[modules] def forceStopSystem(): Unit = lock.synchronized {
		if (isSystemStarted) {
			logV(s"**  SYSTEM FORCED STOP  **")
			val err = new Exception("System is forced to stop.")
			modules.foreach(m ⇒ m.fail(err, false))
			modules.foreach(m ⇒ unbindModule(m.getClass))
			modules.foreach { server ⇒
				val others = modules.filterNot(_ == server)
				others.foreach(client ⇒ unbind(server.getClass, client))
			}
		}
	}

	/* module api */
	final def moduleContent: Seq[Class[_]] = {
		modules.map(_.getClass)
	}

	final def hasModule[M <: Module : Manifest]: Boolean = {
		hasModule(moduleClass)// TODO macros
	}
	final def hasModule[M <: Module](cls: Class[M]): Boolean = {
		find(cls).nonEmpty
	}

	final def bindModule[M <: Module : Manifest]: Unit = {// TODO macros
		bind(moduleClass, null, false, false, null)
	}
	final def bindModule[M <: Module : Manifest](constructor: () ⇒ M): Unit = {
		bind(moduleClass, null, false, false, constructor)
	}
	final def bindModule[M <: Module](clas: Class[M]): Unit = {
		bind(clas, null, false, false, null)
	}
	final def bindModule[M <: Module](clas: Class[M], constructor: () ⇒ M): Unit = {
		bind(clas, null, false, false, constructor)
	}
	final def unbindModule[M <: Module : Manifest]: Unit = {
		unbind(moduleClass, null)// TODO macros
	}
	final def unbindModule[M <: Module](clas: Class[M]): Unit = {
		unbind(clas, null)
	}


	/* callbacks */
	protected[this] def onModulePrepare(promise: ModulePreparePromise): Unit = promise.complete()
	//	protected[this] def onModuleDetached(m: Module): Unit = ()
	protected[this] def onModuleDestroy(m: Module): Unit = ()


	/* module bind */
	private[modules] def bind[M <: Module](serverClas: Class[M], clientModule: Module, sync: Boolean, restore: Boolean, constr: () ⇒ M): M = lock.synchronized {
		val client = if (clientModule == null) root else clientModule
		val isRoot = client == root
		if (!isRoot && !modules.contains(client)) throw new ModuleBindException(serverClas, "It's not registered in system.")
		if (state == STOPPING && isRoot) state = STARTED
		find(serverClas) match {
			case Some(server) ⇒ server.bindingAdd(client, sync, isRoot); server
			case None ⇒ if (isSystemStopped) startSystem()
				args = new ConstructArgs(client, sync, restore, isRoot)
				try {
					construct {
						val m = if (constr == null) serverClas.newInstance() else constr()
						if (m == null) serverClas.newInstance()
					}
					val server = args.server.asInstanceOf[M]
					if (isRoot && restoreAgent != null && server.isInstanceOf[RestorableModule]) restoreAgent.set(server, true)
					server.setConstructed()
					server
				}
				finally args = null
		}
	}
	private[modules] def construct(constructModule: ⇒ Unit): Unit = ModuleSystem.lock.synchronized {
		ModuleSystem.currentSystem = this
		try constructModule catch {
			case e: Throwable ⇒ args.server match {
				case null ⇒ throw e
				case s ⇒ s.fail(e, false)
			}
		}
		finally ModuleSystem.currentSystem = null
	}
	private[modules] def attach(m: Module): Unit = {
		val a = args
		a.server = m
		modules += m
		if (a.restoring) m.setRestored()
	}
	private[modules] def constructed(m: Module): Unit = {
		val a = args
		m.bindingAdd(a.client, a.sync, a.isRoot)
	}
	private[this] def args: ConstructArgs = currentArgs.head
	private[this] def args_=(a: ConstructArgs): Unit = a match {
		case null => currentArgs = currentArgs.tail
		case _ => currentArgs = a :: currentArgs
	}

	private[modules] def prepare(m: Module): Unit = {
		if (m.status.constructed && !m.status.prepared && !detached.exists(isPredecessor(_, m)))
			try onModulePrepare(new ModulePreparePromise(m)) catch {case e: Throwable ⇒ m.fail(e, false)}
	}

	/* module unbind */
	private[modules] def unbind[M <: Module](serverClas: Class[M], clientModule: Module): Option[M] = lock.synchronized {
		val client = if (clientModule == null) root else clientModule
		val isRoot = client == root
		find(serverClas) match {
			case mOp@Some(m) ⇒ m.bindingRemove(client, isRoot) match {
				case true ⇒ if (isRoot && !modules.exists(_.isBoundToSystem)) state = STOPPING; mOp
				case _ ⇒ None
			}
			case _ ⇒ None
		}
	}
	private[modules] def detach(module: Module): Boolean = lock.synchronized {
		module.isUnbound match {
			case true ⇒ modules -= module
				detached += module
				if (restoreAgent != null && module.isInstanceOf[RestorableModule]) restoreAgent.set(module, false)
//				try onModuleDetached(module) catch loggedE//todo let throw ?
				true
			case _ ⇒ false
		}
	}
	private[modules] def destroyed(module: Module): Unit = if (detached.contains(module)) {
		detached -= module
		cancelStateUpdate(module)
		modules.foreach { m ⇒
			if (isPredecessor(m, module)) prepare(m)
			else m.bindingRemove(module, false)
		}
		try onModuleDestroy(module) catch loggedE
		if (canStop) asyncContext.execute(() ⇒ stopSystem())
	}

	/* module misc */
	private[this] def find[M <: Module](clas: Class[M]): Option[M] = {
		modules.find(m ⇒ m.getClass == clas).asInstanceOf[Option[M]]
	}
	private[this] def isPredecessor(m1: Module, m2: Module): Boolean = {
		m1.getClass == m2.getClass && m1.ne(m2)
	}
	private[this] def moduleClass[M <: Module](implicit m: Manifest[M]): Class[M] = {
		m.runtimeClass.asInstanceOf[Class[M]]
	}
	private[modules] def postStateUpdate(delay: Long, m: Module): Unit = {
		asyncContext.execute(m, delay, () => m.updateState(StateParams.PostedUpdate.id))
	}
	private[modules] def cancelStateUpdate(m: Module): Unit = {
		asyncContext.cancel(m)
	}

	/* event api */
	protected[this] final def sendEventToModules[T <: Module : Manifest](e: ModuleEvent[T], modules: T*): Unit = {
		(if (modules.isEmpty) this.modules else modules).foreach {
			case m: T => m.callModuleEvent(e)
			case _ =>
		}
	}

	/* back door */
	private[modules] def postUpdate(delay: Long, m: Module): Unit = postStateUpdate(delay, m)
	private[modules] def cancelUpdate(m: Module): Unit = cancelStateUpdate(m)



	/* INTERNAL UTILS */
	class InternalUtils {
		final def bind[M <: Module](clas: Class[M]): M = {
			thisSystem.bind(clas, root, false, false, null)
		}
		final def bind[M <: Module](clas: Class[M], constructor: () ⇒ M): M = {
			thisSystem.bind(clas, root, false, false, constructor)
		}
	}
}




/* ROOT MODULE */
final class SystemModule private[modules](override val sys: ModuleSystem) extends Module







/* CONSTRUCT ARGS */
class ConstructArgs(val client: Module, val sync: Boolean, val restoring: Boolean, val isRoot: Boolean) {
	var server: Module = _
}






/* MODULE PREPARE PROMISE */
class ModulePreparePromise(val module: Module) {
	var onComplete: () ⇒ Unit = null
	def complete(): Unit = {
		if (onComplete != null) onComplete()
		module.setPrepared()
	}
}






/* RESTORE AGENT */
abstract class ModuleRestoreAgent(implicit system: ModuleSystem) {
	private[this] var phase = 0
	private[this] var temp: Set[String] = null
	lazy val autoStart = true
	if (autoStart) start()

	protected[this] def getList: TraversableOnce[String]
	protected[this] def clearList(): Unit
	protected[this] def add(moduleClass: String): Unit
	protected[this] def remove(moduleClass: String): Unit
	protected[this] def onStart(codeToRun: () ⇒ Unit): Unit = system.asyncContext.execute { () ⇒ codeToRun() }
	protected[this] def onRestored(clas: Class[_]): Unit = ()


	final def start(): Unit = if (phase == 0) {
		phase = 1
		try onStart(exec) catch loggedE
	}
	private[this] def exec(): Unit = {
		val classNames = getList match {
			case null => Nil
			case list => if (list.nonEmpty) clearList(); list
		}
		synchronized {
			if (temp != null) temp.foreach(clas ⇒ try add(clas) catch loggedE)
			phase = 2
			temp = null
		}
		classNames.foreach { name => try restore(name) catch loggedE }
	}
	private[this] def restore(name: String): Unit = {
		val cls = Class.forName(name).asInstanceOf[Class[_ <: Module]]
		val restorable = classOf[RestorableModule].isAssignableFrom(cls)
//		logV(s"BEFORE  RESTORED  $name;  restorable? $restorable;  not yet created? ${!system.hasModule(cls)}")
		if (restorable && !system.hasModule(cls)) {
			system.bind(cls, null, false, true, null)
			onRestored(cls)
		}
//		logV(s"AFTER RESTORED  ${cls.getSimpleName};  created? ${system.hasModule(cls)}")
	}
	protected[modules] final def set(m: Module, yep: Boolean): Unit = synchronized {
		val clas = m.getClass.getName
		if (phase < 2) {
			if (temp == null) temp = Set()
			if (yep) temp += clas else temp -= clas
		}
		else try if (yep) add(clas) else remove(clas) catch loggedE
	}
}
