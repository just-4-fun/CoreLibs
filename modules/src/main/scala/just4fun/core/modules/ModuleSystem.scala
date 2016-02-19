package just4fun.core.modules

import java.util.concurrent.locks.ReentrantLock
import scala.collection.mutable
import just4fun.core.async.{AsyncContextKey, DefaultAsyncContext, AsyncContext, AsyncContextOwner}
import just4fun.core.debug.DebugUtils._

private[modules] object ModuleSystem {
	private[modules] val STOPPED = 0
	private[modules] val STARTED = 1
	private[modules] val STOPPING = 2

	private[this] val systems = new mutable.WeakHashMap[ModuleSystem, Unit]()
	private[modules] var initialized: Boolean = false
	private[this] val lock = new Object
	private[this] var currentArgs: ConstructArgs = null

	def byName(name: String): Option[ModuleSystem] = {
		systems.collectFirst { case (s, _) if s.name == name ⇒ s }
	}
	private[modules] def init(s: ModuleSystem): Unit = {
		initialized = true
		systems.put(s, ())
	}

	private[modules] def construct[M <: Module](system: ModuleSystem, clas: Class[M], client: Module = null, sync: Boolean = false, restoring: Boolean = false, isRoot: Boolean = false)(constructModule: ⇒ M): M = lock.synchronized {
		currentArgs = new ConstructArgs(Thread.currentThread, system, clas, client, sync, restoring, isRoot)
		val m = constructModule
		if (m.isExternal) attach(m)
		else m.onConstructed()
		m
	}
	private[modules] def attach(m: Module): Unit = {
		if (currentArgs == null || currentArgs.locker != Thread.currentThread || currentArgs.serverClas != m.getClass) return
		val args = currentArgs
		currentArgs = null
		if (!m.isInstanceOf[SystemModule]) args.system.attach(m, args)
	}
}


trait ModuleSystem extends AsyncContextOwner {
//private[this] val _dc = disableDebugCode()
	import ModuleSystem.{STOPPED, STARTED, STOPPING}
	val name: String
	protected[this] implicit final val thisSystem: this.type = this
	implicit val asyncContext: AsyncContext = new DefaultAsyncContext
	protected[this] val restoreAgent: ModuleRestoreAgent = null
	private[this] val modules = mutable.ListBuffer[Module]()
	private[this] val detached = mutable.ListBuffer[Module]()
	private[this] val root: SystemModule = new SystemModule(this)
	private[this] val lock = new Object
	private[this] var state = STOPPED
	//
	ModuleSystem.init(this)


	/* system api */
	final def isSystemStarted: Boolean = state > STOPPED
	final def isSystemStopping: Boolean = state == STOPPING
	final def isSystemStopped: Boolean = state == STOPPED
	/* callbacks */
	protected[this] def onSystemStart(): Unit = ()
	protected[this] def onSystemStop(): Unit = ()
	/* internal */
	private[this] def startSystem(): Unit = {
		logV(s"*************  SYSTEM START  **************")
		onSystemStart()
	}
	private[this] def canStop: Boolean = modules.isEmpty && detached.isEmpty
	private[this] def stopSystem(): Unit = {
		if (canStop && isSystemStarted) {
			asyncContext.stop(true)
			onSystemStop()
			asyncContext.stop(true)
			state = STOPPED
			logV(s"*************  SYSTEM  STOP  **************")
		}
	}
	private[modules] def forceStopSystem(): Unit = if (isSystemStarted){
		logV(s"**  SYSTEM FORCED STOP  **")
		val err = new Exception("System is forced to stop.")
		modules.foreach(m ⇒ m.fail(err, false))
		modules.foreach(m ⇒ unbind(m.getClass))
		modules.foreach { server ⇒
			val others = modules.filterNot(_ == server)
			others.foreach(client ⇒ unbind(server.getClass, client))
		}
	}

	/* module api */
	final def moduleContent: Seq[Class[_]] = modules.map(_.getClass)

	final def hasModule[M <: Module : Manifest]: Boolean = hasModule(moduleClass)
	final def hasModule[M <: Module](cls: Class[M]): Boolean = find(cls).nonEmpty

	protected[this] final def bind[M <: Module](clas: Class[M]): M = {
		bind(clas, root, false, false)
	}
	protected[this] final def bind[M <: Module](clas: Class[M], constructor: () ⇒ M): M = {
		bind(clas, root, false, false, constructor)
	}
	protected[this] final def unbind[M <: Module](clas: Class[M]): Option[M] = {
		unbind(clas, root)
	}


	/* callbacks */
	protected[this] def onModulePrepare(promise: ModulePreparePromise): Unit = promise.complete()
	protected[this] def onModuleDestroy(m: Module): Unit = ()
	protected[this] def postStateUpdate(delay: Long)(implicit m: Module): Unit = {
		asyncContext.execute(m, delay, () => m.updateState(StateParams.PostedUpdate.id))
	}
	protected[this] def cancelStateUpdate(implicit m: Module): Unit = {
		asyncContext.cancel(m)
	}


	/* module bind */
	private[modules] def bind[M <: Module](serverClas: Class[M], clientModule: Module, sync: Boolean = false, restoring: Boolean = false, constr: () ⇒ M = null): M = lock.synchronized {
		val client = if (clientModule == null) root else clientModule
		val isRoot = client == root
		if (!isRoot && !modules.contains(client)) throw new ModuleBindException(serverClas, "It's not registered in system.")
		if (state == STOPPED || isRoot) state = STARTED
		find(serverClas) match {
			case Some(server) ⇒ if (!server.isConstructed) server.synchronized(server.wait())
				server.bindingAdd(client, sync, isRoot)
				server
			case None ⇒ if (modules.isEmpty) startSystem()
				val server = ModuleSystem.construct(this, serverClas, client, sync, restoring, isRoot) {
					val m = if (constr == null) serverClas.newInstance() else constr()
					if (m == null) serverClas.newInstance() else m
				}
				if (isRoot && restoreAgent != null && !server.isExternal && server.isInstanceOf[RestorableModule])
					restoreAgent.set(server, true)
				server
		}
	}
	private def attach(m: Module, args: ConstructArgs): Unit = {
		modules += m
		m.onAttached(args)
	}
	private[modules] def constructed(m: Module, args: ConstructArgs): Unit = {
		m.bindingAdd(args.client, args.sync, args.isRoot)
		m.synchronized(m.notifyAll())
	}
	private[modules] def prepare(m: Module): Unit = {
		if (m.isConstructed && !m.isPrepared && !detached.exists(isPredecessor(_, m)))
			try onModulePrepare(new ModulePreparePromise(m))
			catch {case e: Throwable ⇒ m.fail(e, false)}
	}

	/* module unbind */
	private[modules] def unbind[M <: Module](serverClas: Class[M], clientModule: Module): Option[M] = {
		val client = if (clientModule == null) root else clientModule
		val isRoot = client == root
		find(serverClas) match {
			case mOp@Some(m) ⇒ if (!m.isConstructed) m.synchronized(m.wait())
				m.bindingRemove(client, isRoot) match {
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

	/* event api */
	protected[this] final def sendEventToModules[T <: Module : Manifest](e: ModuleEvent[T], modules: T*): Unit = {
		(if (modules.isEmpty) this.modules else modules).foreach {
			case m: T => m.callModuleEvent(e)
			case _ =>
		}
	}

	/* back door */
	private[modules] def postUpdate(delay: Long)(implicit m: Module): Unit = postStateUpdate(delay)
	private[modules] def cancelUpdate()(implicit m: Module): Unit = cancelStateUpdate
}




/* ROOT MODULE */
final class SystemModule private[modules](s: ModuleSystem) extends Module {
	sys = s
}




/* MODULE PREPARE PROMISE */
class ModulePreparePromise(val module: Module) {
	def complete(): Unit = module.onPrepared()
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
			system.bind(cls, null, false, true)
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
