package just4fun.core.modules

import java.util.concurrent.locks.ReentrantLock
import scala.collection.mutable
import scala.concurrent.{Promise, Future}
import scala.util.{Try, Failure, Success}
import just4fun.core.async.{AsyncContextKey, DefaultAsyncContext, AsyncContext, AsyncContextOwner}
import just4fun.core.debug.DebugUtils._

object ModuleContainer {
	private[modules] val EMPTY = 0
	private[modules] val POPULATED = 1
	private[modules] val EMPTYING = 2

	private[this] val containers = new mutable.WeakHashMap[ModuleContainer, Unit]()
	private[modules] var initialized: Boolean = false
	private[modules] val lock = new Object
	private[modules] var currentContainer: ModuleContainer = null

	//TODO are those methods needed anymore ?
	def default: Option[ModuleContainer] = byId(defaultId)
	def defaultId: String = "_defaultContainer_"
	def byId(id: String): Option[ModuleContainer] = id match {
		case null ⇒ None
		case _ ⇒ containers.collectFirst { case (s, _) if s.containerId == id ⇒ s }
	}
	private[modules] def init(c: ModuleContainer): Unit = {
		initialized = true
		containers.put(c, ())
	}

	private[modules] def register(m: Module): ModuleContainer = lock.synchronized {
		if (currentContainer != null) currentContainer.register(m)
		currentContainer
	}
}


trait ModuleContainer extends AsyncContextOwner {
	//todo private[this] val _dc = disableDebugCode()
	import ModuleContainer.{EMPTY, POPULATED, EMPTYING}
	val containerId: String = getClass.getName
	protected[this] implicit final val thisContainer: this.type = this
	implicit val asyncContext: AsyncContext = new DefaultAsyncContext
	protected[this] val internal: InternalUtils = new InternalUtils
	protected[this] val restoreAgent: ModuleRestoreAgent = null
	private[this] val regd = mutable.ListBuffer[Module]()
	private[this] val unregd = mutable.ListBuffer[Module]()
	private[this] val lock = new Object
	private[this] var state = EMPTY
	private[this] var currentArgs: List[ConstructArgs] = Nil
	private[this] lazy val successF = Promise[Unit]().success().future
	//
	ModuleContainer.init(this)


	/* Container api */
	final def isContainerEmptying: Boolean = lock.synchronized(state == EMPTYING)
	final def isContainerEmpty: Boolean = lock.synchronized(state == EMPTY)
	/* callbacks */
	protected[this] def onContainerPopulate(): Unit = ()
	protected[this] def onContainerEmpty(): Unit = ()
	/* internal */
	private[this] def setPopulated(): Unit = {
		state = POPULATED
		logV(s"*************  ${getClass.getSimpleName} POPULATED  **************")
		onContainerPopulate()
	}
	private[this] def isEmpty: Boolean = regd.isEmpty && unregd.isEmpty
	private[this] def setEmpty(): Unit = lock.synchronized {
		if (isEmpty && state != EMPTY) {
			asyncContext.stop(true)
			onContainerEmpty()
			asyncContext.stop(true)
			logV(s"*************  ${getClass.getSimpleName}  EMPTY  **************")
			state = EMPTY
		}
	}
	private[modules] def forceEmpty(): Unit = lock.synchronized {
		if (!isContainerEmpty) {
			logV(s"**  CONTAINER FORCED EMPTY  **")
			val err = new Exception("Container is forced to stop.")
			regd.foreach(m ⇒ m.fail(err, false))
			regd.foreach(m ⇒ unbindModule(m.getClass))
			regd.foreach(m ⇒ unbindAll(m))
		}
	}

	/* module api */
	final def modulesCount: Int = {
		regd.size
	}
	final def containsModule[M <: Module : Manifest]: Boolean = {
		containsModule(moduleClass) // TODO macros
	}
	final def containsModule[M <: Module](cls: Class[M]): Boolean = {
		find(cls).nonEmpty
	}

	final def bindModule[M <: Module : Manifest]: Unit = {
		// TODO macros
		bind(moduleClass[M], null, false, false, null)
	}
	final def bindModule[M <: Module : Manifest](constructor: () ⇒ M): Unit = {
		bind(moduleClass[M], null, false, false, constructor)
	}
	final def bindModule[M <: Module](clas: Class[M]): Unit = {
		bind(clas, null, false, false, null)
	}
	final def bindModule[M <: Module](clas: Class[M], constructor: () ⇒ M): Unit = {
		bind(clas, null, false, false, constructor)
	}
	final def bindModule[M <: Module](m: M): Unit = {
		bind(m.getClass, null, false, false, null)
	}
	final def unbindModule[M <: Module : Manifest]: Unit = {
		unbind(moduleClass[M], null) // TODO macros
	}
	final def unbindModule[M <: Module](clas: Class[M]): Unit = {
		unbind(clas, null)
	}
	final def unbindModule[M <: Module](m: M): Unit = {
		unbind(m.getClass, null)
	}

	protected[this] def foreachModuleOfType[T <: Module : Manifest](exec: T ⇒ Unit): Unit = regd.foreach {
		case b: T ⇒ b.tryOrDie(exec(b))
		case _ ⇒
	}

	/* callbacks */
	protected[this] def onModulePrepare(module: Module): Future[Unit] = successF
	//	protected[this] def onModuleDUnregistered(m: Module): Unit = ()
	protected[this] def onModuleDestroy(m: Module): Unit = ()


	/* module bind */
	private[modules] def bind[M <: Module](boundC: Class[M], binder: Module, sync: Boolean, restore: Boolean, constr: () ⇒ M): M = lock.synchronized {
		val isRoot = binder == null
		if (!isRoot && !regd.contains(binder)) throw new ModuleBindException(boundC, "It's not registered in Container.")
		if (state == EMPTYING && isRoot) state = POPULATED
		find(boundC) match {
			case Some(bound) ⇒ bound.addBinder(binder, sync); bound
			case None ⇒ if (isContainerEmpty) setPopulated()
				args = new ConstructArgs(restore)
				try {
					construct {
						val m = if (constr == null) boundC.newInstance() else constr()
						if (m == null) boundC.newInstance()
					}
					val bound = args.boundModule.asInstanceOf[M]
					bound.addBinder(binder, sync)
					if (isRoot && restoreAgent != null && bound.isInstanceOf[RestorableModule]) restoreAgent.set(bound, true)
					bound.setConstructed()
					bound
				}
				finally args = null
		}
	}
	private[modules] def construct(constructModule: ⇒ Unit): Unit = ModuleContainer.lock.synchronized {
		ModuleContainer.currentContainer = this
		try constructModule catch {
			case e: Throwable ⇒ args.boundModule match {
				case null ⇒ throw e
				case m ⇒ m.fail(e, false)
			}
		}
		finally ModuleContainer.currentContainer = null
	}
	private[modules] def register(m: Module): Unit = {
		regd += m
		val a = args
		a.boundModule = m
		if (a.restoring) m.setRestored()
	}
	private[this] def args: ConstructArgs = currentArgs.head
	private[this] def args_=(a: ConstructArgs): Unit = a match {
		case null => currentArgs = currentArgs.tail
		case _ => currentArgs = a :: currentArgs
	}

	private[modules] def prepare(m: Module): Unit = {
		if (m.status.constructed && !m.status.prepared && !unregd.exists(isPredecessor(_, m))) m.tryOrFail {
			val future = onModulePrepare(m)
			val onCompleted: PartialFunction[Try[Unit], Unit] = {
				case Failure(e) ⇒ m.fail(e, true); m.setPrepared()
				case _ ⇒ m.setPrepared()
			}
			if (future.isCompleted) onCompleted(future.value.get) else future.onComplete(onCompleted)
		}
	}

	/* module unbind */
	private[modules] def unbind[M <: Module](boundC: Class[M], binder: Module): Unit = lock.synchronized {
		val isRoot = binder == null
		find(boundC) match {
			case Some(m) ⇒ m.removeBinder(binder)
				if (isRoot && !regd.exists(_.isBoundByContainer)) state = EMPTYING
			case _ ⇒
		}
	}
	private[modules] def unbindAll(binder: Module): Unit = lock.synchronized {
		regd.foreach(_.removeBinder(binder))
	}
	private[modules] def unregister(module: Module): Boolean = lock.synchronized {
		module.isUnbound match {
			case true ⇒ regd -= module
				unregd += module
				if (restoreAgent != null && module.isInstanceOf[RestorableModule]) restoreAgent.set(module, false)
				//				try onModuleUnregistered(module) catch loggedE//todo let throw ?
				true
			case _ ⇒ false
		}
	}
	private[modules] def destroyed(module: Module): Unit = if (unregd.contains(module)) {
		unregd -= module
		cancelStateUpdate(module)
		regd.foreach { m ⇒
			if (isPredecessor(m, module)) prepare(m)
			else m.removeBinder(module)
		}
		try onModuleDestroy(module) catch loggedE
		if (isEmpty) asyncContext.execute(() ⇒ setEmpty())
	}

	/* module misc */
	private[this] def find[M <: Module](clas: Class[M]): Option[M] = {
		regd.find(m ⇒ m.getClass == clas).asInstanceOf[Option[M]]
	}
	private[this] def isPredecessor(m1: Module, m2: Module): Boolean = {
		m1.getClass == m2.getClass && m1.ne(m2)
	}
	private[modules] def moduleClass[M <: Module](implicit m: Manifest[M]): Class[M] = {
		m.runtimeClass.asInstanceOf[Class[M]]
	}
	private[modules] def postStateUpdate(delay: Long, m: Module): Unit = {
		asyncContext.execute(m, delay, () => m.updateState(StateParams.PostedUpdate.id))
	}
	private[modules] def cancelStateUpdate(m: Module): Unit = {
		asyncContext.cancel(m)
	}


	/* INTERNAL UTILS */
	class InternalUtils {
		final def bind[M <: Module](clas: Class[M]): M = {
			thisContainer.bind(clas, null, false, false, null)
		}
		final def bind[M <: Module](clas: Class[M], constructor: () ⇒ M): M = {
			thisContainer.bind(clas, null, false, false, constructor)
		}
	}
}






/* CONSTRUCT ARGS */
class ConstructArgs(val restoring: Boolean) {
	var boundModule: Module = _
}






/* RESTORE AGENT */
abstract class ModuleRestoreAgent(implicit container: ModuleContainer) {
	private[this] var phase = 0
	private[this] var temp: Set[String] = null
	lazy val autoStart = true
	if (autoStart) start()

	protected[this] def getList: TraversableOnce[String]
	protected[this] def clearList(): Unit
	protected[this] def add(moduleClass: String): Unit
	protected[this] def remove(moduleClass: String): Unit
	protected[this] def onStart(codeToRun: () ⇒ Unit): Unit = container.asyncContext.execute { () ⇒ codeToRun() }
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
		//		logV(s"BEFORE  RESTORED  $name;  restorable? $restorable;  not yet created? ${!container.hasModule(cls)}")
		if (restorable && !container.containsModule(cls)) {
			container.bind(cls, null, false, true, null)
			onRestored(cls)
		}
		//		logV(s"AFTER RESTORED  ${cls.getSimpleName};  created? ${container.hasModule(cls)}")
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
