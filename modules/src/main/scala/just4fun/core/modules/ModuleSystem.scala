package just4fun.core.modules

import java.util.concurrent.locks.ReentrantLock
import scala.collection.mutable
import just4fun.core.async.{FutureContext, FutureContextOwner}
import just4fun.core.debug.DebugUtils._

private[modules] object ModuleSystem {
	val STOPPED = 0
	val STARTED = 1
	val STOPPING = 2

	private[this] val lock = new ReentrantLock
	private[this] var constructArgs: ConstructArgs[_] = null
	private[this] var stopID: Int = 0

	def construct[M <: Module](system: ModuleSystem, clas: Class[M], client: Module = null, sync: Boolean = false, restoring: Boolean = false)(constructModule: ⇒ M): M = {
		lock.lock()
		constructArgs = new ConstructArgs(Thread.currentThread, system, clas, client, sync, restoring)
		try {
			val m = constructModule
			if (m.isExternallyCreated) constructed(m)
			m
		}
		finally if (lock.isHeldByCurrentThread) {
			constructArgs = null
			lock.unlock()
		}
	}
	def constructed(implicit m: Module): ModuleSystem = {
		if (constructArgs == null || constructArgs.locker != Thread.currentThread || constructArgs.clas != m.getClass) return null
		// the only way to hack is to instantiate module of same class as that which is currently constructing
		val args = constructArgs
		constructArgs = null
		lock.unlock()
		m match {
			case _: RootModule =>
			case _ => args.system.attach(m, args.client, args.sync, args.restoring)
		}
		args.system
	}

	def nextStopID: Int = { stopID -= 1; stopID }
}


trait ModuleSystem extends FutureContextOwner {
//private[this] val _dc = disableDebugCode()
	import ModuleSystem.{STOPPED, STARTED, STOPPING}
	implicit val asyncContext: FutureContext
	protected[this] implicit final val thisSystem: this.type = this
	protected[this] val restoreAgent: ModuleRestoreAgent = null
	private[this] val modules = mutable.ListBuffer[Module]()
	private[this] val detached = mutable.ListBuffer[Module]()
	private[this] var root: RootModule = null
	private[this] val lock = new ReentrantLock
	private[this] var state = STOPPED
	private[this] var constructModules: mutable.HashMap[Thread, List[Module]] = mutable.HashMap()


	/* SYSTEM API */
	final def isSystemStarted: Boolean = state > STOPPED
	final def isSystemStopping: Boolean = state == STOPPING
	/* callbacks */
	protected[this] def onSystemStart(): Unit = ()
	protected[this] def onSystemStop(): Unit = ()
	/* internal */
	private[this] def startSystem(): Unit = {
		logV(s"*************  SYSTEM START  **************")
		onSystemStart()
	}
	private[this] def canStop: Boolean = {
//		logV(s"canStop locked? ${lock.isLocked};  detached.empty? ${detached.isEmpty};  mods: ${modules.map(_.getClass.getSimpleName).mkString(", ")}")
		modules.isEmpty && detached.isEmpty
	}
	private[this] def stopSystem(): Unit = {
//		logV(s"SYS Stop: canStop? ${canStop}; notStopped? ${!isSystemStopped};  nonLocked& ${!lock.isLocked}")
		if (canStop && isSystemStarted && !lock.isLocked) {
			asyncContext.stop(true)
			if (!lock.isLocked) onSystemStop()
			asyncContext.stop(true)
			root = null
			state = STOPPED
			logV(s"*************  SYSTEM  STOP  **************")
		}
	}

	/* MODULE API */
	final def moduleContent: Seq[Class[_]] = modules.map(_.getClass)

	final def hasModule[M <: Module : Manifest]: Boolean = hasModule(moduleClass)
	final def hasModule[M <: Module](cls: Class[M]): Boolean = find(cls).nonEmpty

	final def moduleConnector[M <: Module](clas: Class[M], constructor: () ⇒ M = null): ModuleConnector[M] = {
		new ModuleConnector[M](this, clas, constructor)
	}

	protected[this] final def startModule[M <: Module](clas: Class[M], stopID: Int = 0, constructor: () ⇒ M = null): M = {
		bind(clas, rootClient, false, false, stopID, constructor)
	}
	protected[this] final def stopModule[M <: Module](clas: Class[M], stopID: Int = 0): Option[M] = {
		unbind(clas, rootClient, stopID)
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
	protected[this] def currentTimeMs: Long = System.currentTimeMillis()


	/* MODULE BIND */
	private[modules] def bind[M <: Module](serverClas: Class[M], clientModule: Module, sync: Boolean = false, restoring: Boolean = false, stopID: Int = 0, constr: () ⇒ M = null): M = {
//		logV(s"before LOCK bind [${Thread.currentThread().getName}] :  [${serverClas.getSimpleName}]")
		lock.lock()
		val holdCount = lock.getHoldCount
//		logV(s"after LOCK bind [${Thread.currentThread().getName}] :  [${serverClas.getSimpleName}]")
		try {
			val client = if (clientModule == null) rootClient else clientModule
			val isRoot = client == root
			if (!client.isInstanceOf[RootModule] && !modules.contains(client))
				throw new ModuleBindException(serverClas, "It's not registered in system.")
			if (state == STOPPED || isRoot) state = STARTED
			find(serverClas) match {
				case Some(server) ⇒ val added = server.bindingAdd(client, sync)
//					logV(s"before UNLOCK bind [${Thread.currentThread().getName}] :  [${serverClas.getSimpleName}]")
					lock.unlock()
					if (added) {
						if (!server.isConstructed) server.synchronized(server.wait(100))
						server.onBindingAdd(client, sync, stopID, isRoot)
					}
					server
//		logV(s"Sys bind [${client.getClass.getSimpleName}] to [${m.getClass.getSimpleName}];  exists; sync? $sync; restore? $restore")
				case None ⇒ if (modules.isEmpty) startSystem()
					val server = try ModuleSystem.construct(this, serverClas, client, sync, restoring) {
						val m = if (constr == null) serverClas.newInstance() else constr()
						if (m == null) serverClas.newInstance() else m
					}
					catch {case e: Throwable ⇒ currentModule.fail(e, false); currentModule.asInstanceOf[M]}
					if (server != currentModule) throw new ModuleBindException(serverClas, s"It's constructed outside the system.")
					currentModule = null
//		logV(s"Sys Constructed [${clas.getSimpleName}]")
					if (server.isExternallyCreated) {
						// WARN: setCreated and prepare can be called before instance is fully constructed
						asyncContext.execute(null, 50, () ⇒ constructed(server, client, sync, stopID, isRoot))
					}
					else {
						constructed(server, client, sync, stopID, isRoot)
						if (stopID == 0 && isRoot && restoreAgent != null) server match {
							case s: RestorableModule ⇒ restoreAgent.set(s, true)
							case _ ⇒
						}
					}
					server
			}
		}
		finally if (holdCount >= lock.getHoldCount && lock.isHeldByCurrentThread) {
			logV(s"!!! unexpected UNLOCK bind [${Thread.currentThread().getName}] :  [${serverClas.getSimpleName}]")
			lock.unlock()
		}
	}
	private def attach(m: Module, client: Module, sync: Boolean, restoring: Boolean): Unit = {
		modules += m
		currentModule = m
		m.bindingAdd(client, sync)
		this match {case s: RestorableModule if restoring => s.setRestored(); case _ =>}
//			logV(s"before UNLOCK bind [${Thread.currentThread().getName}] :  [${m.getClass.getSimpleName}]")
		lock.unlock()
	}
	private[this] def constructed(m: Module, client: Module, sync: Boolean, stopID: Int, isRoot: Boolean): Unit = {
		m.setConstructed()
		m.synchronized(m.notifyAll())
		m.onBindingAdd(client, sync, stopID, isRoot)
		if (!detached.exists(isPredecessor(_, m))) prepareModule(m)
	}
	private[this] def currentModule_=(m: Module): Unit = {
		val thread = Thread.currentThread()
		var list = constructModules.getOrElse(thread, Nil)
		m match {
			case null => list = list.tail
			case _ => list = m :: list
		}
		if (list.isEmpty) constructModules -= thread else constructModules += thread → list
	}
	private[this] def currentModule: Module = {
		val thread = Thread.currentThread()
		constructModules.get(thread) match {
			case None => null
			case Some(list) => list.head
		}
	}
	private[this] def prepareModule(m: Module): Unit = if (!m.isPrepared) {
		try onModulePrepare(new ModulePreparePromise(m)) catch loggedE
	}

	/* MODULE UNBIND */
	private[modules] def unbind[M <: Module](serverClas: Class[M], clientModule: Module, stopID: Int = 0): Option[M] = {
		val client = if (clientModule == null) rootClient else clientModule
		val isRoot = client == root
		find(serverClas) match {
			case mOp@Some(m) ⇒ if (!m.isConstructed) m.synchronized(m.wait(100))
//				logV(s"Sys unbind [${client.getClass.getSimpleName}] from [${m.getClass.getSimpleName}];  stopID= $stopID")
				m.bindingRemove(client, stopID, isRoot) match {
					case true ⇒ if (isRoot && !modules.exists(_.isRootServer)) state = STOPPING; mOp
					case _ ⇒ None
				}
			case _ ⇒ None
		}
	}
	private[modules] def detach(implicit module: Module): Boolean = {
		lock.lock()
		val fail = try module.isBound || {
			modules -= module
			detached += module
			false
		}
		finally lock.unlock()
		if (!fail && restoreAgent != null) module match {
			case s: RestorableModule ⇒ restoreAgent.set(s, false)
			case _ ⇒
		}
		!fail
	}
	private[modules] def destroyed(implicit module: Module): Unit = {
		detached -= module
		cancelStateUpdate(module)
		modules.foreach { m ⇒
			if (isPredecessor(m, module)) prepareModule(m)
			else m.bindingRemove(module, 0, false)
		}
		try onModuleDestroy(module) catch loggedE
		if (canStop) asyncContext.execute(() ⇒ stopSystem())
	}

	/* MODULE MISC */
	private[this] def find[M <: Module](clas: Class[M]): Option[M] = {
		modules.find(m ⇒ m.getClass == clas).asInstanceOf[Option[M]]
	}
	private[this] def isPredecessor(m1: Module, m2: Module): Boolean = {
		m1.getClass == m2.getClass && m1.ne(m2)
	}
	private[this] def moduleClass[M <: Module](implicit m: Manifest[M]): Class[M] = {
		m.runtimeClass.asInstanceOf[Class[M]]
	}
	private[this] def rootClient: Module = lock synchronized {
		if (root == null) root = ModuleSystem.construct(this, classOf[RootModule])(new RootModule)
		root
	}

	/* EVENT API */
	protected[this] final def sendEventToModules[T <: Module : Manifest](e: ModuleEvent[T]): Unit = {
		modules.foreach {
			case m: T => m.callModuleEvent(e)
			case _ =>
		}
	}

	/* BACK DOOR */
	private[modules] def postUpdate(delay: Long)(implicit m: Module): Unit = postStateUpdate(delay)
	private[modules] def cancelUpdate()(implicit m: Module): Unit = cancelStateUpdate
	private[modules] def now: Long = currentTimeMs
}



/* CONSTRUCT ARGS */
class ConstructArgs[M <: Module](val locker: Thread, val system: ModuleSystem, val clas: Class[_], val client: Module = null, val sync: Boolean = false, val restoring: Boolean = false)




/* ROOT MODULE */
private[modules] class RootModule extends Module




/* MODULE PREPARE PROMISE */
class ModulePreparePromise(val module: Module) {
	def complete(): Unit = module.setPrepared()
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
	protected[modules] final def set(m: RestorableModule, yep: Boolean): Unit = synchronized {
		val clas = m.getClass.getName
		if (phase < 2) {
			if (temp == null) temp = Set()
			if (yep) temp += clas else temp -= clas
		}
		else try if (yep) add(clas) else remove(clas) catch loggedE
	}
}



/* MODULE CONNECTOR */
/** Usage Contract:  Eventually module should be disconnected. Module should not be used after disconnected. */
class ModuleConnector[M <: Module](system: ModuleSystem, clas: Class[M], constr: () ⇒ M) {
	import ModuleSystem._
	private[this] val stopID = nextStopID
	private[this] var _module: M = _

	final def module: M = moduleConnect
	final def moduleConnect(): M = {
		if (_module == null) _module = system.bind(clas, null, false, false, stopID, constr)
		_module
	}
	final def moduleDisconnect(): Unit = {
		if (_module != null) system.unbind(clas, null, stopID)
		_module = null.asInstanceOf[M]
	}
}
