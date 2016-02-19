package just4fun.core.modules

import scala.StringBuilder
import just4fun.core.async.Async.DummyImplicit2
import just4fun.core.async._
import just4fun.core.debug.DebugConfig
import just4fun.core.debug.DebugUtils._
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.concurrent.Future
import scala.language.experimental.macros
import scala.util.{Failure, Try}


/* STATES */
object ModuleState extends Enumeration {
	type ModuleState = Value
	val CONSTRUCTING, PREPARING, RESTING, ACTIVATING, OPERATING, DEACTIVATING, FAILED, DESTROYED = Value
}


object Module {
	private[modules] val uFlagNONE = 0
	private[modules] val uFlagREPEAT = 1
	private[modules] val uFlagBUSY = 2
	private[modules] val uFlagTOUCHED = 3
	private[modules] val uFlagRESUME = 4
	val logTagState = 0x7102111
	val logTagStateX = 0x7102112
	val logTagParam = 0x7102113
	val logTagStats = 0x7102114
}

/* MODULE */
/* TODO: track wasFinishedOk via abstract Prefs */
/** LifeCycleCallbacks and InternalCallbacks are guaranteed to first call not earlier than instance is constructed.  */
trait Module extends DelayedInit with StateProcessor with BindingProcessor with RequestProcessor with AsyncContextOwner {
//todo private[this] val _dc = disableDebugCode()
	import ModuleState._
	import StateParams._
	protected[this] implicit final val thisModule: this.type = this
	private[modules] var sys: ModuleSystem = null
	private[modules] var args: ConstructArgs = null
	implicit lazy val asyncContext: AsyncContext = if (sys == null) new DefaultAsyncContext else sys.asyncContext
	protected[this] lazy val lifeCycle: LifeCycleCallbacks = new LifeCycleCallbacks
	protected[this] lazy val internal: InternalCallbacks = new InternalCallbacks
	lazy val info: ModuleInfo = new ModuleInfo
//
	attach()


	/* */
	protected[this] def system: ModuleSystem = sys

	/* LIFECYCLE CALLBACKS */
	class LifeCycleCallbacks {
		import ModuleState._
		import StateParams._
		protected[this] var restLatencyMs = 10000
		protected[this] var destroyLatencyMs = 0
		// STATE callbacks
		final def completeActivating(): Unit = if (isActivating) on_!!!(Complete)
		final def completeDeactivating(): Unit = if (isDeactivating) on_!!!(Complete)
		protected[this] def onActivatingStart(creating: Boolean, complete: CompleteSelector): CompleteOption = complete.now
		protected[this] def onActivatingComplete(creating: Boolean): Unit = ()
		protected[this] def onDeactivatingStart(destroying: Boolean, complete: CompleteSelector): CompleteOption = complete.now
		protected[this] def onDeactivatingComplete(destroying: Boolean): Unit = ()
		protected[this] def progressBackoff: Long = calcBackoff
		protected[this] final def progressDurationMs: Long = progressDuration
		protected[this] def onFailed(): Unit = ()
		// BACK DOOR
		private[modules] def callFailed(): Unit = {
			try onFailed() catch loggedE
		}
		private[modules] def callStart(): CompleteOption = {
			if (isActivating) onActivatingStart(is(Creating), CompleteSelector) else onDeactivatingStart(is(Detached), CompleteSelector)
		}
		private[modules] def callComplete(): Unit = {
			if (isActivating) onActivatingComplete(is(Creating)) else onDeactivatingComplete(is(Detached))
		}
		private[modules] def latency = if (isUnbound) destroyLatencyMs else restLatencyMs
		private[modules] def backoff: Long = try progressBackoff catch loggedE(calcBackoff)
	}

	/* INTERNAL CALLBACKS */
	class InternalCallbacks {
		protected[this] def onConstructed(): Unit = ()
		protected[this] def onPrepared(): Unit = ()
		protected[this] def onDestroyed(): Unit = ()
		// BINDING callbacks
		protected[this] def onBindingAdd(moduleClas: Class[_], sync: Boolean): Unit = ()
		protected[this] def onBindingRemove(moduleClas: Class[_]): Unit = ()
		// REQUEST callbacks
		protected[this] def onRequestAdd(requestClas: Class[_]): Unit = {}
		protected[this] def onRequestRemove(requestClas: Class[_]): Unit = {}
		//
		protected[this] def onAbleToServeNow(yep: Boolean): Unit = {}
		// BACK DOOR
		private[modules] def callConstructed() = tryOrDie(onConstructed())
		private[modules] def callPrepared() = tryOrDie(onPrepared())
		private[modules] def callDestroyed() = try onDestroyed() catch loggedE
		private[modules] def callBindAdd(clas: Class[_], sync: Boolean): Unit = tryOrDie(onBindingAdd(clas, sync))
		private[modules] def callBindRemove(clas: Class[_]): Unit = tryOrDie(onBindingRemove(clas))
		private[modules] def callRequestAdd(clas: Class[_]): Unit = tryOrDie(onRequestAdd(clas))
		private[modules] def callRequestRemove(clas: Class[_]): Unit = tryOrDie(onRequestRemove(clas))
		private[modules] def callAbleToServeNow(yep: Boolean) = tryOrDie(onAbleToServeNow(yep))
	}

	/* PUBLIC INFO */
	class ModuleInfo {
		import StateParams._
		def state: ModuleState = getState
		def isPreparing: Boolean = getState == PREPARING
		def isOperating: Boolean = getState == OPERATING
		def isActivating: Boolean = getState == ACTIVATING
		def isDeactivating: Boolean = getState == DEACTIVATING
		def isResting: Boolean = getState == RESTING
		def isFailed: Boolean = getState == FAILED
		def isUnavailable: Boolean = is(Detached)
		def isSuspended: Boolean = is(Suspended)
		def isRestful: Boolean = is(Restful)
		def isRestored: Boolean = is(Restored)
		def failure: Option[Throwable] = Module.this.failure
		override def toString(): String = {
			val buff = new StringBuilder
			buff ++= "state: " ++= state.toString ++= ";  bindings: " ++= clientsCount.toString ++= ";  requests: " ++= requestsNum.toString ++= ";  params: "
			StateParams.values.foreach { param ⇒
				if (param.id > 0) buff ++= ", "
				buff ++= param.toString ++= ":"
				if ((getParams & (1 << param.id)) == 0) buff ++= "0" else buff ++= "1"
			}
			buff.toString()
		}
	}

}




/* BINDING PROCESSOR */
trait BindingProcessor {
	self: Module =>
	import StateParams._
	private[this] val clients = mutable.Set[Module]()
	private[modules] val syncClients = mutable.Set[Module]()
	private[modules] val syncServers = mutable.Set[Module]()

	protected[modules] final def isBound: Boolean = clients.nonEmpty
	protected[modules] final def isUnbound: Boolean = clients.isEmpty
	protected[this] final def bind[M <: Module : Manifest]: M = macro Macros.bind[M]
	protected[this] final def bindSync[M <: Module : Manifest]: M = macro Macros.bindS[M]
	protected[this] final def unbind[M <: Module : Manifest]: Unit = {
		internalUnbind(implicitly[Manifest[M]].runtimeClass.asInstanceOf[Class[M]])
	}
	protected[this] final def unbind[M <: Module : Manifest](s: M): Unit = {
		internalUnbind(s.getClass.asInstanceOf[Class[M]])
	}
	//
	protected[this] final def internalBind[M <: Module](clas: Class[M], sync: Boolean): M = {
		if (isDetached) null.asInstanceOf[M] else sys.bind(clas, this, sync)
	}
	protected[this] final def internalUnbind[M <: Module](clas: Class[M]): Unit = {
		if (!isDetached) sys.unbind(clas, this)
	}
	protected[modules] final def isBoundTo(client: Module): Boolean = clients.contains(client)
	/* internal */
	private[modules] def clientsCount = clients.size
	private[modules] def bindingAdd(client: Module, sync: Boolean, root: Boolean): Unit = clients.add(client) match {
		case true ⇒ if (root) on_(BoundBySystem)
			internal.callBindAdd(client.getClass, sync)
			if (not(HasBindings)) on_!!!(HasBindings)
			if (sync) addSyncClient(client)
			if (DebugConfig.isDebug) client.detectCyclicBinding(this, client :: Nil) match {
				case Nil =>
				case trace => logW(s"Cyclic module relations detected in chain [${trace.map(_.getClass.getSimpleName).mkString(", ")}]")
			}
		case false ⇒ if (sync) addSyncClient(client) else removeSyncClient(client)
	}
	private[modules] def bindingRemove(client: Module, root: Boolean): Boolean = {
		clients.remove(client) match {
			case true ⇒ internal.callBindRemove(client.getClass)
				if (root) off_(BoundBySystem)
				removeSyncClient(client)
				if (clients.isEmpty) off_!!!(HasBindings)
				true
			case _ ⇒ false
		}
	}
	private[this] def addSyncClient(client: Module): Unit = if (syncClients.add(client)) {
		client.addSyncServer(this)
		if (isFailed) client.fail(new SyncServerException(this))
		else clientActive(client.is(Active))
	}
	private[this] def removeSyncClient(client: Module): Boolean = syncClients.remove(client) match {
		case true => client.removeSyncServer(this); clientActive(false); true
		case false => false
	}
	private[BindingProcessor] def addSyncServer(server: Module): Unit = {
		if (syncServers.add(server) && !server.isOperating) serverOperating(false)
	}
	private[BindingProcessor] def removeSyncServer(server: Module): Unit = {
		if (syncServers.remove(server)) serverOperating(true)
	}
	private[BindingProcessor] def detectCyclicBinding(server: Module, trace: List[Module]): List[Module] = {
		if (this != server) clients.foreach { client =>
			val res = if (this == client) Nil else if (server == client) client :: trace else client.detectCyclicBinding(server, client :: trace)
			if (res.nonEmpty) return res
		}
		Nil
	}

	/* EVENT API */
	protected[this] def sendEventToClients[T <: Module : Manifest](e: ModuleEvent[T], modules: T*): Unit = {
		(if (modules.isEmpty) clients else modules).foreach {
			case m: T => m.callModuleEvent(e)
			case _ =>
		}
	}
	protected[this] def onModuleEvent(e: ModuleEvent[_]): Unit = ()
	private[modules] def callModuleEvent(e: ModuleEvent[_]): Unit = tryOrDie(onModuleEvent(e))
}




/* REQUEST PROCESSOR */
trait RequestProcessor {
	self: Module =>
	import StateParams._
	//	private[this] val requests = mutable.ArrayBuffer[ModuleRequest[_]]()
	private[this] val requests = mutable.ListBuffer[ModuleRequest[_]]()
	private[this] var syncRequests: Int = 0
	private[this] var execCount: Int = 0

	protected[this] final def hasNoRequests: Boolean = requests.synchronized(requests.isEmpty && syncRequests <= 0)
	protected[this] final def hasRequests: Boolean = !hasNoRequests
	private[modules] def hasNoWork: Boolean = requests.synchronized(hasNoRequests || (is(Suspended) && execCount <= 0))
	private[modules] def hasWork: Boolean = !hasNoWork
	private[modules] def updateHasWork(): Boolean = {
		val ok = requests.synchronized { if (not(HasWork) && hasWork) {on_(HasWork); true} else false}
		if (ok) updateState(HasWork.id)
		ok
	}
	private[modules] def updateHasNoWork(): Boolean = {
		val ok = requests.synchronized{ if (is(HasWork) && hasNoWork) {off_(HasWork); true} else false}
		if (ok) updateState(HasWork.id)
		ok
	}
	private[modules] def requestsNum = requests.size
	protected[this] final def serveRequest[T](request: ModuleRequest[T]): Async[T] = requestAdd(request)
	protected[this] final def cancelRequests(filter: ModuleRequest[_] => Boolean = null): Unit = requests.synchronized {
		filter match {
			case null => requests.foreach(_.cancel())
			case _ => requests.withFilter(filter).foreach(_.cancel())
		}
	}
	/* wrappers */
	protected[this] final def serveAsync[T](code: => T)(implicit asyncContext: AsyncContext): Async[T] = {
		requestAdd(new ModuleRequest[T].task(code))
	}
	protected[this] final def serveAsync[T](code: => Async[T])(implicit asyncContext: AsyncContext, d: DummyImplicit): Async[T] = {
		requestAdd(new ModuleRequest[T].task(code))
	}
	protected[this] final def serveAsync[T](code: => Future[T])(implicit asyncContext: AsyncContext, d: DummyImplicit, d2: DummyImplicit2 = null): Async[T] = {
		requestAdd(new ModuleRequest[T].task(code))
	}
	protected[this] final def serveSyncSilent[T](code: => T): Option[T] = {
		if (isAbleToServeNow) Some(code) else None
	}
	protected[this] final def serveSync[T](code: => T): Option[T] = {
		if (isAbleToServeNow) Some(code)
		else if (isAbleToServe) {
			try {
				requests.synchronized(syncRequests += 1)
				updateHasWork()
				waitStateUpdated()
				if (isAbleToServeNow) Some(code) else None
			}
			finally {
				requests.synchronized(syncRequests -= 1)
				updateHasNoWork()
			}
		}
		else None
	}

	/* internal */
	private[this] def requestAdd[T](request: ModuleRequest[T]): Async[T] = {
		if (not(Constructed)) {// later requestsRestart will be called
			requests += request
			return request
		}
		internal.callRequestAdd(request.getClass)
		request.module = this
		//
		if (request.isDone) requestComplete(request)
		else if (!isAbleToServe) request.cancel(new ModuleServiceException)
		else {
			requests.synchronized(requests += request)
			updateHasWork()
			if (isAbleToServeNow && notUpdating) request.activate()
		}
		request
	}
	private[modules] def requestStart(request: ModuleRequest[_]): Unit = {
		execCount += 1
	}
	private[modules] def requestComplete(request: ModuleRequest[_]): Unit = {
		internal.callRequestRemove(request.getClass)
		requests.synchronized{
			requests -= request
			if (requests.isEmpty) execCount = 0 else execCount -= 1
		}
		updateHasNoWork()
	}
	private[modules] def requestsRestart(): Unit = requests synchronized {
		val rs = requests.toList
		requests.clear()
		rs.foreach(requestAdd(_))
	}
	private[modules] def pauseRequests(): Unit = requests.synchronized(requests.foreach(_.deactivate()))
	private[modules] def resumeRequests(): Unit = requests.synchronized(requests.foreach(_.activate()))
}




/* STATE PROCESSOR */
trait StateProcessor {
	self: Module =>
	import Module._
	import ModuleState._
	import StateParams._
	private[this] var state: ModuleState = CONSTRUCTING
	private[this] var error: Throwable = null
	private[this] var completeOption: CompleteOption = _
	private[this] var stateStartTime = 0L
	private[this] val updateLock = new Object
	private[this] var updateFlag = uFlagNONE
	private[this] var expectUpdate = false
	//
	private[this] var params = 1 << Creating.id | 1 << Detached.id | 1 << AllServersOperating.id

	protected[this] final def setFailed(e: Throwable, recoverable: Boolean = true): Unit = fail(e, recoverable)
	protected[this] final def setRestful(on: Boolean): Unit = set(Restful, on)
	protected[this] final def suspendService(on: Boolean): Unit = if (isSuspended != on) {
		set(Suspended, on, true)
		if (state == OPERATING) on match {
			case true => pauseRequests(); internal.callAbleToServeNow(false)
			case _ => resumeRequests(); internal.callAbleToServeNow(true)
		}
		updateHasWork() || updateHasNoWork()
	}
	protected[this] final def recoverable: Boolean = not(Irrecoverable)
	protected[this] final def recover(): Boolean = state == FAILED && not(Detached) && recoverable match {
		case true ⇒ var serversOk = true
			syncServers.foreach(s ⇒ if (s.isFailed) {s.recover_intr(); serversOk = false})
			if (serversOk) on_!!!(Recover)
			true
		case _ ⇒ false
	}
	private[StateProcessor] def recover_intr(): Boolean = recover()
	private[StateProcessor] def markRestored(): Unit = on_(Restored)

	final def isAbleToServeNow: Boolean = state == OPERATING && not(Suspended)
	final def isAbleToServe: Boolean = state != FAILED && not(Detached)
	// INFO
	protected[modules] final def getState: ModuleState = state
	protected[modules] final def getParams: Int = params
	protected[modules] final def isPreparing: Boolean = state == PREPARING
	protected[modules] final def isOperating: Boolean = state == OPERATING
	protected[modules] final def isActivating: Boolean = state == ACTIVATING
	protected[modules] final def isDeactivating: Boolean = state == DEACTIVATING
	protected[modules] final def isResting: Boolean = state == RESTING
	protected[modules] final def isFailed: Boolean = state == FAILED
	protected[modules] final def isDetached: Boolean = is(Detached)
	protected[modules] final def isConstructed: Boolean = is(Constructed)
	protected[modules] final def isPrepared: Boolean = is(Prepared)
	protected[modules] final def isSuspended: Boolean = is(Suspended)
	protected[modules] final def isRestful: Boolean = is(Restful)
	protected[modules] final def failure: Option[Throwable] = Option(error)
	protected[modules] final def isBoundToSystem: Boolean = is(BoundBySystem)
	private[modules] final def isExternal: Boolean = is(External)
	protected[this] final def isRestored: Boolean = is(Restored)

	/* internal */

	private[this] def set(param: StateParams, on: Boolean, silent: Boolean = false): Unit = setParam(param, on, silent)
	private[modules] def on_!!!(param: StateParams): Unit = setParam(param, true, false)
	private[modules] def on_(param: StateParams): Unit = setParam(param, true, true)
	private[modules] def off_!!!(param: StateParams): Unit = setParam(param, false, false)
	private[modules] def off_(param: StateParams): Unit = setParam(param, false, true)
	private[modules] def is(param: StateParams): Boolean = updateLock synchronized {
		(params & (1 << param.id)) != 0
	}
	private[modules] def not(param: StateParams): Boolean = updateLock synchronized{
		(params & (1 << param.id)) == 0
	}

	private[this] def setParam(param: StateParams, isOn: Boolean, silent: Boolean): Unit = {
		val prev = params
		updateLock synchronized {
			if (isOn) params |= (1 << param.id) else params &= ~(1 << param.id)
		}
		logV(s"[${getClass.getSimpleName}]:  [${(0 until StateParams.values.size).map(n ⇒ if ((params & (1 << n)) == 0) 0 else 1).mkString("")}];  $param= $isOn;  ${if (prev != params && !silent) ">>> " else if (prev != params) "V" else "-"}", logTagParam)
		if (prev != params && !silent) updateState(param.id)
	}

	private[modules] def updateState(cause: Int): Unit = {
		val t0: Long = debug {// TODO WARN: call disableDebugCode before release
			stats(cause * 3) += 1
			System.nanoTime()
		}
		// EXEC
		if (nonBusy) {
			update()
//			if (hasFlag(uFlagRESUME)) logV(s"[${getClass.getSimpleName}]:  UPDATE RESUME")
			if (touched) updateState(RepeatUpdate.id)
		}
		// DEFs
		def update(): Unit = {
			if (expectUpdate) {sys.cancelUpdate(); expectUpdate = false}
			val prevState = state
			state match {
				case CONSTRUCTING ⇒ if (destroy_?) destroy_>> else if (isConstructed) constructed_>>
				case PREPARING => if (destroy_?) destroy_>> else if (isPrepared) prepared_>>
				case RESTING => if (destroy_?) destroy_>> else if (activate_?) activate_>>
				case ACTIVATING => if (activated_?) operate_>>
				case OPERATING => if (deactivate_?) deactivate_>>
				case DEACTIVATING => if (complete_?) rest_>>
				case FAILED => if (destroy_?) destroy_>> else if (is(Recover)) recover_>>
				case DESTROYED | _ =>
			}
			if (prevState != state) {
				stateStartTime = System.currentTimeMillis
				if (state != DESTROYED) setFlag(uFlagTOUCHED)
				debug(stats(cause * 3 + 1) += 1)
			}
			else logW(s"[${getClass.getSimpleName}]:  $prevState -->  X", logTagStateX)
		}
		def nonBusy = updateLock.synchronized {
			updateFlag == uFlagNONE || hasFlag(uFlagREPEAT) match {
				case true ⇒ unsetFlag(uFlagREPEAT); setFlag(uFlagBUSY); true
				case false ⇒ setFlag(uFlagTOUCHED); false
			}
		}
		def touched = updateLock.synchronized {
			val yep = hasFlag(uFlagTOUCHED) match {
				case true ⇒ unsetFlag(uFlagTOUCHED); setFlag(uFlagREPEAT); true
				case _ ⇒ if (hasFlag(uFlagRESUME) && isAbleToServeNow) resumeRequests()
					updateFlag = uFlagNONE
					updateLock.notifyAll()
					false
			}
			debug {
				stats(cause * 3 + 2) += System.nanoTime - t0
//			logV(s"cause: $cause;  time= ${System.nanoTime - t0};  value= ${stats(cause * 3 + 2)}")
				if (state == DESTROYED) logV(s"[${getClass.getName}]: STATS\n$getStats", logTagStats)
			}
			yep
		}
		def hasFlag(f: Int) = (updateFlag & (1 << f)) != 0
		def setFlag(f: Int) = updateFlag |= 1 << f
		def unsetFlag(f: Int) = updateFlag &= ~(1 << f)
	}

	private[modules] def waitStateUpdated(): Unit = updateLock.synchronized {
		if (updateFlag != uFlagNONE) updateLock.wait()
	}
	private[modules] def notUpdating: Boolean = updateLock.synchronized {
		updateFlag == uFlagNONE || {
			updateFlag |= 1 << uFlagRESUME
			false
		}
	}

	private[this] def setState(v: ModuleState): Unit = {
		logW(s"[${getClass.getSimpleName}]:  $state -->  $v", logTagState)
		state = v
	}

	/* transitions */
	private[modules] def attach(): Unit = {
		if (isInstanceOf[SystemModule]) {
			args = new ConstructArgs
			state = RESTING
		}
		else {
			ModuleSystem.attach(this)
			if (args != null) sys = args.system
			else {
				sys = system
				on_(External)
				if (sys != null) {
					val m = sys.bind[this.type](getClass.asInstanceOf[Class[this.type]], null, false, false, () ⇒ this)
					if (m != this) fail(new ModuleCreateException(getClass, s"Another instance is already registered in system."), false)
				}
				else {
					val cause = if (!ModuleSystem.initialized) s"It's created before system."
					else s"It should be bound by the system. Or it's 'system' method should return host system."
					fail(new ModuleCreateException(getClass, cause), false)
				}
				if (args == null) args = new ConstructArgs
				args.server = this
				// WARN: workaround of SI-4330 in DelayedInit to have onConstructed called even if constructor body is empty.
				args.postOnConstructed()
			}
		}
	}
	private[modules] def onAttached(a: ConstructArgs): Unit = {
		args = a
		off_(Detached)
		if (args.restoring) markRestored()
	}
	override final def delayedInit(body: ⇒ Unit): Unit = {
		if (is(External)) args.cancelPostOnConstructed()
		if (args.nonFailed) try body catch {case e: Throwable ⇒ fail(e, false); args.nonFailed = false}
		if (is(External) && not(Constructed)) {
			if ((body _).getClass.getDeclaringClass == getClass) onConstructed()
			else args.postOnConstructed()
		}
	}
	private[modules] def onConstructed(): Unit = if (not(Constructed)) {
		on_(Constructed)
		internal.callConstructed()
		if (isFailed) lifeCycle.callFailed()
		if (not(Detached)) sys.constructed(this, args)
		args.clean()
		args = null
		requestsRestart()
		updateState(Constructed.id)
	}
	private[modules] def onPrepared(): Unit = {
		internal.callPrepared()
		on_!!!(Prepared)
	}

	private[this] def constructed_>> : Unit = {
		setState(PREPARING)
		sys.prepare(this)
	}
	private[this] def prepared_>> : Unit = {
		setState(RESTING)
	}
	private[this] def activate_? : Boolean = changeActive(isUnbound || not(Restful) || hasWork || is(HasClientActive)) && canActivate
	private[this] def canActivate: Boolean = is(AllServersOperating)
	private[this] def activate_>> : Unit = tryOrFail {
		setState(ACTIVATING)
		startProgress()
	}
	private[this] def startProgress(): Unit = {
		completeOption = lifeCycle.callStart()
		if (completeOption == CompleteNow) on_(Complete)
	}
	private[this] def activated_? : Boolean = canActivate && complete_?
	private[this] def complete_? : Boolean = completeOption match {
		case CompleteWhen(isComplete) => tryOrFail(isComplete() || {postUpdateState(lifeCycle.backoff); false})
		case _ if is(Complete) => off_(Complete); true
		case _ => false
	}
	private[this] def operate_>> : Unit = tryOrFail {
		lifeCycle.callComplete()
		if (is(Creating)) off_(Creating)
		setState(OPERATING)
		syncClients.foreach(_.serverOperating(true))
		if (not(Suspended)) {
			resumeRequests()
			internal.callAbleToServeNow(true)
		}
	}
	private[this] def deactivate_? : Boolean = canDeactivate match {
		case true if is(Delayed) || is(Suspended) || lifeCycle.latency == 0 => off_(Delayed); isBound || detach
		case true => on_(Delayed); postUpdateState(lifeCycle.latency); false
		case _ => off_(Delayed); false
	}
	private[this] def canDeactivate: Boolean = hasNoWork && (isUnbound || (is(Restful) && not(HasClientActive)))
	private[this] def detach: Boolean = is(Detached) || (sys.detach(this) match {
		case true ⇒ on_(Detached); true
		case _ ⇒ false
	})
	private[this] def deactivate_>> : Unit = tryOrFail {
		setState(DEACTIVATING)
		syncClients.foreach(_.serverOperating(false))
		startProgress()
		if (not(Suspended)) internal.callAbleToServeNow(false)
		if (is(Detached)) cancelRequests()
	}
	private[this] def rest_>> : Unit = tryOrFail {
		setState(RESTING)
		lifeCycle.callComplete()
		changeActive(false)
		asyncContext.stop(true)
	}
	private[this] def recover_>> : Unit = {
		setState(RESTING)
		on_(Creating)
		off_(Recover)
		error = null
		syncClients.foreach(_.recover_intr())
	}
	private[modules] def tryOrFail[T](code: => T): T = try code catch {
		case err: Throwable => fail(err); null.asInstanceOf[T]
	}
	private[modules] def tryOrDie[T](code: => T): T = try code catch {
		case err: Throwable => fail(err, false); null.asInstanceOf[T]
	}
	private[modules] def fail(err: Throwable, recoverable: Boolean = true): Unit = {
		logE(err, s"Module [${getClass.getName}] is ${if (recoverable) "" else "irrecoverably"} failed in [$state] state. ")
		if (error == null && state != DESTROYED) {
			if (!recoverable) on_(Irrecoverable)
			setState(FAILED)
			error = err
			if (isConstructed) {
				changeActive(false)
				lifeCycle.callFailed()
				cancelRequests()
				if (asyncContext != null) asyncContext.stop()
				syncClients.foreach(_.fail(new SyncServerException(this)))
				internal.callAbleToServeNow(false)
			}
		}
	}
	private[this] def destroy_? : Boolean = is(Constructed) && (is(Detached) || ((isFailed || is(Creating)) && isUnbound && hasNoWork && detach))
	private[this] def destroy_>> : Unit = {
		setState(DESTROYED)
		internal.callDestroyed()
		cancelRequests()
		if (asyncContext != null) asyncContext.exit()
		if (sys != null) sys.destroyed(this)
	}

	private[modules] def serverOperating(on: Boolean): Unit = {
		if (!on) off_(AllServersOperating) else if (syncServers.forall(_.isOperating)) on_!!!(AllServersOperating)
	}
	private[modules] def clientActive(isOn: Boolean): Unit = if (is(HasClientActive) != isOn) {
		if (isOn || syncClients.forall(_.not(Active))) set(HasClientActive, isOn)
	}
	private[this] def changeActive(isOn: Boolean): Boolean = is(Active) != isOn match {
		case true => set(Active, isOn, true); syncServers.foreach(_.clientActive(isOn)); isOn
		case _ => isOn
	}

	private[modules] def postUpdateState(delay: Long): Unit = {
		if (expectUpdate) sys.cancelUpdate() else expectUpdate = true
		sys.postUpdate(delay)
	}
	private[modules] def progressDuration: Long = System.currentTimeMillis - stateStartTime
	private[modules] def calcBackoff: Long = {
		val durationMs = progressDuration
		val delay = if (durationMs < 2000) 200
		else if (durationMs < 10000) 1000
		else if (durationMs < 60000) 5000
		else 10000
		if (isActivating) delay else (delay * 1.5).toLong
	}
	private[this] lazy val stats = mutable.ArrayBuffer[Long]().padTo(StateParams.values.size * 3, 0L)
	private[this] def getStats: String = debug {
		val buf = new StringBuilder
		var tnum = 0L
		var toks = 0L
		var ttime = 0L
		for (n ← 0 until StateParams.values.size) {
			val num = stats(n * 3)
			val oks = stats(n * 3 + 1)
			val time = stats(n * 3 + 2) / 1000000
			tnum += num
			toks += oks
			ttime += time
			val name = StateParams(n).toString
			if (num > 0) buf ++= oks.toString ++= " of " ++= num.toString ++= ":  " ++= time.toString ++= " ms:  " ++= name ++= "\n"
		}
		buf ++= toks.toString ++= " of " ++= tnum.toString ++= ":  " ++= ttime.toString ++= " ms:  " ++= "TOTAL"
		buf.toString()
	}
}


/* STATE PARAMS */
private[modules] object StateParams extends Enumeration {
	type StateParams = Value
	/** Up to 32 params */
	val HasBindings,// (!!!) has clients bound
	HasWork,// (!!!) (has requests and not suspended) or (suspended and currently has executing request)
	Restful,// (!!!) mode allowing go to REST if has no work, and go to OPERATING if has
	Suspended,// for some reason can not currently execute requests
	HasClientActive,// (!!!) if any of sync client is active -> should activate; all sync clients rest -> can rest
	AllServersOperating,// (!!!) if all sync servers active -> can activate
	Active, // derivative from above params
	Creating, // indicates first lap from creating until OPERATING
	Detached, // detached from system, indicates last lap to DESTROYED
	Constructed, // instance constructed
	Prepared, // (!!!) instance prepared
	Complete, // (!!!) transition: ACTIVATING > OPERATING or DEACTIVATING > RESTING
	Delayed, // final step in delayed deactivation
	Recover, // (!!!) recovering after FAILED
	Irrecoverable,// failed with irrecoverable error
	BoundBySystem,// indicates bound by system
	RepeatUpdate,// (!!!) just cause helper for update
	PostedUpdate,// (!!!) just cause helper for update
	Restored,// module is constructed as restored after crash
	External// indicates that module is constructed outside system
	= Value
}



/* RESTORABLE MODULE */
trait RestorableModule extends Module




/* PROGRESS OPTIONS  */

object CompleteSelector extends CompleteSelector

class CompleteSelector {
	def now: CompleteOption = CompleteNow
	def manually: CompleteOption = CompleteManually
	def when(isComplete: => Boolean): CompleteOption = CompleteWhen(() => isComplete)
}

class CompleteOption
object CompleteNow extends CompleteOption
object CompleteManually extends CompleteOption
case class CompleteWhen(isComplete: () => Boolean) extends CompleteOption




/* MODULE EVENT */
abstract class ModuleEvent[T <: Module]







/* CONSTRUCT ARGS */
class ConstructArgs(val locker: Thread, val system: ModuleSystem, val serverClas: Class[_], val client: Module, val sync: Boolean, val restoring: Boolean, val isRoot: Boolean) {
	def this() = this(null, null, null, null, false, false, false)
	var server: Module = _
	var nonFailed: Boolean = true
	implicit lazy val asyncKey = new AsyncContextKey
	lazy val async: AsyncContext = if (server.sys == null) new DefaultAsyncContext else server.sys.asyncContext
	lazy val checker: Runnable = () ⇒ server.onConstructed()
	def postOnConstructed(): Unit = async.execute(checker, 10, checker, false)
	def cancelPostOnConstructed(): Unit = async.cancel(checker)
	def clean(): Unit = if (server != null && async != null) async.stop()
}
