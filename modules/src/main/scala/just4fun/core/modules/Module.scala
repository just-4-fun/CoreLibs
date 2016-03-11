
package just4fun.core.modules

import just4fun.core.async.Async.DummyImplicit2
import just4fun.core.async._
import just4fun.core.debug.DebugConfig
import just4fun.core.debug.DebugUtils._
import scala.collection.mutable
import scala.concurrent.Future
import scala.language.experimental.macros
import scala.util.{Failure, Try}


/* STATES */
object ModulePhase extends Enumeration {
	type ModulePhase = Value
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
	val logTagStat = 0x7102114
}

/* MODULE */
/* TODO: track wasFinishedOk via abstract Prefs */
/** LifeCycleCallbacks and ExtraCallbacks are guaranteed to first call not earlier than instance is constructed.  */
/** STAT: Activated Instance: 0.5 ms;  3.7 Kb (10); 3.2 Kb (100); 1.4 Kb (1000) */
trait Module extends StateProcessor with BindingProcessor with RequestProcessor with AsyncContextOwner {
//todo private[this] val _dc = disableDebugCode()
	import ModulePhase._
	import StateParams._
	protected[this] implicit final val thisModule: this.type = this
	val moduleId: String = getClass.getName
	private[modules] val cont: ModuleContainer = ModuleContainer.register(this) match {
		case null ⇒ throw new ModuleCreateException(getClass)
		case c ⇒ c
	}
	implicit lazy val asyncContext: AsyncContext = cont.asyncContext
	lazy val status: StateInfo = new StateInfo
	protected[this] lazy val state: State = new State
	protected[this] lazy val internal: InternalUtils = new InternalUtils


	/* public */
	def context: ModuleContainer = cont

	/* internal */
	private[modules] def onAddBinder(module: Module, sync: Boolean): Unit = ()
	private[modules] def onRemoveBinder(module: Module): Unit = ()
	private[modules] def onRequestAdd(clas: Class[_]): Unit = ()
	private[modules] def onRequestRemove(clas: Class[_]): Unit = ()


	/* LIFECYCLE CALLBACKS */
	class State {
		import ModulePhase._
		import StateParams._
		protected[this] var restLatencyMs = 10000
		protected[this] var destroyLatencyMs = 0
		//
		final def completeActivating(): Unit = if (isActivating) on_!!!(Complete)
		final def completeDeactivating(): Unit = if (isDeactivating) on_!!!(Complete)
		final def setRestful(on: Boolean): Unit = set(Restful, on)
		final def suspend(on: Boolean): Unit = thisModule.suspend(on)
		final def fail(e: Throwable, recoverable: Boolean = true): Unit = thisModule.fail(e, recoverable)
		final def recover(): Boolean = thisModule.recover()
		//callbacks
		protected[this] def onActivatingStart(initialTime: Boolean, complete: CompleteSelector): CompleteOption = complete.now
		protected[this] def onActivatingComplete(initialTime: Boolean): Unit = ()
		//
		protected[this] def onDeactivatingStart(finalTime: Boolean, complete: CompleteSelector): CompleteOption = complete.now
		protected[this] def onDeactivatingComplete(finalTime: Boolean): Unit = ()
		//
		protected[this] def progressBackoff: Long = calcBackoff
		protected[this] final def progressDurationMs: Long = progressDuration
		//
		protected[this] def onFailed(error: Throwable, phaseFailed: ModulePhase): Unit = ()
		//
		protected[this] def onAbleToServeNow(yep: Boolean): Unit = {}
		protected[this] def onUnbound(): Unit = ()
		//
		protected[this] def onConstructed(): Unit = ()
		protected[this] def onDestroyed(): Unit = ()
		// BACK DOOR
		private[modules] def callFailedEnsured(phase: ModulePhase): Boolean = {
			try onFailed(failure.get, phase) catch loggedE
			isFailed// can be immediately recovered
		}
		private[modules] def callStartProgress(): CompleteOption = {
			if (isActivating) onActivatingStart(is(Creating), CompleteSelector) else onDeactivatingStart(is(Destroying), CompleteSelector)
		}
		private[modules] def callComplete(): Unit = {
			if (isActivating) onActivatingComplete(is(Creating)) else onDeactivatingComplete(is(Destroying))
		}
		private[modules] def latency = if (isUnbound) destroyLatencyMs else restLatencyMs
		private[modules] def backoff: Long = try progressBackoff catch loggedE(calcBackoff)
		private[modules] def callAbleToServeNow(yep: Boolean) = tryOrDie(onAbleToServeNow(yep))
		private[modules] def callUnbound(): Unit = tryOrDie(onUnbound())
		private[modules] def callConstructed() = tryOrDie(onConstructed())
		private[modules] def callDestroyed() = try onDestroyed() catch loggedE
	}


	/* PUBLIC INFO */
	class StateInfo {
		import StateParams._
		final def phase: ModulePhase = getPhase
		final def params: Int = getParams
		final def canServeNow: Boolean = getPhase == OPERATING && not(Suspended)
		final def canServe: Boolean = getPhase != FAILED && not(Destroying)
		final def bound: Boolean = isBound
		final def boundBy[M <: Module : Manifest]: Boolean = isBoundBy[M]
		final def boundByContainer: Boolean = is(BoundByContainer)
		final def hasRequests: Boolean = thisModule.hasRequests
		final def constructed: Boolean = is(Constructed)
		final def prepared: Boolean = is(Prepared)
		final def resting: Boolean = getPhase == RESTING
		final def activating: Boolean = getPhase == ACTIVATING
		final def operating: Boolean = getPhase == OPERATING
		final def deactivating: Boolean = getPhase == DEACTIVATING
		final def destroying: Boolean = is(Destroying)
		final def destroyed: Boolean = getPhase == DESTROYED
		final def failed: Boolean = getPhase == FAILED
		final def recoverable: Boolean = not(Irrecoverable)
		final def suspended: Boolean = is(Suspended)
		final def restored: Boolean = is(Restored)
		final def restful: Boolean = is(Restful)
		final def failure: Option[Throwable] = thisModule.failure
		override def toString: String = {
			val buff = new StringBuilder
			buff ++= "phase: " ++= phase.toString ++= ";  bindings: " ++= bindersCount.toString ++= ";  requests: " ++= requestsNum.toString ++= ";  params: "
			StateParams.values.foreach { param ⇒
				if (param.id > 0) buff ++= ", "
				buff ++= param.toString ++= ":"
				if ((getParams & (1 << param.id)) == 0) buff ++= "0" else buff ++= "1"
			}
			buff.toString()
		}
	}

	/* INTERNAL UTILS */
	class InternalUtils {
		final def bind[M <: Module](clas: Class[M], sync: Boolean): M = {
			if (not(Destroying)) cont.bind(clas, thisModule, sync, false, null) else null.asInstanceOf[M]// todo throw ?
		}
		final def unbind[M <: Module](clas: Class[M]): Unit = {
			if (not(Destroying)) cont.unbind(clas, thisModule)
		}
	}

}




/* BINDING PROCESSOR */
trait BindingProcessor {
	self: Module =>
	import StateParams._
	private[this] val binders = mutable.Set[Module]()
	private[modules] val syncBinders = mutable.Set[Module]()
	private[modules] val syncBounds = mutable.Set[Module]()

	protected[this] final def bind[M <: Module : Manifest]: M = macro Macros.bind[M]
	protected[this] final def bindWithSync[M <: Module : Manifest]: M = macro Macros.bindS[M]
	protected[this] final def unbind[M <: Module : Manifest]: Unit = {// TODO macros: explicit M
		internal.unbind(implicitly[Manifest[M]].runtimeClass.asInstanceOf[Class[M]])
	}
	protected[this] final def unbind[M <: Module : Manifest](m: M): Unit = {
		internal.unbind(m.getClass.asInstanceOf[Class[M]])
	}
	protected[this] final def unbindAll(): Unit = cont.unbindAll(this)
	protected[this] def foreachBinderOfType[T <: Module : Manifest](exec: T ⇒ Unit): Unit = binders.foreach {
		case b: T ⇒ b.tryOrDie(exec(b))
		case _ ⇒
	}

	/* internal */
	private[modules] def isBound: Boolean = binders.nonEmpty || is(BoundByContainer)
	private[modules] def isUnbound: Boolean = binders.isEmpty && not(BoundByContainer)
	private[modules] def isBoundBy[M <: Module](implicit m: Manifest[M]): Boolean = {
		val clas = cont.moduleClass(m)
		binders.exists(_.getClass == clas)
	}
	private[modules] def isBoundByContainer: Boolean = is(BoundByContainer)
	private[modules] def bindersCount: Int = binders.size + (if (is(BoundByContainer)) 1 else 0)

	private[modules] def addBinder(binder: Module, sync: Boolean): Unit = {
		val isRoot = binder == null
		val added = if (isRoot) on_(BoundByContainer) else binders.add(binder)
		if (added) {
			debug(onAddBinder(binder, sync))
			on_!!!(HasBinders)
			if (!isRoot) {
				if (sync) addSyncBinder(binder)
				if (DebugConfig.isDebug) scanCyclicBindings(this, binder)
			}
		}
		else if (!isRoot) if (sync) addSyncBinder(binder) else removeSyncBinder(binder)
	}
	private[modules] def removeBinder(binder: Module): Unit = {
		val isRoot = binder == null
		val removed = if (isRoot) off_(BoundByContainer) else binders.remove(binder)
		if (removed) {
			debug(onRemoveBinder(binder))
			if (!isRoot) removeSyncBinder(binder)
			if (isUnbound) {
				state.callUnbound()
				off_!!!(HasBinders)
			}
		}
	}

	private[this] def addSyncBinder(binder: Module): Unit = if (syncBinders.add(binder)) {
		binder.addSyncBound(this)
		if (isFailed) binder.fail(new SyncBindingException(this))
		else binderActive(binder.is(Active))
	}
	private[this] def removeSyncBinder(binder: Module): Boolean = syncBinders.remove(binder) match {
		case true => binder.removeSyncBound(this); binderActive(false); true
		case false => false
	}
	private[BindingProcessor] def addSyncBound(bound: Module): Unit = {
		if (syncBounds.add(bound) && !bound.isOperating) boundOperating(false)
	}
	private[BindingProcessor] def removeSyncBound(bound: Module): Unit = {
		if (syncBounds.remove(bound)) boundOperating(true)
	}

	private[this] def scanCyclicBindings(bound: Module, binder: Module): Unit = {
		binder.detectCyclicBinding(bound, binder :: Nil) match {
			case Nil =>
			case trace => logE(s"Cyclic module relations detected in chain [${trace.map(_.getClass.getSimpleName).mkString(", ")}]. Consider using 'foreachBinderOfType' method for communication with binders.")
		}
	}
	private[BindingProcessor] def detectCyclicBinding(bound: Module, trace: List[Module]): List[Module] = {
		if (this != bound) binders.foreach { binder =>
			val res = if (this == binder) Nil else if (bound == binder) binder :: trace else binder.detectCyclicBinding(bound, binder :: trace)
			if (res.nonEmpty) return res
		}
		Nil
	}
}




/* REQUEST PROCESSOR */
trait RequestProcessor {
	self: Module =>
	import StateParams._
	private[this] val requests = mutable.ListBuffer[ModuleRequest[_]]()
	private[this] var syncRequests: Int = 0
	private[this] var execCount: Int = 0

	private[modules] def hasNoRequests: Boolean = requests.synchronized(requests.isEmpty && syncRequests <= 0)
	private[modules] def hasRequests: Boolean = !hasNoRequests
	private[modules] def hasNoWork: Boolean = requests.synchronized(hasNoRequests || (is(Suspended) && execCount <= 0))
	private[modules] def hasWork: Boolean = !hasNoWork
	private[modules] def updateHasWork(): Boolean = {
		val ok = requests.synchronized {if (not(HasWork) && hasWork) {on_(HasWork); true} else false}
		if (ok) updateState(HasWork.id)
		ok
	}
	private[modules] def updateHasNoWork(): Boolean = {
		val ok = requests.synchronized {if (is(HasWork) && hasNoWork) {off_(HasWork); true} else false}
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
	protected[this] final def serveSync[T](code: => T): Option[T] = {
		if (status.canServeNow) Some(code)
		else if (status.canServe) {
			try {
				requests.synchronized(syncRequests += 1)
				updateHasWork()
				waitStateUpdated()
				if (status.canServeNow) Some(code) else None
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
		if (!request.isDone) {
			debug(onRequestAdd(request.getClass))
			request.module = this
			if (!status.canServe) request.cancel(new ModuleServiceException)
			else {
				requests.synchronized(requests += request)
				updateHasWork()
				if (status.canServeNow && notUpdating) request.activate()
			}
		}
		request
	}
	private[modules] def requestStart(request: ModuleRequest[_]): Unit = requests.synchronized {
		execCount += 1
	}
	private[modules] def requestComplete(request: ModuleRequest[_]): Unit = {
		debug(onRequestRemove(request.getClass))
		requests.synchronized {
			requests -= request
			if (requests.isEmpty) execCount = 0 else execCount -= 1
		}
		updateHasNoWork()
	}
	private[modules] def pauseRequests(): Unit = requests.synchronized(requests.foreach(_.deactivate()))
	private[modules] def resumeRequests(): Unit = requests.synchronized(requests.foreach(_.activate()))
}




/* STATE PROCESSOR */
trait StateProcessor {
	self: Module =>
	import Module._
	import ModulePhase._
	import StateParams._
	private[this] var phase: ModulePhase = CONSTRUCTING
	private[this] var params = 1 << Creating.id | 1 << AllBoundsOperating.id
	private[this] var error: Throwable = null
	private[this] var completeOption: CompleteOption = _
	private[this] var phaseStartTime = 0L
	private[this] val updateLock = new Object
	private[this] var updateFlag = uFlagNONE
	private[this] var expectUpdate = false

	private[modules] final def suspend(on: Boolean): Unit = if (is(Suspended) != on) {
		set(Suspended, on, true)
		if (phase == OPERATING) on match {
			case true => pauseRequests(); state.callAbleToServeNow(false)
			case _ => resumeRequests(); state.callAbleToServeNow(true)
		}
		updateHasWork() || updateHasNoWork()
	}
	private[modules] final def recover(): Boolean = {
		if (phase == FAILED && is(Constructed) && not(Destroying) && not(Irrecoverable)) {
			var boundsOk = true
			syncBounds.foreach(s ⇒ if (s.isFailed) {s.recover(); boundsOk = false})
			if (boundsOk) on_!!!(Recover)
			true
		} else false
	}
	private[modules] def setRestored(): Unit = on_(Restored)

	// INFO
	private[modules] def getPhase: ModulePhase = phase
	private[modules] def getParams: Int = params
	private[modules] def isActivating: Boolean = phase == ACTIVATING
	private[modules] def isOperating: Boolean = phase == OPERATING
	private[modules] def isDeactivating: Boolean = phase == DEACTIVATING
	private[modules] def isFailed: Boolean = phase == FAILED
	private[modules] def failure: Option[Throwable] = Option(error)

	/* internal */

	private[modules] def set(param: StateParams, on: Boolean, silent: Boolean = false): Boolean = setParam(param, on, silent)
	private[modules] def on_!!!(param: StateParams): Boolean = setParam(param, true, false)
	private[modules] def on_(param: StateParams): Boolean = setParam(param, true, true)
	private[modules] def off_!!!(param: StateParams): Boolean = setParam(param, false, false)
	private[modules] def off_(param: StateParams): Boolean = setParam(param, false, true)
	private[modules] def is(param: StateParams): Boolean = updateLock synchronized ((params & (1 << param.id)) != 0)
	private[modules] def not(param: StateParams): Boolean = updateLock synchronized ((params & (1 << param.id)) == 0)

	private[this] def setParam(param: StateParams, isOn: Boolean, silent: Boolean): Boolean = {
		val prev = params
		updateLock synchronized {
			if (isOn) params |= (1 << param.id) else params &= ~(1 << param.id)
		}
		logV(s"[$moduleId]:  [${(0 until StateParams.values.size).map(n ⇒ if ((params & (1 << n)) == 0) 0 else 1).mkString("")}];  $param= $isOn;  ${if (prev != params && !silent) ">>> " else if (prev != params) "V" else "-"}", logTagParam)
		if (prev != params) {
			if (!silent) updateState(param.id)
			true
		}
		else false
	}

	private[modules] def updateState(cause: Int): Unit = {
		val t0: Long = debug {// TODO WARN: call disableDebugCode before release
			stats(cause * 3) += 1
			System.nanoTime()
		}
		// EXEC
		if (nonBusy) {
			update()
//			if (hasFlag(uFlagRESUME)) logV(s"[$moduleId]:  UPDATE RESUME")
			if (touched) updateState(RepeatUpdate.id)
		}
		// DEFs
		def update(): Unit = {
			if (expectUpdate) {cont.cancelStateUpdate(this); expectUpdate = false}
			val prevPhase = phase
			phase match {
				case CONSTRUCTING ⇒ if (is(Constructed)) constructed_>>
				case PREPARING => if (destroy_?) destroy_>> else if (is(Prepared)) prepared_>>
				case RESTING => if (destroy_?) destroy_>> else if (activate_?) activate_>>
				case ACTIVATING => if (activated_?) operate_>>
				case OPERATING => if (deactivate_?) deactivate_>>
				case DEACTIVATING => if (complete_?) rest_>>
				case FAILED => if (destroy_?) destroy_>> else if (is(Recover)) recover_>>
				case DESTROYED => cont.destroyed(this)
			}
			if (prevPhase != phase) {
				phaseStartTime = System.currentTimeMillis
				setFlag(uFlagTOUCHED)
				debug(stats(cause * 3 + 1) += 1)
			}
			else logW(s"[$moduleId]:  $prevPhase -->  X", logTagStateX)
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
				case _ ⇒ if (hasFlag(uFlagRESUME) && status.canServeNow) resumeRequests()
					updateFlag = uFlagNONE
					updateLock.notifyAll()
					false
			}
			debug {
				stats(cause * 3 + 2) += System.nanoTime - t0
//			logV(s"cause: $cause;  time= ${System.nanoTime - t0};  value= ${stats(cause * 3 + 2)}")
				if (phase == DESTROYED && yep) logV(s"[$moduleId]: STATS\n$getStats", logTagStat)
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

	private[this] def setPhase(v: ModulePhase): Unit = {
		logW(s"[$moduleId]:  $phase -->  $v", logTagState)
		phase = v
	}

	/* transitions */

	private[modules] def setConstructed(): Unit = {
		state.callConstructed()
		if (isFailed) state.callFailedEnsured(CONSTRUCTING)
		on_!!!(Constructed)
	}
	private[this] def constructed_>> : Unit = {
		setPhase(PREPARING)
		cont.prepare(this)
	}
	private[modules] def setPrepared(): Unit = on_!!!(Prepared)
	private[this] def prepared_>> : Unit = {
		setPhase(RESTING)
	}
	private[this] def activate_? : Boolean = changeActive(isUnbound || not(Restful) || hasWork || is(HasBinderActive)) && canActivate
	private[this] def canActivate: Boolean = is(AllBoundsOperating)
	private[this] def activate_>> : Unit = tryOrFail {
		setPhase(ACTIVATING)
		startProgress()
	}
	private[this] def startProgress(): Unit = {
		off_(Complete)
		completeOption = state.callStartProgress()
	}
	private[this] def activated_? : Boolean = canActivate && complete_?
	private[this] def complete_? : Boolean = {
		if (is(Complete)) {off_(Complete); completeOption = null}
		completeOption match {
			case CompleteWhen(isComplete) ⇒ tryOrFail(isComplete()) match {
				case true ⇒ completeOption = null
				case _ ⇒ if (!isFailed) postUpdateState(state.backoff)
			}
			case _ ⇒
		}
		completeOption == null
	}
	private[this] def operate_>> : Unit = tryOrFail {
		state.callComplete()
		if (is(Creating)) off_(Creating)
		setPhase(OPERATING)
		syncBinders.foreach(_.boundOperating(true))
		if (not(Suspended)) {
			resumeRequests()
			state.callAbleToServeNow(true)
		}
	}
	private[this] def deactivate_? : Boolean = canDeactivate match {
		case true if is(Delayed) || is(Suspended) || state.latency == 0 => off_(Delayed); isBound || unregister
		case true => on_(Delayed); postUpdateState(state.latency); false
		case _ => off_(Delayed); false
	}
	private[this] def canDeactivate: Boolean = hasNoWork && (isUnbound || (is(Restful) && not(HasBinderActive)))
	private[this] def unregister: Boolean = is(Destroying) || (cont.unregister(this) match {
		case true ⇒ on_(Destroying); true
		case _ ⇒ false
	})
	private[this] def deactivate_>> : Unit = tryOrFail {
		setPhase(DEACTIVATING)
		syncBinders.foreach(_.boundOperating(false))
		startProgress()
		if (not(Suspended)) state.callAbleToServeNow(false)
		if (is(Destroying)) cancelRequests()
	}
	private[this] def rest_>> : Unit = tryOrFail {
		setPhase(RESTING)
		state.callComplete()
		changeActive(false)
		asyncContext.stop(true)
	}
	private[this] def recover_>> : Unit = {
		setPhase(if (is(Prepared)) RESTING else CONSTRUCTING)
		on_(Creating)
		off_(Recover)
		error = null
		syncBinders.foreach(_.recover())
	}
	private[modules] def tryOrFail[T](code: => T): T = try code catch {
		case err: Throwable => fail(err); null.asInstanceOf[T]
	}
	private[modules] def tryOrDie[T](code: => T): T = try code catch {
		case err: Throwable => fail(err, false); null.asInstanceOf[T]
	}
	private[modules] def fail(err: Throwable, recoverable: Boolean = true): Unit = {
		logE(err, s"Module [$moduleId] is ${if (recoverable) "" else "irrecoverably"} failed in [$phase] phase. ")
		if (error == null && phase != DESTROYED) {
			error = err
			val prevPhase = phase
			setPhase(FAILED)
			if (!recoverable) on_(Irrecoverable)
			if (is(Constructed) && state.callFailedEnsured(prevPhase)) {
				changeActive(false)
				cancelRequests()
				asyncContext.stop()
				state.callAbleToServeNow(false)
				syncBinders.foreach(_.fail(new SyncBindingException(this)))
			}
		}
	}
	private[this] def destroy_? : Boolean = {
		is(Constructed) && (is(Destroying) || ((isFailed || is(Creating)) && isUnbound && hasNoWork && unregister))
	}
	private[this] def destroy_>> : Unit = {
		setPhase(DESTROYED)
		state.callDestroyed()
		cancelRequests()
		asyncContext.exit()
	}

	private[modules] def boundOperating(on: Boolean): Unit = {
		if (!on) off_(AllBoundsOperating) else if (syncBounds.forall(_.isOperating)) on_!!!(AllBoundsOperating)
	}
	private[modules] def binderActive(isOn: Boolean): Unit = if (is(HasBinderActive) != isOn) {
		if (isOn || syncBinders.forall(_.not(Active))) set(HasBinderActive, isOn)
	}
	private[this] def changeActive(isOn: Boolean): Boolean = is(Active) != isOn match {
		case true => set(Active, isOn, true); syncBounds.foreach(_.binderActive(isOn)); isOn
		case _ => isOn
	}

	private[modules] def postUpdateState(delay: Long): Unit = {
		if (expectUpdate) cont.cancelStateUpdate(this) else expectUpdate = true
		cont.postStateUpdate(delay, this)
	}
	private[modules] def progressDuration: Long = System.currentTimeMillis - phaseStartTime
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
	val HasBinders,// (!!!) has modules that bound this one
	HasWork,// (!!!) (has requests and not suspended) or (suspended and currently has executing request)
	Restful,// (!!!) mode allowing go to REST if has no work, and go to OPERATING if has
	Suspended,// for some reason can not currently execute requests
	HasBinderActive,// (!!!) if any of sync binder is active -> should activate; all sync binders rest -> can rest
	AllBoundsOperating,// (!!!) if all sync bounds active -> can activate
	Active, // derivative from above params
	Creating, // registered; indicates first lap from CONSTRUCTING until OPERATING
	Destroying, // unregistered; indicates last lap from OPERATING until DESTROYED
	Constructed, // instance constructed
	Prepared, // (!!!) instance prepared
	Complete, // (!!!) transition: ACTIVATING > OPERATING or DEACTIVATING > RESTING
	Delayed, // final step in delayed deactivation
	Recover, // (!!!) recovering after FAILED
	Irrecoverable,// failed with irrecoverable error
	BoundByContainer,// indicates bound by Container
	RepeatUpdate,// (!!!) just cause helper for update
	PostedUpdate,// (!!!) just cause helper for update
	Restored// module is constructed as restored after crash
	= Value
}



/* RESTORABLE MODULE MARKER */
trait RestorableModule extends Module




/* PROGRESS OPTIONS  */
object CompleteSelector extends CompleteSelector

class CompleteSelector {
	def now: CompleteOption = null
	def manually: CompleteOption = CompleteManually
	def when(isComplete: => Boolean): CompleteOption = CompleteWhen(() => isComplete)
}

class CompleteOption
object CompleteManually extends CompleteOption
case class CompleteWhen(isComplete: () => Boolean) extends CompleteOption
