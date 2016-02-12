package just4fun.core.modules.test.syntax

import scala.concurrent.Future
import scala.util.Try
import just4fun.core.async.FutureX.DummyImplicit2
import just4fun.core.async.{FutureContextOwner, FutureX, DefaultFutureContext, FutureContext}
import just4fun.core.modules.ModuleState.ModuleState
import just4fun.core.modules._

class SystemX1 extends ModuleSystem {
	override implicit val asyncContext: FutureContext = new DefaultFutureContext
	/* callbacks */
	override protected[this] def onSystemStart(): Unit = super.onSystemStart()
	override protected[this] def onSystemStop(): Unit = super.onSystemStop()
	override protected[this] def onModulePrepare(promise: ModulePreparePromise): Unit = super.onModulePrepare(promise)
	override protected[this] def onModuleDestroy(m: Module): Unit = super.onModuleDestroy(m)
	override protected[this] def postStateUpdate(delay: Long)(implicit m: Module): Unit = super.postStateUpdate(delay)
	override protected[this] def cancelStateUpdate(implicit m: Module): Unit = super.cancelStateUpdate
	override protected[this] def currentTimeMs: Long = super.currentTimeMs
	//
	this.isSystemStarted
	this.isSystemStopping
	this.hasModule
}

class ModuleX1 extends Module {
	override implicit protected lazy val asyncContext: FutureContext = null

	override protected[this] lazy val lifeCycle: LifeCycleCallbacks = new LifeCycleCallbacks {
		restLatencyMs = 1000
		destroyLatencyMs = 1000
		override protected[this] def onActivatingStart(creating: Boolean, complete: CompleteSelector): CompleteOption = super.onActivatingStart(creating, complete)
		override protected[this] def onActivatingComplete(creating: Boolean): Unit = super.onActivatingComplete(creating)
		override protected[this] def onDeactivatingStart(destroying: Boolean, complete: CompleteSelector): CompleteOption = super.onDeactivatingStart(destroying, complete)
		override protected[this] def onDeactivatingComplete(destroying: Boolean): Unit = super.onDeactivatingComplete(destroying)
		override protected[this] def progressBackoff: Long = super.progressBackoff
		override protected[this] def onFailed(): Unit = super.onFailed()
	}

	override protected[this] lazy val internal: InternalCallbacks = new InternalCallbacks {
		override protected[this] def onConstructed(): Unit = super.onConstructed()
		override protected[this] def onPrepared(): Unit = super.onPrepared()
		override protected[this] def onDestroyed(): Unit = super.onDestroyed()
		override protected[this] def onAbleToServeNow(yep: Boolean): Unit = super.onAbleToServeNow(yep)
		override protected[this] def onBindingAdd(moduleClas: Class[_], sync: Boolean): Unit = super.onBindingAdd(moduleClas, sync)
		override protected[this] def onBindingRemove(moduleClas: Class[_]): Unit = super.onBindingRemove(moduleClas)
		override protected[this] def onRequestAdd(requestClas: Class[_]): Unit = super.onRequestAdd(requestClas)
		override protected[this] def onRequestRemove(requestClas: Class[_]): Unit = super.onRequestRemove(requestClas)
	}

	//
	this.isBound
	this.isUnbound
	this.isBoundTo _
	this.bind[Module]
	this.bindSync[Module]
	this.unbind

	this.sendEventToClients _

	this.hasRequests
	this.hasNoRequests
	this.cancelRequests _

	this.serveAsync()
	this.serveOpt()
	this.serveTry()
	this.serveRequest _
	
	this.getState

	this.isAbleToServe
	this.isAbleToServeNow
	this.isPreparing
	this.isOperating
	this.isActivating
	this.isDeactivating
	this.isResting
	this.isDetached

	this.isRestful
	this.isSuspended
	this.setRestful _
	this.suspendService _

	this.recover
	this.recoverable
	this.setFailed _
	this.failure
	this.isFailed

	sys.hasModule
	sys.isSystemStarted
	sys.isSystemStopping
	sys.asyncContext
}

class ModuleX2 extends Module {
	val mod = new ModuleX1
	mod.isAbleToServe
	mod.isAbleToServeNow
	mod.info.state
	mod.info.isPreparing
	mod.info.isOperating
	mod.info.isActivating
	mod.info.isDeactivating
	mod.info.isResting
	mod.info.isFailed
	mod.info.isUnavailable
	mod.info.isRestful
	mod.info.isSuspended
	mod.info.failure
}
