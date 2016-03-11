package just4fun.test.modules.accessors.definitions.pkg1

import just4fun.core.async.{AsyncContextKey, AsyncContext}
import just4fun.core.modules.ModulePhase.ModulePhase
import just4fun.core.modules._
import just4fun.test.modules.accessors.definitions.pkg0.ModuleX
import just4fun.test.modules.accessors.definitions.pkg2.ModuleX2

class ModuleX1 extends ModuleX {
	thisModule
	/* MODULE OVERRIDE */
	override implicit lazy val asyncContext: AsyncContext = null
	override protected[this] implicit val asyncContextKey: AsyncContextKey = null
	override lazy val status: StateInfo = new StateInfo
	override def context: ModuleContainer = super.context
	override protected[this] lazy val state: State = new State {
		restLatencyMs = 1000
		destroyLatencyMs = 1000
		override protected[this] def onActivatingStart(initialTime: Boolean, complete: CompleteSelector): CompleteOption = super.onActivatingStart(initialTime, complete)
		override protected[this] def onActivatingComplete(creating: Boolean): Unit = super.onActivatingComplete(creating)
		override protected[this] def onUnbound(): Unit = super.onUnbound()
		override protected[this] def onDeactivatingStart(finalTime: Boolean, complete: CompleteSelector): CompleteOption = super.onDeactivatingStart(finalTime, complete)
		override protected[this] def onDeactivatingComplete(destroying: Boolean): Unit = super.onDeactivatingComplete(destroying)
		override protected[this] def progressBackoff: Long = super.progressBackoff
		override protected[this] def onFailed(error: Throwable, phaseFailed: ModulePhase): Unit = super.onFailed(error, phaseFailed)
		override protected[this] def onConstructed(): Unit = super.onConstructed()
		override protected[this] def onDestroyed(): Unit = super.onDestroyed()
		override protected[this] def onAbleToServeNow(yep: Boolean): Unit = super.onAbleToServeNow(yep)
	}
	/* PROTECTED THIS MODULE ACCESS */
	context
	bind[Module]
	bindWithSync[Module]
	unbind[Module]
	unbindAll()
	foreachBinderOfType[Module] _
	cancelRequests(null)
	serveAsync()
	serveSync()
	serveRequest(null)

	state.setRestful(false)
	state.suspend(false)
	state.recover()
	state.fail(null)
	state.completeActivating()
	state.completeDeactivating()

	status.phase
	status.params
	status.canServe
	status.canServeNow
	status.bound
	status.boundBy(null)
	status.boundByContainer
	status.hasRequests
	status.constructed
	status.prepared
	status.resting
	status.activating
	status.operating
	status.deactivating
	status.destroying
	status.destroyed
	status.failed
	status.recoverable
	status.restful
	status.suspended
	status.failure
	status.toString

	internal.bind(null, false)
	internal.unbind(null)
}
