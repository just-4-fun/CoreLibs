package just4fun.test.modules.accessors.definitions.pkg1

import just4fun.core.async.{AsyncContextKey, AsyncContext}
import just4fun.core.modules._
import just4fun.test.modules.accessors.definitions.pkg0.ModuleX
import just4fun.test.modules.accessors.definitions.pkg2.ModuleX2

class ModuleX1 extends ModuleX {
	thisModule
	/* MODULE OVERRIDE */
	override implicit lazy val asyncContext: AsyncContext = null
	override protected[this] implicit val asyncContextKey: AsyncContextKey = null
	override lazy val status: StateInfo = new StateInfo
	override def system: ModuleSystem = super.system
	override protected[this] lazy val state: State = new State {
		restLatencyMs = 1000
		destroyLatencyMs = 1000
		override protected[this] def onActivatingStart(initialTime: Boolean, complete: CompleteSelector): CompleteOption = super.onActivatingStart(initialTime, complete)
		override protected[this] def onActivatingComplete(creating: Boolean): Unit = super.onActivatingComplete(creating)
		override protected[this] def onGoingToBeDetached(): Unit = super.onGoingToBeDetached()
		override protected[this] def onDeactivatingStart(finalTime: Boolean, complete: CompleteSelector): CompleteOption = super.onDeactivatingStart(finalTime, complete)
		override protected[this] def onDeactivatingComplete(destroying: Boolean): Unit = super.onDeactivatingComplete(destroying)
		override protected[this] def progressBackoff: Long = super.progressBackoff
		override protected[this] def onFailed(): Unit = super.onFailed()
		override protected[this] def onConstructed(): Unit = super.onConstructed()
		override protected[this] def onDestroyed(): Unit = super.onDestroyed()
		override protected[this] def onAbleToServeNow(yep: Boolean): Unit = super.onAbleToServeNow(yep)
		override protected[this] def onModuleEvent(e: ModuleEvent[_]): Unit = super.onModuleEvent(e)
	}
	/* PROTECTED THIS MODULE ACCESS */
	system

	bind[Module]
	bindSync[Module]
	unbind
	sendEventToClients(null)

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
	status.boundBySystem
	status.hasRequests
	status.constructing
	status.constructed
	status.preparing
	status.prepared
	status.resting
	status.activating
	status.operating
	status.deactivating
	status.detached
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
