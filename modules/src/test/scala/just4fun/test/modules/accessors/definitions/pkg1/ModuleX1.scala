package just4fun.test.modules.accessors.definitions.pkg1

import just4fun.core.async.AsyncContext
import just4fun.core.modules.{CompleteOption, CompleteSelector, Module}
import just4fun.test.modules.accessors.definitions.pkg0.ModuleX
import just4fun.test.modules.accessors.definitions.pkg2.ModuleX2

class ModuleX1 extends ModuleX {
	/* MODULE OVERRIDE */
	override implicit lazy val asyncContext: AsyncContext = null

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

	/* PROTECTED THIS MODULE ACCESS */
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
	this.serveSync()
	this.serveSyncSilent()
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
}
