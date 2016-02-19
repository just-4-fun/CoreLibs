package just4fun.test.modules.accessors.definitions.pkg0

import just4fun.core.async.{DefaultAsyncContext, AsyncContext}
import just4fun.core.modules.{Module, ModulePreparePromise, ModuleSystem}
import just4fun.test.modules.accessors.definitions.pkg1.ModuleX1
import just4fun.test.modules.accessors.definitions.pkg2.ModuleX2

class SystemX1 extends ModuleSystem {
	/* SYSTEM OVERRIDE */
	override val name: String = "X1"
	override implicit val asyncContext: AsyncContext = new DefaultAsyncContext
	/* callbacks */
	override protected[this] def onSystemStart(): Unit = super.onSystemStart()
	override protected[this] def onSystemStop(): Unit = super.onSystemStop()
	override protected[this] def onModulePrepare(promise: ModulePreparePromise): Unit = super.onModulePrepare(promise)
	override protected[this] def onModuleDestroy(m: Module): Unit = super.onModuleDestroy(m)
	override protected[this] def postStateUpdate(delay: Long)(implicit m: Module): Unit = super.postStateUpdate(delay)
	override protected[this] def cancelStateUpdate(implicit m: Module): Unit = super.cancelStateUpdate
	/* PROTECTED SYSTEM ACCESS */
	this.isSystemStarted
	this.isSystemStopping
	this.hasModule
	this.moduleContent
	this.bind(null)
	this.unbind(null)
	this.sendEventToModules(null)
}



class ModuleX extends Module {
	protected def test(): Unit = {}

	private val m1 = new ModuleX1
	m1.test() // the only place with advantages of protected access
	private val m2 = new ModuleX2
	m2.test() // the only place with advantages of protected access
}
