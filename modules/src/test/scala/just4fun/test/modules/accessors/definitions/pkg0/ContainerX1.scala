package just4fun.test.modules.accessors.definitions.pkg0

import scala.concurrent.Future
import just4fun.core.async.{DefaultAsyncContext, AsyncContext}
import just4fun.core.modules.{ModuleRestoreAgent, Module, ModulePreparePromise, ModuleContainer}
import just4fun.test.modules.accessors.definitions.pkg1.ModuleX1
import just4fun.test.modules.accessors.definitions.pkg2.ModuleX2

class ContainerX1 extends ModuleContainer {
	/* SYSTEM OVERRIDE */
	override val containerId: String = "X1"
	override implicit val asyncContext: AsyncContext = new DefaultAsyncContext
	override protected[this] val internal: InternalUtils = null
	override protected[this] val restoreAgent: ModuleRestoreAgent = null
	/* callbacks */
	override protected[this] def onContainerPopulate(): Unit = super.onContainerPopulate()
	override protected[this] def onContainerEmpty(): Unit = super.onContainerEmpty()
	override protected[this] def onModulePrepare(m: Module): Future[Unit] = super.onModulePrepare(m)
	override protected[this] def onModuleDestroy(m: Module): Unit = super.onModuleDestroy(m)

	/* PROTECTED SYSTEM ACCESS */
	this.bindModule
	this.unbindModule
	this.containsModule
	this.modulesCount
	this.isContainerEmptying
	this.isContainerEmpty
	this.foreachModuleOfType[Module] _
	this.internal.bind(null)
}



class ModuleX extends Module {
	protected def test(): Unit = {}

	private val m1 = new ModuleX1
	m1.test() // the only place with advantages of protected access
	private val m2 = new ModuleX2
	m2.test() // the only place with advantages of protected access
}
