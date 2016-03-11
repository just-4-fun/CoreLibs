package just4fun.test.modules.accessors

import just4fun.test.modules.accessors.definitions.pkg0.ContainerX1
import just4fun.test.modules.accessors.definitions.pkg1.ModuleX1
import just4fun.test.modules.accessors.definitions.pkg2.ModuleX2


object Test {
	/* PUBLIC SYSTEM ACCESS */
	val sys = new ContainerX1
	sys.containerId
	sys.asyncContext
	sys.isContainerEmpty
	sys.isContainerEmptying
	sys.bindModule
	sys.containsModule
	sys.unbindModule
	sys.modulesCount

	/* PUBLIC MODULE ACCESS */
	val mod1 = new ModuleX1
	mod1.moduleId
	mod1.asyncContext
	mod1.status
	mod1.context

	val mod2 = new ModuleX2
	mod2.moduleId
	mod2.asyncContext
	mod2.status
	mod2.context
}

