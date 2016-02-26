package just4fun.test.modules.accessors

import just4fun.test.modules.accessors.definitions.pkg0.SystemX1
import just4fun.test.modules.accessors.definitions.pkg1.ModuleX1


object Test {
	/* PUBLIC SYSTEM ACCESS */
	val sys = new SystemX1
	sys.asyncContext
	sys.isSystemStarted
	sys.isSystemStopping
	sys.hasModule
	sys.moduleContent

	/* PUBLIC MODULE ACCESS */
	val mod = new ModuleX1
	mod.asyncContext
	mod.status
	mod.system
	mod.isAbleToServe
	mod.isAbleToServeNow
}

