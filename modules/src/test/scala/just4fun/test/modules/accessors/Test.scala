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
	mod.isAbleToServe
	mod.isAbleToServeNow
	mod.delayedInit()
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
	mod.info.isRestored
	mod.info.failure
}

