package just4fun.core.modules.test.syntax.test

import just4fun.core.modules.test.syntax.{ModuleX1, SystemX1}

object Test {
	val sys = new SystemX1
	sys.asyncContext
	sys.isSystemStarted
	sys.isSystemStopping
	sys.hasModule

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

