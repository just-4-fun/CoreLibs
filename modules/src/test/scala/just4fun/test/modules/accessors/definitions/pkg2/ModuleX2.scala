package just4fun.test.modules.accessors.definitions.pkg2

import just4fun.core.modules.Module
import just4fun.test.modules.accessors.definitions.pkg0.ModuleX
import just4fun.test.modules.accessors.definitions.pkg1.ModuleX1

class ModuleX2 extends ModuleX {
	/* PROTECTED MODULE ACCESS */
	// TODO too little can beaccessed from another module
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
	mod.info.isRestored
	mod.info.failure

}
