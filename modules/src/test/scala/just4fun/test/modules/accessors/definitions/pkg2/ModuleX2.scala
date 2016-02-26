package just4fun.test.modules.accessors.definitions.pkg2

import just4fun.core.modules.Module
import just4fun.test.modules.accessors.definitions.pkg0.ModuleX
import just4fun.test.modules.accessors.definitions.pkg1.ModuleX1

class ModuleX2 extends ModuleX {
	/* PROTECTED MODULE ACCESS */
	val mod = new ModuleX1
	mod.system
	mod.asyncContext
	mod.status
}
