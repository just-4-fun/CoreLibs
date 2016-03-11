package just4fun.core.modules


abstract class ModuleException extends Exception


class ModuleServiceException(msg: String = null)(implicit  module: Module) extends ModuleException {
	override def getMessage: String = s"Module is not able to serve ${if (msg != null) msg else if (module.status.suspended) ": it is suspended" else if (module.status.destroying) ": it is detached" else  s"in ${module.getPhase} state"}."
}


class ModuleCreateException(val moduleClas: Class[_]) extends ModuleException {
	override def getMessage: String = s"Module ${moduleClas.getName} is created outside the ModuleSystem."
}


class ModuleBindException(val parentClas: Class[_], reason: String = null) extends ModuleException {
	override def getMessage: String = s"Module can not be bound to ${parentClas.getName}. ${if (reason != null) reason else ""}"
}


class SyncBindingException(bound: Module) extends ModuleException {
	// TODO more readable name
	override def getMessage: String = s"Module is failed because of synchronously bound module ${bound.getClass.getName} failed with  ${bound.failure.foreach(_.getMessage)}"
}
