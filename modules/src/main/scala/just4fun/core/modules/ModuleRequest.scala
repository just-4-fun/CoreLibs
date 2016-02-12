package just4fun.core.modules

import scala.util.Try
import just4fun.core.async.FutureX


class ModuleRequest[T] extends FutureX[T] {
	private[modules] var module: Module = null
	override final protected[this] def onStartExecute(): Unit = {
		if (module != null) module.requestStart(this)
	}
	override final protected[this] def onFinishExecute(v: Try[T]): Unit = {
		if (module != null) module.requestComplete(this)
	}
}
