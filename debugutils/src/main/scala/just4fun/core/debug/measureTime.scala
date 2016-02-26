package just4fun.core.debug

import just4fun.core.debug.DebugUtils._

object measureTime {
	//	import just4fun.utils.devel.ILogger._
	def apply[T](tag: String, times: Int = 1, warmUp: Boolean = true)(code: => T): T = {
		var result: T = null.asInstanceOf[T]
		var ns = 0L
		if (warmUp) {
			code
			code
		}
		if (times == 1) {
			val t0 = System.nanoTime()
			result = code
			ns = System.nanoTime() - t0
			DebugConfig.log(4, "", s"$tag TIME= $ns", 0)
//			logI(s"$tag TIME= $ns")
		}
		else {
			val range = 0 until times
			val t0 = System.nanoTime()
			for (_ <- range) result = code
			ns = System.nanoTime() - t0
			DebugConfig.log(4, "", s"$tag TIME (ms per $times; ns per 1)=  ${(ns / 1000000f).toInt};   ${ns / times}", 0)
//			logI(s"$tag TIME (ms per $times; ns per 1)=  ${(ns / 1000000f).toInt};   ${ns / times}")
		}
		result
	}
}
