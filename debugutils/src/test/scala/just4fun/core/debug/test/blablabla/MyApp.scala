package just4fun.core.debug.test.blablabla
import just4fun.core.debug.DebugUtils._

class MyApp {
	val _dl = disableDebugCode()

	logE("WTF   ?")
	logV("just logV")
	def test(): Unit = {
		val e = new Exception("OOPS...")
		e.initCause(new Exception("BECAUSE..."))
		logE(e, "Don't worry :)")
	}
	def test2(): Unit = {
		throw new Exception("BOOM!!")
	}
}
