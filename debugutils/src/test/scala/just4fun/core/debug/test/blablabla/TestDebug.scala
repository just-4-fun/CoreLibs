package just4fun.core.debug.test.blablabla
import just4fun.core.debug.DebugUtils._

class TestDebug {
	private[this] val _dc = disableDebugCode()

	debug{
		val x = 2+5/3
		println(s"DEBUG:  That code exists ONLY IF ENABLED DEBUG :)")
	}
	println(s"DEBUG:  That code exists ANYWAY :)")
	val v = if (2/2 == 2) true else debug{
		println(s"DEBUG:  DO NOT USE INSIDE CONDITION (will not be removed) !!!")
		false
	}
	println(s"DEBUG:  v= $v")

	val res = debug{
		3+5
	}
	println(s"DEBUG:  res= $res")

	println(s"DEBUG:  call  getString >> ${getString()}")
	def getString(): String = debug {
		"bla"
	}


}
