package just4fun.core.schemify.test

import org.scalatest.{Matchers, FreeSpec}


class TestOfTest extends FreeSpec with Matchers {
	def prn(msg: String, v: Any = null) = {println(msg); v}
	var v = -1
	"[1]" - {
		v = 1
		prn(s"[1 <  $v]")
		"[in 1.1]" in { prn(s"[in 1.1  $v]")}
		"[1.2]" - {
			v = 12
			prn(s"[1.2 <  $v]")
			"[in 1.2.1]" in { prn(s"[in 1.2.1  $v]")}
			"[1.2.2]" - {
				val v = 122
				prn(s"[1.2.2  $v]")
				"[in 1.2.2.1]" in { prn(s"[in 1.2.2.1  $v]")}
				"[in 1.2.2,2]" in { prn(s"[in 1.2.2.2  $v]")}
			}
			"[in 1.2.3]" in { prn(s"[in 1.2.3  $v]")}
			prn(s"[1.2 >  $v]")
		}
		"[in 1.3]" in { prn(s"[in 1.3  $v]")}
		"[1.4]" - {
			v = 14
			prn(s"[1.4  $v]")
		}
		prn(s"[1 >  $v]")
	}
}
