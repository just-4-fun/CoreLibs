package just4fun.core.schemify.test

import just4fun.core.schemify._

object TestOpts extends App{
	import PropType._
	def getOptType[T](v: T)(implicit typ: OptionType[T]): Unit = {
		println(s"OPT_TYPE= $typ")
	}
	implicit val aSch = new ASch
	implicit val aaSch = new AASch
	getOptType(1)
	getOptType(10000)
	getOptType(10000L)
	getOptType("ok")
	getOptType("bla")
	getOptType(new A(0, new B(1)))
	getOptType(new A(100, new B(1)))
	println(impliMap.mkString( "\n"))

	implicit val ysch = OptYSch
	val y = OptYSch.createFromJson("[]")
	println(s"y.x= ${y.x}; y.y= ${y.y}; y.b= ${y.b}; ")
	println(s"vals= ${OptYSch.valuesToJsonMap(y)}; ")
}

class A(val  x: Int, val b: B) {var opt: Option[B] = null; var sopt: Option[String] = null}
class AA(val  x: Int, val b: B) {var opt: Option[B] = null; var sopt: Option[String] = null}
class B(val x: Int) extends Pearl
@Schemify class ASch extends AutoSchema[A] {
	val x = PROP[Int]
	val b = PROP[B]
	val opt = PROP[Option[B]]
	val sopt = PROP[Option[String]]
}
@Schemify class AASch extends AutoSchema[AA] {
	val x = PROP[Int]
	val b = PROP[B]
	val opt = PROP[Option[B]]
	val sopt = PROP[Option[String]]
}

class Y(val x: Option[Int], fake: Option[Int], val b: Option[Z]) {var y: Option[Int] = null}
class Z(val x: Option[Int], val z: Option[Int]) extends  Pearl {var y: Option[Int] = null}
@Schemify object YSch extends AutoSchema[Y] {
	val x = PROP[Option[Int]]
	val y = PROP[Option[Int]]
	val b = PROP[Option[Z]]
}
