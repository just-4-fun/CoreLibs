package just4fun.core.schemify.test

import scala.collection.mutable.ArrayBuffer
import just4fun.core.schemify.impls.{Shelled, Schema, Shell, ArrayBufferType}
import just4fun.core.schemify.{Pearl, Schemify}

object Schemas {
	implicit val buffIntType = new ArrayBufferType[Int]
	//	implicit object Rect extends RectSchema
	implicit val rectSch = Rect
	//	implicit object OvalSch extends OvalSchema
	implicit val ovalSch = Oval
	//	implicizt object AObj extends ASchema
	//	implicit val aSch = AObj

	implicit class AShell(val pearl: AObj) extends Shell[AObj] {
		override val schema = AObj
	}
	implicit class ZenXXImpl(val pearl: ZenXX) extends Shell[ZenXX] {
		override val schema = ZenXXSchXX
	}
	implicit class ZenXImpl(val pearl: ZenX) extends Shell[ZenX] {
		override val schema = ZenXSchXX
	}
	implicit class ZenImpl(val pearl: Zen) extends Shell[Zen] {
		override val schema = ZenSchXX
	}

	implicit class BookImpl(val pearl: Book) extends Shell[Book] {
		override val schema = Book
	}
	//	implicit class SuperAnimalShell(val pearl: SuperAnimal) extends Shell[SuperAnimal] {
	//		override def schema = SuperAnimal
	//	}
}


class Triangle(var a: Float, val b: Char = 'x', var c: String) extends Pearl {
	//	var a: Double = _; val b: Boolean = true; var c: String = "3T"
}
class Rect(var x: Int) {
	//	var x = 0
	var y = 0
	var w = 0
	var h = 0
}
@Schemify object Rect extends Schema[Rect] {
	val x = PROP[Int]
	val y = PROP[Int]
	val w = PROP[Int]
	val h = PROP[Int]
	override def create = new Rect(881)
}
case class Oval(x: Int, var y: Int, fake: String, fake2: String= "ok")(var radius: Int)
@Schemify object Oval extends Schema[Oval] {
	protected var x = PROP[Int]
	val y = PROP[Int].config(p=>())
	val radius = PROP[Int]
	//	override def instance = just4fun.core.schemify.test.Oval(x=0,y=0)(radius=0)
}
//class AObj (
//	val v0: Long,
//	val v1: Int,
//	val v2: Short,
//	val v3: Byte,
//	// Stub
//	val v5: Char,
//	val v6: Double,
//	val v7: Float,
//	val v8: Boolean,
//	val v9: String,
//	val v10: Array[Byte],
//	val v11: ArrayBuffer[Int],
//	val v12: Rect,
//	val v13: Oval,
//	val v14: Triangle
//	)
class AObj {
	var v0: Long = _
	var v1: Int = _
	var v2: Short = _
	var v3: Byte = _
	// Stub
	var v5: Char = _
	var v6: Double = _
	var v7: Float = _
	var v8: Boolean = _
	var v9: String = _
	var v10: Array[Byte] = _
	var v11: ArrayBuffer[Int] = _
	var v12: Rect = _
	var v13: Oval = _
	var v14: Triangle = _
	//	var v15: AObj = _
}
@Schemify object AObj extends Schema[AObj] {
	import Schemas._
	val v0 = PROP[Long]
	val v1 = PROP[Int]
	var v2 = PROP[Short]
	val v3 = PROP[Byte].config(p=>())
	val v4 = STUB.config(p=>())
	val v5 = PROP[Char]
	val v6 = PROP[Double]
	val v7 = PROP[Float]
	val v8 = PROP[Boolean]
	val v9 = PROP[String]
	val v10 = PROP[Array[Byte]]
	val v11 = PROP[ArrayBuffer[Int]]
	val v12 = PROP[just4fun.core.schemify.test.Rect]
	val v13 = PROP[Oval]
	val v14 = PROP[Triangle]
	//	val v15 = Prop[AObj]
}



trait Animal {
	var head: String = _
	var body: String = _
}
//class SuperAnimal extends Animal {
class SuperAnimal extends Animal with Shelled[SuperAnimal] {
	override val schema = MegaAnimal
	def useSuperPower() = {}
}
@Schemify trait AnimalSchema[T <: Animal] extends Schema[T] {
	val head = PROP[String]
	val body = PROP[String]
}
@Schemify class SuperAnimalSch extends AnimalSchema[SuperAnimal]
@Schemify object MegaAnimal extends SuperAnimalSch


object sub {
	class Obj {var x = 1}
}
@Schemify object Obj extends just4fun.core.schemify.impls.Schema[just4fun.core.schemify.test.sub.Obj] {
	val x = PROP[Int]
}
import sub._
@Schemify class ObjSch extends Schema[Obj] {
	val x = PROP[Int]
}


trait Fake
class Zen {
	var id: Long = 0
	val stub = 0
	val id2: Long = 0
}
class ZenX extends Zen {var typ: String = _}
class ZenXX(var misc: String) extends ZenX
@Schemify(printCode = false, dontWarnStub = true, dontWarnVal = true) trait ZenSch[T <: Zen] extends Schema[T] {
	val id = PROP[Long]
	val stub = STUB
	val id2 = PROP[Long]
}
abstract class ZenXSch[T <: ZenX: Manifest] extends ZenSch[T] with Fake {
	val typ = PROP[String]
}
class ZenSchX extends ZenSch[Zen]{
	override def create: Zen = new Zen
}
@Schemify(true, true) class ZenXSchX extends ZenXSch[ZenX]
@Schemify(true, true) class ZenXXSchX extends just4fun.core.schemify.test.ZenXSch[just4fun.core.schemify.test.ZenXX]{
	val misc = PROP[String]
	//	override def instance: ZenXX = new ZenXX("zzz ok")
}
object ZenSchXX extends ZenSchX
object ZenXSchXX extends ZenXSchX
object ZenXXSchXX extends ZenXXSchX


trait Faket[T]
class ObjecT
@Schemify trait SuperSch[V, T] extends Schema[T]
@Schemify trait Super[V, T] extends just4fun.core.schemify.test.SuperSch[V, ObjecT] with Faket[T]
@Schemify object Mega extends Super[String, Boolean]



class Book {
	val title: String = "Hei chell"
	var price: Double = 0
	var pages: Int = _
}
@just4fun.core.schemify.Schemify(dontWarnVal = true, dontWarnStub = true) object Book extends Schema[Book]  {
	val title = PROP[String]
	val price = STUB
	val pages = PROP[Int]
}



class OptionY(val x: Option[Int], fake: Option[Int], val b: Option[OptionZ]) {var y: Option[Int] = null}
class OptionZ(val x: Option[Int], val z: Option[Int]) extends  Pearl {var y: Option[Int] = null}
@Schemify object OptYSch extends Schema[OptionY] {
	val x = PROP[Option[Int]]
	val y = PROP[Option[Int]]
	val b = PROP[Option[OptionZ]]
}
