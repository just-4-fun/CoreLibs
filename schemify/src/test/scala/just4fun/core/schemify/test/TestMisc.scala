package just4fun.core.schemify.test

import just4fun.core.schemify._
import just4fun.core.schemify.impls.{Schema, Shelled}

object TestMisc extends App {


//	val BASE_NULL = 0
//	val BASE_BOOLEAN = 1 << 0
//	val BASE_STRING = 1 << 1
//	val BASE_BYTES = 1 << 2
//	val BASE_NUMERIC = 1 << 3
//	val BASE_INTEGER = 1 << 4
//	val BASE_FLOATING = 1 << 5
//	val BASE_LONG = (1 << 6) + BASE_INTEGER + BASE_NUMERIC
//	val BASE_INT = (1 << 7) + BASE_INTEGER + BASE_NUMERIC
//	val BASE_SHORT = (1 << 8) + BASE_INTEGER + BASE_NUMERIC
//	val BASE_CHAR = (1 << 9) + BASE_INTEGER + BASE_NUMERIC
//	val BASE_BYTE = (1 << 10) + BASE_INTEGER + BASE_NUMERIC
//	val BASE_DOUBLE = (1 << 11) + BASE_FLOATING + BASE_NUMERIC
//	val BASE_FLOAT = (1 << 12) + BASE_FLOATING + BASE_NUMERIC

	//	val valsBytes = xobj.valuesToBytes
	//	println(s"vals Arr= ${valsBytes.mkString(",")};  obj= ${xobj.valuesToJsonArray}")
	//	val xobj2 = XObj.createFromBytes(valsBytes)
	//	println(s"vals Arr= ${xobj2.valuesToBytes.mkString(",")};  obj= ${xobj2.valuesToJsonArray}")

	//	val a1 = new AObj
	//	a1.v0 = 100
	//	a1.v1 = 10
	//	a1.v2 = 1
	//	a1.v3 = 2
	//	a1.v5 = '3'
	//	a1.v6 = 23.5
	//	a1.v7 = 0.07f
	//	a1.v8 = true
	//	a1.v9 = "ok"
	//	a1.v10 = Array[Byte](0, 12, 44, 0)
	//	a1.v11 = ArrayBuffer(1, 2, 3, 4)
	//	a1.v12 = new Rect(11) {y = 22; w = 33; h = 44}
	//	a1.v13 = Oval(21, 41, "fake")(122)
	//	a1.v14 = new Triangle(87, 'c', "Tri")
	//	import Schemas._
	//	val valsBytes = a1.valuesToBytes
	//	println(s"vals Arr= ${valsBytes.mkString(",")}")
	//	val a2 = AObj.createFromBytes(valsBytes)

}

class XObj(var v0: Float, var v1: Double) extends Shelled[XObj] {
	override val schema = XObj
}
@Schemify object XObj extends Schema[XObj] {
	val v0 = PROP[Float]
	val v1 = PROP[Double]
}
