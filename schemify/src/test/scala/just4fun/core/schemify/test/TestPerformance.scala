package just4fun.core.schemify.test

import scala.collection.mutable.ArrayBuffer
import just4fun.core.debug.measureTime

object TestPerformance extends App {
	import Schemas._
	//	val a1 = new AObj(
	//	v0 = 100,
	//		v1 = 10,
	//		v2 = 1,
	//		v3 = 2,
	//		v5 = '3',
	//		v6 = 23.5,
	//		v7 = 0.07f,
	//		v8 = true,
	//		v9 = "ok",
	//		v10 = Array[Byte](0, 12, 44, 0),
	//		v11 = ArrayBuffer(1, 2, 3, 4),
	//		v12 = Rect(11, 22, 33, 44),
	//		v13 = Oval(21, 41, "fake")(122),
	//		v14 = new Triangle(87, 'c', "Tri")
	//	)
	val a1 = new AObj
	a1.v0 = 100
	a1.v1 = 10
	a1.v2 = 1
	a1.v3 = 2
	a1.v5 = '3'
	a1.v6 = 23.5
	a1.v7 = 0.07f
	a1.v8 = true
	a1.v9 = "ok"
	a1.v10 = Array[Byte](0, 12, 44, 0)
	a1.v11 = ArrayBuffer(1, 2, 3, 4)
	a1.v12 = new Rect(11) {y = 22; w = 33; h = 44}
	a1.v13 = Oval(21, 41, "fake")(122)
	a1.v14 = new Triangle(87, 'c', "Tri")
	//	a1.v15 = AObj(50, 51, 52, 53, 54, 55, 56, 57)
	//

	val N = 1
	val values = a1.values
	val valuesMap = AObj.propsMap.map{case(k, p) => k -> values(p.index)}
	val valsBytes = a1.valuesToBytes
	val valsRawArr = a1.valuesToArray
	println(s"vals Arr= ${valsRawArr.mkString(", ")}")
	val valsRawMap = a1.valuesToMap
//		println(s"vals Map= ${valsRawMap.mkString(", ") }")
	val valsArrStr = a1.valuesToJsonArray
	//	println(s"vals ArrStr= $valsArrStr")
	val valsMapStr = a1.valuesToJsonMap
	//	println(s"vals MapStr= $valsMapStr")
	var buff: ArrayBuffer[Any] = null
	var map: collection.mutable.HashMap[String, Any] = null
	var a2: AObj = null

	//	 3: 110.000 (80.000)
	def g0 = measureTime("GET BYTES", N) {
		val valsBytes = a1.valuesToBytes
		//		println(s"get vals Arr= ${valsArr.mkString(", ") }")
	}
	//	 3: 110.000 (80.000)
	def g1 = measureTime("GET ARR", N) {
		val valsArr = a1.valuesToArray
		//		println(s"get vals Arr= ${valsArr.mkString(", ") }")
	}
	//	 7: 215.000 (150.000)
	def g2 = measureTime("GET MAP", N) {
		val valsMap = a1.valuesToMap
		//		println(s"get vals Map= ${valsMap.mkString(", ") }")
	}
	//	 4: 110.000
	def g3 = measureTime("GET STR A", N) {
		val valsArrStr = a1.valuesToJsonArray
		//		println(s"get vals ArrStr= $valsArrStr")
	}
	//	 5: 145.000
	def g4 = measureTime("GET STR M", N) {
		val valsMapStr = a1.valuesToJsonMap
		//		println(s"get vals MapStr= $valsMapStr")
	}
	//	 1: 45.000
	def g5 = measureTime("GET PROPS", N) {
		buff = ArrayBuffer[Any]()
		a1.valuesReading(AObj.props) { (n, p, v) => buff += v }
		//		println(s"get vals Of= ${buff.mkString(", ") }")
	}
	//	 2: 80.000
	def g6 = measureTime("GET ITERA", N) {
		buff = ArrayBuffer[Any]()
		a1.valuesIterating(onNext => for (n <- 0 until AObj.propsSize) onNext((p, v) => buff += v))
		//		println(s"get vals Iter= ${buff.mkString(", ") }")
	}
	//	 6: 180.000
	def g7 = measureTime("GET FIND", N) {
		map = collection.mutable.HashMap[String, Any]()
		a1.valuesFinding(findNext => valsRawMap.foreach { case (k, v) => findNext(k, (p, v) => map += k -> v) })
		//		println(s"get vals Find= ${map.mkString(", ") }")
	}
	//	 1: ???.000
	def g8 = measureTime("GET VALS", N) {
		val arr = a1.values
		//		println(s"get vals Of= ${buff.mkString(", ") }")
	}

	//
	//	 1: 53.000 (49.000)
	def s0 = measureTime("SET BYTES", N) {
		a2 = AObj.createFromBytes(valsBytes)
		//		println(s"set vals Arr= ${a2.getValuesString() }")
	}
	//	 1: 53.000 (49.000)
	def s1 = measureTime("SET ARR", N) {
		a2 = AObj.createFromIterable(valsRawArr)
		//		println(s"set vals Arr= ${a2.getValuesString() }")
	}
	//	 4: 145.000 (90.000)
	def s2 = measureTime("SET MAP", N) {
		a2 = AObj.createFromIterable(valsRawMap)
		//		println(s"set vals Map= ${a2.getValuesString() }")
	}
	//	 6: 215.000
	def s3 = measureTime("SET STR A", N) {
		a2 = AObj.createFromJson(valsArrStr)
		//		println(s"set vals StrArr= ${a2.getValuesString() }")
	}
	//	 7: 310.000
	def s4 = measureTime("SET STR M", N) {
		a2 = AObj.createFromJson(valsMapStr)
		//		println(s"set vals StrMap= ${a2.getValuesString() }")
	}
	//	 2: 60.000
	def s5 = measureTime("SET PROPS", N) {
		a2 = AObj.createReading(AObj.props) { (n, p) => values(p.index) }
		//		println(s"set vals Of= ${a2.getValuesString() }")
	}
	//	 3: 83.000
	def s6 = measureTime("SET ITERA", N) {
		a2 = AObj.createIterating(onNext => values.foreach(v => onNext(p => v)))
		//		println(s"set vals Iter= ${a2.getValuesString() }")
	}
	//	 5: 152.000
	def s7 = measureTime("SET FIND", N) {
		a2 = AObj.createFinding(findNext => valuesMap.foreach { case (k, v) => findNext(k, p => v) })
		//		println(s"set vals Iter= ${a2.getValuesString() }")
	}

	//	 0: 5.600
	def copy1 = measureTime("COPY FLAT", N) {
		a2 = a1.valuesCopy()
		//				println(s"copy flat= ${a2.getValuesString() }")
	}
	//	 1: 50.000
	def copy2 = measureTime("COPY DEEP", N) {
		a2 = a1.valuesCopy(true)
		//				println(s"copy deep= ${a2.getValuesString() }")
	}

	println();
	copy1; copy2; println();
	g0; g1; g2; g3; g4; g5; g6; g7; /*g8*/; println();
	s0; s1; s2; s3; s4; s5; s6; s7; println();
}
