package just4fun.core.schemify.test.suite1

import scala.collection.mutable.ArrayBuffer
import org.scalatest.{Matchers, FreeSpec}

class Test extends FreeSpec with Matchers {
	import Implicits._
	def prn(msg: String, v: Any = null) = { println(msg); v }


	"GENERAL TEST" - {
		val orig = new Obj
		orig.v0 = 0
		orig.v1 = 1
		orig.v2 = 2
		orig.v3 = 3
		orig.v4 = 0.4
		orig.v5 = 0.5f
		orig.v6 = '6'
		orig.v7 = true
		orig.v8 = "8-)"
		orig.v9 = Array[Byte](9, 8, 7)
		orig.v10 = ArrayBuffer(10, 9, 8)
		orig.v11 = new AutoObj(11)
		orig.v11.v1 = 111
		orig.v12 = new InnerObj(12)
		orig.v12.v1 = 122
		orig.v14 = AutoCaseObj(14)
		orig.v14.v1 = 144
		orig.v15 = InnerCaseObj(15)
		orig.v15.v1 = 155
		orig.v16 = ArrayBuffer(new InnerObj(16.1), new InnerObj(16.2))
		orig.v17 = List(List(new InnerObj(17.1), new InnerObj(17.2)))
		orig.v18 = BigDecimal(18)
		orig.v19 = Some("StrOpt_19")
		orig.v20 = Some(new SimpleObj)
		orig.v21 = Some(new AutoObj(112))
		//
		val valsArray = orig.values
		val valsMap = Obj.propsMap.map { case (k, p) => k -> valsArray(p.index) }
		val rawArray = orig.valuesToArray
		val rawMap = orig.valuesToMap
		val origBytes = orig.valuesToBytes
		val origArrayStr = orig.valuesToJsonArray
		val origMapStr = orig.valuesToJsonMap
		prn(s" $origArrayStr")
		prn(s" $origMapStr")
		//		prn(s"${Obj.createFromBytes(origBytes).valuesToJsonArray}\n${origArrayStr}")
		//	info(s"orig Map len= ${origMapStr.length}; str= $origMapStr")

		"When construct object" - {
			"from product" in {
				// order of values should reflect order of schema props including stubs.
				Obj.create(orig.v0, orig.v1, orig.v2, orig.v3, orig.v4, orig.v5, orig.v6, orig.v7, orig.v8, orig.v9, orig.v10, orig.v11, orig.v12, null, orig.v14, orig.v15, orig.v16, orig.v17, orig.v18, orig.v19, orig.v20, orig.v21).valuesEqual(orig) shouldBe true
			}
			"from Bytes" in {
				Obj.createFromBytes(origBytes).valuesEqual(orig) shouldBe true
			}
			"from Array" in {
				Obj.createFromIterable(rawArray).valuesEqual(orig) shouldBe true
			}
			"from Map" in {
				Obj.createFromIterable(rawMap).valuesEqual(orig) shouldBe true
			}
			"from string array" in {
				Obj.createFromJson(origArrayStr).valuesEqual(orig) shouldBe true
			}
			"from string map" in {
				Obj.createFromJson(origMapStr).valuesEqual(orig) shouldBe true
			}
			"setValuesAll" in {
				val obj = Obj.createReadingAll { p => valsArray(p.index) }
				obj.valuesEqual(orig) shouldBe true
			}
			"setValuesOf" in {
				val obj = Obj.createReading(Obj.props) { (n, p) => valsArray(n) }
				obj.valuesEqual(orig) shouldBe true
			}
			"setValuesIterating" in {
				val obj = Obj.createIterating { writeNext => valsArray.foreach(v => writeNext(p => v)) }
				obj.valuesEqual(orig) shouldBe true
			}
			"setValuesFinding" in {
				val obj = Obj.createFinding { writeNext => valsMap.foreach(kv => writeNext(kv._1, p => kv._2)) }
				obj.valuesEqual(orig) shouldBe true
			}
			"from null" in {
				Obj.createFromBytes(null) shouldBe null
				Obj.createFromIterable(null) shouldBe null
				Obj.createFromIterable(null) shouldBe null
				Obj.createFromJson(null) shouldBe null
			}
		}
		"When get values" - {
			"getValuesBytes" in {
				orig.valuesToBytes shouldEqual origBytes
			}
			"getValuesArray" in {
				orig.valuesToArray should contain theSameElementsAs rawArray
			}
			"getValuesMap" in {
				orig.valuesToMap should contain theSameElementsAs rawMap
			}
			"getValuesString as Array" in {
				orig.valuesToJsonArray shouldEqual origArrayStr
			}
			"getValuesString as Map" in {
				orig.valuesToJsonMap shouldEqual origMapStr
			}
			"getValuesAll" in {
				val array = new ArrayBuffer[Any]()
				orig.valuesReadingAll { (p, v) => array += v }
				array should contain theSameElementsAs valsArray
			}
			"getValuesOf" in {
				val array = new ArrayBuffer[Any]()
				orig.valuesReading(Obj.props) { (n, p, v) => array += v }
				array should contain theSameElementsAs valsArray
			}
			"getValuesIterating" in {
				val array = new ArrayBuffer[Any]()
				orig.valuesIterating(readNext => for (n <- 0 until Obj.propsSize) readNext((p, v) => array += v))
				array should contain theSameElementsAs valsArray
			}
			"getValuesFinding" in {
				val map = collection.mutable.Map[String, Any]()
				orig.valuesFinding { readNext => Obj.props.foreach(p => readNext(p.name, (p, v) => map += (p.name -> v))) }
				map should contain theSameElementsAs valsMap
			}
			"When values are of different type" in {
				val obj = Obj.create("v", "v", "v", "v", "v", "v", "v", "v", "v", "v", "v", "v", "v", "v", "v", "v", "v", "v", "v", "v", "v", "v")
				obj.v1 shouldEqual  0
				obj.v12 shouldEqual null
			}
			"When values are nulls" in {
				val obj = Obj.create(null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null)
				obj.v1 shouldEqual  0
				obj.v12 shouldEqual null
			}
			"When obj is null" in {
				Obj.valuesToBytes(null) shouldBe null
				Obj.valuesToArray(null) shouldBe null
				Obj.valuesToMap(null) shouldBe null
				Obj.valuesToJsonArray(null) shouldBe null
				Obj.valuesReadingAll(null) { (p, v) => () }
				Obj.valuesReading(null, Obj.props) { (n, p, v) => () }
				Obj.valuesIterating(null)(readNext => ())
				Obj.valuesFinding(null) { readNext => () }
				Obj.copy(null) shouldBe null
			}
		}
		"When copy object" in {
			orig.valuesCopy(true).valuesEqual(orig) shouldBe true
			orig.valuesCopy().valuesEqual(orig) shouldBe true
		}
		"When access array of objects" - {
			val objects = ArrayBuffer(orig, orig, orig)
			"as Bytes" in {
				val bytes = Obj.arrayType.valuesToBytes(objects)
				val objects2 = Obj.arrayType.createFromBytes(bytes)
				objects.head valuesEqual objects2.head shouldBe true
			}
			"as String" in {
				val string = Obj.arrayType.valuesToJson(objects)
				val objects2 = Obj.arrayType.createFromJson(string)
				objects.head valuesEqual objects2.head shouldBe true
			}
			"When null" in {
				Obj.arrayType.valuesToBytes(null) shouldBe null
				Obj.arrayType.createFromBytes(null) shouldBe null
				Obj.arrayType.valuesToJson(null) shouldBe null
				Obj.arrayType.createFromJson(null) shouldBe null
			}
		}
	}

	"DURABILITY TESTS" - {
		"It's ok  if num of values is more or less than num of schema props" - {
			"In array setter" in {
				var obj = SimpleObj.create("v0", "v1", "v2", "fakse value")
				obj.v2 shouldEqual "v2"
				obj = SimpleObj.create("v0", "v1")
				obj.v2 shouldEqual null
			}
			"In map setter" in {
				var obj = SimpleObj.createFromIterable(Map("v0" -> "v0", "v1" -> "v1", "v2" -> "v2", "fake" -> "fakse value"))
				obj.v2 shouldEqual "v2"
				obj = SimpleObj.createFromIterable(Map("v0" -> "v0", "v1" -> "v1"))
				obj.v2 shouldEqual null
			}
			"In iteration setter" in {
				var itr = List("v0", "v1", "v2", "fakse value")
				var obj = SimpleObj.createIterating { write => itr.foreach(v => write(p => v)) }
				obj.v2 shouldEqual "v2"
				itr = List("v0", "v1")
				obj = SimpleObj.createIterating { write => itr.foreach(v => write(p => v)) }
				obj.v2 shouldEqual null
			}
			"In createFromValues num of values could exceed props" in {
				val obj = SimpleObj.createFromValues(Array("v0", "v1", "v2", "fake value"))
				obj.v2 shouldEqual "v2"
			}
			"In createFromValues num of values could not be less than props" in {
				the[Exception] thrownBy SimpleObj.createFromValues(Array("v0", "v1"))
			}
		}
		"When field is immutable val it can't be set" in {
			val obj = ObjWithVal.create("v0", "v1", "fake value")
			obj.v0 shouldEqual null
		}
		"When field is immutable val in constructor it can't be set" - {
			"it can be constructed from schema" in {
				val obj = ObjWithConstrVal.create("v0", "v1", "fake value")
				obj.v0 shouldEqual "v0"
			}
			//			"it can't be set from values" in {
			//				val obj = ObjWithConstrVal("v0", "v1", "fake value")
			//				obj.setValuesArray(List("new v0", "new v1"))
			//				obj.v1 shouldEqual "new v1"
			//				obj.v0 should not be "new v0"
			//				obj.v0 shouldEqual "v0"
			//			}
		}
		"Case class instance " - {
			"Values can be set from constructor" in {
				val obj = CaseObj.create("v0", "v1", "fake value")
				obj.v0 shouldEqual "v0"
			}
			//			"Values can't be set from values" in {
			//				val obj = CaseObj("v0", "v1", "fake value")
			//				obj.v0 shouldEqual "v0"
			//				obj.setValuesArray(List("new v0", "new v1"))
			//				obj.v0 should not be "new v0"
			//				obj.v0 shouldEqual "v0"
			//			}
		}
		//		"Case class instance with mutable fields" - {
		//			"Values can be set from values" in {
		//				val obj = CaseObjWithVar("v0", "v1", "fake value")
		//				obj.v0 shouldEqual "v0"
		//				obj.setValuesArray(List("new v0", "new v1"))
		//				obj.v0 shouldEqual "new v0"
		//				obj.v1 shouldEqual "new v1"
		//			}
		//		}
		"Object field represented as STUB in schema is not usable. See compiler warning." in {
			val obj = ObjWithStubVal.create("v0", "v1")
			obj.v1 should not be "v1"
			obj.v1 shouldEqual null
		}
		"Object constructor can be of any shape." in {
			var obj = ObjWithComplexConstrSchema.create("v0", "v1")
			obj.v0 shouldEqual "v0"
			obj.v1 shouldEqual "v1"
			obj = new ObjWithComplexConstr("fake0", "v0", "fake1")
			obj.v0 shouldEqual "v0"
			obj.v1 shouldEqual null
		}
	}


	"Hierarchy and @Schemify annotation TEST" - {
		"W hierarchy: concrete schema object annotated" in {
			val par = ParentW.create(1, 2)
			par.v0 shouldEqual 1
			par.v1 shouldEqual 2
			val chil = ChildW.create(1, 2, 3)
			chil.v0 shouldEqual 1
			chil.v1 shouldEqual 2
			chil.v2 shouldEqual 3
		}
		"Y hierarchy: abstract schema annotated. But it requires overriding def create in concrete subclass. WARN: in such case 'create' will be used instead primary constructor." in {
			val par = ParentY.create(1, 2)
			par.v0 shouldEqual 1
			par.v1 shouldEqual 2
			val chil = ChildY.create(1, 2, 3)
			chil.v0 shouldEqual 1
			chil.v1 shouldEqual 2
			chil.v2 shouldEqual 3
		}
		"Z hierarchy: concrete schema class annotated" in {
			val par = ParentZ.create(1, 2)
			par.v0 shouldEqual 1
			par.v1 shouldEqual 2
			val chil = ChildZ.create(1, 2, 3)
			chil.v0 shouldEqual 1
			chil.v1 shouldEqual 2
			chil.v2 shouldEqual 3
		}
		"X hierarchy: Schema class that defines props or it's subclass should be annotated. " +
		  "Or else access to those props throws an exception" in {
			the[IllegalStateException] thrownBy ChildX.create(1, 2, 3)
		}
	}


	"Auto implicit schema generation TEST" - {
		"If implicit SchemaType is not found, it's generated and cached. So threre is only one instance for all those fields." in {
			val obj = AutoShell.create(AutoPearl("0v0", "0v1"), AutoPearl("1v0", "1v1"))
			AutoShell.v0.typ shouldEqual AutoShell.v1.typ
		}
	}
}
