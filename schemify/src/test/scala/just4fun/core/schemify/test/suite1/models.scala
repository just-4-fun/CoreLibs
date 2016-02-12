package just4fun.core.schemify.test.suite1

import scala.collection.mutable.ArrayBuffer
import just4fun.core.schemify._
import just4fun.core.schemify.impls._

object Implicits {
	implicit class ObjImpl(val pearl: Obj) extends Shell[Obj] {val schema = Obj}
	implicit val simpleSchema = SimpleObj
	implicit val innerSchema = InnerSchema
	implicit val innerCaseSchema = InnerCaseSchema
	implicit val arrayOfDouble = new ArrayBufferType[Double]
	implicit val listOfInnerObj = new ListType[InnerObj]
	implicit val listOfListOfInnerObj = new ListType[List[InnerObj]]
	implicit val arrayOfInnerObj = InnerSchema.arrayType
	implicit val bigNumType = new BigDecimalType
}

/* GENERAL TEST */
class Obj {
	var v0: Long = _
	var v1: Int = _
	var v2: Short = _
	var v3: Byte = _
	var v4: Double = _
	var v5: Float = _
	var v6: Char = _
	var v7: Boolean = _
	var v8: String = _
	var v9: Array[Byte] = _
	var v10: ArrayBuffer[Double] = _
	var v11: AutoObj = _
	var v12: InnerObj = _
	//  v13 STUB
	var v14: AutoCaseObj = _
	var v15: InnerCaseObj = _
	var v16: ArrayBuffer[InnerObj] = _
	var v17: List[List[InnerObj]] = _
	var v18: BigDecimal = _
	var v19: Option[String] = _
	var v20: Option[SimpleObj] = _
	var v21: Option[AutoObj] = _
}
@Schemify object Obj extends Schema[Obj] {
	import Implicits._
	val v0 = PROP[Long]
	val v1 = PROP[Int]
	val v2 = PROP[Short]
	val v3 = PROP[Byte]
	val v4 = PROP[Double]
	val v5 = PROP[Float]
	val v6 = PROP[Char]
	val v7 = PROP[Boolean]
	val v8 = PROP[String]
	val v9 = PROP[Array[Byte]]
	val v10 = PROP[ArrayBuffer[Double]]
	val v11 = PROP[AutoObj]
	val v12 = PROP[InnerObj]
	val v13 = STUB
	val v14 = PROP[AutoCaseObj]
	val v15 = PROP[InnerCaseObj]
	val v16 = PROP[ArrayBuffer[InnerObj]]
	val v17 = PROP[List[List[InnerObj]]]
	val v18 = PROP[BigDecimal]
	val v19 = PROP[Option[String]]
	val v20 = PROP[Option[SimpleObj]]
	val v21 = PROP[Option[AutoObj]]
}

class AutoObj(var v0: Double) extends Pearl {var v1: Int = _}

case class AutoCaseObj(var v0: Double) extends Pearl {var v1: Int = _}

class InnerObj(var v0: Double) {var v1: Int = _}
@Schemify object InnerSchema extends Schema[InnerObj] {
	val v0 = PROP[Double]
	val v1 = PROP[Int]
}

case class InnerCaseObj(var v0: Double) {var v1: Int = 0}
@Schemify object InnerCaseSchema extends Schema[InnerCaseObj] {
	val v0 = PROP[Double]
	val v1 = PROP[Int]
}


/* DURABILITY TEST */
class SimpleObj extends Shelled[SimpleObj] {
	override val schema = SimpleObj
	var v0: String = "sim0"
	var v1: String = "sim1"
	var v2: String = "sim2"
}
@Schemify object SimpleObj extends Schema[SimpleObj] {
	val v0 = PROP[String]
	val v1 = PROP[String]
	val v2 = PROP[String]
}

class ObjWithVal extends Shelled[ObjWithVal] {
	override val schema = ObjWithVal
	val v0: String = null
	var v1: String = null
}
@Schemify(true) object ObjWithVal extends Schema[ObjWithVal] {
	val v0 = PROP[String]
	val v1 = PROP[String]
}

class ObjWithConstrVal(val v0: String, var v1: String) extends Shelled[ObjWithConstrVal] {
	override val schema = ObjWithConstrVal
}
@Schemify(true) object ObjWithConstrVal extends Schema[ObjWithConstrVal] {
	val v0 = PROP[String]
	val v1 = PROP[String]
}

case class CaseObj(v0: String, v1: String) extends Shelled[CaseObj] {
	override val schema = CaseObj
}
@Schemify(true) object CaseObj extends Schema[CaseObj] {
	val v0 = PROP[String]
	val v1 = PROP[String]
}

case class CaseObjWithVar(var v0: String, var v1: String) extends Shelled[CaseObjWithVar] {
	override val schema = CaseObjWithVar
}
@Schemify object CaseObjWithVar extends Schema[CaseObjWithVar] {
	val v0 = PROP[String]
	val v1 = PROP[String]
}

class ObjWithStubVal extends Shelled[ObjWithStubVal] {
	override val schema = ObjWithStubVal
	var v0: String = null
	var v1: String = null
}
@Schemify(true, true) object ObjWithStubVal extends Schema[ObjWithStubVal] {
	val v0 = PROP[String]
	val v1 = STUB
}

case class ObjWithComplexConstr(fake0: String = "fake", var v0: String = "ok", fake1: String = "oops") extends Shelled[ObjWithComplexConstr] {
	override val schema: Schema[ObjWithComplexConstr] = ObjWithComplexConstrSchema
	var v1: String = null
}
@Schemify object ObjWithComplexConstrSchema extends Schema[ObjWithComplexConstr] {
	val v0 = PROP[String]
	val v1 = PROP[String]
}


/* HIERARCHY TEST */
trait FakeTrait1[T]
trait FakeTrait2
trait FakeTrait3
trait FakeTrait4

class ParentW(var v0: Int, var v1: Int)
class ChildW(v0: Int, v1: Int, var v2: Int) extends ParentW(v0, v1)
trait ParentWSchema[F, T <: ParentW] extends Schema[T] with FakeTrait1[F] {
	val v0 = PROP[Int]
	val v1 = PROP[Int]
}
trait ChildWSchema[T <: ChildW] extends ParentWSchema[Int, T] with FakeTrait2 {
	val v2 = PROP[Int]
}
@Schemify object ParentW extends ParentWSchema[Int, ParentW] with FakeTrait3
@Schemify object ChildW extends ChildWSchema[ChildW] with FakeTrait4

class ParentY(var v0: Int, var v1: Int)
class ChildY(v0: Int, v1: Int, var v2: Int) extends ParentY(v0, v1)
@Schemify trait ParentYSchema[T <: ParentY, F] extends Schema[T] with FakeTrait1[F] {
	val v0 = PROP[Int]
	val v1 = PROP[Int]
}
@Schemify trait ChildYSchema[T <: ChildY, F] extends ParentYSchema[T, F] with FakeTrait2 {
	val v2 = PROP[Int]
}
class ParentYSchemaExt extends ParentYSchema[ParentY, Int] with FakeTrait3 {
	override def create: ParentY = new ParentY(0, 0)
}
class ChildYSchemaExt extends ChildYSchema[ChildY, Int] with FakeTrait3 {
	override def create: ChildY = new ChildY(0, 0, 0)
}
object ParentY extends ParentYSchemaExt with FakeTrait4
object ChildY extends ChildYSchemaExt with FakeTrait4

class ParentZ(var v0: Int, var v1: Int)
class ChildZ(v0: Int, v1: Int, var v2: Int) extends ParentZ(v0, v1)
trait ParentZSchema[F, T <: ParentZ] extends Schema[T] with FakeTrait1[F] {
	val v0 = PROP[Int]
	val v1 = PROP[Int]
}
trait ChildZSchema[F, T <: ChildZ] extends ParentZSchema[F, T] with FakeTrait2 {
	val v2 = PROP[Int]
}
@Schemify class ParentZSchemaExt extends ParentZSchema[Int, ParentZ] with FakeTrait3
@Schemify class ChildZSchemaExt extends ChildZSchema[Int, ChildZ] with FakeTrait3
object ParentZ extends ParentZSchemaExt with FakeTrait4
object ChildZ extends ChildZSchemaExt with FakeTrait4

class ParentX(var v0: Int, var v1: Int)
class ChildX(v0: Int, v1: Int, var v2: Int) extends ParentX(v0, v1)
@Schemify trait ParentXSchema[T <: ParentX] extends Schema[T] {
	val v0 = PROP[Int]
	val v1 = PROP[Int]
}
object ChildX extends ParentXSchema[ChildX] {
	val v2 = PROP[Int]
	override def create: ChildX = new ChildX(0, 0, 0)
}


case class AutoPearl(v0: String, v1: String) extends Pearl
class AutoShell(var  v0: AutoPearl, var v1: AutoPearl) extends Shelled[AutoShell]{
	override val schema: Schema[AutoShell] = AutoShell
}
@Schemify object AutoShell extends Schema[AutoShell] {
	val v0 = PROP[AutoPearl]
	val v1 = PROP[AutoPearl]
}


/* PROP TYPE */
class BigDecimalType extends StringValueType[BigDecimal] {
	import just4fun.core.schemify._
	override def create(v: String): BigDecimal = BigDecimal(v)
	override def copy(v: BigDecimal, deep: Boolean): BigDecimal = BigDecimal(v.bigDecimal)
	override def eval(v: Any): BigDecimal = v match {
		case v: BigDecimal => v
		case v: String => try BigDecimal(v) catch {case ex: Throwable => BigDecimal(0)}
		case v: Long => BigDecimal(v)
		case v: Int => BigDecimal(v)
		case v: Double => BigDecimal(v)
		case v: Float => BigDecimal(v)
		case null => BigDecimal(0)
		case v: BigInt => BigDecimal(v)
		case _ => BigDecimal(PropType.string2double(v.toString))
	}
	override def equal(v1: BigDecimal, v2: BigDecimal): Boolean = v1.compare(v2) == 0
	override def value(v: BigDecimal): String = v.toString()
}

