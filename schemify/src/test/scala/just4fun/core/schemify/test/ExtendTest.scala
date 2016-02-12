package just4fun.core.schemify.test

import just4fun.core.schemify.impls.ReachSchema
import just4fun.core.schemify.{Prop, PropType, Schemify}
import just4fun.core.debug.DebugUtils._

object ExtendTest extends App {
	logV(TestSchema.props.map(p => s"[${p.name} : ${p.getClass.getSimpleName}]").mkString(", "))
	val o = new TestObject(120000, 1, "ok")
	val vs = TestSchema.valuesToJsonArray(o)
	logV(vs)
}


class Column[O <: DbObject, T](override val schema: DbSchema[O])(implicit override val typ: PropType[T]) extends Prop[O, T](schema) {
}


abstract class DbSchema[T <: DbObject : Manifest] extends ReachSchema[T] {
	override type P[t <: T, v] = Column[t, v]

	val _id = PROP[Long].config { c => () }

	override protected[this] def newProp[W](implicit t: PropType[W]): Column[T, W] = new Column[T, W](this)
}


trait DbObject {
	var _id: Long = 0
}

class DbXSchema extends DbSchema[TestObject]

//@Schemify object TestSchema extends DbSchema[TestObject] with BlaSchema[TestObject] with Fake {
@Schemify object TestSchema extends DbXSchema with BlaXSchema with Fake {
	val x = PROP[Long]
	val y = PROP[Double]
	val name = PROP[String]
	val isOk = STUB
}

class TestObject(var x: Long, var y: Double, var name: String) extends DbObject with MetaObj with BlaObj {
//	var isOk = true
}

trait BlaObj {
	var bla = 0
}

trait BlaSchema[T <: BlaObj] extends ReachSchema[T] {
	val bla = PROP[Int]
}

trait MetaObj{
	var meta: Int = 0
}

trait MetaSchema[T] extends ReachSchema[T] {
	val meta = PROP[Int]
}

trait BlaXSchema extends BlaSchema[TestObject] with MetaSchema[TestObject]
