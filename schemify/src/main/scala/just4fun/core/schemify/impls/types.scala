package just4fun.core.schemify.impls

import scala.collection.mutable.ArrayBuffer
import just4fun.core.schemify.impls.typefactory._
import just4fun.core.schemify.{PropType, SchemaType}

/* SCHEMA IMPL */
abstract class Schema[T: Manifest] extends ReachSchema[T] {
	override type P[t, v] = Prop[t, v]
	override protected[this] def newProp[W](implicit t: PropType[W]): Prop[T, W] = new P[T, W](this)(t)
}



/* ITERABLE  IMPLs */

class ArrayBufferType[T: Manifest](implicit val elementType: PropType[T]) extends just4fun.core.schemify.impls.IterableType[T, ArrayBuffer[T]] {
	override def create: ArrayBuffer[T] = ArrayBuffer()
	override def addElement(itr: ArrayBuffer[T], e: T): ArrayBuffer[T] = itr += e
}


class ListType[T: Manifest](implicit val elementType: PropType[T]) extends just4fun.core.schemify.impls.IterableType[T, List[T]] {
	override def create: List[T] = List()
	override def addElement(itr: List[T], e: T): List[T] = e :: itr
	override protected def afterBuild(v: List[T]): List[T] = v.reverse
}



/* SCHEMA TYPE Extension */

class Prop[O, T] protected[impls](override val schema: ReachSchema[O])(implicit override val typ: PropType[T]) extends just4fun.core.schemify.Prop[O, T](schema)

abstract class ReachSchema[T: Manifest] extends SchemaType[T] {
	lazy val arrayType: ArrayBufferType[T] = {
		def findT(implicit t: ArrayBufferType[T] = null) = t
		findT match {case null => new ArrayBufferType[T]; case t => t}
	}
	def createFromJson(v: String): T = createFrom(v)(JsonReader)
	def createFromBytes(v: Array[Byte]): T = createFrom(v)(ByteReader)
	def createFromIterable(v: Iterable[Any]): T = createFrom(v)(StructReader)
	
	def valuesToJsonArray(v: T): String = valuesTo(v)(JsonArrayWriter)
	def valuesToJsonMap(v: T): String = valuesTo(v)(JsonMapWriter)
	def valuesToBytes(v: T): Array[Byte] = valuesTo(v)(ByteWriter)
	def valuesToArray(v: T): IndexedSeq[_] = valuesTo(v)(StructArrayWriter).asInstanceOf[IndexedSeq[_]]
	def valuesToMap(v: T): collection.Map[String, _] = valuesTo(v)(StructMapWriter).asInstanceOf[collection.Map[String, _]]
}




/* ITERABLE TYPE Extension */
abstract class IterableType[E, T](implicit ev: T <:< Iterable[E], mfe: Manifest[E], override val mft: Manifest[T]) extends just4fun.core.schemify.IterableType[E, T] {
	def createFromJson(v: String): T = createFrom(v)(JsonReader)
	def createFromBytes(v: Array[Byte]): T = createFrom(v)(ByteReader)
	def createFromIterable(v: Iterable[Any]): T = createFrom(v)(StructReader)
	
	def valuesToBytes(v: T): Array[Byte] = valuesTo(v)(ByteWriter)
	def valuesToJson(v: T, mapBased: Boolean = false): String = {
		valuesTo(v)(if (mapBased) JsonMapWriter else JsonArrayWriter)
	}
	def valuesToStruct(v: T, mapBased: Boolean = false): IndexedSeq[_] = {
		valuesTo(v)(if (mapBased) StructMapWriter else StructArrayWriter).asInstanceOf[IndexedSeq[_]]
	}
}



