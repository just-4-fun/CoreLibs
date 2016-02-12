package just4fun.core.schemify.impls.typefactory

import just4fun.core.schemify._

import scala.collection.mutable.OpenHashMap
import scala.collection.mutable.ArrayBuffer

/* READER */
object StructReader extends TypeReader[Iterable[_], Any, Null] {
	def toObject[T](v: Iterable[_], typ: SchemaType[T]): T = v match {
		case v: collection.Map[String, _]@unchecked => typ.createFinding { onFind =>
			v.foreach { case (nm, vl) => onFind(nm, p => toValue(p.typ, vl)) }
		}
		case v: Iterable[_] => typ.createIterating { onNext =>
			v.foreach { vl => onNext(p => toValue(p.typ, vl)) }
		}
		case _ => null.asInstanceOf[T]
	}

	def toArray[E, T](v: Iterable[_], typ: IterableType[E, T]): T = v match {
		case v: Iterable[_] => typ.build { addNext =>
			v.foreach { vl => addNext(toValue(typ.elementType, vl)) }
		}
		case _ => null.asInstanceOf[T]
	}

	def toValue[V](typ: PropType[V], v: Any): Any = typ match {
		case t: SchemaType[V] => v match {
			case v: Iterable[_] => toObject(v, t)
			case _ =>
		}
		case t: IterableType[_, V] => v match {
			case v: Iterable[_] => toArray(v, t)
			case _ =>
		}
		case _ => v
	}

	override def readObject[T](typ: SchemaType[T], d: Any, k: Null = null): T = ???
	override def readArray[E, T](typ: IterableType[E, T], d: Any, k: Null = null): T = ???
	override def readBytes(d: Any, k: Null): Any = ???
	override def readString(d: Any, k: Null, ascii: Boolean): Any = ???
	override def readLong(d: Any, k: Null): Any = ???
	override def readInt(d: Any, k: Null): Any = ???
	override def readShort(d: Any, k: Null): Any = ???
	override def readChar(d: Any, k: Null): Any = ???
	override def readByte(d: Any, k: Null): Any = ???
	override def readDouble(d: Any, k: Null): Any = ???
	override def readFloat(d: Any, k: Null): Any = ???
	override def readBoolean(d: Any, k: Null): Any = ???
	override def readNull(d: Any, k: Null): Any = ???
}






/* Json Array IMPL */
object StructArrayWriter extends StructWriter(true)


/* Json Map IMPL */
object StructMapWriter extends StructWriter(false)



/* WRITER */

class StructWriter(arrayBased: Boolean) extends TypeWriter[Iterable[_], Null, Null] {
	def fromObject[T](v: T, typ: SchemaType[T]): Iterable[_] = {
		if (v == null) null
		else if (arrayBased) {
			val array = ArrayBuffer[Any]()
			typ.valuesReadingAll(v) { (p, v2) => array += fromValue(p.typ, v2) }
			array
		}
		else {
			val map = OpenHashMap[String, Any]()
			typ.valuesReading(v, typ.propsReal) { (n, p, v2) => map += (p.name -> fromValue(p.typ, v2)) }
			map
		}
	}
	def fromArray[E, T](v: T, typ: IterableType[E, T]): Iterable[_] = {
		if (v == null) null
		else {
			val array = ArrayBuffer[Any]()
			v.asInstanceOf[Iterable[E]].foreach(e => array += fromValue(typ.elementType, e))
			array
		}
	}
	
	def fromValue[V](typ: PropType[V], v: V): Any = typ match {
		case t: SchemaType[V] => fromObject(v, t)
		case t: IterableType[_, V] => fromArray(v, t)
		case _ => v
	}
	override def writeObject[T](typ: SchemaType[T], v: T, d: Null, k: Null): Unit = ???
	override def writeString(v: String, d: Null, k: Null, ascii: Boolean): Unit = ???
	override def writeFloat(v: Float, d: Null, k: Null): Unit = ???
	override def writeArray[E, T](typ: IterableType[E, T], v: T, d: Null, k: Null): Unit = ???
	override def writeDouble(v: Double, d: Null, k: Null): Unit = ???
	override def writeShort(v: Short, d: Null, k: Null): Unit = ???
	override def writeInt(v: Int, d: Null, k: Null): Unit = ???
	override def writeBoolean(v: Boolean, d: Null, k: Null): Unit = ???
	override def writeBytes(v: Array[Byte], d: Null, k: Null): Unit = ???
	override def writeChar(v: Char, d: Null, k: Null): Unit = ???
	override def writeLong(v: Long, d: Null, k: Null): Unit = ???
	override def writeNull(d: Null, k: Null): Unit = ???
	override def writeByte(v: Byte, d: Null, k: Null): Unit = ???
}
