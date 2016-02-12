package just4fun.core.schemify.impls.typefactory

import java.io.StringWriter

import com.fasterxml.jackson.core.JsonToken._
import com.fasterxml.jackson.core.{JsonFactory, JsonGenerator, JsonParser, JsonToken}
import just4fun.core.schemify._

private[typefactory] object JsonTypeFactory {
	val factory = new JsonFactory
	def parser(json: String) = factory.createParser(json)
	def generator = factory.createGenerator(new StringWriter)
}



/* READER */
object JsonReader extends TypeReader[String, JsonParser, JsonToken] {

	override def toObject[T](v: String, typ: SchemaType[T]): T = if (v == null) null.asInstanceOf[T]
	else {
		val parser = JsonTypeFactory.parser(v)
		val res = readObject(typ, parser, parser.nextValue())
		parser.close()
		res
	}
	override def toArray[E, T](v: String, typ: IterableType[E, T]): T = if (v == null) null.asInstanceOf[T]
	else {
		val parser = JsonTypeFactory.parser(v)
		val res = readArray(typ, parser, parser.nextValue())
		parser.close()
		res
	}

	override def readObject[T](typ: SchemaType[T], d: JsonParser, k: JsonToken): T = k match {
		//		case START_OBJECT =>
		//			val values = new Array[Any](typ.propsSize)
		//			while (nextValue) {
		//				typ.propsMap.get(parser.getCurrentName) match {
		//					case Some(p) => values(p.index) = p.typ.readType()
		//					case None => skip
		//				}
		//			}
		//			typ.createFromValues(values)
		//		case START_ARRAY =>
		//			val values = new Array[Any](typ.propsSize)
		//			var n = 0
		//			while (nextValue) if (n < typ.propsSize) {
		//				val p = typ.props(n)
		//				values(n) = p.typ.readType()
		//				n += 1
		//			} else skip
		//			typ.createFromValues(values)
		case START_OBJECT => typ.createFinding { onFind =>
			var tk = d.nextValue()
			while (hasNext(tk)) {
				if (!onFind(d.getCurrentName, prop => prop.typ.read(d, tk))) skip(d)
				tk = d.nextValue()
			}
		}
		case START_ARRAY => typ.createIterating { onNext =>
			var tk = d.nextValue()
			while (hasNext(tk)) {
				if (!onNext(prop => prop.typ.read(d, tk))) skip(d)
				tk = d.nextValue()
			}
		}
		case _ => skip(d)
	}
	override def readArray[E, T](typ: IterableType[E, T], d: JsonParser, k: JsonToken): T = k match {
		case START_ARRAY => typ.build { addNext =>
			var tk = d.nextValue()
			while (hasNext(tk)) {
				addNext(typ.elementType.read(d, tk))
				tk = d.nextValue()
			}
		}
		case _ => skip(d)
	}
	override def readBytes(d: JsonParser, k: JsonToken): Any = k match {
		case VALUE_STRING => d.getBinaryValue
		case _ => skip(d)
	}
	override def readString(d: JsonParser, k: JsonToken, ascii: Boolean): Any = readAtomic(d, k)
	override def readLong(d: JsonParser, k: JsonToken): Any = readAtomic(d, k)
	override def readInt(d: JsonParser, k: JsonToken): Any = readAtomic(d, k)
	override def readShort(d: JsonParser, k: JsonToken): Any = readAtomic(d, k)
	override def readChar(d: JsonParser, k: JsonToken): Any = readAtomic(d, k)
	override def readByte(d: JsonParser, k: JsonToken): Any = readAtomic(d, k)
	override def readDouble(d: JsonParser, k: JsonToken): Any = readAtomic(d, k)
	override def readFloat(d: JsonParser, k: JsonToken): Any = readAtomic(d, k)
	override def readBoolean(d: JsonParser, k: JsonToken): Any = readAtomic(d, k)
	override def readNull(d: JsonParser, k: JsonToken): Any = skip(d)

	protected[this] def readAtomic(d: JsonParser, k: JsonToken) = k match {
		case VALUE_STRING => d.getText
		case VALUE_NUMBER_INT => d.getValueAsLong
		case VALUE_NUMBER_FLOAT => d.getValueAsDouble
		case VALUE_NULL => null
		case VALUE_FALSE => false
		case VALUE_TRUE => true
		case _ => skip(d)
	}
	protected[this] def skip[T](d: JsonParser): T = { d.skipChildren(); null.asInstanceOf[T] }
	protected[this] def hasNext(token: JsonToken): Boolean = {
		token != END_ARRAY && token != END_OBJECT && token != null && token != NOT_AVAILABLE
	}
}




/* Json Array IMPL */
object JsonArrayWriter extends JsonWriter(true)


/* Json Map IMPL */
object JsonMapWriter extends JsonWriter(false)


/* WRITER */
class JsonWriter(arrayBased: Boolean) extends TypeWriter[String, JsonGenerator, Null] {

	override def fromObject[T](v: T, typ: SchemaType[T]): String = if (v == null) null
	else {
		val gener = JsonTypeFactory.generator
		writeObject(typ, v, gener)
		gener.close()
		gener.getOutputTarget.toString
	}
	override def fromArray[E, T](v: T, typ: IterableType[E, T]): String = if (v == null) null
	else {
		val gener = JsonTypeFactory.generator
		writeArray(typ, v, gener)
		gener.close()
		gener.getOutputTarget.toString
	}

	override def writeObject[T](typ: SchemaType[T], v: T, d: JsonGenerator, k: Null = null): Unit = {
		if (arrayBased) {
			d.writeStartArray()
			typ.valuesReadingAll(v) { (p, v) => p.typ.write(v, d, k) }
			d.writeEndArray()
		}
		else {
			d.writeStartObject()
			typ.valuesReading(v, typ.propsReal) { (n, p, v) =>
				d.writeFieldName(p.name)
				p.typ.write(v, d, k)
			}
			d.writeEndObject()
		}
	}
	override def writeArray[E, T](typ: IterableType[E, T], v: T, d: JsonGenerator, k: Null = null): Unit = {
		d.writeStartArray()
		v.asInstanceOf[Iterable[E]].foreach(e => typ.elementType.write(e, d, k))
		d.writeEndArray()
	}

	override def writeBytes(v: Array[Byte], d: JsonGenerator, k: Null): Unit = d.writeBinary(v)
	override def writeString(v: String, d: JsonGenerator, k: Null, ascii: Boolean): Unit = d.writeString(v)
	override def writeLong(v: Long, d: JsonGenerator, k: Null): Unit = d.writeNumber(v)
	override def writeInt(v: Int, d: JsonGenerator, k: Null): Unit = d.writeNumber(v)
	override def writeShort(v: Short, d: JsonGenerator, k: Null): Unit = d.writeNumber(v)
	override def writeChar(v: Char, d: JsonGenerator, k: Null): Unit = d.writeString(v.toString)
	override def writeByte(v: Byte, d: JsonGenerator, k: Null): Unit = d.writeNumber(v)
	override def writeDouble(v: Double, d: JsonGenerator, k: Null): Unit = d.writeNumber(v)
	override def writeFloat(v: Float, d: JsonGenerator, k: Null): Unit = d.writeNumber(v)
	override def writeBoolean(v: Boolean, d: JsonGenerator, k: Null): Unit = d.writeBoolean(v)
	override def writeNull(d: JsonGenerator, k: Null): Unit = d.writeNull()

}