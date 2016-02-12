package just4fun.core.schemify.impls.typefactory

import java.nio.charset.StandardCharsets

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

import just4fun.core.schemify._

object ByteTypeFactory {
	
	/* UTILS */
	val TypNUL = 0
	val TypBUL = 1
	val TypINT = 2
	val TypFLT = 3
	val TypSTA = 4
	val TypSTU = 5
	val TypBIN = 6
	val TypOBJ = 7
	val TypARR = 8
	
	// TODO EMPTY Value if size = 0
	val Stop = -1
	val Nul = tokenValue(TypNUL)
	val False = tokenValue(TypBUL)
	val True = tokenValue(TypBUL, 1)
	val ObjBegin = tokenValue(TypOBJ)
	val ObjEnd = tokenValue(TypOBJ, 1)
	val ArrBegin = tokenValue(TypARR)
	val ArrEnd = tokenValue(TypARR, 1)

	private def tokenValue(typ: Int, size: Int = 0): Byte = ((size << 4) | typ).toByte
	
	def getNum(size: Int, b: ReadBuff): Long = {
		size match {
			case 1 => b.read
			case 2 => ((b.read << 8) + (b.read & 0xFF)).toShort
			case 3 => (b.read << 16) + ((b.read & 0xFF) << 8) + (b.read & 0xFF)
			case 4 => (b.read << 24) + ((b.read & 0xFF) << 16) + ((b.read & 0xFF) << 8) + (b.read & 0xFF)
			case 5 => (b.read.toLong << 32) + ((b.read & 0xFFL) << 24) + ((b.read & 0xFFL) << 16) + ((b.read & 0xFFL) << 8) + (b.read & 0xFFL)
			case 6 => (b.read.toLong << 40) + ((b.read & 0xFFL) << 32) + ((b.read & 0xFFL) << 24) + ((b.read & 0xFFL) << 16) + ((b.read & 0xFFL) << 8) + (b.read & 0xFFL)
			case 7 => (b.read.toLong << 48) + ((b.read & 0xFFL) << 40) + ((b.read & 0xFFL) << 32) + ((b.read & 0xFFL) << 24) + ((b.read & 0xFFL) << 16) + ((b.read & 0xFFL) << 8) + (b.read & 0xFFL)
			case 8 => (b.read.toLong << 56) + ((b.read & 0xFFL) << 48) + ((b.read & 0xFFL) << 40) + ((b.read & 0xFFL) << 32) + ((b.read & 0xFFL) << 24) + ((b.read & 0xFFL) << 16) + ((b.read & 0xFFL) << 8) + (b.read & 0xFFL)
			case _ => 0
		}
	}
	def getLong(size: Int, b: ReadBuff): Long = getNum(size, b)
	def getInt(size: Int, b: ReadBuff): Int = getNum(size, b).toInt
	def getShort(size: Int, b: ReadBuff): Short = getNum(size, b).toShort
	def getChar(size: Int, b: ReadBuff): Char = getNum(size, b).toChar
	def getByte(size: Int, b: ReadBuff): Byte = getNum(size, b).toByte
	def getDouble(size: Int, b: ReadBuff): Double = java.lang.Double.longBitsToDouble(getNum(size, b))
	def getFloat(size: Int, b: ReadBuff): Double = java.lang.Float.intBitsToFloat(getNum(size, b).toInt)
	
	
	def setNum(header: Int, buff: WriteBuff): Unit = buff.write(header.toByte)
	def setNum(typ: Int, v: Long, buff: WriteBuff): Unit = {
		if (v >= -128 && v <= 127) write1(typ, v.toByte, buff)
		else if (v >= -32768 && v <= 32767) write2(typ, v.toShort, buff)
		else if (v >= -8388608 && v <= 8388607) write3(typ, v.toInt, buff)
		else if (v >= -2147483648 && v <= 2147483647) write4(typ, v.toInt, buff)
		else if (v >= -549755813888L && v <= 549755813887L) write5(typ, v, buff)
		else if (v >= -140737488355328L && v <= 140737488355327L) write6(typ, v, buff)
		else if (v >= -36028797018963968L && v <= 36028797018963967L) write7(typ, v, buff)
		else write8(typ, v, buff)
	}
	def setNum(typ: Int, v: Int, buff: WriteBuff): Unit = {
		if (v >= -128 && v <= 127) write1(typ, v.toByte, buff)
		else if (v >= -32768 && v <= 32767) write2(typ, v.toShort, buff)
		else if (v >= -8388608 && v <= 8388607) write3(typ, v, buff)
		else write4(typ, v, buff)
	}
	def setNum(typ: Int, v: Short, buff: WriteBuff): Unit = {
		if (v >= -128 && v <= 127) write1(typ, v.toByte, buff)
		else write2(typ, v, buff)
	}
	def setNum(typ: Int, v: Char, buff: WriteBuff): Unit = setNum(typ, v.toShort, buff)
	def setNum(typ: Int, v: Byte, buff: WriteBuff): Unit = write1(typ, v, buff)
	def setNum(typ: Int, v: Float, buff: WriteBuff): Unit = setNum(typ, java.lang.Float.floatToIntBits(v), buff)
	def setNum(typ: Int, v: Double, buff: WriteBuff): Unit = setNum(typ, java.lang.Double.doubleToLongBits(v), buff)
	
	private def write1(typ: Int, v: Byte, buff: WriteBuff): Unit = {
		buff.write(((1 << 4) | typ).toByte)
		buff.write(v)
	}
	private def write2(typ: Int, v: Short, buff: WriteBuff): Unit = {
		buff.write(((2 << 4) | typ).toByte)
		buff.write((v >>> 8).toByte)
		buff.write(v.toByte)
	}
	private def write3(typ: Int, v: Int, buff: WriteBuff): Unit = {
		buff.write(((3 << 4) | typ).toByte)
		buff.write((v >>> 16).toByte)
		buff.write((v >>> 8).toByte)
		buff.write(v.toByte)
	}
	private def write4(typ: Int, v: Int, buff: WriteBuff): Unit = {
		buff.write(((4 << 4) | typ).toByte)
		buff.write((v >>> 24).toByte)
		buff.write((v >>> 16).toByte)
		buff.write((v >>> 8).toByte)
		buff.write(v.toByte)
	}
	private def write5(typ: Int, v: Long, buff: WriteBuff): Unit = {
		buff.write(((5 << 4) | typ).toByte)
		buff.write((v >>> 32).toByte)
		buff.write((v >>> 24).toByte)
		buff.write((v >>> 16).toByte)
		buff.write((v >>> 8).toByte)
		buff.write(v.toByte)
	}
	private def write6(typ: Int, v: Long, buff: WriteBuff): Unit = {
		buff.write(((6 << 4) | typ).toByte)
		buff.write((v >>> 40).toByte)
		buff.write((v >>> 32).toByte)
		buff.write((v >>> 24).toByte)
		buff.write((v >>> 16).toByte)
		buff.write((v >>> 8).toByte)
		buff.write(v.toByte)
	}
	private def write7(typ: Int, v: Long, buff: WriteBuff): Unit = {
		buff.write(((7 << 4) | typ).toByte)
		buff.write((v >>> 48).toByte)
		buff.write((v >>> 40).toByte)
		buff.write((v >>> 32).toByte)
		buff.write((v >>> 24).toByte)
		buff.write((v >>> 16).toByte)
		buff.write((v >>> 8).toByte)
		buff.write(v.toByte)
	}
	private def write8(typ: Int, v: Long, buff: WriteBuff): Unit = {
		buff.write(((8 << 4) | typ).toByte)
		buff.write((v >>> 56).toByte)
		buff.write((v >>> 48).toByte)
		buff.write((v >>> 40).toByte)
		buff.write((v >>> 32).toByte)
		buff.write((v >>> 24).toByte)
		buff.write((v >>> 16).toByte)
		buff.write((v >>> 8).toByte)
		buff.write(v.toByte)
	}
}







/* BUFFERS */

class ReadBuff(bytes: Array[Byte]) {
	private[this] var cursor = -1
	def hasNext: Boolean = cursor + 1 < bytes.length
	def read: Byte = {
		cursor += 1
		bytes(cursor)
	}
	def read(targ: Array[Byte]): Array[Byte] = {
		val len = if (targ.length > bytes.length - cursor + 1) bytes.length - cursor + 1 else targ.length
		Array.copy(bytes, cursor + 1, targ, 0, len)
		cursor += len
		targ
	}
}

class WriteBuff(pageSize: Int = 1024) {
	private[this] var current = new Array[Byte](pageSize)
	private[this] lazy val pages = ArrayBuffer[Array[Byte]]()
	private[this] var index = 0
	private[this] var length = 0

	def write(v: Array[Byte]): Unit = {
		fill(0, v.length)
		// DEFs
		@tailrec def fill(last: Int, left: Int): Unit = {
			if (index >= pageSize) grow()
			val len = if (left > pageSize - index) pageSize - index else left
			Array.copy(v, last, current, index, len)
			index += len
			length += len
			if (left > 0) fill(last + len, left - len)
		}
	}
	def write(v: Byte): Unit = {
		if (index >= pageSize) grow()
		current(index) = v
		index += 1
		length += 1
	}
	private def grow(): Unit = {
		pages += current
		current = new Array[Byte](pageSize)
		index = 0
	}

	def toByteArray: Array[Byte] = {
		val dest = new Array[Byte](length)
		var len = 0
		for (p <- pages.indices) {
			val a = pages(p)
			Array.copy(a, 0, dest, len, a.length)
			len += a.length
		}
		Array.copy(current, 0, dest, len, index)
		dest
	}
	override def toString = {
		val str = new StringBuilder()
		pages.foreach(a => str ++= a.mkString(",") ++= ", ")
		str ++= current.mkString(",")
		str.toString()
	}
	//	def next(v: Boolean): Unit = next(if (v) 1.toByte else 0.toByte)
	//	def next(v: Short): Unit = {
	//		next((v >>> 8).toByte)
	//		next(v.toByte)
	//	}
	//	def next(v: Char): Unit = {
	//		next((v >>> 8).toByte)
	//		next(v.toByte)
	//	}
	//	def next(v: Int): Unit = {
	//		next((v >>> 24).toByte)
	//		next((v >>> 16).toByte)
	//		next((v >>> 8).toByte)
	//		next(v.toByte)
	//	}
	//	def next(v: Long): Unit = {
	//		next((v >>> 56).toByte)
	//		next((v >>> 48).toByte)
	//		next((v >>> 40).toByte)
	//		next((v >>> 32).toByte)
	//		next((v >>> 24).toByte)
	//		next((v >>> 16).toByte)
	//		next((v >>> 8).toByte)
	//		next(v.toByte)
	//	}
	//	def next(v: Float): Unit = next(java.lang.Float.floatToIntBits(v))
	//	def next(v: Double): Unit = next(java.lang.Double.doubleToLongBits(v))
	
	//	def update(ix: Int, v: Byte): Unit = {
	//		if (ix < 0 || ix >= length) throw new ArrayIndexOutOfBoundsException(ix)
	//		val pix = ix / pageSize
	//		val cix = ix % pageSize
	//		if (pix < pages.length) pages(pix)(cix) = v
	//		else current(cix) = v
	//	}
}



/* TOKEN */
class Token(val value: Byte) {
	val size = (value & 0xFF) >> 4
	val typ = ~(-16) & value
}





/* READER */

object ByteReader extends TypeReader[Array[Byte], ReadBuff, Token] {
	import ByteTypeFactory._

	override def toObject[T](v: Array[Byte], typ: SchemaType[T]): T = if (v == null) null.asInstanceOf[T]
	else {
		val reader = new ReadBuff(v)
		readObject(typ, reader, new Token(reader.read))
	}
	override def toArray[E, T](v: Array[Byte], typ: IterableType[E, T]): T = if (v == null) null.asInstanceOf[T]
	else {
		val reader = new ReadBuff(v)
		readArray(typ, reader,  new Token(reader.read))
	}

	override def readObject[T](typ: SchemaType[T], d: ReadBuff, k: Token): T = k.value match {
		case ObjBegin => typ.createIterating { onNext =>
		  var tk = new Token(d.read)
			while (hasNextSubValue(tk.value)) {
				if (!onNext(prop => prop.typ.read(d, tk))) skip(d, tk)
				tk = new Token(d.read)
			}
		}
		case _ => skip(d, k)
	}
	override def readArray[E, T](typ: IterableType[E, T], d: ReadBuff, k: Token): T = k.value match {
		case ArrBegin => typ.build { addNext =>
			var tk = new Token(d.read)
			while (hasNextSubValue(tk.value)) {
				addNext(typ.elementType.read(d, tk))
				tk = new Token(d.read)
			}
		}
		case _ => skip(d, k)
	}
	override def readBytes(d: ReadBuff, k: Token): Any = k.typ match {
		case TypBIN => d.read(new Array[Byte](getInt(k.size, d)))
		case TypNUL => null
		case _ => skip(d, k)
	}
	override def readString(d: ReadBuff, k: Token, ascii: Boolean): Any = readAtomic(d, k)
	override def readLong(d: ReadBuff, k: Token): Any = readAtomic(d, k)
	override def readInt(d: ReadBuff, k: Token): Any = readAtomic(d, k)
	override def readShort(d: ReadBuff, k: Token): Any = readAtomic(d, k)
	override def readChar(d: ReadBuff, k: Token): Any = readAtomic(d, k)
	override def readByte(d: ReadBuff, k: Token): Any = readAtomic(d, k)
	override def readDouble(d: ReadBuff, k: Token): Any = readAtomic(d, k)
	override def readFloat(d: ReadBuff, k: Token): Any = readAtomic(d, k)
	override def readBoolean(d: ReadBuff, k: Token): Any = readAtomic(d, k)
	override def readNull(d: ReadBuff, k: Token): Any = skip(d, k)

	protected[this] def readAtomic(d: ReadBuff, k: Token) = k.typ match {
		case TypSTU | TypSTA => val bytes = new Array[Byte](getInt(k.size, d))
			new String(d.read(bytes), if (k.typ == TypSTA) StandardCharsets.US_ASCII else StandardCharsets.UTF_8)
		case TypINT => getLong(k.size, d)
		case TypFLT => if (k.size == 8) getDouble(k.size, d) else getFloat(k.size, d)
		case TypNUL => null
		case TypBUL => if (k.value == True) true else false
		case _ => skip(d, k)
	}
	protected[this] def skip[T](d: ReadBuff, k: Token): T = {
		k.typ match {
			case TypINT | TypFLT => 0 until k.size foreach (_ => d.read)
			case TypSTA | TypSTU | TypBIN => d.read(new Array[Byte](getInt(k.size, d)))
			case TypOBJ | TypARR =>
				var tk = new Token(d.read)
				while (hasNextSubValue(tk.value)) {
					skip(d, tk)
					tk = new Token(d.read)
				}
			case _ => // TypNUL | TypBUL
		}
		null.asInstanceOf[T]
	}
	protected[this] def hasNextSubValue(tokenValue: Byte): Boolean = {
		tokenValue != ObjEnd && tokenValue != ArrEnd && tokenValue != Stop
	}
	
}






/* WRITER */

object ByteWriter extends TypeWriter[Array[Byte], WriteBuff, Null] {
	import ByteTypeFactory._

	override def fromObject[T](v: T, typ: SchemaType[T]): Array[Byte] = if (v == null) null
	else {
		val writer = new WriteBuff(1024)
		writeObject(typ, v, writer)
		writer.toByteArray
	}
	override def fromArray[E, T](v: T, typ: IterableType[E, T]): Array[Byte] = if (v == null) null
	else {
		val writer = new WriteBuff(1024)
		writeArray(typ, v, writer)
		writer.toByteArray
	}

	override def writeObject[T](typ: SchemaType[T], v: T, d: WriteBuff, k: Null = null): Unit = {
		setNum(ObjBegin, d)
		typ.valuesReadingAll(v) { (p, v) => p.typ.write(v, d, k) }
		setNum(ObjEnd, d)
	}
	override def writeArray[E, T](typ: IterableType[E, T], v: T, d: WriteBuff, k: Null = null): Unit = {
		setNum(ArrBegin, d)
		v.asInstanceOf[Iterable[E]].foreach(e => typ.elementType.write(e, d, k))
		setNum(ArrEnd, d)
	}
	override def writeBytes(v: Array[Byte], d: WriteBuff, k: Null): Unit = {
		setNum(TypBIN, v.length, d)
		d.write(v)
	}
	override def writeString(v: String, d: WriteBuff, k: Null, ascii: Boolean): Unit = {
		val bytes = v.getBytes(if (ascii) StandardCharsets.US_ASCII else StandardCharsets.UTF_8)
		val tp = if (ascii) TypSTA else TypSTU
		setNum(tp, bytes.length, d)
		d.write(bytes)
	}
	override def writeLong(v: Long, d: WriteBuff, k: Null): Unit = setNum(TypINT, v, d)
	override def writeInt(v: Int, d: WriteBuff, k: Null): Unit = setNum(TypINT, v, d)
	override def writeShort(v: Short, d: WriteBuff, k: Null): Unit = setNum(TypINT, v, d)
	override def writeChar(v: Char, d: WriteBuff, k: Null): Unit = setNum(TypINT, v, d)
	override def writeByte(v: Byte, d: WriteBuff, k: Null): Unit = setNum(TypINT, v, d)
	override def writeDouble(v: Double, d: WriteBuff, k: Null): Unit = setNum(TypFLT, v, d)
	override def writeFloat(v: Float, d: WriteBuff, k: Null): Unit = setNum(TypFLT, v, d)
	override def writeBoolean(v: Boolean, d: WriteBuff, k: Null): Unit = setNum(if (v) True else False, d)
	override def writeNull(d: WriteBuff, k: Null): Unit = d.write(Nul)
}

