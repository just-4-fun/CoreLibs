package just4fun.core.schemify

trait TypeReader[SRC, DATA, KEY] {
	implicit protected[this] val _reader = this

	def toObject[T](v: SRC, typ: SchemaType[T]): T
	def toArray[E, T](v: SRC, typ: IterableType[E, T]): T

	def readObject[T](typ: SchemaType[T], d: DATA, k: KEY): T
	def readArray[E, T](typ: IterableType[E, T], d: DATA, k: KEY): T
	def readString(d: DATA, k: KEY, ascii: Boolean = false): Any
	def readBytes(d: DATA, k: KEY): Any
	def readLong(d: DATA, k: KEY): Any
	def readInt(d: DATA, k: KEY): Any
	def readShort(d: DATA, k: KEY): Any
	def readChar(d: DATA, k: KEY): Any
	def readByte(d: DATA, k: KEY): Any
	def readDouble(d: DATA, k: KEY): Any
	def readFloat(d: DATA, k: KEY): Any
	def readBoolean(d: DATA, k: KEY): Any
	def readNull(d: DATA, k: KEY): Any
}

trait TypeWriter[SRC, DATA, KEY] {
	implicit protected[this] val _writer = this

	def fromObject[T](v: T, typ: SchemaType[T]): SRC
	def fromArray[E, T](v: T, typ: IterableType[E, T]): SRC

	def writeObject[T](typ: SchemaType[T], v: T, d: DATA, k: KEY): Unit
	def writeArray[E, T](typ: IterableType[E, T], v: T, d: DATA, k: KEY): Unit
	def writeBytes(v: Array[Byte], d: DATA, k: KEY): Unit
	def writeString(v: String, d: DATA, k: KEY, ascii: Boolean = false): Unit
	def writeLong(v: Long, d: DATA, k: KEY): Unit
	def writeInt(v: Int, d: DATA, k: KEY): Unit
	def writeShort(v: Short, d: DATA, k: KEY): Unit
	def writeChar(v: Char, d: DATA, k: KEY): Unit
	def writeByte(v: Byte, d: DATA, k: KEY): Unit
	def writeDouble(v: Double, d: DATA, k: KEY): Unit
	def writeFloat(v: Float, d: DATA, k: KEY): Unit
	def writeBoolean(v: Boolean, d: DATA, k: KEY): Unit
	def writeNull(d: DATA, k: KEY): Unit
}
