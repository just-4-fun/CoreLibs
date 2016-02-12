package just4fun.core.schemify

import scala.collection.mutable.ArrayBuffer
import scala.language.experimental.macros
import scala.language.higherKinds
import just4fun.core.schemify.PropType.NullType
import just4fun.core.debug.DebugUtils._

/* TYPES */
// 0 level
sealed abstract class PropType[T: Manifest] {
	implicit protected val thisTyp: this.type = this
	val mft = implicitly[Manifest[T]]
	def eval(v: Any): T = v match {
		case v: T => v
		case null => null.asInstanceOf[T]
		case v => PropType.logCastError(s"PropType[$mft]", v); null.asInstanceOf[T]
	}
	def copy(v: T, deep: Boolean = false): T = v
	def equal(v1: T, v2: T): Boolean = v1 == v2
	def differ(v1: T, v2: T): Boolean = !equal(v1, v2)
	def read[D, K](d: D = null.asInstanceOf[D], k: K = null.asInstanceOf[K])(implicit r: TypeReader[_, D, K]): Any
	def write[D, K](v: T, d: D = null.asInstanceOf[D], k: K = null.asInstanceOf[K])(implicit w: TypeWriter[_, D, K]): Unit
}

/* SUB TYPES */
// 1 level
sealed abstract class AtomicType[T: Manifest] extends PropType[T]

sealed abstract class ValueType[T: Manifest] extends PropType[T] {
	type V
	def create(v: V): T
	def value(v: T): V
	override def copy(v: T, deep: Boolean = false): T = if (v == null) null.asInstanceOf[T] else create(value(v))
	override def equal(v1: T, v2: T): Boolean = v1 == v2 || (v1 != null && v2 != null && value(v1) == value(v2))
}

sealed abstract class FormattedType[T: Manifest] extends PropType[T] {
	def create: T
	def createFrom[S](v: S)(implicit reader: TypeReader[S, _, _]): T
	def valuesTo[S](v: T)(implicit writer: TypeWriter[S, _, _]): S
}

class OptionType[T](implicit val elementType: PropType[T], mfe: Manifest[T], override val mft: Manifest[Option[T]]) extends PropType[Option[T]] {
	override def eval(v: Any): Option[T] = v match {
		case v: Option[T] => v
		case v: T => Option(v)
		case null => None
		case _ => Option(elementType.eval(v))
	}
	override def copy(v: Option[T], deep: Boolean = false): Option[T] = v match {
		case Some(v) => Option(elementType.copy(v, deep))
		case _ => v
	}
	override def equal(v1: Option[T], v2: Option[T]): Boolean = {
		v1 == v2 || (v1 != null && v2 != null && v1.nonEmpty && v2.nonEmpty && elementType.equal(v1.get, v2.get))
	}
	def read[D, K](d: D, k: K)(implicit r: TypeReader[_, D, K]): Any = elementType.read(d, k)
	def write[D, K](v: Option[T], d: D, k: K)(implicit w: TypeWriter[_, D, K]): Unit = if (v == null) w.writeNull(d, k) else elementType.write(v.getOrElse(null.asInstanceOf[T]), d, k)
}



/* BASE Sub Types */
// 2 level
abstract class StringValueType[T: Manifest] extends ValueType[T] {
	val asciiEncoded = false
	type V = String
	override def eval(v: Any): T = v match {
		case v: T => v
		case v: String => create(v)
		case null => null.asInstanceOf[T]
		case v => PropType.logCastError(s"StringValueType[$mft]", v); null.asInstanceOf[T]
	}
	def read[D, K](d: D, k: K)(implicit r: TypeReader[_, D, K]): Any = r.readString(d, k, asciiEncoded)
	def write[D, K](v: T, d: D, k: K)(implicit w: TypeWriter[_, D, K]): Unit = if (v == null) w.writeNull(d, k) else w.writeString(value(v), d, k, asciiEncoded)
}

abstract class BytesValueType[T: Manifest] extends ValueType[T] {
	type V = Array[Byte]
	override def equal(v1: T, v2: T): Boolean = v1 == v2 || (v1 != null && v2 != null && java.util.Arrays.equals(value(v1), value(v2)))
	override def copy(v: T, deep: Boolean = false): T = if (v == null) null.asInstanceOf
	else {
		val b = value(v);
		create(java.util.Arrays.copyOf(b, b.length))
	}
	override def eval(v: Any): T = v match {
		case v: T => v
		case v: Array[Byte] => create(v)
		case null => null.asInstanceOf[T]
		case v => PropType.logCastError(s"BytesValueType[$mft]", v); null.asInstanceOf[T]
	}
	def read[D, K](d: D, k: K)(implicit r: TypeReader[_, D, K]): Any = r.readBytes(d, k)
	def write[D, K](v: T, d: D, k: K)(implicit w: TypeWriter[_, D, K]): Unit = if (v == null) w.writeNull(d, k) else w.writeBytes(value(v), d, k)
}

/* Formatted Sub Types */
// 2 level
abstract class IterableType[E, T](implicit ev: T <:< Iterable[E], mfe: Manifest[E], override val mft: Manifest[T]) extends FormattedType[T] {
	val elementType: PropType[E]
	def addElement(itr: T, e: E): T
	def build(code: (Any => Unit) => Unit): T = {
		var inst: T = create
		def addNext(e: Any): Unit = inst = addElement(inst, elementType.eval(e))
		code(addNext)
		afterBuild(inst)
	}
	protected def afterBuild(v: T): T = v
	override def copy(v: T, deep: Boolean = false): T = {
		(if (v == null) null else if (deep) v.map(e => elementType.copy(e, true)) else v.map(e => e)).asInstanceOf[T]
	}
	override def equal(v1: T, v2: T): Boolean = v1 == v2 || {
		if (v1 != null && v2 != null) {
			val itr1 = v1.iterator
			val itr2 = v2.iterator
			while (itr1.hasNext && itr2.hasNext) if (elementType.differ(itr1.next(), itr2.next())) return false
			!itr1.hasNext && !itr2.hasNext
		} else false
	}
	def read[D, K](d: D, k: K)(implicit r: TypeReader[_, D, K]): Any = r.readArray(this, d, k)
	def write[D, K](v: T, d: D, k: K)(implicit w: TypeWriter[_, D, K]): Unit = if (v == null) w.writeNull(d, k) else w.writeArray(this, v, d, k)
	override def createFrom[S](v: S)(implicit reader: TypeReader[S, _, _]): T = reader.toArray(v, this)
	override def valuesTo[S](v: T)(implicit writer: TypeWriter[S, _, _]): S = writer.fromArray(v, this)
}

/* SCHEMA */
/** See [[SchemaType]] below */





/* CONCRETE Types*/
// 3 level
abstract class AutoSchema[T: Manifest] extends SchemaType[T] {
	override type P[t, v] = Prop[t, v]
	override protected[this] def newProp[V](implicit t: PropType[V]): Prop[T, V] = new P[T, V](this)(t)
}





/* IMPLICITS & ATOMICS */
// 2 level
object PropType  {
	def logCastError(typ: String, v: Any) { logE(s"Can't convert  $v  to  $typ") }
	def castNumber[N](castCode: => N)(v: String, recover: Double => N): N = try {
		castCode
	} catch {case _: NumberFormatException => recover(string2double(v))}

	val NumPattern = """[\D&&[^\.,\-]]*(\-?[\D&&[^\.,]]*)(\d*)([\.,]*)(\d*).*""".r
	def string2double(v: String): Double = try {
		var NumPattern(sig, r, pt, f) = v
		//		logd("string2double", s"($sig)($r)($pt)($f)")
		var mult = if (sig.endsWith("-")) -1 else 1
		if (r.length == 0) r = "0"
		if (f.length == 0) f = "0"
		if (pt.length > 1) {if (r != "0") f = "0" else mult = 1}
		s"$r.$f".toDouble * mult
	} catch {case e: Throwable => logCastError("Number", v); 0d}

	def clean(): Unit = {
		// TODO clear soon after app startup
		impliMap.clear()
	}

	private[schemify] val impliMap = collection.mutable.Map[Any, PropType[_]]()

	def genSchema[T]: SchemaType[T] = macro just4fun.core.schemify.SchemifyMacro.genAutoSchema[T]
	implicit def genPearlSchema[T <: Pearl]: SchemaType[T] = macro just4fun.core.schemify.SchemifyMacro.genAutoSchema[T]

	implicit def type2option[T: Manifest](implicit typ: PropType[T]): OptionType[T] = {
		val mft = implicitly[Manifest[Option[T]]]
		impliMap.getOrElseUpdate(mft, new OptionType[T]).asInstanceOf[OptionType[T]]
	}

	object NullType extends AtomicType[Null] {
		override def eval(v: Any) = null
		override def copy(v: Null, deep: Boolean = false): Null = null
		override def equal(o1: Null, o2: Null): Boolean = true
		def read[D, K](d: D, k: K)(implicit r: TypeReader[_, D, K]): Any = r.readNull(d, k)
		def write[D, K](v: Null, d: D, k: K)(implicit w: TypeWriter[_, D, K]): Unit = w.writeNull(d, k)
	}
	implicit object StringType extends AtomicType[String] {
		override def eval(v: Any): String = v match {
			case v: String => v
			case null => null
			case v: Long => v.toString
			case v: Double => v.toString
			case v: Int => v.toString
			case v: Float => v.toString
			case v: Boolean => v.toString
			case v: Short => v.toString
			case v: Byte => v.toString
			case v: Char => v.toString
			case _ => logCastError("String", v); null
		}
		def read[D, K](d: D, k: K)(implicit r: TypeReader[_, D, K]): Any = r.readString(d, k)
		def write[D, K](v: String, d: D, k: K)(implicit w: TypeWriter[_, D, K]): Unit = if (v == null) w.writeNull(d, k) else w.writeString(v, d, k)
	}
	implicit object LongType extends AtomicType[Long] {
		override def eval(v: Any): Long = v match {
			case v: Long => v
			case null => 0L
			case v: String => castNumber(v.toLong)(v, eval)
			case v: Int => v.toLong
			case v: Double => v.toLong
			case v: Float => v.toLong
			case v: Boolean => if (v) 1L else 0L
			case v: Short => v.toLong
			case v: Byte => v.toLong
			case v: Char => v.toLong
			case _ => logCastError("Long", v); 0L
		}
		def read[D, K](d: D, k: K)(implicit r: TypeReader[_, D, K]): Any = r.readLong(d, k)
		def write[D, K](v: Long, d: D, k: K)(implicit w: TypeWriter[_, D, K]): Unit = w.writeLong(v, d, k)
	}
	implicit object IntType extends AtomicType[Int] {
		override def eval(v: Any): Int = v match {
			case v: Int => v
			case null => 0
			case v: Long => v.toInt
			case v: String => castNumber(v.toInt)(v, eval)
			case v: Double => v.toInt
			case v: Float => v.toInt
			case v: Boolean => if (v) 1 else 0
			case v: Short => v.toInt
			case v: Byte => v.toInt
			case v: Char => v.toInt
			case _ => logCastError("Int", v); 0
		}
		def read[D, K](d: D, k: K)(implicit r: TypeReader[_, D, K]): Any = r.readInt(d, k)
		def write[D, K](v: Int, d: D, k: K)(implicit w: TypeWriter[_, D, K]): Unit = w.writeInt(v, d, k)
	}
	implicit object ShortType extends AtomicType[Short] {
		override def eval(v: Any): Short = v match {
			case v: Short => v
			case null => 0
			case v: Long => v.toShort
			case v: Int => v.toShort
			case v: String => castNumber(v.toShort)(v, eval)
			case v: Double => v.toShort
			case v: Float => v.toShort
			case v: Boolean => if (v) 1 else 0
			case v: Byte => v.toShort
			case v: Char => v.toShort
			case _ => logCastError("Short", v); 0
		}
		def read[D, K](d: D, k: K)(implicit r: TypeReader[_, D, K]): Any = r.readShort(d, k)
		def write[D, K](v: Short, d: D, k: K)(implicit w: TypeWriter[_, D, K]): Unit = w.writeShort(v, d, k)
	}
	implicit object ByteType extends AtomicType[Byte] {
		override def eval(v: Any): Byte = v match {
			case v: Byte => v
			case null => 0
			case v: Long => v.toByte
			case v: Double => v.toByte
			case v: Int => v.toByte
			case v: Float => v.toByte
			case v: String => castNumber(v.toByte)(v, eval)
			case v: Boolean => if (v) 1 else 0
			case v: Short => v.toByte
			case v: Char => v.toByte
			case _ => logCastError("Byte", v); 0
		}
		def read[D, K](d: D, k: K)(implicit r: TypeReader[_, D, K]): Any = r.readByte(d, k)
		def write[D, K](v: Byte, d: D, k: K)(implicit w: TypeWriter[_, D, K]): Unit = w.writeByte(v, d, k)
	}
	implicit object CharType extends AtomicType[Char] {
		override def eval(v: Any): Char = v match {
			case v: Char => v
			case null => '\u0000'
			case v: Long => v.toChar
			case v: String => if (v.length > 0) v.charAt(0) else '\u0000'
			case v: Int => v.toChar
			case v: Double => v.toChar
			case v: Float => v.toChar
			case v: Boolean => if (v) '1' else '0'
			case v: Short => v.toChar
			case v: Byte => v.toChar
			case _ => logCastError("Char", v); '\u0000'
		}
		def read[D, K](d: D, k: K)(implicit r: TypeReader[_, D, K]): Any = r.readChar(d, k)
		def write[D, K](v: Char, d: D, k: K)(implicit w: TypeWriter[_, D, K]): Unit = w.writeChar(v, d, k)
	}

	implicit object DoubleType extends AtomicType[Double] {
		override def eval(v: Any): Double = v match {
			case v: Double => v
			case null => 0d
			case v: String => castNumber(v.toDouble)(v, eval)
			case v: Float => v.toDouble
			case v: Long => v.toDouble
			case v: Int => v.toDouble
			case v: Boolean => if (v) 1d else 0d
			case v: Short => v.toDouble
			case v: Byte => v.toDouble
			case v: Char => v.toDouble
			case _ => logCastError("Double", v); 0d
		}
		def read[D, K](d: D, k: K)(implicit r: TypeReader[_, D, K]): Any = r.readDouble(d, k)
		def write[D, K](v: Double, d: D, k: K)(implicit w: TypeWriter[_, D, K]): Unit = w.writeDouble(v, d, k)
	}
	implicit object FloatType extends AtomicType[Float] {
		override def eval(v: Any): Float = v match {
			case v: Float => v
			case null => 0f
			case v: Double => v.toFloat
			case v: String => castNumber(v.toFloat)(v, eval)
			case v: Long => v.toFloat
			case v: Int => v.toFloat
			case v: Boolean => if (v) 1f else 0f
			case v: Short => v.toFloat
			case v: Byte => v.toFloat
			case v: Char => v.toFloat
			case _ => logCastError("Float", v); 0f
		}
		def read[D, K](d: D, k: K)(implicit r: TypeReader[_, D, K]): Any = r.readFloat(d, k)
		def write[D, K](v: Float, d: D, k: K)(implicit w: TypeWriter[_, D, K]): Unit = w.writeFloat(v, d, k)
	}
	implicit object BooleanType extends AtomicType[Boolean] {
		val falses = "" :: "0" :: "null" :: "0.0" :: "0,0" :: Nil
		override def eval(v: Any) = v match {
			case v: Boolean => v
			case null => false
			case 0 => false
			case 1 => true
			case "false" => false
			case "true" => true
			case v: String => !falses.contains(v.toLowerCase)
			case _ => logCastError("Boolean", v); false
		}
		def read[D, K](d: D, k: K)(implicit r: TypeReader[_, D, K]): Any = r.readBoolean(d, k)
		def write[D, K](v: Boolean, d: D, k: K)(implicit w: TypeWriter[_, D, K]): Unit = w.writeBoolean(v, d, k)
	}
	implicit object BytesType extends AtomicType[Array[Byte]] {
		override def copy(v: Array[Byte], deep: Boolean = false): Array[Byte] = if (v == null) null else java.util.Arrays.copyOf(v, v.length)
		override def equal(v1: Array[Byte], v2: Array[Byte]): Boolean = v1 == v2 || (v1 != null && v2 != null && java.util.Arrays.equals(v1, v2))
		def read[D, K](d: D, k: K)(implicit r: TypeReader[_, D, K]): Any = r.readBytes(d, k)
		def write[D, K](v: Array[Byte], d: D, k: K)(implicit w: TypeWriter[_, D, K]): Unit = if (v == null) w.writeNull(d, k) else w.writeBytes(v, d, k)
	}

	implicit lazy val optBytes = new OptionType[Array[Byte]]
	implicit lazy val optString = new OptionType[String]
	implicit lazy val optLong = new OptionType[Long]
	implicit lazy val optInt = new OptionType[Int]
	implicit lazy val optDouble = new OptionType[Double]
	implicit lazy val optFloat = new OptionType[Float]
	implicit lazy val optBoolean = new OptionType[Boolean]
}






/* SCHEMA Impl */

abstract class SchemaType[T: Manifest] extends FormattedType[T] {
	/** Dummy type */
	type V
	/** Override to extend Prop class. */
	type P[t <: T, v] <: Prop[t, v]
	// Schemify Macro annotation overrides _schemified_ = true
	lazy protected[this] val _schemified_ = false
	if (!_schemified_) throwError()
	val schemaName: String = getClass.getSimpleName
	var propsSize = 0
	val props = ArrayBuffer[P[T, V]]()
	lazy val propsReal: List[P[T, V]] = props.filter(p => !p.stub).toList
	lazy val propsMap = collection.mutable.OpenHashMap[String, P[T, V]]() ++ propsReal.map(p => p.name -> p)

	def findProp[W](name: String)(implicit m: Manifest[W]): Option[P[T, W]] = {
		propsMap.get(name) match {
			case Some(p) if m.eq(manifest[Nothing]) || m <:< p.typ.mft => Some(p.asInstanceOf[P[T, W]])
			case _ => None
		}
	}
	protected[this] def newProp[W](implicit t: PropType[W]): P[T, W]
	/** Override to extend Prop class. */
	protected[this] def PROP[W](implicit t: PropType[W]): P[T, W] = {
		val p = newProp(t)
		p.index = propsSize
		props += p.asInstanceOf[P[T, V]]
		propsSize = props.size
		p
	}
	protected[this] def STUB: P[T, Null] = PROP(NullType)

	def read[D, K](d: D, k: K)(implicit r: TypeReader[_, D, K]): Any = r.readObject(this, d, k)
	def write[D, K](v: T, d: D, k: K)(implicit w: TypeWriter[_, D, K]): Unit = if (v == null) w.writeNull(d, k) else w.writeObject(this, v, d, k)
	// can be generated by macro but user can forget to define it for extension of abstract schema
	override def create: T = throwError("method 'instance' should be overridden.")
	override def equal(v1: T, v2: T): Boolean = v1 == v2 || {
		if (v1 != null && v2 != null) !props.exists { p => p.differ(p.getter(v1), p.getter(v2)) } else false
	}
	/** Overridden in Schemify macro */
	override def copy(obj: T, deep: Boolean = false): T = if (obj == null) obj
	else {
		logW(s"${getClass}.copyObject method should be overridden in Schemify macro")
		beforeValuesGet(obj)
		val newObj = create
		props.foreach { p =>
			p.setter(newObj, if (deep) p.copy(p.getter(obj)) else p.getter(obj))
		}
		afterValuesSet(newObj)
		newObj
	}

	// Override to do something with operand after values have set
	protected[this] def afterValuesSet(obj: T): Unit = {}
	// Override to do something with operand before values have gotten
	protected[this] def beforeValuesGet(obj: T): Unit = {}


	/* Set values */
	override def createFrom[S](v: S)(implicit reader: TypeReader[S, _, _]): T = {
		reader.toObject(v, this)
	}
	def create(vals: Any*): T = {
		val values = new Array[Any](propsSize)
		var n = 0
		vals.foreach { v => if (n < propsSize) {values(n) = v; n += 1} }
		createFromValues(values)
	}
	/** Writes values iterating over all schema props. */
	def createReadingAll(read: P[T, V] => Any): T = {
		val values = new Array[Any](propsSize)
		props.foreach { p => values(p.index) = read(p) }
		createFromValues(values)
	}
	/** Writes values iterating over supplied props. */
	def createReading(props: Iterable[P[T, _]])(read: (Int, P[T, V]) => Any): T = {
		val values = new Array[Any](propsSize)
		var n = 0
		props.foreach { p => values(p.index) = read(n, p.asInstanceOf[P[T, V]]); n += 1 }
		createFromValues(values)
	}
	/** Writes values based on external sequence-like iterable over all props in schema defined order. */
	def createIterating(iterate: ((P[T, V] => Any) => Boolean) => Unit): T = {
		val values = new Array[Any](propsSize)
		var n = 0
		def onNextProp(write: P[T, V] => Any): Boolean = {
			val doNext = n < propsSize
			if (doNext) values(n) = write(props(n))
			n += 1
			doNext
		}
		// EXEC
		iterate(onNextProp)
		createFromValues(values)
	}
	/** Writes values based on external map-like iterable over names of props. */
	def createFinding(iterate: (((String, P[T, V] => Any) => Boolean) => Unit)): T = {
		val values = new Array[Any](propsSize)
		def onFindProp(name: String, write: P[T, V] => Any): Boolean =
			propsMap.get(name) match {
				case Some(p) => values(p.index) = write(p); true
				case None => false
			}
		// EXEC
		iterate(onFindProp)
		createFromValues(values)
	}
	/** Overridden in Schemify macro */
	def createFromValues(vals: Array[_]): T = {
		logW(s"${getClass}.constructFromValues method should be overridden in Schemify macro")
		val obj = create
		var n = 0
		props.foreach { p => p.setter(obj, p(vals(n))); n += 1 }
		afterValuesSet(obj)
		obj
	}

	/* Get values */
	def values(v: T): Array[_] = {
		val values = new Array[Any](propsSize)
		var n = 0
		props.foreach { p => values(n) = p.getter(v); n += 1 }
		values
	}
	override def valuesTo[S](v: T)(implicit writer: TypeWriter[S, _, _]): S = {
		writer.fromObject(v, this)
	}
	/** Reads values iterating over all schema props. */
	def valuesReadingAll(obj: T)(read: (P[T, V], V) => Unit): Unit = if (obj != null) {
		beforeValuesGet(obj)
		var n = 0
		props.foreach { p => read(p, p.getter(obj)); n += 1 }
	}
	/** Reads values iterating over supplied props. */
	def valuesReading(obj: T, props: Iterable[P[T, _]])(read: (Int, P[T, V], V) => Unit): Unit = if (obj != null) {
		beforeValuesGet(obj)
		var n = 0
		props.foreach { prop => val p = prop.asInstanceOf[P[T, V]]; read(n, p, p.getter(obj)); n += 1 }
	}
	/** Reads values based on external sequence-like iterable over all props in schema defined order. */
	def valuesIterating(obj: T)(iterate: (((P[T, V], V) => Unit) => Boolean) => Unit): Unit = if (obj != null) {
		beforeValuesGet(obj)
		var n = 0
		def onNextProp(read: (P[T, V], V) => Unit): Boolean = {
			val doNext = n < propsSize
			if (doNext) {
				val p = props(n)
				read(p, p.getter(obj))
			}
			n += 1
			doNext
		}
		// EXEC
		iterate(onNextProp)
	}
	/** Reads values based on external map-like iterable over names of props. */
	def valuesFinding(obj: T)(iterate: (((String, (P[T, V], V) => Unit) => Boolean) => Unit)): Unit = if (obj != null) {
		beforeValuesGet(obj)
		def onFindProp(name: String, read: (P[T, V], V) => Unit): Boolean =
			propsMap.get(name) match {
				case Some(p) => read(p, p.getter(obj)); true
				case None => false
			}
		// EXEC
		iterate(onFindProp)
	}


	private[schemify] def throwError(extraMsg: String = null) = throw new IllegalStateException(
		s"Schema ${getClass.getName} should be annotated with @${classOf[Schemify].getName}. ${if (extraMsg == null) "Or check that macros are enabled" else s"Or $extraMsg"}."
	)
}
