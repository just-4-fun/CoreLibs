package just4fun.android.core.vars

import scala.collection.immutable.HashMap
import android.content.SharedPreferences
import just4fun.android.core.app.ModuleApp
import just4fun.core.schemify._
import just4fun.core.schemify.impls.typefactory.{JsonArrayWriter, JsonReader}


/* PREFS */
/** WARN: can be used not earlier than [[android.app.Application]].onCreate is called. */
object Prefs {
	lazy val defaultCache = ModuleApp().getSharedPreferences("default_cache", 0)
	private[core] lazy val syscache = ModuleApp().getSharedPreferences("system_cache", 0)

	def apply[T](name: String)(implicit typ: PropType[T], prefs: SharedPreferences = null): T = {
		val p = if (prefs == null) defaultCache else prefs
		typ.eval(typ.read(p, name)(PrefReader))
	}
	def get[T](name: String)(implicit typ: PropType[T], prefs: SharedPreferences = null): T = apply[T](name)
	def getOrElse[T](name: String, default: ⇒ T)(implicit typ: PropType[T], prefs: SharedPreferences = null): T = {
		val p = if (prefs == null) defaultCache else prefs
		val res = typ.eval(typ.read(p, name)(PrefReader))
		if (res != null) res else default
	}
	def update[T](name: String, value: T)(implicit typ: PropType[T], prefs: SharedPreferences = null): Unit = if (name != null) {
		val p = if (prefs == null) defaultCache else prefs
		val editor = p.edit()
		typ.write(value, editor, name)(PrefWriter)
		editor.apply()
	}
	def set[T](name: String, value: T)(implicit typ: PropType[T], prefs: SharedPreferences = null): Unit = update[T](name, value)
	def contains(name: String)(implicit prefs: SharedPreferences = null): Boolean = {
		val p = if (prefs == null) defaultCache else prefs
		p.contains(name)
	}
	def remove(name: String)(implicit prefs: SharedPreferences = null): Unit = if (name != null) {
		val p = if (prefs == null) defaultCache else prefs
		p.edit().remove(name).apply()
	}
}




/* PREF READER */
object PrefReader extends TypeReader[SharedPreferences, SharedPreferences, String] {
	override def toObject[T](v: SharedPreferences, typ: SchemaType[T]): T = {
		if (v == null) null.asInstanceOf[T]
		else typ.createFinding { onFind =>
			val itr = v.getAll.keySet().iterator()
			while (itr.hasNext) onFind(itr.next(), p => p.typ.read(v, p.name))
		}
	}
	override def toArray[E, T](v: SharedPreferences, typ: IterableType[E, T]): T = {
		if (v == null) null.asInstanceOf[T]
		else if (v.getAll.isEmpty) null.asInstanceOf[T]
		else readArray(typ, v, v.getAll.keySet().iterator().next())
	}

	override def readObject[T](typ: SchemaType[T], d: SharedPreferences, k: String): T = d.getString(k, null) match {
		case null => null.asInstanceOf[T]
		case v => typ.createFrom(v)(JsonReader)
	}
	override def readArray[E, T](typ: IterableType[E, T], d: SharedPreferences, k: String): T = d.getString(k, null) match {
		case null => null.asInstanceOf[T]
		case v => typ.createFrom(v)(JsonReader)
	}
	override def readBytes(d: SharedPreferences, k: String): Any = d.getString(k, "").getBytes("US-ASCII")
	override def readString(d: SharedPreferences, k: String, ascii: Boolean): Any = d.getString(k, find[String](d, k))
	override def readLong(d: SharedPreferences, k: String): Any = d.getLong(k, find[Long](d, k))
	override def readInt(d: SharedPreferences, k: String): Any = d.getInt(k, find[Int](d, k))
	override def readShort(d: SharedPreferences, k: String): Any = d.getInt(k, find[Int](d, k))
	override def readChar(d: SharedPreferences, k: String): Any = d.getInt(k, find[Int](d, k))
	override def readByte(d: SharedPreferences, k: String): Any = d.getInt(k, find[Int](d, k))
	override def readDouble(d: SharedPreferences, k: String): Any = {
		val n = java.lang.Double.longBitsToDouble(d.getLong(k, Long.MinValue))
		if (n == Long.MinValue) find[Double](d, k)
		else n
	}
	override def readFloat(d: SharedPreferences, k: String): Any = d.getFloat(k, find[Float](d, k))
	override def readBoolean(d: SharedPreferences, k: String): Any = d.getBoolean(k, find[Boolean](d, k))
	override def readNull(d: SharedPreferences, k: String): Any = null

	private[this]  def find[T](d: SharedPreferences, k: String)(implicit typ: PropType[T]): T = {
		if (d.contains(k)) typ.eval(d.getAll.get(k))
		else null.asInstanceOf[T]
	}
}




/* PREF WRITER */
object PrefWriter extends TypeWriter[Null, SharedPreferences.Editor, String] {
	import SharedPreferences.Editor
	override def fromObject[T](v: T, typ: SchemaType[T]): Null = null
	override def fromArray[E, T](v: T, typ: IterableType[E, T]): Null = null

	override def writeObject[T](typ: SchemaType[T], v: T, d: Editor, k: String): Unit = {
		d.putString(k, typ.valuesTo(v)(JsonArrayWriter))
	}
	override def writeArray[E, T](typ: IterableType[E, T], v: T, d: Editor, k: String): Unit = {
		d.putString(k, typ.valuesTo(v)(JsonArrayWriter))
	}
	override def writeBytes(v: Array[Byte], d: Editor, k: String): Unit = d.putString(k, new String(v, "US-ASCII"))
	override def writeString(v: String, d: Editor, k: String, ascii: Boolean): Unit = d.putString(k, v)
	override def writeLong(v: Long, d: Editor, k: String): Unit = d.putLong(k, v)
	override def writeInt(v: Int, d: Editor, k: String): Unit = d.putInt(k, v)
	override def writeShort(v: Short, d: Editor, k: String): Unit = d.putInt(k, v)
	override def writeChar(v: Char, d: Editor, k: String): Unit = d.putInt(k, v)
	override def writeByte(v: Byte, d: Editor, k: String): Unit = d.putInt(k, v)
	override def writeDouble(v: Double, d: Editor, k: String): Unit = d.putLong(k, java.lang.Double.doubleToLongBits(v))
	override def writeFloat(v: Float, d: Editor, k: String): Unit = d.putFloat(k, v)
	override def writeBoolean(v: Boolean, d: Editor, k: String): Unit = d.putBoolean(k, v)
	override def writeNull(d: Editor, k: String): Unit = d.remove(k)
}
