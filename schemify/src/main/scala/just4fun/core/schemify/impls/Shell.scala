package just4fun.core.schemify.impls

import just4fun.core.schemify.TypeWriter


trait Shelled[T] extends Shell[T] {this: T =>
	val pearl: T = this
}

trait Shell[T] {
	val pearl: T
	val schema: ReachSchema[T]
	protected[impls] type V = schema.V
	protected[impls] type P[t<:T, v] = schema.P[t,v]

	def values: Array[_] = schema.values(pearl)
	def valuesTo[F](implicit writer: TypeWriter[F, _, _]): F = schema.valuesTo[F](pearl)
	def valuesToJsonArray: String = schema.valuesToJsonArray(pearl)
	def valuesToJsonMap: String = schema.valuesToJsonMap(pearl)
	def valuesToBytes: Array[Byte] = schema.valuesToBytes(pearl)
	def valuesToArray: IndexedSeq[_] = schema.valuesToArray(pearl)
	def valuesToMap: collection.Map[String, _] = schema.valuesToMap(pearl)
	/** Reads values iterating over supplied all schema props. */
	def valuesReadingAll(read: (P[T, V], V) => Unit): Unit = {
		schema.valuesReadingAll(pearl)(read)
	}
	/** Reads values iterating over supplied props. */
	def valuesReading(props: Iterable[P[T, _]])(read: (Int, P[T, V], V) => Unit): Unit = {
		schema.valuesReading(pearl, props)(read)
	}
	/** Reads values based on external sequence-like iterable over all props. */
	def valuesIterating(iterate: (((P[T, V], V) => Unit) => Boolean) => Unit): Unit = {
		schema.valuesIterating(pearl)(iterate)
	}
	/** Reads values based on external map-like iterable over names of props. */
	def valuesFinding(iterate: (((String, (P[T, V], V) => Unit) => Boolean) => Unit)): Unit = {
		schema.valuesFinding(pearl)(iterate)
	}
	def valuesCopy(deep: Boolean = false): T = schema.copy(pearl, deep)
	def valuesEqual(obj: T): Boolean = schema.equal(pearl, obj)
}
