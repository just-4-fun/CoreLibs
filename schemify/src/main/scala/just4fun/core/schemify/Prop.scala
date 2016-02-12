package just4fun.core.schemify

class Prop[O, T] protected[schemify](val schema: SchemaType[O])(implicit val typ: PropType[T]) {
	var index = -1
	var name = ""
	val stub = typ == PropType.NullType
	var getter: O => T = stub match {
		case false => o => schema.throwError(s"'$index-th ${getClass.getSimpleName}' prop definition is not recognized.")
		case true => o => null.asInstanceOf[T]
	}
	var setter: (O, T) => Unit = stub match {
		case false => (o, v) => schema.throwError(s"'$index-th ${getClass.getSimpleName}' prop definition is not recognized.")
		case true => (o, v) => ()
	}

	def config(code: this.type => Unit): this.type = {code(this); this}

	def apply(v: Any): T = typ.eval(v)
	def copy(v: T, deep: Boolean = false): T = typ.copy(v, deep)
	def equal(o1: T, o2: T): Boolean = typ.equal(o1, o2)
	def differ(o1: T, o2: T): Boolean = typ.differ(o1, o2)

	override def toString = name
}