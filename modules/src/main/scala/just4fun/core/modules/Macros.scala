package just4fun.core.modules

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
	def bind[T: c.WeakTypeTag](c: Context)(m: c.Tree): c.Tree = {
		checkT[T](c, "Module can not be bound to itself.")
		import c.universe._
		implicit val cxt = c
//		q"internal.bind[${weakTypeOf[T]}](false)($m)"
		q"internal.bind(implicitly[Manifest[${weakTypeOf[T]}]].runtimeClass.asInstanceOf[Class[${weakTypeOf[T]}]], false)"
	}
	def bindS[T: c.WeakTypeTag](c: Context)(m: c.Tree): c.Tree = {
		checkT[T](c, "Module can not be bound to itself.")
		import c.universe._
//		q"internal.bind[${weakTypeOf[T]}](true)($m)"
		q"internal.bind(implicitly[Manifest[${weakTypeOf[T]}]].runtimeClass.asInstanceOf[Class[${weakTypeOf[T]}]], true)"
	}
	private def checkT[T: c.WeakTypeTag](c: Context, selfMsg: String, chkConst: Boolean = true): Unit = {
		import c.universe._
		implicit val cxt = c
		val mt = symbolOf[T]
		val ot = c.internal.enclosingOwner.owner
		if (selfMsg != null && mt == ot) abort(selfMsg)
		else if (mt == symbolOf[Nothing]) abort(s"Type parameter [M <: Module] should be specified explicitly.")
		if (chkConst && !hasConstr[T](c)) abort(s"Module [${mt.name}] should have public zero-argument or one-boolean-argument constructor.")
	}
	private def hasConstr[T: c.WeakTypeTag](c: Context): Boolean = {
		c.symbolOf[T].toType.decls.exists { d =>
			d.isConstructor && d.isPublic && {
				val paramss = d.asMethod.paramLists
				paramss.isEmpty || paramss.head.isEmpty || (paramss match {
					case (param :: Nil) :: Nil => param.info =:= c.typeOf[Boolean]
					case _ => false
				})
				//				val params = paramss.map(pms => pms.map(p => s"${p.fullName}:${p.info}").mkString(", ")).mkString(" : ")
				//				prn(s"T=${c.symbolOf[T]} ;  D= $d;  Constr? ${d.isConstructor};  isPublic? ${d.isPublic};  nullable? ${d.asMethod.paramLists.isEmpty};  isEmpty? ${d.asMethod.paramLists.head.isEmpty} ;   paramss>> $params;  ok? $ok")(c)
			}
		}
	}

	def prn(msg: String)(implicit c: Context): Unit = {
		c.info(c.enclosingPosition, msg, true)
	}
	def abort(msg: String)(implicit c: Context): Unit = {
		c.abort(c.enclosingPosition, msg)
	}
}