package just4fun.core.debug

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

private[debug] object DebugMacros {

	var DISABLE_DEBUG = false

	def disableDebugCode(c: Context)(): c.Tree = {
		import c.universe._
		DISABLE_DEBUG = true
//		q"${symbolOf[DebugConfig].companion}"
		q"()"
	}

	def debugT[T: c.WeakTypeTag](c: Context)(code: c.Tree): c.Tree = {
		import c.universe._
		val tree = DISABLE_DEBUG match {
			case false => q"$code"
			case _ => q"null.asInstanceOf[${symbolOf[T]}]"// TODO WARN: null.asInstanceOf[Nothing] throws NPE
		}
//		implicit val _c = c
//		prn(s"DISABLE_DEBUG= $DISABLE_DEBUG")
//		prn(tree.toString)
		tree
	}

	def logV(c: Context)(msg: c.Tree): c.Tree = log(c)(DebugConfig.VERBOSE, msg, c.universe.reify(0).tree)
	def logD(c: Context)(msg: c.Tree): c.Tree = log(c)(DebugConfig.DEBUG, msg, c.universe.reify(0).tree)
	def logI(c: Context)(msg: c.Tree): c.Tree = log(c)(DebugConfig.INFO, msg, c.universe.reify(0).tree)
	def logW(c: Context)(msg: c.Tree): c.Tree = log(c)(DebugConfig.WARN, msg, c.universe.reify(0).tree)
	def logVt(c: Context)(msg: c.Tree, tag: c.Tree): c.Tree = log(c)(DebugConfig.VERBOSE, msg, tag)
	def logDt(c: Context)(msg: c.Tree, tag: c.Tree): c.Tree = log(c)(DebugConfig.DEBUG, msg, tag)
	def logIt(c: Context)(msg: c.Tree, tag: c.Tree): c.Tree = log(c)(DebugConfig.INFO, msg, tag)
	def logWt(c: Context)(msg: c.Tree, tag: c.Tree): c.Tree = log(c)(DebugConfig.WARN, msg, tag)


	def log(c: Context)(level: Int, msg: c.Tree, tag: c.Tree): c.Tree = {
		import c.universe._
		val tree = DISABLE_DEBUG match {
			case false =>
				val logSym = symbolOf[DebugConfig].companion
				val path = ownerPath(c)
				q"$logSym.log($level, $path, $msg, ${tag})"
			case true => q"()"
		}
//		implicit val _c = c
//		prn(s"${if (DISABLE) "DISABLED" else "NOT DISABLED"}:  $level;  $msg")
//		prn(tree.toString)
		tree
	}
	def ownerPath(c: Context): String = {
		import c.universe._
		var list = List[String]()
		var owner = c.internal.enclosingOwner
		while (owner != NoSymbol) {
			//	val nfo = if (owner.isMethod) "meth" else if (owner.isPackageClass) "packCls" else if (owner.isPackage) "pack"
			// else if (owner.isModuleClass) "modCls" else if (owner.isModule) "mod" else if (owner.isClass) "cls" else "???"
			val name = owner.name.toString
			if (name.charAt(0) != '<') list = name :: list
			owner = owner.owner
		}
		val buff = new StringBuilder
		buff ++= list.mkString(".") ++= ":" ++= c.enclosingPosition.line.toString ++= "]::    "
		if (buff.length < 90) buff ++= " " * (90 - buff.length)
		buff.toString()
	}

	def logEMs(c: Context)(msg: c.Tree): c.Tree = {
		import c.universe._
		logEM(c)(q"null", msg)
	}
	def logE(c: Context)(err: c.Tree): c.Tree = {
		import c.universe._
		logEM(c)(err, q"null")
	}
	def logEM(c: Context)(err: c.Tree, msg: c.Tree): c.Tree = {
		import c.universe._
		implicit val _c = c
		val logSym = symbolOf[DebugConfig].companion
		var owner = c.internal.enclosingOwner
		var list = List[String]()
		var packs = List[String]()
		while (owner != NoSymbol) {
			val name = owner.name.toString
			if (name.charAt(0) != '<') {
				list = name :: list
				if (owner.isPackage) packs = name :: packs
			}
			owner = owner.owner
		}
		val buff = new StringBuilder
		buff ++= list.mkString(".") ++= ":" ++= c.enclosingPosition.line.toString ++= "]::    "
		if (buff.length < 90) buff ++= " " * (90 - buff.length)
		val where = buff.toString()
		val root = packs match {
			case Nil => ""
			case head :: Nil => packs.mkString(".")
			case _ => packs.dropRight(1).mkString(".")
		}
		val tree = q"$logSym logE($where, $root, $err, $msg)"
		//		prn(tree.toString)
		tree
	}
	def loggedET[T: c.WeakTypeTag](c: Context)(default: c.Tree): c.Tree = {
		logged(c)(default)
	}
	def loggedE(c: Context): c.Tree = {
		logged(c)(null)
	}
	private def logged(c: Context)(default: c.Tree): c.Tree = {
		import c.universe._
		implicit val _c = c
		val logSym = symbolOf[DebugConfig].companion
		var owner = c.internal.enclosingOwner
		var list = List[String]()
		var packs = List[String]()
		while (owner != NoSymbol) {
			val name = owner.name.toString
			if (name.charAt(0) != '<') {
				list = name :: list
				if (owner.isPackage) packs = name :: packs
			}
			owner = owner.owner
		}
		val buff = new StringBuilder
		buff ++= list.mkString(".") ++= ":" ++= c.enclosingPosition.line.toString ++= "]::    "
		if (buff.length < 90) buff ++= " " * (90 - buff.length)
		val where = buff.toString()
		val root = packs match {
			case Nil => ""
			case head :: Nil => packs.mkString(".")
			case _ => packs.dropRight(1).mkString(".")
		}
		val dflt = if (default == null) q"()" else default
		val tree = q"{case e: Throwable => $logSym logE($where, $root, e, null); $dflt}"
//		prn(tree.toString)
		tree
	}

	def prn(text: String)(implicit c: Context) = c.info(c.enclosingPosition, text, false)
}
