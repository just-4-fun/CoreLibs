package just4fun.core.debug

import java.lang.Thread.UncaughtExceptionHandler

import scala.collection.mutable
import scala.language.experimental.macros
import scala.language.implicitConversions


object DebugUtils {
//	implicit def string2exception(msg: String): Exception = WrappedMessage(msg)

	def logV(msg: => String): Unit = macro DebugMacros.logV
	def logD(msg: => String): Unit = macro DebugMacros.logD
	def logI(msg: => String): Unit = macro DebugMacros.logI
	def logW(msg: => String): Unit = macro DebugMacros.logW
	def logV(msg: => String, tag: Int): Unit = macro DebugMacros.logVt
	def logD(msg: => String, tag: Int): Unit = macro DebugMacros.logDt
	def logI(msg: => String, tag: Int): Unit = macro DebugMacros.logIt
	def logW(msg: => String, tag: Int): Unit = macro DebugMacros.logWt
	def logE(msg: => String): Unit = macro DebugMacros.logEMs
	def logE(err: => Throwable): Unit = macro DebugMacros.logE
	def logE(err: => Throwable, msg: => String): Unit = macro DebugMacros.logEM
	def loggedE: PartialFunction[Throwable, Unit] = macro DebugMacros.loggedE
	def loggedE[T](default: T): PartialFunction[Throwable, T] = macro DebugMacros.loggedET[T]
	def logMuted: PartialFunction[Throwable, Unit] = { case e: Throwable => }
	def logMuted[T](default: T): PartialFunction[Throwable, T] = { case e: Throwable => default }
	def debug[T](code: => T): T = macro DebugMacros.debugT[T]
	/** Removes all code defined in this class except error logs.
	  * WARN: place call as val assignment in the beginning of each class where want to disable logs or code.
	  * Because typed expressions are compiled before void ones.
	  * Ex: val disable = disableDebugCode() */
	def disableDebugCode(): Unit = macro DebugMacros.disableDebugCode
}




object DebugConfig {
	val VERBOSE = 2
	val DEBUG = 3
	val INFO = 4
	val WARN = 5
	val ERROR = 6
	val ASSERT = 7
	val tagPrefix = "(::)"

	private[this] var _debug = true
	private[this] var _pkgRoots = List[String]()
	//("just4fun")
	private[this] var _logDef: (Int, String, String) => Unit = (level, tag, message) => println(s"$tag    $message")
	private[this] var _delegateErrorDef: (Throwable, String, String) => Unit = null
	private[this] val onlyTags = mutable.Set[Int]()
	private[this] val skipTags = mutable.Set[Int]()
	private[this] var _skipWhere = false
	private[this] var _short = true

	uncaughtErrorDef(e => logE("LoggerConfig::   ", "", e, "Handle uncaught exception:"))


	def isDebug: Boolean = _debug
	def debug(yes: Boolean): this.type = { _debug = yes; this }
	def addPackageRoot(v: String): this.type = { _pkgRoots = v :: _pkgRoots; this }
	def logDef(f: (Int, String, String) => Unit): this.type = { _logDef = f; this }
	def delegateErrorDef(f: (Throwable, String, String) => Unit): this.type = { _delegateErrorDef = f; this }
	def fullPath(): this.type = { _short = false; this }
	def skipPath(): this.type = { _skipWhere = true; this }
	def skipTag(tag: Int, on: Boolean = true): this.type = { if (on) skipTags += tag else skipTags -= tag; this }
	def onlyTag(tag: Int, on: Boolean = true): this.type = { if (on) onlyTags += tag else onlyTags -= tag; this }
	def uncaughtErrorDef(f: Throwable => Unit): this.type = {
		Thread.setDefaultUncaughtExceptionHandler(new UncaughtExceptionHandler {
			private[this] val sysErrorHandler = Thread.getDefaultUncaughtExceptionHandler
			override def uncaughtException(thread: Thread, ex: Throwable): Unit = {
				f(ex)
				if (sysErrorHandler != null) sysErrorHandler.uncaughtException(thread, ex)
				else System.exit(2)
			}
		})
		this
	}

	def log(level: Int, where: String, what: => String, tag: Int) = if (_debug && printable(tag)) {
		val w = if (_skipWhere) "" else if (_short) _pkgRoots.foldLeft(where)((init, next) => init.replaceFirst(next, ".")) else where
		_logDef(level, tagPrefix, w + what)
	}
	private[this] def printable(tag: Int): Boolean = {
		(skipTags.isEmpty || !skipTags.contains(tag)) && (onlyTags.isEmpty || onlyTags.contains(tag))
	}
	def logE(where: String, pkgRoot: String, error: => Throwable, msg: => String) = if (_debug || _delegateErrorDef != null) {
		val e = error
		val m = msg
		val what = e match {
			case null => if (m == null) "" else m
//			case WrappedMessage(m) => m
			case _ => val builder = new StringBuilder
				if (m != null) builder ++= m ++= "\n"
				val roots = _pkgRoots.exists(pkgRoot.startsWith) match {
					case false => pkgRoot :: _pkgRoots
					case _ => _pkgRoots
				}
				reducedStackTrace(e, builder, roots).toString()
		}
		val w = if (_short) _pkgRoots.foldLeft(where)((init, next) => init.replaceFirst(next, ".")) else where
		if (_delegateErrorDef == null) _logDef(ERROR, tagPrefix, w + what)
		else _delegateErrorDef(e, tagPrefix, w + what)
	}

	private def reducedStackTrace(error: Throwable, builder: StringBuilder, pkgRoots: List[String]): StringBuilder = {
		var msg = error.toString
		if (msg == null || msg.isEmpty) msg = error.getClass.getName + ": " + error.getMessage
		builder ++= "    " ++= msg ++= "\n"
		val stack = error.getStackTrace
		var root = false
		var n = 0
		var next = n < stack.length
		while (next) {
			val elt = stack(n)
			builder ++= "    " ++= elt.toString ++= "\n"
			n += 1
			val _root = pkgRoots.exists(elt.getClassName.startsWith)
			next = n < stack.length && (!root || _root)
			root = _root
		}
		if (n < stack.length) builder ++= "    ... " ++= (stack.length - n).toString ++= " more stack records."
		val cause = error.getCause
		if (cause != null && error != cause) reducedStackTrace(error.getCause, builder ++= "\n    ... Caused by: \n", pkgRoots)
		builder
	}
}

private class DebugConfig

