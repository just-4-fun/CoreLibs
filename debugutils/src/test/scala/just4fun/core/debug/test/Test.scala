package just4fun.core.debug.test

import just4fun.core.debug.test.blablabla.{MyApp, TestDisableLogs2, TestDisableLogs1, TestDebug}
import just4fun.core.debug.{DebugConfig, DebugUtils}
import just4fun.core.debug.DebugUtils._


object Test extends App {
	val _dl = DebugUtils.disableDebugCode()
	DebugConfig
	  .addPackageRoot("just4fun.utils.logger")
	  .addPackageRoot("com.blabla.myapp")
//	.skipPath()

	new TestDebug
	new TestDisableLogs1
	new TestDisableLogs2

	IncludeExcludeTags

	try 4/0 catch loggedE
	logV(s"ok")
	val a = new A
	a.test()
	val e = new Exception("oops  ...")
	e.initCause(new Exception("because..."))
	logE(e)
	val app = new MyApp
	app.test()
	try app.test2() catch {case e: Throwable => logE(e)}

	BuildConfig2

	def log(e: Throwable) ={
		logE(e) // TODO WARN!!! macro doesn't work this way
	}
	log(new Exception("wrapped Exception"))
	//
	val opt: Option[String] = try {4/0; Option("ok")} catch loggedE(None)
	println(s"OPT= $opt")
	throw new Exception("Catch me :)")
}

object IncludeExcludeTags {
	DebugConfig
	  .skipTag(1)
//	  .skipTag(0)
//	  .onlyTag(2)
	if (2/2 == 1) logI(s"####  Common")
	logI(s"####  Exclude", 1)
	logI(s"####  Only", 2)
}

object BuildConfig2 {
	println(">>>>>>>>  " + getClass.getName)
	val DEBUG = false
}

object BuildConfig3 {
	object Internal {
		val DEBUG = true
	}
}

class A {
	logV(s"A")
	val b = new B
	b.test()

	def test(): Unit = {
		logV(s"A.just4fun.core.schemify.test()")
		logE("OH, NO!!!")
	}

	class B {
		logV(s"B")

		def test(): Unit = {
			logV(s"B.just4fun.core.schemify.test()")
		}
	}
}

//	try {
//		val clas = Class.forName("just4fun.utils.logger.just4fun.core.schemify.test.BuildConfig")
//		logD(clas.getDeclaredFields.map(f=> "["+f.getName+":"+f.getType.getName+"]").mkString(", "))
////		val moduleF = clas.getDeclaredField("MODULE$")
////		moduleF.setAccessible(true)
////		val module = moduleF.get(null)
//		val valueF = clas.getDeclaredField("DEBUG")
//		valueF.setAccessible(true)
//		valueF.getBoolean(null)
////		valueF.getBoolean(module)
//	}
//	catch {case e: Throwable => println(e); false}


