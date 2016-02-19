package just4fun.core.modules.test

import java.util.Scanner
import scala.StringBuilder
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.language.existentials
import just4fun.core.async.{AsyncContext, Async, AsyncContextOwner, DefaultAsyncContext}
import just4fun.core.debug.DebugConfig
import just4fun.core.modules.Module
import just4fun.core.debug.DebugUtils._

object TestApp {
	val tagCallbacks = 299332
	val tagEvents = 299338
	private var i: TestApp[_, _, _, _, _] = _
	def apply() = i
}


/* APP */
class TestApp[M1 <: Module1 : Manifest, M2 <: Module2 : Manifest, M3 <: Module3 : Manifest, M4 <: Module4 : Manifest, M5 <: Module5 : Manifest] extends App with AsyncContextOwner {
	import TestApp._
	import HitPoints._
	DebugConfig.addPackageRoot("just4fun.core")
	  .skipPath()
	  .skipTag(tagCallbacks)
//	  .skipTag(tagEvents)
	  .skipTag(DefaultAsyncContext.tag)
	  .skipTag(Module.logTagParam)
	  .skipTag(Module.logTagStateX)
//	  .skipTag(Module.tagState)

	TestApp.i = this
	implicit var system: TestSystem = newSystem
	implicit val asyncContext: DefaultAsyncContext = new DefaultAsyncContext
	val clas1 = implicitly[Manifest[M1]].runtimeClass.asInstanceOf[Class[M1]]
	val clas2 = implicitly[Manifest[M2]].runtimeClass.asInstanceOf[Class[M2]]
	val clas3 = implicitly[Manifest[M3]].runtimeClass.asInstanceOf[Class[M3]]
	val clas4 = implicitly[Manifest[M4]].runtimeClass.asInstanceOf[Class[M4]]
	val clas5 = implicitly[Manifest[M5]].runtimeClass.asInstanceOf[Class[M5]]
	var m1: M1 = null.asInstanceOf[M1]
	var m2: M2 = null.asInstanceOf[M2]
	var m3: M3 = null.asInstanceOf[M3]
	var m4: M4 = null.asInstanceOf[M4]
	var m5: M5 = null.asInstanceOf[M5]
	var cfg1 = TestConfig()
	var cfg2 = TestConfig()
	var cfg3 = TestConfig()
	var cfg4 = TestConfig()
	var cfg5 = TestConfig()
	//	  .setInject(ModCreate, true, _ ⇒ m1.setRestful(true))
//	  .setInject(ModActStart, true, _ ⇒ throw new TestingException)
	private[this] var scanner: Scanner = _
	private[this] var manual = false
	private[this] var quit = false
	val rx = """(\d*)(\D+)(\d*)""".r
	private[this] var parallel = false
	private[this] var newSys = false
	private[this] val commands = ArrayBuffer[String]()
	private[this] var testCount = 0
	private[this] var failedReports: List[String] = _
	var systemPrepareDelay = 0


	def newSystem = new TestSystem
	def reinit(): Unit = {
//		system = newSystem
		systemPrepareDelay = 0
		cfg1 = TestConfig()
		cfg2 = TestConfig()
		cfg3 = TestConfig()
		cfg4 = TestConfig()
		cfg5 = TestConfig()
	}
	def setModule[M <: TestModule](m: M): TestConfig = m match {
		case m: M1 ⇒ m1 = m; cfg1
		case m: M2 ⇒ m2 = m; cfg2
		case m: M3 ⇒ m3 = m; cfg3
		case m: M4 ⇒ m4 = m; cfg4
		case m: M5 ⇒ m5 = m; cfg5
		case _ ⇒ TestConfig()
	}
	def getModule(n: Int): (TestModule, TestConfig, Class[_ <: TestModule]) = n match {
		case 1 ⇒ (m1, cfg1, clas1)
		case 2 ⇒ (m2, cfg2, clas2)
		case 3 ⇒ (m3, cfg3, clas3)
		case 4 ⇒ (m4, cfg4, clas4)
		case 5 ⇒ (m5, cfg5, clas5)
		case _ ⇒ (null, null, null)
	}
	def moduleIndex(clas: Class[_]): Int = {
		if (clas == clas1) 1 else if (clas == clas2) 2 else if (clas == clas3) 3 else if (clas == clas4) 4 else if (clas == clas5) 5 else 0
	}
	def moduleOf(clas: Class[_]): TestModule = {
		if (clas == clas1) m1 else if (clas == clas2) m2 else if (clas == clas3) m3 else if (clas == clas4) m4 else if (clas == clas5) m5 else null
	}

	def onCommands(commands: String, extraCases: PartialFunction[(String, String, String), Unit] = null): Unit = {
		// DEF
		def exec(com: String, par: Boolean) = {
			logV(s"${if (par) "⇒>" else "->>"} $com")
			try onCommand(com, extraCases) catch loggedE
		}
		// EXEC
		commands.split(' ').foreach { com ⇒
			if (parallel && com.charAt(0) != '/') new Thread(() ⇒ exec(com, true)).start()
			else exec(com, false)
		}
	}
	def onCommand(command: String, extraCases: PartialFunction[(String, String, String), Unit] = null): Unit = {
		commands += command
		command match {
			case rx(s1, com, s2) ⇒ val n1 = if (s1.isEmpty) 0 else s1.toInt
				val n2 = if (s2.isEmpty) 0 else s2.toInt
				val (m1, cfg1, clas1) = getModule(if (n1 == 0) 1 else n1)
				val (m2, cfg2, clas2) = getModule(n2)
				com match {
					case "s" ⇒ system.start(clas1)
					case "sx" ⇒ system.stop(clas1)
					case "b" ⇒ m1.bind(clas2, false)
					case "bs" ⇒ m1.bind(clas2, true)
					case "bx" ⇒ m1.unbind(clas2)
					case "u" ⇒ m1.useAsync(n2)
					case "uu" ⇒ m1.useSync(n2)
					case "r" ⇒ m1.setRestful_(true)
					case "rx" ⇒ m1.setRestful_(false)
					case "p" ⇒ m1.suspendService_(true)
					case "px" ⇒ m1.suspendService_(false)
					case "f" ⇒ if (s2.isEmpty) m1.setFailed() else cfg1.fail(n2, true)
					case "fx" ⇒ if (s2.isEmpty) {if (!m1.recover_()) logV(s"Oops.. can't recover")} else cfg1.fail(n2, false)
					case "i" ⇒ if (s2.isEmpty) cfg1.printInjects() else cfg1.switchInject(n2, true)
					case "ix" ⇒ if (s2.isEmpty) cfg1.bits = 0 else cfg1.switchInject(n2, false)
					case "?" ⇒ logV(m1.info.toString())
					case "z" ⇒ logV(draftReport())
					case "zx" ⇒ logV(draftReport(true))
					case "L" ⇒ DebugConfig.skipTag(tagCallbacks, n2 != 1)
					case "x" ⇒ newSys = true
					case "@" ⇒ reinit()
					case "/" ⇒ quit = true; appQuit()
					case "//" ⇒ waitSystemFinish(n2)
					case "///" ⇒ Thread.sleep(n2); logV(s"->> wake")
					case "/=" ⇒ parallel = true
					case "/=/" ⇒ parallel = false
					case "rs" ⇒ system.restore(s2)
					case "#pd" ⇒ systemPrepareDelay = n2
					case "#sp" ⇒ cfg1.startParallel = n2 == 1
					case "#sr" ⇒ cfg1.startRestful = n2 == 1
					case "#ss" ⇒ cfg1.startSuspended = n2 == 1
					case "#ad" ⇒ cfg1.activatingDelay = n2
					case "#dd" ⇒ cfg1.deactivatingDelay = n2
					case "#rl" ⇒ cfg1.restLatency = n2
					case "#dl" ⇒ cfg1.destroyLatency = n2
					case "#ao" ⇒ cfg1.activOpt = n2
					case "#do" ⇒ cfg1.deactOpt = n2 // #pd0 1#sr0 1#ss0 1#ao2 1#ad1000 1#rl1000 1#dl1000 1#do2 1#dd1000
					case _ ⇒ if (extraCases != null && extraCases.isDefinedAt((s1, com, s2))) extraCases(s1, com, s2)
					else println(s"Command not found: '$command'")
				}
			case _ ⇒ println(s"Command not found: '$command'")
		}
	}

	/* MANUAL TEST */
	protected[this] def manualTest(configCode: ⇒ Unit)(extraCases: PartialFunction[(String, String, String), Unit] = null): Unit = {
		configCode
		manual = true
		scanner = new Scanner(System.in)
		while (true) {
			val line = scanner.nextLine
			onCommands(line, extraCases)
		}
	}
	private[this] def manualTestFinish(): Unit = {
		parallel = false
		logV(s"Commands\n${commands.mkString(" ")}")
		logV(draftReport(true))
	}
	protected[this] def draftReport(reset: Boolean = false): String = {
		val buff = new StringBuilder("Report draft\n")
		buff ++= HitPoints.printSeq(HitPoints.sequence)
		if (reset) HitPoints.reset()
		buff.toString()
	}

	/* AUTO TEST */
	protected[this] def autoTest(autoTests: (() ⇒ AutoTest)*): Unit = {
		failedReports = Nil
		val t0 = System.currentTimeMillis
		autoTests.foreach { newTest ⇒
			val test: AutoTest = newTest()
			testCount += 1
			logV(s"\n\n*************                        TEST:$testCount  [${test.name}]")
			onCommands(test.commands, test.extraCases)
			waitSystemFinish(test.delay)
			logW(report(test.name, test.assertions))
			HitPoints.reset()
			parallel = false
			reinit()
		}
		logW(s"Tests time= ${(System.currentTimeMillis - t0) / 1000} sec")
		if (failedReports.nonEmpty) logW(s"\nReports failed ${failedReports.size}:\n${failedReports.mkString("\n")}")
		quit = true
		appQuit()
	}
	protected[this] def report(name: String, asserts: Seq[Assertion]): String = {
		val failures = asserts.map(_ ()).withFilter(!_.isOk).map(_.toString()).mkString("\n")
		if (failures.nonEmpty) failedReports = s"TEST:$testCount  [$name]" :: failedReports
		s"\n\n** REPORT **:\nActual sequence:\n${printSeq(sequence)}\n\n${if (failures.isEmpty) "OK" else failures}\n"
	}
	protected[this] implicit def autotest2f(test: ⇒ AutoTest): (() ⇒ AutoTest) = () ⇒ test
	protected[this] implicit def assert2f(asserts: ⇒ Assertion): (() ⇒ Assertion) = () ⇒ asserts

	/* APP */
	private[this] def waitSystemFinish(delay: Int = 30000): Unit = synchronized {
		if (system.asyncContext.isStarted) {
			wait(delay)
			system.forceStopSystem()
			logV(s"->> wake")
		}
	}
	def onSystemFinish(): Unit = synchronized {
		notifyAll()
		if (manual) manualTestFinish()
		appQuit()
	}
	def appQuit(): Unit = {
		if (!system.asyncContext.isStarted) {
			if (quit) Async.post(500)(System.exit(4))
			else if (newSys) {
				logV(draftReport())
				system = newSystem
				newSys = false
			}
		}
	}
	protected[this] def appAwait(): Unit = {
		system.await()
		asyncContext.await()
	}
}



/* AUTOTEST */
abstract class AutoTest(val name: String, val commands: String, val delay: Int = 30000) {
	def assertions: Seq[Assertion]
	def extraCases: PartialFunction[(String, String, String), Unit] = null
}



/* POINTS */
object HitPoints extends HitPointSet {
	val ModCreate, ModConstr, ModPrepare, ModActStart, ModActProgress, ModActCompl,
	ModDeactStart, ModDeactProgress, ModDeactCompl, ModFailed, ModDestroy,
	ModBindAdd, ModBindRemove, ModReqAdd, ModReqExec, ModReqComplete, ModReqRemove, ModSyncReq, ModRestored, ModRestoreAdd, ModRestoreRemove = new HitPointVal(false)
	val SysStart, SysFinish, SysModPrepare, SysModDestroy = new HitPointVal(true)
	val points: mutable.Buffer[HitPointVal] = values.to[mutable.Buffer].map(p ⇒ p.asInstanceOf[HitPointVal])
	val sysPoints: mutable.Buffer[HitPointVal] = points.filter(_.system)
	val modPoints: mutable.Buffer[HitPointVal] = points.filter(!_.system)
	val sequence = mutable.ArrayBuffer[Hit]()
	val alterSeq = mutable.ArrayBuffer[Hit]()
	def reset(): Unit = {
		sequence.clear()
		alterSeq.clear()
	}
	def add2seq(h: Hit): Unit = synchronized(sequence += h)
	def add2altSeq(h: Hit) = synchronized(alterSeq += h)
	def add2altSeqSkip = {
		val prev = alterSeq.last
		alterSeq(alterSeq.length - 1) = null
		alterSeq += prev
	}
	def delay(t: Int): Unit = {
		alterSeq.last.delay = t
	}
	def printSeq(seq: mutable.ArrayBuffer[Hit]): String = if (seq.isEmpty) ""
	else {
		val buff = new StringBuilder
		var line = 0
		seq.foldLeft(Hit(0, 0, 0)) { (h1, h2) ⇒
			val delay = if (h1 == null) 0 else if (h1.delay > 0) h1.delay else if (h2 != null) h2.time - h1.time else 0
			if (delay >= 500) buff ++= s">@$delay"
			if (buff.nonEmpty) buff ++= s">>"
			buff ++= (if (h2 == null) "" else h2.toString)
			if (buff.length / 100 > line) {line += 1; buff ++= "\n"}
			h2
		}
		buff ++= ">?"
		buff ++= s"\nTime: ${System.currentTimeMillis() - seq.head.time} ms"
		buff.toString()
	}
	def compareSequences(name: String, positive: Boolean): Assertion = {
		var cursor = 0
		var prevTime = 0L
		var delay = 0
		var realDelay = -1L
		//DEFs
		def skipTo(h: Hit): Boolean = {
			while (cursor < sequence.length) {
				if (isCurrent(h)) return true
			}
			false
		}
		def isCurrent(h: Hit): Boolean = {
			val ch = sequence(cursor)
			cursor += 1
			if (!ch.sameAs(h)) false
			else {
				// check curr delay
				if (delay > 0) {
					realDelay = ch.time - prevTime
					if (realDelay > delay - 50) {realDelay = -1; delay = 0; prevTime = 0}
				}
				// check new delay
				if (h.delay > 0 && realDelay == -1) {
					delay = h.delay
					prevTime = ch.time
				}
				realDelay == -1
			}
		}
		//EXEC
		var error = ""
		var skip = false
		val res = alterSeq.forall { h ⇒
			val found = cursor match {
				case 0 => skipTo(h)
				case _ if cursor >= sequence.length => false
				case _ => h match {
					case null => skip = true; true
					case _ => val r = if (skip) skipTo(h) else isCurrent(h)
						skip = false
						r
				}
			}
			if (!found) error = if (realDelay >= 0) s"$h delay specified= $delay; real= $realDelay" else s"not found $h in sequence"
			found
		}
//		val s = printSeq(alterSeq)
		alterSeq.clear()
		if (positive) Assertion(res, s"Sequence $name", error)
		else Assertion(!res, s"Sequence $name", "Sequence exists")
	}
}




case class Hit(pid: Int, mid: Int, param: Any) {
	val time: Long = System.currentTimeMillis()
	var delay = 0
	def name = HitPoints(pid).toString
	override def toString: String = {
		val p = param2string(param)
		val params = if (p == null) {if (mid == 0) "()" else s"($mid)"} else s"($mid,$p)"
		s"$name$params"
	}
	def param2string(param: Any): String = param match {
		case null ⇒ null
		case _: Boolean | _: String | _: Int | _: Long | _: Double | _: Float | _: Short | _: Char | _: Byte => param.toString
		case m: Module => m.getClass.getSimpleName
		case op: Option[_] => op.toString
		case _ => null
	}
	def sameAs(h: Hit): Boolean = {
		h.pid == pid && (h.mid == 0 || h.mid == mid) && (h.param == null || param2string(h.param) == param2string(param))
	}
}

class HitPointSet extends Enumeration {
	class HitPointVal(val system: Boolean) extends Val with HitPoint
}


object HitPoint {
	implicit def p2ix(p: HitPoint): Int = p.index
}

trait HitPoint {
	this: Enumeration#Value ⇒
	val system: Boolean
	lazy val index = this.id
	lazy val name = toString
	def apply(mid: Int = 0, param: Any = null): HitPoint = {
		HitPoints add2altSeq Hit(index, mid, param)
		this
	}
	def >>(p: HitPoint): HitPoint = p
	def >>>>(p: HitPoint): HitPoint = {
		HitPoints.add2altSeqSkip
		p
	}
	def >@(t: Int): HitPoint = {
		HitPoints.delay(t)
		this
	}
	def >? : Assertion = HitPoints.compareSequences("", true)
	def >?(name: String): Assertion = HitPoints.compareSequences(name, true)
	def >!?(name: String): Assertion = HitPoints.compareSequences(name, false)
	def hit(param: Any = null)(implicit m: TestModule): Unit = {
		val h = Hit(index, if (m != null) m.id else 0, param)
		HitPoints add2seq h
		if (m != null) m.config.execInject(index, param)
	}
}




/* ASSERTION */
case class Assertion(ok: Boolean, expr: String, failMsg: String = "") {
	def isOk: Boolean = ok
	override def toString(): String = s"${if (ok) "OK" else s"FAILED"}: ${if (!ok && failMsg.nonEmpty) s" :: $failMsg" else ""} : [$expr]"
}


