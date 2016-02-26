package just4fun.core.modules.test.tests

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.{Success, Failure}
import just4fun.core.async.{AsyncContext, Async}
import just4fun.core.debug.measureTime
import just4fun.core.modules._
import just4fun.core.modules.test._
import just4fun.core.debug.DebugUtils._

object Test extends TestApp[M1, M2, M3, M4, M5] {
	import HitPoints._

	val auto = false
//	val auto = true

	if (auto) {
		autoTest(t27, t26)
//		autoTest(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47)
		// 185 sec
	}
	else manualTest {
//		cfg1.setInject(ModCreate, true, p ⇒ m1.internal.internalBind(classOf[M2], true))
	} {
		case (s1, "ns", s2) ⇒ val s = new TestSystem; s.bindModule(classOf[M1], () ⇒ new M1); s.unbindModule(classOf[M1])
		case (s1, "ne", s2) ⇒ val m = try new M1 catch loggedE
		case (s1, com, s2) ⇒ println(s"Command not found: '${s1 + com + s2}'")
	}

	def injectFail(param: Any): Unit = throw new Exception("OOPS...")

	/*TESTS*/
	lazy val t47 = new AutoTest("Restore middle - Restored all", "3f3 1s 1bs2 2bs3 ///1000 3fx3 2fx ///2000 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModFailed(3) >> ModFailed(2) >> ModFailed(1) >>>> ModActCompl(3, true) >>>> ModActCompl(2, true) >>>> ModActCompl(1, true) >?
		)
	}
	lazy val t46 = new AutoTest("SyncRequest in Restful no ActivDelay", "1#ao0 1#sr1 1s 1uu 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModActCompl(1, true) >> ModSyncReq(1, Some(1)) >?
		)
	}
	lazy val t45 = new AutoTest("SyncRequest in Restful", "1#sr1 1s 1uu 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModSyncReq(1, None) >>>> ModActCompl(1, true) >?
		)
	}
	lazy val t44 = new AutoTest("Cyclic dependency unresolved", "1s 1b2 2b1 1sx", 4000) {
		override def assertions: Seq[Assertion] = Seq(
			ModFailed(1) >> ModFailed(2) >?
		)
	}
	lazy val t43 = new AutoTest("The whole tree is bound by system", "1s 1bs2 1bs3 2bs4 3bs5 4bs5 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModActCompl(5, true) >? "1", ModActCompl(4, true) >? "2", ModActCompl(2, true) >? "3", ModActCompl(3, true) >? "4", ModActCompl(1, true) >? "6",
			ModDestroy(1) >>>> ModDestroy(2) >>>> ModDestroy(3) >>>> ModDestroy(4) >>>> ModDestroy(5) >? "6"
		)
	}
	lazy val t42 = new AutoTest("Idempotent bind", "1s 1b2 1b2 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModDestroy(1) >>>> ModDestroy(2) >?
		)
	}
	lazy val t41 = new AutoTest("Use - Suspend - Stop > Fail request", "1s ///1100 1u100 1u100 ///10 1p 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModReqComplete(1, true) >? "ok",
			ModReqComplete(1, false) >? "fail"
		)
	}
	lazy val t40 = new AutoTest("Restful - Use - Suspend - Use", "1#sr1 1s 1u ///1000 1u100 1u100 1u100 1u100 1u100 ///200 1p ///4000 1px 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModReqExec(1) >>>> ModDeactCompl(1, false) >@ 2000 >> ModActStart(1, false) >>>> ModReqExec(1) >>>> ModDeactStart(1, true) >?
		)
	}
	lazy val t39 = new AutoTest("Use - Suspend - Use", "1s ///1000 1u100 1u100 1u100 1u100 1u100 ///200 1p ///3000 1px 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModReqComplete(1, true) >@ 2000 >>>> ModReqExec(1) >>>> ModReqComplete(1, true) >>>> ModDeactCompl(1) >?
		)
	}
	lazy val t38 = new AutoTest("Restore", "rs15 2s ///100 1sx 2sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModCreate(1) >>>> ModRestoreAdd(1) >>>> ModRestored(1) >>>> ModActCompl(1, true) >>>> ModRestoreRemove(1) >> ModDeactStart(1, true) >? "1",
			ModCreate(2) >>>> ModRestoreAdd(2) >>>> ModActCompl(2, true) >>>> ModRestoreRemove(2) >> ModDeactStart(2, true) >? "2",
			ModCreate(5) >!? "5"
		)
	}
	lazy val t37 = new AutoTest("Parallel Start - Bind - Stop", "/= 1s 2s 3s 4s ///100 1b5 2b5 3b5 4b5 1sx 2sx 3sx 4sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModActCompl(1, true) >>>> ModDestroy(1) >>>> ModDestroy(5) >? "1",
			ModActCompl(2, true) >>>> ModDestroy(2) >>>> ModDestroy(5) >? "2",
			ModActCompl(3, true) >>>> ModDestroy(3) >>>> ModDestroy(5) >? "3",
			ModActCompl(4, true) >>>> ModDestroy(4) >>>> ModDestroy(5) >? "4",
			ModActCompl(5, true) >>>> ModDestroy(5) >? "5"
		)
	}
	lazy val t36 = new AutoTest("Wait Predecessor - Use", "1#dd2000 1s 1b2 1sx ///2500 1#dd1000 1s 1b2 1u 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModActCompl(2, true) >>>> ModDestroy(1) >>>> ModActCompl(1, true) >> ModReqExec(1, 1) >>>> ModDestroy(1) >> ModBindRemove(2) >>>> ModDeactStart(2, true) >? "ok",
			ModCreate(2) >>>> ModCreate(2) >!? "Created twice"
		)
	}
	lazy val t35 = new AutoTest("Restful - BindSync after start", "1#sr1 2#sr1 1s 2s 1bs2 1u 1sx 2sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModReqAdd(1) >> ModActStart(2, true) >>>> ModActCompl(2, true) >> ModActStart(1, true) >>>> ModActCompl(1, true) >> ModReqExec(1, 1) >>>> ModDestroy(1) >> ModBindRemove(2) >> ModDeactStart(2, true) >?
		)
	}
	lazy val t34 = new AutoTest("Cyclic BindSync", "1s 1bs2 2bs3 3bs1 3bx1 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModActStart(1, true) >>>> ModActStart(2, true) >>>> ModActStart(3, true) >>>> ModDestroy(1) >>>> ModDestroy(2) >>>> ModDestroy(3) >?
		)
	}
	lazy val t33 = new AutoTest("Cyclic Bind", "1s 1b2 2b3 3b1 3bx1 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModActStart(1, true) >>>> ModActStart(2, true) >>>> ModActStart(3, true) >>>> ModDestroy(1) >>>> ModDestroy(2) >>>> ModDestroy(3) >?
		)
	}
	lazy val t32 = new AutoTest("Fail SyncServer in Constructor - Try Recover SyncServer - Failed", "2f0 1s 2fx 1sx") {
		cfg1.setInject(ModCreate, true, p ⇒ m1.bind(classOf[M2], true))
		override def assertions: Seq[Assertion] = Seq(
			ModFailed(2) >>>> ModFailed(1) >? "1", ModActCompl(1, true) >!? "2", ModActCompl(2, true) >!? "3"
		)
	}
	lazy val t31 = new AutoTest("Fail SyncServer - Recover SyncServer", "2f3 1s 1bs2 ///1000 2fx3 2fx 1u 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModFailed(2) >> ModFailed(1) >>>> ModReqAdd(1) >>>> ModActCompl(2, true) >>>> ModActCompl(1, true) >> ModReqExec(1, 1) >>>> ModDestroy(1) >> ModBindRemove(2) >>>> ModDeactStart(2, true) >?
		)
	}
	lazy val t30 = new AutoTest("Restful - Rebind Sync", "1#sr1 2#sr1 1s 1b2 1u ///4000 1bs2 1u 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModReqAdd(1) >>>> ModActStart(2, true) >>>> ModActCompl(2, true) >> ModActStart(1, false) >>>> ModReqExec(1, 2) >>>> ModDestroy(1) >> ModBindRemove(2) >> ModDeactStart(2, true) >?
		)
	}
	lazy val t29 = new AutoTest("Restful - 1 BindSync 2 - Use - Rest - Use", "1#sr1 2#sr1 1s 1bs2 1u ///6000 1u 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModActCompl(2, true) >> ModActStart(1, true) >>>> ModActCompl(1, true) >> ModReqExec(1, 1) >>>> ModDeactCompl(1, false) >> ModDeactStart(2, false) >>>> ModReqAdd(1) >>>> ModActCompl(2, false) >> ModActStart(1, false) >>>> ModActCompl(1, false) >> ModReqExec(1, 2) >>>> ModDestroy(1) >> ModBindRemove(2) >> ModDeactStart(2, true) >?
		)
	}
	lazy val t28 = new AutoTest("No delay - No latency - 1 BindSync 2 - Use", "1#ao0 1#do0 1#rl0 1#dl0 2#ao0 2#do0 2#rl0 2#dl0 1#sr1 2#sr1 1s 1bs2 1u 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModActCompl(2, true) >> ModActStart(1, true) >>>> ModReqExec(1, 1) >>>> ModDestroy(1) >> ModBindRemove(2) >>>> ModDeactStart(2, true) >?
		)
	}
	lazy val t27 = new AutoTest("Zero delays, Start - Use - Stop", "#pd0 #ad0 #dd0 #rl0 #dl #ao0 #do0 1s 1u 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			SysStart()>>ModCreate(1)>>ModConstr(1)>>ModBindAdd(1)>>SysModPrepare()>>ModPrepare(1)>>ModActStart(1,true) >> ModActCompl(1,true)>>>>ModReqExec(1,1)>>>>ModDeactStart(1,true) >> ModDeactCompl(1,true)>>ModDestroy(1)>>SysModDestroy()>>SysFinish()>?"1",
			ModReqComplete(1,true)>?"2",
			Assertion(time < 200, "Test time < 200 ms", s"time= $time")
		)
	}
	lazy val t26 = new AutoTest("Zero delays, Start - Stop", "#pd0 #ad0 #dd0 #rl0 #dl #ao0 #do0 1s 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			SysStart() >> ModCreate(1) >> ModConstr(1) >> ModBindAdd(1) >> SysModPrepare() >> ModPrepare(1) >> ModActStart(1, true) >> ModActCompl(1, true) >> ModBindRemove(1) >> ModDeactStart(1, true) >> ModDeactCompl(1, true) >> ModDestroy(1) >> SysModDestroy() >> SysFinish() >?,
			Assertion(time < 200, "Test time < 200 ms", s"time= $time")
		)
	}
	lazy val t25 = new AutoTest("Parallel Use in ParallelContext", "1#sp1 1s 1sx /= 1u 1u 1u 1u 1u") {
		override def assertions: Seq[Assertion] = Seq(
			ModActCompl(1, true) >>>> ModReqRemove(1) >>>> ModReqRemove(1) >>>> ModReqRemove(1) >>>> ModReqRemove(1) >>>> ModReqRemove(1) >>>> ModDeactStart(1, true) >?
		)
	}
	lazy val t24 = new AutoTest("Parallel Use", "1s 1sx /= 1u 1u 1u 1u 1u") {
		override def assertions: Seq[Assertion] = Seq(
			ModActCompl(1, true) >>>> ModReqComplete(1, true) >> ModReqComplete(1, true) >> ModReqComplete(1, true) >> ModReqComplete(1, true) >> ModReqComplete(1, true) >?
		)
	}
	lazy val t23 = new AutoTest("Fail in ModDeactCompl - Recover - Use", "1#sr1 1f8 1s 1u ///4000 1fx8 1fx 1u 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModDeactCompl(1, false) >>>> ModFailed(1) >> ModReqAdd(1) >>>> ModReqComplete(1, true) >?
		)
	}
	lazy val t22 = new AutoTest("Fail in ModDeactProgress - Recover - Use", "1#sr1 1f7 1s 1u ///3000 1fx7 1fx 1u 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModDeactProgress(1, false) >>>> ModFailed(1) >>>> ModReqAdd(1) >> ModActStart(1, true) >>>> ModDeactStart(1, true) >?
		)
	}
	lazy val t21 = new AutoTest("Fail in ModDeactStart - Recover - Use", "1#sr1 1f6 1s 1u ///3000 1fx6 1fx 1u 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModDeactStart(1, false) >>>> ModFailed(1) >>>> ModReqAdd(1) >> ModActStart(1, true) >>>> ModActCompl(1, true) >>>> ModReqComplete(1, true) >?
		)
	}
	lazy val t20 = new AutoTest("Fail in ModActCompl - Recover - Use", "1f5 1s ///1500 1fx5 1fx 1u 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModActCompl(1, true) >>>> ModFailed(1) >> ModActStart(1, true) >>>> ModReqAdd(1) >>>> ModReqComplete(1, true) >?
		)
	}
	lazy val t19 = new AutoTest("Fail in ModActProgress - Recover - Use", "1f4 1s 1fx4 1fx 1u 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModActProgress(1, false) >>>> ModFailed(1) >> ModActStart(1, true) >> ModActProgress(1, false) >> ModReqAdd(1) >>>> ModActCompl(1, true) >>>> ModReqComplete(1, true) >?
		)
	}
	lazy val t18 = new AutoTest("Fail in ModActStart - Recover - Use", "1f3 1s 1fx3 1fx 1u 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModActStart(1, true) >>>> ModFailed(1) >> ModActStart(1, true) >>>> ModReqAdd(1) >>>> ModActCompl(1, true) >>>> ModReqComplete(1, true) >?
		)
	}
	lazy val t17 = new AutoTest("Fail in ModPrepare - Recover", "1f2 1s 1fx 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			SysStart() >> ModCreate(1) >> ModConstr(1) >> ModBindAdd(1) >> SysModPrepare() >> ModPrepare(1) >> ModFailed(1) >> ModBindRemove(1) >> ModDestroy(1) >> SysModDestroy() >> SysFinish() >?
		)
	}
	lazy val t16 = new AutoTest("Fail in ModConstr - Recover", "1f1 1s 1fx 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModCreate(1) >> ModConstr(1) >> ModFailed(1) >>>> ModDestroy(1) >?
		)
	}
	lazy val t15 = new AutoTest("Fail in ModCreate - Recover", "1f0 1s 1fx 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModCreate(1) >> ModConstr(1) >> ModFailed(1) >>>> ModDestroy(1) >?
		)
	}
	lazy val t14 = new AutoTest("No delays - Start Restful - Use - Suspend -Use - Release - Stop", "1#dd0 1#ad0 1#rl0 1#dl0 1#sr1 1s 1u 1p 1u 1px 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModReqAdd(1) >>>> ModActCompl(1, true) >>>> ModDeactCompl(1, false) >>>> ModActCompl(1, false) >> ModReqExec(1, 1) >>>> ModReqExec(1, 2) >>>> ModDeactCompl(1, true) >? "1",
			ModReqComplete(1, true) >> ModReqComplete(1, true) >? "2"
		)
	}
	lazy val t13 = new AutoTest("Start Restful - Use - Suspend -Use - Release - Stop", "1#sr1 1s 1u ///1500 1p 1u ///1500 1px 1sx", 40000) {
		override def assertions: Seq[Assertion] = Seq(
			ModReqAdd(1) >>>> ModActCompl(1, true) >> ModReqExec(1, 1) >>>> ModDeactStart(1, false) >>>> ModDeactCompl(1, false) >> ModActStart(1, false) >>>> ModActCompl(1, false) >> ModReqExec(1, 2) >>>> ModDeactStart(1, true) >? "1",
			ModReqComplete(1, true) >>>> ModReqComplete(1, true) >? "2"
		)
	}
	lazy val t12 = new AutoTest("Start Restful - Use - Rest -Use - Stop", "1#sr1 1s 1u ///2500 1u 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModActCompl(1, true) >>>> ModReqComplete(1, true) >@ 1000 >> ModDeactStart(1, false) >>>> ModReqAdd(1) >>>> ModDeactCompl(1, false) >> ModActStart(1, false) >>>> ModActCompl(1, false) >>>> ModReqComplete(1, true) >@ 1000 >> ModDeactStart(1, true) >?
		)
	}
	lazy val t11 = new AutoTest("Start - Use - Suspend -Use - Release - Stop", "1s ///1000 1u ///100 1p 1u ///1000 1px 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModReqExec(1, 1) >>>> ModReqComplete(1, true) >> ModReqAdd(1) >@ 1000 >> ModReqExec(1, 2) >>>> ModReqComplete(1, true) >?
		)
	}
	lazy val t10 = new AutoTest("Start Suspended - Use - Release - Stop", "1#ss1 1s ///1000 1u ///1000 1px 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModActCompl(1, true) >>>> ModReqExec(1, 1) >>>> ModReqRemove(1) >>>> ModDeactStart(1, true) >?,
			ModReqComplete(1, true) >?
		)
	}
	lazy val t9 = new AutoTest("Start Suspended - Use - Stop", "1#ss1 1s 1u 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModActCompl(1, true) >> ModDeactStart(1, true) >>>> ModDeactCompl(1, true) >>>> SysFinish() >? "1",
			ModReqComplete(1, false) >? "2"
		)
	}
	lazy val t8 = new AutoTest("Start Restful - Use - Stop", "1#sr1 1s 1u ///2500 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModReqAdd(1) >> ModActStart(1, true) >@ 1000 >>>> ModActProgress(1, true) >> ModActCompl(1, true) >> ModReqExec(1, 1) >> ModReqRemove(1) >> ModReqComplete(1, true) >@ 1000 >> ModDeactStart(1, false) >@ 1000 >>>> ModDeactProgress(1, true) >> ModDeactCompl(1, false) >> ModActStart(1, false) >@ 1000 >>>> ModActProgress(1, true) >> ModActCompl(1, false) >@ 1000 >> ModDeactStart(1, true) >@ 1000 >>>> ModDeactProgress(1, true) >> ModDeactCompl(1, true) >?
		)
	}
	lazy val t7 = new AutoTest("Start Restful - Stop", "1#sr1 1s ///100 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModConstr(1) >> ModBindAdd(1) >> SysModPrepare() >> ModPrepare(1) >> ModBindRemove(1) >> ModDestroy(1) >?
		)
	}
	lazy val t6 = new AutoTest("Progress CompleteWhen", "1#ao2 1#do2 1s ///100 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModActStart(1, true) >@ 1000 >>>> ModActProgress(1, true) >> ModActCompl(1, true) >>>> ModDeactStart(1, true) >@ 1000 >>>> ModDeactProgress(1, true) >> ModDeactCompl(1, true) >?
		)
	}
	lazy val t5 = new AutoTest("Progress CompleteManually", "1#ao1 1#do1 1s ///100 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModActStart(1, true) >@ 1000 >>>> ModActCompl(1, true) >>>> ModDeactStart(1, true) >@ 1000 >> ModDeactCompl(1, true) >?
		)
	}
	lazy val t4 = new AutoTest("Progress CompleteNow", "1#ao0 1#do0 1s ///100 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModActStart(1, true) >> ModActCompl(1, true) >>>> ModDeactStart(1, true) >> ModDeactCompl(1, true) >?
		)
	}
	lazy val t3 = new AutoTest("Start - Request - Stop", "1s 1u 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			ModReqAdd(1) >>>> ModActCompl(1, true) >> ModReqExec(1, 1) >> ModReqRemove(1) >> ModReqComplete(1, true) >@ 1000 >>>> ModDeactStart(1, true) >?
		)
	}
	lazy val t2 = new AutoTest("Start - Prepare - Stop", "#pd100 1s 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			SysStart() >> ModCreate(1) >> ModConstr(1) >> ModBindAdd(1) >> SysModPrepare() >> ModBindRemove(1) >> ModDestroy(1) >> SysModDestroy() >> SysFinish() >?
		)
	}
	lazy val t1 = new AutoTest("Start - Stop", "1s 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			SysStart() >> ModCreate(1) >> ModConstr(1) >> ModBindAdd(1) >> SysModPrepare() >> ModPrepare(1) >> ModActStart(1, true) >@ 1000 >>>> ModActProgress(1, true) >> ModActCompl(1, true) >@ 1000 >> ModDeactStart(1, true) >@ 1000 >>>> ModDeactProgress(1, true) >> ModDeactCompl(1, true) >> ModDestroy(1) >> SysModDestroy() >> SysFinish() >? "1",
			ModBindAdd(1) >>>> ModBindRemove(1) >>>> ModDeactStart(1, true) >? "2"
		)
	}
}

class M1 extends Module1
class M2 extends Module2
class M3 extends Module3
class M4 extends Module4
class M5 extends Module5
