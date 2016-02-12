package just4fun.core.modules.test

import scala.collection.mutable.ArrayBuffer
import scala.language.experimental.macros
import scala.util.{Failure, Success}
import just4fun.core.async.{FutureContext, DefaultFutureContext, FutureX}
import just4fun.core.modules._
import just4fun.core.debug.DebugUtils._

/* CONFIG */
case class TestConfig(var startRestful: Boolean = false, var startSuspended: Boolean = false, var bits: Int = 0, var activatingDelay: Int = 1000, var deactivatingDelay: Int = 1000, var restLatency: Int = 1000, var destroyLatency: Int = 1000, var activOpt: Int = 2, var deactOpt: Int = 2, var startParallel: Boolean = false) {
	val injects = ArrayBuffer[Any ⇒ Unit]().padTo(HitPoints.modPoints.size, null)
	def hasInject(ix: Int): Boolean = (bits & (1 << ix)) != 0 && injects(ix) != null
	def execInject(ix: Int, param: Any): Unit = if ((bits & (1 << ix)) != 0 && injects(ix) != null) injects(ix)(param)
	def switchInject(ix: Int, isOn: Boolean): Unit = if (isOn) bits |= (1 << ix) else bits &= ~(1 << ix)
	def setInject(ix: Int, isOn: Boolean, code: Any ⇒ Unit): TestConfig = {
		injects(ix) = code
		if (isOn) bits |= (1 << ix) else bits &= ~(1 << ix)
		this
	}
	def printInjects(): Unit = {
		logV(HitPoints.modPoints.map(p ⇒ s"${p.index}:$p=${if (hasInject(p.index)) "1" else "0"}").mkString(" : "))
	}
	def fail(ix: Int, isOn: Boolean): Unit = {
		setInject(ix, isOn, param ⇒ throw new Exception("OOPS..."))
	}
}


/* SYSTEM */
class TestSystem extends ModuleSystem {
	import HitPoints._
	import TestApp._
	override implicit val asyncContext: DefaultFutureContext = new DefaultFutureContext
	override val restoreAgent: ModuleRestoreAgent = new RestoreTestAgent
	var restoreClasses: Iterable[String] = null
	val app = TestApp()
	asyncContext.start()

	def test() = println(s" test")
	def await() = asyncContext.await()
	def start[M <: Module](clas: Class[M], stopID: Int = 0, constructor: () ⇒ M = null): M = {
		startModule(clas, stopID, constructor)
	}
	def stop[M <: Module](clas: Class[M], stopID: Int = 0): Unit = {
		stopModule(clas, stopID)
	}

	/* callbacks */
	override protected[this] def onSystemStart(): Unit = {
		SysStart.hit()(null)
		logV(s"@ System  onSystemStart", tagCallbacks)
	}
	override protected[this] def onSystemStop(): Unit = {
		SysFinish.hit()(null)
		logV(s"@ System  onSystemFinish", tagCallbacks)
		TestApp().onSystemFinish()
	}
	override protected[this] def onModulePrepare(promise: ModulePreparePromise): Unit = {
		SysModPrepare.hit()(null)
		logV(s"@ System  onModulePrepare [${promise.module.getClass.getSimpleName}]", tagCallbacks)
		if (TestApp().systemPrepareDelay == 0) promise.complete()
		else FutureX.post(TestApp().systemPrepareDelay, promise.module)(promise.complete())
	}
	override protected[this] def onModuleDestroy(m: Module): Unit = {
		SysModDestroy.hit()(null)
		logV(s"@ System  onModuleDestroy [${m.getClass.getSimpleName}]", tagCallbacks)
	}
	def restore(nums: String): Unit = {
		restoreClasses = nums.map(n ⇒ TestApp().getModule(n.toString.toInt)._3.getName)
		restoreAgent.start()
	}
}



/* MODULE */
abstract class TestModule(val id: Int = 0) extends Module {
	import HitPoints._
	import TestApp._
	val moduleId = getClass.getSimpleName
	lazy val app = TestApp()
	lazy val config: TestConfig = app.setModule(this)
	import config._
	private[this] var activatingDone = false
	private[this] var deactivatingDone = false
	protected[this] var count = 0
	override lazy val internal = new TestCallbacks
	override lazy val lifeCycle = new TestLifeCycle
	override implicit protected lazy val asyncContext: FutureContext = if (startParallel) new TestParallelFutureContext else if (sys == null) new DefaultFutureContext else sys.asyncContext
	if (startRestful) setRestful(true)
	if (startSuspended) suspendService(true)
	//
	ModCreate.hit()

	def startSelf() = bindSelf()
	def stopSelf() = unbindSelf()
	override def stateInfo(): String = super.stateInfo()
	def setRestful_(v: Boolean) { super.setRestful(v) }
	def suspendService_(v: Boolean) { super.suspendService(v) }
	def recover_(): Boolean = { super.recover() }
	def setFailed() { setFailed(new TestingException) }
	def bind(clas: Class[_ <: TestModule], sync: Boolean): Unit = internal.bind(clas, sync)
	def unbind(clas: Class[_ <: TestModule]): Unit = internal.unbind(clas)
	def use(time: Int = 0): FutureX[Int] = {
		count += 1
		val n = count
		val fx = serveAsync {
			ModReqExec.hit(n)
			logV(s"[$moduleId]  $n  exec request...", Module.tagState)
			if (time != 0) Thread.sleep(time)
			n
		}
		fx.onComplete {
			case Failure(e) ⇒ logV(s"[$moduleId] $n request failed with $e", Module.tagState)
				ModReqComplete.hit(false)
			case Success(n) ⇒ logV(s"[$moduleId]  $n  request ok", Module.tagState)
				ModReqComplete.hit(true)
		}
		fx
	}
	override protected[this] def onModuleEvent(e: ModuleEvent[_]): Unit = e match {
		case e: AbleToServeEvent ⇒ logV(s"[$moduleId]:  ${e.server.moduleId} is ABLE to serve", tagEvents)
		case e: UnableToServeEvent ⇒ logV(s"[$moduleId]:  ${e.server.moduleId} is UNABLE serve", tagEvents)
		case _ ⇒
	}


	/* callbacks */
	class TestCallbacks extends InternalCallbacks {
		override protected[this] def onConstructed(): Unit = {
			ModConstr.hit()
			logV(s"@ [$moduleId] onConstructed", tagCallbacks)
		}
		override protected[this] def onPrepared(): Unit = {
			ModPrepare.hit()
			logV(s"@ [$moduleId] onPrepared", tagCallbacks)
		}
		override protected[this] def onDestroyed(): Unit = {
			ModDestroy.hit()
			logV(s"@ [$moduleId] onDestroyed", tagCallbacks)
		}
		// BINDING callbacks
		override protected[this] def onBindingAdd(moduleClas: Class[_], sync: Boolean): Unit = {
			ModBindAdd.hit(TestApp().moduleOf(moduleClas))
			logV(s"@ [$moduleId] onBound [${moduleClas.getSimpleName}];  sync? $sync", tagCallbacks)
		}
		override protected[this] def onBindingRemove(moduleClas: Class[_]): Unit = {
			ModBindRemove.hit(TestApp().moduleOf(moduleClas))
			logV(s"@ [$moduleId] onUnbound [${moduleClas.getSimpleName}];  unbound? $isUnbound", tagCallbacks)
		}
		// REQUEST callbacks
		override protected[this] def onRequestAdd(clas: Class[_]): Unit = {
			ModReqAdd.hit()
			logV(s"@ [$moduleId] onRequestAdd;  hasRequests? $hasRequests", tagCallbacks)
		}
		override protected[this] def onRequestRemove(clas: Class[_]): Unit = {
			ModReqRemove.hit()
			logV(s"@ [$moduleId] onRequestRemove;  hasRequests? $hasRequests", tagCallbacks)
		}
		override protected[this] def onAbleToServeNow(yep: Boolean): Unit = {
			sendEventToClients(if (yep) new AbleToServeEvent else new UnableToServeEvent)
		}
	}

	/**/
	class TestLifeCycle extends LifeCycleCallbacks {
		restLatencyMs = restLatency
		destroyLatencyMs = destroyLatency
		// STATE callbacks
		override protected[this] def onFailed(): Unit = {
			ModFailed.hit()
			logV(s"@ [$moduleId] onFailed", tagCallbacks)
		}
		override protected[this] def onActivatingStart(creating: Boolean, complete: CompleteSelector): CompleteOption = {
			ModActStart.hit(creating)
			logV(s"@ [$moduleId] onActivatingStart;  creating? $creating", tagCallbacks)
			FutureX.post(activatingDelay, this) {
				activatingDone = true
				if (config.activOpt == 1) completeActivating()
			}
			config.activOpt match {
				case 0 ⇒ CompleteNow
				case 1 ⇒ CompleteManually
				case 2 ⇒ CompleteSelector.when {
					ModActProgress.hit(activatingDone)
					logV(s"@ [$moduleId]:  Activating...", tagCallbacks)
					activatingDone
				}
			}
		}
		override protected[this] def onActivatingComplete(creating: Boolean): Unit = {
			ModActCompl.hit(creating)
			activatingDone = false
			logV(s"@ [$moduleId] onActivatingComplete;  creating? $creating", tagCallbacks)
		}
		override protected[this] def onDeactivatingStart(destroying: Boolean, complete: CompleteSelector): CompleteOption = {
			ModDeactStart.hit(destroying)
			logV(s"@ [$moduleId] onDeactivatingStart;  destroying? $destroying", tagCallbacks)
			FutureX.post(deactivatingDelay, this) {
				deactivatingDone = true
				if (config.deactOpt == 1) completeDeactivating()
			}
			config.deactOpt match {
				case 0 ⇒ CompleteNow
				case 1 ⇒ CompleteManually
				case 2 ⇒ CompleteSelector.when {
					ModDeactProgress.hit(deactivatingDone)
					logV(s"@ [$moduleId]:  Deactivating...", tagCallbacks)
					deactivatingDone
				}
			}
		}
		override protected[this] def onDeactivatingComplete(destroying: Boolean): Unit = {
			ModDeactCompl.hit(destroying)
			deactivatingDone = false
			logV(s"@ [$moduleId] onDeactivatingComplete;  destroying? $destroying", tagCallbacks)
		}
		override protected[this] def progressBackoff: Long = {
			val durationMs = progressDurationMs
			val delay = if (durationMs < 2000) 500
			else if (durationMs < 10000) 2000
			else if (durationMs < 60000) 10000
			else 10000
			if (isActivating) (delay * 2).toLong else (delay * 2).toLong
		}
	}
}




/* RESTORE */
class RestoreTestAgent(implicit system: TestSystem) extends ModuleRestoreAgent {
	import HitPoints._
	override lazy val autoStart = false
	var list: ArrayBuffer[String] = _

	override def getList: Iterable[String] = {
		if (list == null) list = ArrayBuffer[String]() ++= system.restoreClasses
		list.toList
	}
	override def clearList(): Unit = list.clear()
	override def remove(clas: String): Unit = {
		list -= clas
		ModRestoreRemove.hit()(TestApp().moduleOf(Class.forName(clas)))
	}
	override def add(clas: String): Unit = {
		list += clas
		ModRestoreAdd.hit()(TestApp().moduleOf(Class.forName(clas)))
	}
	override protected[this] def onRestored(clas: Class[_]): Unit = {
		ModRestored.hit()(TestApp().moduleOf(clas))
	}
}



class Module1 extends TestModule(1) with RestorableModule
class Module2 extends TestModule(2) with RestorableModule
class Module3 extends TestModule(3) with RestorableModule
class Module4 extends TestModule(4)
class Module5 extends TestModule(5)




class TestingException extends Exception


abstract class TestEvent extends ModuleEvent[TestModule]
class AbleToServeEvent(implicit val server: TestModule) extends TestEvent
class UnableToServeEvent(implicit val server: TestModule) extends TestEvent