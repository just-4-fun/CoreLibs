package just4fun.core.modules.test

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Promise, Future}
import scala.language.experimental.macros
import scala.util.{Failure, Success}
import just4fun.core.async.{AsyncContext, DefaultAsyncContext, Async}
import just4fun.core.modules.ModulePhase.ModulePhase
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


/* CONTAINER */
class TestContainer extends ModuleContainer {

	import HitPoints._
	import TestApp._
	override val containerId: String = TestApp.contId
	override implicit val asyncContext: DefaultAsyncContext = new DefaultAsyncContext
	override val restoreAgent: ModuleRestoreAgent = new RestoreTestAgent
	var restoreClasses: Iterable[String] = null
	val app = TestApp()
	var currentModule: Module = null
	private[this] lazy val successF = Promise[Unit]().success().future
	asyncContext.start()

	def test() = println(s" test")
	def await() = asyncContext.await()

	/* callbacks */
	override protected[this] def onContainerPopulate(): Unit = {
		ContStart.hit()(null)
		logV(s"@ Container  onContainerStart", tagCallbacks)
	}
	override protected[this] def onContainerEmpty(): Unit = {
		ContFinish.hit()(null)
		logV(s"@ Container  onContainerFinish", tagCallbacks)
		TestApp().onContainerFinish()
	}
	override protected[this] def onModulePrepare(m: Module): Future[Unit] = {
		currentModule = m
		ContModPrepare.hit()(null)
		logV(s"@ Container  onModulePrepare [${m.moduleId}]", tagCallbacks)
		m  match {
			case m: TestModule =>
				if (TestApp().prepareDelay == 0) {m.onPrepared(); successF}
				else Async.post(TestApp().prepareDelay, m)(m.onPrepared()).future
			case _ => successF
		}
	}
	override protected[this] def onModuleDestroy(m: Module): Unit = {
		ContModDestroy.hit()(null)
		logV(s"@ Container  onModuleDestroy [${m.getClass.getSimpleName}]", tagCallbacks)
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
	override val moduleId = getClass.getSimpleName
	val app = TestApp()
	val config: TestConfig = app.setModule(this)
	import config._
	private[this] var activatingDone = false
	private[this] var deactivatingDone = false
	protected[this] val asyncCount = new AtomicInteger()
	protected[this] val syncCount = new AtomicInteger()
	override lazy val state = new TestLifeCycle
	override implicit lazy val asyncContext: AsyncContext = if (startParallel) new TestParallelAsyncContext else cont.asyncContext
	if (startRestful) state.setRestful(true)
	if (startSuspended) suspend(true)
	//
	ModCreate.hit()


	/* */
	override def context: TestContainer = super.context.asInstanceOf[TestContainer]
	def setRestful_(v: Boolean) { super.state.setRestful(v) }
	def suspendService_(v: Boolean) { super.suspend(v) }
	def recover_(): Boolean = { super.recover() }
	def setFailed() { state.fail(new TestingException) }
	def stop(): Unit = context.unbindModule(getClass)
	def bind(clas: Class[_ <: TestModule], sync: Boolean): Unit = internal.bind(clas, sync)
	def unbind(clas: Class[_ <: TestModule]): Unit = internal.unbind(clas)
	def unbindAlll(): Unit = unbindAll()
	def useSync(time: Int = 0): Unit = {
		val n = syncCount.incrementAndGet()
		val res = serveSync {if (time != 0) Thread.sleep(time); n}
		ModSyncReq.hit(res)
		logV(s"[$moduleId]  $n  exec  SYNC  request $res", Module.logTagState)
	}
	def useAsync(time: Int = 0): Async[Int] = {
		val n = asyncCount.incrementAndGet()
		val fx = serveAsync {
			ModReqExec.hit(n)
			logV(s"[$moduleId]  $n  exec request...", Module.logTagState)
			if (time != 0) Thread.sleep(time)
			n
		}
		fx.onComplete {
			case Failure(e) ⇒ logV(s"[$moduleId] $n request failed with $e", Module.logTagState)
				ModReqComplete.hit(false)
			case Success(n) ⇒ logV(s"[$moduleId]  $n  request ok", Module.logTagState)
				ModReqComplete.hit(true)
		}
		fx
	}
	def onPrepared(): Unit = {
		ModPrepare.hit()
		logV(s"@ [$moduleId] onPrepared", tagCallbacks)
	}


	/* callbacks */
	override def onAddBinder(module: Module, sync: Boolean): Unit = {
		ModBindAdd.hit(module)
		logV(s"@ [$moduleId] onBound [${if(module == null) "Root" else module.getClass.getSimpleName}];  sync? $sync", tagCallbacks)
	}
	override def onRemoveBinder(module: Module): Unit = {
		ModBindRemove.hit(module)
		logV(s"@ [$moduleId] onUnbound [${if(module == null) "Root" else module.getClass.getSimpleName}];  unbound? $isUnbound", tagCallbacks)
	}
	override def onRequestAdd(clas: Class[_]): Unit = {
		ModReqAdd.hit()
		logV(s"@ [$moduleId] onRequestAdd;  hasRequests? $hasRequests", tagCallbacks)
	}
	override def onRequestRemove(clas: Class[_]): Unit = {
		ModReqRemove.hit()
		logV(s"@ [$moduleId] onRequestRemove;  hasRequests? $hasRequests", tagCallbacks)
	}

	/**/
	class TestLifeCycle extends State {
		restLatencyMs = restLatency
		destroyLatencyMs = destroyLatency
		// STATE callbacks
		override protected[this] def onFailed(error: Throwable, phaseFailed: ModulePhase): Unit = {
			ModFailed.hit()
			logV(s"@ [$moduleId] onFailed", tagCallbacks)
		}
		override protected[this] def onActivatingStart(creating: Boolean, complete: CompleteSelector): CompleteOption = {
			ModActStart.hit(creating)
			logV(s"@ [$moduleId] onActivatingStart;  creating? $creating", tagCallbacks)
			Async.post(activatingDelay, this) {
				activatingDone = true
				if (config.activOpt == 1) completeActivating()
			}
			config.activOpt match {
				case 0 ⇒ null
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
			Async.post(deactivatingDelay, this) {
				deactivatingDone = true
				logV(s"@ [$moduleId]:  Post Deactivate;  delay=$deactivatingDelay", tagCallbacks)
				if (config.deactOpt == 1) completeDeactivating()
			}
			config.deactOpt match {
				case 0 ⇒ null
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
		override protected[this] def onAbleToServeNow(yep: Boolean): Unit = {
			foreachBinderOfType[TestModule](b⇒
				logV(s"[${b.moduleId}]:  $moduleId is ${if (yep) "ABLE" else "UNABLE"} to serve", tagEvents)
			)
		}
		override protected[this] def onConstructed(): Unit = {
			ModConstr.hit()
			logV(s"@ [$moduleId] onConstructed", tagCallbacks)
		}
		override protected[this] def onDestroyed(): Unit = {
			ModDestroy.hit()
			logV(s"@ [$moduleId] onDestroyed", tagCallbacks)
		}
	}
}




/* RESTORE */
class RestoreTestAgent(implicit container: TestContainer) extends ModuleRestoreAgent {
	import HitPoints._
	override lazy val autoStart = false
	var list: ArrayBuffer[String] = _

	override def getList: Iterable[String] = {
		if (list == null) list = ArrayBuffer[String]() ++= container.restoreClasses
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