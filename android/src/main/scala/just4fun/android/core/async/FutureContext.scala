package just4fun.android.core.async

import java.util.concurrent.ThreadPoolExecutor.AbortPolicy
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent._
import android.os._
import just4fun.core.async.{FutureContextOwner, FutureContext}
import just4fun.core.debug.DebugUtils._

/* HANDLER   implementation */
class HandlerContext(name: String, mainThread: Boolean = true)(implicit val owner: FutureContextOwner) extends FutureContext  {
	protected[this] var handler: Handler = null
	private[this] val QUIT = 0x911
	override protected[this] def start_inner(): Unit = {
		val looper = if (mainThread) Looper.getMainLooper
		else {
			val thread = new HandlerThread(name)
			thread.start()
			thread.getLooper
		}
		handler = new Handler(looper) {
			override def dispatchMessage(msg: Message): Unit = msg.getCallback match {
				case r: Runnable => handle(r)
				case null if msg.what == QUIT => getLooper.quit()
				case _ => logW(s" Unknown callback:  what= ${msg.what}, token= ${msg.obj}")
			}
		}
	}
	override protected[this] def execute_inner(id: Any, delay: Long, r: Runnable): Unit = {
		handler.postAtTime(r, id, SystemClock.uptimeMillis() + delay)
	}
	override protected[this] def stop_inner(softly: Boolean): Unit ={
		if (mainThread) clear()
		else if (softly) handler.sendEmptyMessage(QUIT)
		else handler.getLooper.quit()
		handler = null
	}
	override protected[this] def clear_inner(): Unit = {
		handler.removeCallbacksAndMessages(null)
	}
	override protected[this] def cancel_inner(idOrRunnable: Any): Unit =  idOrRunnable match {
		case r: Runnable => handler.removeCallbacks(r)
		case id => if (id != null) handler.removeCallbacksAndMessages(id)
	}
	protected[this] def handle(r: Runnable): Unit = r.run()
	def isMainThread = mainThread
}




/* UI Thread implementation */
object MainThreadContext extends HandlerContext("Main")(null)




/* UI Thread implementation */
//TODO
/** Executes only if UI is available. Re-posts runnable if UI is reconfiguring */
//object UiThreadContext extends HandlerContext("Ui")(null) {
//	override def handle(runnable: Runnable): Unit = Modules.uiContext match {
//		case Some(a) =>
//			if (a.isChangingConfigurations) handler.post(runnable)
//			else super.handle(runnable)
//		case None =>
//	}
//}





/* THREAD POOL user implementation */
/** Uses global thread pool to execute runnable. */
class ThreadPoolContext(name: String)(implicit override val owner: FutureContextOwner) extends HandlerContext(name) {
	override protected[this] def handle(runnable: Runnable): Unit = {
		ThreadPoolContext.execute(runnable)
	}
	override protected[this] def execute_inner(id: Any, delay: Long, r: Runnable): Unit = {
		if (delay > 0) super.execute_inner(id, delay, r)
		else ThreadPoolContext.execute(r)
	}
	override protected[this] def cancel_inner(idOrRunnable: Any): Unit = {
		super.cancel(idOrRunnable)
		ThreadPoolContext.cancel(idOrRunnable)
	}
}





/* THREAD POOL global  implementation */

object ThreadPoolContext extends FutureContext {
	override protected[this] val owner: FutureContextOwner = null
	private[this] var executor: ThreadPoolExecutor = _
	private[this] var handler: Handler = _
	// Can be replaced with more specific ThreadPoolExecutor before using this object by reassigning var
	var constructThreadPool: () => ThreadPoolExecutor = () => {
		val cpus = Runtime.getRuntime.availableProcessors
		val corePoolSize = cpus + 1
		val maxPoolSize = cpus * 2 + 1
		val keepAlive = 1
		val factory = new ThreadFactory {
			private val threadNo = new AtomicInteger(1)
			def newThread(r: Runnable) = new Thread(r, "AsyncContext #" + threadNo.getAndIncrement)
		}
		val policy = new AbortPolicy // CallerRunsPolicy // DiscardOldestPolicy
		val queue = new LinkedBlockingQueue[Runnable](128)
		new ThreadPoolExecutor(corePoolSize, maxPoolSize, keepAlive, TimeUnit.SECONDS, queue, factory, policy)
	}

	override protected[this] def start_inner(): Unit = {
		executor = constructThreadPool()
		handler = new Handler(Looper.getMainLooper) {
			override def dispatchMessage(msg: Message) {
				msg.getCallback match {
					case r: Runnable => execute(r)
					case _ =>
				}
			}
		}
	}
	override protected[this] def execute_inner(id: Any, delay: Long, r: Runnable): Unit = {
		if (delay > 0) handler.postAtTime(r, id, SystemClock.uptimeMillis() + delay)
		else try executor.execute(r) catch {case e: RejectedExecutionException => logE(e)}
	}
	override protected[this] def stop_inner(softly: Boolean): Unit = {
		handler.removeCallbacksAndMessages(null)
		if (softly) executor.shutdown() else executor.shutdownNow()
		executor = null
		handler = null
	}
	override protected[this] def clear_inner(): Unit = {
		handler.removeCallbacksAndMessages(null)
		import scala.collection.JavaConverters._
		executor.getQueue.asScala.toSeq.foreach(executor.remove)
	}
	override protected[this] def cancel_inner(idOrRunnable: Any): Unit = idOrRunnable match {
		case r: Runnable => handler.removeCallbacks(r); executor.remove(r)
		case id => if (id != null) handler.removeCallbacksAndMessages(id)
	}
}
