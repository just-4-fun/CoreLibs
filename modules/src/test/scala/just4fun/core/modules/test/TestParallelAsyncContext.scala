package just4fun.core.modules.test

import just4fun.core.async.{AsyncContext, AsyncContextKey}

class TestParallelAsyncContext (implicit val key: AsyncContextKey) extends AsyncContext {
	override protected[this] def execute_inner(id: Any, delay: Long, r: Runnable): Unit = {
		new Thread(() ⇒ {
			if (delay > 0) Thread.sleep(delay)
			r.run()
		}).start()
	}
	override protected[this] def start_inner(): Unit = {}
	override protected[this] def stop_inner(softly: Boolean): Unit = {}
	override protected[this] def clear_inner(): Unit = {}
	override protected[this] def cancel_inner(idOrRunnable: Any): Unit = {}
}
