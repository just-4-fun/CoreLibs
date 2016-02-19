package just4fun.core.async

import just4fun.core.debug.DebugUtils._

import scala.concurrent.ExecutionContext

trait AsyncContext extends ExecutionContext {
	private[this] var exited = false
	private[this] var stopped = true
	protected[this] val key: AsyncContextKey

	protected[this] def start_inner(): Unit
	protected[this] def execute_inner(id: Any, delay: Long, r: Runnable): Unit
	protected[this] def cancel_inner(idOrRunnable: Any): Unit
	protected[this] def stop_inner(softly: Boolean): Unit
	protected[this] def clear_inner(): Unit

	def isExited: Boolean = synchronized(exited)
	def isStarted: Boolean = synchronized(!stopped)
	def hasPermission(implicit k: AsyncContextKey): Boolean = {
		k == key || key == null
	}
	def execute(runnable: Runnable): Unit = synchronized {
		if (start()) {
			if (runnable == null) throw new NullPointerException
			execute_inner(runnable, 0, runnable)
		}
	}
	def execute(id: Any, delay: Long, runnable: Runnable, replace: Boolean = false): Boolean = synchronized {
		start() match {
			case true ⇒ if (replace && id != null) cancel(id)
				val _id = if (id == null) runnable else id
				if (_id == null) throw new NullPointerException
				execute_inner(_id, delay, runnable);
				true
			case _ ⇒ false
		}
	}
	def cancel(idOrRunnable: Any): Boolean = synchronized {
		!stopped && idOrRunnable != null match {
			case true ⇒ cancel_inner(idOrRunnable); true
			case _ ⇒ false
		}
	}
	def start(): Boolean = synchronized {
		if (stopped && !exited) {
			start_inner()
			stopped = false
		}
		!stopped
	}
	def clear()(implicit k: AsyncContextKey): Boolean = synchronized {
		!stopped && hasPermission match {
			case true ⇒ clear_inner(); true
			case _ ⇒ false
		}
	}
	def stop(softly: Boolean = false)(implicit k: AsyncContextKey): Boolean = synchronized {
		if (!stopped && hasPermission) {
			stop_inner(softly)
			stopped = true
		}
		stopped
	}
	def exit(softly: Boolean = false)(implicit k: AsyncContextKey): Boolean = synchronized {
		if (hasPermission && stop(softly)) exited = true
		exited
	}
	override def prepare(): AsyncContext = {
		start()
		this
	}
	override def reportFailure(t: Throwable): Unit = logE(t)
}



trait AsyncContextOwner {
	protected[this] implicit val asyncContextKey: AsyncContextKey = new AsyncContextKey
	protected[this] implicit val asyncContext: AsyncContext
}

class AsyncContextKey