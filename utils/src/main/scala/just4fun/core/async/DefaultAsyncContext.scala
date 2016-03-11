package just4fun.core.async

import java.lang.System._
import just4fun.core.debug.DebugUtils._

object DefaultAsyncContext {
	// todo remove logs ???
	val logTag = 803842779
}

class DefaultAsyncContext(implicit val key: AsyncContextKey) extends AsyncContext {
	import DefaultAsyncContext._
	protected[this] val list = new OrderedList[iMessage]
	protected[this] var stopNow = false
	protected[this] var stopSoftly = false
	protected[this] var thread: Thread = _// new Thread {override def run(): Unit = loop()}

	def await(): Unit = if (thread != null) thread.join()

	override protected[this] def start_inner(): Unit = {
		if (thread == null) {
			thread = new Thread {override def run(): Unit = loop()}
			thread.start()
		}
		stopNow = false
		stopSoftly = false
	}
	override protected[this] def execute_inner(id: Any, delay: Long, r: Runnable): Unit = {
		add(new Message(id, delay, r))
	}
	override protected[this] def clear_inner(): Unit = {
		list.clear()
	}
	override protected[this] def stop_inner(softly: Boolean): Unit = {
		logV(s"Quit soft? $softly", logTag)
		if (softly) stopSoftly = true else stopNow = true
		notify()
	}
	override protected[this] def cancel_inner(idOrRunnable: Any): Unit = {
		list.removeAll(_.id == idOrRunnable)
		logV(s"Canceled  id=$idOrRunnable;   [$ids]", logTag)
	}

	protected[this] def add(m: iMessage): Unit = synchronized {
		val prevTime = nextTime
		list.add(m)
		logV(s"Add:: delay=${m.delay};  id=${m.id};   [$ids]", logTag)
		if (prevTime == 0L || nextTime < prevTime) notify()
	}
	protected[this] def loop(): Unit = {
		while (hasNext) nextMessage match {
			case null =>
			case m => logV(s"Execute:: id=${m.id};   [$ids]", logTag)
				try m.execute() catch {case e: Throwable => logV(s"Exception while execute message ${m.id}: $e", logTag)}
		}
	}
	protected[this] def hasNext: Boolean = synchronized {
		stopNow match {
			case false => true
			case true => thread = null
				stopNow = false
				stopSoftly = false
				clear_inner() //todo ???
				false
		}
	}
	protected[this] def nextMessage: iMessage = synchronized {
		val now = currentTimeMillis
		list.head match {
			case null => off(0)
			case _ if list.head.time > now => off(list.head.time - now)
			case _ => list.removeHead()
		}
	}
	protected[this] def nextTime: Long = if (list.isEmpty) 0L else list.head.time
	protected[this] def off(delay: Long): iMessage = {
		stopSoftly match {
			case true => stopNow = true
			case _ => logV(s"Wait:: $delay", logTag)
				try wait(delay) catch {case e: Throwable => logV(s"Wait error= $e", logTag)}
		}
		null
	}
	protected[this] def ids: String = {
		list.toArray.map(_.id).mkString(",")
	}
	def isExecutingThread(thread: Thread): Boolean = thread == this.thread
}


/* MESSAGE */
class Message(val id: Any, val delay: Long, val callback: Runnable) extends iMessage


trait iMessage extends OrderedItem[iMessage]  {
	val delay: Long
	val id: Any
	val callback: Runnable
	var time: Long = currentTimeMillis + delay
	def execute(): Unit = callback.run()
	override def compare(that: iMessage): Int = (time - that.time).toInt
}


