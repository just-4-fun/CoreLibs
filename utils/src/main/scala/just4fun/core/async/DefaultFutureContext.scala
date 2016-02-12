package just4fun.core.async

import java.lang.System._
import just4fun.core.debug.DebugUtils._

object DefaultFutureContext {
	// todo remove logs ???
	val tag = 803842779
}

class DefaultFutureContext(implicit val owner: FutureContextOwner) extends FutureContext {
	import DefaultFutureContext._
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
		logV(s"$ownerName Quit soft? $softly", tag)
		if (softly) stopSoftly = true else stopNow = true
		notify()
	}
	override protected[this] def cancel_inner(idOrRunnable: Any): Unit = {
		list.removeAll(_.id == idOrRunnable)
		logV(s"$ownerName Canceled  id=$idOrRunnable;   [$ids]", tag)
	}

	protected[this] def add(m: iMessage): Unit = synchronized {
		val prevTime = nextTime
		list.add(m)
		logV(s"$ownerName Add:: delay=${m.delay};  id=${m.id};   [$ids]", tag)
		if (prevTime == 0L || nextTime < prevTime) notify()
	}
	protected[this] def loop(): Unit = {
		while (hasNext) nextMessage match {
			case null =>
			case m => logV(s"$ownerName Execute:: id=${m.id};   [$ids]", tag)
				try m.execute() catch {case e: Throwable => logV(s"$ownerName Exception while execute message ${m.id}: $e", tag)}
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
			case _ => logV(s"$ownerName Wait:: $delay", tag)
				try wait(delay) catch {case e: Throwable => logV(s"$ownerName Wait error= $e", tag)}
		}
		null
	}
	protected[this] def ids: String = {
		list.toArray.map(_.id).mkString(",")
	}
	protected[this] def ownerName: String = {
		if (owner == null) "[Self context]: " else s"[${owner.getClass.getSimpleName} context]: "
	}
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


