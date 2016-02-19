package just4fun.core.async

import java.util.concurrent.CancellationException

import scala.language.implicitConversions
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}
import just4fun.core.debug.DebugUtils._


object Async {
	object State extends Enumeration {val NONE, WAIT, EXEC, DONE = Value}

	implicit def async2future[T](async: Async[T]): Future[T] = async.future

	def apply[T](code: => T)(implicit c: AsyncContext): Async[T] = {
		new Async[T].task(code).activate()
	}
	def apply[T](code: => Async[T])(implicit c: AsyncContext, d: DummyImplicit): Async[T] = {
		new Async[T].task(code).activate()
	}
	def apply[T](code: => Future[T])(implicit c: AsyncContext, d: DummyImplicit, d2: DummyImplicit2 = null): Async[T] = {
		new Async[T].task(code).activate()
	}
	def post[T](delay: Long = 0, id: Any = null, replace: Boolean = true)(code: => T)(implicit c: AsyncContext): Async[T] = {
		new Async[T].task(code)(c).activate(id, delay, replace)
	}
	def postAsync[T](delay: Long = 0, id: Any = null, replace: Boolean = true)(code: => Async[T])(implicit c: AsyncContext): Async[T] = {
		new Async[T].task(code).activate(id, delay, replace)
	}
	def postFuture[T](delay: Long = 0, id: Any = null, replace: Boolean = true)(code: => Future[T])(implicit c: AsyncContext): Async[T] = {
		new Async[T].task(code).activate(id, delay, replace)
	}
	def cancel(id: Any)(implicit c: AsyncContext): Unit = {
		c.cancel(id)
	}

	class DummyImplicit2
}





/* ASYNC  BASE */

sealed class AsyncBase[T] extends Runnable {
	import Async._
	import State._
	var id: Any = ""
	protected[this] var _state = NONE
	// ThreadPoolContext
	protected[this] var _context: AsyncContext = null
	protected[this] var _task: AsyncTask[T] = null
	protected[this] val promise = Promise[T]()
	val future: Future[T] = promise.future
	private[async] val root: AsyncBase[_] = this

	/* USAGE */
	def state: State.Value = _state
	def context: AsyncContext = _context
	def isActivated = state > NONE
	def isExecuting = state == EXEC
	def isDone = state == DONE

	/** Override to execute containing code. Alternatively use task(...). */
	//TODO return T
	def execute(): Try[T] = Failure(new Exception(s"There is nothing to execute in this ${getClass.getSimpleName}"))

	def task(t: AsyncTask[T])(implicit c: AsyncContext): this.type = {
		_context = c
		_task = t
		this
	}
	def activate(id: Any = null, delay: Long = 0, replace: Boolean = true): this.type = synchronized {
		if (_state == NONE) {
			_state = WAIT
			if (_context != null) _context.execute(if (id == null) this else id, delay , this, id != null)
			else finishExecute(Failure(new Exception("Context is null")))
		}
		this
	}
	def deactivate(): this.type = synchronized {
		if (_state == WAIT) {
			_state = NONE
			if (_context != null) _context.cancel(this)
		}
		this
	}
	def cancel(err: Throwable = null): Unit = synchronized {
		if (_state < DONE) {
			if (_state == WAIT && _context != null) {
				_context.cancel(this)
				_task match {
					case t: TaskCancellable => t.cancel()
					case _ =>
				}
			}
			finishExecute(Failure(if (err == null) new CancellationException else err))
		}
	}

	protected[this] def onStartExecute(): Unit = {}
	protected[this] def onFinishExecute(v: Try[T]): Unit = {}

	/* INTERNAL */

	override final def run(): Unit = startExecute()

	private[async] def startExecute(): Unit = {
		val exec = root synchronized {
			_state match {
				case WAIT => _state = EXEC; true
				case _ => false
			}
		}
		if (exec) {
			try onStartExecute() catch loggedE
			if (_task != null) _task.execute(this)
			else finishExecute(try execute() catch {case e: Throwable => Failure(e)})
		}
		else if (_state != DONE) finishExecute(Failure(new IllegalStateException(s"Can't execute in state ${_state}")))
	}
	private[async] def finishExecute(v: Try[T]): Unit = root synchronized {
		_state = DONE
		try onFinishExecute(v) catch loggedE
		v match {
			case Success(v) => promise.trySuccess(v)
//			case Failure(e: CancellationException) => promise.tryFailure(e)
//			case Failure(e) => logE(e); promise.tryFailure(e)
			case Failure(e) => promise.tryFailure(e)
		}
	}
}





/* ASYNC */

class Async[T] extends AsyncBase[T] {
	import Async._

	def task(code: => T)(implicit c: AsyncContext): this.type = {
		_context = c
		_task = new AsyncTaskSync[T](code)
		this
	}
	def task(code: => Async[T])(implicit c: AsyncContext, d: DummyImplicit): this.type = {
		_context = c
		_task = new AsyncTaskFX[T](code)
		this
	}
	def task(code: => Future[T])(implicit c: AsyncContext, d: DummyImplicit, d2: DummyImplicit2 = null): this.type = {
		_context = c
		_task = new AsyncTaskF[T](code)
		this
	}
	def taskCancellable(code: (() => Boolean) => T)(implicit c: AsyncContext): this.type = {
		_context = c
		_task = new AsyncTaskCancellableSync[T](code)
		this
	}
	def taskCancellable(code: (() => Boolean) => Async[T])(implicit c: AsyncContext, d: DummyImplicit): this.type = {
		_context = c
		_task = new AsyncTaskCancellableFX[T](code)
		this
	}
	def taskCancellable(code: (() => Boolean) => Future[T])(implicit c: AsyncContext, d: DummyImplicit, d2: DummyImplicit2 = null): this.type = {
		_context = c
		_task = new AsyncTaskCancellableF[T](code)
		this
	}
	def thanTask[V](code: T => V)(implicit c: AsyncContext): Async[V] = {
		new AsyncP[V, T](this).postTask(code)(c)
	}
	def thanTaskAsync[V](code: T => Async[V])(implicit c: AsyncContext): Async[V] = {
		new AsyncP[V, T](this).postTaskAsync(code)(c)
	}
	def thanTaskFuture[V](code: T => Future[V])(implicit c: AsyncContext): Async[V] = {
		new AsyncP[V, T](this).postTaskFuture(code)(c)
	}
}






/* POST ASYNC */

class AsyncP[T, V] private[async](val parent: Async[V]) extends Async[T] {
	import Async._
	import State._
	override private[async] val root = parent.root
	_state = parent.state

	override def activate(id: Any = null, delay: Long = 0, replace: Boolean = true): this.type = root synchronized {
		if (root.state == NONE) {
			_state = WAIT
			parent.activate(id, delay, replace)
		}
		this
	}
	override def deactivate(): this.type = root synchronized {
		if (root.state == WAIT) {
			_state = NONE
			parent.deactivate()
		}
		this
	}
	override def cancel(err: Throwable = null): Unit = root synchronized {
		if (_state < DONE) {
			parent.cancel()
			finishExecute(Failure(if (err == null) new CancellationException else err))
		}
	}

	/* INTERNAL */

	private[async] def postTask(code: V => T)(implicit c: AsyncContext): this.type = {
		_context = c
		_task = new AsyncPostTaskSync[T, V](this, code)
		this
	}
	private[async] def postTaskAsync(code: V => Async[T])(implicit c: AsyncContext): this.type = {
		_context = c
		_task = new AsyncPostTaskFX[T, V](this, code)
		this
	}
	private[async] def postTaskFuture(code: V => Future[T])(implicit c: AsyncContext): this.type = {
		_context = c
		_task = new AsyncPostTaskF[T, V](this, code)
		this
	}
}






/* ASYNC TASK */

trait AsyncTask[T] {
	def execute(async: AsyncBase[T]): Unit
}



/* TASKs */

private[async] abstract class AsyncTaskSyncBase[T] extends AsyncTask[T] {
	def execute(async: AsyncBase[T]): Unit = {
		async.finishExecute(Try(onExecute()))
	}
	def onExecute(): T
}
private[async] abstract class AsyncTaskFXBase[T] extends AsyncTask[T] {
	def execute(async: AsyncBase[T]): Unit = {
		Try(onExecute()) match {
			case v: Failure[_] => async.finishExecute(v.asInstanceOf[Failure[T]])
			case Success(fx) => fx.activate().onComplete(async.finishExecute)(async.context)
		}
	}
	def onExecute(): Async[T]
}
private[async] abstract class AsyncTaskFBase[T] extends AsyncTask[T] {
	def execute(async: AsyncBase[T]): Unit = {
		Try(onExecute()) match {
			case v: Failure[_] => async.finishExecute(v.asInstanceOf[Failure[T]])
			case Success(f) => f.onComplete(async.finishExecute)(async.context)
		}
	}
	def onExecute(): Future[T]
}



private[async] class AsyncTaskSync[T](code: => T) extends AsyncTaskSyncBase[T] {
	def onExecute(): T = code
}
private[async] class AsyncTaskFX[T](code: => Async[T]) extends AsyncTaskFXBase[T] {
	def onExecute(): Async[T] = code
}
private[async] class AsyncTaskF[T](code: => Future[T]) extends AsyncTaskFBase[T] {
	def onExecute(): Future[T] = code
}



private[async] trait TaskCancellable {
	var cancelled = false
	def cancel() = cancelled = true
}
private[async] class AsyncTaskCancellableSync[T](code: (() => Boolean) => T) extends AsyncTaskSyncBase[T] with TaskCancellable {
	def onExecute(): T = code(() => cancelled)
}
private[async] class AsyncTaskCancellableFX[T](code: (() => Boolean) => Async[T]) extends AsyncTaskFXBase[T] with TaskCancellable {
	def onExecute(): Async[T] = code(() => cancelled)
}
private[async] class AsyncTaskCancellableF[T](code: (() => Boolean) => Future[T]) extends AsyncTaskFBase[T] with TaskCancellable {
	def onExecute(): Future[T] = code(() => cancelled)
}





private[async] trait PostTask[T, V] extends AsyncTask[T] {
	val async: AsyncP[T, V]
	var value: V = _
	async.parent.onComplete {
		case v: Failure[_] => async.finishExecute(v.asInstanceOf[Failure[T]])
		case Success(v) => value = v; async.startExecute()
	}(async.context)
}
private[async] class AsyncPostTaskSync[T, V](val async: AsyncP[T, V], code: V => T) extends AsyncTaskSyncBase[T] with PostTask[T, V] {
	def onExecute(): T = code(value)
}
private[async] class AsyncPostTaskFX[T, V](val async: AsyncP[T, V], code: V => Async[T]) extends AsyncTaskFXBase[T] with PostTask[T, V] {
	def onExecute(): Async[T] = code(value)
}
private[async] class AsyncPostTaskF[T, V](val async: AsyncP[T, V], code: V => Future[T]) extends AsyncTaskFBase[T] with PostTask[T, V] {
	def onExecute(): Future[T] = code(value)
}
