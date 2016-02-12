package just4fun.core.async.test

import scala.language.implicitConversions
import scala.util.Random
import just4fun.core.async.DefaultFutureContext

object TestTiker extends App {
	var N = 0
	def nextId: Int = { N += 1; N }
	def rnd(seed: Int): Int = Random.nextInt(seed)
	implicit def toRunnable(code: => Unit): Runnable = new Runnable {
		override def run(): Unit = code
	}

	val tiker = new DefaultFutureContext()(null)
	val t1 = new Thread() {
		override def run(): Unit = {
			tiker.execute(nextId, rnd(6000), toRunnable())
			Thread.sleep(200)
			tiker.execute(nextId, rnd(4000),  toRunnable())
			Thread.sleep(200)
			tiker.execute(nextId, rnd(10000), toRunnable{
				tiker.execute(nextId, 1000, toRunnable())
				tiker.execute(nextId, 0, toRunnable())
				tiker.stop(Random.nextBoolean())(null)
			})
		}
	}
	val t2 = new Thread() {
		override def run(): Unit = {
			tiker.execute(nextId, 0, toRunnable())
			tiker.execute(nextId, 0, toRunnable())
			val i = tiker.execute(nextId, 0, toRunnable())
			tiker.execute(nextId, 0, toRunnable())
			tiker.cancel(2)
		}
	}
	t2.start()
	t1.start()
	tiker.await()
	println(s"exit app")
}
