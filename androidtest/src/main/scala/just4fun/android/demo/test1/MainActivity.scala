package just4fun.android.demo.test1

import scala.collection.mutable
import android.annotation.TargetApi
import android.app.ActivityManager
import android.os.{Build, Debug, Bundle}
import just4fun.android.core.app.{ActivityTracker, ActivityModule, ModuleActivity}
import just4fun.android.core.debug.measureMemory
import just4fun.android.demo.R
import just4fun.core.debug.measureTime
import just4fun.core.debug.DebugUtils._
import just4fun.core.modules.{ModuleSystem, Module}

class MainActivity extends ModuleActivity[MainModule] {
	override def onCreate(savedInstanceState: Bundle) {
		super.onCreate(savedInstanceState)
		setContentView(R.layout.main)
		setTitle("Test1 MainActivity")
	}
}


class MainModule extends ActivityModule[MainActivity] {
	override protected[this] def onActivityCreated(): Unit = {
//			testPerformance()
		testMemory()
	}

	def testMemory(): Unit = {
		logV(s"RUN TEST MEMORY.........................................................................................................................")
		measureMemory(activityOp.get) {
			logV(s"TEST MEMORY.........................................................................................................................")
//			val arr = List[String]().padTo(400000, "0123456789")// 1.5 byte per char
			val list = for (n <- 1 to 1000) yield system.bindModule(() ⇒ new TestExtModule{}) // uncomment fail
//			val list =  for (n <- 0 to 10) yield new AnyRef{}
		}
	}
	def testPerformance(): Unit = {
		{run; run; run; run; run; run; run; run; run; run; run; run;}
		def run = measureTime("ANDRIOD", 1) {
			var sum = 0
			for (n <- 0 to 1000000) sum += 1
			sum
		}
	}
}

object TestExtModule {
	var count = 0
	def nextId = {
		count += 1
		count.toString
	}
}
trait TestExtModule extends Module {
	override val moduleId: String = TestExtModule.nextId
}
