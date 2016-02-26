package just4fun.android.core.debug

import scala.collection.mutable
import android.annotation.TargetApi
import android.app.ActivityManager
import android.content.Context
import android.os._
import just4fun.core.debug.DebugUtils._
import just4fun.core.debug.measureTime

//@TargetApi(23)
object measureMemory {
	def apply(context: Context)(payload: ⇒ Unit) {
		def memInfo(): Debug.MemoryInfo = {
			context.getSystemService(Context.ACTIVITY_SERVICE).asInstanceOf[ActivityManager].getProcessMemoryInfo(Array(android.os.Process.myPid()))(0)
		}
		def toMap(info: Debug.MemoryInfo): mutable.HashMap[String, Any] = {
			val map = mutable.HashMap[String, Any]()
			if (Build.VERSION.SDK_INT >= 23) info.getMemoryStats.keySet.toArray().toIterable.foreach { key ⇒ map.put(key.toString, info.getMemoryStat(key.toString)) }
			if (Build.VERSION.SDK_INT >= 19) map.put("getTotalPrivateClean", info.getTotalPrivateClean)
			map.put("getTotalPrivateDirty", info.getTotalPrivateDirty)
			if (Build.VERSION.SDK_INT >= 19) map.put("getTotalSharedClean", info.getTotalSharedClean)
			map.put("getTotalSharedDirty", info.getTotalSharedDirty)
			map
		}
		def cmpMemo(before: Debug.MemoryInfo, after: Debug.MemoryInfo): Unit = {
			val m0 = toMap(before)
			val m1 = toMap(after)
			val buff = new StringBuilder("****************  MEMORY CONSUMPTION TEST...\n")
			buff ++= "**** BEFORE\n"
			buff ++= m0.map(pair ⇒ s"${pair._1}= ${pair._2} Kb").mkString("\n")
			buff ++= "\n"
			buff ++= "**** AFTER\n"
			buff ++= m1.map(pair ⇒ s"${pair._1}= ${pair._2} Kb").mkString("\n")
			buff ++= "\n"
			buff ++= "**** DIFF\n"
			m1.foreach { case (k, v1) ⇒
				val v0 = m0.getOrElse(k, "-2")
				val n0 = try v0.toString.toInt catch loggedE(-1)
				val n1 = try v1.toString.toInt catch loggedE(-1)
				if (n0 != n1) buff ++= s"$k:  $v1 - $v0 = ${n1 - n0}\n"
			}
			buff ++= "\n"
			logI(buff.toString())
		}
		// EXEC
		System.gc()
		System.gc()
		Thread.sleep(1000)
		val before = memInfo()
		measureTime("****************  MEMORY CONSUMPTION TEST ", 1, false)(payload)
		System.gc()
		System.gc()
		Thread.sleep(1000)
		val after = memInfo()
		cmpMemo(before, after)
	}
}
