package just4fun.android.core.app

import scala.collection.mutable
import android.app.{Service, Activity}
import android.content.{Intent, Context, BroadcastReceiver}
import android.os.{Bundle, IBinder}
import just4fun.android.core.vars.{TempVar, AsyncVar}
import just4fun.core.modules.{RestorableModule, ModuleSystem, Module}
import just4fun.core.debug.DebugUtils._

trait AndroidModule extends Module {
	private[app] lazy val asyncVars = mutable.ListBuffer[AsyncVar[_]]()
	override def system: ModuleApp = super.system.asInstanceOf[ModuleApp]
	private[core] def registerAsyncVar(v: AsyncVar[_]) = asyncVars += v
}



/* ACTIVITY */
trait ActivityModule[A <: ModuleActivityBase[_]] extends AndroidModule {
	private[this] var activity: A = null.asInstanceOf[A]
	private[this] var tempVars = List[TempVar[_]]()
	private[this] var counter = 0

	protected[this] def activityOp: Option[A] = Option(activity)
	protected[this] def onActivityConstructed(): Unit = {}
	protected[this] def onActivityCreated(): Unit = {}
	protected[this] def onActivityDestroyed(): Unit = {}
	protected[this] def onRestoreState(inState: Bundle): Unit = {}
	protected[this] def onSaveState(outState: Bundle): Unit = {}

	private[app] def callActivityConstructed(a: ModuleActivityBase[_]): Unit = {
		activity = a.asInstanceOf[A]
		onActivityConstructed()
	}
	private[app] def callActivityCreated(): Unit = {
		counter += 1
		onActivityCreated()
	}
	private[app] def callActivityDestroyed(): Unit = {
		onActivityDestroyed()
		counter -= 1
		activity = null.asInstanceOf[A]
		if (counter == 0) system.unbindModule(getClass)
	}
	private[app] def callRestoreState(inState: Bundle): Unit = {
		onRestoreState(inState)
	}
	private[app] def callSaveState(outState: Bundle): Unit = {
		onSaveState(outState)
	}
	private[core] def registerTempVar(v: TempVar[_]): Unit = tempVars = v :: tempVars
}


abstract class ModuleActivity[M <: ActivityModule[_] : Manifest] extends ModuleActivityBase[M] {
	protected[app] def moduleClas: Class[M] = implicitly[Manifest[M]].runtimeClass.asInstanceOf[Class[M]]
}


trait ModuleActivityBase[M <: ActivityModule[_]] extends Activity {
	protected[app] def moduleClas: Class[M]
	val module: M = ModuleApp().internalBindModule(moduleClas)
	ModuleApp().onActivityConstructed(this)
}



/* RECEIVER */
abstract class ModuleReceiver[M <: AndroidModule : Manifest] extends ModuleReceiverBase[M] {
	protected[this] def moduleClas: Class[M] = implicitly[Manifest[M]].runtimeClass.asInstanceOf[Class[M]]
}

trait ModuleReceiverBase[M <: AndroidModule] extends BroadcastReceiver {
	protected[this] def moduleClas: Class[M]
	protected[this] def onReceive(module: M, context: Context, intent: Intent): Unit

	override final def onReceive(context: Context, intent: Intent): Unit = {
		val m = ModuleApp().internalBindModule(moduleClas)
		onReceive(m, context, intent)
		ModuleApp().unbindModule(moduleClas)
	}
}



/* SERVICE */
//trait ModuleServiceBase extends Service with AndroidModule with ExternalModule {
//	protected[this] def onDestroyModule(): Unit = ()
//	override protected[this] def bindingSystemIdentifier: String = ModuleSystem.defaultIdentifier
//
//	override final def onDestroy(): Unit = {
//		system.unbindModule(getClass)
//		onDestroyModule()
//	}
//}



