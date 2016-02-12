package just4fun.android.core.app

import android.app.{Service, Activity}
import android.content.{Intent, Context, BroadcastReceiver}
import android.os.IBinder
import just4fun.core.modules.{RestorableModule, ModuleSystem, Module}

trait AndroidModule extends Module {
	override protected[this] def system: ModuleApp = super.asInstanceOf[ModuleApp]
}



/* ACTIVITY */
trait ActivityModule[A <: ModuleActivityBase[_]] extends AndroidModule {
	private[this] var activity: A = null.asInstanceOf[A]

	def activityOp: Option[A] = Option(activity)

	private[app] def callActivityConstructed(a: ModuleActivityBase[_]): Unit = {
		activity = a.asInstanceOf[A]
		// todo callbacks
	}
}


class ModuleActivity[M <: ActivityModule[_] : Manifest] extends ModuleActivityBase[M] {
	protected[app] def moduleClas: Class[M] = implicitly[Manifest[M]].runtimeClass.asInstanceOf[Class[M]]
}


trait ModuleActivityBase[M <: ActivityModule[_]] extends Activity {
	protected[app] def moduleClas: Class[M]
	val module: M = ModuleApp().connectModule(moduleClas)
	module.callActivityConstructed(this)

}



/* RECEIVER */
abstract class ModuleReceiver[M <: AndroidModule : Manifest] extends ModuleReceiverBase[M] {
	protected[this] def moduleClas: Class[M] = implicitly[Manifest[M]].runtimeClass.asInstanceOf[Class[M]]
}

trait ModuleReceiverBase[M <: AndroidModule] extends BroadcastReceiver {
	protected[this] def moduleClas: Class[M]
	protected[this] def onReceive(module: M, context: Context, intent: Intent): Unit

	override final def onReceive(context: Context, intent: Intent): Unit = {
		val m = ModuleApp().connectModule(moduleClas)
		onReceive(m, context, intent)
		ModuleApp().disconnectModule(moduleClas)
	}
}



/* SERVICE */
trait ModuleServiceBase extends Service with AndroidModule {
	protected[this] def onDestroyModule(): Unit = ()
	/** Override because module is intended to be singleton and be created outside system (by Android framework). */
	override protected[this] def system: ModuleApp = ModuleApp()
	override final def onDestroy(): Unit = {
		unbindSelf()
		onDestroyModule()
	}
}



