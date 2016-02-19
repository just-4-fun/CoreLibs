package just4fun.android.core.app

import scala.collection.mutable
import android.app.{Service, Activity}
import android.content.{Intent, Context, BroadcastReceiver}
import android.os.{Bundle, IBinder}
import just4fun.android.core.vars.{TempVar, AsyncVar}
import just4fun.core.modules.{RestorableModule, ModuleSystem, Module}

trait AndroidModule extends Module {
	private[app] lazy val asyncVars = mutable.ListBuffer[AsyncVar[_]]()
	override protected[this] def system: ModuleApp = super.system.asInstanceOf[ModuleApp]
	private[core] def registerAsyncVar(v: AsyncVar[_]) = asyncVars += v
}



/* ACTIVITY */
trait ActivityModule[A <: ModuleActivityBase[_]] extends AndroidModule {
	private[this] var activity: A = null.asInstanceOf[A]
	private[this] var tempVars = List[TempVar[_]]()
	private[this] var counter = 0

	def activityOp: Option[A] = Option(activity)

	private[app] def onActivityConstructed(a: ModuleActivityBase[_]): Unit = {
		activity = a.asInstanceOf[A]
	}
	private[app] def onActivityCreated(): Unit ={
		counter += 1
	}
	private[app] def onActivityDestroyed(): Unit ={
		counter -= 1
		if (counter == 0) unbindSelf()
	}
	private[app] def onRestoreState(inState: Bundle): Unit ={

	}
	private[app] def onSaveState(outState: Bundle): Unit ={

	}
	private[core] def registerTempVar(v: TempVar[_]): Unit = tempVars = v :: tempVars
}


class ModuleActivity[M <: ActivityModule[_] : Manifest] extends ModuleActivityBase[M] {
	protected[app] def moduleClas: Class[M] = implicitly[Manifest[M]].runtimeClass.asInstanceOf[Class[M]]
}


trait ModuleActivityBase[M <: ActivityModule[_]] extends Activity {
	protected[app] def moduleClas: Class[M]
	val module: M = ModuleApp().connectModule(moduleClas)
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



