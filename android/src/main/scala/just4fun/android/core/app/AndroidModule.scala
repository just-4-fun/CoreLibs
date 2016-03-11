package just4fun.android.core.app

import scala.collection.mutable
import scala.concurrent.Future
import android.app.{Service, Activity}
import android.content.{Intent, Context, BroadcastReceiver}
import android.os.{Bundle, IBinder}
import just4fun.android.core.vars.{TempVar, AsyncVar}
import just4fun.core.modules.{RestorableModule, ModuleContainer, Module}
import just4fun.core.debug.DebugUtils._

trait AndroidModule extends Module {
	private[app] lazy val asyncVars = mutable.ListBuffer[AsyncVar[_]]()
	override def context: ModuleApp = super.context.asInstanceOf[ModuleApp]
	private[core] def registerAsyncVar(v: AsyncVar[_]) = asyncVars += v
	private[core] def setFailed(error: Throwable, recoverable: Boolean): Unit = state.fail(error, recoverable)

	/* PERMISSIONS API */
	protected[this] def permissions: Seq[Permission] = null
	private[app] def getPermissions: Seq[Permission] = permissions

}



/* ACTIVITY */
abstract class ModuleActivity[M <: ActivityModule[_] : Manifest] extends ModuleActivityBase[M] {
	protected[this] def moduleClas: Class[M] = implicitly[Manifest[M]].runtimeClass.asInstanceOf[Class[M]]
}


trait ModuleActivityBase[M <: ActivityModule[_]] extends Activity with AppEntryPoint {
	protected[this] val context = getApplication.asInstanceOf[ModuleApp]
	val module: M = onEnter(moduleClas)
	module.callActivityConstructed(this)
	protected[this] def moduleClas: Class[M]
}


trait ActivityModule[A <: ModuleActivityBase[_]] extends AndroidModule {
	private[this] var activity: A = null.asInstanceOf[A]
	private[this] var tempVars = List[TempVar[_]]()
	private[this] var counter = 0// there can be more than 1 instance of A class, so count them

	protected[this] def activityOp: Option[A] = Option(activity)
	protected[this] def onActivityCreated(): Unit = {}
	protected[this] def onActivityDestroyed(): Unit = {}
	protected[this] def onRestoreState(inState: Bundle): Unit = {}
	protected[this] def onSaveState(outState: Bundle): Unit = {}

	private[app] def callActivityConstructed(a: ModuleActivityBase[_]): Unit = {
		activity = a.asInstanceOf[A]
	}
	private[app] def callActivityCreated(): Unit = {
		counter += 1
		onActivityCreated()
	}
	private[app] def callActivityDestroyed(): Unit = {
		onActivityDestroyed()
		counter -= 1
		activity = null.asInstanceOf[A]
		if (counter == 0) context.unbindModule(getClass)//todo ? entryPoint onExit(moduleClas)
	}
	private[app] def callRestoreState(inState: Bundle): Unit = {
		onRestoreState(inState)
	}
	private[app] def callSaveState(outState: Bundle): Unit = {
		onSaveState(outState)
	}
	private[core] def registerTempVar(v: TempVar[_]): Unit = tempVars = v :: tempVars
}




/* RECEIVER */
abstract class ModuleReceiver[M <: AndroidModule : Manifest] extends ModuleReceiverBase[M] {
	protected[this] def moduleClas: Class[M] = implicitly[Manifest[M]].runtimeClass.asInstanceOf[Class[M]]
}


trait ModuleReceiverBase[M <: AndroidModule] extends BroadcastReceiver with AppEntryPoint {
	protected[this] def moduleClas: Class[M]
	protected[this] def onReceive(module: M, context: ModuleApp, intent: Intent): Unit

	override final def onReceive(context: Context, intent: Intent): Unit = {
		val app = context.getApplicationContext.asInstanceOf[ModuleApp]
		val m = onEnter(moduleClas)
		onReceive(m, app, intent)
		onExit(moduleClas)
	}
}



/* SERVICE */
abstract class ModuleService[M <: AndroidModule : Manifest] extends ModuleServiceBase[M] {
	protected[this] def moduleClas: Class[M] = implicitly[Manifest[M]].runtimeClass.asInstanceOf[Class[M]]
}


trait ModuleServiceBase[M <: AndroidModule] extends Service with AppEntryPoint {
	protected[this] val context = getApplication.asInstanceOf[ModuleApp]
	val module: M = onEnter(moduleClas)
	protected[this] def moduleClas: Class[M]
	protected[this] def onDestroyModule(): Unit = ()

	override final def onDestroy(): Unit = {
		onExit(moduleClas)
		onDestroyModule()
	}
}



/* ENTRY POINT */
trait AppEntryPoint {
	protected[this] def onEnter[M <: AndroidModule](clas: Class[M]): M = {
		ModuleApp().onAppEnterPoint(this, clas)
	}
	protected[this] def onExit[M <: AndroidModule](clas: Class[M]): Unit = {
		ModuleApp().onAppExitPoint(this, clas)
	}
}