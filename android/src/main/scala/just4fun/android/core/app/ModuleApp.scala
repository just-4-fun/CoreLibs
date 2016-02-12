package just4fun.android.core.app

import android.app.Application
import android.util.Log
import just4fun.android.core.LibRoot
import just4fun.core.async.{DefaultFutureContext, FutureContext}
import just4fun.core.debug.DebugConfig
import just4fun.core.debug.DebugUtils._
import just4fun.core.modules.{Module, ModuleRestoreAgent, ModuleSystem}

private[core] object ModuleApp {
	private var i: ModuleApp = null
	def apply(): ModuleApp = i
}


trait ModuleApp extends Application with ModuleSystem with ComponentManager with AndroidUtils with LibResources with KeepAliveService with PermissionsSystem {
	ModuleApp.i = this
	override implicit val asyncContext = new DefaultFutureContext
	override protected[this] val restoreAgent = new AndroidModuleRestoreAgent
	//
	DebugConfig.debug(true).logDef(Log.println)

	/* CALLBACKS */
	protected[this] def onAppCreate(): Unit = ()
	protected[this] def onAppStart(): Unit = ()
	protected[this] def onAppStop(): Unit = ()


	/* INTERNAL API */

	override final def onCreate(): Unit = {
		super.onCreate()
		DebugConfig.debug(isDebug)
		  .addPackageRoot(classOf[LibRoot].getPackage.getName)
		  .addPackageRoot(getPackageName)
		logV(s"<<<<<<<<<<<<<<<<<<<<                    APP   CREATED                    >>>>>>>>>>>>>>>>>>>>")
		checkRequiredManifestEntries()
		onAppCreate()
		asyncContext.execute(() ⇒ restoreAgent.start())
	}
	override protected[this] final def onSystemStart(): Unit = {

		onAppStart()
	}
	override protected[this] final def onSystemStop(): Unit = {
		onAppStop()

	}

	private[app] def connectModule[M <: Module](moduleClas: Class[M]): M = {
		startModule(moduleClas)
	}
	private[app] def disconnectModule[M <: Module](moduleClas: Class[M]): Unit = {
		stopModule(moduleClas)
	}

	//	todo override def onTrimMemory(level: Int): Unit = Modules.onTrimMemory(level)
//	todo override def onConfigurationChanged(newConfig: Configuration): Unit = Modules.mManager.onConfigurationChanged(newConfig)


	/* MISC INTERNAL API */

	private[this] def isDebug: Boolean = {
		try {
			val clas = Class.forName(getPackageName + ".BuildConfig")
			val valueF = clas.getDeclaredField("DEBUG")
			valueF.setAccessible(true)
			valueF.getBoolean(null)
		} catch {case e: Throwable => println(e); false}
	}
	private[this] def checkRequiredManifestEntries(): Unit = {
		val warn = new StringBuilder()
		// check KeepAliveService
		//todo
//		val clas: Class[_] = classOf[KeepAliveService]
//		val intent = new Intent(this, clas)
//		val resolvers: java.util.List[_] = getPackageManager.queryIntentServices(intent, 0)
//		if (resolvers == null || resolvers.size() == 0) warn ++= s"""\n<service android:name="${clas.getName}"/>"""
//		if (warn.nonEmpty) throw new Exception(s"The following components are required by ${classOf[LibRoot].getPackage.getName} library and should be declared in your AndroidManifest.xml:$warn")
	}

}







/* COMPONENT MANAGER */
trait ComponentManager {
	self: ModuleApp ⇒

}




/* KEEP ALIVE */
trait KeepAliveService {

}



/* UTILS */
trait AndroidUtils {

}



/* RESOURCES */
trait LibResources {

}




/* PERMISSIONS */
trait PermissionsSystem {

}






/* RESTORE AGENT */
class AndroidModuleRestoreAgent extends ModuleRestoreAgent {
	override lazy val autoStart: Boolean = false
	override protected[this] def getList: TraversableOnce[String] = ???
	override protected[this] def clearList(): Unit = ???
	override protected[this] def remove(moduleClass: String): Unit = ???
	override protected[this] def add(moduleClass: String): Unit = ???
}