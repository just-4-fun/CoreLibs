package just4fun.android.core.app

import scala.collection.mutable
import android.app.{Activity, Application, Notification, Service}
import android.app.Application.ActivityLifecycleCallbacks
import android.content.Intent
import android.os.{Bundle, IBinder}
import android.util.Log
import just4fun.android.core.LibRoot
import just4fun.android.core.async.{HandlerContext, ThreadPoolContext}
import just4fun.android.core.vars.Prefs
import just4fun.core.async.{DefaultAsyncContext, AsyncContext, Async}
import just4fun.core.debug.DebugConfig
import just4fun.core.debug.DebugUtils._
import just4fun.core.modules.{Module, ModuleRestoreAgent, ModuleSystem}
import just4fun.core.schemify.PropType
import just4fun.core.schemify.impls.ListType

private[core] object ModuleApp {
	private var i: ModuleApp = null
	def apply(): ModuleApp = i
}


trait ModuleApp extends Application with ModuleSystem with ActivityTracker with AndroidUtils with LibResources with KeepAliveManager with PermissionsSystem {
	ModuleApp.i = this
	//
	override val systemId: String = ModuleSystem.defaultIdentifier
//	override implicit val asyncContext: AsyncContext = new HandlerContext(?)
	protected[this] implicit lazy val cache = Prefs.syscache
	protected[this] implicit val listType = new ListType[String]()
	override protected[this] val restoreAgent = new AndroidModuleRestoreAgent
	private[this] var firstRun = false
	//
	DebugConfig.debug(true).logDef(Log.println(_, _, _))

	/* public api */
	def isFirstRun = firstRun

	/* callbacks */
	protected[this] def onAppCreate(): Unit = ()
	protected[this] def onAppStart(): Unit = ()
	protected[this] def onAppStop(): Unit = ()


	/* internal api */

	override final def onCreate(): Unit = {
		super.onCreate()
		DebugConfig.debug(isDebug)
		DebugConfig.debug(true).skipPath()// TODO remove
		  .addPackageRoot(classOf[LibRoot].getPackage.getName)
		  .addPackageRoot(getPackageName)
		  .skipTag(DefaultAsyncContext.tag)
		  .skipTag(Module.logTagParam)
		  .skipTag(Module.logTagStateX)
		  .skipTag(Module.logTagState)
		  .skipTag(Module.logTagStat)
		logV(s"<<<<<<<<<<<<<<<<<<<<                    APP   CREATED                    >>>>>>>>>>>>>>>>>>>>")
		checkRequiredManifestEntries()
		checkFirstRun()
		onAppCreate()
		asyncContext.execute(() ⇒ restoreAgent.start())
	}
	override protected[this] final def onSystemStart(): Unit = {
		logV(s"<<<<<<<<<<<<<<<<<<<<                    APP   START                    >>>>>>>>>>>>>>>>>>>>")
		if (uiContext.isEmpty) startDaemon()
//		initPermissionSubsystem()
		onAppStart()
	}
	override protected[this] final def onSystemStop(): Unit = {
		onAppStop()
//		exitPermissionSubsystem()
		PropType.clean()
		ThreadPoolContext.stop()
		stopDaemon()
		logV(s"<<<<<<<<<<<<<<<<<<<<                    APP   STOP                    >>>>>>>>>>>>>>>>>>>>")
	}

	//	todo override def onTrimMemory(level: Int): Unit = Modules.onTrimMemory(level)
//	todo override def onConfigurationChanged(newConfig: Configuration): Unit = Modules.mManager.onConfigurationChanged(newConfig)


	/* misc internal api */
	private[app] def internalBindModule[M <: Module](clas: Class[M]): M = internal.bind(clas)
	private[app] def internalBindModule[M <: Module](clas: Class[M], constructor: () ⇒ M): M = internal.bind(clas, constructor)

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
		/** check [[KeepAliveService]] */
		val clas: Class[_] = classOf[KeepAliveService]
		val intent = new Intent(this, clas)
		val resolvers: java.util.List[_] = getPackageManager.queryIntentServices(intent, 0)
		if (resolvers == null || resolvers.size() == 0) warn ++= s"""\n<service android:name="${clas.getName}"/>"""
		if (warn.nonEmpty) throw new Exception(s"The following components are required by ${classOf[LibRoot].getPackage.getName} library and should be declared in your AndroidManifest.xml:$warn")
	}
	private[this] def checkFirstRun(): Unit = {
		implicit val cache = Prefs.syscache
		val KEY_LAUNCHED = "app_launched_"
		firstRun = !Prefs.contains(KEY_LAUNCHED)
		if (firstRun) Prefs(KEY_LAUNCHED) = 1
	}
}





/* COMPONENT MANAGER */
object ActivityState extends Enumeration {
	val NONE, CREATED, STARTED, RESUMED, PAUSED, STOPPED, DESTROYED = Value
}


object UiEvent extends Enumeration {
	val NONE, CREATE, SHOW, HIDE, DESTROY = Value
}


trait ActivityTracker extends ActivityLifecycleCallbacks {
	self: ModuleApp ⇒
	import ActivityState._
	private[this] var activity: Activity = _
	private[this] var state: ActivityState.Value = NONE
	private[this] var uiEvent: UiEvent.Value = UiEvent.NONE
	private[this] var reconfiguring = false
	private[this] var visible = false
	//
	registerActivityLifecycleCallbacks(this)

	def uiContext: Option[Activity] = Option(activity)
	def uiVisible = state == RESUMED
	def uiAlive = state >= CREATED && state < DESTROYED && !activity.isFinishing && (!reconfiguring || state < PAUSED)

	private[app] def onActivityConstructed(a: Activity): Unit = {
//		if (activity == null) uiEvent = UiEvent.CREATE
		activity = a
		fireConstruct(a)
	}
	override private[app] def onActivityCreated(a: Activity, inState: Bundle): Unit = {
//		if (activity == null) uiEvent = UiEvent.CREATE
		activity = a
		uiEvent = UiEvent.CREATE
		onStateChange(a, CREATED)
		if (!reconfiguring) {
			fireCreate(a)
			if (inState != null) fireRestore(a, inState)
		}
	}
	override private[app] def onActivityStarted(a: Activity): Unit = {
		activity = a
		onStateChange(a, STARTED)
	}
	override private[app] def onActivityResumed(a: Activity): Unit = {
		activity = a
		if (!visible) {
			visible = true
			uiEvent = UiEvent.SHOW
			stopDaemon()
		}
		reconfiguring = false
		onStateChange(a, RESUMED)
	}
	override private[app] def onActivityPaused(a: Activity): Unit = {
		if (activity == a) reconfiguring = a.isChangingConfigurations
		onStateChange(a, PAUSED)
	}
	override private[app] def onActivitySaveInstanceState(a: Activity, outState: Bundle): Unit = {
		fireSave(a, outState)
	}
	override private[app] def onActivityStopped(a: Activity): Unit = {
		if (activity == a && !a.isChangingConfigurations) {
			visible = false
			uiEvent = UiEvent.HIDE
			startDaemon()
		}
		onStateChange(a, STOPPED)
	}
	override private[app] def onActivityDestroyed(a: Activity): Unit = {
		if (activity == a) {
			reconfiguring = a.isChangingConfigurations
			if (!reconfiguring) {
				activity = null
				uiEvent = UiEvent.DESTROY
			}
		}
		onStateChange(a, DESTROYED)
		if (!reconfiguring) fireDestroy(a)
	}
	private def onStateChange(a: Activity, newStt: Value): Unit = {
		val isCurrent = activity == null || activity == a
		if (isCurrent) state = newStt
		fireUiEvent()
		fireActivityEvent(ActivityEvent(a.hashCode, newStt, isCurrent, reconfiguring, a.isFinishing, uiEvent))
		uiEvent = UiEvent.NONE
	}
	private def reason(a: Activity): String = if (uiEvent != UiEvent.NONE) uiEvent.toString else if (a.isFinishing) "finishing" else if (reconfiguring) "reconfiguring" else "replacing"

	private[this] def fireConstruct(a: Activity): Unit = {
		a match {
		  case a: ModuleActivityBase[_] => a.module.callActivityConstructed(a)
		  case _ =>
		}
	}
	private[this] def fireCreate(a: Activity): Unit = {
		a match {
		  case a: ModuleActivityBase[_] => a.module.callActivityCreated()
		  case _ =>
		}
	}
	private[this] def fireRestore(a: Activity, inState: Bundle): Unit = {
		a match {
			case a: ModuleActivityBase[_] => a.module.callRestoreState(inState)
			case _ =>
		}
	}
	private[this] def fireSave(a: Activity, outState: Bundle): Unit = {
		a match {
			case a: ModuleActivityBase[_] => a.module.callSaveState(outState)
			case _ =>
		}
	}
	private[this] def fireDestroy(a: Activity): Unit = {
		a match {
			case a: ModuleActivityBase[_] => a.module.callActivityDestroyed()
			case _ =>
		}
	}
	private[this] def fireActivityEvent(e: ActivityEvent): Unit = {
		logV(e.toString)
		// todo fire event
	}
	private[this] def fireUiEvent(): Unit = if (uiEvent != UiEvent.NONE) {
		// todo fire event
	}
}


/* EVENT */
case class ActivityEvent(hash: Int, state: ActivityState.Value, current: Boolean, reconfiguring: Boolean, finishing: Boolean, uiEvent: UiEvent.Value) {
	override def toString: String = s"Current= $current;  reason= ${if (uiEvent != UiEvent.NONE) uiEvent.toString else if (finishing) "finishing" else if (reconfiguring) "reconfiguring" else "replacing"};  state= $state"
}




/* KEEP ALIVE */
class KeepAliveService extends Service {
	ModuleApp().onDaemonCreate(this)
//	override def onCreate(): Unit = ModuleApp().onDaemonCreate(this)
	override def onStartCommand(intent: Intent, flags: Int, startId: Int): Int = {
		ModuleApp().onDaemonStart(intent, flags, startId)
	}
	override def onDestroy(): Unit = ModuleApp().onDaemonDestroy(this)
	override def onBind(intent: Intent): IBinder = null
}


trait KeepAliveManager {
	self: ModuleApp ⇒
	import Service.{START_FLAG_REDELIVERY, START_FLAG_RETRY}
	private[this] var daemon: Service = _
	private[this] var keepAliveForeground = false

	def startForeground(id: Int, notification: Notification): Unit = {
		keepAliveForeground = true
		if (daemon == null) {
			val info = new Bundle
			info.putParcelable("notification", notification)
			info.putInt("id", id)
			info.putBoolean("foreground", keepAliveForeground)
			startDaemon(info)
		}
		else daemon.startForeground(id, notification)
	}
	def stopForeground(removeNotification: Boolean): Unit = {
		keepAliveForeground = false
		if (daemon != null) daemon.stopForeground(removeNotification)
	}

	private[app] def startDaemon(info: Bundle = null): Unit = if (daemon == null && isSystemStarted) {
		val intent = new Intent(this, classOf[KeepAliveService])
		if (info != null) intent.putExtra("info", info)
		startService(intent)
		logV("start KEEP ALIVE")
	}
	private[app] def stopDaemon(): Unit = if (daemon != null && (!keepAliveForeground || !isSystemStarted)) {
		if (keepAliveForeground) stopForeground(true)
		daemon.stopSelf()
		daemon = null
		logV("stop KEEP ALIVE")
	}

	/* SERVICE CALLBACKS */
	private[app]  def onDaemonCreate(s: KeepAliveService): Unit = {
		daemon = s
		logV("onCreate")
	}
	private[app]  def onDaemonDestroy(s: KeepAliveService): Unit = {
		// WARN !! is called when app is killed via Settings
		daemon = null
		logV("onDestroy")
	}
	private[app]  def onDaemonStart(intent: Intent, flags: Int, startId: Int): Int = {
		val isRestart = flags == START_FLAG_REDELIVERY || flags == START_FLAG_RETRY
		if (intent != null) {
			val info = intent.getBundleExtra("info")
			if(info != null) {
				if (isRestart) keepAliveForeground = info.getBoolean("foreground")
				if (keepAliveForeground) {
					val id = info.getInt("id")
					val ntf = info.getParcelable[Notification]("notification")
					daemon.startForeground(id, ntf)
				}
			}
		}
		val cancel = !isSystemStarted || (uiVisible && !keepAliveForeground)
		logV(s"onStart:   cancel? $cancel")
		if (cancel) stopDaemon()


		// TODO REmove
		var counter = 0
		if (!cancel) spam()
		def spam(): Unit = {
			logV("KeepAlive SPAM "+ counter )
			counter += 1
			Async.post(10000, "SPAM") {spam()} (ThreadPoolContext)
		}

		// todo how tochange dynamically
		android.app.Service.START_REDELIVER_INTENT
	}
}



/* UTILS */
trait AndroidUtils {
	self: ModuleApp ⇒
	def systemService[T](contextServiceName: String): T = {
		getSystemService(contextServiceName).asInstanceOf[T]
	}
//	def hasPermission(pm: String): Boolean = {
////		hasPermission(pm)
//	 todo	???
//	}
}



/* RESOURCES */
trait LibResources {

}




/* PERMISSIONS */
trait PermissionsSystem {

}






/* RESTORE AGENT */
class AndroidModuleRestoreAgent(implicit system: ModuleSystem, listType: ListType[String]) extends ModuleRestoreAgent {
	private[this] val KEY_RESTORE = "restorables_"
	private[this] val restorables = mutable.Set[String]()

	override lazy val autoStart: Boolean = false
	override protected[this] def getList: TraversableOnce[String] = {
		Prefs[List[String]](KEY_RESTORE)
	}
	override protected[this] def clearList(): Unit = {
		restorables.clear()
		Prefs(KEY_RESTORE) = restorables.toList
		logV(s"Restorables clear > ${Prefs[String](KEY_RESTORE)}")
	}
	override protected[this] def remove(moduleClass: String): Unit = {
		restorables.remove(moduleClass)
		Prefs(KEY_RESTORE) = restorables.toList
		logV(s"Restorables remove > ${Prefs[String](KEY_RESTORE)}")
	}
	override protected[this] def add(moduleClass: String): Unit = {
		restorables.add(moduleClass)
		Prefs(KEY_RESTORE) = restorables.toList
		logV(s"Restorables add > ${Prefs[String](KEY_RESTORE)}")
	}
}