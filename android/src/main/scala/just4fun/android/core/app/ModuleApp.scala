package just4fun.android.core.app

import scala.collection.mutable
import scala.concurrent.Future
import scala.util.{Try, Failure, Success}
import android.app.{Activity, Application, Notification, Service}
import android.app.Application.ActivityLifecycleCallbacks
import android.content.{Context, Intent}
import android.os.{Bundle, IBinder}
import android.util.Log
import just4fun.android.core.LibRoot
import just4fun.android.core.async.{HandlerContext, ThreadPoolContext}
import just4fun.android.core.vars.Prefs
import just4fun.core.async.{DefaultAsyncContext, AsyncContext, Async}
import just4fun.core.debug.DebugConfig
import just4fun.core.debug.DebugUtils._
import just4fun.core.modules._
import just4fun.core.schemify.PropType
import just4fun.core.schemify.impls.{ArrayBufferType, ListType}

private[core] object ModuleApp {
	private var i: ModuleApp = null
	def apply(): ModuleApp = i
}


trait ModuleApp extends Application with ModuleContainer with ActivityTracker with AndroidUtils with LibResources with KeepAliveManager {
	ModuleApp.i = this
	//
	override val containerId: String = ModuleContainer.defaultId
	//	override implicit val asyncContext: AsyncContext = new HandlerContext(?)
	protected[this] implicit lazy val cache = Prefs.syscache
	protected[app] implicit val listType = new ListType[String]()
	//	protected[this] implicit val buffType = new ArrayBufferType[String]()
	override protected[this] val restoreAgent = new AndroidModuleRestoreAgent
	private[this] var firstRun = false
	private[app] lazy val permissionMgr = new PermissionsManager
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
		DebugConfig.debug(true).skipPath() // TODO remove
		  .addPackageRoot(classOf[LibRoot].getPackage.getName)
		  .addPackageRoot(getPackageName)
		  .skipTag(DefaultAsyncContext.logTag)
//		  .skipTag(PermissionManager.logTag)
		  .skipTag(Module.logTagParam)
		  .skipTag(Module.logTagStateX)
		  //		  .skipTag(Module.logTagState)
		  .skipTag(Module.logTagStat)
		logV(s"*************  ${getClass.getSimpleName} CREATED  **************")
		checkRequiredManifestEntries()
		checkFirstRun()
		onAppCreate()
		asyncContext.execute(() ⇒ restoreAgent.start())
	}
	override protected[this] final def onContainerPopulate(): Unit = {
		if (uiContext.isEmpty) startDaemon()
		// todo activity is not yet created > excess keepAlive cycle
		onAppStart()
	}
	override protected[this] final def onContainerEmpty(): Unit = {
		onAppStop()
		PropType.clean()
		ThreadPoolContext.stop()
		stopDaemon()
	}

	override protected[this] def onModulePrepare(m: Module): Future[Unit] = m match {
		case m: AndroidModule ⇒ requestPermissions(m.getPermissions: _*)
		case _ ⇒ super.onModulePrepare(m)
	}

	//	todo override def onTrimMemory(level: Int): Unit = Modules.onTrimMemory(level)
	//	todo override def onConfigurationChanged(newConfig: Configuration): Unit = Modules.mManager.onConfigurationChanged(newConfig)

	/*  permissions */
	// todo ui events: ? let peermissions manager react faster to close overlapping activity
	def hasPermission(p: String): Boolean = {
		permissionMgr.hasPermission(p)
	}
	/** Override to define custom dialog view. */
	def permissionsDialog(listener: PermissionDialogListener)(implicit context: Activity): PermissionDialogHelper = {
		new DefaultPermissionDialogHelper(listener)
	}
	def requestPermissions(perms: Permission*): Future[Unit] = {
		permissionMgr.requestPermissions(perms:_*)
	}



	/* misc internal api */
	//	private[app] def internalBindModule[M <: Module](clas: Class[M]): M = internal.bind(clas)
	//	private[app] def internalBindModule[M <: Module](clas: Class[M], constructor: () ⇒ M): M = internal.bind(clas, constructor)

	//	private[app] def onActivityConstructed[M <: ActivityModule[_]](a: ModuleActivityBase[M], clas: Class[M]): M = {
	//		a.module.callActivityConstructed(a)
	//		internal.bind(clas)
	//	}
	//	private[app] def onReceiverConstructed[M <: AndroidModule](r: ModuleReceiverBase[M], clas: Class[M]): M = {
	//		internal.bind(clas)
	//	}
	//	private[app] def onServiceConstructed[M <: AndroidModule](s: ModuleServiceBase[M], clas: Class[M]): M = {
	//		internal.bind(clas)
	//	}
	private[app] def onAppEnterPoint[M <: AndroidModule](p: AppEntryPoint, clas: Class[M]): M = {
		internal.bind(clas)
	}
	private[app] def onAppExitPoint[M <: AndroidModule](p: AppEntryPoint, clas: Class[M]): Unit = {
		unbindModule(clas)
	}

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


trait ActivityTracker extends ActivityLifecycleCallbacks {self: ModuleApp ⇒
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

	override private[app] def onActivityCreated(a: Activity, inState: Bundle): Unit = {
		if (activity == null) uiEvent = UiEvent.CREATE
		activity = a
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


trait KeepAliveManager {self: ModuleApp ⇒
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

	private[app] def startDaemon(info: Bundle = null): Unit = if (daemon == null && !isContainerEmpty) {
		val intent = new Intent(this, classOf[KeepAliveService])
		if (info != null) intent.putExtra("info", info)
		startService(intent)
		logV("KEEP ALIVE start ")
	}
	private[app] def stopDaemon(): Unit = if (daemon != null && (!keepAliveForeground || isContainerEmpty)) {
		if (keepAliveForeground) stopForeground(true)
		daemon.stopSelf()
		daemon = null
		logV("KEEP ALIVE stop")
	}

	/* SERVICE CALLBACKS */
	private[app] def onDaemonCreate(s: KeepAliveService): Unit = {
		daemon = s
		logV("KEEP ALIVE onCreate")
	}
	private[app] def onDaemonDestroy(s: KeepAliveService): Unit = {
		// WARN !! is called when app is killed via Settings
		daemon = null
		logV("KEEP ALIVE onDestroy")

		//todo remove
		Async.cancel("SPAM")(ThreadPoolContext)

	}
	private[app] def onDaemonStart(intent: Intent, flags: Int, startId: Int): Int = {
		val isRestart = flags == START_FLAG_REDELIVERY || flags == START_FLAG_RETRY
		if (intent != null) {
			val info = intent.getBundleExtra("info")
			if (info != null) {
				if (isRestart) keepAliveForeground = info.getBoolean("foreground")
				if (keepAliveForeground) {
					val id = info.getInt("id")
					val ntf = info.getParcelable[Notification]("notification")
					daemon.startForeground(id, ntf)
				}
			}
		}
		val cancel = isContainerEmpty || (uiVisible && !keepAliveForeground)
		logV(s"KEEP ALIVE onStart:   cancel? $cancel")
		if (cancel) stopDaemon()


		// TODO REmove
		var counter = 0
		if (!cancel) spam()
		def spam(): Unit = {
			logV("KEEP ALIVE  SPAM " + counter)
			counter += 1
			Async.post(10000, "SPAM") {spam()}(ThreadPoolContext)
		}

		// todo how tochange dynamically
		android.app.Service.START_REDELIVER_INTENT
	}
}



/* UTILS */
trait AndroidUtils {self: ModuleApp ⇒
	def systemService[T](contextServiceName: String): T = {
		getSystemService(contextServiceName).asInstanceOf[T]
	}
	//	def hasPermission(pm: String): Boolean = {
	////		hasPermission(pm)
	//	 todo	???
	//	}
	def dp2pix(dp: Int)(implicit context: Context): Int = {
		val displayMetrics = context.getResources.getDisplayMetrics
		((dp * displayMetrics.density) + 0.5).toInt
	}
	def pix2dp(pix: Int)(implicit context: Context): Int = {
		val displayMetrics = context.getResources.getDisplayMetrics
		((pix / displayMetrics.density) + 0.5).toInt
	}
}



/* RESOURCES */
trait LibResources {
	/** TODO Key-Value map where Value-class replaces Key-class when instantiating [[Module]]. Can be added by overriding. */
	//	val preferedModuleClasses: mutable.HashMap[Class[_], Class[_]] = null
}





/* RESTORE AGENT */
class AndroidModuleRestoreAgent(implicit system: ModuleContainer, listType: ListType[String]) extends ModuleRestoreAgent {
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