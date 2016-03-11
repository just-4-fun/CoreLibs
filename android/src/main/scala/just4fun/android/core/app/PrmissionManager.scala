package just4fun.android.core.app

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Future, Promise}
import android.annotation.TargetApi
import android.app.{Activity, DialogFragment}
import android.content.Intent
import android.content.pm.{PermissionGroupInfo, PermissionInfo}
import android.content.pm.PackageManager._
import android.net.Uri
import android.os.{Build, Bundle}
import android.provider.Settings
import android.util.TypedValue
import android.view._
import android.view.ViewGroup.LayoutParams
import android.widget._
import just4fun.android.core.app.PermissionManager._
import just4fun.android.core.async.MainThreadContext
import just4fun.android.core.vars.Prefs
import just4fun.core.debug.DebugUtils._
import just4fun.core.modules.ModuleException

object PermissionManager {
	private[app] val KEY_REQUESTED_PMS = "_dontAskPms_"
	private[this] var requestId = 9
	private[app] def nextId = { requestId += 1; requestId }
	val logTag = 403812566
}

class PermissionsManager {
	private[this] val context = ModuleApp()
	private[this] var started = 0L
	private[this] var requests: List[PermissionRequest] = null
	private[app] var permissions: List[Permission] = null
	private[this] var requestedPms: List[String] = null
	private[this] var dontAskPms: List[String] = null
	private[this] var manifestPms: List[String] = null
	private[this] var handler: PermissionsRequestHandler = null
	private[this] val ph_tag = "permissions_h"
	private[this] val asyncId = "permit_handle"
	private[this] val lock = new Object
	private[this] lazy val successF = Promise[Unit]().success().future
	private[this] implicit val listType = context.listType

	/* public */

	def hasPermission(p: String): Boolean = {
		context.checkPermission(p, android.os.Process.myPid(), android.os.Process.myUid()) == PERMISSION_GRANTED
	}
	/** Returns future success if all permissions are granted, or denied permissions are non-critical (i.e. [[just4fun.android.core.app.Permission]].critical is false). Otherwise returns future failure.
	  * Request returns future failure immediately if not all critical permissions are declared in Manifest.
	  * Request returns future success immediately if Api level < 23 or permissions are empty or already granted or there are no denied critical permissions and all non-critical permissions are either not declared in Manifest or checked with 'Never ask again'.
	  * Rationale Dialog with permission descriptions will be shown if requested permission has [[just4fun.android.core.app.Permission]].rationaleResId > 0 or critical permission was checked previously with 'Never ask again'.
	  * After pressing 'OK' on dialog If requested permission was previously checked with 'Never ask again' the App Settings page will be displayed. Otherwise standard system permissions dialog will be displayed.
	  */
	def requestPermissions(perms: Permission*): Future[Unit] = {
		if (Build.VERSION.SDK_INT < 23 || perms == null || perms.isEmpty) successF
		else lock.synchronized {
			logD(s"PMS requestPermissions ${perms.map(_.name).mkString(", ")} ", logTag)
			if (permissions == null) initialize()
			val promise = Promise[Unit]()
			val actualPms = perms.filter { p ⇒
				!hasPermission(p.name) && {
					p.dontAsk = dontAskPms.contains(p.name)
					val declared = isDeclared(p.name)
					if (!declared && p.critical) promise.tryFailure(new ModulePermissionException(p.name))
					declared && (p.critical || !p.dontAsk)
				}
			}
			if (promise.isCompleted) ()
			else if (actualPms.isEmpty) promise.success()
			else {
				actualPms.foreach(putPermission)
				requests ::= new PermissionRequest(promise, ArrayBuffer(actualPms.map(_.copy): _*))
				handle()
			}
			if (permissions.isEmpty) utilize()
			promise.future
		}
	}

	/* handling */

	private[this] def initialize(): Unit = {
		permissions = Nil
		requests = Nil
		val pmArray = context.getPackageManager.getPackageInfo(context.getPackageName, GET_PERMISSIONS).requestedPermissions
		manifestPms = if (pmArray == null) Nil else pmArray.toList
		dontAskPms = Nil
		requestedPms = Prefs.getOrElse(KEY_REQUESTED_PMS, List[String]())
		requestedPms = requestedPms.map {
			case p if p.charAt(0) == '-' ⇒ val p0 = p.substring(1); dontAskPms ::= p0; p0
			case p ⇒ p
		}
		logD(s"PMS at init dontAsk= ${dontAskPms.mkString(", ")}", logTag)
	}
	private[this] def utilize(): Unit = {
		permissions = null
		requests = null
		handler = null
		manifestPms = null
		if (requestedPms.nonEmpty) Prefs(KEY_REQUESTED_PMS) = requestedPms.map { p ⇒ if (dontAskPms.contains(p)) "-" + p else p }
		logD(s"PMS  at utilizing   dontAsk: dontAskPms= ${dontAskPms.mkString(",")};  requestedPms=  ${Prefs(KEY_REQUESTED_PMS)};", logTag)
		dontAskPms = null
		requestedPms = null
		started = 0
	}
	private[this] def isDeclared(p: String): Boolean = manifestPms.contains(p) match {
		case true ⇒ true
		case _ ⇒ logE(s"Permission $p is requested but not declared in Manifest"); false
	}
	private[this] def putPermission(p: Permission): Unit = permissions.indexOf(p) match {
		case -1 ⇒ permissions ::= p
		case ix ⇒ val p0 = permissions(ix)
			if (p.critical) p0.critical = true
			if (p.rationaleResId > 0 && p0.rationaleResId == 0) p0.rationaleResId = p.rationaleResId
	}
	private[app] def hasPms: Boolean = lock.synchronized(permissions != null)
	/** Leaves only first from group as if one permission is granted than others are granted either. */
	private[app] def assignRequestId(requestId: Int): Unit = lock.synchronized {
		permissions.foreach(p ⇒ if (p.requestId == 0) p.requestId = requestId)
	}
	private[app] def orderedPms: Seq[Permission] = lock.synchronized {
		var groups = List[String]()
		permissions.sortBy(!_.critical).filter { p =>
			val g = p.info.group
			if (g == null) true
			else if (groups.contains(g)) false
			else {groups = g :: groups; true}
		}
	}
	private[this] def handle(): Unit = lock.synchronized {
		if (started == 0) {
			started = System.currentTimeMillis
			MainThreadContext.execute(asyncId, 0, () ⇒ tryStartHandler())
		}
		//DEFs
		def tryStartHandler(): Unit = {
			if (context.isContainerEmpty) ()
			else if (context.uiAlive) {
				handler = new PermissionsRequestHandler
				context.uiContext.get.getFragmentManager.beginTransaction().add(handler, ph_tag).commit()
				postCheckHandler()
				logD(s"PMS     showing dialog", logTag)
			} else {
				val waitingUi = System.currentTimeMillis - started
				logD(s"    PMS waiting Ui $waitingUi ms", logTag)
				if (waitingUi > 1000 || (context.uiContext.isEmpty && waitingUi > 500)) onHandle(0)
				else MainThreadContext.execute(asyncId, 100, () ⇒ tryStartHandler(), true)
			}
		}
		def checkHandler(): Unit = {
			logD(s"PMS     checking handler= $handler;  hasUi? ${context.uiContext.nonEmpty};  ", logTag)
			if (handler == null) ()
			else if (context.uiContext.isEmpty) onHandle(0)
			else if (context.uiContext.get.getFragmentManager.findFragmentByTag(ph_tag) == null && handler.getActivity != null) {
				// WARN: finish activity that overlaps current (Home > Launch).
				context.uiContext.get.finish()
				postCheckHandler()
			}
			else postCheckHandler()
		}
		def postCheckHandler(): Unit = MainThreadContext.execute(asyncId, 4000, () ⇒ checkHandler(), true)
	}
	@TargetApi(23) private[app] def onHandle(id: Int): Boolean = lock.synchronized {
		// check started because something may went wrong with handler swapping
		if (context.isContainerEmpty && started == 0) return true
		val allDone = id == 0
		permissions = permissions.filterNot { p ⇒
			val granted = hasPermission(p.name)
			val done = granted || allDone || p.requestId == id
			if (done) {
				requests = requests.filterNot(_.isCompleted(p, granted))
				val asked = requestedPms.contains(p.name)
				if (!asked) requestedPms ::= p.name
				if (granted) dontAskPms = dontAskPms.filterNot(_ == p.name)
				else {
					val canAsk = !asked || context.uiContext.get.shouldShowRequestPermissionRationale(p.name)
					if (canAsk && p.dontAsk) dontAskPms = dontAskPms.filterNot(_ == p.name)
					else if (!canAsk && !dontAskPms.contains(p.name)) dontAskPms ::= p.name
				}
				logD(s"PMS     [${p.name}]::  id= ${p.requestId};   allDone? $allDone;  granted? $granted;  done? $done", logTag)
			}
			done
		}
		if (permissions.isEmpty) {
			MainThreadContext.cancel(asyncId)
			if (requests.nonEmpty) logW(s"!!!!!!!!!!!!!!!!!!!!!!!!!!  PMS requests non empty ${requests.length}  !!!!!!!!!!!!!!!!!!!!!!!!!!", logTag)
			requests.foreach(_.onCompleted())
			utilize()
		}
		logD(s"PMS             handle RESULT: ${if (hasPms) permissions.length else "0"}", logTag)
		permissions == null
	}
}




/* HANDLER  DIALOG FRAGMENT */
class PermissionsRequestHandler extends DialogFragment with PermissionDialogListener {
	val (sNONE, sEXPECT_RESULT, sEXPECT_SETTINGS) = (0, 1, 2)
	val context = ModuleApp()
	val manager = context.permissionMgr
	implicit val cache = Prefs.syscache
	implicit val listType = context.listType
	//getActivity.getApplication.asInstanceOf[ModuleApp]
	var dialog: PermissionDialogHelper = null
	var state: Int = sNONE

	override def onCreate(state: Bundle): Unit = {
		if (state != null) onRestoreInstanceState(state)
		if (!manager.hasPms) onCancel()
		else {
			dialog = context.permissionsDialog(this)(getActivity)
			val theme = if (dialog.theme != null) dialog.theme.resId
			else android.R.style.Theme_DeviceDefault_Dialog_Alert
			setStyle(DialogFragment.STYLE_NO_TITLE, theme)
		}
		setCancelable(false)
		super.onCreate(state)
	}
	override def onCreateView(inflater: LayoutInflater, container: ViewGroup, state: Bundle): View = {
		val v = dialog.createView()
		beginRequest()
		//			logD(s"CREATED STATIC PMS DIALOG")
		v
	}
	private[this] def beginRequest(): Unit = {
		val shouldShow = manager.permissions.exists(p ⇒ p.rationaleResId > 0 || (p.critical && p.dontAsk))
		if (shouldShow) {
			if (isHidden) getFragmentManager.beginTransaction().show(this).commitAllowingStateLoss()
			dialog.update(manager.orderedPms)
		} else {
			if (!isHidden) getFragmentManager.beginTransaction().hide(this).commitAllowingStateLoss()
			requestPms()
		}
	}
	override def onRequestPermissionsResult(id: Int, pms: Array[String], res: Array[Int]): Unit = {
		//		logD(s"PMS ON REQUESTED ID= $id; results:: ${if (pms != null) pms.zip(res).map(p => s"${p._1}=${p._2}").mkString(", ") else "null"}", tag)
		state = sNONE
		manager.onHandle(id) match {
			case true => dismiss()
			case _ => beginRequest()
		}
	}
	def onOk(): Unit = {
		val hasDontAsk = manager.permissions.exists(p => p.critical && p.dontAsk)
		logD(s"PMS     onOk :: hasDontAsk? $hasDontAsk;  state? $state", logTag)
		if (hasDontAsk) requestSettings() else requestPms()
		if (!isHidden) getFragmentManager.beginTransaction().hide(this).commitAllowingStateLoss()
	}
	private[this] def requestSettings(): Unit = {
		state = sEXPECT_SETTINGS
		val intent = new Intent()
		intent.setAction(Settings.ACTION_APPLICATION_DETAILS_SETTINGS)
		val uri = Uri.fromParts("package", getActivity.getPackageName, null)
		intent.setData(uri)
		getActivity.startActivity(intent)
	}
	@TargetApi(23) private[this] def requestPms(): Unit = if (state == sNONE) {
		state = sEXPECT_RESULT
		val requestId = nextId
		manager.assignRequestId(requestId)
		val pms = manager.orderedPms.map(_.name)
		logD(s"PMS     REQ  :: ${pms.mkString(", ")}", logTag)
		requestPermissions(pms.toArray, requestId)
	}

	def onCancel(): Unit = {
		onRequestPermissionsResult(0, null, null)
	}
	override def onResume(): Unit = {
		if (state == sEXPECT_SETTINGS) onRequestPermissionsResult(0, null, null)
		super.onResume()
	}
	def onRestoreInstanceState(inState: Bundle): Unit = {
		state = inState.getInt(s"${getTag}_state")
	}
	override def onSaveInstanceState(outState: Bundle): Unit = {
		outState.putInt(s"${getTag}_state", state)
		super.onSaveInstanceState(outState)
	}
}






/* LISTENER */
trait PermissionDialogListener {
	def onOk(): Unit
	def onCancel(): Unit
}







/* DIALOG UI */
abstract class PermissionDialogHelper {
	implicit val context: Activity
	val listener: PermissionDialogListener
	val titleResId = 0
	val subtitleResId = 0
	val dontAskResId = 0
	val theme: ThemeInfo = null
	var titleV: TextView = null
	var subtitleV: TextView = null
	var okV: Button = null
	var cancelV: Button = null
	var listV: ListView = null
	private[this] var permissions: Seq[Permission] = Nil
	private[this] var adapter: BaseAdapter = null
	private[this] var first = true

	protected def onCreateView(): View
	protected def onCreateListItem(permiInfo: Permission): PermissionDialogListItemHelper

	def update(permissions: Seq[Permission]) = {
		this.permissions = permissions
		adapter.notifyDataSetChanged()
		first = false
	}
	final def createView(): View = {
		val v = onCreateView()
		if (titleV != null) titleV.setText(if (titleResId == 0) "Permissions required" else context.getString(titleResId))
		if (subtitleV != null) subtitleV.setText(if (subtitleResId == 0) "To operate properly app requires permissions" else context.getString(subtitleResId))
		if (okV != null) okV.setOnClickListener((v: View) => listener.onOk())
		if (cancelV != null) cancelV.setOnClickListener((v: View) => listener.onCancel())
		adapter = new BaseAdapter {
			override def getItemId(position: Int): Long = position
			override def getCount: Int = permissions.length
			override def getItem(position: Int): AnyRef = permissions(position)
			override def getView(position: Int, convertView: View, parent: ViewGroup): View = {
				val h = onCreateListItem(permissions(position))
				h.createView()
			}
		}
		listV.setAdapter(adapter)
		v
	}

	case class ThemeInfo(resId: Int, light: Boolean)
}


/* list item */
abstract class PermissionDialogListItemHelper {
	implicit val context: Activity
	val permission: Permission
	val colorCritical: Int
	val colorNormal: Int
	var iconV: ImageView = null
	var titleV: TextView = null
	var subtitleV: TextView = null

	protected def onCreateView(): View

	final def createView(): View = {
		val v = onCreateView()
		if (iconV != null) iconV.setImageResource(permission.icon)
		if (iconV != null) iconV.setColorFilter(if (permission.critical) colorCritical else colorNormal)
		if (titleV != null) titleV.setText(permission.label)
		if (titleV != null) titleV.setTextColor(if (permission.critical) colorCritical else colorNormal)
		if (subtitleV != null) subtitleV.setText(permission.description)
		v
	}
}


/* dialog impl */
class DefaultPermissionDialogHelper(val listener: PermissionDialogListener)(implicit val context: Activity) extends PermissionDialogHelper {
	import LayoutParams.{MATCH_PARENT ⇒ MATCH, WRAP_CONTENT ⇒ WRAP}
	override val theme = ThemeInfo(android.R.style.Theme_DeviceDefault_Dialog_Alert, false)
	lazy val light = theme.light
	lazy val firstTextColor = if (light) 0xFF555555 else 0xFFCCCCCC
	lazy val secondTextColor = if (light) 0xFF888888 else 0xFFAAAAAA

	override protected def onCreateView(): View = {
		val dp10 = ModuleApp().dp2pix(10)
		val dp16 = ModuleApp().dp2pix(16)
		//
		val root = new LinearLayout(context)
		root.setOrientation(LinearLayout.VERTICAL)
		root.setLayoutParams(new LayoutParams(MATCH, WRAP))
		//
		titleV = new TextView(context)
		titleV.setId(1)
		titleV.setTextColor(firstTextColor)
		titleV.setTextSize(TypedValue.COMPLEX_UNIT_SP, 20)
		titleV.setPadding(dp10, dp10, dp10, dp10)
		var params = new LinearLayout.LayoutParams(WRAP, WRAP)
		params.gravity = Gravity.CENTER
		titleV.setLayoutParams(params)
		root.addView(titleV)
		//
		subtitleV = new TextView(context)
		subtitleV.setId(2)
		subtitleV.setTextColor(secondTextColor)
		subtitleV.setTextSize(TypedValue.COMPLEX_UNIT_SP, 16)
		subtitleV.setPadding(dp16, 0, dp16, 0)
		subtitleV.setLayoutParams(new LayoutParams(WRAP, WRAP))
		root.addView(subtitleV)
		//
		listV = new ListView(context)
		listV.setId(3)
		listV.setLayoutParams(new LinearLayout.LayoutParams(WRAP, WRAP, 1f))
		listV.setPadding(dp16, dp10, dp16, dp10)
		root.addView(listV)
		//
		val subroot = new LinearLayout(context)
		subroot.setOrientation(LinearLayout.HORIZONTAL)
		subroot.setPadding(dp10, 0, dp10, 0)
		params = new LinearLayout.LayoutParams(WRAP, WRAP)
		params.gravity = Gravity.RIGHT
		subroot.setLayoutParams(params)
		root.addView(subroot)
		//
		cancelV = new Button(context) //, null, android.R.attr.buttonBarNeutralButtonStyle)
		cancelV.setId(5)
		cancelV.setText(android.R.string.cancel)
		cancelV.setBackgroundColor(0x00FFFFFF)
		cancelV.setTextColor(firstTextColor)
		cancelV.setTextSize(TypedValue.COMPLEX_UNIT_SP, 14)
		cancelV.setLayoutParams(new LayoutParams(WRAP, WRAP))
		subroot.addView(cancelV)
		//
		okV = new Button(context) //, null, android.R.attr.buttonBarPositiveButtonStyle)
		okV.setId(6)
		okV.setText(android.R.string.ok)
		okV.setBackgroundColor(0x00FFFFFF)
		okV.setTextColor(firstTextColor)
		okV.setTextSize(TypedValue.COMPLEX_UNIT_SP, 14)
		okV.setLayoutParams(new LayoutParams(WRAP, WRAP))
		subroot.addView(okV)
		//
		root
	}
	override protected def onCreateListItem(permission: Permission) = {
		new DefaultPermissionDialogListItemHelper(permission)
	}


	/* list item impl */
	class DefaultPermissionDialogListItemHelper(val permission: Permission)(implicit val context: Activity) extends PermissionDialogListItemHelper {
		override val colorCritical: Int = 0xFFFE8C79
		override val colorNormal: Int = 0xFF6787F0
		override protected def onCreateView(): View = {
			val dp10 = ModuleApp().dp2pix(10)
			//
			val root = new LinearLayout(context)
			root.setPadding(0, 0, 0, dp10)
			root.setOrientation(LinearLayout.VERTICAL)
			root.setLayoutParams(new LayoutParams(MATCH, WRAP))
			//
			val s1Lay = new LinearLayout(context)
			s1Lay.setOrientation(LinearLayout.HORIZONTAL)
			s1Lay.setLayoutParams(new LayoutParams(MATCH, WRAP))
			root.addView(s1Lay)
			//
			iconV = new ImageView(context)
			iconV.setId(1)
			iconV.setPadding(dp10, 0, dp10, 0)
			iconV.setLayoutParams(new LayoutParams(WRAP, WRAP))
			s1Lay.addView(iconV)
			//
			titleV = new TextView(context)
			titleV.setId(2)
			titleV.setTextSize(TypedValue.COMPLEX_UNIT_SP, 18)
			titleV.setGravity(Gravity.CENTER_VERTICAL)
			titleV.setLayoutParams(new LayoutParams(WRAP, WRAP))
			s1Lay.addView(titleV)
			//
			subtitleV = new TextView(context)
			subtitleV.setId(3)
			subtitleV.setTextColor(secondTextColor)
			subtitleV.setTextSize(TypedValue.COMPLEX_UNIT_SP, 14)
			subtitleV.setLayoutParams(new LayoutParams(MATCH, WRAP))
			root.addView(subtitleV)
			//
			root
		}
	}
}





/* REQUEST */
private[app] class PermissionRequest(promise: Promise[Unit], permissions: ArrayBuffer[Permission]) {
	/** @return true if request is done */
	def isCompleted(p: Permission, granted: Boolean): Boolean = permissions.indexOf(p) match {
		case -1 ⇒ false
		case ix ⇒ val p0 = permissions(ix)
			if (!granted && p0.critical) {
				logD(s"PMS             request completed:  FAILED", logTag)
				promise.tryFailure(new ModulePermissionException(p.name))
				true
			}
			else {
				permissions.remove(ix)
				if (permissions.isEmpty) promise.trySuccess()
				if (permissions.isEmpty) logD(s"PMS             request completed:  OK", logTag)
				permissions.isEmpty
			}
	}

	def onCompleted(): Unit = permissions.exists(_.critical) match {
		case false ⇒ promise.trySuccess()
		case _ ⇒ promise.tryFailure(new ModulePermissionException(permissions.filter(_.critical).map(_.name).mkString))
	}
}




/* MODULE PERMISSION */
case class Permission(name: String, var critical: Boolean = true, var rationaleResId: Int = 0) {
	private[app] var requestId = 0
	private[app] var dontAsk = false
	private[app] lazy val packmgr = ModuleApp().getPackageManager
	private[app] lazy val info: PermissionInfo = packmgr.getPermissionInfo(name, GET_META_DATA)
	private[this] lazy val grpInfo: PermissionGroupInfo = if (info.group != null) packmgr.getPermissionGroupInfo(info.group, GET_META_DATA) else null

	protected[app] def icon: Int = if (grpInfo == null) info.icon else grpInfo.icon
	protected[app] def label: CharSequence = if (grpInfo == null) info.loadLabel(packmgr) else grpInfo.loadLabel(packmgr)
	protected[app] def description: CharSequence = {
		if (rationaleResId > 0) ModuleApp().getString(rationaleResId)
		else {if (grpInfo == null) info.loadDescription(packmgr) else grpInfo.loadDescription(packmgr)}
	}
	private[app] def copy: Permission = Permission(name, critical)
	override def equals(o: Any): Boolean = o match {
		case p: Permission => name == p.name
		case p: String => name == p
		case _ => false
	}
}




/* EXCEPTION */
class ModulePermissionException(permission: String) extends ModuleException {
	override def getMessage: String = s"Module has no permission $permission"
}
