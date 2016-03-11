package just4fun.android.demo

import android.app.Activity
import just4fun.android.core.app._

class App extends ModuleApp {

	override protected[this] def onAppCreate(): Unit = {

	}
	override protected[this] def onAppStart(): Unit = {
		//startModule()
	}
	override protected[this] def onAppStop(): Unit = {

	}
	override def permissionsDialog(listener: PermissionDialogListener)(implicit context: Activity): PermissionDialogHelper = {
		new DefaultPermissionDialogHelper(listener) {
			override val titleResId = R.string.permDialogTitle
			override val subtitleResId = R.string.permDialogMessage
			override val dontAskResId = R.string.permDialogDontAsk
			override val theme = ThemeInfo(android.R.style.Theme_DeviceDefault_Light_Dialog_Alert, true)
//			protected override def onCreateView(): View = {
//				val root = context.getLayoutInflater.inflate(R.layout.pms, null, false) // TODO switch to runtime layout
//				titleV = root.findViewById(R.id.title).asInstanceOf[TextView]
//				subtitleV = root.findViewById(R.id.info).asInstanceOf[TextView]
//				cancelV = root.findViewById(R.id.buttonNeg).asInstanceOf[Button]
//				okV = root.findViewById(R.id.buttonPos).asInstanceOf[Button]
//				listV = root.findViewById(R.id.list).asInstanceOf[ListView]
//				dontAskV = root.findViewById(R.id.notAgain).asInstanceOf[CheckBox]
//				root
//			}
//			protected override def onCreateListItem(permission: PermissionExtra): DefaultPermissionDialogListItemHelper = {
//				new ListItemHlpr(permission)
//			}
//			class ListItemHlpr(permission: PermissionExtra) extends DefaultPermissionDialogListItemHelper(permission) {
//				protected override def onCreateView(): View = {
//					val v = context.getLayoutInflater.inflate(R.layout.pms_item, null)
//					iconV = v.findViewById(R.id.pm_icon).asInstanceOf[ImageView]
//					titleV = v.findViewById(R.id.pm_title).asInstanceOf[TextView]
//					subtitleV = v.findViewById(R.id.pm_info).asInstanceOf[TextView]
//					v
//				}
//			}
		}
	}

}
