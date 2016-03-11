package just4fun.android.demo.permissions

import scala.util.{Failure, Success}
import android.content.Intent
import android.os.Bundle
import android.view.View
import android.view.View.OnClickListener
import android.widget.Button
import just4fun.android.core.app.{AndroidModule, ActivityModule, ModuleActivity, Permission}
import just4fun.android.demo.R
import just4fun.core.debug.DebugUtils._
import android.Manifest.permission._

/**
  * <test 0 declared - 0 pms requested => Ok />
  * <test 0 declared - 1 requested statically, critical, dangerous => Ok />
  * <test 0 declared - 1 requested statically, optional, dangerous => Ok />
  * <test 0 declared - 1 requested statically, critical, normal => Ok />
  * <test 0 declared - 1 requested statically, optional, normal => Ok />
  * <test 0 declared - 1 requested dynamically => Ok />
  * <test 0 declared - 1 requested dynamically => Ok />
  * <test 1 declared - 2 requested statically, critical => Ok />
  * <test 1 declared - 2 requested statically, optional => Ok />
  * <test 2 static, critical, no rationale, 1 group => Ok />
  * <test 2 static, critical, fake rationale, 1 group => Ok. Fails with resource not found />
  * <test 2 static, critical, with rationale, 1 group, Allow => Ok />
  * <test 2 static, critical, with rationale, 1 group, Deny, Dont ask => Ok />
  * <test 2 static, optional+critical, 1 group, Allow => Ok />
  * <test 2 static, optional+critical, 1 group, Deny, Dont ask, Allow => Ok />
  * <test 4 critical, 2 + Null groups, Allow Deny Mix => Ok />
  * <test 4 optional, 2 + Null groups, Cancel Deny Allow => Ok />
  * <test 4 mix, 2 + Null groups, Cancel Deny Allow => Ok />
  * <test 8 mix, 2 + Null groups, Cancel Deny Allow => Ok />
  * <test DontAsk, optional, no rid  => Ok />
  * <test DontAsk, optional, w rid  => Ok />
  * <test DontAsk, mix, no rid  => Ok />
  * <test DontAsk, mix, w rid  => Ok />
  * <test DontAsk, critical, no rid  => Ok />
  * <test DontAsk, critical, w rid  => Ok />
  * <test Rotation  => Ok />
  * <test New Activity on top of dialog (Home - Launch)  => Ok />
  * <test Module1+ Module2 + dynamic, with and wo rid => Ok />
  * <test Module1+ Module2 + dynamic,  => Ok />
  * <test Sequence of dynamic calls => Ok />
  * <test Deny via settings while app is running => Restarts app. Ok />
  * <test Parallel => Ok />
  */


class MainActivity extends ModuleActivity[Module1] {
	override def onCreate(savedInstanceState: Bundle) {
		super.onCreate(savedInstanceState)
		setContentView(R.layout.main)
		setTitle("TestPermissions")
		val b1 = findViewById(R.id.button1).asInstanceOf[Button]
		b1.setOnClickListener((v: View) => do1())
		val b2 = findViewById(R.id.button2).asInstanceOf[Button]
		b2.setOnClickListener((v: View) =>do2())
		val b3 = findViewById(R.id.button3).asInstanceOf[Button]
		b3.setOnClickListener((v: View) => do3())
		val b4 = findViewById(R.id.button4).asInstanceOf[Button]
		b4.setOnClickListener((v: View) => do4())
	}
	def do1(): Unit = {
		startActivity(new Intent(this, classOf[just4fun.android.demo.MainActivity]))
	}
	def do2(): Unit = {}
	def do3(): Unit = {}
	def do4(): Unit = {}
}


class Module1 extends ActivityModule[MainActivity] {
	val rid0 = 0
	val rid = R.string.pm_write_contacts

	bind[Module2]

	override protected[this] def permissions: Seq[Permission] = {
//		ModulePermission(critical = true, rationaleResId = rid0, name = INTERNET) ::
//		  ModulePermission(critical = true, rationaleResId = rid0, name = ACCESS_NETWORK_STATE) ::
//		  ModulePermission(critical = true, rationaleResId = rid0, name = FLASHLIGHT) ::
		  Permission(critical = false, rationaleResId = rid, name = CAMERA) ::
//		  ModulePermission(critical = true, rationaleResId = rid0, name = BODY_SENSORS) ::
		  Permission(critical = false, rationaleResId = rid0, name = READ_CONTACTS) ::
//		  ModulePermission(critical = true, rationaleResId = rid0, name = WRITE_CONTACTS) ::
//		  ModulePermission(critical = true, rationaleResId = rid0, name = READ_PHONE_STATE) ::
//		  ModulePermission(critical = true, rationaleResId = rid0, name = CALL_PHONE) ::
//		  ModulePermission(critical = true, rationaleResId = rid0, name = RECEIVE_SMS) ::
//		  ModulePermission(critical = true, rationaleResId = rid0, name = SEND_SMS) ::
//		  ModulePermission(critical = true, rationaleResId = rid0, name = ACCESS_COARSE_LOCATION) ::
//		  ModulePermission(critical = true, rationaleResId = rid0, name = ACCESS_FINE_LOCATION) ::
//		  ModulePermission(critical = true, rationaleResId = rid0, name = READ_CALENDAR) ::
//		  ModulePermission(critical = true, rationaleResId = rid0, name = WRITE_CALENDAR) ::
		  Nil
	}
	
	override protected[this] def onActivityCreated(): Unit = {
//		inParallel()
		inSequence()
	}
	protected[this] def inSequence(): Unit = {
		import android.Manifest.permission._
		requestDynamic(READ_CONTACTS)
		asyncContext.execute("pmsRq", 100, () ⇒ requestDynamic(WRITE_CONTACTS))
		asyncContext.execute("pmsRq", 500, () ⇒ requestDynamic(READ_PHONE_STATE))
		asyncContext.execute("pmsRq", 1000, () ⇒ requestDynamic(RECEIVE_SMS, true, rid))
		asyncContext.execute("pmsRq", 15000, () ⇒ requestDynamic(READ_CALENDAR, true, rid))
	}
	protected[this] def inParallel(): Unit = {
		import android.Manifest.permission._
		def request(p: String, rid: Int = 0): Unit ={
			new Thread(() ⇒ requestDynamic(p, true, rid)).start()
		}
		request(READ_CONTACTS)
		request(WRITE_CONTACTS)
		request(READ_PHONE_STATE)
		request(RECEIVE_SMS)
		request(READ_CALENDAR, rid)
	}

	def requestDynamic(p: String, critical: Boolean = true, rid: Int = 0): Unit = {
		context.requestPermissions(Permission(p, true, rid)).onComplete {
			case Success(res) => logD(s"PMS             req dynamically [$p]:  OK;  granted? ${context.hasPermission(p)}")
			case Failure(e) => logW(s"PMS             req dynamically [$p]:  failed with: $e")
		}
	}
}


class Module2 extends AndroidModule {
	override protected[this] def permissions: Seq[Permission] = {
		val rid0 = 0
		val rid = R.string.pm_write_contacts
		  Permission(critical = true, rationaleResId = rid0, name = BODY_SENSORS) ::
		  Nil
	}
}