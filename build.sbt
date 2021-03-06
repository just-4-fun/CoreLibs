lazy val corelibs = (project in file("."))
  .aggregate(utils, debugutils, modules, schemify, android, androidtest)
//  .settings(commonSettings: _*)
//  .settings(androidCommands)
  .settings(
	version := "1.0-SNAPSHOT"
	, scalaVersion := "2.11.7"
// include the macro classes and resources in the main jar
//	  , mappings in(Compile, packageBin) ++= mappings.in(modules, Compile, packageBin).value
//	  , mappings in(Compile, packageBin) ++= mappings.in(shemify, Compile, packageBin).value
//	  // include the macro sources in the main source jar
//	  , mappings in(Compile, packageSrc) ++= mappings.in(modules, Compile, packageSrc).value
//	  , mappings in(Compile, packageSrc) ++= mappings.in(shemify, Compile, packageSrc).value
//	  , libraryDependencies += scalaReflect.value
)

lazy val androidtest = (project in file("androidtest"))
  .dependsOn(utils, debugutils, modules, schemify)
  .androidBuildWith(android)
  .settings(commonSettings: _*)
  .settings(androidSettings: _*)
  .settings(proguardSettings: _*)
  .settings(testSettings: _*)
  .settings(
	  version := "1.0-SNAPSHOT"
//	  , publish := {}
//	  , publishLocal := {}
	  , useProguard in Android := true
	  , useProguardInDebug in Android := true
	  , proguardScala in Android := true
  )

lazy val android = (project in file("android"))
  .dependsOn(utils, debugutils, modules, schemify)
  .settings(commonSettings: _*)
  .settings(androidSettings: _*)
  .settings(proguardSettings: _*)
  .settings(testSettings: _*)
  .settings(
	  version := "1.0-SNAPSHOT"
//	  , publish := {}
//	  , publishLocal := {}
//	  , androidBuildAar// helps with "duplicate core.R" ...
	  , androidBuild
	  , libraryProject := true
//	  , transitiveAndroidLibs := false
  )

lazy val modules = (project in file("modules"))
  .dependsOn(utils, debugutils)
  .settings(commonSettings: _*)
  .settings(
	  version := "1.0-SNAPSHOT"
//	  , publish := {}
//	  , publishLocal := {}
	  , exportJars := true
	  , libraryDependencies += scalaReflect.value
  )

lazy val utils = (project in file("utils"))
  .dependsOn(debugutils, schemify)
  .settings(commonSettings: _*)
  .settings(
	  version := "1.0-SNAPSHOT"
//	  , publish := {}
//	  , publishLocal := {}
	  , exportJars := true
	  , libraryDependencies += scalaReflect.value
  )

lazy val schemify = (project in file("schemify"))
  .dependsOn(debugutils)
//  .dependsOn(corelibs % "test->compile")
  .settings(commonSettings: _*)
  .settings(testSettings: _*)
  .settings(
	  version := "1.0-SNAPSHOT"
//	  , publish := {}
//	  , publishLocal := {}
	  , exportJars := true
	  , libraryDependencies += scalaReflect.value
	  , libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % "2.5.1"
	  , addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )

lazy val debugutils = (project in file("debugutils"))
  .settings(commonSettings: _*)
  .settings(
	  version := "1.0-SNAPSHOT"
//	  , publish := {}
//	  , publishLocal := {}
	  , exportJars := true
	  , libraryDependencies += scalaReflect.value
  )

lazy val commonSettings = Seq(
	scalaVersion := "2.11.7"
	, organization := "just4fun"
	, licenses := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php"))
	, homepage := Some(url("https://github.com/just-4-fun"))
	, scalacOptions += "-Xexperimental"
	, scalacOptions in Compile += "-feature"
	, javacOptions ++= Seq("-source", "1.7", "-target", "1.7")//helps with dex bad class file magic error
)

lazy val androidSettings = Seq(
	minSdkVersion := "14"
	, targetSdkVersion := "23"
	, platformTarget := "android-23"
	, typedResources := false
)

lazy val testSettings = Seq(
	publishArtifact in Test := false
	, libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
)

lazy val proguardSettings = Seq(
	proguardOptions ++= Seq(
		"-keepattributes Signature"
		, "-dontwarn scala.collection.**"
		, "-keepattributes InnerClasses"//?
//		,"-keep class scala.DelayedInit"
	)
)


lazy val scalaReflect = Def.setting {"org.scala-lang" % "scala-reflect" % scalaVersion.value}
