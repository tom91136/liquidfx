import sbt.librarymanagement.CrossVersion

enablePlugins(JavaAppPackaging)
enablePlugins(JmhPlugin)

val osName: SettingKey[String] = SettingKey[String]("osName")

osName := (System.getProperty("os.name") match {
	case name if name.startsWith("Linux")   => "linux"
	case name if name.startsWith("Mac")     => "mac"
	case name if name.startsWith("Windows") => "win"
	case _                                  => throw new Exception("Unknown platform!")
})

lazy val javaFxVersion = "12-ea+12"

lazy val commonSettings = Seq(
	organization := "net.kurobako",
	scalaVersion := "2.12.8",
	scalacOptions ++= Seq(
		"-target:jvm-1.8",
		"-encoding", "UTF-8",
		"-unchecked",
		"-deprecation",
		"-explaintypes",
		"-feature",
		"-Xfuture",

		"-language:existentials",
		"-language:experimental.macros",
		"-language:higherKinds",
		"-language:postfixOps",
		"-language:implicitConversions",

		"-Xlint:adapted-args",
		"-Xlint:by-name-right-associative",
		"-Xlint:constant",
		"-Xlint:delayedinit-select",
		"-Xlint:doc-detached",
		"-Xlint:inaccessible",
		"-Xlint:infer-any",
		"-Xlint:missing-interpolator",
		"-Xlint:nullary-override",
		"-Xlint:nullary-unit",
		"-Xlint:option-implicit",
		//		"-Xlint:package-object-classes", // TODO enable after project works
		"-Xlint:poly-implicit-overload",
		"-Xlint:private-shadow",
		"-Xlint:stars-align",
		"-Xlint:type-parameter-shadow",
		"-Xlint:unsound-match",

		"-Yno-adapted-args",
		"-Ywarn-dead-code",
		"-Ywarn-extra-implicit",
		"-Ywarn-inaccessible",
		"-Ywarn-infer-any",
		"-Ywarn-nullary-override",
		"-Ywarn-nullary-unit",
		"-Ywarn-numeric-widen",
		"-Ywarn-unused:implicits",
		//		"-Ywarn-unused:imports",
		"-Ywarn-unused:locals",
		"-Ywarn-unused:params",
		"-Ywarn-unused:patvars",
		"-Ywarn-unused:privates",
		"-Ywarn-value-discard",
		"-Ypartial-unification",

		// TODO enable to Scala 2.12.5
		"-Ybackend-parallelism", "4",
		//				"-Ycache-plugin-class-loader:last-modified",
		//				"-Ycache-macro-class-loader:last-modified",

		// XXX enable for macro debug
		//		"-Ymacro-debug-lite",
		//			"-Xlog-implicits",
		"-P:bm4:no-filtering:y",
		"-P:bm4:no-tupling:y",
		"-P:bm4:no-map-id:y",
	),
	javacOptions ++= Seq(
		"-target", "11",
		"-source", "11",
		"-Xlint:all"),
	addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full),
	addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9"),
	addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.2.4"),
	resolvers += Resolver.sonatypeRepo("releases"),
)

lazy val liquidfx = project.in(file(".")).settings(commonSettings)

lazy val app = project.in(file("app")).settings(
	commonSettings,
	name := "app",
	version := "0.1.0-SNAPSHOT",
	mainClass in Compile := Some("net.kurobako.fxgrep.CLIApp"),
	mainClass in assembly := Some("net.kurobako.fxgrep.CLIApp"),
	assemblyMergeStrategy in assembly := {
		case "module-info.java" |
			 "module-info.class" => MergeStrategy.discard
		case x                   => (assemblyMergeStrategy in assembly).value(x)
	},
	jdkPackagerType := "installer",
	libraryDependencies ++= Seq(

		"com.lihaoyi" %% "upickle" % "0.7.1",

		//		"com.h2database" % "h2" % "1.4.197",
		//		"io.getquill" %% "quill-jdbc" % "2.6.0",

		"com.google.guava" % "guava" % "27.0.1-jre",
		"com.github.oshi" % "oshi-core" % "3.13.0",


		"com.github.pathikrit" %% "better-files" % "3.7.0",

		"com.chuusai" %% "shapeless" % "2.3.3",
		"org.typelevel" %% "cats-core" % "1.6.0",
		"org.typelevel" %% "cats-effect" % "1.2.0",
		"org.typelevel" %% "kittens" % "1.2.0",
		"io.estatico" %% "newtype" % "0.4.2",

		"io.chrisdavenport" %% "log4cats-slf4j" % "0.3.0-M2",

		"ch.qos.logback" % "logback-classic" % "1.2.3",

		"com.beachape" %% "enumeratum" % "1.5.13",
		"com.github.mpilquist" %% "simulacrum" % "0.15.0",

		"org.scalafx" %% "scalafx" % "11-R16",
		"org.controlsfx" % "controlsfx" % "9.0.0",

		"org.fxmisc.flowless" % "flowless" % "0.6.1",
		"org.fxmisc.easybind" % "easybind" % "1.0.3",

		"org.scalatest" %% "scalatest" % "3.0.5" % Test,
		"org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
	) ++ Seq("controls", "graphics", "fxml", "media", "web", "base").map {
		module => "org.openjfx" % s"javafx-$module" % javaFxVersion classifier (osName in liquidfx).value
	}
)

lazy val benchmark = project.in(file("benchmark")).settings(
	commonSettings,
	name := "benchmark",
	libraryDependencies ++= Seq(

	)
).dependsOn(app).enablePlugins(JmhPlugin)
