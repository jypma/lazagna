import org.scalajs.linker.interface.ModuleSplitStyle

val zioVersion = "2.0.21"
val javaTimeVersion = "2.5.0"

val commonSettings = Seq(
  scalaVersion := "3.3.1",
  scalacOptions ++= Seq(
    "-Wunused:imports",
    "-feature",
    "-explaintypes",
    "-language:implicitConversions"
  ),
  semanticdbEnabled := true,
  semanticdbVersion := scalafixSemanticdb.revision,
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
  Compile / PB.targets := Seq(
    scalapb.gen() -> (Compile / sourceManaged).value / "protos"
  ),

  // (optional) If you need scalapb/scalapb.proto or anything from
  // google/protobuf/*.proto
  libraryDependencies ++= Seq(
    "com.thesamet.scalapb" %%% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf"
  ))

lazy val data = crossProject(JSPlatform, JVMPlatform).in(file("draw-data"))
  .settings(commonSettings)
  .settings(
    Compile / PB.protoSources := Seq(baseDirectory.value / ".." / "shared" / "src" / "main" / "protobuf"),
  ).
  jvmSettings(
    // Add JVM-specific settings here
  ).
  jsSettings(
    // Add JS-specific settings here
  )

lazy val client = project.in(file("draw-client"))
  .enablePlugins(ScalaJSPlugin) // Enable the Scala.js plugin in this project
  .settings(commonSettings)
  .dependsOn(data.js)
  .settings(
    // Tell Scala.js that this is an application with a main method
    scalaJSUseMainModuleInitializer := true,

    /* Configure Scala.js to emit modules in the optimal way to
     * connect to Vite's incremental reload.
     * - emit ECMAScript modules
     * - emit as many small modules as possible for classes in the "draw" package
     * - emit as few (large) modules as possible for all other classes
     *   (in particular, for the standard library)
     */
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(List("draw", "zio.lazagna")))
    },

    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.4.0",
      "dev.zio" %%% "zio" % zioVersion,
      "dev.zio" %%% "zio-streams" % zioVersion,
      "io.github.cquiroz" %%% "scala-java-time" % javaTimeVersion,
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % javaTimeVersion,

      "dev.zio" %%% "zio-test"          % zioVersion % Test,
      "dev.zio" %%% "zio-test-sbt"      % zioVersion % Test,
      "dev.zio" %%% "zio-test-magnolia" % zioVersion % Test
    ),
  )

val log4jVersion = "2.23.0"
lazy val server = project.in(file("draw-server"))
  .settings(commonSettings)
  .dependsOn(data.jvm)
  .settings(
    resolvers += "Sonatype OSS Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots",
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio" % zioVersion,
      "dev.zio" %%% "zio-streams" % zioVersion,
      "dev.zio" %%% "zio-test"          % zioVersion % Test,
      "dev.zio" %%% "zio-test-sbt"      % zioVersion % Test,
      "dev.zio" %%% "zio-test-magnolia" % zioVersion % Test,
      "dev.zio" %% "zio-http" % "3.0.0-RC4",

      "io.github.palanga" %% "zio-cassandra" % "0.10.0+48-99339c35-SNAPSHOT",
      "org.apache.logging.log4j" % "log4j-api" % log4jVersion,
      "org.apache.logging.log4j" % "log4j-core" % log4jVersion,
      "org.apache.logging.log4j" % "log4j-slf4j-impl" % log4jVersion
    ),
  )
