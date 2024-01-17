import org.scalajs.linker.interface.ModuleSplitStyle

val zioVersion = "2.0.21"
val javaTimeVersion = "2.5.0"

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"

val commonSettings = Seq(
  scalaVersion := "3.3.1",
  scalacOptions ++= Seq(
    "-Wunused:all",
    "-feature",
    "-explain",
    "-language:implicitConversions"
  ),
  semanticdbEnabled := true,
  semanticdbVersion := scalafixSemanticdb.revision,
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
    //name := "foo",
    //version := "0.1-SNAPSHOT",
  ).
  jvmSettings(
    // Add JVM-specific settings here
  ).
  jsSettings(
    // Add JS-specific settings here
    //scalaJSUseMainModuleInitializer := true,
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
          ModuleSplitStyle.SmallModulesFor(List("draw")))
    },

    /* Depend on the scalajs-dom library.
     * It provides static types for the browser DOM APIs.
     */
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

    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

  )
