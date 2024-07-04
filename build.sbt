import org.scalajs.linker.interface.ModuleSplitStyle
import xerial.sbt.Sonatype._

val zioVersion = "2.0.21"
val javaTimeVersion = "2.5.0"

val commonSettings = Seq(
  ThisBuild / sonatypeCredentialHost := sonatypeCentralHost,
  sonatypeProfileName := "io.github.jypma",
  publishMavenStyle := true,
  licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  sonatypeProjectHosting := Some(GitHubHosting("jypma", "lazagna", "jan@ypmania.net")),

  ThisBuild / organization := "io.github.jypma",
  ThisBuild / organizationName := "Jan Ypma",
  ThisBuild / organizationHomepage := Some(url("http://github.com/jypma")),
  ThisBuild / versionScheme := Some("early-semver"),
  ThisBuild / version := "0.9.1",
  scalaVersion := "3.3.3",
  scalacOptions ++= Seq(
    "-Wunused:imports",
    "-feature",
    "-deprecation",
    "-language:implicitConversions"
  ),
  semanticdbEnabled := true,
  semanticdbVersion := scalafixSemanticdb.revision,
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),

  // (optional) If you need scalapb/scalapb.proto or anything from
  // google/protobuf/*.proto
  libraryDependencies ++= Seq(
    "dev.zio" %%% "zio" % zioVersion,
    "dev.zio" %%% "zio-streams" % zioVersion,
    "dev.zio" %%% "zio-test"          % zioVersion % Test,
    "dev.zio" %%% "zio-test-sbt"      % zioVersion % Test,
    "dev.zio" %%% "zio-test-magnolia" % zioVersion % Test
  ))

lazy val core = crossProject(JSPlatform, JVMPlatform).in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "lazagna-core",
    publishTo := sonatypePublishToBundle.value
  )
  .jsSettings(
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
      "io.github.cquiroz" %%% "scala-java-time" % javaTimeVersion,
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % javaTimeVersion,
    ),
  )
