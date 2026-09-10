import org.typelevel.sbt.gha.JavaSpec

val Scala3           = "3.7.4"
val catsV            = "2.13.0"
val catsCollectionsV = "0.9.10"
val munitV           = "1.3.4"
val munitCheckV      = "1.3.0"
val disciplineMunitV = "2.0.0"
val scalaCheckV      = "1.19.0"

// Gale is not yet published. Ordinary builds consume an immutable source
// revision; coordinated development can opt into a sibling checkout.
lazy val galeRevision = "83cac90a678d1b8a31c590e0c1b8fc8bf3427161"
lazy val galeBuild =
  sys.props
    .get("graph4s.gale.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/gale.git#$galeRevision"))
lazy val galeCoreJVM = ProjectRef(galeBuild, "coreJVM")
lazy val galeCoreJS  = ProjectRef(galeBuild, "coreJS")

ThisBuild / tlBaseVersion    := "0.1"
ThisBuild / organization     := "io.github.canardlapin"
ThisBuild / organizationName := "Bradley Buchsbaum"
ThisBuild / startYear        := Some(2026)
ThisBuild / licenses         := Seq(License.Apache2)
ThisBuild / developers       := List(
  tlGitHubDev("canardlapin", "Bradley Buchsbaum")
)

ThisBuild / scalaVersion       := Scala3
ThisBuild / crossScalaVersions := Seq(Scala3)
ThisBuild / tlJdkRelease       := Some(11)
ThisBuild / githubWorkflowJavaVersions := Seq(
  JavaSpec.temurin("17"),
  JavaSpec.temurin("21")
)

lazy val commonSettings = Seq(
  // Scala 3.7/Scala.js emits this scala.caps namespace warning even though
  // dependency resolution selects one Scala standard library.
  scalacOptions +=
    "-Wconf:msg=package scala contains object and package with same name.*caps:silent",
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit"            % munitV      % Test,
    "org.scalameta" %%% "munit-scalacheck" % munitCheckV % Test
  ),
  Test / parallelExecution := false
)

lazy val root = tlCrossRootProject
  .aggregate(core, expr, indexed, data, algorithms, gale, laws)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "graph4s-core",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core"             % catsV,
      "org.typelevel" %%% "cats-collections-core" % catsCollectionsV
    )
  )

lazy val expr = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("expr"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(name := "graph4s-expr")

lazy val indexed = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("indexed"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(name := "graph4s-indexed")

lazy val data = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("data"))
  .dependsOn(core, indexed)
  .settings(commonSettings)
  .settings(name := "graph4s-data")

lazy val algorithms = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("algorithms"))
  .dependsOn(core, indexed, data)
  .settings(commonSettings)
  .settings(name := "graph4s-algorithms")

lazy val gale = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("gale"))
  .dependsOn(indexed, data, algorithms % "test->compile")
  .settings(commonSettings)
  .settings(
    name := "graph4s-gale"
  )
  .jvmConfigure(_.dependsOn(galeCoreJVM))
  .jsConfigure(_.dependsOn(galeCoreJS))

lazy val laws = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("laws"))
  .dependsOn(core, expr, indexed, data, algorithms)
  .settings(commonSettings)
  .settings(
    name := "graph4s-laws",
    libraryDependencies ++= Seq(
      "org.scalameta"  %%% "munit"            % munitV,
      "org.typelevel"  %%% "discipline-munit" % disciplineMunitV,
      "org.typelevel"  %%% "cats-laws"        % catsV,
      "org.scalacheck" %%% "scalacheck"       % scalaCheckV
    )
  )

addCommandAlias(
  "compileAll",
  (
    List("core", "expr", "indexed", "data", "algorithms", "laws")
      .flatMap(module =>
        List("JVM", "JS", "Native").map(platform => s"$module$platform/compile")
      ) ++
      List("galeJVM/compile", "galeJS/compile")
  ).mkString(";", ";", "")
)

addCommandAlias(
  "testAll",
  (
    List("core", "expr", "indexed", "data", "algorithms", "laws")
      .flatMap(module => List("JVM", "JS", "Native").map(platform => s"$module$platform/test")) ++
      List("galeJVM/test", "galeJS/test")
  ).mkString(";", ";", "")
)
