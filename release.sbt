// This branch prepares only the JVM dependency artifacts listed in docs/dependency-release.md.
// Publishing is an explicit local staging operation; Central upload is a separate step.
ThisBuild / isSnapshot := false
ThisBuild / version := "0.1.0-RC1"
ThisBuild / homepage := Some(url("https://github.com/canardlapin/graph4s"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/canardlapin/graph4s"),
    "scm:git:https://github.com/canardlapin/graph4s.git"
  )
)
ThisBuild / publishMavenStyle := true
ThisBuild / publishTo := localStaging.value

addCommandAlias("stageDependencyRelease", ";coreJVM/publish;indexedJVM/publish;dataJVM/publish")

ThisBuild / tlCiReleaseBranches := Seq.empty
ThisBuild / tlCiReleaseTags := false
