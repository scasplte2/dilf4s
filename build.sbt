import Dependencies._
import sbt._
import sbt.Keys._

ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "xyz.kd5ujc"
ThisBuild / scalaVersion := "2.13.11"
ThisBuild / evictionErrorLevel := Level.Warn
ThisBuild / scalafixDependencies += Libraries.organizeImports
ThisBuild / scalafixScalaBinaryVersion := "2.13"

ThisBuild / assemblyMergeStrategy := {
  case "logback.xml" => MergeStrategy.first
  case x if x.contains("io.netty.versions.properties") => MergeStrategy.discard
  case PathList("xyz", "kd5ujc", "buildinfo", xs @ _*) => MergeStrategy.first
  case PathList(xs@_*) if xs.last == "module-info.class" => MergeStrategy.first
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}

lazy val commonSettings = Seq(
  scalacOptions ++= List("-Ymacro-annotations", "-Yrangepos", "-Wconf:cat=unused:info", "-language:reflectiveCalls"),
  resolvers += Resolver.mavenLocal,
  libraryDependencies ++= Seq(
    CompilerPlugin.kindProjector,
    CompilerPlugin.betterMonadicFor,
    CompilerPlugin.semanticDB,
    Libraries.cats,
    Libraries.catsEffect,
    Libraries.scrypto,
    Libraries.levelDb,
    Libraries.levelDbJni,
    Libraries.circeCore,
    Libraries.circeGeneric,
    Libraries.circeParser,
    Libraries.log4cats,
    Libraries.derevoCore,
    Libraries.derevoCirce
  )
)

lazy val commonTestSettings = Seq(
  scalacOptions ++= commonScalacOptions,
  semanticdbEnabled := true,
  semanticdbVersion := scalafixSemanticdb.revision,
  testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
  libraryDependencies ++= Seq(
    Libraries.weaverCats,
    Libraries.weaverDiscipline,
    Libraries.weaverScalaCheck,
    Libraries.catsEffectTestkit
  ).map(_ % Test)
)

lazy val buildInfoSettings = Seq(
  buildInfoKeys := Seq[BuildInfoKey](
    name,
    version,
    scalaVersion,
    sbtVersion
  ),
  buildInfoPackage := "xyz.kd5ujc.buildinfo"
)

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-feature",
  "-language:higherKinds",
  "-language:postfixOps",
  "-unchecked",
  "-Ywarn-unused:_",
  "-Yrangepos",
  "-Ywarn-macros:after"
)

lazy val integration = project
  .in(file("modules/integration"))
  .settings(
    inConfig(Test)(Defaults.testSettings)
  )

lazy val root = project
  .in(file("modules/root"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "dilf4s",
    buildInfoSettings,
    commonSettings,
    commonTestSettings
  )
  .dependsOn(integration % "test->test")

addCommandAlias("checkPR", s"; scalafixAll --check; scalafmtCheckAll")
addCommandAlias("preparePR", s"; scalafixAll; scalafmtAll")
