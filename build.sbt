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

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:reflectiveCalls",
  "-language:higherKinds",
  "-language:postfixOps",
  "-Yrangepos",
  "-Ymacro-annotations",
  "-Ywarn-unused:_",
  "-Ywarn-macros:after",
  "-Wconf:cat=unused:info",
)

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions,
  resolvers += Resolver.mavenLocal,
  libraryDependencies ++= Seq(
    CompilerPlugin.kindProjector,
    CompilerPlugin.betterMonadicFor,
    CompilerPlugin.semanticDB,
    Libraries.bc,
    Libraries.cats,
    Libraries.catsEffect,
    Libraries.circeCore,
    Libraries.circeGeneric,
    Libraries.circeParser,
    Libraries.levelDb,
    Libraries.levelDbJni,
    Libraries.logback,
    Libraries.log4cats
  )
)

lazy val commonTestSettings = Seq(
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

lazy val root = project.in(file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "dilf4s",
    buildInfoSettings,
    commonSettings,
    commonTestSettings
  )

addCommandAlias("checkPR", s"; scalafixAll --check; scalafmtCheckAll")
addCommandAlias("preparePR", s"; scalafixAll; scalafmtAll")
