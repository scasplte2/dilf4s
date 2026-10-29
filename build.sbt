import Dependencies.*
import sbt.*
import sbt.Keys.*

ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "xyz.kd5ujc"
ThisBuild / scalaVersion := "2.13.18"
ThisBuild / evictionErrorLevel := Level.Warn
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

ThisBuild / assemblyMergeStrategy := {
  case "logback.xml"                                       => MergeStrategy.first
  case x if x.contains("io.netty.versions.properties")     => MergeStrategy.discard
  case PathList("xyz", "kd5ujc", "buildinfo", xs @ _*)     => MergeStrategy.first
  case PathList(xs @ _*) if xs.last == "module-info.class" => MergeStrategy.first
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
  "-Wconf:cat=unused:info"
)

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions,
  resolvers += Resolver.mavenLocal,
  libraryDependencies ++= Seq(
    CompilerPlugin.kindProjector,
    CompilerPlugin.betterMonadicFor
  ),
  // Purity gate (see .workspace/crypto-architecture.md §1): main sources only; tests and
  // benchmarks are the edge. Files in LegacyPurity are pending the signing/accumulator rewrite.
  Compile / compile / wartremoverErrors ++= PurityGate.warts,
  wartremoverExcluded ++= LegacyPurity.files.map((LocalRootProject / baseDirectory).value / _),
  Compile / scalafix / unmanagedSources := {
    val legacy = LegacyPurity.files.map((LocalRootProject / baseDirectory).value / _).toSet
    (Compile / unmanagedSources).value.filterNot(legacy)
  },
  Test / scalafixConfig := Some((LocalRootProject / baseDirectory).value / ".scalafix-test.conf")
)

lazy val commonTestSettings = Seq(
  testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
  // weaver's expect macro re-emits clue strings, which trips the missing-interpolator lint
  Test / scalacOptions += "-Wconf:cat=lint-missing-interpolator:s",
  libraryDependencies ++= Seq(
    Libraries.catsEffectTestkit,
    Libraries.weaverCats,
    Libraries.weaverDiscipline,
    Libraries.weaverScalaCheck
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

// ─── Root aggregator ───
lazy val root = project.in(file("."))
  .settings(
    name := "dilf4s",
    commonSettings,
    publish / skip := true
  )
  .aggregate(models, sharedTest, numerics, core, accumulators, storage, storageRocksDb, crypto, signing, vrf, kes, benchmarks)

// ─── Models: cross-cutting value types ───
lazy val models = project.in(file("models"))
  .settings(
    name := "dilf4s-models",
    commonSettings,
    commonTestSettings,
    libraryDependencies ++= Seq(
      Libraries.cats,
      Libraries.circeCore,
    )
  )

// ─── Shared Test Utilities ───
lazy val sharedTest = project.in(file("shared-test"))
  .settings(
    name := "dilf4s-shared-test",
    commonSettings,
    libraryDependencies ++= Seq(
      Libraries.weaverCats,
      Libraries.weaverScalaCheck,
    )
  )
  .dependsOn(models)

// ─── Numerics: exact rational arithmetic ───
lazy val numerics = project.in(file("numerics"))
  .settings(
    name := "dilf4s-numerics",
    commonSettings,
    commonTestSettings,
    libraryDependencies ++= Seq(
      Libraries.cats,
      Libraries.catsKernelLaws % Test
    )
  )
  .dependsOn(sharedTest % Test)

// ─── Core: canonical encoding, tagged hashing, hash implementations ───
lazy val core = project.in(file("core"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "dilf4s-core",
    buildInfoSettings,
    commonSettings,
    commonTestSettings,
    libraryDependencies ++= Seq(
      Libraries.bc,
      Libraries.cats,
      Libraries.catsEffect,
      Libraries.circeCore,
      Libraries.circeGeneric,
      Libraries.circeParser,
      Libraries.scodecBits
    )
  )
  .dependsOn(models, numerics, sharedTest % Test)

// ─── Crypto: typed signature schemes, VRF, KES, BLS, zk primitives ───
lazy val crypto = project.in(file("crypto"))
  .settings(
    name := "dilf4s-crypto",
    commonSettings,
    commonTestSettings,
    libraryDependencies ++= Seq(
      Libraries.cats,
      Libraries.catsEffect,
      Libraries.catsMtl,
      Libraries.scodecBits,
      Libraries.bc,
      Libraries.curve25519Elisabeth,
      Libraries.secp256k1,
      Libraries.circeParser % Test,
      Libraries.catsKernelLaws % Test
    )
  )
  .dependsOn(core, sharedTest % Test)

// ─── Accumulators: Merkle, MPT, Verkle (self-contained) ───
lazy val accumulators = project.in(file("accumulators"))
  .settings(
    name := "dilf4s-accumulators",
    commonSettings,
    commonTestSettings,
    libraryDependencies ++= Seq(
      Libraries.cats,
      Libraries.catsEffect,
      Libraries.catsMtl,
      Libraries.circeCore,
      Libraries.circeGeneric,
      Libraries.circeParser,
    )
  )
  .dependsOn(models, core, storage, sharedTest % Test)

// ─── Storage: Store, VersionedStore (self-contained) ───
lazy val storage = project.in(file("storage"))
  .settings(
    name := "dilf4s-storage",
    commonSettings,
    commonTestSettings,
    libraryDependencies ++= Seq(
      Libraries.cats,
      Libraries.catsEffect,
      Libraries.catsMtl,
      Libraries.fs2Core,
      Libraries.scodecBits
    )
  )
  .dependsOn(core, sharedTest % Test)

// ─── Storage on RocksDB: separate artifact so the pure core never pulls in native code ───
lazy val storageRocksDb = project.in(file("storage-rocksdb"))
  .settings(
    name := "dilf4s-storage-rocksdb",
    commonSettings,
    commonTestSettings,
    libraryDependencies += Libraries.rocksDb
  )
  .dependsOn(storage % "compile->compile;test->test", sharedTest % Test)

// ─── Signing: Ed25519, Extended Ed25519 ───
lazy val signing = project.in(file("signing"))
  .settings(
    name := "dilf4s-signing",
    commonSettings,
    commonTestSettings,
    libraryDependencies ++= Seq(
      Libraries.cats,
      Libraries.catsEffect,
      Libraries.bc,
      Libraries.curve25519Elisabeth,
      Libraries.circeGeneric % Test,
      Libraries.circeParser  % Test,
    )
  )
  .dependsOn(models, sharedTest % Test)

// ─── VRF: ECVRF-ED25519-SHA512-TAI ───
lazy val vrf = project.in(file("vrf"))
  .settings(
    name := "dilf4s-vrf",
    commonSettings,
    commonTestSettings,
    libraryDependencies ++= Seq(
      Libraries.cats,
      Libraries.catsEffect,
      Libraries.curve25519Elisabeth,
      Libraries.circeGeneric % Test,
      Libraries.circeParser  % Test,
    )
  )
  .dependsOn(models, signing, sharedTest % Test)

// ─── KES: Forward-secure Key Evolving Signatures ───
lazy val kes = project.in(file("kes"))
  .settings(
    name := "dilf4s-kes",
    commonSettings,
    commonTestSettings,
    libraryDependencies ++= Seq(
      Libraries.cats,
      Libraries.catsEffect,
      Libraries.bc,
      Libraries.circeGeneric % Test,
      Libraries.circeParser  % Test,
    )
  )
  .dependsOn(models, signing, core, sharedTest % Test)

addCommandAlias("checkPR", s"; scalafixAll --check; scalafmtCheckAll; scalafmtSbtCheck")
addCommandAlias("preparePR", s"; scalafixAll; scalafmtAll; scalafmtSbt")

// ─── Benchmarks: JMH, never published ───
lazy val benchmarks = project.in(file("benchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(
    name := "dilf4s-benchmarks",
    commonSettings,
    publish / skip := true
  )
  .dependsOn(core, accumulators)
