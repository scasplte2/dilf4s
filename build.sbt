import Dependencies.*
import sbt.*
import sbt.Keys.*

ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "xyz.kd5ujc"
ThisBuild / scalaVersion := "3.9.0"
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
  "-Wunused:all"
)

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions,
  resolvers += Resolver.mavenLocal,
  // Purity gate: main sources only; tests and benchmarks are the edge.
  Compile / compile / wartremoverErrors ++= PurityGate.warts,
  Test / scalafixConfig := Some((LocalRootProject / baseDirectory).value / ".scalafix-test.conf")
)

lazy val commonTestSettings = Seq(
  testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
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
  .aggregate(sharedTest, numerics, core, accumulators, storage, storageRocksDb, crypto, modelsGen, models, benchmarks)

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
  .dependsOn(numerics, sharedTest % Test)

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
      Libraries.jblst,
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
  .dependsOn(core, storage, models, modelsGen % "test->test", sharedTest % Test)

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
  .dependsOn(core, models, sharedTest % Test)

// ─── Storage on RocksDB: separate artifact so the pure core never pulls in native code ───
lazy val storageRocksDb = project.in(file("storage-rocksdb"))
  .settings(
    name := "dilf4s-storage-rocksdb",
    commonSettings,
    commonTestSettings,
    libraryDependencies += Libraries.rocksDb
  )
  .dependsOn(storage % "compile->compile;test->test", sharedTest % Test)

// ─── models-gen: CDDL (RFC 8610) -> types and canonical codecs (build-time tool, ported from scarab) ───
lazy val modelsGen = project.in(file("models-gen"))
  .settings(
    name := "dilf4s-models-gen",
    commonSettings,
    commonTestSettings,
    publish / skip := true,
    libraryDependencies ++= Seq(
      Libraries.cats,
      Libraries.catsEffect,
      Libraries.catsParse
    )
  )
  .dependsOn(core % Test, sharedTest % Test)

// ─── models: wire types and canonical codecs generated from shared/schemas at build time ───
lazy val models = project.in(file("models"))
  .settings(
    name := "dilf4s-models",
    commonSettings,
    commonTestSettings,
    libraryDependencies ++= Seq(Libraries.scodecBits, Libraries.circeParser % Test),
    Compile / sourceGenerators += Def.task {
      val log = streams.value.log
      val schemas = (LocalRootProject / baseDirectory).value / "shared" / "schemas"
      val out = (Compile / sourceManaged).value / "dilf4s-models"
      val cp = (modelsGen / Compile / fullClasspath).value.files
      val genRunner = (modelsGen / Compile / runner).value
      val generatorClasses = ((modelsGen / Compile / classDirectory).value ** "*.class").get
      val inputs = ((schemas ** "*.cddl").get ++ generatorClasses).toSet
      val cached = FileFunction.cached(streams.value.cacheDirectory / "models-gen", FilesInfo.hash) { _ =>
        IO.delete(out)
        genRunner
          .run("xyz.kd5ujc.modelsgen.Main", cp, Seq(schemas.getAbsolutePath, out.getAbsolutePath, "xyz.kd5ujc.models"), log)
          .failed
          .foreach(e => sys.error(s"models generator failed: ${e.getMessage}"))
        (out ** "*.scala").get.toSet
      }
      cached(inputs).toSeq.sortBy(_.getAbsolutePath)
    }.taskValue
  )
  .dependsOn(core, sharedTest % Test)

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
