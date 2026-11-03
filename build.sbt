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
  // Hyperledger Besu's native libraries are not on Maven Central; the pinned copies are vendored here (vendor/README.md)
  resolvers += MavenRepository("dilf4s-vendor", ((ThisBuild / baseDirectory).value / "vendor" / "maven").toURI.toString),
  update := Pinned.check(update.value),
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
  .aggregate(sharedTest, numerics, core, accumulators, storage, storageRocksDb, crypto, modelsGen, jlvm, benchmarks, scalafixRules, scalafixInput, scalafixTests)

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
    // main sources hold no hashed collection: canonical maps carry caller-chosen keys (.scalafix-core.conf)
    Compile / scalafixConfig := Some((LocalRootProject / baseDirectory).value / ".scalafix-core.conf"),
    libraryDependencies ++= Seq(
      Libraries.bc,
      Libraries.cats,
      Libraries.catsEffect,
      Libraries.circeCore,
      Libraries.circeGeneric,
      Libraries.circeParser,
      Libraries.scodecBits
    ),
    // wire types and canonical codecs, generated from shared/schemas at build time (package xyz.kd5ujc.models). The
    // generator is referenced by id: its tests depend on core, so naming its project value here would be a cycle.
    Compile / sourceGenerators += Def.task {
      val log = streams.value.log
      val schemas = (LocalRootProject / baseDirectory).value / "shared" / "schemas"
      val out = (Compile / sourceManaged).value / "dilf4s-models"
      val cp = (LocalProject("modelsGen") / Compile / fullClasspath).value.files
      val genRunner = (LocalProject("modelsGen") / Compile / runner).value
      val generatorClasses = ((LocalProject("modelsGen") / Compile / classDirectory).value ** "*.class").get
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
  .dependsOn(numerics, sharedTest % Test, scalafixRules % ScalafixConfig)

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
      Libraries.besuGnark,
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
  .dependsOn(core, storage % "compile->compile;test->test", modelsGen % "test->test", sharedTest % Test)

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

// ─── JLVM: the strict JsonLogic-syntax ledger VM (docs/specs/jlvm.md) ───
lazy val jlvm = project.in(file("jlvm"))
  .settings(
    name := "dilf4s-jlvm",
    commonSettings,
    commonTestSettings,
    libraryDependencies ++= Seq(
      Libraries.cats,
      Libraries.catsEffect,
      Libraries.circeCore,
      Libraries.circeParser,
      Libraries.scodecBits
    )
  )
  .dependsOn(core % "compile->compile;test->test", crypto, accumulators, sharedTest % Test)

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
  .dependsOn(core, accumulators, jlvm)

// ─── scalafix-rules: core's semantic lint (NoHashing), the code it must refuse, and the testkit suite that checks it ───
lazy val scalafixRules = project.in(file("scalafix-rules/rules"))
  .settings(
    name := "dilf4s-scalafix-rules",
    scalaVersion := _root_.scalafix.sbt.BuildInfo.scala213,
    libraryDependencies ++= Seq(
      "ch.epfl.scala" %% "scalafix-core" % _root_.scalafix.sbt.BuildInfo.scalafixVersion,
      // the classpath symbol table (scalafix's own runtime provides it): a Scala 3 SemanticDB omits library symbols
      ("org.scalameta" % "semanticdb-scalac-core" % "4.17.4" % Provided).cross(CrossVersion.full)
    ),
    // the Scala 2.13 library the rule reads library symbols from: Scala 3.8's own carries TASTy, not Scala 2 signatures
    Compile / resourceGenerators += Def.task {
      val library = (Compile / dependencyClasspath).value.files
        .find(f => f.getName.startsWith("scala-library-2.13"))
        .getOrElse(sys.error("NoHashing needs a Scala 2.13 scala-library on its classpath"))
      val out = (Compile / resourceManaged).value / "xyz" / "kd5ujc" / "scalafix" / "scala-library.path"
      IO.write(out, library.getAbsolutePath)
      Seq(out)
    }.taskValue,
    publish / skip := true
  )

// every way of hashing a key the rule must refuse, each line marked with the diagnostic it must produce
lazy val scalafixInput = project.in(file("scalafix-rules/input"))
  .settings(
    name := "dilf4s-scalafix-input",
    publish / skip := true
  )
  .dependsOn(core)

lazy val scalafixTests = project.in(file("scalafix-rules/tests"))
  .enablePlugins(ScalafixTestkitPlugin)
  .settings(
    name := "dilf4s-scalafix-tests",
    scalaVersion := _root_.scalafix.sbt.BuildInfo.scala213,
    publish / skip := true,
    libraryDependencies +=
      ("ch.epfl.scala" % "scalafix-testkit" % _root_.scalafix.sbt.BuildInfo.scalafixVersion % Test).cross(CrossVersion.full),
    // the fixtures as the testkit expects them back: a lint changes nothing, less their config header and assertions
    scalafixTestkitOutputSourceDirectories := Seq(target.value / "fixtures-expected"),
    Test / resourceGenerators += Def.task {
      val out = target.value / "fixtures-expected"
      (scalafixInput / Compile / unmanagedSourceDirectories).value.flatMap { dir =>
        (dir ** "*.scala").get.map { f =>
          val text = IO.read(f).replaceFirst("(?s)\\A/\\*\\nrules = .*?\\n \\*/\\n", "").replace("// assert: NoHashing", "")
          val dest = out / IO.relativize(dir, f).getOrElse(f.getName)
          IO.write(dest, text)
          dest
        }
      }
    }.taskValue,
    scalafixTestkitInputSourceDirectories := (scalafixInput / Compile / unmanagedSourceDirectories).value,
    scalafixTestkitInputClasspath := (scalafixInput / Compile / fullClasspath).value,
    scalafixTestkitInputScalacOptions := (scalafixInput / Compile / scalacOptions).value,
    scalafixTestkitInputScalaVersion := (scalafixInput / Compile / scalaVersion).value
  )
  .dependsOn(scalafixRules)
