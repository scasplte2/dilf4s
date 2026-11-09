import Dependencies.*
import sbt.*
import sbt.Keys.*

ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "xyz.kd5ujc"
ThisBuild / scalaVersion := "3.9.0"
ThisBuild / evictionErrorLevel := Level.Warn
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

// POM metadata of every published module
ThisBuild / homepage := Some(url("https://github.com/scasplte2/dilf4s"))
ThisBuild / licenses := List("MPL-2.0" -> url("https://www.mozilla.org/en-US/MPL/2.0/"))
ThisBuild / scmInfo := Some(ScmInfo(url("https://github.com/scasplte2/dilf4s"), "scm:git:git@github.com:scasplte2/dilf4s.git"))
ThisBuild / developers := List(Developer("scasplte2", "scasplte2", "", url("https://github.com/scasplte2")))
ThisBuild / versionScheme := Some("early-semver")

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
  .aggregate(sharedTest, numerics, core, circeInterop, accumulators, storage, storageRocksDb, crypto, modelsGen, jlvm, javaApi, benchmarks)

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
      Libraries.scodecBits
    ),
    // no circe, in main or test: JSON is CanonicalJson's alone, and circe interop lives in circeInterop
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
  .dependsOn(numerics, sharedTest % Test)

// ─── circe interop: typed data with circe codecs, in and out of the canonical model (core has no circe) ───
lazy val circeInterop = project.in(file("circe-interop"))
  .settings(
    name := "dilf4s-circe-interop",
    commonSettings,
    commonTestSettings,
    libraryDependencies ++= Seq(
      Libraries.cats,
      Libraries.circeCore,
      Libraries.circeGeneric % Test,
      Libraries.circeParser  % Test
    )
  )
  .dependsOn(core % "compile->compile;test->test", sharedTest % Test)

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
      // the vector generators only
      Libraries.circeCore   % Test,
      Libraries.circeParser % Test
    )
  )
  .dependsOn(
    core % "compile->compile;test->test",
    storage % "compile->compile;test->test",
    modelsGen % "test->test",
    circeInterop % Test,
    sharedTest % Test
  )

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
      Libraries.scodecBits,
      // the vector generators and the corpus tests only: programs are read by core's parser (docs/specs/jlvm.md)
      Libraries.circeCore   % Test,
      Libraries.circeParser % Test
    )
  )
  .dependsOn(core % "compile->compile;test->test", crypto, accumulators, circeInterop % Test, sharedTest % Test)

// ─── Java API: records, sealed families and Result over core, crypto, accumulators and jlvm (java/README.md) ───
// Its public API is Java (src/main/java). The conversions from the reference's types are Scala (src/main/scala,
// package xyz.kd5ujc.dilf4s.internal), so every match over a reference family is checked exhaustive.
lazy val javaApi = project.in(file("java"))
  .settings(
    name := "dilf4s-java",
    description := "A Java API over dilf4s: canonical values, signing, proofs and the JLVM, with no Scala type in its signatures",
    commonSettings,
    // a Java artifact: its name carries no Scala version (its Scala dependencies carry theirs)
    crossPaths := false,
    javacOptions ++= Seq("--release", "21", "-encoding", "UTF-8"),
    // compiling only: publishLocal's javadoc reuses javacOptions and rejects these
    Compile / compile / javacOptions ++= Seq("-Xlint:all", "-Werror"),
    Test / compile / javacOptions ++= Seq("-Xlint:all", "-Werror"),
    // the docs jar is the Java API's javadoc: its Java sources but internal (scaladoc of a mixed project documents only
    // the Scala bridge), against the compiled classes the sources import
    Compile / doc / sources := (Compile / doc / sources).value
      .filter(f => f.getName.endsWith(".java") && !f.getPath.contains(s"${java.io.File.separator}internal${java.io.File.separator}")),
    Compile / doc / dependencyClasspath += Attributed.blank((Compile / classDirectory).value),
    Compile / doc := (Compile / doc).dependsOn(Compile / compile).value,
    Compile / doc / javacOptions += "-Xdoclint:all,-missing",
    libraryDependencies ++= Seq(Libraries.jupiterInterface % Test, Libraries.junitJupiterApi % Test),
    testFrameworks += new TestFramework("com.github.sbt.junit.jupiter.api.JupiterFramework"),
    // each suite in a fresh JVM (so its first calls run cold, unwarmed by the others) at the JVM's default 1 MB thread
    // stack, the stack java/README.md's guarantee names: .jvmopts gives sbt 32 MB stacks, which hide recursion as deep
    // as its input (the reference's PBKDF2 and JLVM parser; see java/README.md, "Thread stacks")
    Test / fork := true,
    Test / javaOptions ++= Seq(
      "-Xss1m",
      s"-Ddilf4s.vectors=${((LocalRootProject / baseDirectory).value / "shared" / "vectors").getAbsolutePath}",
      // the sources EffectScanTest reads: the one effect the API runs is in one Java method
      s"-Ddilf4s.sources=${(baseDirectory.value / "src").getAbsolutePath}"
    ),
    Test / testGrouping := {
      val options = (Test / javaOptions).value.toVector
      (Test / definedTests).value.map(t => Tests.Group(t.name, Seq(t), Tests.SubProcess(ForkOptions().withRunJVMOptions(options))))
    }
  )
  .dependsOn(core, crypto, accumulators, jlvm)

addCommandAlias("checkPR", s"; scalafixAll --check; scalafmtCheckAll; scalafmtSbtCheck")
addCommandAlias("preparePR", s"; scalafixAll; scalafmtAll; scalafmtSbt")

// ─── Benchmarks: JMH, never published ───
lazy val benchmarks = project.in(file("benchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(
    name := "dilf4s-benchmarks",
    commonSettings,
    commonTestSettings,
    publish / skip := true,
    libraryDependencies ++= Seq(
      // the gas calibration's cases are written as JSON program text
      Libraries.circeCore   % Test,
      Libraries.circeParser % Test
    )
  )
  .dependsOn(core, accumulators, jlvm)
