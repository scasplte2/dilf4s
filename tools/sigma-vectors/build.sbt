// Dev-only: generates and cross-checks the sigma vectors in shared/vectors against Ergo's own implementation.
// Not part of the main build (sigmastate is Scala 2.13). The regeneration order is in docs/specs/sigma.md section 9;
// paths must be absolute, since the run forks:
//   sbt "run generate <repo>/shared/vectors/sigma_v1.json"
//   (root) sbt "crypto/Test/runMain xyz.kd5ujc.crypto.sigma.SigmaProofsGen"
//   sbt "run check <repo>/target/sigma_dilf4s_proofs.json <repo>/shared/vectors/sigma_dilf4s_v1.json"
ThisBuild / scalaVersion := "2.13.11"

lazy val root = project
  .in(file("."))
  .settings(
    name := "sigma-vectors",
    // as the Ergo node does: sigma-state 5.0.13 names a scrypto snapshot that is no longer published
    libraryDependencies ++= Seq(
      ("org.scorexfoundation" %% "sigma-state" % "5.0.13").exclude("org.scorexfoundation", "scrypto_2.13"),
      "org.scorexfoundation" %% "scrypto" % "2.3.0"
    ),
    run / fork := true
  )
