package xyz.kd5ujc.crypto.vrf

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}

import xyz.kd5ujc.crypto.CryptoVectors

import io.circe.Json
import io.circe.syntax._
import weaver.SimpleIOSuite

/** Conformance vectors for docs/specs/vrf.md: every suite over the shared seeds and messages. */
object VrfVectors {

  val path: Path = Paths.get("shared", "vectors", "vrf_v1.json")

  private def suite[S](implicit V: Vrf[S]): Json =
    Json.obj(
      "suite" -> V.name.asJson,
      "keys" -> CryptoVectors.seeds.map { seed =>
        val kp = Vrf.fromSeed[S](seed).fold(e => sys.error(e.toString), identity)
        Json.obj(
          "seed"         -> seed.toHex.asJson,
          "signingKey"   -> kp.signing.bytes.toHex.asJson,
          "verifyingKey" -> kp.verifying.bytes.toHex.asJson,
          "proofs" -> CryptoVectors.messages.map { alpha =>
            val (pi, beta) = V.prove(kp.signing, alpha).fold(e => sys.error(e.toString), identity)
            Json.obj("alpha" -> alpha.toHex.asJson, "pi" -> pi.bytes.toHex.asJson, "beta" -> beta.bytes.toHex.asJson)
          }.asJson
        )
      }.asJson
    )

  def render: String =
    Json
      .obj(
        "spec"    -> "docs/specs/vrf.md".asJson,
        "version" -> 1.asJson,
        "suites"  -> List(suite[EcVrfTai], suite[EcVrfEll2], suite[EcVrfDraft03]).asJson
      )
      .spaces2 + "\n"
}

object VrfVectorGen extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    IO(Files.write(VrfVectors.path, VrfVectors.render.getBytes(UTF_8))).as(ExitCode.Success)
}

object VrfVectorsSuite extends SimpleIOSuite {
  test("committed VRF vectors match the reference implementation") {
    IO(new String(Files.readAllBytes(VrfVectors.path), UTF_8)).map { committed =>
      expect(
        committed == VrfVectors.render,
        s"${VrfVectors.path} is stale; regenerate with: sbt \"crypto/Test/runMain xyz.kd5ujc.crypto.vrf.VrfVectorGen\""
      )
    }
  }
}
