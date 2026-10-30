package xyz.kd5ujc.crypto.kes

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}

import xyz.kd5ujc.core.hash.{Blake2b256, Sha3_512}

import io.circe.Json
import io.circe.syntax._
import scodec.bits.{ByteVector, hex}
import weaver.SimpleIOSuite

/** Conformance vectors for docs/specs/kes.md: every period of small instances of each construction. */
object KesVectors {

  val path: Path = Paths.get("shared", "vectors", "kes_v1.json")
  val seed: ByteVector = ByteVector((0 until 32).map(_.toByte))
  val message: ByteVector = hex"6b6573"

  private def get[A](e: Either[KesError, A]): A = e.fold(err => sys.error(err.toString), identity)

  /** One record per period: the encoded key, and the signature on [[message]]. */
  private def periods[K](
    name:   String,
    params: Json,
    gen:    ByteVector => Either[KesError, K],
    vk:     K => ByteVector,
    bytes:  K => ByteVector,
    period: K => Long,
    sign:   K => Either[KesError, ByteVector],
    evolve: K => Either[KesError, K]
  ): Json = {
    val k0 = get(gen(seed))
    val keys = List.unfold(Option(k0))(_.map(k => (k, evolve(k).toOption)))
    Json.obj(
      "construction" -> name.asJson,
      "parameters"   -> params,
      "seed"         -> seed.toHex.asJson,
      "message"      -> message.toHex.asJson,
      "verifyingKey" -> vk(k0).toHex.asJson,
      "periods" -> keys.map { k =>
        Json.obj("period" -> period(k).asJson, "key" -> bytes(k).toHex.asJson, "signature" -> get(sign(k)).toHex.asJson)
      }.asJson
    )
  }

  def render: String = {
    val sum = get(KesSum[Blake2b256](2))
    val sum512 = get(KesSum[Sha3_512](1))
    val product = get(KesProduct[Blake2b256](1, 2))
    val cardano = get(CardanoSumKes(2))
    Json
      .obj(
        "spec"    -> "docs/specs/kes.md".asJson,
        "version" -> 1.asJson,
        "vectors" -> List(
          periods[sum.Key](
            "dilf4s-sum",
            Json.obj("hash" -> "BLAKE2b-256".asJson, "height" -> 2.asJson),
            sum.generate,
            sum.verifyingKey,
            _.bytes,
            _.period,
            sum.sign(_, message),
            sum.evolve
          ),
          periods[sum512.Key](
            "dilf4s-sum",
            Json.obj("hash" -> "SHA3-512".asJson, "height" -> 1.asJson),
            sum512.generate,
            sum512.verifyingKey,
            _.bytes,
            _.period,
            sum512.sign(_, message),
            sum512.evolve
          ),
          periods[product.Key](
            "dilf4s-product",
            Json.obj("hash" -> "BLAKE2b-256".asJson, "superHeight" -> 1.asJson, "subHeight" -> 2.asJson),
            product.generate,
            product.verifyingKey,
            _.bytes,
            _.period,
            product.sign(_, message),
            product.evolve
          ),
          periods[cardano.Key](
            "cardano-sum",
            Json.obj("depth" -> 2.asJson),
            cardano.generate,
            cardano.verifyingKey,
            _.bytes,
            _.period,
            k => Right(cardano.sign(k, message)),
            cardano.evolve
          )
        ).asJson
      )
      .spaces2 + "\n"
  }
}

object KesVectorGen extends IOApp {
  def run(args: List[String]): IO[ExitCode] = IO(Files.write(KesVectors.path, KesVectors.render.getBytes(UTF_8))).as(ExitCode.Success)
}

object KesVectorsSuite extends SimpleIOSuite {
  test("committed KES vectors match the reference implementation") {
    IO(new String(Files.readAllBytes(KesVectors.path), UTF_8)).map { committed =>
      expect(
        committed == KesVectors.render,
        s"${KesVectors.path} is stale; regenerate with: sbt \"crypto/Test/runMain xyz.kd5ujc.crypto.kes.KesVectorGen\""
      )
    }
  }
}
