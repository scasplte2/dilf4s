package hash

import cats.effect.IO

import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.hash.api.{DigestProducer, HashVerifier}
import xyz.kd5ujc.hash.impl.Blake2b256Hasher
import xyz.kd5ujc.hash.l256

import io.circe.Json
import io.circe.syntax.EncoderOps
import org.bouncycastle.util.encoders.Hex
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object Blake2b256HasherSuite extends SimpleIOSuite with Checkers {
  private val hasherIO = for {
    implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
    hasher = new Blake2b256Hasher[IO]
  } yield hasher

  test("Hasher.hash should return a non-empty digest") {
    forall(Gen.alphaNumStr) { str =>
      hasherIO.flatMap { implicit hasher: DigestProducer[IO] =>
        hasher
          .hash(str)
          .map(digest => expect(digest.value.nonEmpty))
      }
    }
  }

  test("Hasher.compare should return true when hash of the data matches the expected hash") {
    forall(Gen.alphaNumStr) { str =>
      hasherIO.flatMap { implicit hasher: DigestProducer[IO] =>
        for {
          expectedHash <- hasher.hash(str)
          result       <- HashVerifier[F].confirmDigest(str, expectedHash)
        } yield expect(result)
      }
    }
  }

  test("Hasher.compare should return false when hash of the data does not match the expected hash") {
    forall(Gen.alphaNumStr) { str =>
      hasherIO.flatMap { implicit hasher: DigestProducer[IO] =>
        for {
          expectedHash <- hasher.hash(str)
          result       <- HashVerifier[F].confirmDigest(str + "_updated", expectedHash)
        } yield expect(!result)
      }
    }
  }

  test("Hasher.compare should return expected 256bit hash for fixed value") {
    hasherIO.flatMap { implicit hasher: DigestProducer[IO] =>
      // hash is utf-8 bytes of {"test":123}
      val expectedHash = l256.unsafe(
        Hex.decodeStrict(
          "f4e96eb3066f5bf78b3901de033271356e68e559d2843438dc432096a802d727"
        )
      )

      HashVerifier[F]
        .confirmDigest(Json.obj("test" -> 123.asJson), expectedHash)
        .map(expect(_))
    }
  }
}
