package hash

import cats.effect.IO

import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.hash.api.{DigestProducer, HashVerifier}
import xyz.kd5ujc.hash.impl.Sha3_256Hasher
import xyz.kd5ujc.hash.l256

import io.circe.Json
import io.circe.syntax.EncoderOps
import org.bouncycastle.util.encoders.Hex
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object Sha3256HasherSuite extends SimpleIOSuite with Checkers {
  private val hasherIO = for {
    implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
    hasher = new Sha3_256Hasher[IO]
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
          "8868f335dffde4e47eb50b75d6dae8b1a1d9f8ba9b116a5a0e3527ba7da6e74c"
        )
      )

      HashVerifier[F]
        .confirmDigest(Json.obj("test" -> 123.asJson), expectedHash)
        .map(expect(_))
    }
  }
}
