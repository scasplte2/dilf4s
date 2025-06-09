package hash

import cats.effect.IO

import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.hash.api.{DigestProducer, HashVerifier}
import xyz.kd5ujc.hash.impl.Blake2b512Hasher
import xyz.kd5ujc.hash.l512

import io.circe.Json
import io.circe.syntax.EncoderOps
import org.bouncycastle.util.encoders.Hex
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object Blake2b512HasherSuite extends SimpleIOSuite with Checkers {
  private val hasherIO = for {
    implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
    hasher = new Blake2b512Hasher[IO]
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

  test("Hasher.compare should return expected 512bit hash for fixed value") {
    hasherIO.flatMap { implicit hasher: DigestProducer[IO] =>
      // hash is utf-8 bytes of {"test":123}
      val expectedHash = l512.unsafe(
        Hex.decodeStrict(
          "a2582130f8f37400f9a7da828d077f45c3d741d269116fedd2eb6305c340a858b3345cef09d31917149c6ff71b5b3ddc55cfcb1cfc46bd0b7cf509e8de18df9c"
        )
      )

      HashVerifier[F]
        .confirmDigest(Json.obj("test" -> 123.asJson), expectedHash)
        .map(expect(_))
    }
  }
}
