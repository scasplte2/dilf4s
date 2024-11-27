package hash

import cats.effect.IO

import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.hash.{Sha3512Hasher, l512}

import io.circe.Json
import io.circe.syntax.EncoderOps
import org.bouncycastle.util.encoders.Hex
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object Sha3512HasherSuite extends SimpleIOSuite with Checkers {
  private val hasherIO = for {
    implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
    hasher = new Sha3512Hasher[IO]
  } yield hasher

  test("Hasher.hash should return a non-empty digest") {
    forall(Gen.alphaNumStr) { str =>
      for {
        hasher <- hasherIO
        digest <- hasher.hash(str)
      } yield expect(digest.value.nonEmpty)
    }
  }

  test("Hasher.compare should return true when hash of the data matches the expected hash") {
    forall(Gen.alphaNumStr) { str =>
      for {
        hasher       <- hasherIO
        expectedHash <- hasher.hash(str)
        result       <- hasher.compare(str, expectedHash)
      } yield expect(result)
    }
  }

  test("Hasher.compare should return false when hash of the data does not match the expected hash") {
    forall(Gen.alphaNumStr) { str =>
      for {
        hasher       <- hasherIO
        expectedHash <- hasher.hash(str)
        result       <- hasher.compare(str + "_updated", expectedHash)
      } yield expect(!result)
    }
  }

  test("Hasher.compare should return expected 512bit hash for fixed value") {
    for {
      hasher <- hasherIO
      // hash is utf-8 bytes of {"test":123}
      expectedHash = l512.unsafe(
        Hex.decodeStrict(
          "1b9f8d070a7e35531de189af81d8f3e3ad17ea7d013a1ef0369381130c3f6dc40354ab90947b98b9340952d4fc93ed44c35fb89a2373f5df21f05924c2be2a91"
        )
      )
      value = Json.obj("test" -> 123.asJson)
      result <- hasher.compare(value, expectedHash)
    } yield expect(result)
  }
}
