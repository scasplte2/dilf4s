package hash

import cats.effect.IO
import io.circe.Json
import io.circe.syntax.EncoderOps
import org.bouncycastle.util.encoders.Hex
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers
import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.hash.{Sha3256Hasher, l256}

object Sha3256HasherSuite extends SimpleIOSuite with Checkers {
  private val hasherIO = for {
    implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
    hasher = new Sha3256Hasher[IO]
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

  test("Hasher.compare should return expected 256bit hash for fixed value") {
    for {
      hasher <- hasherIO
      // hash is utf-8 bytes of {"test":123}
      expectedHash = l256.unsafe(Hex.decodeStrict("8868f335dffde4e47eb50b75d6dae8b1a1d9f8ba9b116a5a0e3527ba7da6e74c"))
      value = Json.obj("test" -> 123.asJson)
      result <- hasher.compare(value, expectedHash)
    } yield expect(result)
  }
}
