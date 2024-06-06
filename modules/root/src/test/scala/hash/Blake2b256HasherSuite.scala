package hash

import cats.effect.IO
import io.circe.Json
import io.circe.syntax.EncoderOps
import org.bouncycastle.crypto.digests.Blake2bDigest
import org.bouncycastle.util.encoders.Hex
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers
import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.hash.{Blake2b256Hasher, l256, l512}

object Blake2b256HasherSuite extends SimpleIOSuite with Checkers {
  private val hasherIO = for {
    implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
    hasher = new Blake2b256Hasher[IO]
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
        hasher <- hasherIO
        expectedHash <- hasher.hash(str)
        result <- hasher.compare(str, expectedHash)
      } yield expect(result)
    }
  }

  test("Hasher.compare should return false when hash of the data does not match the expected hash") {
    forall(Gen.alphaNumStr) { str =>
      for {
        hasher <- hasherIO
        expectedHash <- hasher.hash(str)
        result <- hasher.compare(str + "_updated", expectedHash)
      } yield expect(!result)
    }
  }

  test("Hasher.compare should return expected 256bit hash for fixed value") {
    for {
      hasher <- hasherIO
      // hash is utf-8 bytes of {"test":123}
      expectedHash = l256.unsafe(Hex.decodeStrict("f4e96eb3066f5bf78b3901de033271356e68e559d2843438dc432096a802d727"))
      value = Json.obj("test" -> 123.asJson)
      result <- hasher.compare(value, expectedHash)
    } yield expect(result)
  }
}
