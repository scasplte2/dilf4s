package xyz.kd5ujc.core.vectors

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}

import xyz.kd5ujc.core.canonical.{CanonicalJson, CanonicalValue, Cbor}
import xyz.kd5ujc.core.hash._

import io.circe.Json
import io.circe.syntax._
import scodec.bits.{ByteVector, hex}

/** The conformance vectors for docs/specs/canonical-encoding.md; every SDK must reproduce them. */
object CanonicalVectors {

  val path: Path = Paths.get("shared", "vectors", "canonical_encoding_v1.json")

  private val tagName = "dilf4s/vectors/canonical/v1"
  private val tag = DomainTag(tagName).fold(e => sys.error(e.toString), identity)
  private val sha256 = TaggedHash[Sha256](tag)

  /** JSON text inputs; each yields either an encoding and digest or an error kind. */
  val jsonInputs: List[String] = List(
    "null",
    "true",
    "false",
    "0",
    "-0",
    "1",
    "-1",
    "23",
    "24",
    "255",
    "256",
    "65535",
    "65536",
    "4294967295",
    "4294967296",
    "9007199254740991",
    "9007199254740992",
    "9007199254740993",
    "-9007199254740993",
    "18446744073709551615",
    "18446744073709551616",
    "-18446744073709551616",
    "-18446744073709551617",
    "123456789012345678901234567890123456789",
    "0.1",
    "0.5",
    "-0.375",
    "1.0",
    "1.00",
    "100e-2",
    "1e2",
    "1E-7",
    "2.5e-3",
    "3.14159265358979323846264338327950288",
    "\"\"",
    "\"a\"",
    "\"\\u00e9\"",
    "\"é\"",
    "\"\\ud83d\\ude00\"",
    "\"\\u0000\"",
    "\"\\\"\\\\/\\b\\f\\n\\r\\t\"",
    "[]",
    "[null]",
    "[1,[2,[3]]]",
    "{}",
    "{\"a\":null}",
    "{\"b\":1,\"a\":2}",
    "{\"aa\":1,\"b\":2,\"a\":3,\"é\":4,\"\\ud83d\\ude00\":5,\"\\ufb33\":6}",
    "{\"10\":null,\"1\":[],\"d\":true}",
    "[56,{\"d\":true,\"10\":null,\"1\":[]}]",
    // rejected
    "{\"a\":1,\"a\":2}",
    "{\"a\":null,\"a\":2}",
    "\"\\ud800\"",
    "{\"\\udc00\":1}",
    "01",
    "1.",
    ".5",
    "1e",
    "NaN",
    "Infinity",
    "[1,]",
    "{\"a\":1,}",
    "1e-2000000000",
    "[1]]",
    "\"\\u0001\"".replace("\\u0001", "\u0001")
  )

  /** Values with no JSON form, given directly in the data model. */
  val modelInputs: List[(String, CanonicalValue)] = List(
    "bytes(empty)"    -> CanonicalValue.bytes(ByteVector.empty),
    "bytes(01020304)" -> CanonicalValue.bytes(hex"01020304"),
    "rational(1/3)"   -> CanonicalValue.number(xyz.kd5ujc.numerics.Ratio.of(1, 3).fold(e => sys.error(e.toString), identity)),
    "rational(-2^70/3)" -> CanonicalValue.number(
      xyz.kd5ujc.numerics.Ratio.of(-(BigInt(1) << 70), 3).fold(e => sys.error(e.toString), identity)
    )
  )

  val cborRejections: List[ByteVector] = List(
    hex"1817",
    hex"780161",
    hex"9f01ff",
    hex"f93c00",
    hex"fb3ff0000000000000",
    hex"f7",
    hex"c11a514b67b0",
    hex"c24101",
    hex"c2490001000000000000000000",
    hex"d81e820101",
    hex"d81e820204",
    hex"a2616201616101",
    hex"a2616101616102",
    hex"a16161f6",
    hex"a10101",
    hex"63eda080",
    hex"62c0af",
    hex"0000",
    hex"1a0001",
    hex"9bffffffffffffffff"
  )

  private val tagMessages: List[ByteVector] = List(ByteVector.empty, ByteVector("abc".getBytes(UTF_8)), hex"00ff00ff")

  private def errorKind(e: Product): Json = e.productPrefix.asJson

  def build: Json = Json.obj(
    "spec"                 -> "docs/specs/canonical-encoding.md".asJson,
    "version"              -> 1.asJson,
    "contentHashTag"       -> tagName.asJson,
    "contentHashAlgorithm" -> "SHA-256".asJson,
    "json" -> jsonInputs.map { in =>
      CanonicalJson.parse(in) match {
        case Right(v) =>
          val bytes = Cbor.encode(v)
          Json.obj("json" -> in.asJson, "cbor" -> bytes.toHex.asJson, "contentHash" -> sha256(bytes).hex.asJson)
        case Left(e) => Json.obj("json" -> in.asJson, "error" -> errorKind(e))
      }
    }.asJson,
    "model" -> modelInputs.map {
      case (name, v) =>
        val bytes = Cbor.encode(v)
        Json.obj("value" -> name.asJson, "cbor" -> bytes.toHex.asJson, "contentHash" -> sha256(bytes).hex.asJson)
    }.asJson,
    "cborRejections" -> cborRejections.map { b =>
      Json.obj("cbor" -> b.toHex.asJson, "error" -> Cbor.decode(b).fold(errorKind, v => s"ACCEPTED $v".asJson))
    }.asJson,
    "taggedHash" -> (for {
      m <- tagMessages
      (name, digest) <- List[(String, ByteVector => Digest[?])](
        "SHA-256"     -> TaggedHash[Sha256](tag).apply,
        "SHA3-256"    -> TaggedHash[Sha3_256](tag).apply,
        "SHA3-512"    -> TaggedHash[Sha3_512](tag).apply,
        "BLAKE2b-256" -> TaggedHash[Blake2b256](tag).apply,
        "BLAKE2b-512" -> TaggedHash[Blake2b512](tag).apply
      )
    } yield
      Json.obj("algorithm" -> name.asJson, "tag" -> tagName.asJson, "message" -> m.toHex.asJson, "digest" -> digest(m).hex.asJson)).asJson
  )

  def render: String = build.spaces2 + "\n"
}

/** `sbt "core/Test/runMain xyz.kd5ujc.core.vectors.CanonicalVectorGen"` regenerates the committed vectors. */
object CanonicalVectorGen extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    IO {
      Files.createDirectories(CanonicalVectors.path.getParent)
      Files.write(CanonicalVectors.path, CanonicalVectors.render.getBytes(UTF_8))
    }.as(ExitCode.Success)
}
