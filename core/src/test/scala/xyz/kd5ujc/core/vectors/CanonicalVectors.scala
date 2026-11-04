package xyz.kd5ujc.core.vectors

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}

import xyz.kd5ujc.core.canonical.{CanonicalGens, CanonicalJson, CanonicalValue, Cbor}
import xyz.kd5ujc.core.hash._
import xyz.kd5ujc.core.vectors.VectorJson.{arr, bool, num, obj, str}

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
    // surrogate pairs across the supplementary planes, escaped and literal: U+10000, U+20000, U+10FFFF
    "\"\\ud800\\udc00\"",
    "\"\\ud840\\udc00\"",
    "\"" + new String(Character.toChars(0x20000)) + "\"",
    "\"\\udbff\\udfff\"",
    "\"" + new String(Character.toChars(0x10ffff)) + "\"",
    "{\"\\ud840\\udc00\":1,\"" + new String(Character.toChars(0x10ffff)) + "\":2}",
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
    // a 24-byte key takes a 2-byte head, so it sorts after every shorter key
    s"{\"${"a" * 24}\":1,\"${"b" * 23}\":2,\"c\":3}",
    "1e400",
    "-1e-400",
    "0e10000",
    "0.000",
    // significant digits: 10,000 is the bound, and leading zeros (of the fraction, or of the exponent) are not counted
    "9" * 10000,
    "0." + "0" * 10000 + "1e1",
    "1e0000000002",
    // integers: 33220 bits is the bound (CanonicalValue.MaxIntegerBits), for either sign. 10^10000 is within it and has
    // 10001 digits, so it reads but does not render; the widest a 10,000-digit significand times ten reaches is the
    // largest multiple of ten within 2^33220 - 1, and the next one up is a bit too wide
    "1e10000",
    "-1e10000",
    "1.5e10000",
    widestTenth.toString + "e1",
    "-" + widestTenth.toString + "e1",
    // depth: 256 nested arrays is the limit, and an empty array is a level
    "[" * 255 + "[]" + "]" * 255,
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
    "\"\\u0001\"".replace("\\u0001", "\u0001"),
    "0e-10001",
    "1e10001",
    "9" * 10001,
    "0." + "1" * 10001 + "e1",
    "1e" + "1" * 10001,
    "2e10000",
    "-2e10000",
    (widestTenth + 1).toString + "e1",
    "-" + (widestTenth + 1).toString + "e1",
    "9" * 10000 + "e10000",
    "[" * 256 + "[]" + "]" * 256,
    "[1,2",
    "{\"a\":1",
    "\"abc",
    "\"\\x\"",
    "\"\\u12\"",
    "-",
    "+1",
    "1 2",
    "{1:2}",
    "[1]x"
  )

  /** Values with no JSON form, given directly in the data model. */
  val modelInputs: List[(String, CanonicalValue)] = List(
    "bytes(empty)"    -> CanonicalValue.bytes(ByteVector.empty),
    "bytes(01020304)" -> CanonicalValue.bytes(hex"01020304"),
    "rational(1/3)" -> CanonicalValue
      .number(xyz.kd5ujc.numerics.Ratio.of(1, 3).fold(e => sys.error(e.toString), identity))
      .fold(e => sys.error(e.toString), identity),
    "rational(-2^70/3)" -> CanonicalValue
      .number(
        xyz.kd5ujc.numerics.Ratio.of(-(BigInt(1) << 70), 3).fold(e => sys.error(e.toString), identity)
      )
      .fold(e => sys.error(e.toString), identity),
    "rational(1/2^64)" -> CanonicalValue
      .number(
        xyz.kd5ujc.numerics.Ratio.of(1, BigInt(1) << 64).fold(e => sys.error(e.toString), identity)
      )
      .fold(e => sys.error(e.toString), identity),
    // the widest parts a rational has (CanonicalValue.MaxRationalBits, the bit length of 10^10000), for either sign; the last pair
    // is consecutive Fibonacci numbers, coprime and the slowest gcd of its width
    "rational((2^33220-1)/2)"     -> rational(Edge - 1, 2),
    "rational(-2^33220/3)"        -> rational(-Edge, 3),
    "rational(2/(2^33220-1))"     -> rational(2, Edge - 1),
    "rational(F(47851)/F(47850))" -> rational(fibonacci._1, fibonacci._2),
    // the widest integers (CanonicalValue.MaxIntegerBits), for either sign: 4153 bytes of magnitude
    "int(2^33220-1)" -> CanonicalGens.int(IntEdge - 1),
    "int(-2^33220)"  -> CanonicalGens.int(-IntEdge)
  )

  /** `2^MaxIntegerBits`: one bit wider than an integer may be. */
  private lazy val IntEdge: BigInt = BigInt(1) << CanonicalValue.MaxIntegerBits

  /** The largest `t` with `10 t` within [[CanonicalValue.MaxIntegerBits]]: 10,000 digits, as a JSON significand may have. */
  private lazy val widestTenth: BigInt = (IntEdge - 1) / 10

  private def rational(n: BigInt, d: BigInt): CanonicalValue =
    CanonicalValue
      .number(xyz.kd5ujc.numerics.Ratio.of(n, d).fold(e => sys.error(e.toString), identity))
      .fold(e => sys.error(e.toString), identity)

  /** `2^MaxRationalBits`: one bit wider than a rational's part may be. */
  private lazy val Edge: BigInt = BigInt(1) << CanonicalValue.MaxRationalBits

  /** The consecutive Fibonacci numbers `(F(k + 1), F(k))` with `F(k + 1)` exactly `MaxRationalBits` wide. */
  private lazy val fibonacci: (BigInt, BigInt) = {
    @scala.annotation.tailrec
    def go(a: BigInt, b: BigInt): (BigInt, BigInt) = if (b.bitLength >= CanonicalValue.MaxRationalBits) (b, a) else go(b, a + b)
    go(BigInt(1), BigInt(1))
  }

  /** Tag 30 around `[n, d]`, whatever `n` and `d` are. */
  private def rationalBytes(n: BigInt, d: BigInt): ByteVector =
    hex"d81e82" ++ CanonicalGens.integerBytes(n) ++ CanonicalGens.integerBytes(d)

  /**
   * Values with a CBOR form whose JSON form would break the bounds parsing enforces, so rendering refuses them: 10^10000
   * (10,001 digits), a scale of 10,001, a significand of 10,001 digits within the scale, and a denominator of the widest
   * a rational has (5^14306, 33,218 bits) whose scale is past the bound.
   */
  val renderRejections: List[CanonicalValue] = List(
    CanonicalGens.int(BigInt(10).pow(10000)),
    CanonicalGens.int(-IntEdge),
    rational(1, BigInt(1) << 10001),
    rational(BigInt(10).pow(10000) + 1, 2),
    rational(1, BigInt(5).pow(14306))
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
    hex"9bffffffffffffffff",
    hex"bb00000000ffffffff",
    hex"9800",
    hex"190017",
    hex"1c",
    hex"5f4100ff",
    hex"e0",
    hex"f818",
    hex"c248ffffffffffffffff",
    hex"c34101",
    hex"c201",
    hex"d81e01",
    hex"d81e83010203",
    hex"d81e820100",
    hex"d81e82c24901000000000000000001",
    hex"6261",
    hex"a1",
    hex"a2616101",
    hex"f0",
    // a 24-byte key before a shorter one
    hex"a2781861616161616161616161616161616161616161616161616101616201",
    // 257 levels: 256 arrays around an empty array, and 257 arrays around 0
    ByteVector.fill(256)(0x81) ++ hex"80",
    ByteVector.fill(256)(0x81) ++ hex"a0",
    ByteVector.fill(257)(0x81) ++ hex"00",
    // a rational part one bit wider than CanonicalValue.MaxRationalBits, for either sign and either part, is outside the data
    // model: refused before any gcd, so before lowest terms (the first), before the denominator's range (the fifth) and
    // before the denominator is read (the sixth)
    rationalBytes(Edge * 2, Edge + 2),
    rationalBytes(Edge, 3),
    rationalBytes(-Edge - 1, 2),
    rationalBytes(3, Edge),
    rationalBytes(1, -Edge * 2),
    hex"d81e82" ++ CanonicalGens.integerBytes(Edge) ++ hex"6161",
    rationalBytes(1, BigInt(5).pow(14400)),
    // a numerator whose bignum has a leading zero byte and 4154 bytes in all: its form is checked before its width, so
    // it is non-canonical, not too wide
    hex"d81e82c259103a00" ++ ByteVector.fill(4153)(0xff) ++ hex"03",
    // an integer one bit wider than CanonicalValue.MaxIntegerBits, for either sign: refused from its length (4153 bytes,
    // the first 0x10), before it is built; and one a byte wider still
    CanonicalGens.integerBytes(IntEdge),
    CanonicalGens.integerBytes(-IntEdge - 1),
    CanonicalGens.integerBytes(IntEdge * 255),
    // a bignum's form, and that it is present in full, are checked before its width: 4154 bytes with a leading zero is
    // non-canonical, and a 2^32 - 1-byte bignum with 4 bytes present is truncated
    hex"c259103a00" ++ ByteVector.fill(4153)(0xff),
    hex"c35affffffff" ++ ByteVector.fill(4)(0xff),
    // the wrapped item's head is read before its kind: a float is Unsupported, a truncated head UnexpectedEnd
    hex"c2f90000",
    hex"c279"
  )

  private val tagMessages: List[ByteVector] = List(ByteVector.empty, ByteVector("abc".getBytes(UTF_8)), hex"00ff00ff")

  /** Longer than every block size (SHA-512 and BLAKE2b 128 bytes, SHA3-256 and Keccak-256 136). */
  private val digestMessages: List[ByteVector] =
    List(ByteVector.empty, ByteVector("abc".getBytes(UTF_8)), ByteVector((0 until 200).map(_.toByte)))

  private val digesters: List[(String, ByteVector => ByteVector)] = List(
    "SHA-256"     -> (Hasher[Sha256].digest(_).bytes),
    "SHA-512"     -> (Hasher[Sha512].digest(_).bytes),
    "SHA3-256"    -> (Hasher[Sha3_256].digest(_).bytes),
    "SHA3-512"    -> (Hasher[Sha3_512].digest(_).bytes),
    "BLAKE2b-256" -> (Hasher[Blake2b256].digest(_).bytes),
    "BLAKE2b-512" -> (Hasher[Blake2b512].digest(_).bytes),
    "Keccak-256"  -> (Hasher[Keccak256].digest(_).bytes)
  )

  /** Candidate domain tags; each is checked against the grammar of section 4. */
  val domainTags: List[String] = List(
    tagName,
    "dilf4s/mpt/leaf/v1",
    "a/v0",
    "x1/a.b_c-d/v12",
    "a/0/v99",
    "v1",
    "",
    "/v1",
    "a/v",
    "a/v1x",
    "a/V1",
    "Dilf4s/a/v1",
    "dilf4s//v1",
    "dilf4s/.a/v1",
    "dilf4s/a/v1/",
    "dilf4s/a b/v1",
    "dilf4s/a/v1\n",
    "dilf4s/é/v1"
  )

  private def errorKind(e: Product): VectorJson = str(e.productPrefix)

  def build: VectorJson = obj(
    "spec"                 -> str("docs/specs/canonical-encoding.md"),
    "version"              -> num(1),
    "contentHashTag"       -> str(tagName),
    "contentHashAlgorithm" -> str("SHA-256"),
    "json" -> arr(jsonInputs.map { in =>
      CanonicalJson.parse(in) match {
        case Right(v) =>
          val bytes = Cbor.encode(v)
          obj(
            "json"        -> str(in),
            "cbor"        -> str(bytes.toHex),
            "contentHash" -> str(sha256(bytes).hex),
            CanonicalJson.render(v).fold(e => "renderError" -> errorKind(e), r => "render" -> str(r))
          )
        case Left(e) => obj("json" -> str(in), "error" -> errorKind(e))
      }
    }),
    "model" -> arr(modelInputs.map {
      case (name, v) =>
        val bytes = Cbor.encode(v)
        obj("value" -> str(name), "cbor" -> str(bytes.toHex), "contentHash" -> str(sha256(bytes).hex))
    }),
    "renderRejections" -> arr(renderRejections.map { v =>
      obj("cbor" -> str(Cbor.encode(v).toHex), "error" -> CanonicalJson.render(v).fold(errorKind, r => str(s"RENDERED $r")))
    }),
    "cborRejections" -> arr(cborRejections.map { b =>
      obj("cbor" -> str(b.toHex), "error" -> Cbor.decode(b).fold(errorKind, v => str(s"ACCEPTED $v")))
    }),
    "taggedHash" -> arr(for {
      m <- tagMessages
      (name, digest) <- List[(String, ByteVector => Digest[?])](
        "SHA-256"     -> TaggedHash[Sha256](tag).apply,
        "SHA-512"     -> TaggedHash[Sha512](tag).apply,
        "SHA3-256"    -> TaggedHash[Sha3_256](tag).apply,
        "SHA3-512"    -> TaggedHash[Sha3_512](tag).apply,
        "Keccak-256"  -> TaggedHash[Keccak256](tag).apply,
        "BLAKE2b-256" -> TaggedHash[Blake2b256](tag).apply,
        "BLAKE2b-512" -> TaggedHash[Blake2b512](tag).apply
      )
    } yield obj("algorithm" -> str(name), "tag" -> str(tagName), "message" -> str(m.toHex), "digest" -> str(digest(m).hex))),
    "digests" -> arr(for {
      m              <- digestMessages
      (name, digest) <- digesters
    } yield obj("algorithm" -> str(name), "message" -> str(m.toHex), "digest" -> str(digest(m).toHex))),
    "domainTags" -> arr(domainTags.map(t => obj("tag" -> str(t), "valid" -> bool(DomainTag(t).isRight))))
  )

  def render: String = VectorJson.pretty(build) + "\n"
}

/** `sbt "core/Test/runMain xyz.kd5ujc.core.vectors.CanonicalVectorGen"` regenerates the committed vectors. */
object CanonicalVectorGen extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    IO {
      Files.createDirectories(CanonicalVectors.path.getParent)
      Files.write(CanonicalVectors.path, CanonicalVectors.render.getBytes(UTF_8))
    }.as(ExitCode.Success)
}
