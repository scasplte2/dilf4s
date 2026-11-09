package xyz.kd5ujc.core.vectors

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}

import xyz.kd5ujc.core.canonical.{CanonicalError, CanonicalGens, CanonicalJson, CanonicalValue, Cbor}
import xyz.kd5ujc.core.hash._
import xyz.kd5ujc.core.vectors.VectorJson.{arr, num, obj, str}

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
    "1" * 10000,
    // a scale of 10,000: within the rational bound only when the denominator loses a factor (2/10^10000 = 1/(2^9999 5^10000))
    "0." + "0" * 10000 + "2e1",
    "1e0000000002",
    // integers: 33219 bits is the bound (CanonicalValue.MaxIntegerBits), for either sign, and every integer within it
    // has at most 10,000 digits, so it renders: 2^33219 - 1 and -2^33219 are the widest, 8e9999 is within it
    (IntEdge - 1).toString,
    "-" + IntEdge.toString,
    "8e9999",
    "-8e9999",
    // depth: 256 nested arrays is the limit, and an empty array or object is a level
    "[" * 255 + "[]" + "]" * 255,
    "[" * 255 + "{}" + "]" * 255,
    // JSON's four whitespace characters, around and between every token
    " \t\n\r[ \t\n\r1 \t\n\r, \t\n\r{ \t\n\r\"a\" \t\n\r: \t\n\r2 \t\n\r} \t\n\r] \t\n\r",
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
    // one bit past the integer bound, for either sign, within the digit and scale bounds; 10,000 nines; 10^10000; a
    // 10,000-digit significand times 10^10000
    IntEdge.toString,
    (-IntEdge - 1).toString,
    "9" * 10000,
    "9e9999",
    "-9e9999",
    "1e10000",
    "-1e10000",
    "9" * 10000 + "e10000",
    // one bit past the rational bound: 1/10^10000, (10^10000 - 1)/10^10000
    "0." + "0" * 9999 + "1",
    "0." + "0" * 10000 + "1e1",
    "0." + "9" * 10000,
    "[" * 256 + "[]" + "]" * 256,
    // an empty container is refused at its bracket, so before what follows it is read
    "[" * 256 + "{}" + "]" * 256,
    "[" * 256 + "{}",
    // refusal order (canonical-encoding.md section 3): the first fault in reading order, but a member name's unpaired
    // surrogate is refused when its object closes, after its values and the check for a repeat
    "{\"\\ud800\":1,\"\\ud800\":2}",
    "{\"\\udc00\":01}",
    "{\"\\udc00\":1",
    "{\"\\udc00\":" + "[" * 300 + "]" * 300 + "}",
    "{\"\\udc00\":\"\\ud800\"}",
    "{\"a\":{\"\\udc00\":1},\"a\":2}",
    "{\"\\udc00\":1,\"a\":1,\"a\":2}",
    "{\"a\":1,\"\\u0061\":2}",
    "{\"\\ud83d\\ude00\":1,\"\ud83d\ude00\":2}",
    // a byte-order mark is not whitespace, nor are the other Unicode spaces and JSON's excluded controls
    "\ufeff1",
    "\ufeff{}",
    "\u00a01",
    "[1,\u00a02]",
    "\u000b1",
    "1\u000c",
    "[1,\u000b2]",
    "{\"a\"\u000c:1}",
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
  ) ++ exponentBudget

  /**
   * The exponent budget (canonical-encoding.md section 3), 2^24: 1677 numbers of exponent 9999 and one of 8893 spend it
   * exactly. The number that passes it is InvalidNumber before it is converted, even one too wide, and nothing after it
   * is read; at it, the text goes on to its next fault. A text at the budget is ~7 MB of value, so no row accepts one.
   */
  def exponentBudget: List[String] = {
    val full = List.fill(1677)("8e9999") :+ "8e8893"
    List(
      List.fill(1678)("8e9999").mkString("[", ",", "]"),
      (full :+ "1e1").mkString("[", ",", "]"),
      full.mkString("[", ",", ",]"),
      (full :+ "9e9999").mkString("[", ",", "]"),
      List.fill(1678)("1e-9999").mkString("[", ",", "]"),
      List.fill(1678)("8e9999").mkString("[", ",", ",\"x")
    )
  }

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
    // the widest parts a rational has (CanonicalValue.MaxRationalBits), for either sign; the last pair
    // is consecutive Fibonacci numbers, coprime and the slowest gcd of its width
    s"rational((2^$RationalBits-1)/2)"                     -> rational(Edge - 1, 2),
    s"rational(-2^$RationalBits/3)"                        -> rational(-Edge, 3),
    s"rational(2/(2^$RationalBits-1))"                     -> rational(2, Edge - 1),
    s"rational(F(${fibonacci._3})/F(${fibonacci._3 - 1}))" -> rational(fibonacci._1, fibonacci._2),
    // the widest integers (CanonicalValue.MaxIntegerBits), for either sign: 4153 bytes of magnitude
    s"int(2^$IntegerBits-1)" -> CanonicalGens.int(IntEdge - 1),
    s"int(-2^$IntegerBits)"  -> CanonicalGens.int(-IntEdge)
  )

  // defs: modelInputs and jsonInputs are built before vals below them would be set
  private def IntegerBits: Int = CanonicalValue.MaxIntegerBits
  private def RationalBits: Int = CanonicalValue.MaxRationalBits

  /** `2^MaxIntegerBits`: one bit wider than an integer may be. */
  private def IntEdge: BigInt = BigInt(1) << IntegerBits

  private def rational(n: BigInt, d: BigInt): CanonicalValue =
    CanonicalValue
      .number(xyz.kd5ujc.numerics.Ratio.of(n, d).fold(e => sys.error(e.toString), identity))
      .fold(e => sys.error(e.toString), identity)

  /** `2^MaxRationalBits`: one bit wider than a rational's part may be. */
  private lazy val Edge: BigInt = BigInt(1) << CanonicalValue.MaxRationalBits

  /** The consecutive Fibonacci numbers `(F(k + 1), F(k))` with `F(k + 1)` exactly `MaxRationalBits` wide, and `k + 1`. */
  private lazy val fibonacci: (BigInt, BigInt, Int) = {
    @scala.annotation.tailrec
    def go(a: BigInt, b: BigInt, k: Int): (BigInt, BigInt, Int) =
      if (b.bitLength >= CanonicalValue.MaxRationalBits) (b, a, k) else go(b, a + b, k + 1)
    go(BigInt(1), BigInt(1), 2)
  }

  /** Tag 30 around `[n, d]`, whatever `n` and `d` are. */
  private def rationalBytes(n: BigInt, d: BigInt): ByteVector =
    hex"d81e82" ++ CanonicalGens.integerBytes(n) ++ CanonicalGens.integerBytes(d)

  /**
   * Values with a CBOR form whose JSON form would break the bounds parsing enforces, so rendering refuses them: only
   * rationals, since every integer renders: a scale of 10,001, a significand of 10,001 digits within the scale (the
   * widest numerator over 2), and a denominator of the widest a rational has (5^14306, 33,218 bits) whose scale is past
   * the bound.
   */
  val renderRejections: List[CanonicalValue] = List(
    rational(1, BigInt(1) << 10001),
    rational(Edge - 1, 2),
    rational(1, BigInt(5).pow(14306))
  )

  /**
   * Budgeted calls (canonical-encoding.md §2, §3): `Cbor.encodeWithin` ("cbor") or `CanonicalJson.renderWithin`
   * ("json") of a value within a limit, each at the output's length and one byte under it, refused from the weight or
   * as measured; and JSON's first fault in writing order, a part past the limit or what JSON cannot write.
   */
  val limitCases: List[(String, String, CanonicalValue, List[Long])] = {
    def at(encoding: String, v: CanonicalValue): List[Long] = {
      val length =
        if (encoding == "cbor") CanonicalGens.encoded(v).size
        else CanonicalJson.render(v).fold(e => sys.error(s"$v does not render: $e"), _.getBytes(UTF_8).length.toLong)
      List(length, length - 1)
    }
    val abc = text("abc")
    val nested = parsed("""{"b":[1,0.5,true,null,"é"],"a":{"k":-18446744073709551617}}""")
    val escaped = text("a\"\\\n\u0001é😀")
    val withBytes = array(CanonicalValue.CInt(1), CanonicalValue.CBytes(hex"01"))
    List(
      ("cbor: a text, its weight past one byte under its length", "cbor", abc, at("cbor", abc)),
      (
        "cbor: 2^32 - 1, weight 1 in 5 bytes, measured past one byte under",
        "cbor",
        CanonicalValue.CInt((1L << 32) - 1),
        at("cbor", CanonicalValue.CInt((1L << 32) - 1))
      ),
      ("cbor: a nested value", "cbor", nested, at("cbor", nested)),
      ("cbor: null within 0 bytes", "cbor", CanonicalValue.CNull, List(0L)),
      (
        "cbor: bytes, within their length and one under",
        "cbor",
        CanonicalValue.CBytes(hex"0102030405"),
        at("cbor", CanonicalValue.CBytes(hex"0102030405"))
      ),
      ("json: a text, measured past one byte under its length", "json", abc, at("json", abc)),
      ("json: an escaped text, longer than its weight", "json", escaped, at("json", escaped)),
      ("json: a nested value, members in code-point order", "json", nested, at("json", nested)),
      ("json: bytes after a part that fits: TooLarge within 4, NotJsonRepresentable within 5", "json", withBytes, List(4L, 5L)),
      ("json: a non-terminating rational: TooLarge from its weight, else NotJsonRepresentable", "json", rational(1, 3), List(1L, 1000L))
    )
  }

  /** A budgeted call's outcome: its output, or its refusal by kind and, for `TooLarge`, its limit. */
  private def limitCase(name: String, encoding: String, v: CanonicalValue, maxBytes: Long): VectorJson = {
    val result: Either[List[(String, VectorJson)], String] =
      if (encoding == "cbor") Cbor.encodeWithin(v, maxBytes).left.map(e => tooLarge(e.limit)).map(_.toHex)
      else
        CanonicalJson.renderWithin(v, maxBytes).left.map {
          case CanonicalJson.RenderError.TooLarge(limit) => tooLarge(limit)
          case CanonicalJson.RenderError.Canonical(e)    => List("kind" -> errorKind(e))
        }
    obj(
      List(
        "name"     -> str(name),
        "encoding" -> str(encoding),
        "value"    -> str(CanonicalGens.encoded(v).toHex),
        "maxBytes" -> num(maxBytes),
        "outcome"  -> result.fold(fields => obj(fields*), _ => str("accepted"))
      ) ++ result.toOption.map(o => "output" -> str(o)).toList*
    )
  }

  private def tooLarge(limit: Long): List[(String, VectorJson)] = List("kind" -> str("TooLarge"), "limit" -> num(limit))

  private def text(s:      String): CanonicalValue = CanonicalValue.text(s).fold(e => sys.error(e.toString), identity)
  private def parsed(json: String): CanonicalValue = CanonicalJson.parse(json).fold(e => sys.error(e.toString), identity)
  private def array(vs:    CanonicalValue*): CanonicalValue = CanonicalValue.array(vs.toVector).fold(e => sys.error(e.toString), identity)

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
    // a container's count is checked against the bytes left before the depth limit: truncated, not too deep
    ByteVector.fill(256)(0x81) ++ hex"9bffffffffffffffff",
    ByteVector.fill(256)(0x81) ++ hex"81",
    // a map key is read whole before it is checked as text: a key nested too deep is DepthExceeded, not Unsupported
    hex"a1" ++ ByteVector.fill(256)(0x81) ++ hex"80",
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
    // the first 0x08), before it is built; and one a byte wider still
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

  /** Candidate hash function names: the seven of section 4, then near misses of each kind. */
  val algorithmNames: List[String] = Hasher.all.map(_.name).toList ++ List(
    "sha-256",
    "SHA256",
    "SHA-1",
    "SHA3-384",
    "Keccak256",
    "keccak-256",
    "BLAKE2b",
    "BLAKE2B-256",
    "",
    " SHA-256",
    "SHA-256 ",
    "SHA-256\u0000",
    "SHA‐256",
    "é"
  )

  private val sha256Hex = Hasher[Sha256].digest(ByteVector("abc".getBytes(UTF_8))).hex
  private val sha512Hex = Hasher[Sha512].digest(ByteVector("abc".getBytes(UTF_8))).hex
  private val blake2bHex = Hasher[Blake2b256].digest(ByteVector("abc".getBytes(UTF_8))).hex

  /**
   * Digest hex, by algorithm (section 4): an optional `0x` and exactly the width's ASCII hex digits, either case.
   * Refusals: the width's digits and one more are read before the length is (conformance.md section 8), so a non-ASCII
   * character is refused at the same offset in every language's string units.
   */
  val digestHex: List[(String, String)] = {
    val d = sha256Hex
    List(
      "SHA-256"     -> d,
      "SHA-256"     -> d.toUpperCase,
      "SHA-256"     -> ("0x" + d),
      "SHA-256"     -> ("0x" + d.toUpperCase),
      "SHA-512"     -> sha512Hex,
      "BLAKE2b-256" -> blake2bHex,
      "Keccak-256"  -> d,
      // the prefix is "0x" alone, read once, and counted in an offset
      "SHA-256" -> ("0X" + d),
      "SHA-256" -> ("0x0x" + d.drop(4)),
      "SHA-256" -> ("x" + d.drop(1)),
      // lengths: empty, short, odd (the missing digit's offset), long by one digit and by one byte, twice the width
      "SHA-256"     -> "",
      "SHA-256"     -> "0x",
      "SHA-256"     -> d.dropRight(2),
      "SHA-256"     -> d.dropRight(1),
      "SHA-256"     -> ("0x" + d.dropRight(1)),
      "SHA-256"     -> (d + "0"),
      "SHA-256"     -> (d + "00"),
      "SHA-256"     -> ("0x" + d + "0"),
      "SHA-256"     -> (d * 2),
      "SHA-512"     -> d,
      "BLAKE2b-256" -> sha512Hex,
      // characters that are not ASCII hex digits: letters past f, space, underscore, a fullwidth digit, non-ASCII
      // text whose UTF-8 is longer than the width while its UTF-16 is not, and a non-digit one past the width
      "SHA-256" -> ("zz" + d.drop(2)),
      "SHA-256" -> (d.take(10) + "g" + d.drop(11)),
      "SHA-256" -> (d.take(2) + " " + d.drop(3)),
      "SHA-256" -> (d.take(1) + "_" + d.drop(2)),
      "SHA-256" -> ("１" + d.drop(1)),
      "SHA-256" -> ("é" + d.drop(1)),
      "SHA-256" -> (d.take(62) + "é"),
      "SHA-256" -> ("é" * 33),
      "SHA-256" -> ("é" * 40),
      "SHA-256" -> ("😀" * 20),
      "SHA-256" -> (d + "é"),
      "SHA-256" -> (d + "g" + d),
      // the reach is one digit past the width, not the whole text: a non-digit past it is not read, so the text is
      // refused by its length; a non-digit within it, after a prefix, is refused at its offset counting the prefix
      "SHA-256" -> (d + "0g"),
      "SHA-256" -> (d + "000g"),
      "SHA-256" -> ("0x" + d + "0g"),
      "SHA-256" -> ("0x" + d + "g"),
      // a non-digit before an odd length is refused at its offset, in every string unit (ASCII)
      "SHA-256" -> (d.take(62) + "g")
    )
  }

  private def ascii(s: String): ByteVector = ByteVector(s.getBytes(UTF_8))
  private val quux256 = ascii("QUUX-V01-CS02-with-expander-SHA256-128")
  private val quux512 = ascii("QUUX-V01-CS02-with-expander-SHA512-256")
  private val expandTag = ascii("dilf4s/vectors/expand/v1")

  /**
   * `expand_message_xmd` (RFC 9380 section 5.3.1): (algorithm, message, DST, length). RFC 9380's own K.1 and K.3
   * cases, every algorithm, DSTs at and past 255 bytes, the longest output; refusals in their order.
   */
  val expansions: List[(String, ByteVector, ByteVector, Long)] = List(
    ("SHA-256", ByteVector.empty, quux256, 32L),
    ("SHA-256", ascii("abc"), quux256, 32L),
    ("SHA-256", ascii("abcdef0123456789"), quux256, 32L),
    ("SHA-256", ByteVector.empty, quux256, 128L),
    ("SHA-512", ByteVector.empty, quux512, 32L),
    ("SHA-512", ascii("abc"), quux512, 128L),
    ("SHA3-256", ascii("abc"), expandTag, 48L),
    ("SHA3-512", ascii("abc"), expandTag, 48L),
    ("Keccak-256", ascii("abc"), expandTag, 48L),
    ("BLAKE2b-256", ascii("abc"), expandTag, 48L),
    ("BLAKE2b-512", ascii("abc"), expandTag, 48L),
    ("SHA-256", ascii("abc"), ByteVector.fill(255)(0x41), 32L),
    ("SHA-256", ascii("abc"), ByteVector.fill(256)(0x41), 32L),
    ("SHA-512", ascii("abc"), ByteVector.fill(300)(0x42), 64L),
    ("SHA-256", ascii("abc"), ByteVector(1), 1L),
    ("SHA-256", ByteVector.empty, expandTag, 8160L),
    ("SHA-256", ByteVector.empty, expandTag, 8161L),
    ("SHA-256", ByteVector.empty, expandTag, 0L),
    ("SHA-512", ByteVector.empty, expandTag, 16321L),
    ("SHA3-512", ByteVector.empty, expandTag, 65536L),
    ("SHA-256", ByteVector.empty, ByteVector.fill(256)(0x41), 4294967296L),
    // lengths checked before any narrowing: 2^16 + 32 and 2^32 + 32 are 32 in 16 or 32 bits, which would be accepted
    ("SHA-256", ascii("abc"), expandTag, 65568L),
    ("SHA-256", ascii("abc"), expandTag, 4294967328L),
    ("SHA-256", ascii("abc"), ByteVector.empty, 32L),
    ("SHA-256", ascii("abc"), ByteVector.empty, 0L),
    ("SHA-512", ascii("abc"), ByteVector.empty, 99999L)
  )

  private def hashOutcome(r: Either[HashError, ?]): List[(String, VectorJson)] =
    "outcome" -> r.fold(e => str(e.kind), _ => str("accepted")) :: (r match {
      case Left(HashError.InvalidHex(offset))              => List("offset" -> num(offset))
      case Left(HashError.InvalidExpansion(refusal, _, _)) => List("refusal" -> str(refusal))
      case _                                               => Nil
    })

  private def fromHex[H](h: Hasher[H], text: String): Either[HashError, ByteVector] = Digest.fromHex[H](text)(using h).map(_.bytes)

  private def expand[H](h: Hasher[H], msg: ByteVector, dst: ByteVector, length: Long): Either[HashError, ByteVector] =
    ExpandMessage.xmd[H](msg, dst, math.min(length, Int.MaxValue.toLong).toInt)(using h)

  private def errorKind(e: Product): VectorJson = str(e.productPrefix)

  /** A refusal's fields: its kind, and its normative `limit` when it has one (conformance.md §3). */
  private def refusal(e: CanonicalError): List[(String, VectorJson)] =
    ("error" -> errorKind(e)) :: (e match {
      case CanonicalError.RationalTooWide(limit) => List("limit" -> num(limit))
      case CanonicalError.IntegerTooWide(limit)  => List("limit" -> num(limit))
      case _                                     => Nil
    })

  def build: VectorJson = obj(
    "spec"                 -> str("docs/specs/canonical-encoding.md"),
    "version"              -> num(1),
    "contentHashTag"       -> str(tagName),
    "contentHashAlgorithm" -> str("SHA-256"),
    "json" -> arr(jsonInputs.map { in =>
      CanonicalJson.parse(in) match {
        case Right(v) =>
          val bytes = xyz.kd5ujc.core.canonical.CanonicalGens.encoded(v)
          obj(
            "json"        -> str(in),
            "cbor"        -> str(bytes.toHex),
            "contentHash" -> str(sha256(bytes).hex),
            // every value JSON reads renders and reads back (canonical-encoding.md section 3)
            "render" -> str(CanonicalJson.render(v).fold(e => sys.error(s"$in reads but does not render: $e"), identity))
          )
        case Left(e) => obj(("json" -> str(in)) :: refusal(e)*)
      }
    }),
    "model" -> arr(modelInputs.map {
      case (name, v) =>
        val bytes = xyz.kd5ujc.core.canonical.CanonicalGens.encoded(v)
        obj("value" -> str(name), "cbor" -> str(bytes.toHex), "contentHash" -> str(sha256(bytes).hex))
    }),
    "renderRejections" -> arr(renderRejections.map { v =>
      CanonicalJson.render(v) match {
        case Left(CanonicalJson.RenderError.Canonical(e)) =>
          obj(("cbor" -> str(xyz.kd5ujc.core.canonical.CanonicalGens.encoded(v).toHex)) :: refusal(e)*)
        case other => sys.error(s"$v renders as $other")
      }
    }),
    "limits" -> arr(limitCases.flatMap { case (name, encoding, v, limits) => limits.map(limitCase(name, encoding, v, _)) }),
    "cborRejections" -> arr(cborRejections.map { b =>
      Cbor.decode(b).fold(e => obj(("cbor" -> str(b.toHex)) :: refusal(e)*), v => sys.error(s"${b.toHex} decodes as $v"))
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
    "domainTags" -> arr(domainTags.map(t => obj(("tag" -> str(t)) :: hashOutcome(DomainTag(t))*))),
    "algorithms" -> arr(algorithmNames.map(n => obj(("name" -> str(n)) :: hashOutcome(Hasher.named(n))*))),
    "digestHex" -> arr(digestHex.map {
      case (name, text) =>
        obj(("algorithm" -> str(name)) :: ("hex" -> str(text)) :: hashOutcome(Hasher.named(name).flatMap(fromHex(_, text)))*)
    }),
    "expansions" -> arr(expansions.map {
      case (name, msg, dst, length) =>
        val r = Hasher.named(name).flatMap(expand(_, msg, dst, length))
        obj(
          List(
            "algorithm" -> str(name),
            "message"   -> str(msg.toHex),
            "dst"       -> str(dst.toHex),
            "length"    -> num(length)
          ) ++ hashOutcome(r) ++ r.toOption.map(o => "output" -> str(o.toHex)).toList*
        )
    })
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
