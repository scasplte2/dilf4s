package xyz.kd5ujc.benchmarks

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.effect.std.SecureRandom
import cats.syntax.all._

import xyz.kd5ujc.accumulators.ValueCommitment
import xyz.kd5ujc.accumulators.merkle.{MerkleCodec, MerkleTree}
import xyz.kd5ujc.accumulators.trie.{KeyMode, Trie, TrieHashing, TrieProofs}
import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.hash.{Hasher, Sha256, Sha512}
import xyz.kd5ujc.crypto.bls.Bls12381
import xyz.kd5ujc.crypto.sigma.{Secp256k1Group, SigmaProp, SigmaProver}
import xyz.kd5ujc.crypto.signing._
import xyz.kd5ujc.crypto.vrf.{EcVrfTai, Vrf}
import xyz.kd5ujc.jlvm._

import cafe.cryptography.curve25519.CompressedEdwardsY
import io.circe.syntax._
import io.circe.{Json, parser}
import scodec.bits.ByteVector

/**
 * The cases GasRatios times: for each operation, a program and a baseline that evaluates the same argument reads, at
 * sizes that separate a base charge from a per-unit one, plus adversarial shapes (keys sharing long prefixes, long
 * trie keys, ground VRF counters). Each case is plain text (program, baseline, data in the JSON surface), so the Rust
 * harness (`sdk/rust/jlvm/examples/gas_ratios.rs`) times exactly the same inputs.
 */
object GasCases {

  /**
   * `adjust` is added to `gasUsed(program) - gasUsed(baseline)` to give the gas the schedule charges for the work being
   * timed: for an operation over argument reads, the baseline's array expression (`literal + element × n`).
   */
  final case class GasCase(name: String, group: String, program: String, baseline: String, data: String, adjust: Long) {
    def line: String = List(name, group, adjust.toString, program, baseline, data).mkString("\t")
  }

  val AnchorName: String = "anchor: secp256k1_verify, 18-byte message"

  /** The second reference, for the cross-check: `sha256` of 1024 bytes, hashing rather than curve arithmetic. */
  val ReferenceName: String = "reference: sha256, 1024 bytes"

  private val schedule = GasSchedule.V1
  private def get[A](e: Either[?, A]): A = e.fold(err => sys.error(err.toString), identity)
  private def t(s:      String): CanonicalValue = get(CanonicalValue.text(s))
  private def a(vs:     Vector[CanonicalValue]): CanonicalValue = get(CanonicalValue.array(vs))
  private def m(kvs:    (String, CanonicalValue)*): CanonicalValue = get(CanonicalValue.map(kvs.toVector))
  private def int(n:    BigInt): CanonicalValue = CInt(n)
  private def b(bs:     ByteVector): CanonicalValue = CBytes(bs)
  private def render(v: CanonicalValue): String = get(JsonSurface.render(v))

  private def reads(n: Int): Vector[Json] = Vector.tabulate(n)(i => Json.obj("var" -> s"a$i".asJson))

  /** `{op: [a0, a1, ...]}` over its arguments as data, against `[a0, a1, ...]`. */
  def op(name: String, o: Op, args: CanonicalValue*): GasCase =
    GasCase(
      s"${o.wire}: $name",
      o.wire,
      Json.obj(o.wire -> Json.fromValues(reads(args.size))).noSpaces,
      Json.fromValues(reads(args.size)).noSpaces,
      render(m(args.zipWithIndex.map { case (v, i) => s"a$i" -> v }*)),
      schedule.literal.value + schedule.element.value * args.size
    )

  /** A program against a baseline, both given as text; the charge timed is the difference in gas used. */
  def pair(name: String, group: String, program: String, baseline: String, data: CanonicalValue, adjust: Long = 0L): GasCase =
    GasCase(name, group, program, baseline, render(data), adjust)

  // ---------------------------------------------------------------- values

  /** A `bits`-bit number with no structure, derived from `seed` by hashing. */
  private def word(bits: Int, seed: Int): BigInt = {
    val stream = (0 to bits / 256).foldLeft(ByteVector.empty)((acc, i) =>
      acc ++ Hasher[Sha256].digest(ByteVector.fromInt(seed) ++ ByteVector.fromInt(i)).bytes
    )
    (BigInt(1, stream.toArray) >> (stream.size.toInt * 8 - bits)).setBit(bits - 1)
  }
  private def wint(bits: Int, seed: Int): CanonicalValue = CInt(word(bits, seed))
  private def ints(n:    Int): CanonicalValue = a(Vector.tabulate(n)(i => CInt(BigInt(i))))
  private def texts(n:   Int): CanonicalValue = a(Vector.tabulate(n)(i => t(s"t$i")))
  private def map(n:     Int): CanonicalValue = get(CanonicalValue.map(Vector.tabulate(n)(i => s"k$i" -> CInt(BigInt(i)))))
  private def chars(n:   Int): String = ("ab c" * (n / 4 + 1)).take(n)
  private def text(n:    Int): CanonicalValue = t(chars(n))
  private def bytes(n:   Int): CanonicalValue = CBytes(ByteVector.fill(n.toLong)(0x5a))

  /** `n` keys sharing a prefix of `prefix` characters, then a 6-digit index: every comparison scans the prefix. */
  private def prefixed(prefix: Int, i: Int): String = "k" * prefix + f"$i%06d"
  private def prefixedMap(n: Int, prefix: Int): CanonicalValue =
    get(CanonicalValue.map(Vector.tabulate(n)(i => prefixed(prefix, i) -> CInt(BigInt(i)))))
  private def prefixedTexts(n: Int, prefix: Int): CanonicalValue = a(Vector.tabulate(n)(i => t(prefixed(prefix, n - 1 - i))))

  /** A BN254 scalar-field element: 250 bits, so below `r`. */
  private def fe(seed: Int): CanonicalValue = CBytes(ByteVector(word(250, seed).toByteArray).padLeft(32))
  private def fes(n:   Int): Vector[CanonicalValue] = Vector.tabulate(n)(i => fe(i + 1))

  // ---------------------------------------------------------------- the pure operations

  private def unary(o: Op, f: Int => CanonicalValue, sizes: Int*): List[GasCase] = sizes.toList.map(n => op(s"size $n", o, f(n)))
  private def binary(o: Op, f: Int => CanonicalValue, sizes: Int*): List[GasCase] =
    sizes.toList.map(n => op(s"size $n, twice", o, f(n), f(n)))
  private def numbers(o: Op, bits: Int*): List[GasCase] = bits.toList.map(n => op(s"$n-bit integers", o, wint(n, 1), wint(n, 2)))

  /** `{op: [a0/a1, a2/a3]}` against `[a0/a1, a2/a3]`: rationals with unstructured denominators, built in the program. */
  private def rationals(o: Op, bits: Int*): List[GasCase] =
    bits.toList.map { n =>
      val q = Vector(Json.obj("/" -> Json.fromValues(reads(4).take(2))), Json.obj("/" -> Json.fromValues(reads(4).drop(2))))
      pair(
        s"${o.wire}: $n-bit rationals",
        o.wire,
        Json.obj(o.wire -> Json.fromValues(q)).noSpaces,
        Json.fromValues(q).noSpaces,
        m("a0" -> wint(n, 1), "a1" -> CInt(word(n, 2).setBit(0)), "a2" -> wint(n, 3), "a3" -> CInt(word(n, 4).setBit(0))),
        schedule.literal.value + schedule.element.value * 2
      )
    }

  private val arithmetic: List[GasCase] =
    List(Op.Add, Op.Sub, Op.Div, Op.Mod, Op.Min, Op.Max, Op.Lt, Op.Le, Op.Gt, Op.Ge, Op.Eq, Op.Ne, Op.StrictEq, Op.StrictNe)
      .flatMap(numbers(_, 64, 4000)) ++
    numbers(Op.Mul, 64, 2000) ++
    List(Op.Add, Op.Sub, Op.Mul, Op.Div, Op.Mod, Op.Lt, Op.Min, Op.Max).flatMap(rationals(_, 256, 1000, 2000)) ++
    List(Op.Abs, Op.Round, Op.RoundHalfUp, Op.Floor, Op.Ceil).flatMap(o =>
      List(256, 2000).map { n =>
        val q = Json.obj("/" -> Json.fromValues(reads(2)))
        pair(
          s"${o.wire}: a $n-bit rational",
          o.wire,
          Json.obj(o.wire -> Json.arr(q)).noSpaces,
          Json.arr(q).noSpaces,
          m("a0" -> wint(n, 1), "a1" -> CInt(word(n, 2).setBit(0))),
          schedule.literal.value + schedule.element.value
        )
      }
    ) ++
    List(
      op("a 64-bit integer to the 2nd", Op.Pow, wint(64, 1), int(2)),
      op("a 64-bit integer to the 60th", Op.Pow, wint(64, 1), int(60)),
      op("a 64-bit integer to the -60th", Op.Pow, wint(64, 1), int(-60)),
      op("3 to the 999th", Op.Pow, int(3), int(999)),
      op("a 4-bit integer to the 999th", Op.Pow, int(15), int(999)),
      op("1.5 to the 999th", Op.Pow, get(CanonicalValue.number(get(xyz.kd5ujc.numerics.Ratio.of(BigInt(3), BigInt(2))))), int(999)),
      op(
        "a 4-bit rational to the -999th",
        Op.Pow,
        get(CanonicalValue.number(get(xyz.kd5ujc.numerics.Ratio.of(BigInt(3), BigInt(2))))),
        int(-999)
      )
    ) ++
    List(Op.Eq, Op.StrictEq, Op.Lt).flatMap(o =>
      List(op("equal 64,000-character texts", o, text(64000), t(new String(chars(64000).toCharArray)))) ++
      Option.when(o != Op.Lt)(op("equal 8000-int arrays", o, ints(8000), ints(8000))).toList
    )

  /** Variadic arithmetic over a spread: every element is an operand, converted, combined or compared. */
  private lazy val spreads: List[GasCase] = {
    def rat(n: BigInt, d: BigInt) = get(CanonicalValue.number(get(xyz.kd5ujc.numerics.Ratio.of(n, d))))
    val operands = List(
      "10,000 ones"             -> Vector.fill(10000)(int(1)),
      "10,000 64-bit integers"  -> Vector.tabulate(10000)(i => wint(64, i + 1)),
      "1000 1000-bit rationals" -> Vector.tabulate(1000)(i => rat(word(1000, i + 1).setBit(0), BigInt(1) << 1000)),
      "5000 ones, 5000 halves"  -> Vector.tabulate(10000)(i => if (i % 2 == 0) int(1) else rat(BigInt(1), BigInt(2)))
    )
    for {
      o          <- List(Op.Add, Op.Mul, Op.Min, Op.Max)
      (name, xs) <- operands
      if o != Op.Mul || name == "10,000 ones" // any other product passes the number bound
    } yield
      pair(
        s"${o.wire}: a spread of $name",
        o.wire,
        Json.obj(o.wire -> Json.obj("var" -> "a0".asJson)).noSpaces,
        """{"var": "a0"}""",
        m("a0" -> a(xs))
      )
  } ++ List(Op.Cat, Op.Merge, Op.Exists, Op.TypeOf).map { o =>
    // arguments with no entries and no size: each is still an argument the operation takes and walks
    pair(
      s"${o.wire}: a spread of 10,000 empty arrays",
      o.wire,
      Json.obj(o.wire -> Json.obj("var" -> "a0".asJson)).noSpaces,
      """{"var": "a0"}""",
      m("a0" -> a(Vector.fill(10000)(a(Vector.empty))))
    )
  }

  private val logic: List[GasCase] =
    List(Op.Not, Op.Truthy, Op.TypeOf, Op.Exists).flatMap(unary(_, ints, 1, 8000)) ++
    List(
      pair("if: three conditions", "if", """{"if": [false, 1, false, 2, true, 3, 4]}""", """[false, 1, false, 2, true, 3, 4]""", CNull),
      pair("and: four values", "and", """{"and": [true, 1, 2, 3]}""", """[true, 1, 2, 3]""", CNull),
      pair("or: four values", "or", """{"or": [false, 0, null, 3]}""", """[false, 0, null, 3]""", CNull),
      pair("default: two values", "default", """{"default": [null, 1]}""", """[null, 1]""", CNull),
      pair("let: one binding", "let", """{"let": [[["x", 1]], {"var": "$x"}]}""", """[1, {"var": "a0"}]""", m("a0" -> int(1)))
    )

  private val textOps: List[GasCase] =
    List(Op.Length, Op.Lower, Op.Upper, Op.Trim).flatMap(unary(_, text, 16, 64000)) ++
    List(
      op("size 16", Op.Substr, text(16), int(2), int(8)),
      op("size 64000", Op.Substr, text(64000), int(2), int(32000)),
      op("size 16", Op.StartsWith, text(16), text(4)),
      op("size 64000", Op.StartsWith, text(64000), text(32000)),
      op("size 16", Op.EndsWith, text(16), text(4)),
      op("size 64000", Op.EndsWith, text(64000), t(chars(64000).takeRight(32000))),
      op("size 16", Op.Split, text(16), t(" ")),
      op("size 64000", Op.Split, text(64000), t(" ")),
      op("substring, size 16", Op.In, t("zz"), text(16)),
      op("substring, size 64000", Op.In, t("zz"), text(64000)),
      op("substring, a ^32000 b in a^64000 (KMP's worst)", Op.In, t("a" * 32000 + "b"), t("a" * 64000)),
      op("bytes, a 32,000-byte needle in 64,000", Op.In, CBytes(ByteVector.fill(32000)(1) :+ 2.toByte), CBytes(ByteVector.fill(64000)(1))),
      op("array membership, 4", Op.In, int(-1), ints(4)),
      op("array membership, 8000", Op.In, int(-1), ints(8000)),
      op("4 texts", Op.Cat, texts(4)),
      op("4000 texts", Op.Cat, texts(4000)),
      op("2 texts of 32,000", Op.Cat, text(32000), text(32000)),
      op("4 texts", Op.Join, texts(4), t(",")),
      op("8000 texts", Op.Join, texts(8000), t(",")),
      op("4 64-bit integers", Op.Cat, a(Vector.fill(4)(wint(64, 1)))),
      op("200 4000-bit integers", Op.Cat, a(Vector.fill(200)(wint(4000, 1))))
    ) ++ rendering

  /** Numbers `cat` and `join` render as decimal text: a spread, so every element is an argument. */
  private lazy val rendering: List[GasCase] = {
    def rat(n: BigInt, d: BigInt) = get(CanonicalValue.number(get(xyz.kd5ujc.numerics.Ratio.of(n, d))))
    val numbers = List(
      "20 64-bit integers"   -> Vector.fill(20)(wint(64, 1)),
      "20 4000-bit integers" -> Vector.fill(20)(wint(4000, 1)),
      "20 × 1/2^4000"        -> Vector.fill(20)(rat(BigInt(1), BigInt(1) << 4000)),
      "20 × 1/5^1700"        -> Vector.fill(20)(rat(BigInt(1), BigInt(5).pow(1700)))
    )
    numbers.flatMap {
      case (name, vs) =>
        List(
          pair(s"cat: $name", "cat", """{"cat": {"var": "a0"}}""", """{"var": "a0"}""", m("a0" -> a(vs))),
          pair(
            s"join: $name",
            "join",
            """{"join": [{"var": "a0"}, ","]}""",
            """[{"var": "a0"}, ","]""",
            m("a0" -> a(vs)),
            schedule.literal.value + schedule.element.value * 2
          )
        )
    }
  }

  private val arrays: List[GasCase] =
    List(Op.Unique, Op.Reverse, Op.Count, Op.Keys, Op.Values, Op.Entries).flatMap(o =>
      unary(o, if (Set(Op.Keys, Op.Values, Op.Entries)(o)) map else ints, 4, 8000)
    ) ++
    List(Op.Merge, Op.Intersect).flatMap(binary(_, ints, 4, 4000)) ++
    binary(Op.Merge, map, 4, 8000) ++
    List(
      op("size 4", Op.Slice, ints(4), int(1), int(3)),
      op("size 8000", Op.Slice, ints(8000), int(1), int(7000)),
      op("2 of 4", Op.Flatten, a(Vector.fill(2)(ints(4)))),
      op("2000 of 4", Op.Flatten, a(Vector.fill(2000)(ints(4)))),
      op("4096 texts with a 1-character prefix", Op.Unique, prefixedTexts(4096, 1)),
      op("4096 texts with a 1024-character prefix", Op.Unique, prefixedTexts(4096, 1024)),
      op("8 texts with a 1024-character prefix", Op.Unique, prefixedTexts(8, 1024)),
      op("4096 texts with a 1-character prefix", Op.Intersect, prefixedTexts(4096, 1), prefixedTexts(4096, 1)),
      op("4096 texts with a 1024-character prefix", Op.Intersect, prefixedTexts(4096, 1024), prefixedTexts(4096, 1024)),
      op("8 texts with a 1024-character prefix", Op.Intersect, prefixedTexts(8, 1024), prefixedTexts(8, 1024)),
      op("4096 keys with a 1024-character prefix", Op.Merge, prefixedMap(4096, 1024), prefixedMap(4096, 1024))
    )

  /** Callbacks over `n` elements, the body a literal: the per-element charge and `callbackRun`. */
  private val callbacks: List[GasCase] =
    List(Op.Map, Op.Filter, Op.All, Op.Some, Op.None, Op.Find).flatMap { o =>
      List(4, 4000).map(n =>
        pair(
          s"${o.wire}: $n elements, a literal body",
          o.wire,
          Json
            .obj(o.wire -> Json.arr(Json.obj("var" -> "a0".asJson), if (o == Op.Find || o == Op.None) false.asJson else true.asJson))
            .noSpaces,
          Json.arr(Json.obj("var" -> "a0".asJson), true.asJson).noSpaces,
          m("a0" -> ints(n)),
          schedule.literal.value + schedule.element.value * 2
        )
      )
    } ++ List(4, 4000).map(n =>
      pair(
        s"reduce: $n elements, a literal body",
        "reduce",
        """{"reduce": [{"var": "a0"}, 1, 0]}""",
        """[{"var": "a0"}, 1, 0]""",
        m("a0" -> ints(n)),
        schedule.literal.value + schedule.element.value * 3
      )
    )

  /** Map lookups and updates: maps of `n` keys sharing a `prefix`-character prefix, the key the first, last or absent. */
  private val maps: List[GasCase] = {
    val shapes = List((8, 1), (8000, 1), (8, 4096), (128, 4096), (1024, 4096), (8, 65536), (64, 65536))
    val big = {
      val n = 1 << 20
      val mp = prefixedMap(n, 1)
      def name(which: String) = s"$n keys, 1-character prefix, the $which"
      List(
        op(name("last"), Op.Get, mp, t(prefixed(1, n - 1))),
        op(name("absent"), Op.Set, mp, t("k" + "zzzzzz"), int(1)),
        op(name("last"), Op.Set, mp, t(prefixed(1, n - 1)), int(1)),
        op(name("last"), Op.Unset, mp, t(prefixed(1, n - 1)))
      )
    }
    big ++ shapes.flatMap {
      case (n, prefix) =>
        val mp = prefixedMap(n, prefix)
        val (first, last, absent) = (prefixed(prefix, 0), prefixed(prefix, n - 1), "k" * prefix + "zzzzzz")
        def name(which: String) = s"$n keys, $prefix-character prefix, the $which"
        List(
          op(name("first"), Op.Get, mp, t(first)),
          op(name("last"), Op.Get, mp, t(last)),
          op(name("last"), Op.Has, mp, t(last)),
          op(name("absent"), Op.Set, mp, t(absent), int(1)),
          op(name("last"), Op.Set, mp, t(last), int(1)),
          op(name("last"), Op.Unset, mp, t(last))
        )
    }
  }

  /** Reads of data: a path into a map of `n` keys with a long shared prefix, and `missing` on the same key. */
  private val lookups: List[GasCase] =
    List((8, 1), (8000, 1), (1 << 20, 1), (8, 4096), (1024, 4096), (64, 65536)).flatMap {
      case (n, prefix) if n > 8000 =>
        val data =
          get(CanonicalValue.map(Vector.tabulate(n)(i => prefixed(prefix, i) -> CInt(BigInt(i))) :+ ("p" -> t(prefixed(prefix, n - 1)))))
        List(
          pair(
            s"var: a static key, $n keys, $prefix-character prefix",
            "var",
            Json.obj("var" -> prefixed(prefix, n - 1).asJson).noSpaces,
            "1",
            data
          ),
          pair(
            s"missing: one key, $n keys, $prefix-character prefix",
            "missing",
            """{"missing": [{"var": "p"}]}""",
            """[{"var": "p"}]""",
            data
          )
        )
      case (n, prefix) =>
        val data =
          get(CanonicalValue.map(Vector.tabulate(n)(i => prefixed(prefix, i) -> CInt(BigInt(i))) :+ ("p" -> t(prefixed(prefix, n - 1)))))
        val key = prefixed(prefix, n - 1)
        List(
          pair(s"var: a static key, $n keys, $prefix-character prefix", "var", Json.obj("var" -> key.asJson).noSpaces, "1", data),
          pair(s"var: a computed key, $n keys, $prefix-character prefix", "var", """{"var": {"var": "p"}}""", """{"var": "p"}""", data),
          pair(
            s"missing: one key, $n keys, $prefix-character prefix",
            "missing",
            """{"missing": [{"var": "p"}]}""",
            """[{"var": "p"}]""",
            data
          ),
          pair(
            s"missing_some: one key, $n keys, $prefix-character prefix",
            "missing_some",
            """{"missing_some": [1, [{"var": "p"}]]}""",
            """[1, [{"var": "p"}]]""",
            data
          )
        )
    }

  /** Node charges: programs of growing size against a small one. */
  private val nodes: List[GasCase] = {
    def arr(n: Int, item: String) = List.fill(n)(item).mkString("[", ",", "]")
    val data = m("x" -> int(1), "xs" -> ints(1000), "deep" -> (1 to 32).foldLeft[CanonicalValue](int(1))((v, _) => m("d" -> v)))
    List(
      pair("node: 1000 literals in an array, against 10", "literal", arr(1000, "1"), arr(10, "1"), data),
      pair("node: 1000 var reads in an array, against 10", "varAccess", arr(1000, """{"var":"x"}"""), arr(10, """{"var":"x"}"""), data),
      pair(
        "node: a 32-segment path, against 2",
        "perPathSegment",
        Json.obj("var" -> List.fill(32)("d").mkString(".").asJson).noSpaces,
        Json.obj("var" -> "d.d".asJson).noSpaces,
        data
      ),
      pair(
        "node: map over 1000 elements, against 10",
        "callbackRun",
        """{"map": [{"slice": [{"var": "xs"}, 0, 1000]}, 1]}""",
        """{"map": [{"slice": [{"var": "xs"}, 0, 10]}, 1]}""",
        data
      ),
      pair(
        "node: if with 100 untaken branches, against 1",
        "element",
        """{"if": [""" + List.fill(100)("false, 1").mkString(", ") + ", 2]}",
        """{"if": [false, 1, 2]}""",
        data
      )
    )
  }

  /** Hex text exists only inside a program (data, and a literal "0x...", read as bytes): built with `cat`, in the baseline too. */
  private def hexText(o: Op, n: Int): GasCase =
    pair(
      s"${o.wire}: hex text of $n bytes",
      o.wire,
      Json.obj(o.wire -> Json.arr(Json.obj("cat" -> Json.arr("0".asJson, Json.obj("var" -> "a0".asJson))))).noSpaces,
      Json.arr(Json.obj("cat" -> Json.arr("0".asJson, Json.obj("var" -> "a0".asJson)))).noSpaces,
      m("a0" -> t("x" + "ab" * n)),
      schedule.literal.value + schedule.element.value
    )

  private val bytesOps: List[GasCase] =
    unary(Op.Hex, bytes, 32, 64000) ++
    List(
      hexText(Op.Unhex, 32),
      hexText(Op.Unhex, 32000),
      hexText(Op.HexToInt, 8),
      hexText(Op.HexToInt, 512),
      op("size 8", Op.BytesToInt, bytes(8)),
      op("size 512", Op.BytesToInt, bytes(512)),
      op("64 bits to 8 bytes", Op.IntToBytes, wint(64, 1), int(8)),
      op("4000 bits to 512 bytes", Op.IntToBytes, wint(4000, 1), int(512))
    ) ++
    List(Op.Sha256, Op.Sha3_256, Op.Keccak256, Op.Blake2b256).flatMap(unary(_, bytes, 32, 64000))

  // ---------------------------------------------------------------- cryptography

  private def seed(n: Int): ByteVector = ByteVector.fill(32)(n.toByte)
  private val message = ByteVector("transfer 10 to bob".getBytes(UTF_8))
  private val long = ByteVector.fill(64000)(0x61)

  private val L: BigInt = BigInt(2).pow(252) + BigInt("27742317777372353535851937790883648493")

  /** The Euclidean steps ZIP-215's verifier takes to split `k` (Lattice: until a remainder is below 2^126). */
  private def euclidSteps(k: BigInt): Int = {
    @scala.annotation.tailrec
    def go(r0: BigInt, r1: BigInt, n: Int): Int = if (r1.bitLength <= 126 || r1 == 0) n else go(r1, r0 % r1, n + 1)
    go(L, k, 0)
  }

  /**
   * An Ed25519 signature whose challenge `k` takes the most Euclidean steps to split, of 2^14 messages: a caller grinds
   * messages for one at a SHA-512 a try, so the verifier is priced for it.
   */
  private def groundEd25519(ed: KeyPair[Ed25519]): GasCase = {
    def challenge(msg: ByteVector, sig: ByteVector): BigInt =
      BigInt(1, Hasher[Sha512].digest(sig.take(32) ++ ed.verifying.bytes ++ msg).bytes.reverse.toArray).mod(L)
    val (msg, sig) = (0 until (1 << 14))
      .map(i => ByteVector.fromInt(i))
      .map(m => (m, Ed25519.signer.sign(ed.signing, m).bytes))
      .maxBy { case (m, sg) => euclidSteps(challenge(m, sg)) }
    op(s"a ground challenge (${euclidSteps(challenge(msg, sig))} Euclidean steps)", Op.Ed25519Verify, b(ed.verifying.bytes), b(msg), b(sig))
  }

  private def signatures(secp: Secp256k1, bls: Bls12381): List[GasCase] = {
    val ed = get(Ed25519.seedDerivation.fromSeed(seed(1)))
    val ec = get(secp.ecdsaSeedDerivation.fromSeed(seed(2)))
    val sch = get(secp.bip340SeedDerivation.fromSeed(seed(3)))
    val m32 = ByteVector.fill(32)(7)
    val bip = get(secp.bip340SignWithAux(sch.signing, get(Message32.from(m32)), ByteVector.fill(32)(0)))
    val bk = List.tabulate(64)(i => get(bls.seedDerivation.fromSeed(seed(10 + i))))
    def agg(n: Int, msg: ByteVector) =
      op(
        s"$n keys, ${msg.size}-byte message",
        Op.BlsAggregateVerify,
        a(bk.take(n).map(k => b(k.verifying.bytes)).toVector),
        b(msg),
        b(get(bls.aggregate(bk.take(n).map(k => get(bls.sign(k.signing, msg))))).bytes)
      )
    // r = x(G), s = 1, so a key is always recovered
    val ecrecoverArgs = Vector(
      b(ByteVector.fill(32)(0x42)),
      b(ByteVector.fromValidHex("79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798" + "00" * 31 + "011b"))
    )
    List(message, long).flatMap { msg =>
      List(
        op(s"${msg.size}-byte message", Op.Ed25519Verify, b(ed.verifying.bytes), b(msg), b(Ed25519.signer.sign(ed.signing, msg).bytes)),
        op(s"${msg.size}-byte message", Op.Secp256k1Verify, b(ec.verifying.bytes), b(msg), b(secp.ecdsaSigner.sign(ec.signing, msg).bytes)),
        op(s"${msg.size}-byte message", Op.BlsVerify, b(bk.head.verifying.bytes), b(msg), b(get(bls.sign(bk.head.signing, msg)).bytes))
      )
    } ++ List(
      groundEd25519(ed),
      op("32-byte message", Op.Bip340Verify, b(sch.verifying.bytes), b(m32), b(bip.bytes)),
      op("recovers a key", Op.Ecrecover, ecrecoverArgs*),
      agg(1, message),
      agg(8, message),
      agg(64, message),
      agg(8, long)
    )
  }

  /** The challenge input of TAI hash-to-curve counter `ctr`, and whether it decodes (RFC 8032 strict) to a point. */
  private def taiDecodes(pk: ByteVector, alpha: ByteVector, ctr: Int): Boolean = {
    val h = Hasher[Sha512].digest(ByteVector(0x03, 0x01) ++ pk ++ alpha ++ ByteVector(ctr, 0x00)).bytes.take(32)
    Either
      .catchNonFatal(new CompressedEdwardsY(h.toArray).decompress())
      .toOption
      .exists(p => ByteVector.view(p.compress().toByteArray) == h)
  }

  /** The first alpha (`prefix`, then `i` big-endian, from 0) whose first `k` TAI counters all fail to decode. */
  private def ground(pk: ByteVector, k: Int, prefix: ByteVector = ByteVector.empty): ByteVector =
    LazyList
      .from(0)
      .map(i => prefix ++ ByteVector.fromInt(i))
      .find(alpha => (0 until k).forall(c => !taiDecodes(pk, alpha, c)))
      .getOrElse(ByteVector.empty)

  private val vrf: List[GasCase] = {
    val kp = get(Vrf.fromSeed[EcVrfTai](seed(5)))
    def proved(alpha: ByteVector) = get(Vrf[EcVrfTai].prove(kp.signing, alpha))._1.bytes
    val pk = kp.verifying.bytes
    val first = LazyList.from(0).map(i => ByteVector.fromInt(i)).find(alpha => taiDecodes(pk, alpha, 0)).getOrElse(ByteVector.empty)
    List(
      op("valid, counter 0", Op.EcvrfVerify, b(pk), b(first), b(proved(first))),
      op("valid, 64,000-byte alpha", Op.EcvrfVerify, b(pk), b(long), b(proved(long))),
      op("valid, 12 counters fail first", Op.EcvrfVerify, b(pk), b(ground(pk, 12)), b(proved(ground(pk, 12)))),
      // each counter must not hash alpha again
      op(
        "valid, 64,000-byte alpha, 10 counters fail first",
        Op.EcvrfVerify,
        b(pk),
        b(ground(pk, 10, long)),
        b(proved(ground(pk, 10, long)))
      )
    )
  }

  private def sigma(random: SecureRandom[IO]): IO[List[GasCase]] = {
    given SecureRandom[IO] = random
    val ws = Vector.tabulate(8)(i => BigInt(1000 + i))
    def pt(w:    BigInt) = Secp256k1Group.base(w)
    def dlogC(w: BigInt): CanonicalValue = m("type" -> t("dlog"), "pk" -> b(pt(w).bytes))
    def dlog(w:  BigInt): SigmaProp = get(SigmaProp.dlog(pt(w)))
    val g = pt(BigInt(7))
    val h = pt(BigInt(11))
    val dh = get(SigmaProp.dhTuple(g, h, g * ws(0), h * ws(0)))
    val dhC = m("type" -> t("dhtuple"), "g" -> b(g.bytes), "h" -> b(h.bytes), "u" -> b((g * ws(0)).bytes), "v" -> b((h * ws(0)).bytes))
    def prove(p: SigmaProp, secrets: Vector[BigInt], msg: ByteVector): IO[ByteVector] =
      SigmaProver.prove[IO](p, secrets, msg).flatMap(e => IO.fromEither(e.leftMap(new IllegalStateException(_))))
    val and8 = get(SigmaProp.and(ws.map(dlog)))
    val and8C = m("type" -> t("and"), "children" -> a(ws.map(dlogC)))
    val th = get(SigmaProp.threshold(4, ws.map(dlog)))
    val thC = m("type" -> t("threshold"), "k" -> int(4), "children" -> a(ws.map(dlogC)))
    (
      prove(dlog(ws(0)), Vector(ws(0)), message),
      prove(dh, Vector(ws(0)), message),
      prove(and8, ws, message),
      prove(th, ws.take(4), message),
      prove(dlog(ws(0)), Vector(ws(0)), long)
    ).mapN { (p1, p2, p3, p4, p5) =>
      List(
        op("dlog", Op.SigmaVerify, dlogC(ws(0)), b(p1), b(message)),
        op("dhtuple", Op.SigmaVerify, dhC, b(p2), b(message)),
        op("and of 8 dlogs", Op.SigmaVerify, and8C, b(p3), b(message)),
        op("4 of 8 dlogs", Op.SigmaVerify, thC, b(p4), b(message)),
        op("18-byte message", Op.SchnorrVerify, b(pt(ws(0)).bytes), b(message), b(p1)),
        op("64,000-byte message", Op.SchnorrVerify, b(pt(ws(0)).bytes), b(long), b(p5))
      )
    }
  }

  /** groth16 and the BN254 family from the committed vectors: their inputs are fixed points and proofs. */
  private def zk: IO[List[GasCase]] =
    IO.blocking(new String(Files.readAllBytes(Paths.get("shared", "vectors", "jlvm_v1.json")), UTF_8)).flatMap { s =>
      IO.fromEither(parser.parse(s).flatMap(_.hcursor.downField("cases").as[Vector[Json]])).map { cases =>
        def valid(o: String): Option[Vector[Json]] =
          cases.collectFirst {
            case c
                if c.hcursor
                  .get[String]("outcome")
                  .isLeft && c.hcursor.downField("outcome").get[String]("json").toOption.exists(_ != "false") &&
                c.hcursor.get[String]("program").exists(_.startsWith(s"""{"$o":["0x""")) =>
              c.hcursor
                .get[String]("program")
                .toOption
                .flatMap(p => parser.parse(p).toOption)
                .flatMap(_.hcursor.get[Vector[Json]](o).toOption)
          }.flatten
        def literal(o: Op, name: String, args: Vector[Json]): GasCase =
          GasCase(
            s"${o.wire}: $name",
            o.wire,
            Json.obj(o.wire -> Json.fromValues(args)).noSpaces,
            Json.fromValues(args).noSpaces,
            "null",
            schedule.literal.value * (1 + args.size) + schedule.element.value * args.size
          )
        val groth = valid("groth16_verify").map(literal(Op.Groth16Verify, "SP1 proof", _)).toList
        val add = valid("bn254_add").map(literal(Op.Bn254Add, "two points", _)).toList
        val mul = valid("bn254_mul").map(literal(Op.Bn254Mul, "a point and a scalar", _)).toList :+
          op("G1 times a 250-bit scalar", Op.Bn254Mul, CBytes(ByteVector.fromValidHex("00" * 31 + "01" + "00" * 31 + "02")), fe(77))
        val g1 = "0x" + "00" * 31 + "01" + "00" * 31 + "02"
        val g2 = "0x198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c2" +
          "1800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed" +
          "090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b" +
          "12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa"
        val pairings = List(1, 2, 8, ZkOps.MaxPairs).map(n =>
          GasCase(
            s"bn254_pairing: $n pairs",
            "bn254_pairing",
            Json.obj("bn254_pairing" -> Json.arr(Json.fromValues(Vector.fill(n)(Json.arr(g1.asJson, g2.asJson))))).noSpaces,
            Json.arr(Json.fromValues(Vector.fill(n)(Json.arr(g1.asJson, g2.asJson)))).noSpaces,
            "null",
            schedule.literal.value * (2 + 3 * n) + schedule.element.value * (1 + 3 * n)
          )
        )
        val poseidon = List(1, 2, 4).map(n => op(s"$n inputs", Op.Poseidon, fes(n)*))
        val pmt = List(8, 64, ZkOps.MaxSiblings).distinct.map(d => op(s"$d siblings", Op.PmtVerify, fe(1000), fe(1001), int(5), a(fes(d))))
        groth ++ add ++ mul ++ pairings ++ poseidon ++ pmt
      }
    }

  private val merkle: List[GasCase] = {
    def tree(n: Int) = get(MerkleTree.fromLeaves[Sha256, CanonicalValue](List.tabulate(n)(i => CBytes(ByteVector.fromInt(i)))))
    List(16, 65536).flatMap { n =>
      val tr = tree(n)
      val head = m("root" -> CBytes(tr.root.bytes), "size" -> int(tr.size))
      val proof = get(tr.inclusionProof(2L).flatMap(MerkleCodec.encodeInclusion(_)))
      val old = tree(n / 2 + 1)
      val consistency = get(tr.consistencyProof(old.size).flatMap(MerkleCodec.encodeConsistency(_)))
      List(
        op(s"leaf 2 of $n", Op.MerkleVerify, head, CBytes(ByteVector.fromInt(2)), proof),
        op(
          s"${n / 2 + 1} to $n",
          Op.MerkleConsistencyVerify,
          m("root" -> CBytes(old.root.bytes), "size" -> int(old.size)),
          head,
          consistency
        )
      )
    }
  }

  /** Tries of 16 entries whose keys are `width` bytes sharing all but their last two: proofs walk the whole key. */
  private val tries: List[GasCase] =
    List(32, 1024, 16384, 65535).flatMap { width =>
      val mode = get(KeyMode.variable(65535))
      val keys = List.tabulate(16)(i => ByteVector.fill(width.toLong - 2)(0x11) ++ ByteVector.fromShort((i * 4099).toShort))
      def value(i: Int) = ByteVector.fromInt(i)
      val trie = get(
        Trie.fromEntries[Sha256](
          mode,
          keys.zipWithIndex.map { case (k, i) => k -> TrieHashing[Sha256].value(ValueCommitment.bytes(CBytes(value(i)))) }
        )
      )
      val md = m("variable" -> int(65535))
      val root = CBytes(trie.root.bytes)
      val proof = get(trie.prove(List(keys(5))).flatMap(TrieProofs.encode(_)))
      val prefix = keys(0).take(width.toLong - 2)
      val listed = a(keys.zipWithIndex.sortBy(_._1.toHex).map { case (k, i) => a(Vector(CBytes(k), CBytes(value(i)))) }.toVector)
      val prefixProof = get(trie.prove(Nil, List(prefix)).flatMap(TrieProofs.encode(_)))
      val rangeProof = get(trie.prove(Nil, Nil, List(prefix -> scala.None)).flatMap(TrieProofs.encode(_)))
      List(
        op(s"present, $width-byte keys", Op.TrieVerify, md, root, CBytes(keys(5)), CBytes(value(5)), proof),
        op(s"16 entries, $width-byte keys", Op.TriePrefixVerify, md, root, CBytes(prefix), listed, prefixProof),
        op(s"16 entries, $width-byte keys", Op.TrieRangeVerify, md, root, CBytes(prefix), CNull, listed, rangeProof)
      )
    }

  /**
   * Listings that grow with the trie: `n` entries under 32-byte keys from hashing, the whole trie by the empty prefix,
   * and the middle half by a bounded range. Both checks are linear in the proof's weight and their bounds.
   */
  private val listings: List[GasCase] =
    List(64, 512, 2048).flatMap { n =>
      val mode = get(KeyMode.variable(65535))
      val keys = List.tabulate(n)(i => Hasher[Sha256].digest(ByteVector.fromInt(i)).bytes)
      def value(i: Int) = ByteVector.fromInt(i)
      val trie = get(
        Trie.fromEntries[Sha256](
          mode,
          keys.zipWithIndex.map { case (k, i) => k -> TrieHashing[Sha256].value(ValueCommitment.bytes(CBytes(value(i)))) }
        )
      )
      val md = m("variable" -> int(65535))
      val root = CBytes(trie.root.bytes)
      val sorted = keys.zipWithIndex.sortBy(_._1.toHex)
      def listed(es: List[(ByteVector, Int)]) = a(es.map { case (k, i) => a(Vector(CBytes(k), CBytes(value(i)))) }.toVector)
      val (from, until) = (sorted(n / 4)._1, sorted(3 * n / 4)._1)
      val prefixProof = get(trie.prove(Nil, List(ByteVector.empty)).flatMap(TrieProofs.encode(_)))
      val rangeProof = get(trie.prove(Nil, Nil, List(from -> Some(until))).flatMap(TrieProofs.encode(_)))
      List(
        op(s"all $n entries, 32-byte keys", Op.TriePrefixVerify, md, root, CBytes(ByteVector.empty), listed(sorted), prefixProof),
        op(
          s"the middle ${n / 2} of $n entries, 32-byte keys",
          Op.TrieRangeVerify,
          md,
          root,
          CBytes(from),
          CBytes(until),
          listed(sorted.slice(n / 4, 3 * n / 4)),
          rangeProof
        )
      )
    }

  /**
   * A chain of `n` branches along one key: entry `i` is `i × step` bytes of 0x01 then 0x00, so the path to the key of
   * `n × step` bytes of 0x01 passes a branch every `step` bytes, and the proof carries a node for each.
   */
  private val trieChains: List[GasCase] =
    List((64, 16), (2048, 16), (256, 255)).map {
      case (n, step) =>
        val mode = get(KeyMode.variable(65535))
        val keys = List.tabulate(n)(i => ByteVector.fill((i * step).toLong)(1) :+ 0.toByte)
        val target = ByteVector.fill((n * step).toLong)(1)
        def commit(i: Int) = TrieHashing[Sha256].value(ValueCommitment.bytes(CBytes(ByteVector.fromInt(i))))
        val trie = get(Trie.fromEntries[Sha256](mode, (target -> commit(n)) :: keys.zipWithIndex.map { case (k, i) => k -> commit(i) }))
        val proof = get(trie.prove(List(target)).flatMap(TrieProofs.encode(_)))
        op(
          s"present, a chain of $n branches every $step bytes",
          Op.TrieVerify,
          m("variable" -> int(65535)),
          CBytes(trie.root.bytes),
          CBytes(target),
          CBytes(ByteVector.fromInt(n)),
          proof
        )
    }

  def anchor(secp: Secp256k1): GasCase = {
    val ec = get(secp.ecdsaSeedDerivation.fromSeed(seed(2)))
    op("anchor", Op.Secp256k1Verify, b(ec.verifying.bytes), b(message), b(secp.ecdsaSigner.sign(ec.signing, message).bytes))
      .copy(name = AnchorName, group = "anchor")
  }

  def all(secp: Secp256k1, bls: Bls12381, random: SecureRandom[IO]): IO[List[GasCase]] =
    (sigma(random), zk).mapN { (sg, z) =>
      anchor(secp) :: op("reference", Op.Sha256, bytes(1024)).copy(
        name = ReferenceName,
        group = "reference"
      ) :: (nodes ++ logic ++ arithmetic ++ spreads ++ textOps ++ arrays ++ callbacks ++ maps ++ lookups ++ bytesOps ++
      signatures(secp, bls) ++ vrf ++ sg ++ z ++ merkle ++ tries ++ listings ++ trieChains)
    }
}
