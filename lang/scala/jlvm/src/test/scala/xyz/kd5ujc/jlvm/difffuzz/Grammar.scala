package xyz.kd5ujc.jlvm.difffuzz

import cats.data.{NonEmptyVector, State}
import cats.syntax.all._

import xyz.kd5ujc.jlvm.Op

import Gen._
import Sj._

/** The kind of value an argument position expects, which the generator mostly gives and sometimes does not. */
enum K {
  case AnyV, Num, Int, Index, Text, Bytes, Hashable, Arr, Obj, Key, Bool, Exponent, Width, Sep, TextOrNull
}

/** How an operation is written: argument lists by kind, a variadic list (or a spread), a lazy or binding form, or a cryptographic call. */
enum Sig {
  case Fixed(alternatives: NonEmptyVector[Vector[K]])
  case Variadic(kind: K, max: Int)
  case Form
  case Crypto
}

/**
 * Edge values (docs/specs/jlvm.md sections 1 and 2, canonical-encoding.md section 3), as surface JSON: integers around
 * 2^31, 2^53, 2^63 and 2^64 and far beyond, decimals at the scale and digit limits, spellings JSON allows and refuses,
 * astral characters, combining marks, the White_Space set and its look-alikes, long text, and hex at and around the
 * byte-string form.
 */
object Edge {

  private val two = BigInt(2)

  val intEdges: NonEmptyVector[BigInt] = NonEmptyVector(
    BigInt(0),
    (List(31, 32, 52, 53, 54, 62, 63, 64, 65, 127, 128, 255, 256, 1023, 4095, 4096).flatMap(b =>
      List(two.pow(b) - 1, two.pow(b), two.pow(b) + 1)
    ) ++
    List(BigInt(1), BigInt(-1), BigInt(10).pow(18), BigInt(10).pow(77))).toVector
  )

  val small: G[Sj] = between(-20, 20).map(i => Num(i.toString))

  /** Any width up to past the number bound (4096 bits) and the data model's (33219 bits). */
  val wide: G[Sj] = (between(1, 4200), long, chance(0.5)).mapN { (bits, x, neg) =>
    val n = (two.pow(bits - 1) + (BigInt(x) & (two.pow(bits.min(63)) - 1))).abs
    Num((if (neg) -n else n).toString)
  }

  val int: G[Sj] = weighted(
    6 -> small,
    5 -> (pick(intEdges), chance(0.4), between(-1, 1)).mapN((n, neg, d) => Num(((if (neg) -n else n) + d).toString)),
    1 -> wide,
    1 -> oneOf(Num("8e9999"), Num("1e5000"), Num("-1e4000"), Num("1e309"), Num("-9e9998"), Num("1e400"), Num(s"1${"0" * 9999}"))
  )

  val decimal: G[Sj] = weighted(
    8 -> oneOf(
      "0.5",
      "-0.5",
      "1.5",
      "2.5",
      "-2.5",
      "0.25",
      "-0.125",
      "0.1",
      "-0.1",
      "1e-3",
      "12.345",
      "1.005",
      "99.99",
      "2.4",
      "-2.6",
      "-0",
      "0.0",
      "1.0",
      "0.10",
      "1E2",
      "1e+2",
      "1E-2",
      "-0.0",
      "5e-1",
      "123456789.987654321"
    ).map(Num(_)),
    1 -> oneOf(
      "1e-9999",
      s"0.${"0" * 9998}1",
      s"1.${"0" * 9999}",
      s"${"1" * 5000}.${"1" * 5000}",
      s"${"9" * 9999}e-9999",
      "1e-4096",
      "3e-1233",
      s"0.${"3" * 1000}"
    ).map(Num(_))
  )

  /** Any decimal: a sign, digits on each side, and sometimes an exponent, so rationals of every size reach every operation. */
  val randomDecimal: G[Sj] =
    (chance(0.4), between(1, 30), between(0, 30), weighted(4 -> pure(0), 1 -> between(-400, 400))).tupled.flatMap {
      case (neg, a, b, e) =>
        (vectorOf(a, below(10)), vectorOf(b, below(10))).mapN { (x, y) =>
          Num((if (neg) "-" else "") + x.mkString + (if (y.isEmpty) "" else "." + y.mkString) + (if (e == 0) "" else s"e$e"))
        }
    }

  val num: G[Sj] = weighted(6 -> int, 3 -> decimal, 2 -> randomDecimal)

  /** Numbers the canonical parser refuses: not JSON, or past the digit and scale limits. */
  val badNumber: G[Sj] = oneOf(
    "01",
    "-01",
    "+1",
    ".5",
    "1.",
    "1e",
    "-",
    "NaN",
    "Infinity",
    "0x10",
    "1e-10001",
    "1e-10000",
    s"0.${"0" * 9999}1",
    "9e9999",
    "1e10000",
    "1" * 10001,
    s"0.${"0" * 10000}1",
    "1_000"
  ).map(Num(_))

  val texts: NonEmptyVector[String] = NonEmptyVector(
    "",
    List(
      "a",
      "abc",
      "x",
      "k",
      "Hello World",
      "  pad  ",
      "\u00e9t\u00e9",
      "\ud83d\ude00x\ud83d\ude01",
      "\udbff\udfff",
      "e\u0301",
      "\u00e9",
      "a.b.c",
      "a,b,,c",
      "a..b",
      ".",
      "$x",
      "$",
      "0xab",
      "0xAB",
      "0x",
      "0x0",
      "0xg0",
      "\u00dfI\u0130K",
      "\u0130i\u0307",
      "\u03a3\u03c3\u03c2",
      "\u01c5",
      "\ufb03",
      "\t\n\r \u000b\u000c",
      "\u0085x\u0085",
      "\u00a0y\u00a0",
      "\u1680",
      "\u2000\u200a",
      "\u2028\u2029",
      "\u202f\u205f\u3000",
      "\u180ez\u180e",
      "\u200bq\u200b",
      "\ufefft\ufeff",
      "line\nbreak",
      "nul\u0000nul",
      "\u007f",
      "-1",
      "01",
      "1",
      "true",
      "null",
      "+",
      "var",
      "literal",
      "if",
      "_x",
      "\ud803\udd50\ud803\udd70",
      "\u13a0\uab70"
    ).toVector
  )

  /** Long text: around the preview's 32 code points and the 32-unit path-segment blocks, and in each UTF-8 width. */
  val longText: G[Sj] =
    (oneOf("a", "\u00e9", "\u4e2d", "\ud83d\ude00", "e\u0301", "k"), oneOf(31, 32, 33, 63, 64, 65, 127, 128, 129, 300, 1000, 5000))
      .mapN((u, n) => Str(u * n))

  /** Escapes JSON allows: a pair escaped (either case), `\/`, the control escapes. */
  val escaped: G[Sj] = oneOf(
    "\"\\ud83d\\ude00\"",
    "\"\\uD83D\\uDE00\"",
    "\"\\u0041\"",
    "\"\\/\"",
    "\"a\\u0000b\"",
    "\"\\b\\f\\r\"",
    "\"\\u00e9\""
  ).map(Esc(_))

  /** Escapes the canonical parser refuses: lone surrogates, and escapes JSON does not have. */
  val badEscaped: G[Sj] =
    oneOf("\"\\ud800\"", "\"\\udc00\"", "\"\\ud800\\u0041\"", "\"\\udfff\\ud800\"", "\"\\x41\"", "\"\\u12\"", "\"\\'\"").map(Esc(_))

  /**
   * Code points that order, measure and map differently in UTF-8, UTF-16 and code points, or that case mapping, trim
   * and normalisation treat specially: the last BMP characters past the surrogates and astral ones (which UTF-16 sorts
   * the other way), combining marks, White_Space and its look-alikes, and letters whose Unicode case maps to ASCII.
   */
  private val alphabet: NonEmptyVector[String] = NonEmptyVector(
    "a",
    Vector(
      "b",
      "k",
      "z",
      "A",
      "K",
      "Z",
      "0",
      "9",
      ".",
      "$",
      " ",
      "_",
      "-",
      "\u00e9",
      "\u00ff",
      "\u0100",
      "\u07ff",
      "\u0800",
      "\ud7ff",
      "\ue000",
      "\ufffd",
      "\ufeff",
      "\uffff",
      "\ud800\udc00",
      "\ud83d\ude00",
      "\udbff\udfff",
      "\u0301",
      "\u0085",
      "\u00a0",
      "\u2028",
      "\u3000",
      "\u180e",
      "\u200b",
      "\t",
      "\n",
      "\u0000",
      "\u007f",
      "\u0130",
      "\u0131",
      "\u017f",
      "\u212a",
      "\u00df",
      "\u03a3",
      "\u03c2"
    )
  )

  val unicode: G[String] = between(0, 12).flatMap(n => vectorOf(n, pick(alphabet))).map(_.mkString)

  /** Every `White_Space` code point (jlvm.md section 2) and the look-alikes `trim` must keep. */
  private val spaces: NonEmptyVector[String] = NonEmptyVector(
    "\t",
    Vector(
      "\n",
      "\u000b",
      "\u000c",
      "\r",
      " ",
      "\u0085",
      " ",
      " ",
      " ",
      " ",
      " ",
      " ",
      " ",
      " ",
      " ",
      "　",
      "᠎",
      "​",
      "⁠",
      "﻿",
      "\u001f",
      "­"
    )
  )

  /** Text between runs of spaces and look-alikes, for `trim` and every operation that reads text. */
  val spaced: G[String] = (upTo(2, pick(spaces)), unicode, upTo(2, pick(spaces))).mapN((a, t, b) => a.mkString + t + b.mkString)

  val text: G[Sj] = weighted(10 -> pick(texts).map(Str(_)), 6 -> unicode.map(Str(_)), 3 -> spaced.map(Str(_)), 2 -> longText, 1 -> escaped)

  private val hexDigits = "0123456789abcdef"

  def randomHex(n: Int): G[String] = vectorOf(2 * n, below(16).map(hexDigits.charAt)).map(_.mkString)

  val bytes: G[Sj] = weighted(
    4 -> oneOf("0x", "0x00", "0x01", "0xff", "0x0102", "0x0001", "0x8000", "0x" + "00" * 32, "0x" + "ff" * 32, "0x" + "ab" * 20)
      .map(Str(_)),
    3 -> oneOf(1, 20, 31, 32, 33, 48, 63, 64, 65, 96, 128, 300).flatMap(randomHex).map(h => Str("0x" + h))
  )

  /** Arrays: empty, flat, mixed and nested, and nested close to the depth bound of 256 levels. */
  /** One value several ways: the same number spelled differently, and equal collections, for `unique` and `intersect`. */
  private def alike(d: Int): G[Sj] =
    (any(d - 1), between(1, 4)).mapN { (v, n) =>
      Arr(
        Vector.fill(n)(v) ++ Vector(
          Num("2"),
          Num("2.0"),
          Num("2e0"),
          Str("2"),
          Arr(Vector(Num("1"))),
          Arr(Vector(Num("1.0"))),
          Obj(Vector("a" -> Num("1"), "b" -> Null)),
          Obj(Vector("a" -> Num("1.0")))
        )
      )
    }

  def array(d: Int): G[Sj] = weighted(
    1 -> alike(d),
    2 -> pure(Arr(Vector.empty)),
    5 -> upTo(5, scalar).map(Arr(_)),
    2 -> upTo(4, any(d - 1)).map(Arr(_)),
    1 -> (oneOf(126, 127, 128, 250, 252, 253, 254), scalar).mapN(nested)
  )

  def nested(n: Int, leaf: Sj): Sj = (1 to n).foldLeft(leaf)((x, _) => Arr(Vector(x)))

  val key: G[String] = weighted(
    8 -> oneOf("a", "b", "x", "y", "k", "xs", "n"),
    3 -> unicode,
    1 -> oneOf(70, 64, 65, 130).map(n => "\ud83d\ude00" * n),
    3 -> pick(texts),
    1 -> oneOf(300, 31, 32, 33).map(n => "k" * n + "0007")
  )

  /** Maps: empty, small, with null members (dropped), keys sharing long prefixes, repeated names (refused), nested. */
  def obj(d: Int): G[Sj] = weighted(
    2 -> pure(Obj(Vector.empty)),
    6 -> upTo(4, (key, any(d - 1)).tupled).map(kvs => Obj(kvs.distinctBy(_._1))),
    1 -> between(1, 40).map(n =>
      Obj((0 until n).toVector.map(i => ("k" * 200 + i.toString.reverse.padTo(4, '0').reverse) -> Num(i.toString)))
    ),
    1 -> key.map(k => Obj(Vector(k -> Null, "z" -> Num("1"))))
  )

  val scalar: G[Sj] = weighted(4 -> num, 4 -> text, 2 -> bytes, 1 -> oneOf(Bool(true), Bool(false), Null))

  /**
   * Values the parsers refuse: numbers past the digit, scale or width limits or not JSON, refused escapes, a repeated
   * member name, and nesting past the depth bound. Drawn rarely, so most cases get past parsing.
   */
  val refused: G[Sj] = weighted(
    3 -> badNumber,
    3 -> badEscaped,
    2 -> (key, scalar).mapN((k, v) => Obj(Vector(k -> v, "z" -> Num("1"), k -> v))),
    1 -> (oneOf(255, 256, 257, 300), scalar).mapN(nested)
  )

  def any(d: Int): G[Sj] =
    if (d <= 0) scalar
    else weighted(6 -> scalar, 2 -> array(d), 2 -> obj(d))

  /** A value of the kind. */
  def of(k: K, d: Int): G[Sj] = k match {
    case K.AnyV     => any(d)
    case K.Num      => num
    case K.Int      => int
    case K.Index    => weighted(5 -> between(-3, 6).map(i => Num(i.toString)), 2 -> int, 1 -> oneOf(Num("1.5"), Num("-0.5"), Num("1e2")))
    case K.Text     => text
    case K.Bytes    => bytes
    case K.Hashable => choose(bytes, text)
    case K.Arr      => array(d.max(1))
    case K.Obj      => obj(d.max(1))
    case K.Key      => key.map(Str(_))
    case K.Bool     => oneOf(Bool(true), Bool(false))
    case K.Exponent =>
      weighted(
        4 -> between(-3, 12).map(i => Num(i.toString)),
        2 -> oneOf(63, 64, 999, -999, 1000, -1000, 4095, 4096).map(i => Num(i.toString)),
        1 -> int
      )
    case K.Width      => oneOf(0, 1, 2, 8, 31, 32, 33, 1024, 1025, -1).map(i => Num(i.toString))
    case K.Sep        => oneOf(",", "", "a", ".", "\u00e9", "bc", "aa", "--", "\ud83d\ude00").map(Str(_))
    case K.TextOrNull => weighted(5 -> text, 1 -> pure(Null))
  }
}

/** What an expression can see: the callback frames around it (a reduce among them binds `$accumulator`) and the `let` names in scope. */
final case class Ctx(frames: Int, accumulator: Boolean, lets: Vector[String])

/**
 * The grammar over the closed operation list (`JlvmOp`): each operation's argument lists and kinds, the kinds its result
 * has, and the lazy, binding, callback and lookup forms, written with edge values placed inline, quoted, in the data or
 * in a host binding, and with reads that hit, miss and partly hit.
 */
object Grammar {

  private def fixed(first: Vector[K], rest: Vector[K]*): Sig = Sig.Fixed(NonEmptyVector(first, rest.toVector))
  private def v(ks:        K*): Vector[K] = ks.toVector

  /** Every operation, by a total match, so a new one in `jlvm.cddl` does not compile until it is placed here. */
  def sig(op: Op): Sig = op match {
    case Op.If | Op.And | Op.Or | Op.Default | Op.Let                                     => Sig.Form
    case Op.Map | Op.Filter | Op.Reduce | Op.All | Op.Some | Op.None | Op.Find | Op.Count => Sig.Form
    case Op.Missing | Op.MissingSome                                                      => Sig.Form
    case Op.Not | Op.Truthy | Op.TypeOf                                                   => fixed(v(K.AnyV))
    case Op.Eq | Op.Ne | Op.StrictEq | Op.StrictNe                                        => fixed(v(K.AnyV, K.AnyV))
    case Op.Lt | Op.Le   => fixed(v(K.Num, K.Num), v(K.Text, K.Text), v(K.Num, K.Num, K.Num), v(K.Text, K.Text, K.Text))
    case Op.Gt | Op.Ge   => fixed(v(K.Num, K.Num), v(K.Text, K.Text))
    case Op.Add | Op.Mul => Sig.Variadic(K.Num, 5)
    case Op.Min | Op.Max => Sig.Variadic(K.Num, 4)
    case Op.Cat          => Sig.Variadic(K.AnyV, 4)
    case Op.Merge        => Sig.Variadic(K.Obj, 3)
    case Op.Sub          => fixed(v(K.Num), v(K.Num, K.Num))
    case Op.Div | Op.Mod => fixed(v(K.Num, K.Num))
    case Op.Abs | Op.Round | Op.RoundHalfUp | Op.Floor | Op.Ceil => fixed(v(K.Num))
    case Op.Pow                                                  => fixed(v(K.Num, K.Exponent))
    case Op.In                               => fixed(v(K.Text, K.Text), v(K.Bytes, K.Bytes), v(K.AnyV, K.Arr), v(K.TextOrNull, K.Text))
    case Op.Intersect                        => fixed(v(K.Arr, K.Arr))
    case Op.Unique | Op.Reverse | Op.Flatten => fixed(v(K.Arr))
    case Op.Slice                            => fixed(v(K.Arr, K.Index), v(K.Arr, K.Index, K.Index))
    case Op.Length                           => fixed(v(K.Text), v(K.Arr), v(K.Bytes), v(K.Obj))
    case Op.Substr                           => fixed(v(K.Text, K.Index), v(K.Text, K.Index, K.Index))
    case Op.Lower | Op.Upper | Op.Trim       => fixed(v(K.Text))
    case Op.Join                             => fixed(v(K.Arr, K.Sep))
    case Op.Split                            => fixed(v(K.Text, K.Sep))
    case Op.StartsWith | Op.EndsWith         => fixed(v(K.TextOrNull, K.TextOrNull))
    case Op.Keys | Op.Values | Op.Entries    => fixed(v(K.Obj))
    case Op.Get                              => fixed(v(K.Obj, K.Key), v(K.Obj, K.Key, K.AnyV))
    case Op.Has | Op.Unset                   => fixed(v(K.Obj, K.Key))
    case Op.Set                              => fixed(v(K.Obj, K.Key, K.AnyV))
    case Op.Exists                           => fixed(v(), v(K.AnyV), v(K.AnyV, K.AnyV), v(K.AnyV, K.AnyV, K.AnyV))
    case Op.Hex | Op.BytesToInt              => fixed(v(K.Bytes))
    case Op.Unhex | Op.HexToInt | Op.Sha256 | Op.Sha3_256 | Op.Keccak256 | Op.Blake2b256        => fixed(v(K.Hashable))
    case Op.IntToBytes                                                                          => fixed(v(K.Int), v(K.Int, K.Width))
    case Op.Ed25519Verify | Op.Secp256k1Verify | Op.Bip340Verify | Op.Ecrecover | Op.BlsVerify  => Sig.Crypto
    case Op.BlsAggregateVerify | Op.Poseidon | Op.PmtVerify | Op.Groth16Verify | Op.EcvrfVerify => Sig.Crypto
    case Op.Bn254Add | Op.Bn254Mul | Op.Bn254Pairing | Op.SchnorrVerify | Op.SigmaVerify        => Sig.Crypto
    case Op.MerkleVerify | Op.MerkleConsistencyVerify | Op.TrieVerify | Op.TriePrefixVerify | Op.TrieRangeVerify => Sig.Crypto
  }

  /** The kind of an operation's result, for nesting (total, like [[sig]]). */
  def returns(op: Op): K = op match {
    case Op.Add | Op.Sub | Op.Mul | Op.Div | Op.Mod | Op.Min | Op.Max | Op.Abs | Op.Pow | Op.RoundHalfUp => K.Num
    case Op.Round | Op.Floor | Op.Ceil | Op.Length | Op.Count | Op.HexToInt | Op.BytesToInt              => K.Int
    case Op.Cat | Op.Substr | Op.Lower | Op.Upper | Op.Trim | Op.Join | Op.Hex | Op.TypeOf               => K.Text
    case Op.Eq | Op.Ne | Op.StrictEq | Op.StrictNe | Op.Lt | Op.Le | Op.Gt | Op.Ge | Op.Not | Op.Truthy  => K.Bool
    case Op.In | Op.Intersect | Op.Has | Op.StartsWith | Op.EndsWith | Op.Exists | Op.All | Op.Some      => K.Bool
    case Op.None                                                                                         => K.Bool
    case Op.Map | Op.Filter | Op.Merge | Op.Split | Op.Keys | Op.Values | Op.Entries | Op.Unique         => K.Arr
    case Op.Reverse | Op.Flatten | Op.Slice | Op.Missing | Op.MissingSome                                => K.Arr
    case Op.Set | Op.Unset                                                                               => K.Obj
    case Op.Unhex | Op.Sha256 | Op.Sha3_256 | Op.Keccak256 | Op.Blake2b256 | Op.IntToBytes               => K.Bytes
    case Op.If | Op.And | Op.Or | Op.Default | Op.Let | Op.Reduce | Op.Find | Op.Get                     => K.AnyV
    case Op.Ed25519Verify | Op.Secp256k1Verify | Op.Bip340Verify | Op.BlsVerify | Op.BlsAggregateVerify  => K.Bool
    case Op.PmtVerify | Op.Groth16Verify | Op.Bn254Pairing | Op.SchnorrVerify | Op.SigmaVerify           => K.Bool
    case Op.MerkleVerify | Op.MerkleConsistencyVerify | Op.TrieVerify | Op.TriePrefixVerify              => K.Bool
    case Op.TrieRangeVerify                                                                              => K.Bool
    case Op.Ecrecover | Op.Poseidon | Op.Bn254Add | Op.Bn254Mul                                          => K.Bytes
    case Op.EcvrfVerify                                                                                  => K.Obj
  }

  private val nonCrypto: Vector[Op] = Op.values.toVector.filter(o => sig(o) != Sig.Crypto)

  private val returning: Map[K, NonEmptyVector[Op]] =
    nonCrypto.groupBy(returns).flatMap { case (k, ops) => NonEmptyVector.fromVector(ops).map(k -> _) }

  def opsFor(k: K): NonEmptyVector[Op] = k match {
    case K.Num | K.Index | K.Exponent | K.Width =>
      returning.getOrElse(K.Num, NonEmptyVector.one(Op.Add)) ++ returning.get(K.Int).fold(Vector.empty[Op])(_.toVector)
    case K.Int                                              => returning.getOrElse(K.Int, NonEmptyVector.one(Op.Length))
    case K.Text | K.Key | K.Sep | K.TextOrNull | K.Hashable => returning.getOrElse(K.Text, NonEmptyVector.one(Op.Cat))
    case K.Bytes                                            => returning.getOrElse(K.Bytes, NonEmptyVector.one(Op.Sha256))
    case K.Arr                                              => returning.getOrElse(K.Arr, NonEmptyVector.one(Op.Map))
    case K.Obj                                              => returning.getOrElse(K.Obj, NonEmptyVector.one(Op.Set))
    case K.Bool                                             => returning.getOrElse(K.Bool, NonEmptyVector.one(Op.Not))
    case K.AnyV                                             => NonEmptyVector.fromVectorUnsafe(nonCrypto)
  }

  /** Paths into [[world]]: hits, misses, partial hits (a key under a scalar, an index past the end), and malformed ones. */
  val worldPath: G[String] = oneOf(
    "x",
    "y",
    "a",
    "a.b",
    "a.b.0",
    "a.b.1.c",
    "a.b.5",
    "a.b.-1",
    "a.b.01",
    "a.c",
    "a.b.c",
    "xs",
    "xs.0.x",
    "xs.1.y",
    "xs.9",
    "n.0",
    "t.x",
    "\u00e9",
    "01",
    "-1",
    "",
    "nope",
    "nope.deeper",
    "a..b",
    ".a",
    "a.",
    "deep.0.0.0.0",
    "k" * 33,
    "\ud83d\ude00" * 17,
    "\ud83d\ude00" * 16,
    "\u00e9" * 33,
    "\u00e9" * 32 + ".x",
    "0",
    "1"
  )

  /** The data a program reads besides its placed values. */
  val world: G[Vector[(String, Sj)]] =
    (Edge.num, Edge.text, Edge.any(2), Edge.any(1), Edge.text).mapN { (n, t, w, u, s) =>
      Vector(
        "x"                   -> n,
        "y"                   -> t,
        "a"                   -> Obj(Vector("b" -> Arr(Vector(Num("1"), Obj(Vector("c" -> Str("d"))))), "c" -> w)),
        "xs"                  -> Arr(Vector(Obj(Vector("x" -> Num("1"), "y" -> Num("2"))), Obj(Vector("x" -> Num("3"), "z" -> u)))),
        "n"                   -> Num("7"),
        "t"                   -> s,
        "\u00e9"              -> Num("2"),
        "01"                  -> Str("zero-one"),
        "-1"                  -> Str("minus-one"),
        "a.b"                 -> Str("dotted"),
        "deep"                -> Arr(Vector(Arr(Vector(Arr(Vector(Arr(Vector(Num("4"))))))))),
        ("k" * 33)            -> Num("33"),
        ("\ud83d\ude00" * 17) -> Num("17"),
        ("\u00e9" * 33)       -> Num("33"),
        ("\u00e9" * 32)       -> Obj(Vector("x" -> Num("32")))
      )
    }
}

/**
 * The generators of [[Grammar]]'s programs, with the cryptographic operations' arguments drawn from `seeds` (the
 * vector rows) and mutated.
 */
final class Grammar(seeds: Map[Op, Vector[CryptoSeed]]) {

  import Grammar._

  /** Where a value goes: inline (a scalar, an inline array), quoted, in the data, or in a host binding. */
  def place(value: Sj): G[Sj] = value match {
    case Arr(_) => weighted(4 -> pure(lit(value)), 3 -> inData(value), 1 -> inEnv(value), 1 -> pure(value))
    case Obj(_) => weighted(4 -> pure(lit(value)), 3 -> inData(value), 1 -> inEnv(value))
    case _      => weighted(6 -> pure(value), 2 -> inData(value), 1 -> inEnv(value), 1 -> pure(lit(value)))
  }

  /** A read: of the data (by text, by default, computed as text or as exact segments), of a binding, or of a frame. */
  def read(ctx: Ctx): G[Sj] = {
    val data = weighted(
      6 -> worldPath.map(p => obj("var", Str(p))),
      2 -> (worldPath, Edge.scalar).mapN((p, d) => obj("var", Arr(Vector(Str(p), d)))),
      1 -> worldPath.map(p => obj("var", Arr(Vector(Str(p), Null)))),
      1 -> (worldPath, Edge.array(1)).mapN((p, d) => obj("var", Arr(Vector(Str(p), lit(d))))),
      1 -> (worldPath, Edge.array(1)).mapN((p, d) => obj("var", Arr(Vector(Str(p), d)))),
      1 -> worldPath.map(p => obj("var", call("cat", Str(p), Str(""), Str("")))),
      1 -> worldPath.map(p => obj("var", lit(Str(p)))),
      2 -> (oneOf("a", "xs", "deep", "a.b"), Edge.of(K.Index, 0)).mapN((k, i) => obj("var", Arr(Vector(Arr(Vector(Str(k), i)))))),
      1 -> oneOf(Num("0"), Num("1.5"), Num("-1"), Num("1e2"), Bool(true), Null).map(p => obj("var", p)),
      1 -> pure(obj("var", Arr(Vector.empty))),
      1 -> pure(obj("var", Arr(Vector(Str("x"), Num("1"), Num("2")))))
    )
    val bound = weighted(
      3 -> State
        .inspect[GenState, Vector[String]](_.env.map(_._1))
        .flatMap(ks => NonEmptyVector.fromVector(ks).fold(Edge.any(1).flatMap(inEnv))(nk => pick(nk).map(k => obj("var", Str("$" + k))))),
      2 -> Edge.any(1).flatMap(inEnv),
      1 -> pure(Null).flatMap(inEnv),
      1 -> pure(obj("var", Str("$nope"))),
      1 -> Edge.scalar.map(d => obj("var", Arr(Vector(Str("$nope"), d)))),
      1 -> pure(obj("var", Str("$h0.x"))),
      1 -> pure(obj("var", Str("$")))
    )
    val frame = weighted(
      3 -> pure(obj("var", Str("$current"))),
      2 -> pure(obj("var", Str("$index"))),
      2 -> oneOf("x", "y", "z", "").map(p => obj("var", Str(p))),
      1 -> pure(obj("var", Str("$current.x"))),
      1 -> pure(obj("var", Str(if (ctx.accumulator) "$accumulator" else "$current")))
    )
    val lets = NonEmptyVector.fromVector(ctx.lets).map(ns => pick(ns).map(n => obj("var", Str("$" + n))))
    weighted(5 -> data, (List(2 -> bound) ++ Option.when(ctx.frames > 0)(4 -> frame) ++ lets.map(3 -> _))*)
  }

  /** An argument of the kind: a value (placed), a read, or a nested expression; sometimes a value of another kind. */
  def arg(k: K, d: Int, ctx: Ctx): G[Sj] =
    weighted(
      70                     -> Edge.of(k, 2).flatMap(place),
      20                     -> read(ctx),
      (if (d > 0) 40 else 0) -> defer(pick(opsFor(k)).flatMap(o => invoke(o, d - 1, ctx))),
      10                     -> Edge.any(1).flatMap(place),
      1                      -> Edge.refused.flatMap(place)
    )

  private def args(ks: Vector[K], d: Int, ctx: Ctx): G[Vector[Sj]] = ks.traverse(arg(_, d, ctx))

  /** Sometimes one argument too few or too many. */
  private def misArity(xs: Vector[Sj], d: Int, ctx: Ctx): G[Vector[Sj]] =
    weighted(
      14 -> pure(xs),
      1  -> pure(xs.dropRight(1)),
      1  -> arg(K.AnyV, d, ctx).map(xs :+ _)
    )

  private def collection(d: Int, ctx: Ctx): G[Sj] = weighted(
    6 -> pure(
      lit(Arr(Vector(Obj(Vector("x" -> Num("1"), "y" -> Str("a"))), Obj(Vector("x" -> Num("2"))), Obj(Vector("z" -> Bool(true))))))
    ),
    2                     -> upTo(4, Edge.obj(1)).map(xs => lit(Arr(xs))),
    3                     -> Edge.array(1).flatMap(place),
    2                     -> pure(obj("var", Str("xs"))),
    1                     -> pure(obj("var", Str("a.b"))),
    1                     -> oneOf(Null, Num("3"), Str("abc"), lit(Obj(Vector("x" -> Num("1"))))).flatMap(place),
    (if (d > 0) 2 else 0) -> defer(invoke(Op.Map, d - 1, ctx)),
    1                     -> pure(Arr(Vector(Num("1"), Num("2"), Num("3"))))
  )

  private def callback(op: Op, d: Int, ctx: Ctx): G[Sj] = {
    val inner = Ctx(ctx.frames + 1, ctx.accumulator || op == Op.Reduce, ctx.lets)
    // reads of the element (and of the data or an outer element that binds the same key, which warns), on their own
    // and in small expressions
    val frame: G[Sj] = oneOf(
      "x",
      "x",
      "y",
      "y",
      "z",
      "",
      "$current",
      "$current.x",
      "$index",
      "current",
      "accumulator",
      if (inner.accumulator) "$accumulator" else "x"
    )
      .map(p => obj("var", Str(p)))
    val framed: G[Sj] = weighted(
      4                     -> frame,
      2                     -> (frame, frame).mapN((a, b) => Sj.call("==", a, b)),
      2                     -> frame.map(a => Sj.call("+", a, Num("1"))),
      1                     -> frame.map(a => Sj.call("cat", a, Str("-"))),
      (if (d > 0) 2 else 0) -> defer(oneOf(Op.Map, Op.Filter, Op.Some, Op.Reduce).flatMap(o => callback(o, d - 1, inner)))
    )
    val body = op match {
      case Op.Reduce =>
        weighted(
          3 -> pure(call("+", obj("var", Str("$accumulator")), obj("var", Str("$current")))),
          1 -> pure(call("cat", obj("var", Str("$accumulator")), obj("var", Str("x")))),
          2 -> arg(K.AnyV, d, inner)
        )
      case Op.Map => weighted(3 -> framed, 2 -> arg(K.AnyV, d, inner))
      case _      => weighted(3 -> framed, 3 -> arg(K.Bool, d, inner), 1 -> arg(K.AnyV, d, inner))
    }
    for {
      coll <- collection(d, ctx)
      b    <- body
      extra <- op match {
        case Op.Reduce => weighted(2 -> arg(K.AnyV, 0, ctx).map(Vector(_)), 1 -> pure(Vector.empty[Sj]))
        case Op.Some   => weighted(2 -> pure(Vector.empty[Sj]), 1 -> Edge.of(K.Index, 0).map(Vector(_)))
        case _         => weighted(12 -> pure(Vector.empty[Sj]), 1 -> arg(K.AnyV, 0, ctx).map(Vector(_)))
      }
      all <- misArity(Vector(coll, b) ++ extra, 0, ctx)
    } yield obj(op.wire, Arr(all))
  }

  private val letName: G[String] =
    weighted(
      8 -> oneOf("x", "y", "acc", "\u00e9", "n1", "current"),
      2 -> Edge.unicode.map(_.replace(".", "").replace("$", "")),
      1 -> oneOf("", "a.b", "$x", "1", "_q")
    )

  private def let(d: Int, ctx: Ctx): G[Sj] =
    for {
      n       <- between(0, 3)
      names   <- vectorOf(n, letName)
      values  <- names.traverse(_ => arg(K.AnyV, d, ctx))
      body    <- arg(K.AnyV, d, ctx.copy(lets = ctx.lets ++ names))
      asPairs <- chance(0.7)
    } yield
      call(
        "let",
        if (asPairs) Arr(names.zip(values).map { case (k, x) => Arr(Vector(Str(k), x)) })
        else Obj(names.zip(values).sortBy(_._1)),
        body
      )

  private def lazyForm(op: Op, d: Int, ctx: Ctx): G[Sj] =
    between(0, if (op == Op.If) 6 else 4)
      .flatMap(n =>
        vectorOf(
          n,
          weighted(3 -> arg(K.AnyV, d, ctx), 2 -> oneOf(Num("0"), Str(""), Bool(false), Null, Arr(Vector.empty), Num("1"), Str("a")))
        )
      )
      .map(xs => obj(op.wire, Arr(xs)))

  private val missingKey: G[Sj] = weighted(5 -> worldPath.map(Str(_)), 1 -> Edge.scalar)

  private def lookup(op: Op): G[Sj] = op match {
    case Op.MissingSome =>
      (Edge.of(K.Index, 0), upTo(4, missingKey))
        .mapN((m, ks) => call(op.wire, m, Arr(ks)))
        .flatMap(x => weighted(10 -> pure(x), 1 -> pure(call(op.wire, Num("1")))))
    case _ =>
      weighted(
        3 -> upTo(4, missingKey).map(ks => obj(op.wire, Arr(ks))),
        1 -> upTo(4, missingKey).map(ks => call(op.wire, Arr(ks))),
        1 -> pure(obj(op.wire, obj("var", Str("xs"))))
      )
  }

  private def form(op: Op, d: Int, ctx: Ctx): G[Sj] = op match {
    case Op.Let                              => let(d, ctx)
    case Op.If | Op.And | Op.Or | Op.Default => lazyForm(op, d, ctx)
    case Op.Missing | Op.MissingSome         => lookup(op)
    case Op.Count                            => weighted(2 -> callback(op, d, ctx), 1 -> collection(d, ctx).map(c => call(op.wire, c)))
    case Op.Map | Op.Filter | Op.Reduce | Op.All | Op.Some | Op.None | Op.Find => callback(op, d, ctx)
    case other                                                                 => pure(call(other.wire))
  }

  private def variadic(op: Op, k: K, max: Int, d: Int, ctx: Ctx): G[Sj] = {
    val kind = op match {
      case Op.Cat   => oneOf(K.Text, K.Num, K.Bool, K.TextOrNull, K.Arr, K.Bytes)
      case Op.Merge => oneOf(K.Obj, K.Arr, K.AnyV)
      case _        => pure(k)
    }
    weighted(
      5 -> kind
        .flatMap(kk => between(0, max).flatMap(n => vectorOf(n, weighted(5 -> arg(kk, d, ctx), 1 -> arg(K.AnyV, d, ctx)))))
        .map(xs => obj(op.wire, Arr(xs))),
      2 -> kind.flatMap(kk => upTo(max, Edge.of(kk, 1))).flatMap(xs => inData(Arr(xs))).map(r => obj(op.wire, r)),
      1 -> kind.flatMap(kk => upTo(max, Edge.of(kk, 1))).map(xs => obj(op.wire, lit(Arr(xs)))),
      1 -> oneOf(Str("x"), Num("5"), Null, obj("object", Obj(Vector.empty)), lit(Num("1")), obj("var", Str("x"))).map(x => obj(op.wire, x))
    )
  }

  /** A call of `op`, nesting at most `d` more. */
  def invoke(op: Op, d: Int, ctx: Ctx): G[Sj] = sig(op) match {
    case Sig.Fixed(alts) =>
      for {
        ks     <- pick(alts)
        xs     <- args(ks, d, ctx)
        all    <- misArity(xs, d, ctx)
        single <- chance(0.1)
      } yield
        all match {
          case Vector(Arr(_))         => obj(op.wire, Arr(all))
          case Vector(only) if single => obj(op.wire, only)
          case _                      => obj(op.wire, Arr(all))
        }
    case Sig.Variadic(k, max) => variadic(op, k, max, d, ctx)
    case Sig.Form             => form(op, d, ctx)
    case Sig.Crypto           => crypto(op)
  }

  /** An argument of a cryptographic call: byte strings mostly inline, structures quoted or in the data. */
  private def placeCrypto(value: Sj): G[Sj] = value match {
    case Str(_) => weighted(5 -> pure(value), 2 -> inData(value), 1 -> inEnv(value))
    case _      => place(value)
  }

  private def edit(args: Vector[Sj], f: Sj => G[Sj]): G[Vector[Sj]] =
    if (args.isEmpty) pure(args) else below(args.size).flatMap(i => f(args(i)).map(args.updated(i, _)))

  /**
   * A call of a cryptographic operation on a vector row's arguments: as they are (true, or a value), with a byte
   * flipped, cut or added anywhere in them (false, or a refusal), with an argument of another row (false), or of
   * another kind, or reordered, or one too few or too many.
   */
  def crypto(op: Op): G[Sj] =
    NonEmptyVector.fromVector(seeds.getOrElse(op, Vector.empty)).fold(upTo(4, Edge.bytes).map(xs => obj(op.wire, Arr(xs)))) { ss =>
      for {
        s <- pick(ss)
        o <- pick(ss)
        args <- weighted(
          9 -> pure(s.args),
          5 -> edit(s.args, Mutate.someHex),
          2 -> (if (s.args.isEmpty || o.args.size != s.args.size) pure(o.args)
                else below(s.args.size).map(i => s.args.updated(i, o.args(i)))),
          1 -> edit(s.args, _ => Edge.any(1)),
          1 -> pure(s.args.reverse),
          1 -> pure(s.args.dropRight(1)),
          1 -> pure(s.args :+ Str("0x00")),
          1 -> edit(s.args, x => Mutate.tree(x, 1, this))
        )
        placed <- args.traverse(placeCrypto)
        spread <- chance(0.15)
      } yield
        if (spread && (op == Op.Poseidon || op == Op.Bn254Pairing)) obj(op.wire, lit(Arr(args)))
        else obj(op.wire, Arr(placed))
    }

  /**
   * Values built up to the result bound (2^24, jlvm.md section 2) by doubling: text, bytes, arrays and maps, `k`
   * doublings of a small seed, so the last steps straddle the bound. Run with a raised gas limit.
   */
  val bulk: G[Sj] =
    (between(20, 23), oneOf("ab", "0x0102", "\u00e9\ud83d\ude00")).mapN { (k, seed) =>
      (k, seed)
    }.flatMap {
      case (k, seed) =>
        val steps = lit(Arr((1 to k).toVector.map(i => Num(i.toString))))
        val acc = obj("var", Str("$accumulator"))
        // measured, so the record stays small: what is compared is whether the bound refused the last doubling
        oneOf(
          obj("reduce", Arr(Vector(steps, Sj.call("cat", acc, acc), Str(seed)))),
          obj("reduce", Arr(Vector(steps, Sj.call("merge", acc, acc), lit(Arr(Vector(Str(seed))))))),
          obj("reduce", Arr(Vector(steps, Sj.call("cat", acc, acc), lit(Arr(Vector(Num("1"), Str(seed))))))),
          obj(
            "reduce",
            Arr(
              Vector(
                steps,
                Sj.call("set", acc, Sj.call("cat", Str("k"), obj("var", Str("$index"))), acc),
                lit(Obj(Vector("s" -> Str(seed))))
              )
            )
          )
        ).map(x => Sj.call("length", x))
    }

  /** Nested `n` deep around the program's operation bound (127 nested operations, jlvm.md section 1). */
  val deep: G[Sj] =
    (oneOf("!", "-", "abs", "+", "if", "literal"), oneOf(120, 125, 126, 127, 128, 130)).mapN { (op, n) =>
      (1 to n).foldLeft[Sj](Num("1")) { (x, _) =>
        op match {
          case "literal" => Arr(Vector(x))
          case "+"       => call("+", x, Num("1"))
          case "if"      => call("if", Bool(true), x)
          case o         => call(o, x)
        }
      }
    }
}
