package xyz.kd5ujc.jlvm

import java.nio.charset.StandardCharsets.UTF_8

import cats.Show

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{CanonicalGens, CanonicalValue}
import xyz.kd5ujc.numerics.Ratio

import org.scalacheck.Gen
import scodec.bits.{ByteVector, hex}
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object PureOpsSuite extends SimpleIOSuite with Checkers {

  private given Show[Op] = Show.show(_.wire)
  private given Show[CanonicalValue] = Show.fromToString

  private def run(op: Op, args: CanonicalValue*): Either[JlvmError, CanonicalValue] =
    PureOps(op, args.toVector).getOrElse(Left(JlvmError.Malformed(s"${op.wire} is evaluator-owned")))

  private def t(s:   String): CanonicalValue = CanonicalValue.text(s).fold(e => sys.error(e.toString), identity)
  private def i(n:   Long): CanonicalValue = CInt(BigInt(n))
  private def q(n:   Long, d: Long): CanonicalValue = CanonicalValue.number(Ratio.of(n, d).fold(e => sys.error(e.toString), identity))
  private def a(vs:  CanonicalValue*): CanonicalValue = CanonicalValue.array(vs.toVector).fold(e => sys.error(e.toString), identity)
  private def m(kvs: (String, CanonicalValue)*): CanonicalValue = CanonicalValue.map(kvs).fold(e => sys.error(e.toString), identity)
  private def ok(v:  CanonicalValue): Either[JlvmError, CanonicalValue] = Right(v)

  private def isType(r: Either[JlvmError, CanonicalValue]): Boolean = r match {
    case Left(JlvmError.Type(_, _)) => true
    case _                          => false
  }

  private val number: Gen[CanonicalValue] =
    Gen.oneOf(CanonicalGens.bigInt.map(CInt(_)), CanonicalGens.ratio.map(CanonicalValue.number))

  private val evaluatorOwned: Set[Op] = {
    import Op.{Map => MapOp, None => NoneOp, Some => SomeOp, _}
    scala.collection.immutable.Set[Op](If, And, Or, Default, Let, MapOp, Filter, Reduce, All, SomeOp, NoneOp, Find, Missing, MissingSome)
  }

  // ---- totality: the property that rules out unaccounted effects

  private def isVerifier(op: Op): Boolean =
    op.wire.endsWith("_verify") || scala.collection.immutable.Set[Op](Op.Poseidon, Op.Bn254Add, Op.Bn254Mul, Op.Bn254Pairing).contains(op)

  private def owned(op: Op): Boolean = evaluatorOwned(op) || isVerifier(op)

  test("every value op is total: any arguments give a value or a typed error, never an exception") {
    val args = Gen.choose(0, 3).flatMap(Gen.listOfN(_, CanonicalGens.value(2))).map(_.toVector)
    forall(Gen.zip(Gen.oneOf(Op.values.toSeq), args)) {
      case (op, vs) =>
        expect.same(owned(op), PureOps(op, vs).isEmpty)
    }
  }

  pureTest("the evaluator owns exactly the lazy, binding, callback, lookup and verification ops") {
    expect.same(Op.values.toSet.filter(owned), Op.values.toSet.filter(op => PureOps(op, Vector.empty).isEmpty))
  }

  // ---- equality and ordering

  test("== is structural, != is its negation, and both are symmetric") {
    forall(Gen.zip(CanonicalGens.value(2), CanonicalGens.value(2))) {
      case (x, y) =>
        val eq = run(Op.Eq, x, y)
        expect
          .same(ok(CBool(x == y)), eq)
          .and(expect.same(eq, run(Op.Eq, y, x)))
          .and(expect.same(eq, run(Op.StrictEq, x, y)))
          .and(expect.same(ok(CBool(x != y)), run(Op.Ne, x, y)))
    }
  }

  pureTest("strict equality: no coercion, but 2.0 and 2 are one number") {
    expect
      .same(ok(False), run(Op.Eq, t("1"), i(1)))
      .and(expect.same(ok(False), run(Op.Eq, CNull, False)))
      .and(expect.same(ok(False), run(Op.StrictNe, CNull, CNull)))
      .and(expect.same(ok(True), run(Op.Eq, q(4, 2), i(2))))
      .and(expect.same(ok(True), run(Op.Eq, a(i(1), t("x")), a(i(1), t("x")))))
  }

  test("numbers order exactly, and the three relations agree") {
    forall(Gen.zip(number, number)) {
      case (x, y) =>
        val c = Values.number(x).get.compare(Values.number(y).get)
        expect
          .same(ok(CBool(c < 0)), run(Op.Lt, x, y))
          .and(expect.same(ok(CBool(c <= 0)), run(Op.Le, x, y)))
          .and(expect.same(ok(CBool(c > 0)), run(Op.Gt, x, y)))
          .and(expect.same(ok(CBool(c >= 0)), run(Op.Ge, x, y)))
    }
  }

  test("text orders by code point, which is the order of its UTF-8 bytes (portable across languages)") {
    forall(Gen.zip(CanonicalGens.text, CanonicalGens.text)) {
      case (x, y) =>
        val bytes = ByteVector(x.getBytes(UTF_8)).toIndexedSeq.map(_ & 0xff)
        val other = ByteVector(y.getBytes(UTF_8)).toIndexedSeq.map(_ & 0xff)
        val byByte = bytes
          .zip(other)
          .collectFirst { case (p, r) if p != r => Integer.compare(p, r) }
          .getOrElse(Integer.compare(bytes.size, other.size))
        expect.same(ok(CBool(byByte < 0)), run(Op.Lt, t(x), t(y)))
    }
  }

  pureTest("between, and ordering across kinds is a type error") {
    expect
      .same(ok(True), run(Op.Lt, i(1), i(2), i(3)))
      .and(expect.same(ok(False), run(Op.Lt, i(1), i(3), i(3))))
      .and(expect.same(ok(True), run(Op.Le, i(1), i(3), i(3))))
      .and(expect(isType(run(Op.Lt, i(1), t("2"))), "number < text"))
      .and(expect(isType(run(Op.Gt, CNull, i(0))), "null > number"))
      .and(expect(run(Op.Gt, i(1), i(2), i(3)).isLeft, "> takes 2 arguments"))
  }

  // ---- arithmetic

  test("+ and * are exact, commutative and associative; - and / invert them") {
    forall(Gen.zip(number, number, number)) {
      case (x, y, z) =>
        val nonZero = y != CInt(BigInt(0))
        expect
          .same(run(Op.Add, x, y), run(Op.Add, y, x))
          .and(expect.same(run(Op.Mul, x, y), run(Op.Mul, y, x)))
          .and(expect.same(run(Op.Add, x, y, z), run(Op.Add, x, y).flatMap(s => run(Op.Add, s, z))))
          .and(expect.same(ok(x), run(Op.Add, x, y).flatMap(s => run(Op.Sub, s, y))))
          .and(expect(!nonZero || run(Op.Div, x, y).flatMap(d => run(Op.Mul, d, y)) == ok(x), "(x / y) * y == x"))
    }
  }

  test("% truncates like the host languages: x = y * trunc(x / y) + r, |r| < |y|, r has x's sign") {
    forall(Gen.zip(CanonicalGens.bigInt, CanonicalGens.bigInt.suchThat(_ != 0))) {
      case (x, y) =>
        val r = run(Op.Mod, CInt(x), CInt(y))
        expect
          .same(ok(CInt(x - y * (x / y))), r)
          .and(expect.same(ok(CInt(x % y)), r))
    }
  }

  test("rounding: half-even within 1/2, floor <= x < floor + 1, ceil - 1 < x <= ceil") {
    forall(CanonicalGens.ratio) { r =>
      val v = CanonicalValue.number(r)
      val half = Ratio.of(1, 2).toOption.get
      def n(x: Either[JlvmError, CanonicalValue]): Ratio = x.toOption.flatMap(Values.number).get
      val (rd, fl, ce) = (n(run(Op.Round, v)), n(run(Op.Floor, v)), n(run(Op.Ceil, v)))
      expect((r - rd).abs <= half, s"round($r) = $rd")
        .and(expect(fl <= r && r < fl + Ratio.integer(1), s"floor($r) = $fl"))
        .and(expect(ce - Ratio.integer(1) < r && r <= ce, s"ceil($r) = $ce"))
    }
  }

  pureTest("rounding at the tie points, division by zero, and numbers only") {
    expect
      .same(ok(i(2)), run(Op.Round, q(5, 2)))
      .and(expect.same(ok(i(4)), run(Op.Round, q(7, 2))))
      .and(expect.same(ok(i(-2)), run(Op.Round, q(-5, 2))))
      .and(expect.same(ok(i(3)), run(Op.RoundHalfUp, q(5, 2))))
      .and(expect.same(ok(i(-3)), run(Op.RoundHalfUp, q(-5, 2))))
      .and(expect.same(ok(q(1, 3)), run(Op.Div, i(1), i(3))))
      .and(expect.same(Left(JlvmError.Arithmetic("/", "division by zero")), run(Op.Div, i(1), i(0))))
      .and(expect.same(Left(JlvmError.Arithmetic("%", "division by zero")), run(Op.Mod, i(1), i(0))))
      .and(expect(isType(run(Op.Add, i(1), CNull)), "null arithmetic is a type error"))
      .and(expect(isType(run(Op.Add, i(1), t("2"))), "no numeric strings"))
      .and(expect(run(Op.Add).isLeft, "+ needs an argument"))
      .and(expect.same(ok(i(-5)), run(Op.Sub, i(5))))
      .and(expect.same(ok(i(1)), run(Op.Min, i(3), i(1), i(2))))
      .and(expect.same(ok(q(3, 2)), run(Op.Max, i(1), q(3, 2))))
  }

  pureTest("pow: integer exponents in -999..999, sizes bounded before computing") {
    expect
      .same(ok(i(1024)), run(Op.Pow, i(2), i(10)))
      .and(expect.same(ok(q(1, 8)), run(Op.Pow, i(2), i(-3))))
      .and(expect.same(ok(q(9, 4)), run(Op.Pow, q(3, 2), i(2))))
      .and(expect.same(ok(i(1)), run(Op.Pow, i(0), i(0))))
      .and(expect(run(Op.Pow, i(2), i(1000)).isLeft, "exponent bound"))
      .and(expect(run(Op.Pow, i(2), q(1, 2)).isLeft, "fractional exponent"))
      .and(expect(run(Op.Pow, i(0), i(-1)).isLeft, "zero to a negative power"))
      .and(
        expect(
          run(Op.Pow, CInt(BigInt(2).pow(1000)), i(999)) match {
            case Left(JlvmError.Limit(_)) => true
            case _                        => false
          },
          "result size bound"
        )
      )
  }

  // ---- text

  pureTest("cat renders numbers exactly and refuses non-terminating ones; arrays concatenate") {
    expect
      .same(ok(t("a1-2.5true")), run(Op.Cat, t("a"), i(1), q(-5, 2), True))
      .and(expect.same(ok(t("ab")), run(Op.Cat, t("a"), CNull, t("b"))))
      .and(expect(run(Op.Cat, q(1, 3)).isLeft, "1/3 has no decimal form"))
      .and(expect.same(ok(a(i(1), i(2), i(3))), run(Op.Cat, a(i(1)), a(i(2), i(3)))))
      .and(expect(isType(run(Op.Cat, a(i(1)), t("x"))), "no mixing arrays and text"))
  }

  test("split and join are inverse for a non-empty separator") {
    forall(Gen.zip(CanonicalGens.text, Gen.oneOf(Gen.oneOf(",", "ab", "😀", "\u0000"), CanonicalGens.text.map(_ + "x")))) {
      case (s, sep) =>
        expect.same(ok(t(s)), run(Op.Split, t(s), t(sep)).flatMap(parts => run(Op.Join, parts, t(sep))))
    }
  }

  pureTest("split keeps empty pieces; an empty separator is a domain error") {
    expect
      .same(ok(a(t(""), t("a"), t(""), t("b"), t(""))), run(Op.Split, t(",a,,b,"), t(",")))
      .and(expect.same(Left(JlvmError.Domain("split", "the separator must not be empty")), run(Op.Split, t("ab"), t(""))))
  }

  pureTest("length and substr count code points") {
    expect
      .same(ok(i(3)), run(Op.Length, t("a😀b")))
      .and(expect.same(ok(t("😀b")), run(Op.Substr, t("a😀b"), i(1))))
      .and(expect.same(ok(t("😀")), run(Op.Substr, t("a😀b"), i(1), i(1))))
      .and(expect.same(ok(t("log")), run(Op.Substr, t("jsonlogic"), i(4), i(-2))))
      .and(expect.same(ok(t("ic")), run(Op.Substr, t("jsonlogic"), i(-2))))
      .and(expect.same(ok(t("")), run(Op.Substr, t("abc"), i(10))))
      .and(expect.same(ok(i(2)), run(Op.Length, CBytes(hex"0102"))))
  }

  test("substr agrees with slicing the code points") {
    forall(Gen.zip(CanonicalGens.text, Gen.choose(-10, 10), Gen.choose(0, 10))) {
      case (s, from, len) =>
        val cps = Values.codePoints(s)
        val start = if (from < 0) math.max(0, cps.size + from) else math.min(from, cps.size)
        expect.same(ok(t(Values.fromCodePoints(cps.slice(start, start + len)))), run(Op.Substr, t(s), i(from.toLong), i(len.toLong)))
    }
  }

  test("substring search is linear-time KMP and agrees with String.indexOf") {
    val alphabet = Gen.oneOf("a", "b", "😀")
    val str = Gen.choose(0, 12).flatMap(Gen.listOfN(_, alphabet)).map(_.mkString)
    forall(Gen.zip(str, str, Gen.choose(0, 12))) {
      case (h, n, from) =>
        val start = math.min(from, h.length)
        expect.same(h.indexOf(n, start), Values.indexOf(h, n, start))
    }
  }

  pureTest("substr and slice clamp lengths and indices beyond 64 bits") {
    val huge = CInt(BigInt(10).pow(30))
    expect
      .same(ok(t("bc")), run(Op.Substr, t("abc"), i(1), huge))
      .and(expect.same(ok(t("abc")), run(Op.Substr, t("abc"), CInt(-huge.value))))
      .and(expect.same(ok(a(i(2))), run(Op.Slice, a(i(1), i(2)), i(1), huge)))
  }

  pureTest("numbers stay within MaxNumberBits, checked on operands and results") {
    val big = CInt(BigInt(2).pow(Values.MaxNumberBits - 1))
    def limited(r: Either[JlvmError, CanonicalValue]) = r match {
      case Left(JlvmError.Limit(_)) => true
      case _                        => false
    }
    val half = CInt(BigInt(2).pow(Values.MaxNumberBits - 2))
    expect
      .same(ok(big), run(Op.Add, half, half))
      .and(expect(limited(run(Op.Add, big, big)), "2^4096 has 4097 bits"))
      .and(expect(limited(run(Op.Mul, big, big)), "product too wide"))
      .and(expect(limited(run(Op.Add, CInt(BigInt(2).pow(5000)), i(1))), "operand too wide"))
      .and(expect(limited(run(Op.Lt, CInt(BigInt(2).pow(5000)), i(1))), "ordering reads the bound too"))
      .and(expect(limited(run(Op.BytesToInt, CBytes(ByteVector.fill(600)(1)))), "bytes_to_int bound"))
      .and(expect.same(ok(i(1)), run(Op.BytesToInt, CBytes(ByteVector.fill(600)(0) :+ 1.toByte))))
  }

  pureTest("results stay within MaxWeight: doubling text with cat hits a limit, not memory") {
    val start = t("x" * 4096)
    val doubled = (1 to 16).foldLeft[Either[JlvmError, CanonicalValue]](ok(start))((acc, _) => acc.flatMap(v => run(Op.Cat, v, v)))
    expect(
      doubled match {
        case Left(JlvmError.Limit(_)) => true
        case _                        => false
      },
      "doubling 4 KiB sixteen times exceeds 16 Mi weight"
    )
  }

  pureTest("case mapping is Unicode full mapping, locale-independent; trim strips Unicode white space") {
    expect
      .same(ok(t("STRASSE")), run(Op.Upper, t("straße")))
      .and(expect.same(ok(t("i")), run(Op.Lower, t("I"))))
      .and(expect.same(ok(t("x")), run(Op.Trim, t(" x "))))
      .and(expect.same(ok(t("x\ufeff")), run(Op.Trim, t("\u0085x\ufeff"))))
      .and(expect(isType(run(Op.Upper, i(1))), "text only"))
  }

  pureTest("startsWith and endsWith: null means absent") {
    expect
      .same(ok(True), run(Op.StartsWith, t("abc"), t("ab")))
      .and(expect.same(ok(True), run(Op.EndsWith, t("abc"), t("bc"))))
      .and(expect.same(ok(False), run(Op.StartsWith, CNull, t("a"))))
      .and(expect.same(ok(False), run(Op.EndsWith, t("a"), CNull)))
      .and(expect(isType(run(Op.StartsWith, i(1), t("1"))), "no coercion"))
  }

  // ---- collections

  test("reverse is an involution, unique is idempotent, length of a cat is the sum") {
    forall(Gen.zip(CanonicalGens.value(1), CanonicalGens.value(1)).map { case (x, y) => (a(x, y, x), a(y)) }) {
      case (xs, ys) =>
        expect
          .same(ok(xs), run(Op.Reverse, xs).flatMap(run(Op.Reverse, _)))
          .and(expect.same(run(Op.Unique, xs), run(Op.Unique, xs).flatMap(run(Op.Unique, _))))
          .and(expect.same(ok(i(4)), run(Op.Cat, xs, ys).flatMap(run(Op.Length, _))))
    }
  }

  pureTest("in, intersect, merge, slice, flatten, count") {
    expect
      .same(ok(True), run(Op.In, t("b"), t("abc")))
      .and(expect.same(ok(True), run(Op.In, i(2), a(i(1), i(2)))))
      .and(expect.same(ok(False), run(Op.In, CNull, a(CNull))))
      .and(expect.same(ok(True), run(Op.In, CBytes(hex"02"), CBytes(hex"010203"))))
      .and(expect(isType(run(Op.In, i(1), t("123"))), "no coercion of the needle"))
      .and(expect.same(ok(True), run(Op.Intersect, a(i(1)), a(i(2), i(1)))))
      .and(expect.same(ok(True), run(Op.Intersect, CNull, a())))
      .and(expect.same(ok(False), run(Op.Intersect, a(i(1)), CNull)))
      .and(expect.same(ok(a(i(1), i(2), i(3), a(i(4)))), run(Op.Merge, a(i(1)), i(2), a(i(3), a(i(4))))))
      .and(expect.same(ok(m("a" -> i(1), "b" -> i(3))), run(Op.Merge, m("a" -> i(1), "b" -> i(2)), m("b" -> i(3)))))
      .and(expect.same(ok(a(i(2), i(3))), run(Op.Slice, a(i(1), i(2), i(3), i(4)), i(1), i(-1))))
      .and(expect.same(ok(a(i(1), i(2), a(i(3)))), run(Op.Flatten, a(a(i(1), i(2)), a(a(i(3)))))))
      .and(expect.same(ok(i(3)), run(Op.Count, a(i(1), CNull, False))))
  }

  // ---- objects

  pureTest("keys, values and entries in code-point order; null is absent") {
    val o = m("b" -> i(2), "a" -> i(1), "é" -> i(3), "Z" -> i(0))
    expect
      .same(ok(a(t("Z"), t("a"), t("b"), t("é"))), run(Op.Keys, o))
      .and(expect.same(ok(a(i(0), i(1), i(2), i(3))), run(Op.Values, o)))
      .and(expect.same(ok(a(a(t("a"), i(1)))), run(Op.Entries, m("a" -> i(1)))))
      .and(expect.same(ok(CNull), run(Op.Keys, CNull)))
  }

  test("set then get reads back; set to null and unset both remove; has agrees with get") {
    forall(Gen.zip(CanonicalGens.value(2), CanonicalGens.text, CanonicalGens.value(1))) {
      case (base, key, v) =>
        val o = base match {
          case CMap(_) => base
          case other   => m("x" -> other)
        }
        val k = t(key)
        val removed = run(Op.Unset, o, k)
        expect
          .same(ok(v), run(Op.Set, o, k, v).flatMap(run(Op.Get, _, k)))
          .and(expect.same(removed, run(Op.Set, o, k, CNull)))
          .and(expect.same(ok(False), removed.flatMap(run(Op.Has, _, k))))
          .and(expect.same(ok(i(7)), removed.flatMap(run(Op.Get, _, k, i(7)))))
    }
  }

  // ---- bytes and hashing

  test("hex and unhex are inverse; int_to_bytes and bytes_to_int are inverse") {
    forall(Gen.zip(Gen.listOf(Gen.choose(Byte.MinValue, Byte.MaxValue)), CanonicalGens.bigInt.map(_.abs))) {
      case (bs, n) =>
        val b = CBytes(ByteVector(bs))
        expect
          .same(ok(b), run(Op.Hex, b).flatMap(run(Op.Unhex, _)))
          .and(expect.same(ok(CInt(n)), run(Op.IntToBytes, CInt(n)).flatMap(run(Op.BytesToInt, _))))
          .and(expect.same(ok(CInt(n)), run(Op.IntToBytes, CInt(n), i(40)).flatMap(run(Op.BytesToInt, _))))
    }
  }

  pureTest("hex_to_int takes bytes (the 0x surface) or 0x text; int_to_bytes widths") {
    expect
      .same(ok(i(255)), run(Op.HexToInt, CBytes(hex"ff")))
      .and(expect.same(ok(i(4096)), run(Op.HexToInt, t("0x1000"))))
      .and(expect(run(Op.HexToInt, t("0xFF")).isLeft, "lowercase only, as in metakit"))
      .and(expect(run(Op.HexToInt, t("ff")).isLeft, "0x prefix required"))
      .and(expect.same(ok(CBytes(hex"0001")), run(Op.IntToBytes, i(1), i(2))))
      .and(expect.same(ok(CBytes(ByteVector.empty)), run(Op.IntToBytes, i(0))))
      .and(expect(run(Op.IntToBytes, i(256), i(1)).isLeft, "does not fit"))
      .and(expect(run(Op.IntToBytes, i(-1)).isLeft, "unsigned"))
  }

  pureTest("hashes of bytes and of UTF-8 text") {
    expect
      .same(
        ok(CBytes(hex"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")),
        run(Op.Sha256, t("abc"))
      )
      .and(expect.same(run(Op.Sha256, t("abc")), run(Op.Sha256, CBytes(hex"616263"))))
      .and(
        expect.same(
          ok(CBytes(hex"a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a")),
          run(Op.Sha3_256, CBytes(ByteVector.empty))
        )
      )
      .and(
        expect.same(
          ok(CBytes(hex"0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8")),
          run(Op.Blake2b256, CBytes(ByteVector.empty))
        )
      )
      .and(expect(isType(run(Op.Sha256, i(1))), "numbers are not hashed implicitly"))
  }

  pureTest("truthiness, typeof and exists") {
    expect
      .same(ok(False), run(Op.Truthy, q(0, 1)))
      .and(expect.same(ok(True), run(Op.Truthy, q(1, 2))))
      .and(expect.same(ok(True), run(Op.Not, CBytes(ByteVector.empty))))
      .and(expect.same(ok(False), run(Op.Truthy, m())))
      .and(expect.same(ok(t("number")), run(Op.TypeOf, q(1, 2))))
      .and(expect.same(ok(t("bytes")), run(Op.TypeOf, CBytes(hex"00"))))
      .and(expect.same(ok(False), run(Op.Exists, i(1), CNull)))
  }
}
