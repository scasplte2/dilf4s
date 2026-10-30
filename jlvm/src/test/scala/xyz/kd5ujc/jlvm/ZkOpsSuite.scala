package xyz.kd5ujc.jlvm

import java.nio.charset.StandardCharsets.UTF_8

import cats.Show
import cats.effect.{IO, Resource}

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._

import io.circe.{Decoder, Json, parser}
import org.scalacheck.Gen
import scodec.bits.{ByteVector, hex}
import weaver.SimpleIOSuite
import weaver.scalacheck.{CheckConfig, Checkers}

object ZkOpsSuite extends SimpleIOSuite with Checkers {

  override def checkConfig: CheckConfig = CheckConfig.default.withMinimumSuccessful(40)

  private given Show[Op] = Show.show(_.wire)
  private given Show[CanonicalValue] = Show.fromToString

  private val zkOps: Set[Op] =
    Set(Op.Poseidon, Op.PmtVerify, Op.Groth16Verify, Op.EcvrfVerify, Op.Bn254Add, Op.Bn254Mul, Op.Bn254Pairing)

  private def run(op: Op, args: CanonicalValue*): Either[JlvmError, CanonicalValue] =
    ZkOps(op, args.toVector).getOrElse(Left(JlvmError.Malformed(s"${op.wire} is not a zk op")))

  private def a(vs: CanonicalValue*): CanonicalValue = CanonicalValue.array(vs.toVector).fold(e => sys.error(e.toString), identity)
  private def b(h:  ByteVector): CanonicalValue = CBytes(h)

  /** The outcome's category: `value`, or the error's kind. */
  private def kind(r: Either[JlvmError, CanonicalValue]): String = r match {
    case Left(JlvmError.Type(_, _))   => "Type"
    case Left(JlvmError.Arity(_, _))  => "Arity"
    case Left(JlvmError.Crypto(_, _)) => "Crypto"
    case Left(other)                  => other.getClass.getSimpleName
    case Right(_)                     => "value"
  }

  // ---- metakit conformance vectors (jlvm/src/test/resources/vectors/jlvm_zk_from_metakit.json)

  final private case class Case(
    category: String,
    op:       String,
    args:     Json,
    expected: Option[Json],
    error:    Option[String],
    status:   String,
    note:     Option[String]
  )

  private given Decoder[Case] =
    Decoder.forProduct7("category", "op", "args", "expected", "error", "status", "note")(Case.apply)

  private val cases: IO[List[Case]] =
    Resource
      .fromAutoCloseable(IO.blocking(getClass.getResourceAsStream("/vectors/jlvm_zk_from_metakit.json")))
      .use(in => IO.blocking(new String(in.readAllBytes(), UTF_8)))
      .flatMap(text => IO.fromEither(parser.parse(text).flatMap(_.hcursor.downField("cases").as[List[Case]])))

  private def surface(j: Json): CanonicalValue = JsonSurface.value(j).fold(e => sys.error(e.toString), identity)

  private def check(c: Case) = {
    val clue = s"${c.category} ${c.op} ${c.note.getOrElse("")}"
    val result = (Op.fromWire(c.op), surface(c.args)) match {
      case (Some(op), CArray(args)) => ZkOps(op, args)
      case other                    => sys.error(s"bad vector: $other")
    }
    (c.expected, c.error) match {
      case (Some(e), None) => expect(result == Some(Right(surface(e))), s"$clue: expected ${e.noSpaces}, got $result")
      case (None, Some(k)) => expect(result.map(kind).contains(k), s"$clue: expected a $k error, got $result")
      case _               => failure(s"$clue: a vector needs exactly one of expected and error")
    }
  }

  test("metakit zk conformance vectors: every value and error category is reproduced") {
    cases.map { cs =>
      cs.map(check)
        .foldLeft(expect.same(54, cs.size))(_ and _)
        .and(expect.same(48, cs.count(_.status == "ported")))
        .and(expect.same(6, cs.count(_.status == "adapted")))
        .and(expect.same(zkOps.map(_.wire), cs.map(_.op).toSet))
    }
  }

  // ---- dispatch and totality

  pureTest("ZkOps implements exactly the seven zk ops and returns None for every other op") {
    Op.values.toList.map(op => expect.same(zkOps(op), ZkOps(op, Vector.empty).isDefined)).foldLeft(success)(_ and _)
  }

  private val rawBytes: Gen[CanonicalValue] =
    Gen
      .oneOf(0, 1, 31, 32, 33, 64, 80, 128, 192, 356)
      .flatMap(n => Gen.listOfN(n, Gen.choose(Byte.MinValue, Byte.MaxValue)))
      .map(bs => CBytes(ByteVector(bs)))

  private val scalar: Gen[CanonicalValue] =
    Gen.oneOf(rawBytes, Gen.choose(-3L, 300L).map(n => CInt(BigInt(n))), Gen.const(CNull), Gen.const(True))

  private val argument: Gen[CanonicalValue] =
    Gen.oneOf(
      scalar,
      Gen.choose(0, 3).flatMap(Gen.listOfN(_, scalar)).map(xs => a(xs*)),
      Gen.choose(0, 2).flatMap(Gen.listOfN(_, Gen.listOfN(2, rawBytes).map(xs => a(xs*)))).map(xs => a(xs*))
    )

  test("every zk op is total on random exact-width bytes, integers and nested arrays: a value or a typed error") {
    val args = Gen.choose(0, 4).flatMap(Gen.listOfN(_, argument)).map(_.toVector)
    forall(Gen.zip(Gen.oneOf(zkOps.toList), args)) {
      case (op, vs) =>
        val r = ZkOps(op, vs)
        expect(r.isDefined, s"${op.wire} must be implemented")
          .and(expect(r.map(kind).forall(Set("value", "Type", "Arity", "Crypto")), s"unexpected outcome: $r"))
    }
  }

  // ---- shapes

  private val one = b(hex"0000000000000000000000000000000000000000000000000000000000000001")
  private val two = b(hex"0000000000000000000000000000000000000000000000000000000000000002")

  /** r - 1, so `bn254_mul(P, minusOne)` is `-P`. */
  private val minusOne = b(hex"30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000000")

  private val g1Gen = b(ByteVector.fromLong(1L).padLeft(32) ++ ByteVector.fromLong(2L).padLeft(32))
  private val g2Gen = b(
    hex"198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa"
  )
  private val infinity = b(ByteVector.low(64))

  pureTest("poseidon takes its elements as arguments or as one array, and returns 32 bytes") {
    val variadic = run(Op.Poseidon, one, two)
    expect
      .same(variadic, run(Op.Poseidon, a(one, two)))
      .and(expect.same(Right(b(hex"115cc0f5e7d690413df64c6b9662e9cf2a3617f2743245519e19607a4417189a")), variadic))
      .and(expect.same("Arity", kind(run(Op.Poseidon, a(one, one, one, one, one)))))
      .and(expect.same("Type", kind(run(Op.Poseidon, a()))))
      .and(expect.same("Type", kind(run(Op.Poseidon, CInt(BigInt(1))))))
  }

  pureTest("bn254_pairing takes pairs as arguments or one array of pairs; e(P, Q) * e(-P, Q) is the identity") {
    val neg = run(Op.Bn254Mul, g1Gen, minusOne).fold(e => sys.error(e.toString), identity)
    expect
      .same(Right(True), run(Op.Bn254Pairing, a(g1Gen, g2Gen), a(neg, g2Gen)))
      .and(expect.same(Right(True), run(Op.Bn254Pairing, a(a(g1Gen, g2Gen), a(neg, g2Gen)))))
      .and(expect.same(Right(False), run(Op.Bn254Pairing, a(g1Gen, g2Gen))))
      .and(expect.same(Right(True), run(Op.Bn254Pairing)))
      .and(expect.same(Right(True), run(Op.Bn254Pairing, a(infinity, g2Gen))))
      .and(expect.same("Crypto", kind(run(Op.Bn254Pairing, a(g1Gen)))))
      .and(expect.same("Type", kind(run(Op.Bn254Pairing, g1Gen))))
      .and(expect.same("Crypto", kind(run(Op.Bn254Pairing, a(Vector.fill(ZkOps.MaxPairs + 1)(a(infinity, g2Gen))*)))))
  }

  pureTest("bn254_add and bn254_mul agree: P + P = 2P, P + (-P) = O, 0P = O, and any 256-bit scalar is accepted") {
    expect
      .same(run(Op.Bn254Mul, g1Gen, two), run(Op.Bn254Add, g1Gen, g1Gen))
      .and(expect.same(Right(infinity), run(Op.Bn254Mul, g1Gen, minusOne).flatMap(n => run(Op.Bn254Add, g1Gen, n))))
      .and(expect.same(Right(infinity), run(Op.Bn254Mul, g1Gen, b(ByteVector.low(32)))))
      .and(expect.same(Right(g1Gen), run(Op.Bn254Add, infinity, g1Gen)))
      .and(expect(run(Op.Bn254Mul, g1Gen, b(ByteVector.high(32))).isRight, "mul takes any 256-bit scalar"))
      .and(expect.same("Arity", kind(run(Op.Bn254Add, g1Gen))))
      .and(expect.same("Type", kind(run(Op.Bn254Mul, g1Gen, CInt(BigInt(2))))))
  }

  pureTest("pmt_verify: a depth-0 proof checks leaf == root; shapes and bounds are typed errors") {
    val zero = CInt(BigInt(0))
    expect
      .same(Right(True), run(Op.PmtVerify, one, one, zero, a()))
      .and(expect.same(Right(False), run(Op.PmtVerify, one, two, zero, a())))
      .and(expect.same("Crypto", kind(run(Op.PmtVerify, one, one, CInt(BigInt(-1)), a()))))
      .and(expect.same("Crypto", kind(run(Op.PmtVerify, one, one, CInt(BigInt(1)), a()))))
      .and(expect.same("Crypto", kind(run(Op.PmtVerify, one, one, zero, a(Vector.fill(ZkOps.MaxSiblings + 1)(one)*)))))
      .and(expect.same("Type", kind(run(Op.PmtVerify, one, one, one, a()))))
      .and(expect.same("Type", kind(run(Op.PmtVerify, one, one, zero, one))))
      .and(expect.same("Arity", kind(run(Op.PmtVerify, one, one, zero))))
  }

  pureTest("ecvrf_verify and groth16_verify: invalid is false, wrong widths are Crypto errors, wrong kinds Type errors") {
    val pk = b(ByteVector.low(32))
    val proof = b(ByteVector.low(80))
    val invalid = CanonicalValue.map(List("valid" -> False)).fold(e => sys.error(e.toString), identity)
    expect
      .same(Right(invalid), run(Op.EcvrfVerify, pk, b(ByteVector.empty), proof))
      .and(expect.same("Crypto", kind(run(Op.EcvrfVerify, pk, b(ByteVector.empty), b(ByteVector.low(79))))))
      .and(expect.same("Type", kind(run(Op.EcvrfVerify, pk, CInt(BigInt(1)), proof))))
      .and(expect.same("Arity", kind(run(Op.EcvrfVerify, pk, proof))))
      .and(expect.same("Crypto", kind(run(Op.Groth16Verify, b(ByteVector.low(32)), b(ByteVector.empty), b(ByteVector.low(355))))))
      .and(expect.same(Right(False), run(Op.Groth16Verify, b(ByteVector.low(32)), b(ByteVector.empty), b(ByteVector.low(356)))))
  }
}
