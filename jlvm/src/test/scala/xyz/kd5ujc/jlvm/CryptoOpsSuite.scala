package xyz.kd5ujc.jlvm

import cats.effect.{IO, Resource}
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.crypto.bls.Bls12381
import xyz.kd5ujc.crypto.signing._

import scodec.bits.ByteVector
import weaver.{Expectations, IOSuite}

object CryptoOpsSuite extends IOSuite {

  final case class Backends(jlvm: Jlvm, secp: Secp256k1, bls: Bls12381)
  type Res = Backends
  def sharedResource: Resource[IO, Backends] =
    Resource.eval((Jlvm.load[IO], Secp256k1.load[IO], Bls12381.load[IO]).mapN(Backends.apply))

  private def seed(n: Int): ByteVector = ByteVector.fill(32)(n.toByte)
  private val message = ByteVector("transfer 10 to bob".getBytes("UTF-8"))
  private val other = ByteVector("transfer 99 to eve".getBytes("UTF-8"))

  private def verify(jlvm: Jlvm, op: Op, pk: CanonicalValue, msg: ByteVector, sig: ByteVector): Either[JlvmError, CanonicalValue] = {
    val program = Program.from(Expr.Apply(op, Vector("pk", "msg", "sig").map(k => Expr.Var(VarRef.Data(Vector(k)), None)))).toOption.get
    val data = CanonicalValue.map(List("pk" -> pk, "msg" -> CBytes(msg), "sig" -> CBytes(sig))).toOption.get
    jlvm.evaluate(program, data, Map.empty, Config()).result
  }

  private def crypto(r: Either[JlvmError, CanonicalValue]): Boolean = r match {
    case Left(JlvmError.Crypto(_, _)) => true
    case _                            => false
  }

  /** A good signature verifies; another message gives false; a truncated signature is malformed input. */
  private def scheme(jlvm: Jlvm, op: Op, pk: ByteVector, sig: ByteVector, msg: ByteVector = message): Expectations =
    expect
      .same(Right(True), verify(jlvm, op, CBytes(pk), msg, sig))
      .and(expect.same(Right(False), verify(jlvm, op, CBytes(pk), other, sig)))
      .and(expect(crypto(verify(jlvm, op, CBytes(pk), msg, sig.dropRight(1))), s"${op.wire}: short signature"))
      .and(expect(verify(jlvm, op, CInt(BigInt(1)), msg, sig).isLeft, "a number is not a key"))

  test("ed25519_verify") { b =>
    IO {
      val kp = Ed25519.seedDerivation.fromSeed(seed(1)).toOption.get
      scheme(b.jlvm, Op.Ed25519Verify, kp.verifying.bytes, Ed25519.signer.sign(kp.signing, message).bytes)
    }
  }

  test("secp256k1_verify (ECDSA over SHA-256 of the message)") { b =>
    IO {
      val kp = b.secp.ecdsaSeedDerivation.fromSeed(seed(2)).toOption.get
      scheme(b.jlvm, Op.Secp256k1Verify, kp.verifying.bytes, b.secp.ecdsaSigner.sign(kp.signing, message).bytes)
    }
  }

  test("bip340_verify takes exactly 32-byte messages") { b =>
    IO {
      val kp = b.secp.bip340SeedDerivation.fromSeed(seed(3)).toOption.get
      val m32 = ByteVector.fill(32)(7)
      val sig = b.secp.bip340SignWithAux(kp.signing, Message32.from(m32).toOption.get, ByteVector.fill(32)(0)).toOption.get
      expect
        .same(Right(True), verify(b.jlvm, Op.Bip340Verify, CBytes(kp.verifying.bytes), m32, sig.bytes))
        .and(expect.same(Right(False), verify(b.jlvm, Op.Bip340Verify, CBytes(kp.verifying.bytes), ByteVector.fill(32)(8), sig.bytes)))
        .and(expect(crypto(verify(b.jlvm, Op.Bip340Verify, CBytes(kp.verifying.bytes), message, sig.bytes)), "not 32 bytes"))
    }
  }

  test("bls_verify and bls_aggregate_verify") { b =>
    IO {
      val kps = List(4, 5, 6).map(n => b.bls.seedDerivation.fromSeed(seed(n)).toOption.get)
      val sigs = kps.map(kp => b.bls.sign(kp.signing, message).toOption.get)
      val agg = b.bls.aggregate(sigs).toOption.get
      val keys = CanonicalValue.array(kps.map(kp => CBytes(kp.verifying.bytes): CanonicalValue).toVector).toOption.get
      scheme(b.jlvm, Op.BlsVerify, kps.head.verifying.bytes, sigs.head.bytes)
        .and(expect.same(Right(True), verify(b.jlvm, Op.BlsAggregateVerify, keys, message, agg.bytes)))
        .and(expect.same(Right(False), verify(b.jlvm, Op.BlsAggregateVerify, keys, other, agg.bytes)))
        .and(expect(crypto(verify(b.jlvm, Op.BlsAggregateVerify, keys, message, agg.bytes.dropRight(1))), "short aggregate"))
    }
  }

  test("a multisig: at least two of three keys signed") { b =>
    IO {
      val kps = List(7, 8, 9).map(n => Ed25519.seedDerivation.fromSeed(seed(n)).toOption.get)
      val sigs = kps.map(kp => Ed25519.signer.sign(kp.signing, message))
      def entry(i: Int, signed: Boolean): CanonicalValue =
        CanonicalValue
          .map(List("pk" -> CBytes(kps(i).verifying.bytes), "sig" -> CBytes(if (signed) sigs(i).bytes else ByteVector.fill(64)(0))))
          .toOption
          .get
      val program = Parser
        .parse(
          """{"some": [{"var": "signers"},
            |  {"ed25519_verify": [{"var": "$current.pk"}, {"var": "msg"}, {"var": "$current.sig"}]}, 2]}""".stripMargin
        )
        .toOption
        .get
      def run(signed: List[Boolean]) = {
        val data = CanonicalValue
          .map(
            List(
              "msg"     -> CBytes(message),
              "signers" -> CanonicalValue.array(signed.zipWithIndex.map { case (s, i) => entry(i, s) }.toVector).toOption.get
            )
          )
          .toOption
          .get
        b.jlvm.evaluate(program, data, Map.empty, Config()).result
      }
      expect
        .same(Right(True), run(List(true, false, true)))
        .and(expect.same(Right(False), run(List(true, false, false))))
    }
  }
}
