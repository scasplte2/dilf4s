package xyz.kd5ujc.crypto.signing

import java.nio.charset.StandardCharsets.UTF_8

import cats.Show
import cats.effect.{IO, Resource}
import cats.syntax.all._

import xyz.kd5ujc.core.hash.{Hasher, Keccak256}

import fr.acinq.secp256k1.{Secp256k1 => Native}
import org.scalacheck.Gen
import scodec.bits.{ByteVector, hex}
import weaver.scalacheck.Checkers
import weaver.{Expectations, IOSuite}

/** `ecrecover` (BouncyCastle) against known Ethereum answers, and against libsecp256k1's own recovery. */
object EcrecoverSuite extends IOSuite with Checkers {

  /** libsecp256k1 is loaded once for the suite, as an effect, rather than while the suite object initialises. */
  type Res = Native
  def sharedResource: Resource[IO, Native] = Resource.eval(IO(Native.get()))

  /** A test that needs libsecp256k1, run inside IO so a failure stays in the test. */
  private def nativeTest(name: String)(body: Native ?=> Expectations): Unit = test(name)(native => IO(body(using native)))

  private val N = BigInt("fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141", 16)

  private def be32(i: BigInt): ByteVector = ByteVector(i.toByteArray).takeRight(32).padLeft(32)
  private def address(key: BigInt)(using native: Native): ByteVector =
    Ecrecover.address(ByteVector.view(native.pubkeyCreate(be32(key).toArray))).toOption.get

  /** libsecp256k1's recovery, as an address; its failures are None. */
  private def reference(digest: ByteVector, sig64: ByteVector, recid: Int)(using native: Native): Option[ByteVector] =
    Either
      .catchNonFatal(native.ecdsaRecover(sig64.toArray, digest.toArray, recid))
      .toOption
      .flatMap(pk => Ecrecover.address(ByteVector.view(pk)).toOption)

  private val scalar: Gen[BigInt] = Gen.listOfN(32, Gen.choose(Byte.MinValue, Byte.MaxValue)).map(b => BigInt(1, b.toArray).mod(N - 1) + 1)
  private val digest: Gen[ByteVector] = Gen.listOfN(32, Gen.choose(Byte.MinValue, Byte.MaxValue)).map(b => ByteVector(b))

  private given Show[(BigInt, ByteVector)] = Show.fromToString
  private given Show[(BigInt, BigInt, ByteVector)] = Show.fromToString

  pureTest("Keccak-256 known answers (Ethereum's padding, not SHA3-256's)") {
    expect
      .same(hex"c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470", Hasher[Keccak256].digest(ByteVector.empty).bytes)
      .and(
        expect.same(
          hex"4e03657aea45a94fc7d47ba826c8d667c0d1e6e33a64a036ec44f58fa12d6c45",
          Hasher[Keccak256].digest(ByteVector("abc".getBytes(UTF_8))).bytes
        )
      )
  }

  nativeTest("addresses of private keys 1 and 2, as every Ethereum tool derives them") {
    expect
      .same(hex"7e5f4552091a69125d5dfcb7b8c2659029395bdf", address(1))
      .and(expect.same(hex"2b5ad5c4795c026514f8317c7a215e218dccd6cf", address(2)))
  }

  test("a libsecp256k1 signature recovers its signer under one v, and v = 0/1 reads as 27/28") { native =>
    given Native = native
    forall(Gen.zip(scalar, digest)) {
      case (key, d) =>
        val sig = ByteVector.view(Signer.sign(key, d))
        val by = List(27, 28, 0, 1).map(v => Ecrecover.recover(d, sig :+ v.toByte))
        val signer = address(key)
        expect(by.take(2).count(_ == Right(Some(signer))) == 1, s"exactly one of v = 27, 28 gives the signer: $by")
          .and(expect.same(by.take(2), by.drop(2)))
    }
  }

  test("recovery agrees with libsecp256k1's for any low-s r and s, including r that is no point's x") { native =>
    given Native = native
    forall(Gen.zip(scalar, scalar.map(s => if (s > N / 2) N - s else s), digest)) {
      case (r, s, d) =>
        val sig64 = be32(r) ++ be32(s)
        List(0, 1).foldLeft(success) { (acc, recid) =>
          acc.and(expect.same(Right(reference(d, sig64, recid)), Ecrecover.recover(d, sig64 :+ (27 + recid).toByte)))
        }
    }
  }

  nativeTest("high s (EIP-2), and r or s zero or not below the order, recover nothing") {
    val d = hex"ce0677bb30baa8cf067c88db9811f4333d131bf8bcf12fe7065d211dce971008"
    val sig = ByteVector.view(Signer.sign(BigInt(7), d))
    val r = BigInt(1, sig.take(32).toArray)
    val s = BigInt(1, sig.drop(32).toArray)
    def recover(r: BigInt, s: BigInt) = List(27, 28).map(v => Ecrecover.recover(d, be32(r) ++ be32(s) :+ v.toByte))
    expect(recover(r, s).contains(Right(Some(address(7)))), "the low-s signature recovers")
      .and(expect.same(List(Right(None), Right(None)), recover(r, N - s)))
      .and(expect.same(List(Right(None), Right(None)), recover(BigInt(0), s)))
      .and(expect.same(List(Right(None), Right(None)), recover(r, BigInt(0))))
      .and(expect.same(List(Right(None), Right(None)), recover(N, s)))
  }

  pureTest("wrong sizes and other v values are malformed") {
    val d = ByteVector.fill(32)(1)
    val sig = ByteVector.fill(64)(1)
    val sizes = expect
      .same(
        Left(Ecrecover.Error.DigestLength(31)),
        Ecrecover.recover(d.drop(1), sig :+ 27.toByte)
      )
      .and(
        expect.same(
          Left(Ecrecover.Error.SignatureLength(64)),
          Ecrecover.recover(d, sig)
        )
      )
    List(2, 26, 29, 35, 255).foldLeft(sizes) { (checked, v) =>
      checked.and(
        expect.same(
          Left(Ecrecover.Error.InvalidRecoveryId(v)),
          Ecrecover.recover(d, sig :+ v.toByte)
        )
      )
    }
  }

  pureTest("malformed public keys have a distinct refusal") {
    Ecrecover.address(ByteVector.empty) match {
      case Left(Ecrecover.Error.InvalidPublicKey(cause)) => expect.same(classOf[IllegalArgumentException], cause.getClass)
      case other => failure(s"expected an invalid public key with its original decoder cause, got $other")
    }
  }

  pureTest("the shared secp256k1 order is SEC 2's n and BouncyCastle's curve order") {
    val bouncyCastle = BigInt(org.bouncycastle.crypto.ec.CustomNamedCurves.getByName("secp256k1").getN)
    expect.same(N, Secp256k1.Order).and(expect.same(bouncyCastle, Secp256k1.Order))
  }

  /** libsecp256k1 signs with RFC 6979 nonces and low s. */
  private object Signer {
    def sign(key: BigInt, digest: ByteVector)(using native: Native): Array[Byte] =
      Secp256k1.Ffi.signRfc6979(native, digest.toArray, be32(key).toArray)
  }
}
