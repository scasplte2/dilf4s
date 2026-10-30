package xyz.kd5ujc.crypto.signing

import cats.effect.IO
import cats.effect.std.SecureRandom
import cats.syntax.all._

import cafe.cryptography.curve25519.RistrettoElement
import org.scalacheck.Gen
import scodec.bits.{ByteVector, HexStringSyntax}
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object Ristretto255Suite extends SimpleIOSuite with Checkers {

  private val bytes32: Gen[ByteVector] = Gen.listOfN(32, Gen.choose(Byte.MinValue, Byte.MaxValue)).map(ByteVector(_))
  private val message: Gen[ByteVector] = Gen.listOf(Gen.choose(Byte.MinValue, Byte.MaxValue)).map(ByteVector(_))

  implicit private val showTriple: cats.Show[(ByteVector, ByteVector, ByteVector)] =
    cats.Show.show { case (a, b, c) => s"${a.toHex}/${b.toHex}/${c.toHex}" }

  private val C = Ristretto255Schnorr.codec
  private val V = Ristretto255Schnorr.verifier

  /** RFC 9591 Appendix E.2, FROST(ristretto255, SHA-512). */
  private val groupSecret = hex"1b25a55e463cfd15cf14a5d3acc3d15053f08da49c8afcf3ab265f2ebc4f970b"
  private val groupPublic = hex"e2a62f39eede11269e3bd5a7d97554f5ca384f9f6d3dd9c3c0d05083c7254f57"
  private val groupSignature =
    hex"fc45655fbc66bbffad654ea4ce5fdae253a49a64ace25d9adb62010dd9fb25552164141787162e5b4cab915b4aa45d94655dbb9ed7c378a53b980a0be220a802"

  pureTest("RFC 9591 vector: group key derivation and the threshold signature verify") {
    val sk = C.signingKey(groupSecret).toOption.get
    expect
      .same(groupPublic, Ristretto255Schnorr.publicKeyOf.verifyingKey(sk).bytes)
      .and(expect(V.verify(VerifyingKey(groupPublic), hex"74657374", Signature(groupSignature)), "aggregate signature verifies"))
      .and(expect(!V.verify(VerifyingKey(groupPublic), hex"74657375", Signature(groupSignature)), "other message rejected"))
  }

  test("signatures verify, tampering is rejected, encodings round-trip") {
    forall(Gen.zip(bytes32, bytes32, message)) {
      case (seed, rand, msg) =>
        val kp = Ristretto255Schnorr.seedDerivation.fromSeed(seed).toOption.get
        val sig = Ristretto255Schnorr.signWithRandomness(kp.signing, msg, rand).toOption.get
        val tampered = sig.bytes.update(40, (sig.bytes(40) ^ 1).toByte)
        expect(V.verify(kp.verifying, msg, sig), "verifies")
          .and(expect(!V.verify(kp.verifying, msg ++ hex"00", sig), "extended message rejected"))
          .and(expect(!V.verify(kp.verifying, msg, Signature(tampered)), "tampered z rejected"))
          .and(expect.same(Right(sig), C.signature(sig.bytes)))
          .and(expect.same(Right(kp.verifying), C.verifyingKey(kp.verifying.bytes)))
          .and(expect.same(Right(sig), Ristretto255Schnorr.signWithRandomness(kp.signing, msg, rand)))
    }
  }

  test("the randomised signer gives distinct, valid signatures") {
    SecureRandom.javaSecuritySecureRandom[IO].flatMap { implicit R =>
      val kp = Ristretto255Schnorr.seedDerivation.fromSeed(ByteVector.fill(32)(7)).toOption.get
      val signer = Ristretto255Schnorr.signer[IO]
      (signer.sign(kp.signing, hex"01"), signer.sign(kp.signing, hex"01")).tupled.map {
        case (a, b) =>
          expect(a != b, "fresh nonces").and(expect(V.verify(kp.verifying, hex"01", a))).and(expect(V.verify(kp.verifying, hex"01", b)))
      }
    }
  }

  pureTest("identity elements, non-canonical scalars and zero keys are rejected") {
    val identity = ByteVector.view(RistrettoElement.IDENTITY.compress().toByteArray)
    val kp = Ristretto255Schnorr.seedDerivation.fromSeed(ByteVector.fill(32)(1)).toOption.get
    val sig = Ristretto255Schnorr.signWithRandomness(kp.signing, hex"", ByteVector.fill(32)(2)).toOption.get
    // z + L is the same scalar, non-canonically encoded
    val l = hex"edd3f55c1a631258d69cf7a2def9de1400000000000000000000000000000010"
    val zPlusL = ByteVector
      .view((BigInt(1, sig.bytes.drop(32).reverse.toArray) + BigInt(1, l.reverse.toArray)).toByteArray)
      .reverse
      .padRight(32)
      .take(32)
    expect(C.verifyingKey(identity).isLeft, "identity verifying key")
      .and(expect(!V.verify(VerifyingKey(identity), hex"", sig), "identity key never verifies"))
      .and(expect(C.signature(identity ++ sig.bytes.drop(32)).isLeft, "identity R"))
      .and(expect(!V.verify(kp.verifying, hex"", Signature(sig.bytes.take(32) ++ zPlusL)), "non-canonical z"))
      .and(expect(C.signingKey(ByteVector.fill(32)(0)).isLeft, "zero secret"))
      .and(expect(C.signingKey(l).isLeft, "secret >= L"))
      .and(expect(Ristretto255Schnorr.signWithRandomness(kp.signing, hex"", hex"00").isLeft, "short randomness"))
  }

}
