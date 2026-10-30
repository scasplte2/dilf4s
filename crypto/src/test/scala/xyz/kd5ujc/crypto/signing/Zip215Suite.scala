package xyz.kd5ujc.crypto.signing

import cafe.cryptography.curve25519.{CompressedEdwardsY, Constants, Scalar}
import org.bouncycastle.crypto.digests.SHA512Digest
import scodec.bits.{ByteVector, HexStringSyntax}
import weaver.SimpleIOSuite

/** ZIP-215 against RFC 8032 strict verification, on signatures constructed from first principles. */
object Zip215Suite extends SimpleIOSuite {

  private def sha512(b: ByteVector): Array[Byte] = {
    val d = new SHA512Digest(); val out = new Array[Byte](64); d.update(b.toArray, 0, b.size.toInt); d.doFinal(out, 0); out
  }

  private val seed = ByteVector.fill(32)(0x42)
  private val kp = Ed25519.seedDerivation.fromSeed(seed).toOption.get
  private val a: Scalar = {
    val h = sha512(seed).take(32)
    h(0) = (h(0) & 248).toByte; h(31) = ((h(31) & 127) | 64).toByte
    Scalar.fromBits(h).reduce()
  }
  private val message = ByteVector("zip215".getBytes("UTF-8"))

  /** An order-8 torsion point. */
  private val torsion = new CompressedEdwardsY(hex"26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05".toArray).decompress()

  /** S = r + H(R' || A || M) a, with R' = rB + T: valid only under the cofactored equation. */
  private def mixedOrderSignature: ByteVector = {
    val r = Scalar.fromBytesModOrderWide(sha512(ByteVector("nonce".getBytes("UTF-8"))))
    val rPrime = Constants.ED25519_BASEPOINT.multiply(r).add(torsion).compress().toByteArray
    val k = Scalar.fromBytesModOrderWide(sha512(ByteVector(rPrime) ++ kp.verifying.bytes ++ message))
    ByteVector(rPrime) ++ ByteVector(k.multiplyAndAdd(a, r).toByteArray)
  }

  pureTest("the torsion point is small-order and the derived key matches RFC 8032") {
    expect(torsion.isSmallOrder && !torsion.isIdentity)
      .and(expect.same(kp.verifying.bytes, ByteVector(Constants.ED25519_BASEPOINT.multiply(a).compress().toByteArray)))
  }

  pureTest("a mixed-order signature (valid only cofactored) is accepted by ZIP-215") {
    expect(Ed25519.verifier.verify(kp.verifying, message, Signature[Ed25519](mixedOrderSignature)))
  }

  /** r = 0, so R is the identity, encoded non-canonically as y = p + 1; S = H(R || A || M) a. */
  private def nonCanonicalRSignature: ByteVector = {
    val rBytes = hex"eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f"
    val k = Scalar.fromBytesModOrderWide(sha512(rBytes ++ kp.verifying.bytes ++ message))
    rBytes ++ ByteVector(k.multiply(a).toByteArray)
  }

  pureTest("a non-canonical R encoding: ZIP-215 accepts, BouncyCastle's strict RFC 8032 rejects") {
    val sig = Signature[Ed25519](nonCanonicalRSignature)
    expect(Ed25519.verifier.verify(kp.verifying, message, sig), "ZIP-215 accepts non-canonical R")
      .and(expect(!Ed25519.strictVerifier.verify(kp.verifying, message, sig), "strict verification requires canonical R"))
  }

  pureTest("a non-canonical S (S + l) is rejected by both rules") {
    val l = BigInt("1000000000000000000000000000000014def9dea2f79cd65812631a5cf5d3ed", 16)
    val sig = Ed25519.signer.sign(kp.signing, message).bytes
    val sPlusL = BigInt(1, sig.drop(32).reverse.toArray) + l
    val bumped = Signature[Ed25519](sig.take(32) ++ ByteVector(sPlusL.toByteArray).reverse.padRight(32).take(32))
    expect(!Ed25519.verifier.verify(kp.verifying, message, bumped))
      .and(expect(!Ed25519.strictVerifier.verify(kp.verifying, message, bumped)))
  }

  pureTest("non-canonical point encodings decode under ZIP-215 (y = p + 1 is the identity)") {
    val nonCanonicalIdentity = hex"eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f"
    expect(Ed25519.Zip215.decode(nonCanonicalIdentity).exists(_.isIdentity))
      .and(expect(Ed25519.codec.verifyingKey(nonCanonicalIdentity).isRight, "ZIP-215 keys may be non-canonical"))
  }

  pureTest("honest signatures verify under both rules") {
    val sig = Ed25519.signer.sign(kp.signing, message)
    expect(Ed25519.verifier.verify(kp.verifying, message, sig)).and(expect(Ed25519.strictVerifier.verify(kp.verifying, message, sig)))
  }
}
