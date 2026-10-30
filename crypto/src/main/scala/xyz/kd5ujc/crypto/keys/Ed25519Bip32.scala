package xyz.kd5ujc.crypto.keys

import cats.syntax.all._

import xyz.kd5ujc.crypto.signing._

import cafe.cryptography.curve25519.{CompressedEdwardsY, Constants, Scalar}
import org.bouncycastle.crypto.digests.SHA512Digest
import scodec.bits.ByteVector

/**
 * BIP32-Ed25519 (Khovratovich–Law, as used by Cardano/Icarus, CIP-0003). A signing key is the 64-byte extended scalar
 * `kL || kR`; signatures are ordinary Ed25519 signatures, verified by the ZIP-215 [[Ed25519]] verifier.
 *
 * Secret-key arithmetic is fixed-width little-endian byte addition with no data-dependent branches, and secret-scalar
 * multiplications use curve25519-elisabeth's constant-time basepoint table.
 */
sealed trait Ed25519Bip32

object Ed25519Bip32 {

  private def sha512(b: ByteVector): Array[Byte] = {
    val d = new SHA512Digest(); val out = new Array[Byte](64); d.update(b.toArray, 0, b.size.toInt); d.doFinal(out, 0); out
  }

  /** `kL` must be a multiple of 8 below 2^255 (clamped, and kept so by derivation). */
  private def validKL(kL: ByteVector): Boolean = kL.size == 32L && (kL(0) & 0x07) == 0 && (kL(31) & 0x80) == 0

  implicit val codec: SchemeCodec[Ed25519Bip32] = new SchemeCodec[Ed25519Bip32] {
    val name: String = "Ed25519-BIP32"
    def signingKey(bytes: ByteVector): Either[CryptoError, SigningKey[Ed25519Bip32]] =
      Either.cond(
        bytes.size == 64L && validKL(bytes.take(32)),
        new SigningKey[Ed25519Bip32](bytes),
        CryptoError.InvalidKey("expected 64-byte kL || kR with a clamped kL")
      )
    def verifyingKey(bytes: ByteVector): Either[CryptoError, VerifyingKey[Ed25519Bip32]] =
      Ed25519.codec.verifyingKey(bytes).map(k => VerifyingKey[Ed25519Bip32](k.bytes))
    def signature(bytes: ByteVector): Either[CryptoError, Signature[Ed25519Bip32]] =
      Ed25519.codec.signature(bytes).map(s => Signature[Ed25519Bip32](s.bytes))
  }

  private def scalarOf(kL: ByteVector): Scalar = Scalar.fromBytesModOrder(kL.toArray)

  implicit val publicKeyOf: PublicKeyOf[Ed25519Bip32] = (key: SigningKey[Ed25519Bip32]) =>
    VerifyingKey[Ed25519Bip32](
      ByteVector.view(Constants.ED25519_BASEPOINT_TABLE.multiply(scalarOf(key.bytes.take(32))).compress().toByteArray)
    )

  /** RFC 8032 signing with an expanded key: r = H(kR || M), S = r + H(R || A || M) kL. */
  implicit val signer: Signer[Ed25519Bip32] = (key: SigningKey[Ed25519Bip32], message: ByteVector) => {
    val a = publicKeyOf.verifyingKey(key).bytes
    val r = Scalar.fromBytesModOrderWide(sha512(key.bytes.drop(32) ++ message))
    val rPoint = ByteVector.view(Constants.ED25519_BASEPOINT_TABLE.multiply(r).compress().toByteArray)
    val k = Scalar.fromBytesModOrderWide(sha512(rPoint ++ a ++ message))
    Signature[Ed25519Bip32](rPoint ++ ByteVector.view(k.multiplyAndAdd(scalarOf(key.bytes.take(32)), r).toByteArray))
  }

  implicit val verifier: Verifier[Ed25519Bip32] =
    (key: VerifyingKey[Ed25519Bip32], message: ByteVector, signature: Signature[Ed25519Bip32]) =>
      Ed25519.verifier.verify(VerifyingKey[Ed25519](key.bytes), message, Signature[Ed25519](signature.bytes))

  /** Root key from 96 bytes (`kL || kR || chainCode`), clamped per CIP-0003. */
  def root(secret: Secret): Either[KeyError, ExtendedKey[Ed25519Bip32]] =
    Either.cond(secret.bytes.size == 96L, secret.bytes, KeyError.InvalidDerivation("root secret must be 96 bytes")).flatMap { b =>
      val clamped = b.update(0, (b(0) & 0xf8).toByte).update(31, ((b(31) & 0x1f) | 0x40).toByte)
      codec.signingKey(clamped.take(64)).bimap(e => KeyError.InvalidDerivation(e.toString), ExtendedKey(_, clamped.drop(64)))
    }

  /** Icarus (CIP-0003) master key: PBKDF2-HMAC-SHA512(passphrase, entropy, 4096, 96), clamped. */
  def icarus(entropy: Secret, passphrase: String = ""): Either[KeyError, ExtendedKey[Ed25519Bip32]] =
    root(Secret(Hmac.pbkdf2Sha512(ByteVector(passphrase.getBytes("UTF-8")), entropy.bytes, 4096, 96)))

  // ------------------------------------------------------------------ fixed-width little-endian arithmetic

  /** `(a + b) mod 2^256`, byte by byte with a carry; the same work for every input. */
  private def add256(a: ByteVector, b: ByteVector): ByteVector =
    ByteVector.view(
      (0 until 32)
        .foldLeft((Vector.empty[Byte], 0)) {
          case ((acc, carry), i) =>
            val sum = (a(i.toLong) & 0xff) + (b(i.toLong) & 0xff) + carry
            (acc :+ sum.toByte, sum >>> 8)
        }
        ._1
        .toArray
    )

  /** `8 * zL` for the first 28 bytes of `z`, as a 32-byte little-endian value (fits: 227 bits). */
  private def times8(z: ByteVector): ByteVector = {
    val zl = z.take(28).padRight(32)
    ByteVector.view(
      (0 until 32).map(i => (((zl(i.toLong) & 0xff) << 3) | (if (i == 0) 0 else (zl(i.toLong - 1) & 0xff) >>> 5)).toByte).toArray
    )
  }

  def child(parent: ExtendedKey[Ed25519Bip32], index: ChildIndex): Either[KeyError, ExtendedKey[Ed25519Bip32]] = {
    val (kL, kR) = (parent.key.bytes.take(32), parent.key.bytes.drop(32))
    val idx = ChildIndex.le32(index)
    val a = publicKeyOf.verifyingKey(parent.key).bytes
    val (zData, ccData) =
      if (index.hardened) (ByteVector(0x00) ++ kL ++ kR ++ idx, ByteVector(0x01) ++ kL ++ kR ++ idx)
      else (ByteVector(0x02) ++ a ++ idx, ByteVector(0x03) ++ a ++ idx)
    val z = Hmac.sha512(parent.chainCode, zData)
    val childKey = add256(times8(z), kL) ++ add256(z.drop(32), kR)
    codec
      .signingKey(childKey)
      .bimap(e => KeyError.InvalidDerivation(e.toString), ExtendedKey(_, Hmac.sha512(parent.chainCode, ccData).drop(32)))
  }

  def derive(root: ExtendedKey[Ed25519Bip32], path: List[ChildIndex]): Either[KeyError, ExtendedKey[Ed25519Bip32]] = path.foldM(root)(child)

  /** Public (soft-only) derivation: A' = A + (8 zL) B, from the verifying key and chain code alone. */
  def publicChild(
    key:       VerifyingKey[Ed25519Bip32],
    chainCode: ByteVector,
    index:     ChildIndex
  ): Either[KeyError, (VerifyingKey[Ed25519Bip32], ByteVector)] =
    if (index.hardened) KeyError.InvalidDerivation("hardened children need the private key").asLeft
    else {
      val idx = ChildIndex.le32(index)
      val z = Hmac.sha512(chainCode, ByteVector(0x02) ++ key.bytes ++ idx)
      Either
        .catchNonFatal(new CompressedEdwardsY(key.bytes.toArray).decompress())
        .leftMap(_ => KeyError.InvalidDerivation("verifying key does not decode"))
        .map { point =>
          val child = Constants.ED25519_BASEPOINT_TABLE.multiply(Scalar.fromBytesModOrder(times8(z).toArray)).add(point)
          (
            VerifyingKey[Ed25519Bip32](ByteVector.view(child.compress().toByteArray)),
            Hmac.sha512(chainCode, ByteVector(0x03) ++ key.bytes ++ idx).drop(32)
          )
        }
    }
}
