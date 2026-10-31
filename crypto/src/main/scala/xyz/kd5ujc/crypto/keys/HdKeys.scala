package xyz.kd5ujc.crypto.keys

import java.nio.charset.StandardCharsets.UTF_8

import cats.syntax.all._

import xyz.kd5ujc.crypto.signing._

import scodec.bits.ByteVector

/** A child index below 2^31; hardened indices are serialised with the top bit set. */
sealed trait ChildIndex extends Product with Serializable {
  def value: Long
  def serialized: Long
  def hardened: Boolean
}

object ChildIndex {
  final case class Soft private[keys] (value: Long) extends ChildIndex { def serialized: Long = value; def hardened: Boolean = false }
  final case class Hardened private[keys] (value: Long) extends ChildIndex {
    def serialized: Long = value | 0x80000000L; def hardened: Boolean = true
  }

  def soft(i: Long): Either[KeyError, ChildIndex] =
    Either.cond(i >= 0 && i < 0x80000000L, Soft(i), KeyError.InvalidDerivation(s"index $i out of range"))
  def hardened(i: Long): Either[KeyError, ChildIndex] =
    Either.cond(i >= 0 && i < 0x80000000L, Hardened(i), KeyError.InvalidDerivation(s"index $i out of range"))

  /**
   * `m/44'/0'/0'/0/7` (or `h` in place of `'`), relative to the master key; `m` alone is the empty path. A segment is
   * one or more ASCII digits, then an optional hardening mark. Nothing is trimmed, and an empty segment (`m/`, `m//1`),
   * a sign, a non-ASCII digit or any other character is `InvalidDerivation` (docs/specs/signing.md §3).
   */
  def path(p: String): Either[KeyError, List[ChildIndex]] =
    p.split("/", -1).toList match {
      case "m" :: rest => rest.traverse(segment)
      case _           => KeyError.InvalidDerivation("path must start with m").asLeft
    }

  private def segment(seg: String): Either[KeyError, ChildIndex] = {
    val hard = seg.endsWith("'") || seg.endsWith("h")
    val digits = if (hard) seg.dropRight(1) else seg
    Either
      .cond(digits.nonEmpty && digits.forall(c => c >= '0' && c <= '9'), digits, KeyError.InvalidDerivation(s"bad path segment '$seg'"))
      // leading zeros are allowed, so the digits are unbounded: BigInt, then the range check
      .map(BigInt(_))
      .flatMap(i =>
        Either
          .cond(i.isValidLong, i.toLong, KeyError.InvalidDerivation(s"index $digits out of range"))
          .flatMap(if (hard) hardened else soft)
      )
  }

  def be32(i: ChildIndex): ByteVector = ByteVector.fromInt(i.serialized.toInt)
  def le32(i: ChildIndex): ByteVector = ByteVector.fromInt(i.serialized.toInt, ordering = scodec.bits.ByteOrdering.LittleEndian)
}

/** An extended private key: a signing key plus a 32-byte chain code. */
final case class ExtendedKey[S] private[keys] (key: SigningKey[S], chainCode: ByteVector) {
  override def toString: String = s"ExtendedKey(<redacted>, chainCode=${chainCode.toHex})"
}

object ExtendedKey {
  private[keys] val ChainCodeLength: Long = 32

  private[keys] def chainCode(cc: ByteVector): Either[KeyError, ByteVector] =
    Either.cond(cc.size == ChainCodeLength, cc, KeyError.InvalidDerivation(s"a chain code is $ChainCodeLength bytes, got ${cc.size}"))

  /** An extended key from its parts; the chain code must be 32 bytes. */
  def from[S](key: SigningKey[S], chainCode: ByteVector): Either[KeyError, ExtendedKey[S]] =
    ExtendedKey.chainCode(chainCode).map(ExtendedKey(key, _))
}

/** SLIP-10 for Ed25519: hardened derivation only; the 32-byte key is an RFC 8032 seed. */
object Slip10Ed25519 {

  private val curveKey = ByteVector("ed25519 seed".getBytes(UTF_8))

  private def split(i: ByteVector): Either[KeyError, ExtendedKey[Ed25519]] =
    Ed25519.codec.signingKey(i.take(32)).bimap(e => KeyError.InvalidDerivation(e.toString), ExtendedKey(_, i.drop(32)))

  def master(seed: Secret): Either[KeyError, ExtendedKey[Ed25519]] =
    Either
      .cond(seed.bytes.size >= 16 && seed.bytes.size <= 64, seed, KeyError.InvalidDerivation("seed must be 16..64 bytes"))
      .flatMap(s => split(Hmac.sha512(curveKey, s.bytes)))

  def child(parent: ExtendedKey[Ed25519], index: ChildIndex): Either[KeyError, ExtendedKey[Ed25519]] =
    if (!index.hardened) KeyError.InvalidDerivation("SLIP-10 Ed25519 supports hardened indices only").asLeft
    else split(Hmac.sha512(parent.chainCode, ByteVector(0) ++ parent.key.bytes ++ ChildIndex.be32(index)))

  def derive(seed: Secret, path: List[ChildIndex]): Either[KeyError, ExtendedKey[Ed25519]] = master(seed).flatMap(m => path.foldM(m)(child))
}

/** BIP32 for secp256k1. Scalar additions run in libsecp256k1 (constant time). */
final class Bip32Secp256k1(backend: Secp256k1) {

  private val curveKey = ByteVector("Bitcoin seed".getBytes(UTF_8))

  private val n = BigInt("fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141", 16)

  private def keyOf(b: ByteVector): Either[KeyError, SigningKey[Secp256k1Ecdsa]] =
    backend.ecdsaCodec
      .signingKey(b)
      .leftMap(_ => KeyError.InvalidDerivation("derived key is not a valid scalar (skip this index, per BIP32)"))

  /** A child's tweak `I_L`: an index with `I_L >= n` is skipped (BIP32, signing.md §2.3); `I_L = 0` is a valid tweak. */
  private def tweakOf(iL: ByteVector): Either[KeyError, ByteVector] =
    Either.cond(BigInt(1, iL.toArray) < n, iL, KeyError.InvalidDerivation("I_L is not below n (skip this index, per BIP32)"))

  def master(seed: Secret): Either[KeyError, ExtendedKey[Secp256k1Ecdsa]] =
    Either.cond(seed.bytes.size >= 16 && seed.bytes.size <= 64, seed, KeyError.InvalidDerivation("seed must be 16..64 bytes")).flatMap {
      s =>
        val i = Hmac.sha512(curveKey, s.bytes)
        keyOf(i.take(32)).map(ExtendedKey(_, i.drop(32)))
    }

  def child(parent: ExtendedKey[Secp256k1Ecdsa], index: ChildIndex): Either[KeyError, ExtendedKey[Secp256k1Ecdsa]] = {
    val data =
      if (index.hardened) ByteVector(0) ++ parent.key.bytes ++ ChildIndex.be32(index)
      else backend.ecdsaPublicKeyOf.verifyingKey(parent.key).bytes ++ ChildIndex.be32(index)
    childOf(parent, Hmac.sha512(parent.chainCode, data))
  }

  /** CKDpriv from `I = HMAC-SHA512(...)`: `k' = I_L + k mod n`, chain code `I_R`. */
  private[keys] def childOf(parent: ExtendedKey[Secp256k1Ecdsa], i: ByteVector): Either[KeyError, ExtendedKey[Secp256k1Ecdsa]] =
    for {
      tweak <- tweakOf(i.take(32))
      sum   <- backend.tweakAdd(parent.key.bytes, tweak).leftMap(e => KeyError.InvalidDerivation(e.toString))
      key   <- keyOf(sum)
    } yield ExtendedKey(key, i.drop(32))

  /** CKDpub: `K' = K + [I_L]G` from the parent verifying key and chain code; soft indices only. */
  def publicChild(
    key:       VerifyingKey[Secp256k1Ecdsa],
    chainCode: ByteVector,
    index:     ChildIndex
  ): Either[KeyError, (VerifyingKey[Secp256k1Ecdsa], ByteVector)] =
    if (index.hardened) KeyError.InvalidDerivation("hardened children need the private key").asLeft
    else ExtendedKey.chainCode(chainCode).flatMap(cc => publicChildOf(key, Hmac.sha512(cc, key.bytes ++ ChildIndex.be32(index))))

  /** CKDpub from `I`: `K' = K + [I_L]G`, chain code `I_R`. */
  private[keys] def publicChildOf(
    key: VerifyingKey[Secp256k1Ecdsa],
    i:   ByteVector
  ): Either[KeyError, (VerifyingKey[Secp256k1Ecdsa], ByteVector)] =
    for {
      tweak <- tweakOf(i.take(32))
      child <- backend.pubTweakAdd(key.bytes, tweak).leftMap(e => KeyError.InvalidDerivation(e.toString))
      vk    <- backend.ecdsaCodec.verifyingKey(child).leftMap(e => KeyError.InvalidDerivation(e.toString))
    } yield (vk, i.drop(32))

  def derive(seed: Secret, path: List[ChildIndex]): Either[KeyError, ExtendedKey[Secp256k1Ecdsa]] =
    master(seed).flatMap(m => path.foldM(m)(child))
}
