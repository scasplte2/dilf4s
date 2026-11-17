package xyz.kd5ujc.crypto.keys

import java.nio.charset.StandardCharsets.UTF_8

import cats.syntax.all._

import scala.annotation.tailrec

import xyz.kd5ujc.crypto.signing._

import scodec.bits.ByteVector

/**
 * Why a derivation path is refused: the first rule it breaks, its segments checked in order (docs/specs/signing.md §3),
 * as the `paths` vectors spell it. Generated from `shared/schemas/crypto-errors.cddl`; `PathRefusal.values` lists them.
 */
export xyz.kd5ujc.models.crypto_errors.PathRefusal

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
    Either.cond(i >= 0 && i < 0x80000000L, Soft(i), KeyError.InvalidDerivation(KeyError.DerivationReason.IndexOutOfRange(i)))
  def hardened(i: Long): Either[KeyError, ChildIndex] =
    Either.cond(i >= 0 && i < 0x80000000L, Hardened(i), KeyError.InvalidDerivation(KeyError.DerivationReason.IndexOutOfRange(i)))

  /** The most digits a segment may have: 2^31 - 1 has ten. */
  val MaxSegmentDigits: Int = xyz.kd5ujc.models.limits.PathMaxSegmentDigits

  /** The most segments a path may have after `m`: BIP32 serialises the depth in one byte. */
  val MaxDepth: Int = xyz.kd5ujc.models.limits.PathMaxDepth

  /**
   * `m/44'/0'/0'/0/7` (or `h` in place of `'`), relative to the master key; `m` alone is the empty path. A segment is
   * `0` or a nonzero ASCII digit and more digits, then an optional hardening mark, so each index has one spelling.
   * Nothing is trimmed, and an empty segment (`m/`, `m//1`), a leading zero, a sign, a non-ASCII digit or any other
   * character is `InvalidPath`, typed by the first rule broken (a [[PathRefusal]]; docs/specs/signing.md §3). A segment
   * of more than [[MaxSegmentDigits]] digits is refused before any conversion, and the scan stops at segment
   * [[MaxDepth]], so parsing is linear in the path and never holds more than [[MaxDepth]] indices. The message names a
   * segment by its position (0 is the first after `m`) and, past the digit bound, its length; never by its text.
   */
  def path(p: String): Either[KeyError.InvalidPath, List[ChildIndex]] = {
    @tailrec
    def scan(from: Int, at: Int, acc: List[ChildIndex]): Either[KeyError.InvalidPath, List[ChildIndex]] = {
      val slash = p.indexOf('/', from)
      val end = if (slash < 0) p.length else slash
      val parsed =
        if (at < MaxDepth) segment(p.substring(from, end), at)
        else KeyError.InvalidPath(KeyError.PathReason.TooDeep).asLeft
      parsed match {
        case Left(e)               => e.asLeft
        case Right(c) if slash < 0 => (c :: acc).reverse.asRight
        case Right(c)              => scan(slash + 1, at + 1, c :: acc)
      }
    }
    if (p == "m") Nil.asRight
    else if (p.startsWith("m/")) scan(2, 0, Nil)
    else KeyError.InvalidPath(KeyError.PathReason.MissingMaster).asLeft
  }

  private def segment(seg: String, at: Int): Either[KeyError.InvalidPath, ChildIndex] = {
    val hard = seg.endsWith("'") || seg.endsWith("h")
    val digits = if (hard) seg.dropRight(1) else seg
    for {
      _ <- Either.cond[KeyError.InvalidPath, Unit](
        digits.nonEmpty && digits.forall(c => c >= '0' && c <= '9'),
        (),
        KeyError.InvalidPath(KeyError.PathReason.Malformed(at))
      )
      _ <- Either.cond[KeyError.InvalidPath, Unit](
        digits == "0" || !digits.startsWith("0"),
        (),
        KeyError.InvalidPath(KeyError.PathReason.LeadingZero(at))
      )
      _ <- Either.cond[KeyError.InvalidPath, Unit](
        digits.length <= MaxSegmentDigits,
        (),
        KeyError.InvalidPath(KeyError.PathReason.TooManyDigits(at, digits.length))
      )
      // at most ten digits: no overflow
      i = digits.foldLeft(0L)((acc, c) => acc * 10 + (c - '0'))
      index <- Either.cond[KeyError.InvalidPath, Long](i < 0x80000000L, i, KeyError.InvalidPath(KeyError.PathReason.OutOfRange(at)))
    } yield if (hard) Hardened(index) else Soft(index)
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
    Either.cond(cc.size == ChainCodeLength, cc, KeyError.InvalidDerivation(KeyError.DerivationReason.ChainCodeLength(cc.size)))

  /** An extended key from its parts; the chain code must be 32 bytes. */
  def from[S](key: SigningKey[S], chainCode: ByteVector): Either[KeyError, ExtendedKey[S]] =
    ExtendedKey.chainCode(chainCode).map(ExtendedKey(key, _))
}

/** SLIP-10 for Ed25519: hardened derivation only; the 32-byte key is an RFC 8032 seed. */
object Slip10Ed25519 {

  private val curveKey = ByteVector("ed25519 seed".getBytes(UTF_8))

  private def split(i: ByteVector): Either[KeyError, ExtendedKey[Ed25519]] =
    Ed25519.codec
      .signingKey(i.take(32))
      .bimap(e => KeyError.InvalidDerivation(KeyError.DerivationReason.Crypto(e)), ExtendedKey(_, i.drop(32)))

  def master(seed: Secret): Either[KeyError, ExtendedKey[Ed25519]] =
    Either
      .cond(
        seed.bytes.size >= 16 && seed.bytes.size <= 64,
        seed,
        KeyError.InvalidDerivation(KeyError.DerivationReason.SeedLength(seed.bytes.size))
      )
      .flatMap(s => split(Hmac.sha512(curveKey, s.bytes)))

  def child(parent: ExtendedKey[Ed25519], index: ChildIndex): Either[KeyError, ExtendedKey[Ed25519]] =
    if (!index.hardened) KeyError.InvalidDerivation(KeyError.DerivationReason.HardenedOnly).asLeft
    else split(Hmac.sha512(parent.chainCode, ByteVector(0) ++ parent.key.bytes ++ ChildIndex.be32(index)))

  def derive(seed: Secret, path: List[ChildIndex]): Either[KeyError, ExtendedKey[Ed25519]] = master(seed).flatMap(m => path.foldM(m)(child))
}

/** BIP32 for secp256k1. Scalar additions run in libsecp256k1 (constant time). */
final class Bip32Secp256k1(backend: Secp256k1) {

  private val curveKey = ByteVector("Bitcoin seed".getBytes(UTF_8))

  private val n = Secp256k1.Order

  private def keyOf(b: ByteVector): Either[KeyError, SigningKey[Secp256k1Ecdsa]] =
    backend.ecdsaCodec
      .signingKey(b)
      .leftMap(e => KeyError.InvalidDerivation(KeyError.DerivationReason.InvalidScalar(e)))

  /** A child's tweak `I_L`: an index with `I_L >= n` is skipped (BIP32, signing.md §2.3); `I_L = 0` is a valid tweak. */
  private def tweakOf(iL: ByteVector): Either[KeyError, ByteVector] =
    Either.cond(BigInt(1, iL.toArray) < n, iL, KeyError.InvalidDerivation(KeyError.DerivationReason.TweakOutOfRange))

  def master(seed: Secret): Either[KeyError, ExtendedKey[Secp256k1Ecdsa]] =
    Either
      .cond(
        seed.bytes.size >= 16 && seed.bytes.size <= 64,
        seed,
        KeyError.InvalidDerivation(KeyError.DerivationReason.SeedLength(seed.bytes.size))
      )
      .flatMap { s =>
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
      sum   <- backend.tweakAdd(parent.key.bytes, tweak).leftMap(e => KeyError.InvalidDerivation(KeyError.DerivationReason.Crypto(e)))
      key   <- keyOf(sum)
    } yield ExtendedKey(key, i.drop(32))

  /** CKDpub: `K' = K + [I_L]G` from the parent verifying key and chain code; soft indices only. */
  def publicChild(
    key:       VerifyingKey[Secp256k1Ecdsa],
    chainCode: ByteVector,
    index:     ChildIndex
  ): Either[KeyError, (VerifyingKey[Secp256k1Ecdsa], ByteVector)] =
    if (index.hardened) KeyError.InvalidDerivation(KeyError.DerivationReason.PrivateKeyRequired).asLeft
    else ExtendedKey.chainCode(chainCode).flatMap(cc => publicChildOf(key, Hmac.sha512(cc, key.bytes ++ ChildIndex.be32(index))))

  /** CKDpub from `I`: `K' = K + [I_L]G`, chain code `I_R`. */
  private[keys] def publicChildOf(
    key: VerifyingKey[Secp256k1Ecdsa],
    i:   ByteVector
  ): Either[KeyError, (VerifyingKey[Secp256k1Ecdsa], ByteVector)] =
    for {
      tweak <- tweakOf(i.take(32))
      child <- backend.pubTweakAdd(key.bytes, tweak).leftMap(e => KeyError.InvalidDerivation(KeyError.DerivationReason.Crypto(e)))
      vk    <- backend.ecdsaCodec.verifyingKey(child).leftMap(e => KeyError.InvalidDerivation(KeyError.DerivationReason.Crypto(e)))
    } yield (vk, i.drop(32))

  def derive(seed: Secret, path: List[ChildIndex]): Either[KeyError, ExtendedKey[Secp256k1Ecdsa]] =
    master(seed).flatMap(m => path.foldM(m)(child))
}
