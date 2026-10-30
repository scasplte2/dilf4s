package xyz.kd5ujc.crypto.keys

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
  final case class Soft private (value: Long) extends ChildIndex { def serialized: Long = value; def hardened: Boolean = false }
  final case class Hardened private (value: Long) extends ChildIndex {
    def serialized: Long = value | 0x80000000L; def hardened: Boolean = true
  }

  def soft(i: Long): Either[KeyError, ChildIndex] =
    Either.cond(i >= 0 && i < 0x80000000L, Soft(i), KeyError.InvalidDerivation(s"index $i out of range"))
  def hardened(i: Long): Either[KeyError, ChildIndex] =
    Either.cond(i >= 0 && i < 0x80000000L, Hardened(i), KeyError.InvalidDerivation(s"index $i out of range"))

  /** `m/44'/0'/0'/0/7` (or with `h`), relative to the master key. */
  def path(p: String): Either[KeyError, List[ChildIndex]] =
    p.trim.split("/").toList match {
      case "m" :: rest =>
        rest.traverse { seg =>
          val hard = seg.endsWith("'") || seg.endsWith("h")
          val digits = if (hard) seg.dropRight(1) else seg
          Either
            .catchNonFatal(digits.toLong)
            .leftMap(_ => KeyError.InvalidDerivation(s"bad path segment '$seg'"))
            .flatMap(i => if (hard) hardened(i) else soft(i))
        }
      case _ => KeyError.InvalidDerivation("path must start with m").asLeft
    }

  def be32(i: ChildIndex): ByteVector = ByteVector.fromInt(i.serialized.toInt)
  def le32(i: ChildIndex): ByteVector = ByteVector.fromInt(i.serialized.toInt, ordering = scodec.bits.ByteOrdering.LittleEndian)
}

/** An extended private key: a signing key plus a 32-byte chain code. */
final case class ExtendedKey[S](key: SigningKey[S], chainCode: ByteVector) {
  override def toString: String = s"ExtendedKey(<redacted>, chainCode=${chainCode.toHex})"
}

/** SLIP-10 for Ed25519: hardened derivation only; the 32-byte key is an RFC 8032 seed. */
object Slip10Ed25519 {

  private val curveKey = ByteVector("ed25519 seed".getBytes("UTF-8"))

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

  private val curveKey = ByteVector("Bitcoin seed".getBytes("UTF-8"))

  private def keyOf(b: ByteVector): Either[KeyError, SigningKey[Secp256k1Ecdsa]] =
    backend.ecdsaCodec
      .signingKey(b)
      .leftMap(_ => KeyError.InvalidDerivation("derived key is not a valid scalar (skip this index, per BIP32)"))

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
    val i = Hmac.sha512(parent.chainCode, data)
    for {
      tweak <- keyOf(i.take(32))
      sum   <- backend.tweakAdd(parent.key.bytes, tweak.bytes).leftMap(e => KeyError.InvalidDerivation(e.toString))
      key   <- keyOf(sum)
    } yield ExtendedKey(key, i.drop(32))
  }

  def derive(seed: Secret, path: List[ChildIndex]): Either[KeyError, ExtendedKey[Secp256k1Ecdsa]] =
    master(seed).flatMap(m => path.foldM(m)(child))
}
