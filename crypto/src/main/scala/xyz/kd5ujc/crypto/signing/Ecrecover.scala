package xyz.kd5ujc.crypto.signing

import cats.syntax.all._

import xyz.kd5ujc.core.hash.{Hasher, Keccak256}

import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.math.ec.{ECAlgorithms, ECPoint}
import scodec.bits.ByteVector

/**
 * Ethereum's signer recovery (`ecrecover`, yellow paper Appendix F) over secp256k1, in BouncyCastle: the address whose
 * key made an ECDSA signature over a 32-byte digest. Verification only: every input is public.
 *
 * Stricter than the EVM precompile, as OpenZeppelin's `ECDSA.recover` is: a high-`s` signature (EIP-2) recovers
 * nothing, so a signature cannot be re-encoded into a second valid one. Wallets sign with low `s`.
 */
object Ecrecover {

  val DigestSize: Int = 32
  val SignatureSize: Int = 65
  val AddressSize: Int = 20

  private val params = CustomNamedCurves.getByName("secp256k1")
  private val curve = params.getCurve
  private val N: BigInt = BigInt(params.getN)
  private val HalfN: BigInt = N / 2

  /**
   * `signature` is `r ‖ s ‖ v` (65 bytes), `v` in {27, 28} or {0, 1}. Wrong sizes or another `v` are malformed (`Left`).
   * A well-formed signature that recovers no key is `Right(None)`: `r` or `s` zero or not below the order, a high `s`,
   * or an `r` that is no point's x-coordinate.
   */
  def recover(digest: ByteVector, signature: ByteVector): Either[String, Option[ByteVector]] =
    for {
      _ <- Either.cond(digest.size == DigestSize.toLong, (), s"the digest is $DigestSize bytes, got ${digest.size}")
      _ <- Either.cond(
        signature.size == SignatureSize.toLong,
        (),
        s"a signature is $SignatureSize bytes (r ‖ s ‖ v), got ${signature.size}"
      )
      v = signature(64) & 0xff
      parity <- v match {
        case 0 | 27 => 0.asRight
        case 1 | 28 => 1.asRight
        case other  => s"v is 27 or 28 (or 0 or 1), got $other".asLeft
      }
    } yield {
      val r = BigInt(1, signature.take(32).toArray)
      val s = BigInt(1, signature.slice(32, 64).toArray)
      Option
        .when(r > 0 && r < N && s > 0 && s <= HalfN)(())
        .flatMap(_ => point(r, parity))
        .map { big =>
          val e = BigInt(1, digest.toArray).mod(N)
          val rInv = r.modInverse(N)
          // Q = r^-1 (s·R - e·G)
          ECAlgorithms
            .sumOfTwoMultiplies(big, (s * rInv).mod(N).bigInteger, params.getG, (-e * rInv).mod(N).bigInteger)
            .normalize()
        }
        .filterNot(_.isInfinity)
        .map(addressOf)
    }

  /** The point with x-coordinate `r` and the given y parity, if there is one. */
  private def point(r: BigInt, parity: Int): Option[ECPoint] =
    Either
      .catchNonFatal(curve.decodePoint((ByteVector(0x02 + parity) ++ ByteVector(r.toByteArray).takeRight(32).padLeft(32)).toArray))
      .toOption

  /** An Ethereum address: the last 20 bytes of Keccak-256 over the uncompressed key's `x ‖ y`. */
  private def addressOf(key: ECPoint): ByteVector =
    Hasher[Keccak256].digest(ByteVector.view(key.normalize().getEncoded(false)).drop(1)).bytes.takeRight(AddressSize.toLong)

  /** The address of a SEC1-encoded public key (33 or 65 bytes), if it is a point of the curve. */
  def address(key: ByteVector): Either[String, ByteVector] =
    Either.catchNonFatal(curve.decodePoint(key.toArray)).leftMap(_ => "not a secp256k1 public key").map(addressOf)
}
