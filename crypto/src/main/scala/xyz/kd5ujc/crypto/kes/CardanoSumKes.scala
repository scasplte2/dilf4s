package xyz.kd5ujc.crypto.kes

import xyz.kd5ujc.core.hash.{Blake2b256, Hasher}
import xyz.kd5ujc.crypto.signing.{Ed25519, Signature, SigningKey, VerifyingKey}

import scodec.bits.ByteVector

sealed trait KesError extends Product with Serializable

object KesError {
  final case class Malformed(what: String, reason: String) extends KesError
  final case class PeriodOutOfRange(period: Long, total: Long) extends KesError

  /** The key cannot evolve further: its last period has been reached. */
  final case class Exhausted(period: Long) extends KesError
  final case class InconsistentKey(reason: String) extends KesError
}

/**
 * Cardano's `SumKES Blake2b_256 (... (SingleKES Ed25519DSIGN))` of depth `d` (cardano-crypto-class
 * `Cardano.Crypto.KES.Sum`), byte-compatible: `Sum6KES` (d = 6, 64 periods) is the Praos hot key.
 *
 *   - seed expansion: `(Blake2b-256(01 || r), Blake2b-256(02 || r))`; a depth-0 key is the Ed25519 seed itself
 *   - verifying key: `Blake2b-256(vk0 || vk1)` (depth 0: the Ed25519 key)
 *   - signature: `sigma || vk0 || vk1`, recursively, `64 + 64 d` bytes
 *   - signing key: `sk || r1 || vk0 || vk1`, recursively, `32 + 96 d` bytes; `r1` is zeroed once the right half is in use
 *
 * Leaf signatures are checked with libsodium's Ed25519 rules ([[Ed25519.libsodiumVerifier]]), as cardano-node does.
 * Keys are immutable values: evolution returns a new key and the old one becomes garbage. Erasure of the old value's
 * memory is best effort (the JVM gives no guarantee), so callers must drop references to past keys.
 */
final class CardanoSumKes private (val depth: Int) {

  val totalPeriods: Long = 1L << depth
  val signingKeyLength: Long = 32L + 96L * depth
  val signatureLength: Long = 64L + 64L * depth

  private val blake = Hasher[Blake2b256]
  private def h(b: ByteVector): ByteVector = blake.digest(b).bytes
  private val zeroSeed = ByteVector.low(32)

  /** A signing key at a known period. */
  final class Key private[CardanoSumKes] (private[kes] val tree: ByteVector, val period: Long) {
    override def toString: String = s"CardanoSumKes.Key(depth=$depth, period=$period, <redacted>)"
    def bytes: ByteVector = tree
  }

  // ------------------------------------------------------------------ tree layout: sk(d) = sk(d-1) || r1 || vk0 || vk1

  private def skLen(d: Int): Long = 32L + 96L * d

  private def leafVk(seed: ByteVector): ByteVector =
    Ed25519.publicKeyOf.verifyingKey(new SigningKey[Ed25519](seed)).bytes

  private def vkOf(sk: ByteVector, d: Int): ByteVector =
    if (d == 0) leafVk(sk)
    else h(sk.slice(skLen(d - 1) + 32, skLen(d - 1) + 64) ++ sk.slice(skLen(d - 1) + 64, skLen(d)))

  private def gen(seed: ByteVector, d: Int): ByteVector =
    if (d == 0) seed
    else {
      val (r0, r1) = (h(ByteVector(1) ++ seed), h(ByteVector(2) ++ seed))
      val sk0 = gen(r0, d - 1)
      sk0 ++ r1 ++ vkOf(sk0, d - 1) ++ vkOf(gen(r1, d - 1), d - 1)
    }

  private def sign(sk: ByteVector, d: Int, t: Long, msg: ByteVector): ByteVector =
    if (d == 0) Ed25519.signer.sign(new SigningKey[Ed25519](sk), msg).bytes
    else {
      val half = 1L << (d - 1)
      val inner = sk.take(skLen(d - 1))
      sign(inner, d - 1, if (t < half) t else t - half, msg) ++ sk.slice(skLen(d - 1) + 32, skLen(d))
    }

  private def update(sk: ByteVector, d: Int, t: Long): Option[ByteVector] =
    if (d == 0) None
    else {
      val half = 1L << (d - 1)
      val (inner, rest) = (sk.take(skLen(d - 1)), sk.drop(skLen(d - 1)))
      val (r1, vks) = (rest.take(32), rest.drop(32))
      if (t + 1 < half) update(inner, d - 1, t).map(_ ++ r1 ++ vks)
      else if (t + 1 == half) Some(gen(r1, d - 1) ++ zeroSeed ++ vks)
      else update(inner, d - 1, t - half).map(_ ++ r1 ++ vks)
    }

  private def verify(vk: ByteVector, d: Int, t: Long, msg: ByteVector, sig: ByteVector): Boolean =
    if (d == 0) Ed25519.libsodiumVerifier.verify(VerifyingKey[Ed25519](vk), msg, Signature[Ed25519](sig))
    else {
      val (inner, vk0, vk1) = (sig.dropRight(64), sig.takeRight(64).take(32), sig.takeRight(32))
      val half = 1L << (d - 1)
      h(vk0 ++ vk1) == vk && (if (t < half) verify(vk0, d - 1, t, msg, inner) else verify(vk1, d - 1, t - half, msg, inner))
    }

  // ------------------------------------------------------------------ API

  /** The period-0 key for a 32-byte seed (`genKeyKES`). */
  def generate(seed: ByteVector): Either[KesError, Key] =
    Either.cond(seed.size == 32L, new Key(gen(seed, depth), 0L), KesError.Malformed("KES seed", s"expected 32 bytes, got ${seed.size}"))

  /** Loads Cardano's raw signing key (`kes.skey` payload) at the period the caller knows it to be at. */
  def load(raw: ByteVector, period: Long): Either[KesError, Key] =
    for {
      _ <- Either.cond(
        raw.size == signingKeyLength,
        (),
        KesError.Malformed("KES signing key", s"expected $signingKeyLength bytes, got ${raw.size}")
      )
      _ <- Either.cond(period >= 0 && period < totalPeriods, (), KesError.PeriodOutOfRange(period, totalPeriods))
    } yield new Key(raw, period)

  def verifyingKey(key: Key): ByteVector = vkOf(key.tree, depth)

  def sign(key: Key, message: ByteVector): ByteVector = sign(key.tree, depth, key.period, message)

  /** The key for the next period (`updateKES`); the last period cannot evolve. */
  def evolve(key: Key): Either[KesError, Key] =
    update(key.tree, depth, key.period).map(new Key(_, key.period + 1)).toRight(KesError.Exhausted(key.period))

  /** Verification of `signature` by root key `vk` at relative `period` (`verifyKES`). */
  def verify(vk: ByteVector, period: Long, message: ByteVector, signature: ByteVector): Either[KesError, Boolean] =
    for {
      _ <- Either.cond(vk.size == 32L, (), KesError.Malformed("KES verifying key", s"expected 32 bytes, got ${vk.size}"))
      _ <- Either.cond(
        signature.size == signatureLength,
        (),
        KesError.Malformed("KES signature", s"expected $signatureLength bytes, got ${signature.size}")
      )
      _ <- Either.cond(period >= 0 && period < totalPeriods, (), KesError.PeriodOutOfRange(period, totalPeriods))
    } yield verify(vk, depth, period, message, signature)
}

object CardanoSumKes {

  /** `SumKES` of depth 0..7 (`Sum0KES` .. `Sum7KES`). */
  def apply(depth: Int): Either[KesError, CardanoSumKes] =
    Either.cond(depth >= 0 && depth <= 7, new CardanoSumKes(depth), KesError.Malformed("KES depth", s"expected 0..7, got $depth"))

  /** The Praos hot-key scheme. */
  val Sum6: CardanoSumKes = new CardanoSumKes(6)
}
