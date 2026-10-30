package xyz.kd5ujc.crypto.kes

import cats.syntax.all._

import xyz.kd5ujc.core.hash.{DomainTag, Hasher, TaggedHash}
import xyz.kd5ujc.crypto.signing.{Ed25519, Signature, SigningKey, VerifyingKey}

import scodec.bits.ByteVector

/**
 * The dilf4s KES constructions: the MMM sum composition with Merkle-witness signatures (as in Topl/Bifrost), and the
 * product composition of a "super" sum tree certifying successive "sub" sum trees. Improvements over the Topl design:
 * tagged hashes for every role, the period as a verification input rather than part of the key, super signatures bound
 * to their role by a tag, immutable keys with typed errors, canonical fixed-length encodings and ZIP-215 leaves.
 *
 * The hash `H` is a type parameter (any [[Hasher]] with at least 32 bytes of output): digests are `H.outputSize`
 * bytes, seeds and Ed25519 material stay 32 bytes.
 *
 * Normative description: `docs/specs/kes.md`.
 */
private[kes] object MerkleKes {

  val Tags: List[String] = List("dilf4s/kes/seed/v1", "dilf4s/kes/leaf/v1", "dilf4s/kes/node/v1", "dilf4s/kes/product/v1")

  val Zero32: ByteVector = ByteVector.low(32)

  /**
   * A sum-tree signing key of height `d`: the active subtree, the seed of the right subtree while it is still pending
   * (erased once the right half is active), and both subtree roots. A leaf's seed is `None` once erased.
   */
  sealed trait Tree extends Product with Serializable
  final case class Leaf(seed: Option[ByteVector], vk: ByteVector) extends Tree
  final case class Node(active: Tree, rightSeed: Option[ByteVector], left: ByteVector, right: ByteVector) extends Tree

  def leafVk(seed: ByteVector): ByteVector = Ed25519.publicKeyOf.verifyingKey(new SigningKey[Ed25519](seed)).bytes

  def eraseActiveLeaf(t: Tree): Tree = t match {
    case Leaf(_, vk)              => Leaf(None, vk)
    case Node(active, seed, l, r) => Node(eraseActiveLeaf(active), seed, l, r)
  }
}

/** The tree algorithms over hash `H`. */
private[kes] final class MerkleKesOps[H](implicit H: Hasher[H]) {
  import MerkleKes._

  /** Digest length. */
  val n: Long = H.outputSize

  private val seedHash = TaggedHash[H](DomainTag.literal("dilf4s/kes/seed/v1"))
  private val leafHash = TaggedHash[H](DomainTag.literal("dilf4s/kes/leaf/v1"))
  private val nodeHash = TaggedHash[H](DomainTag.literal("dilf4s/kes/node/v1"))
  private val productHash = TaggedHash[H](DomainTag.literal("dilf4s/kes/product/v1"))

  /** Two 32-byte child seeds: the first 32 bytes of the tagged digests of `00 || seed` and `01 || seed`. */
  def expand(seed: ByteVector): (ByteVector, ByteVector) =
    (seedHash(ByteVector(0) ++ seed).bytes.take(32), seedHash(ByteVector(1) ++ seed).bytes.take(32))

  def leafRoot(vk: ByteVector): ByteVector = leafHash(vk).bytes
  def node(l:      ByteVector, r: ByteVector): ByteVector = nodeHash(l ++ r).bytes

  /** The message a super tree signs to certify a sub tree root. */
  def productMessage(subRoot: ByteVector): ByteVector = productHash(subRoot).bytes

  def root(t: Tree): ByteVector = t match {
    case Leaf(_, vk)      => leafRoot(vk)
    case Node(_, _, l, r) => node(l, r)
  }

  def gen(seed: ByteVector, d: Int): Tree =
    if (d == 0) Leaf(Some(seed), leafVk(seed))
    else {
      val (r0, r1) = expand(seed)
      val sub = gen(r0, d - 1)
      Node(sub, Some(r1), root(sub), root(gen(r1, d - 1)))
    }

  /** `vk || sig || witnesses (bottom-up)`, or `None` if the active leaf was erased. */
  def sign(t: Tree, d: Int, period: Long, msg: ByteVector): Option[ByteVector] = t match {
    case Leaf(seed, vk) => seed.map(s => vk ++ Ed25519.signer.sign(new SigningKey[Ed25519](s), msg).bytes)
    case Node(active, _, l, r) =>
      val half = 1L << (d - 1)
      sign(active, d - 1, period % half, msg).map(_ ++ (if (period < half) r else l))
  }

  /**
   * The tree for `period + 1`. Activating a right half regenerates it from its seed, which is checked against the
   * stored root there (a corrupted seed cannot be detected more cheaply, so `decode` does not regenerate subtrees).
   */
  def evolve(t: Tree, d: Int, period: Long): Either[KesError, Tree] = t match {
    case Leaf(_, _) => KesError.Exhausted(period).asLeft
    case Node(active, rightSeed, l, r) =>
      val half = 1L << (d - 1)
      if (period + 1 < half) evolve(active, d - 1, period).map(Node(_, rightSeed, l, r))
      else if (period + 1 == half)
        rightSeed
          .map(gen(_, d - 1))
          .filter(root(_) == r)
          .map(sub => Node(sub, None, l, r): Tree)
          .toRight(KesError.InconsistentKey(s"the right seed at height $d does not match its root"))
      else evolve(active, d - 1, period - half).map(Node(_, None, l, r))
  }

  def signatureLength(d: Int): Long = 96L + n * d

  def verify(rootDigest: ByteVector, d: Int, period: Long, msg: ByteVector, sig: ByteVector): Boolean =
    sig.size == signatureLength(d) && period >= 0 && period < (1L << d) && {
      val (vk, leafSig, witnesses) = (sig.take(32), sig.slice(32, 96), sig.drop(96).grouped(n).toList)
      val computed = witnesses.zipWithIndex.foldLeft(leafRoot(vk)) {
        case (acc, (w, level)) =>
          if (((period >> level) & 1L) == 0L) node(acc, w) else node(w, acc)
      }
      computed == rootDigest && Ed25519.verifier.verify(VerifyingKey[Ed25519](vk), msg, Signature[Ed25519](leafSig))
    }

  // ------------------------------------------------------------------ canonical key encoding

  /** Leaf: `seed (zeros once erased) || vk`; node: `active || rightSeed (zeros once used) || left || right`. */
  def encode(t: Tree): ByteVector = t match {
    case Leaf(seed, vk)      => seed.getOrElse(Zero32) ++ vk
    case Node(a, seed, l, r) => encode(a) ++ seed.getOrElse(Zero32) ++ l ++ r
  }

  def encodedLength(d: Int): Long = 64L + (32L + 2L * n) * d

  /**
   * Decodes a tree of height `d` at `period`: the period says which right seeds must already be used (and so zero),
   * and whether the leaf may be erased. Every stored root is checked against the subtree it describes.
   */
  def decode(b: ByteVector, d: Int, period: Long, leafErased: Boolean): Either[KesError, Tree] =
    if (d == 0) {
      val (seed, vk) = (b.take(32), b.drop(32))
      (leafErased, seed == Zero32) match {
        case (true, true)                         => (Leaf(None, vk): Tree).asRight
        case (false, false) if leafVk(seed) == vk => (Leaf(Some(seed), vk): Tree).asRight
        case _                                    => KesError.InconsistentKey("leaf seed and verifying key disagree").asLeft
      }
    } else {
      val half = 1L << (d - 1)
      val inner = b.take(encodedLength(d - 1))
      val at = encodedLength(d - 1)
      val (seed, l, r) = (b.slice(at, at + 32), b.slice(at + 32, at + 32 + n), b.drop(at + 32 + n))
      for {
        active <- decode(inner, d - 1, period % half, leafErased)
        _ <- Either.cond(
          (period < half) == (seed != Zero32),
          (),
          KesError.InconsistentKey(s"right seed state does not match period $period at height $d")
        )
        _ <- Either.cond(
          root(active) == (if (period < half) l else r),
          (),
          KesError.InconsistentKey(s"active subtree root mismatch at height $d")
        )
      } yield Node(active, Option.when(period < half)(seed), l, r)
    }
}

/** The dilf4s sum composition over hash `H`: `2^height` periods, signatures `vk || ed25519 sig || Merkle path` (`96 + n h` bytes). */
final class KesSum[H] private (val height: Int)(implicit H: Hasher[H]) {
  import MerkleKes._
  private val ops = new MerkleKesOps[H]
  import ops._

  val totalPeriods: Long = 1L << height
  val signatureLength: Long = ops.signatureLength(height)
  val keyLength: Long = encodedLength(height)

  final class Key private[KesSum] (private[kes] val tree: Tree, val period: Long) {
    override def toString: String = s"KesSum.Key(height=$height, period=$period, <redacted>)"
    def bytes: ByteVector = encode(tree)
  }

  def generate(seed: ByteVector): Either[KesError, Key] =
    Either.cond(seed.size == 32L, new Key(gen(seed, height), 0L), KesError.Malformed("KES seed", s"expected 32 bytes, got ${seed.size}"))

  def load(raw: ByteVector, period: Long): Either[KesError, Key] =
    for {
      _ <- Either.cond(raw.size == keyLength, (), KesError.Malformed("KES key", s"expected $keyLength bytes, got ${raw.size}"))
      _ <- Either.cond(period >= 0 && period < totalPeriods, (), KesError.PeriodOutOfRange(period, totalPeriods))
      t <- decode(raw, height, period, leafErased = false)
    } yield new Key(t, period)

  /** The root: fixed for the key's lifetime. */
  def verifyingKey(key: Key): ByteVector = root(key.tree)

  def sign(key: Key, message: ByteVector): Either[KesError, ByteVector] =
    ops.sign(key.tree, height, key.period, message).toRight(KesError.InconsistentKey("the active leaf is erased"))

  def evolve(key: Key): Either[KesError, Key] =
    ops
      .evolve(key.tree, height, key.period)
      .leftMap { case KesError.Exhausted(_) => KesError.Exhausted(key.period); case e => e }
      .map(new Key(_, key.period + 1))

  def verify(vk: ByteVector, period: Long, message: ByteVector, signature: ByteVector): Either[KesError, Boolean] =
    for {
      _ <- Either.cond(vk.size == n, (), KesError.Malformed("KES verifying key", s"expected $n bytes, got ${vk.size}"))
      _ <- Either.cond(
        signature.size == signatureLength,
        (),
        KesError.Malformed("KES signature", s"expected $signatureLength bytes, got ${signature.size}")
      )
      _ <- Either.cond(period >= 0 && period < totalPeriods, (), KesError.PeriodOutOfRange(period, totalPeriods))
    } yield ops.verify(vk, height, period, message, signature)
}

object KesSum {
  def apply[H](height: Int)(implicit H: Hasher[H]): Either[KesError, KesSum[H]] =
    for {
      _ <- Either.cond(
        H.outputSize >= 32L,
        (),
        KesError.Malformed("KES hash", s"${H.name} has a ${H.outputSize}-byte output; at least 32 required")
      )
      _ <- Either.cond(height >= 0 && height <= 30, (), KesError.Malformed("KES height", s"expected 0..30, got $height"))
    } yield new KesSum[H](height)
}

/**
 * The dilf4s product composition: a super sum tree of height `hSup` certifies `2^hSup` successive sub sum trees of
 * height `hSub`, for `2^(hSup + hSub)` periods. Sub-tree seeds form a forward chain, and the super tree's active leaf is
 * erased as soon as it has certified the current sub tree, so a leaked key cannot certify another sub tree for the
 * same super period. Signatures are `superSig || subRoot || subSig`.
 */
final class KesProduct[H] private (val superHeight: Int, val subHeight: Int)(implicit H: Hasher[H]) {
  import MerkleKes._
  private val ops = new MerkleKesOps[H]
  import ops._

  val totalPeriods: Long = 1L << (superHeight + subHeight)
  val signatureLength: Long = ops.signatureLength(superHeight) + n + ops.signatureLength(subHeight)
  val keyLength: Long = encodedLength(superHeight) + encodedLength(subHeight) + 32L + ops.signatureLength(superHeight)

  private val subPeriods: Long = 1L << subHeight

  /** Super tree (active leaf erased), sub tree, the next sub-chain seed and the super signature on the sub root. */
  final class Key private[KesProduct] (
    private[kes] val superTree: Tree,
    private[kes] val subTree:   Tree,
    private[kes] val nextSeed:  ByteVector,
    private[kes] val superSig:  ByteVector,
    val period:                 Long
  ) {
    override def toString: String = s"KesProduct.Key(heights=($superHeight,$subHeight), period=$period, <redacted>)"
    def bytes: ByteVector = encode(superTree) ++ encode(subTree) ++ nextSeed ++ superSig
  }

  /** Certifies a fresh sub tree from a chain seed with the super tree at `superPeriod`, then erases the super leaf. */
  private def certify(superTree: Tree, superPeriod: Long, chainSeed: ByteVector, period: Long): Either[KesError, Key] = {
    val (subSeed, next) = expand(chainSeed)
    val sub = gen(subSeed, subHeight)
    ops
      .sign(superTree, superHeight, superPeriod, productMessage(root(sub)))
      .toRight(KesError.InconsistentKey("the super leaf is erased"))
      .map(sig => new Key(eraseActiveLeaf(superTree), sub, next, sig, period))
  }

  def generate(seed: ByteVector): Either[KesError, Key] =
    Either.cond(seed.size == 32L, expand(seed), KesError.Malformed("KES seed", s"expected 32 bytes, got ${seed.size}")).flatMap {
      case (superSeed, chainSeed) => certify(gen(superSeed, superHeight), 0L, chainSeed, 0L)
    }

  def verifyingKey(key: Key): ByteVector = root(key.superTree)

  def sign(key: Key, message: ByteVector): Either[KesError, ByteVector] =
    ops
      .sign(key.subTree, subHeight, key.period % subPeriods, message)
      .toRight(KesError.InconsistentKey("the sub leaf is erased"))
      .map(sub => key.superSig ++ root(key.subTree) ++ sub)

  def evolve(key: Key): Either[KesError, Key] = {
    val (tSup, tSub) = (key.period / subPeriods, key.period % subPeriods)
    if (tSub + 1 < subPeriods)
      ops.evolve(key.subTree, subHeight, tSub).map(s => new Key(key.superTree, s, key.nextSeed, key.superSig, key.period + 1))
    else
      ops
        .evolve(key.superTree, superHeight, tSup)
        .leftMap { case KesError.Exhausted(_) => KesError.Exhausted(key.period); case e => e }
        .flatMap(certify(_, tSup + 1, key.nextSeed, key.period + 1))
  }

  def load(raw: ByteVector, period: Long): Either[KesError, Key] = {
    val (supLen, subLen) = (encodedLength(superHeight), encodedLength(subHeight))
    val (tSup, tSub) = (period / subPeriods, period % subPeriods)
    for {
      _   <- Either.cond(raw.size == keyLength, (), KesError.Malformed("KES key", s"expected $keyLength bytes, got ${raw.size}"))
      _   <- Either.cond(period >= 0 && period < totalPeriods, (), KesError.PeriodOutOfRange(period, totalPeriods))
      sup <- decode(raw.take(supLen), superHeight, tSup, leafErased = true)
      sub <- decode(raw.slice(supLen, supLen + subLen), subHeight, tSub, leafErased = false)
      (next, sig) = (raw.slice(supLen + subLen, supLen + subLen + 32), raw.drop(supLen + subLen + 32))
      _ <- Either.cond(
        ops.verify(root(sup), superHeight, tSup, productMessage(root(sub)), sig),
        (),
        KesError.InconsistentKey("the super signature does not certify the sub tree")
      )
    } yield new Key(sup, sub, next, sig, period)
  }

  def verify(vk: ByteVector, period: Long, message: ByteVector, signature: ByteVector): Either[KesError, Boolean] =
    for {
      _ <- Either.cond(vk.size == n, (), KesError.Malformed("KES verifying key", s"expected $n bytes, got ${vk.size}"))
      _ <- Either.cond(
        signature.size == signatureLength,
        (),
        KesError.Malformed("KES signature", s"expected $signatureLength bytes, got ${signature.size}")
      )
      _ <- Either.cond(period >= 0 && period < totalPeriods, (), KesError.PeriodOutOfRange(period, totalPeriods))
    } yield {
      val supLen = ops.signatureLength(superHeight)
      val (superSig, subRoot, subSig) = (signature.take(supLen), signature.slice(supLen, supLen + n), signature.drop(supLen + n))
      ops.verify(vk, superHeight, period / subPeriods, productMessage(subRoot), superSig) &&
      ops.verify(subRoot, subHeight, period % subPeriods, message, subSig)
    }
}

object KesProduct {
  def apply[H](superHeight: Int, subHeight: Int)(implicit H: Hasher[H]): Either[KesError, KesProduct[H]] =
    for {
      _ <- Either.cond(
        H.outputSize >= 32L,
        (),
        KesError.Malformed("KES hash", s"${H.name} has a ${H.outputSize}-byte output; at least 32 required")
      )
      _ <- Either.cond(
        superHeight >= 0 && subHeight >= 0 && superHeight + subHeight <= 30,
        (),
        KesError.Malformed("KES heights", s"expected non-negative heights summing to at most 30, got ($superHeight, $subHeight)")
      )
    } yield new KesProduct[H](superHeight, subHeight)
}
