/*
 * Ported from the user's Bifrost BloomFilter (co.topl.modifier.block.BloomFilter, 2020, MPL-2.0), itself modelled on
 * the Ethereum logs bloom. Changes: parameterised size and hash count; Kirsch–Mitzenmacher double hashing from one
 * tagged digest (k distinct bits per item, one hash call instead of four); immutable bits with consistent equality;
 * typed errors instead of `require`; union, intersection and a canonical encoding.
 */
package xyz.kd5ujc.core.bloom

import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{Canonical, CanonicalError, CanonicalValue, Cbor}
import xyz.kd5ujc.core.hash.{DomainTag, Hasher, TaggedHash}

import scodec.bits.{BitVector, ByteVector}

/** Filter size `bits` (a power of two, 64..2^24) and hash count `hashes` (1..32). */
sealed abstract case class BloomParams(bits: Int, hashes: Int)

object BloomParams {

  def apply(bits: Int, hashes: Int): Either[BloomError, BloomParams] =
    if (bits < 64 || bits > (1 << 24) || Integer.bitCount(bits) != 1)
      BloomError.InvalidParams(s"bits must be a power of two in 64..2^24, got $bits").asLeft
    else if (hashes < 1 || hashes > 32) BloomError.InvalidParams(s"hashes must be 1..32, got $hashes").asLeft
    else (new BloomParams(bits, hashes) {}).asRight

  /** 2048 bits, 4 hashes: the Ethereum-style ledger bloom (the original Bifrost size). */
  val Ledger2048: BloomParams = new BloomParams(2048, 4) {}
}

sealed trait BloomError extends Product with Serializable

object BloomError {
  final case class InvalidParams(reason: String) extends BloomError
  final case class Mismatch(left: BloomParams, right: BloomParams) extends BloomError
  final case class Encoding(reason: String) extends BloomError
}

/**
 * A Bloom filter over byte strings. `contains` never gives a false negative; false positives occur at roughly
 * `(1 - e^(-k n / m))^k` for `n` items. Items are hashed once with a tagged hash; the `k` indices are
 * `h1 + i * h2 (mod m)` with `h2` odd, which for power-of-two `m` are always distinct.
 */
final case class BloomFilter[H] private (params: BloomParams, bits: BitVector) {

  private def indices(item: ByteVector)(implicit H: Hasher[H]): List[Long] = {
    val d = BloomFilter.tagged[H].apply(item).bytes
    val h1 = d.take(8).toLong(signed = false)
    val h2 = d.drop(8).take(8).toLong(signed = false) | 1L
    val mask = params.bits.toLong - 1
    (0 until params.hashes).toList.map(i => (h1 + i.toLong * h2) & mask)
  }

  def add(item: ByteVector)(implicit H: Hasher[H]): BloomFilter[H] =
    copy(bits = indices(item).foldLeft(bits)((b, i) => b.set(i)))

  def addAll(items: Iterable[ByteVector])(implicit H: Hasher[H]): BloomFilter[H] = items.foldLeft(this)(_ add _)

  /** Adds a typed value by its canonical encoding. */
  def addValue[A: Canonical](value: A)(implicit H: Hasher[H]): Either[CanonicalError, BloomFilter[H]] =
    Canonical[A].canonical(value).map(v => add(Cbor.encode(v)))

  /** `false` means definitely absent; `true` means possibly present. */
  def contains(item: ByteVector)(implicit H: Hasher[H]): Boolean = indices(item).forall(bits.get)

  def containsValue[A: Canonical](value: A)(implicit H: Hasher[H]): Either[CanonicalError, Boolean] =
    Canonical[A].canonical(value).map(v => contains(Cbor.encode(v)))

  /** The filter of the union of both item sets. */
  def union(that: BloomFilter[H]): Either[BloomError, BloomFilter[H]] =
    Either.cond(params == that.params, copy(bits = bits | that.bits), BloomError.Mismatch(params, that.params))

  /** A filter at least as precise as the filter of the intersection (bitwise AND). */
  def intersect(that: BloomFilter[H]): Either[BloomError, BloomFilter[H]] =
    Either.cond(params == that.params, copy(bits = bits & that.bits), BloomError.Mismatch(params, that.params))

  def isEmpty: Boolean = bits.populationCount == 0L
  def setBits: Long = bits.populationCount

  /** Diagnostic only (floating point): the false-positive rate implied by the current fill, `(set / m)^k`. */
  def estimatedFalsePositiveRate: Double = math.pow(setBits.toDouble / params.bits.toDouble, params.hashes.toDouble)
}

object BloomFilter {

  private[kd5ujc] val Tag = "dilf4s/bloom/v1"
  private[kd5ujc] val Tags: List[String] = List(Tag)

  private[bloom] def tagged[H: Hasher]: TaggedHash[H] = TaggedHash[H](DomainTag.literal(Tag))

  def empty[H](params: BloomParams): BloomFilter[H] = new BloomFilter[H](params, BitVector.low(params.bits.toLong))

  def of[H: Hasher](params: BloomParams, items: Iterable[ByteVector]): BloomFilter[H] = empty[H](params).addAll(items)

  /** `{ "m": bits, "k": hashes, "b": filter bytes }`, bits MSB-first. */
  def encode[H](f: BloomFilter[H]): CanonicalValue =
    CanonicalValue.trustedMap(List("m" -> CInt(f.params.bits), "k" -> CInt(f.params.hashes), "b" -> CanonicalValue.bytes(f.bits.bytes)))

  def decode[H](v: CanonicalValue): Either[BloomError, BloomFilter[H]] =
    v match {
      case CMap(m) if m.keySet == Set("m", "k", "b") =>
        (m("m"), m("k"), m("b")) match {
          case (CInt(bits), CInt(k), CBytes(b)) if bits.isValidInt && k.isValidInt =>
            BloomParams(bits.toInt, k.toInt).flatMap { p =>
              Either.cond(
                b.size * 8 == p.bits.toLong,
                new BloomFilter[H](p, b.bits),
                BloomError.Encoding(s"expected ${p.bits / 8} bytes, got ${b.size}")
              )
            }
          case other => BloomError.Encoding(s"invalid bloom fields $other").asLeft
        }
      case other => BloomError.Encoding(s"bloom filter must be {m, k, b}: $other").asLeft
    }
}
