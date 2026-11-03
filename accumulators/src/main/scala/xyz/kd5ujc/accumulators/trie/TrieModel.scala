package xyz.kd5ujc.accumulators.trie

import cats.syntax.all._

import scala.annotation.tailrec

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}
import xyz.kd5ujc.core.hash.{Digest, DomainTag, Hasher, TaggedHash}

import scodec.bits.{BitVector, ByteVector}

/** How keys are validated (docs/specs/trie.md §1). */
sealed trait KeyMode extends Product with Serializable {

  /** The key's path (MSB-first bits) if the key is valid in this mode. */
  def path(key: ByteVector): Either[TrieError, BitVector]

  /** Longest possible path, in bits. */
  def maxBits: Long

  def allowsBranchValues: Boolean
}

object KeyMode {

  val MaxFixedBytes: Int = 1024
  val MaxVariableBytes: Int = 65535

  sealed abstract case class Fixed(bytes: Int) extends KeyMode {
    def path(key: ByteVector): Either[TrieError, BitVector] =
      Either.cond(key.size == bytes.toLong, key.bits, TrieError.InvalidKey(s"fixed($bytes) mode needs $bytes-byte keys, got ${key.size}"))
    def maxBits: Long = bytes.toLong * 8
    def allowsBranchValues: Boolean = false
  }

  sealed abstract case class Variable(maxBytes: Int) extends KeyMode {
    def path(key: ByteVector): Either[TrieError, BitVector] =
      Either.cond(
        key.size <= maxBytes.toLong,
        key.bits,
        TrieError.InvalidKey(s"variable($maxBytes) mode allows at most $maxBytes bytes, got ${key.size}")
      )
    def maxBits: Long = maxBytes.toLong * 8
    def allowsBranchValues: Boolean = true
  }

  def fixed(bytes: Int): Either[TrieError, KeyMode] =
    Either.cond(
      bytes >= 1 && bytes <= MaxFixedBytes,
      new Fixed(bytes) {},
      TrieError.InvalidKey(s"fixed key length must be 1..$MaxFixedBytes")
    )

  def variable(maxBytes: Int): Either[TrieError, KeyMode] =
    Either.cond(
      maxBytes >= 0 && maxBytes <= MaxVariableBytes,
      new Variable(maxBytes) {},
      TrieError.InvalidKey(s"variable key limit must be 0..$MaxVariableBytes")
    )
}

sealed trait TrieError extends Product with Serializable

object TrieError {
  final case class InvalidKey(reason: String) extends TrieError
  final case class MissingNode(hash: String) extends TrieError
  final case class InvalidNode(reason: String) extends TrieError
  final case class InvalidProof(reason: String) extends TrieError
  final case class RootMismatch(expected: String, computed: String) extends TrieError
  final case class Encoding(reason: String) extends TrieError

  /** A verified proof that does not reach the query: it establishes neither presence nor absence. */
  final case class NotCovered(what: String) extends TrieError

  /**
   * Over `limit` (`ProofInput.MaxProofWeight`) in weight: claimed values a verifier would hash, or a proof a producer
   * would emit that every verifier refuses.
   */
  final case class TooLarge(limit: Long) extends TrieError
}

/** A stored trie node; children are referenced by hash (docs/specs/trie.md §2). */
sealed trait Node[H] extends Product with Serializable {
  def segment: BitVector
  def hash: Digest[H]
}

object Node {

  final case class Leaf[H] private[trie] (segment: BitVector, value: Digest[H], hash: Digest[H]) extends Node[H]

  final case class Branch[H] private[trie] (
    segment: BitVector,
    value:   Option[Digest[H]],
    left:    Option[Digest[H]],
    right:   Option[Digest[H]],
    hash:    Digest[H]
  ) extends Node[H] {
    def child(bit: Boolean): Option[Digest[H]] = if (bit) right else left
    def childCount: Int = left.fold(0)(_ => 1) + right.fold(0)(_ => 1)
  }

  def leaf[H](segment: BitVector, value: Digest[H])(implicit T: TrieHashing[H]): Leaf[H] =
    Leaf(segment, value, T.leaf(segment, value))

  /** Callers guarantee the branch invariant: two children, or one child and a value. */
  def branch[H](segment: BitVector, value: Option[Digest[H]], left: Option[Digest[H]], right: Option[Digest[H]])(
    implicit T: TrieHashing[H]
  ): Branch[H] = Branch(segment, value, left, right, T.branch(segment, value, left, right))

  def validBranch(value: Option[Any], children: Int): Boolean = children == 2 || (children == 1 && value.isDefined)
}

/** Hashing and canonical node encodings (docs/specs/trie.md §3, §5). */
trait TrieHashing[H] {
  def hasher: Hasher[H]
  def empty: Digest[H]
  def value(bytes:    ByteVector): Digest[H]
  def leaf(segment:   BitVector, value: Digest[H]): Digest[H]
  def branch(segment: BitVector, value: Option[Digest[H]], left: Option[Digest[H]], right: Option[Digest[H]]): Digest[H]
}

object TrieHashing {

  def apply[H](implicit T: TrieHashing[H]): TrieHashing[H] = T

  private[kd5ujc] val ValueTag = "dilf4s/trie/value/v1"
  private[kd5ujc] val LeafTag = "dilf4s/trie/leaf/v1"
  private[kd5ujc] val BranchTag = "dilf4s/trie/branch/v1"
  private[kd5ujc] val EmptyTag = "dilf4s/trie/empty/v1"
  private[kd5ujc] val Tags: List[String] = List(ValueTag, LeafTag, BranchTag, EmptyTag)

  private def tag(name: String): DomainTag = DomainTag.literal(name)

  implicit def tagged[H](implicit H: Hasher[H]): TrieHashing[H] = new TrieHashing[H] {
    val hasher: Hasher[H] = H
    private val valueTag = TaggedHash[H](tag(ValueTag))
    private val leafTag = TaggedHash[H](tag(LeafTag))
    private val branchTag = TaggedHash[H](tag(BranchTag))
    val empty: Digest[H] = TaggedHash[H](tag(EmptyTag)).apply(ByteVector.empty)
    def value(bytes:  ByteVector): Digest[H] = valueTag(bytes)
    def leaf(segment: BitVector, value: Digest[H]): Digest[H] = leafTag(Cbor.encode(NodeCodec.leaf(segment, value)))
    def branch(segment: BitVector, value: Option[Digest[H]], left: Option[Digest[H]], right: Option[Digest[H]]): Digest[H] =
      branchTag(Cbor.encode(NodeCodec.branch(segment, value.map(_.bytes), left.map(_.bytes), right.map(_.bytes))))
  }
}

/** Canonical encodings of segments and node commitments; maps here have fixed valid keys, so construction is total. */
private[trie] object NodeCodec {

  private def fields(entries: List[(String, CanonicalValue)]): CanonicalValue = CanonicalValue.trustedMap(entries)

  def segment(bits: BitVector): CanonicalValue = fields(List("n" -> CInt(bits.size), "b" -> CanonicalValue.bytes(bits.toByteVector)))

  def leaf[H](segmentBits: BitVector, value: Digest[H]): CanonicalValue =
    fields(List("s" -> segment(segmentBits), "v" -> CanonicalValue.bytes(value.bytes)))

  def branch(segmentBits: BitVector, value: Option[ByteVector], left: Option[ByteVector], right: Option[ByteVector]): CanonicalValue =
    fields(
      List("s" -> segment(segmentBits)) ++ value.map(v => "v" -> CanonicalValue.bytes(v)).toList ++
      left.map(l => "l" -> CanonicalValue.bytes(l)).toList ++ right.map(r => "r" -> CanonicalValue.bytes(r)).toList
    )

  /** The domain segment of a wire segment: `len(b) == ceil(n / 8)` and zero padding, which the schema cannot state. */
  def fromWire(w: xyz.kd5ujc.models.trie.TrieSegment): Either[TrieError, BitVector] =
    if (w.n > w.b.size * 8 || w.b.size != (w.n + 7) / 8) TrieError.Encoding(s"segment of ${w.n} bits in ${w.b.size} bytes").asLeft
    else {
      val bits = w.b.bits.take(w.n)
      Either.cond(bits.toByteVector == w.b, bits, TrieError.Encoding("segment padding bits must be zero"))
    }

  def decodeSegment(v: CanonicalValue): Either[TrieError, BitVector] =
    xyz.kd5ujc.models.trie.TrieSegment.codec.decoder.decode(v).leftMap(e => TrieError.Encoding(e.message)).flatMap(fromWire)
}

private[trie] object Bits {

  /** Length of the longest common prefix of two bit vectors. */
  def commonPrefix(a: BitVector, b: BitVector): Long = agreeing(a, 0L, b, 0L, a.size.min(b.size))

  /** How many bits of `a` from `ia` and `b` from `ib` agree before the first difference, at most `n`: `n` in range. */
  def agreeing(a: BitVector, ia: Long, b: BitVector, ib: Long, n: Long): Long = {
    @tailrec
    def go(i: Long): Long = if (i < n && a(ia + i) == b(ib + i)) go(i + 1) else i
    go(0L)
  }

  /** No path is longer in any mode (`KeyMode.MaxVariableBytes` bytes): a bound compares with paths as these bits do. */
  val PathCap: Long = KeyMode.MaxVariableBytes.toLong * 8 + 1

  /** The most bits `compact` holds in one array (2^31 - 1 bytes): it refuses more. */
  private val Compactable: Long = Int.MaxValue.toLong * 8

  /**
   * `bits` in one flat array, so reading a bit is O(1) and stack-safe however the vector was built (a rope of appends
   * indexes recursively, in time and stack its depth); and the bits copied. Callers pass at most `PathCap` bits, far
   * below what `compact` refuses. Flattening is a no-op on a vector that is already flat.
   */
  def flat(bits: BitVector): (BitVector, Long) = (bits.compact, bits.size)

  /**
   * A bound or prefix flattened for reading, cut to its first `PathCap` bits: a walk reads no further, since every
   * verified path is shorter, and a path compares with the cut bound as with the whole (docs/specs/trie.md §4). With
   * the bits copied: the whole vector when flattened before cutting, which is stack-safe on any rope `compact` holds.
   */
  def readable(bits: BitVector): (BitVector, Long) =
    if (bits.size <= PathCap) flat(bits)
    else if (bits.size <= Compactable) (bits.compact.take(PathCap), bits.size)
    else (bits.take(PathCap).compact, PathCap)

  /**
   * Whether the path held as `pieces` (newest first, each flat) and `depth` bits long is `key`, and the bits compared
   * or copied. The length is compared first, so the work is bounded by the key's length.
   */
  def spells(pieces: List[BitVector], depth: Long, key: BitVector): (Boolean, Long) = {
    @tailrec
    def go(k: BitVector, rest: List[BitVector], end: Long, steps: Long): (Boolean, Long) =
      rest match {
        case Nil => (true, steps)
        case p :: older =>
          val start = end - p.size
          val same = agreeing(p, 0L, k, start, p.size)
          if (same < p.size) (false, steps + same + 1) else go(k, older, start, steps + same + 1)
      }
    if (key.size != depth) (false, 1L)
    else {
      val (k, copied) = flat(key)
      go(k, pieces, depth, copied + 1)
    }
  }

}

/**
 * Where a walk's path stands against a bound in key order (bitwise, a proper prefix before its extensions). `Along`:
 * the path is a prefix of the bound, equal to it when as long. Once the two part, every extension of the path stays on
 * the same side, so a walk carries the side down and compares only the bits each node adds (docs/specs/trie.md §4).
 */
private[trie] sealed trait Side extends Product with Serializable

private[trie] object Side {
  case object Along extends Side
  case object Below extends Side
  case object Above extends Side

  /** The side of the path extended by `bits`, from the side of the path (`depth` bits long); and the bits compared. */
  def extend(side: Side, depth: Long, bits: BitVector, bound: BitVector): (Side, Long) =
    side match {
      case Along =>
        val room = bound.size - depth
        val n = bits.size.min(room)
        val same = Bits.agreeing(bits, 0L, bound, depth, n)
        val next =
          if (same < n) { if (bits(same)) Above else Below }
          else if (bits.size > room) Above
          else Along
        (next, same + 1)
      case parted => (parted, 0L)
    }

  /** Whether the path (`depth` bits long) sorts before `bound`. */
  def before(side: Side, depth: Long, bound: BitVector): Boolean =
    side match {
      case Along => depth < bound.size
      case Below => true
      case Above => false
    }
}

/** Key bounds `[from, until)`; `until` absent is unbounded. */
final private[trie] case class Bounds(from: BitVector, until: Option[BitVector]) {

  /** The cursor moved past `bits`, and the bits compared with the bounds. */
  def extend[P](c: Cursor[P], bits: BitVector, grow: (P, BitVector) => P): (Cursor[P], Long) = {
    val (f, nf) = Side.extend(c.from, c.depth, bits, from)
    val (u, nu) = until.fold((c.until, 0L))(Side.extend(c.until, c.depth, bits, _))
    (Cursor(grow(c.path, bits), c.depth + bits.size, f, u), nf + nu + 1)
  }

  /** Whether the cursor's path is a key in the bounds: `from <= path < until`. */
  def holds(c: Cursor[?]): Boolean = !Side.before(c.from, c.depth, from) && until.forall(Side.before(c.until, c.depth, _))

  /**
   * Whether keys starting with the cursor's path can lie in the bounds: not wholly below (`from` does not extend the
   * path, and the path sorts before it) and not wholly above (`until <= path`).
   */
  def meets(c: Cursor[?]): Boolean = c.from != Side.Below && until.forall(Side.before(c.until, c.depth, _))
}

private[trie] object Bounds {

  /** Every key. */
  val All: Bounds = Bounds(BitVector.empty, None)

  /** `[from, until)` with each bound readable (`Bits.readable`), and the bits copied. */
  def readable(from: BitVector, until: Option[BitVector]): (Bounds, Long) = {
    val (f, nf) = Bits.readable(from)
    val (u, nu) = until.map(Bits.readable).fold((Option.empty[BitVector], 0L)) { case (b, n) => (Some(b), n) }
    (Bounds(f, u), nf + nu)
  }
}

/**
 * A walk's position: the path so far, carried as the walk needs it (`P`: pieces to compare, a vector to list, or
 * nothing), its length in bits, and where it stands against each bound.
 */
final private[trie] case class Cursor[P](path: P, depth: Long, from: Side, until: Side)

private[trie] object Cursor {

  /** The empty path, a prefix of every bound. */
  def origin[P](path: P): Cursor[P] = Cursor(path, 0L, Side.Along, Side.Along)
}
