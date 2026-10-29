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

  /** Decodes `{n, b}` with canonical zero padding. */
  def decodeSegment(v: CanonicalValue): Either[TrieError, BitVector] =
    v match {
      case CMap(m) if m.keySet == Set("n", "b") =>
        (m("n"), m("b")) match {
          case (CInt(n), CBytes(b)) if n >= 0 && n <= b.size * 8 && b.size == (n + 7) / 8 =>
            val bits = b.bits.take(n.toLong)
            Either.cond(bits.toByteVector == b, bits, TrieError.Encoding("segment padding bits must be zero"))
          case other => TrieError.Encoding(s"invalid segment fields $other").asLeft
        }
      case other => TrieError.Encoding(s"segment must be {n, b}: $other").asLeft
    }
}

private[trie] object Bits {

  /** Length of the longest common prefix of two bit vectors. */
  def commonPrefix(a: BitVector, b: BitVector): Long = {
    @tailrec
    def go(i: Long): Long = if (i < a.size && i < b.size && a(i) == b(i)) go(i + 1) else i
    go(0L)
  }
}
