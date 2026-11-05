package xyz.kd5ujc.accumulators.versioned

import java.nio.charset.StandardCharsets.UTF_8

import cats.syntax.all._
import cats.{Order, Show}

import xyz.kd5ujc.core.canonical.{Cbor, Preview, Utf8}
import xyz.kd5ujc.storage.{KeyCodec, StoreError}

import scodec.bits.ByteVector

/**
 * Why a state key or namespace was refused, by kind, in the order docs/specs/versioned-trie.md §2 gives. A refusal
 * names the input by a [[Preview]], or not at all when it was refused before it was read (too long, or bytes that are
 * not UTF-8), and a segment by its position, never its text (docs/specs/conformance.md §8).
 */
sealed trait StateKeyError extends Product with Serializable {

  /** The input, previewed; none for one refused before it was read. */
  def input: Option[Preview]
}

object StateKeyError {

  /** The empty text. */
  case object Empty extends StateKeyError {
    def input: Option[Preview] = None
  }

  /** More than `limit` UTF-8 bytes ([[StateKey.MaxLength]]), refused by its length before it is read. */
  final case class TooLong(limit: Int) extends StateKeyError {
    def input: Option[Preview] = None
  }

  /** Trie-key bytes that are not well-formed UTF-8. */
  case object NotUtf8 extends StateKeyError {
    def input: Option[Preview] = None
  }

  /** More than `limit` segments ([[StateKey.MaxSegments]]). */
  final case class TooManySegments(key: Preview, limit: Int) extends StateKeyError {
    def input: Option[Preview] = Some(key)
  }

  /** Segment `segment` (from 0) does not match `[a-z0-9][a-z0-9._-]*`. */
  final case class InvalidSegment(key: Preview, segment: Int) extends StateKeyError {
    def input: Option[Preview] = Some(key)
  }

  /** Segment `segment` (from 0) is longer than `limit` characters ([[StateKey.MaxSegmentLength]]). */
  final case class SegmentTooLong(key: Preview, segment: Int, limit: Int) extends StateKeyError {
    def input: Option[Preview] = Some(key)
  }

  /** A namespace that leaves no room for one more segment: over 254 characters, or 16 segments already. */
  final case class NamespaceFull(namespace: Preview) extends StateKeyError {
    def input: Option[Preview] = Some(namespace)
  }

  /** A short message for logs and store errors; the kind and the normative fields are what conformance compares. */
  def describe(e: StateKeyError): String = e match {
    case Empty                        => "empty key"
    case TooLong(limit)               => s"longer than $limit UTF-8 bytes"
    case NotUtf8                      => "not UTF-8"
    case TooManySegments(k, limit)    => s"$k has more than $limit segments"
    case InvalidSegment(k, at)        => s"$k: segment $at does not match [a-z0-9][a-z0-9._-]*"
    case SegmentTooLong(k, at, limit) => s"$k: segment $at is longer than $limit characters"
    case NamespaceFull(ns)            => s"namespace $ns leaves no room for one more segment"
  }
}

/**
 * A key of a versioned trie's state: `segment *("/" segment)`, each segment `[a-z0-9][a-z0-9._-]*` (docs/specs/versioned-trie.md
 * §2). Its trie key is its UTF-8 bytes, so keys order by code point and a namespace is a byte prefix.
 */
sealed abstract case class StateKey(value: String) {
  def bytes: ByteVector = ByteVector(value.getBytes(UTF_8))

  /** The key as a diagnostic names it (docs/specs/conformance.md §8). */
  def preview: Preview = Preview.of(value)
  override def toString: String = value
}

object StateKey {

  val MaxSegmentLength: Int = xyz.kd5ujc.models.limits.StateKeyMaxSegmentLength
  val MaxSegments: Int = xyz.kd5ujc.models.limits.StateKeyMaxSegments
  val MaxLength: Int = xyz.kd5ujc.models.limits.StateKeyMaxLength

  private val segment = "[a-z0-9][a-z0-9._-]*".r

  /**
   * Refusals come in the order of docs/specs/versioned-trie.md §2: empty; over [[MaxLength]] UTF-8 bytes; over
   * [[MaxSegments]] segments; then each segment in order, its grammar before its length. The length is measured before
   * the text is split, and only within [[MaxLength]] code units (a UTF-8 length is never shorter), so a refusal costs
   * O(MaxLength) whatever the input: an over-long key is refused by its length alone, never previewed (a preview
   * measures the whole text).
   */
  def parse(s: String): Either[StateKeyError, StateKey] =
    if (s.isEmpty) StateKeyError.Empty.asLeft
    else if (s.length > MaxLength || utf8Length(s) > MaxLength) StateKeyError.TooLong(MaxLength).asLeft
    else {
      // a limit of -1 keeps empty trailing segments, so "a/" is rejected rather than read as "a"
      val segments = s.split("/", -1).toList
      if (segments.size > MaxSegments) StateKeyError.TooManySegments(Preview.of(s), MaxSegments).asLeft
      else
        segments.zipWithIndex.collectFirst {
          case (seg, at) if !segment.matches(seg)         => StateKeyError.InvalidSegment(Preview.of(s), at)
          case (seg, at) if seg.length > MaxSegmentLength => StateKeyError.SegmentTooLong(Preview.of(s), at, MaxSegmentLength)
        }.toLeft(new StateKey(s) {})
    }

  /**
   * The UTF-8 length of `s`, an unpaired surrogate counting 3 (as a preview counts it); called only on a text of at
   * most [[MaxLength]] code units.
   */
  private def utf8Length(s: String): Int = {
    @scala.annotation.tailrec
    def go(i: Int, n: Int): Int =
      if (i >= s.length) n
      else {
        val c = s.charAt(i)
        if (c < 0x80) go(i + 1, n + 1)
        else if (c < 0x800) go(i + 1, n + 2)
        else if (Character.isHighSurrogate(c) && i + 1 < s.length && Character.isLowSurrogate(s.charAt(i + 1))) go(i + 2, n + 4)
        else go(i + 1, n + 3)
      }
    go(0, 0)
  }

  /** Bytes longer than [[MaxLength]] are refused by their length, before they are read as text; then UTF-8, then [[parse]]. */
  def fromBytes(bytes: ByteVector): Either[StateKeyError, StateKey] =
    if (bytes.size > MaxLength) StateKeyError.TooLong(MaxLength).asLeft
    else Either.cond(Utf8.isWellFormed(bytes.toArray), new String(bytes.toArray, UTF_8), StateKeyError.NotUtf8).flatMap(parse)

  /** Byte order, which for this grammar's ASCII alphabet is also string order: the trie's key order. */
  implicit val stateKeyOrder: Order[StateKey] = Order.from((a, b) => Cbor.compareBytes(a.bytes, b.bytes))
  implicit val stateKeyOrdering: Ordering[StateKey] = stateKeyOrder.toOrdering
  implicit val stateKeyShow: Show[StateKey] = Show.show(_.value)

  implicit val stateKeyCodec: KeyCodec[StateKey] =
    KeyCodec.instance(_.bytes, b => fromBytes(b).leftMap(e => StoreError.Codec("state key", StateKeyError.describe(e))))
}

/**
 * A namespace: a key `ns` naming the keys below `ns/`. Its prefix is `utf8(ns + "/")`, so namespace `fiber` never
 * matches `fiberx/...`, and a key under it is `ns/` plus at least one segment, which bounds `ns` to 254 characters.
 */
sealed abstract case class StateNamespace(value: String) {
  def prefix: ByteVector = ByteVector((value + "/").getBytes(UTF_8))
  def contains(key: StateKey): Boolean = key.bytes.startsWith(prefix)

  /** The namespace as a diagnostic names it (docs/specs/conformance.md §8). */
  def preview: Preview = Preview.of(value)
  override def toString: String = value
}

object StateNamespace {

  val MaxLength: Int = xyz.kd5ujc.models.limits.StateNamespaceMaxLength

  def parse(s: String): Either[StateKeyError, StateNamespace] =
    StateKey.parse(s).flatMap { k =>
      Either.cond(
        k.value.length <= MaxLength && k.value.count(_ == '/') < StateKey.MaxSegments - 1,
        new StateNamespace(k.value) {},
        StateKeyError.NamespaceFull(Preview.of(s))
      )
    }
}
