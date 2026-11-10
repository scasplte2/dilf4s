package xyz.kd5ujc.accumulators.versioned

import java.nio.charset.StandardCharsets.UTF_8

import cats.syntax.all._
import cats.{Order, Show}

import xyz.kd5ujc.core.canonical.{Cbor, Preview, Utf8}
import xyz.kd5ujc.storage.{KeyCodec, StoreError}

import scodec.bits.ByteVector

/**
 * Why a state key or namespace was refused, by kind, in the order docs/specs/versioned-trie.md §2 gives: generated from
 * `shared/schemas/codec-errors.cddl`, with the Rust and TypeScript SDKs' kinds and fields. A refusal names the input by
 * a [[Preview]], or not at all when it was refused before it was read (too long, or bytes that are not UTF-8), and a
 * segment by its position, never its text (docs/specs/conformance.md §8).
 */
export xyz.kd5ujc.models.codec_errors.{StateKeyError, StateKeyErrorKind}

extension (e: StateKeyError) {

  /** The input, previewed; none for one refused before it was read. */
  def input: Option[Preview] = e match {
    case StateKeyError.TooManySegments(key, _)                                    => Some(key)
    case StateKeyError.InvalidSegment(key, _)                                     => Some(key)
    case StateKeyError.SegmentTooLong(key, _, _)                                  => Some(key)
    case StateKeyError.NamespaceFull(namespace)                                   => Some(namespace)
    case StateKeyError.Empty | StateKeyError.NotUtf8 | (_: StateKeyError.TooLong) => None
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
    else if (s.length > MaxLength || utf8Length(s) > MaxLength) StateKeyError.TooLong(MaxLength.toLong).asLeft
    else {
      // a limit of -1 keeps empty trailing segments, so "a/" is rejected rather than read as "a"
      val segments = s.split("/", -1).toList
      if (segments.size > MaxSegments) StateKeyError.TooManySegments(Preview.of(s), MaxSegments.toLong).asLeft
      else
        segments.zipWithIndex.collectFirst {
          case (seg, at) if !segment.matches(seg)         => StateKeyError.InvalidSegment(Preview.of(s), at.toLong)
          case (seg, at) if seg.length > MaxSegmentLength => StateKeyError.SegmentTooLong(Preview.of(s), at.toLong, MaxSegmentLength.toLong)
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
    if (bytes.size > MaxLength) StateKeyError.TooLong(MaxLength.toLong).asLeft
    else Either.cond(Utf8.isWellFormed(bytes.toArray), new String(bytes.toArray, UTF_8), StateKeyError.NotUtf8).flatMap(parse)

  /** Byte order, which for this grammar's ASCII alphabet is also string order: the trie's key order. */
  implicit val stateKeyOrder: Order[StateKey] = Order.from((a, b) => Cbor.compareBytes(a.bytes, b.bytes))
  implicit val stateKeyOrdering: Ordering[StateKey] = stateKeyOrder.toOrdering
  implicit val stateKeyShow: Show[StateKey] = Show.show(_.value)

  implicit val stateKeyCodec: KeyCodec[StateKey] =
    KeyCodec.instance(
      _.bytes,
      b =>
        fromBytes(b).leftMap(e =>
          StoreError.Encoding(xyz.kd5ujc.storage.StoreContext.StateKey, xyz.kd5ujc.storage.EncodingReason.StateKey(e))
        )
    )
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
