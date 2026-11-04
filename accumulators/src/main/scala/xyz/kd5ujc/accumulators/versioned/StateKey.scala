package xyz.kd5ujc.accumulators.versioned

import java.nio.charset.StandardCharsets.UTF_8

import cats.syntax.all._
import cats.{Order, Show}

import xyz.kd5ujc.core.canonical.{Cbor, Preview, Utf8}
import xyz.kd5ujc.storage.{KeyCodec, StoreError}

import scodec.bits.ByteVector

/**
 * Why a commit key or namespace was rejected: the input as a [[Preview]], or none for bytes that are not text
 * (docs/specs/conformance.md §8).
 */
final case class StateKeyError(input: Option[Preview], reason: String)

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
   * The length is checked before the text is read, so a refusal costs O(MaxLength) whatever the input: an over-long
   * key is refused by its length alone, never previewed (a preview measures the whole text).
   */
  def parse(s: String): Either[StateKeyError, StateKey] = {
    def bad(reason: String) = StateKeyError(Some(Preview.of(s)), reason).asLeft[StateKey]
    if (s.isEmpty) bad("empty key")
    else if (s.length > MaxLength) StateKeyError(None, s"${s.length} characters, longer than $MaxLength").asLeft
    else {
      // a limit of -1 keeps empty trailing segments, so "a/" is rejected rather than read as "a"
      val segments = s.split("/", -1).toList
      if (segments.size > MaxSegments) bad(s"more than $MaxSegments segments")
      else
        segments.zipWithIndex.find { case (seg, _) => seg.length > MaxSegmentLength || !segment.matches(seg) } match {
          case Some((seg, at)) if seg.length > MaxSegmentLength => bad(s"segment $at is longer than $MaxSegmentLength characters")
          case Some((_, at))                                    => bad(s"segment $at does not match [a-z0-9][a-z0-9._-]*")
          case None                                             => (new StateKey(s) {}).asRight
        }
    }
  }

  /** Bytes longer than [[MaxLength]] are refused by their length, before they are read as text. */
  def fromBytes(bytes: ByteVector): Either[StateKeyError, StateKey] =
    if (bytes.size > MaxLength) StateKeyError(None, s"${bytes.size} bytes, longer than $MaxLength").asLeft
    else Either.cond(Utf8.isWellFormed(bytes.toArray), new String(bytes.toArray, UTF_8), StateKeyError(None, "not UTF-8")).flatMap(parse)

  /** Byte order, which for this grammar's ASCII alphabet is also string order: the trie's key order. */
  implicit val stateKeyOrder: Order[StateKey] = Order.from((a, b) => Cbor.compareBytes(a.bytes, b.bytes))
  implicit val stateKeyOrdering: Ordering[StateKey] = stateKeyOrder.toOrdering
  implicit val stateKeyShow: Show[StateKey] = Show.show(_.value)

  implicit val stateKeyCodec: KeyCodec[StateKey] =
    KeyCodec.instance(_.bytes, b => fromBytes(b).leftMap(e => StoreError.Codec("state key", e.reason)))
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
        StateKeyError(Some(Preview.of(s)), "a namespace must leave room for one more segment")
      )
    }
}
