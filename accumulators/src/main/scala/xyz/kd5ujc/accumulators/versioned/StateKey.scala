package xyz.kd5ujc.accumulators.versioned

import java.nio.charset.StandardCharsets.UTF_8

import cats.syntax.all._
import cats.{Order, Show}

import xyz.kd5ujc.core.canonical.{Cbor, Utf8}
import xyz.kd5ujc.storage.{KeyCodec, StoreError}

import scodec.bits.ByteVector

/** Why a commit key or namespace was rejected. */
final case class StateKeyError(input: String, reason: String)

/**
 * A key of a versioned trie's state: `segment *("/" segment)`, each segment `[a-z0-9][a-z0-9._-]*` (docs/specs/versioned-trie.md
 * §2). Its trie key is its UTF-8 bytes, so keys order by code point and a namespace is a byte prefix.
 */
sealed abstract case class StateKey(value: String) {
  def bytes: ByteVector = ByteVector(value.getBytes(UTF_8))
  override def toString: String = value
}

object StateKey {

  val MaxSegmentLength: Int = 64
  val MaxSegments: Int = 16
  val MaxLength: Int = 256

  private val segment = "[a-z0-9][a-z0-9._-]*".r

  def parse(s: String): Either[StateKeyError, StateKey] = {
    // a limit of -1 keeps empty trailing segments, so "a/" is rejected rather than read as "a"
    val segments = s.split("/", -1).toList
    def bad(reason: String) = StateKeyError(s, reason).asLeft[StateKey]
    if (s.isEmpty) bad("empty key")
    else if (s.length > MaxLength) bad(s"longer than $MaxLength characters")
    else if (segments.size > MaxSegments) bad(s"more than $MaxSegments segments")
    else
      segments.find(seg => seg.length > MaxSegmentLength || !segment.matches(seg)) match {
        case Some(seg) if seg.length > MaxSegmentLength => bad(s"segment '$seg' is longer than $MaxSegmentLength characters")
        case Some(seg)                                  => bad(s"segment '$seg' does not match [a-z0-9][a-z0-9._-]*")
        case None                                       => (new StateKey(s) {}).asRight
      }
  }

  def fromBytes(bytes: ByteVector): Either[StateKeyError, StateKey] =
    Either.cond(Utf8.isWellFormed(bytes.toArray), new String(bytes.toArray, UTF_8), StateKeyError(bytes.toHex, "not UTF-8")).flatMap(parse)

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
  override def toString: String = value
}

object StateNamespace {

  val MaxLength: Int = StateKey.MaxLength - 2

  def parse(s: String): Either[StateKeyError, StateNamespace] =
    StateKey.parse(s).flatMap { k =>
      Either.cond(
        k.value.length <= MaxLength && k.value.count(_ == '/') < StateKey.MaxSegments - 1,
        new StateNamespace(k.value) {},
        StateKeyError(s, "a namespace must leave room for one more segment")
      )
    }
}
