package xyz.kd5ujc.core.hash

import java.nio.charset.StandardCharsets.UTF_8

import scala.annotation.tailrec

import xyz.kd5ujc.core.canonical.{ByteSource, Canonical, CanonicalError, CanonicalValue, Cbor, Preview}

import scodec.bits.ByteVector

/** A versioned domain-separation tag, e.g. `dilf4s/mpt/leaf/v1` (docs/specs/canonical-encoding.md §4). */
sealed abstract case class DomainTag(value: String)

object DomainTag {

  /**
   * The whole of the tag grammar (canonical-encoding.md §4): a name of `[a-z0-9]+`, any number of `/` and a segment of
   * `[a-z0-9][a-z0-9._-]*`, then `/v` and `[0-9]+`. Read as its `/`-separated parts, in one pass: `java.util.regex`
   * recurses once per repetition of a group, so a tag of many segments would overflow the stack.
   */
  def apply(value: String): Either[HashError, DomainTag] =
    Either.cond(wellFormed(parts(value, 0, Vector.empty)), new DomainTag(value) {}, HashError.InvalidDomainTag(Preview.of(value)))

  private def wellFormed(ps: Vector[String]): Boolean =
    ps.size >= 2 &&
    ps.headOption.exists(p => p.nonEmpty && p.forall(alphanumeric)) &&
    ps.slice(1, ps.size - 1).forall(p => p.headOption.exists(alphanumeric) && p.forall(c => alphanumeric(c) || ".-_".contains(c))) &&
    ps.lastOption.exists(p => p.length >= 2 && p.charAt(0) == 'v' && p.drop(1).forall(digit))

  private def digit(c:        Char): Boolean = c >= '0' && c <= '9'
  private def alphanumeric(c: Char): Boolean = digit(c) || (c >= 'a' && c <= 'z')

  /** The `/`-separated parts of `s` from `from`, empty ones included. */
  @tailrec
  private def parts(s: String, from: Int, acc: Vector[String]): Vector[String] =
    s.indexOf('/', from) match {
      case -1 => acc :+ s.substring(from)
      case at => parts(s, at + 1, acc :+ s.substring(from, at))
    }

  /**
   * For dilf4s's own compile-time tags only: skips validation instead of throwing. Every library tag is checked
   * against [[apply]] by `LibraryTagsSuite`.
   */
  private[kd5ujc] def literal(value: String): DomainTag = new DomainTag(value) {}
}

/**
 * `taggedHash(tag, msg) = H(H(tag) || H(tag) || msg)`, the BIP-340 construction over any `H`. Building a [[TaggedHash]]
 * hashes the tag once, so hashing many messages under one tag costs one hash each.
 */
final class TaggedHash[H] private (val tag: DomainTag, prefix: ByteVector)(implicit H: Hasher[H]) {

  private val prefixed: ByteSource = ByteSource.bytes(prefix)

  def apply(message: ByteVector): Digest[H] = H.digest(prefix ++ message)

  /** The tagged hash of `message`'s bytes, absorbed as they are written (`Hasher.digestOf`). */
  def of(message: ByteSource): Digest[H] = H.digestOf(prefixed ++ message)

  /**
   * Content hash of an already-canonical value, its encoding streamed into the hash: never built whole.
   * [[valueWithin]] at `Cbor.MaxEncodedBytes`, as the TypeScript SDK's `content`: an encoding longer is `TooLarge`.
   */
  def value(v: CanonicalValue): Either[Cbor.TooLarge, Digest[H]] = valueWithin(v, Cbor.MaxEncodedBytes)

  /**
   * [[value]], if the encoding is at most `maxBytes` long, with the bounds and refusal of `Cbor.encodeWithin`: a value
   * heavier than the limit is refused from its weight, in O(1), and a refused value is never hashed, not even in part.
   */
  def valueWithin(v: CanonicalValue, maxBytes: Long): Either[Cbor.TooLarge, Digest[H]] = Cbor.sourceWithin(v, maxBytes).map(of)

  /** Content hash of typed data: canonicalise, encode deterministically, tagged-hash, within [[value]]'s ceiling. */
  def content[A](a: A)(implicit C: Canonical[A]): Either[CanonicalError | Cbor.TooLarge, Digest[H]] = C.canonical(a).flatMap(value)
}

object TaggedHash {

  def apply[H](tag: DomainTag)(implicit H: Hasher[H]): TaggedHash[H] = {
    val t = H.digest(ByteVector(tag.value.getBytes(UTF_8))).bytes
    new TaggedHash[H](tag, t ++ t)
  }
}
