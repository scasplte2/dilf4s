package xyz.kd5ujc.core.hash

import java.nio.charset.StandardCharsets.UTF_8

import xyz.kd5ujc.core.canonical.{Canonical, CanonicalError, CanonicalValue, Cbor}

import scodec.bits.ByteVector

/** A versioned domain-separation tag, e.g. `dilf4s/mpt/leaf/v1` (docs/specs/canonical-encoding.md §4). */
sealed abstract case class DomainTag(value: String)

object DomainTag {

  private val Grammar = "^[a-z0-9]+(/[a-z0-9][a-z0-9._-]*)*/v[0-9]+$".r.pattern

  def apply(value: String): Either[HashError, DomainTag] =
    Either.cond(Grammar.matcher(value).matches(), new DomainTag(value) {}, HashError.InvalidDomainTag(value))
}

/**
 * `taggedHash(tag, msg) = H(H(tag) || H(tag) || msg)`, the BIP-340 construction over any `H`. Building a [[TaggedHash]]
 * hashes the tag once, so hashing many messages under one tag costs one hash each.
 */
final class TaggedHash[H] private (val tag: DomainTag, prefix: ByteVector)(implicit H: Hasher[H]) {

  def apply(message: ByteVector): Digest[H] = H.digest(prefix ++ message)

  /** Content hash of an already-canonical value. */
  def value(v: CanonicalValue): Digest[H] = apply(Cbor.encode(v))

  /** Content hash of typed data: canonicalise, encode deterministically, tagged-hash. */
  def content[A](a: A)(implicit C: Canonical[A]): Either[CanonicalError, Digest[H]] = C.canonical(a).map(value)
}

object TaggedHash {

  def apply[H](tag: DomainTag)(implicit H: Hasher[H]): TaggedHash[H] = {
    val t = H.digest(ByteVector(tag.value.getBytes(UTF_8))).bytes
    new TaggedHash[H](tag, t ++ t)
  }
}
