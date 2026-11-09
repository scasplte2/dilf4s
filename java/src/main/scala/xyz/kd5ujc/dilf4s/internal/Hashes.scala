package xyz.kd5ujc.dilf4s.internal

import cats.syntax.all._

import xyz.kd5ujc.accumulators.ValueCommitment
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor, Preview}
import xyz.kd5ujc.core.hash._
import xyz.kd5ujc.dilf4s.Hashing.Algorithm
import xyz.kd5ujc.dilf4s.Result
import xyz.kd5ujc.dilf4s.error.{EncodeError, HashError => JHashError, TooLarge, TypeMismatch}

import scodec.bits.ByteVector

/** Hashing for the Java API: the seven hash functions, digests, domain tags, tagged hashing and value commitments. */
object Hashes {

  /** The reference's hash function for `a`. */
  def hasher(a: Algorithm): Hasher[?] =
    a match {
      case Algorithm.SHA_256     => Hasher.sha256
      case Algorithm.SHA_512     => Hasher.sha512
      case Algorithm.SHA3_256    => Hasher.sha3_256
      case Algorithm.SHA3_512    => Hasher.sha3_512
      case Algorithm.KECCAK_256  => Hasher.keccak256
      case Algorithm.BLAKE2B_256 => Hasher.blake2b256
      case Algorithm.BLAKE2B_512 => Hasher.blake2b512
    }

  private def hashError(e: HashError): JHashError = Errors.hashError(e)

  def name(a:       Algorithm): String = hasher(a).name
  def outputSize(a: Algorithm): Long = hasher(a).outputSize
  def blockSize(a:  Algorithm): Long = hasher(a).blockSize

  /** The algorithm with the reference's name `name`; any other name is the reference's `UnknownAlgorithm`. */
  def named(name: String): Result[Algorithm, JHashError] =
    Values.result(
      Hasher
        .named(name)
        .flatMap(h => Algorithm.values.toList.find(a => hasher(a).name == h.name).toRight(HashError.UnknownAlgorithm(Preview.of(name))))
        .left
        .map(hashError)
    )

  def digest(a: Algorithm, data: Array[Byte]): Array[Byte] = hasher(a).digest(ByteVector(data)).bytes.toArray

  /** `bytes`, if they are a digest of `a`'s length. */
  def fromBytes(a: Algorithm, bytes: Array[Byte]): Result[Array[Byte], JHashError] = fromBytesOf(hasher(a), bytes)

  private def fromBytesOf[H](h: Hasher[H], bytes: Array[Byte]): Result[Array[Byte], JHashError] =
    Values.result(Digest.fromBytes[H](ByteVector(bytes))(using h).bimap(hashError, _.bytes.toArray))

  def fromHex(a: Algorithm, text: String): Result[Array[Byte], JHashError] = fromHexOf(hasher(a), text)

  private def fromHexOf[H](h: Hasher[H], text: String): Result[Array[Byte], JHashError] =
    Values.result(Digest.fromHex[H](text)(using h).bimap(hashError, _.bytes.toArray))

  /** `tag`, if it is a domain tag. */
  def domainTag(tag: String): Result[String, JHashError] = Values.result(DomainTag(tag).bimap(hashError, _.value))

  private def tagged[H](h: Hasher[H], tag: String): Either[HashError, TaggedHash[H]] =
    DomainTag(tag).map(TaggedHash[H](_)(using h))

  def taggedHash(a: Algorithm, tag: String, message: Array[Byte]): Result[Array[Byte], JHashError] = taggedHashOf(hasher(a), tag, message)

  private def taggedHashOf[H](h: Hasher[H], tag: String, message: Array[Byte]): Result[Array[Byte], JHashError] =
    Values.result(tagged(h, tag).bimap(hashError, _.apply(ByteVector(message)).bytes.toArray))

  /** The tagged hash of `v`'s canonical encoding, streamed: `TooLarge` past `maxBytes`, before anything is hashed. */
  def taggedValue(a: Algorithm, tag: String, v: CanonicalValue, maxBytes: Long): Result[Array[Byte], EncodeError] =
    taggedValueOf(hasher(a), tag, v, maxBytes)

  private def taggedValueOf[H](h: Hasher[H], tag: String, v: CanonicalValue, maxBytes: Long): Result[Array[Byte], EncodeError] =
    Values.result(
      tagged(h, tag).left
        .map(e => new TypeMismatch(s"expected a domain tag: ${e.kind}"): EncodeError)
        .flatMap(_.valueWithin(v, maxBytes).bimap(e => new TooLarge(e.limit), _.bytes.toArray))
    )

  def expansionLimit(a: Algorithm): Long = limitOf(hasher(a))

  private def limitOf[H](h: Hasher[H]): Long = ExpandMessage.limit[H](using h)

  /** A length past the `Int` the reference takes is clamped to it: outside `1..limit` either way, so refused alike. */
  def expand(a: Algorithm, message: Array[Byte], dst: Array[Byte], length: Long): Result[Array[Byte], JHashError] =
    expandOf(hasher(a), message, dst, length.max(Int.MinValue.toLong).min(Int.MaxValue.toLong).toInt)

  private def expandOf[H](h: Hasher[H], message: Array[Byte], dst: Array[Byte], length: Int): Result[Array[Byte], JHashError] =
    Values.result(ExpandMessage.xmd[H](ByteVector(message), ByteVector(dst), length)(using h).bimap(hashError, _.toArray))

  // ---- value commitments (docs/specs/merkle.md section 2)

  val BytesTag: Byte = ValueCommitment.BytesTag
  val CborTag: Byte = ValueCommitment.CborTag

  def commitment(v: CanonicalValue, maxBytes: Long): Result[Array[Byte], EncodeError] =
    Values.result(ValueCommitment.bytesWithin(v, maxBytes).bimap(e => new TooLarge(e.limit), _.toArray))

  val MaxEncodedBytes: Long = Cbor.MaxEncodedBytes
}
