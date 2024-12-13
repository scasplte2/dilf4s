package xyz.kd5ujc.hash

import cats.data.Validated
import cats.implicits.toBifunctorOps

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, KeyDecoder, KeyEncoder}
import org.bouncycastle.util.encoders.Hex

sealed abstract class Digest {
  val value: Array[Byte]
  val size: Int

  override def toString: String = Hex.toHexString(value)

  override def equals(obj: Any): Boolean = obj match {
    case digest: Digest => this.size == digest.size && this.value.sameElements(digest.value)
    case _              => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(value)
}

object Digest {

  implicit val digestEncoder: Encoder[Digest] = Encoder.instance {
    case l256Inst: l256 => l256Inst.asJson
    case l512Inst: l512 => l512Inst.asJson
  }

  implicit val digestDecoder: Decoder[Digest] = Decoder.instance { (c: HCursor) =>
    c.as[String].map(Hex.decode).map(_.length).flatMap {
      case 32    => c.as[l256]
      case 64    => c.as[l512]
      case l @ _ => Left(DecodingFailure(s"Unsupported digest length: $l", c.history))
    }
  }

  implicit val digestKeyEncoder: KeyEncoder[Digest] = {
    case l256Inst: l256 => l256Inst.toString
    case l512Inst: l512 => l512Inst.toString
  }

  implicit val digestKeyDecoder: KeyDecoder[Digest] = (digest: String) => {
    val bytes = Hex.decode(digest)

    bytes.length match {
      case 32 => Some(l256.unsafe(bytes))
      case 64 => Some(l512.unsafe(bytes))
      case _  => None
    }
  }
}

class l256 private (bytes: Array[Byte]) extends Digest {
  override val value: Array[Byte] = bytes
  override val size: Int = l256.size
}

object l256 {
  private val size: Int = 32

  def unsafe(bytes: Array[Byte]): l256 = new l256(bytes)

  def empty: l256 = unsafe(Array.fill(size)(0: Byte))

  def from(bytes: Array[Byte]): Validated[InvalidDigest, l256] =
    Validated.cond(bytes.length == size, new l256(bytes), IncorrectSize)

  implicit val l256Ordering: Ordering[l256] = (x: l256, y: l256) => java.util.Arrays.compare(x.value, y.value)

  implicit val l256Encoder: Encoder[l256] = (a: l256) => Hex.toHexString(a.value).asJson

  implicit val l256Decoder: Decoder[l256] = (c: HCursor) =>
    for {
      value  <- c.as[String]
      result <- l256.from(Hex.decode(value)).toEither.leftMap(err => DecodingFailure(err.toString, c.history))
    } yield result
}

class l512 private (bytes: Array[Byte]) extends Digest {
  override val value: Array[Byte] = bytes
  override val size: Int = l512.size
}

object l512 {
  private val size: Int = 64

  def unsafe(bytes: Array[Byte]): l512 = new l512(bytes)

  def empty: l512 = unsafe(Array.fill(size)(0: Byte))

  def from(bytes: Array[Byte]): Validated[InvalidDigest, l512] =
    Validated.cond(bytes.length == size, new l512(bytes), IncorrectSize)

  implicit val l512Ordering: Ordering[l512] = (x: l512, y: l512) => java.util.Arrays.compare(x.value, y.value)

  implicit val l512Encoder: Encoder[l512] = (a: l512) => Hex.toHexString(a.value).asJson

  implicit val l512Decoder: Decoder[l512] = (c: HCursor) =>
    for {
      value  <- c.as[String]
      result <- l512.from(Hex.decode(value)).toEither.leftMap(err => DecodingFailure(err.toString, c.history))
    } yield result
}

sealed trait InvalidDigest
case object IncorrectSize extends InvalidDigest
