package xyz.kd5ujc.accumulators.mpt

import cats.data.Validated
import cats.syntax.option._
import cats.implicits.toBifunctorOps
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, KeyDecoder, KeyEncoder}
import org.bouncycastle.util.encoders.Hex
import xyz.kd5ujc.hash.Digest

import scala.collection.immutable.ArraySeq

class Nibble private (val value: Byte) extends AnyVal {
  override def toString: String = Hex.toHexString(Array(value))
}

object Nibble {
  def apply(byte: Byte): Seq[Nibble] =
    Seq(
      Nibble.unsafe((byte >> 4 & 0x0F).toByte),
      Nibble.unsafe((byte & 0x0F).toByte)
    )

  def apply(bytes: Array[Byte]): Seq[Nibble] =
    ArraySeq.unsafeWrapArray(bytes).flatMap(apply)

  def apply[L](digest: Digest[L]): Seq[Nibble] =
    apply(digest.value)

  def toBytes(nibbles: Seq[Nibble]): Array[Byte] =
    nibbles.grouped(2).collect {
      case Seq(high, low) =>
        ((high.value << 4) | (low.value & 0x0F)).toByte
    }.toArray

  def validated(byte: Byte): Validated[InvalidNibble, Nibble] =
    if (byte >= 0 && byte <= 15) Validated.valid(new Nibble(byte))
    else Validated.invalid(NibbleOutOfRange)

  def unsafe(byte: Byte): Nibble = new Nibble(byte)

  def empty: Nibble = unsafe(0.toByte)

  def commonPrefix(a: Seq[Nibble], b: Seq[Nibble]): Seq[Nibble] =
    a.zip(b)
      .takeWhile(Function.tupled(_.value == _.value))
      .map(_._1)

  implicit val nibbleEncoder: Encoder[Nibble] = (a: Nibble) => Hex.toHexString(Array(a.value)).asJson

  implicit val nibbleDecoder: Decoder[Nibble] = (c: HCursor) =>
    for {
      value  <- c.as[String]
      result <- Nibble.validated(Hex.decode(value).head).toEither.leftMap(err => DecodingFailure(err.toString, c.history))
    } yield result

  implicit val nibbleKeyEncoder: KeyEncoder[Nibble] = (key: Nibble) => Hex.toHexString(Array(key.value))

  implicit val nibbleKeyDecoder: KeyDecoder[Nibble] = (key: String) => Nibble.validated(Hex.decode(key).head).toOption
}

sealed trait InvalidNibble
case object NibbleOutOfRange extends InvalidNibble
