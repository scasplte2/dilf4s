package xyz.kd5ujc.signature.extended

import java.nio.{ByteBuffer, ByteOrder}

import cats.data.Validated
import cats.syntax.bifunctor._

import io.circe._

sealed trait Bip32Index {
  val value: Long

  val bytes: Array[Byte] =
    ByteBuffer
      .allocate(java.lang.Long.SIZE)
      .order(ByteOrder.LITTLE_ENDIAN)
      .putLong(value)
      .array()
      .take(4)

  def isHardened: Boolean = value >= Bip32Index.hardenedOffset
  def isPublic: Boolean = !isHardened

  override def toString: String =
    if (isHardened) s"${value - Bip32Index.hardenedOffset}h"
    else value.toString
}

sealed trait InvalidBip32Index
case object IndexOutOfRange extends InvalidBip32Index
case object NegativeIndex extends InvalidBip32Index
case object InvalidFormat extends InvalidBip32Index

object Bip32Index {
  val hardenedOffset: Long = 1L << 31
  val maxIndex: Long = (1L << 32) - 1

  def apply(value: Long): Validated[InvalidBip32Index, Bip32Index] =
    if (value < 0) Validated.invalid(NegativeIndex)
    else if (value > maxIndex) Validated.invalid(IndexOutOfRange)
    else if (value < hardenedOffset) Validated.valid(Bip32Indexes.PublicIndex.unsafe(value))
    else Validated.valid(Bip32Indexes.HardenedIndex.unsafe(value))

  def fromString(s: String): Validated[InvalidBip32Index, Bip32Index] =
    try
      if (s.endsWith("h") || s.endsWith("'")) {
        val num = s.dropRight(1).toLong
        if (num < 0) Validated.invalid(NegativeIndex)
        else Bip32Indexes.HardenedIndex.validated(num)
      } else {
        val num = s.toLong
        Bip32Index(num)
      }
    catch {
      case _: NumberFormatException => Validated.invalid(InvalidFormat)
    }
}

object Bip32Indexes {
  final case class PublicIndex private (override val value: Long) extends Bip32Index
  final case class HardenedIndex private (override val value: Long) extends Bip32Index

  object PublicIndex {
    def apply(value: Long): Validated[InvalidBip32Index, PublicIndex] =
      if (value < 0) Validated.invalid(NegativeIndex)
      else if (value >= Bip32Index.hardenedOffset) Validated.invalid(IndexOutOfRange)
      else Validated.valid(unsafe(value))

    def unsafe(value: Long): PublicIndex = new PublicIndex(value)

    def validated(value: Long): Validated[InvalidBip32Index, PublicIndex] = apply(value)
  }

  object HardenedIndex {
    def apply(value: Long): Validated[InvalidBip32Index, HardenedIndex] =
      if (value < 0) Validated.invalid(NegativeIndex)
      else validated(value)

    def unsafe(value: Long): HardenedIndex = new HardenedIndex(value)

    def validated(value: Long): Validated[InvalidBip32Index, HardenedIndex] = {
      val hardenedValue = value + Bip32Index.hardenedOffset
      if (hardenedValue > Bip32Index.maxIndex) Validated.invalid(IndexOutOfRange)
      else Validated.valid(unsafe(hardenedValue))
    }
  }

  implicit class Bip32IndexOps(value: Long) {
    def public: Validated[InvalidBip32Index, PublicIndex] = PublicIndex(value)
    def hardened: Validated[InvalidBip32Index, HardenedIndex] = HardenedIndex(value)
  }

  implicit val bip32IndexEncoder: Encoder[Bip32Index] =
    (a: Bip32Index) => Json.fromString(a.toString)

  implicit val bip32IndexDecoder: Decoder[Bip32Index] =
    (c: HCursor) =>
      c.as[String].flatMap { str =>
        Bip32Index.fromString(str).toEither.leftMap(err => DecodingFailure(err.toString, c.history))
      }
}
