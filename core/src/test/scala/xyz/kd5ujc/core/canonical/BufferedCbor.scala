package xyz.kd5ujc.core.canonical

import java.nio.charset.StandardCharsets.UTF_8

import cats.Eval
import cats.syntax.traverse._

import xyz.kd5ujc.core.canonical.CanonicalValue._

import scodec.bits.ByteVector

/**
 * The encoder as it was before encodings were streamed, verbatim: it builds every encoding whole. Kept in tests only,
 * as the oracle the streaming encoder and every streamed hash are checked against.
 */
object BufferedCbor {

  private val TwoTo64: BigInt = BigInt(1) << 64
  private val TagPositiveBignum: Int = 2
  private val TagNegativeBignum: Int = 3
  private val TagRational: Int = 30

  def encode(value: CanonicalValue): ByteVector = encodeEval(value).value

  private def encodeEval(value: CanonicalValue): Eval[ByteVector] =
    value match {
      case CNull        => Eval.now(ByteVector(0xf6))
      case CBool(false) => Eval.now(ByteVector(0xf4))
      case CBool(true)  => Eval.now(ByteVector(0xf5))
      case CInt(n)      => Eval.now(integer(n))
      case r: CRational =>
        Eval.now(head(6, TagRational.toLong) ++ head(4, 2L) ++ integer(r.value.numerator) ++ integer(r.value.denominator))
      case t: CText   => Eval.now(text(t.value))
      case CBytes(b)  => Eval.now(head(2, b.size) ++ b)
      case CArray(vs) => Eval.defer(vs.traverse(encodeEval)).map(parts => parts.foldLeft(head(4, vs.size.toLong))(_ ++ _))
      case m: CMap =>
        Eval
          .defer(m.entries.toVector.traverse { case (k, v) => encodeEval(v).map(ev => (text(k), ev)) })
          .map { entries =>
            entries.sortWith { case ((ka, _), (kb, _)) => Cbor.compareBytes(ka, kb) < 0 }
              .foldLeft(head(5, entries.size.toLong)) { case (acc, (k, v)) => acc ++ k ++ v }
          }
    }

  private def text(s: String): ByteVector = {
    val utf8 = ByteVector(s.getBytes(UTF_8))
    head(3, utf8.size) ++ utf8
  }

  private def integer(n: BigInt): ByteVector =
    if (n.signum >= 0) { if (n < TwoTo64) head(0, n) else bignum(TagPositiveBignum, n) }
    else {
      val m = -1 - n
      if (m < TwoTo64) head(1, m) else bignum(TagNegativeBignum, m)
    }

  private def bignum(tag: Int, magnitude: BigInt): ByteVector = {
    val bytes = ByteVector(magnitude.toByteArray).dropWhile(_ == 0)
    head(6, tag.toLong) ++ head(2, bytes.size) ++ bytes
  }

  private def head(major: Int, argument: Long): ByteVector = head(major, BigInt(argument))

  private def head(major: Int, argument: BigInt): ByteVector = {
    val mt = major << 5
    def fixed(len: Int): ByteVector = ByteVector(argument.toByteArray).takeRight(len.toLong).padLeft(len.toLong)
    if (argument < 24) ByteVector(mt | argument.toInt)
    else if (argument < 0x100) ByteVector(mt | 24, argument.toInt)
    else if (argument < 0x10000) ByteVector(mt | 25) ++ fixed(2)
    else if (argument < (BigInt(1) << 32)) ByteVector(mt | 26) ++ fixed(4)
    else ByteVector(mt | 27) ++ fixed(8)
  }

  /** The commitment `0x00 ‖ b` or `0x01 ‖ cbor(v)` (merkle.md §2), built whole. */
  def commitment(v: CanonicalValue): ByteVector =
    v match {
      case CBytes(b) => 0x00.toByte +: b
      case other     => 0x01.toByte +: encode(other)
    }
}
