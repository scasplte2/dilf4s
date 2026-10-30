package xyz.kd5ujc.core.hash

import java.nio.charset.StandardCharsets.US_ASCII

import scodec.bits.ByteVector

/** `expand_message_xmd` (RFC 9380 §5.3.1) over a Merkle–Damgård hash `H`, for hash-to-field and hash-to-curve. */
object ExpandMessage {

  private val oversizePrefix = ByteVector("H2C-OVERSIZE-DST-".getBytes(US_ASCII))

  /**
   * `lenInBytes` uniform bytes from `msg` under domain separation tag `dst`. A DST over 255 bytes is first reduced to
   * `H("H2C-OVERSIZE-DST-" || DST)` (§5.3.3). Lengths beyond `255 * outputSize` or 65535, and an empty DST, are errors.
   */
  def xmd[H](msg: ByteVector, dst: ByteVector, lenInBytes: Int)(implicit H: Hasher[H]): Either[HashError, ByteVector] = {
    val bIn = H.outputSize.toInt
    val ell = (lenInBytes + bIn - 1) / bIn
    val dstEff = if (dst.size > 255L) H.digest(oversizePrefix ++ dst).bytes else dst
    for {
      _ <- Either.cond(dst.nonEmpty, (), HashError.InvalidExpansion("the domain separation tag must not be empty"))
      _ <- Either.cond(
        lenInBytes > 0 && ell <= 255 && lenInBytes <= 65535,
        (),
        HashError.InvalidExpansion(s"cannot expand to $lenInBytes bytes with ${H.name}")
      )
    } yield {
      val dstPrime = dstEff ++ ByteVector(dstEff.size.toByte)
      val msgPrime = ByteVector.low(H.blockSize) ++ msg ++ ByteVector.fromShort(lenInBytes.toShort) ++ ByteVector(0) ++ dstPrime
      val b0 = H.digest(msgPrime).bytes
      val b1 = H.digest(b0 ++ ByteVector(1) ++ dstPrime).bytes
      (2 to ell)
        .foldLeft((b1, b1)) {
          case ((prev, acc), i) =>
            val bi = H.digest(b0.xor(prev) ++ ByteVector(i.toByte) ++ dstPrime).bytes
            (bi, acc ++ bi)
        }
        ._2
        .take(lenInBytes.toLong)
    }
  }
}
