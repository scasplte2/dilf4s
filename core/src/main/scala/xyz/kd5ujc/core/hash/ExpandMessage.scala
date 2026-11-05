package xyz.kd5ujc.core.hash

import java.nio.charset.StandardCharsets.US_ASCII

import scodec.bits.ByteVector

/** `expand_message_xmd` (RFC 9380 §5.3.1) over a Merkle–Damgård hash `H`, for hash-to-field and hash-to-curve. */
object ExpandMessage {

  private val oversizePrefix = ByteVector("H2C-OVERSIZE-DST-".getBytes(US_ASCII))

  /** The most bytes `H` expands to: `min(255 * outputSize, 65535)`. */
  def limit[H](implicit H: Hasher[H]): Long = math.min(255L * H.outputSize, 65535L)

  /**
   * `lenInBytes` uniform bytes from `msg` under domain separation tag `dst`. A DST over 255 bytes is first reduced to
   * `H("H2C-OVERSIZE-DST-" || DST)` (§5.3.3). It is refused (`InvalidExpansion`), in this order, for an empty DST
   * (`emptyDst`) and for a length outside `1..`[[limit]] (`outputLength`), before anything is hashed.
   */
  def xmd[H](msg: ByteVector, dst: ByteVector, lenInBytes: Int)(implicit H: Hasher[H]): Either[HashError, ByteVector] = {
    def refused(r: ExpansionRefusal): HashError = HashError.InvalidExpansion(r, H.name, limit[H])
    for {
      _ <- Either.cond(dst.nonEmpty, (), refused("emptyDst"))
      _ <- Either.cond(lenInBytes > 0 && lenInBytes <= limit[H], (), refused("outputLength"))
    } yield {
      val bIn = H.outputSize.toInt
      val ell = (lenInBytes + bIn - 1) / bIn
      val dstEff = if (dst.size > 255L) H.digest(oversizePrefix ++ dst).bytes else dst
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
