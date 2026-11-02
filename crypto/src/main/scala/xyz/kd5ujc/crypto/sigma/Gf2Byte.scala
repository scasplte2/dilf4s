package xyz.kd5ujc.crypto.sigma

/**
 * An element of GF(2^192) of degree below 8, i.e. one byte `0..255`: the points at which threshold children's
 * challenges are evaluated. Distinct from [[Gf2_192]] so that `Gf2_192 * Gf2Byte`, which takes 8 shift-and-add steps
 * instead of 192, can only be given an operand that fits in a byte. Sums of bytes stay bytes.
 */
opaque type Gf2Byte = Int

object Gf2Byte {

  /** The low 8 bits of `b`; a threshold child's point is its 1-based index, at most `SigmaProp.MaxChildren`. */
  def apply(b: Int): Gf2Byte = b & 0xff

  extension (b: Gf2Byte) {
    def value: Int = b
    def +(c: Gf2Byte): Gf2Byte = b ^ c
  }
}
