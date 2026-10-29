package xyz.kd5ujc.signing

import java.nio.{ByteBuffer, ByteOrder}

/**
 * BIP32 hierarchical deterministic key index.
 *
 * BIP32 indices are 4-byte little-endian uint32.
 * - Soft index: bit 31 = 0 (range 0..2^31-1)
 * - Hardened index: bit 31 = 1 (range 2^31..2^32-1)
 */
sealed trait Bip32Index {
  def value: Long
  def isHardened: Boolean

  def bytes: Array[Byte] = {
    val bb = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(value.toInt)
    bb.array()
  }
}

object Bip32Index {

  case class SoftIndex(value: Long) extends Bip32Index {
    require(value >= 0 && value < (1L << 31), s"Soft index out of range: $value")
    override def isHardened: Boolean = false
  }

  case class HardenedIndex private (value: Long) extends Bip32Index {
    override def isHardened: Boolean = true
  }

  object HardenedIndex {

    def apply(index: Long): HardenedIndex = {
      require(index >= 0 && index < (1L << 31), s"Hardened index out of range: $index")
      new HardenedIndex(index + (1L << 31))
    }
  }

  def soft(index:     Long): SoftIndex = SoftIndex(index)
  def hardened(index: Long): HardenedIndex = HardenedIndex(index)
}
