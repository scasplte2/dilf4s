package xyz.kd5ujc.crypto.keys

import org.bouncycastle.crypto.digests.SHA512Digest
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.params.KeyParameter
import scodec.bits.ByteVector

/** HMAC-SHA512 and single-block PBKDF2 over BouncyCastle; each call owns its MAC state and buffers. */
private[crypto] object Hmac {

  def sha512(key: ByteVector, data: ByteVector): ByteVector = {
    val mac = new HMac(new SHA512Digest())
    val out = new Array[Byte](mac.getMacSize)
    mac.init(new KeyParameter(key.toArray))
    mac.update(data.toArray, 0, data.size.toInt)
    mac.doFinal(out, 0)
    ByteVector.view(out)
  }

  /** PBKDF2-HMAC-SHA512 producing exactly one 64-byte block: T = U1 xor ... xor Uc, U1 = HMAC(P, S || INT(1)). */
  def pbkdf2Sha512Block(password: ByteVector, salt: ByteVector, iterations: Int): ByteVector = block(password, salt, 1, iterations)

  /** PBKDF2-HMAC-SHA512 of any length: the concatenated blocks, truncated. */
  def pbkdf2Sha512(password: ByteVector, salt: ByteVector, iterations: Int, length: Int): ByteVector =
    (1 to (length + 63) / 64)
      .foldLeft(ByteVector.empty)((acc, i) => acc ++ block(password, salt, i, iterations))
      .take(length.toLong)

  /**
   * Block `index`. `xor` is a lazy view of its operands, so each round's sum is copied flat: kept as views, reading
   * the block would recurse once per round, past a normal stack at BIP-39's 2048 (`compact` keeps a view as it is).
   */
  private def block(password: ByteVector, salt: ByteVector, index: Int, iterations: Int): ByteVector = {
    val first = sha512(password, salt ++ ByteVector.fromInt(index))
    (2 to iterations)
      .foldLeft((first, first)) {
        case ((u, t), _) =>
          val next = sha512(password, u)
          (next, t.xor(next).copy)
      }
      ._2
  }
}
