package xyz.kd5ujc.signing

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import cafe.cryptography.curve25519.{CompressedEdwardsY, Constants, Scalar}

/**
 * Extended Ed25519 signature scheme (BIP32-Ed25519 / Khovratovich et al.).
 *
 * Provides hierarchical deterministic key derivation for Ed25519 keys.
 *
 * The secret key consists of 96 bytes:
 *   - leftKey (32 bytes): clamped scalar for signing
 *   - rightKey (32 bytes): nonce prefix for deterministic nonce generation
 *   - chainCode (32 bytes): for BIP32 child key derivation
 *
 * Signing delegates to [[Ed25519Signing]] — same core as standard Ed25519.
 *
 * Dependencies: curve25519-elisabeth + JCE HMAC-SHA-512 (no BouncyCastle).
 */
class ExtendedEd25519 extends SignatureScheme[ExtendedEd25519.SecretKey, ExtendedEd25519.PublicKey] {

  import ExtendedEd25519._

  override val seedLength: Int = SeedLength

  override def deriveSecretKeyFromSeed(seed: Array[Byte]): SecretKey = {
    require(
      seed.length >= SeedLength,
      s"Invalid seed length. Expected >= $SeedLength, got: ${seed.length}"
    )

    // Make a copy to avoid mutating the input
    val clamped = seed.slice(0, SeedLength).clone()

    // Clamp bits per BIP32-Ed25519 / CIP-0003 / SLIP-0023
    clamped(0) = (clamped(0) & 0xf8).toByte
    clamped(31) = (clamped(31) & 0x1f).toByte
    clamped(31) = (clamped(31) | 0x40).toByte

    SecretKey(
      clamped.slice(0, 32),
      clamped.slice(32, 64),
      clamped.slice(64, 96)
    )
  }

  override def sign(privateKey: SecretKey, message: Array[Byte]): Array[Byte] = {
    val pk = Ed25519Signing.publicKeyFromScalar(privateKey.leftKey)
    Ed25519Signing.sign(privateKey.leftKey, privateKey.rightKey, pk, message)
  }

  override def verify(
    signature: Array[Byte],
    message:   Array[Byte],
    publicKey: PublicKey
  ): Boolean =
    Ed25519Signing.verify(signature, message, publicKey.vk.bytes)

  override def getVerificationKey(privateKey: SecretKey): PublicKey = {
    val pkBytes = Ed25519Signing.publicKeyFromScalar(privateKey.leftKey)
    PublicKey(Ed25519.PublicKey(pkBytes), privateKey.chainCode.clone())
  }

  /**
   * Derive a child secret key using BIP32-Ed25519 derivation.
   * Works with both soft and hardened indices.
   */
  def deriveChildSecretKey(sk: SecretKey, index: Bip32Index): SecretKey = {
    val lNum: BigInt = BigInt(1, sk.leftKey.reverse) // little-endian → BigInt
    val rNum: BigInt = BigInt(1, sk.rightKey.reverse)
    val pk = getVerificationKey(sk)

    // z = HMAC-SHA-512(chainCode, data)
    val zData = if (index.isHardened) {
      Array(0x00.toByte) ++ sk.leftKey ++ sk.rightKey ++ index.bytes
    } else {
      Array(0x02.toByte) ++ pk.vk.bytes ++ index.bytes
    }
    val z = hmacSha512(sk.chainCode, zData)

    val zLeft = BigInt(1, z.slice(0, 28).reverse) // only first 28 bytes
    val zRight = BigInt(1, z.slice(32, 64).reverse)

    val nextLeft = sec256LE(zLeft * 8 + lNum)
    val nextRight = sec256LE((zRight + rNum) % BigInt(2).pow(256))

    // Chain code derivation
    val ccData = if (index.isHardened) {
      Array(0x01.toByte) ++ sk.leftKey ++ sk.rightKey ++ index.bytes
    } else {
      Array(0x03.toByte) ++ pk.vk.bytes ++ index.bytes
    }
    val nextChainCode = hmacSha512(sk.chainCode, ccData).slice(32, 64)

    SecretKey(nextLeft, nextRight, nextChainCode)
  }

  /**
   * Derive a child public key using BIP32-Ed25519 derivation.
   * Only works with soft indices (hardened requires the secret key).
   */
  def deriveChildVerificationKey(pk: PublicKey, index: Bip32Index.SoftIndex): PublicKey = {
    val z = hmacSha512(pk.chainCode, Array(0x02.toByte) ++ pk.vk.bytes ++ index.bytes)
    val zL = z.slice(0, 28)

    val zLMult8 = sec256LE(BigInt(1, zL.reverse) * 8)

    // EC point operations using curve25519-elisabeth
    // legacy, deleted with the signing module
    val scaledZL = Constants.ED25519_BASEPOINT_TABLE.multiply(Scalar.fromBits(zLMult8)) // scalafix:ok DisableSyntax.noScalarFromBits
    val existingPoint = new CompressedEdwardsY(pk.vk.bytes).decompress()
    val newPoint = scaledZL.add(existingPoint)
    val newPkBytes = newPoint.compress().toByteArray

    val nextCC = hmacSha512(pk.chainCode, Array(0x03.toByte) ++ pk.vk.bytes ++ index.bytes).slice(32, 64)

    PublicKey(Ed25519.PublicKey(newPkBytes), nextCC)
  }

  private def hmacSha512(key: Array[Byte], data: Array[Byte]): Array[Byte] = {
    val mac = Mac.getInstance("HmacSHA512")
    mac.init(new SecretKeySpec(key, "HmacSHA512"))
    mac.doFinal(data)
  }

  /**
   * Serialize BigInt to 32-byte little-endian array.
   */
  private def sec256LE(p: BigInt): Array[Byte] = {
    val bytes = p.toByteArray // big-endian, may have leading zero byte
    val reversed = bytes.reverse // now little-endian
    reversed.padTo(32, 0.toByte).take(32)
  }
}

object ExtendedEd25519 {
  val SignatureLength: Int = 64
  val KeyLength: Int = 32
  val PublicKeyLength: Int = 32
  val ChainCodeLength: Int = 32
  val SeedLength: Int = 96

  case class SecretKey(
    leftKey:   Array[Byte],
    rightKey:  Array[Byte],
    chainCode: Array[Byte]
  ) extends SigningKey {
    require(leftKey.length == KeyLength, s"Invalid leftKey length. Expected: $KeyLength, got: ${leftKey.length}")
    require(rightKey.length == KeyLength, s"Invalid rightKey length. Expected: $KeyLength, got: ${rightKey.length}")
    require(
      chainCode.length == ChainCodeLength,
      s"Invalid chainCode length. Expected: $ChainCodeLength, got: ${chainCode.length}"
    )

    override def bytes: Array[Byte] = leftKey ++ rightKey ++ chainCode

    override def equals(obj: Any): Boolean = obj match {
      case that: SecretKey =>
        java.util.Arrays.equals(this.leftKey, that.leftKey) &&
        java.util.Arrays.equals(this.rightKey, that.rightKey) &&
        java.util.Arrays.equals(this.chainCode, that.chainCode)
      case _ => false
    }

    override def hashCode(): Int =
      java.util.Arrays.hashCode(leftKey) +
      31 * java.util.Arrays.hashCode(rightKey) +
      31 * 31 * java.util.Arrays.hashCode(chainCode)
  }

  case class PublicKey(vk: Ed25519.PublicKey, chainCode: Array[Byte]) extends VerificationKey {
    require(
      chainCode.length == ChainCodeLength,
      s"Invalid chainCode length. Expected: $ChainCodeLength, got: ${chainCode.length}"
    )

    override def bytes: Array[Byte] = vk.bytes ++ chainCode

    override def equals(obj: Any): Boolean = obj match {
      case that: PublicKey =>
        java.util.Arrays.equals(this.vk.bytes, that.vk.bytes) &&
        java.util.Arrays.equals(this.chainCode, that.chainCode)
      case _ => false
    }

    override def hashCode(): Int =
      java.util.Arrays.hashCode(vk.bytes) + 31 * java.util.Arrays.hashCode(chainCode)
  }
}
