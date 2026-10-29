package xyz.kd5ujc.kes

import java.security.MessageDigest

import xyz.kd5ujc.kes.KesBinaryTree._
import xyz.kd5ujc.signing.Ed25519Signing

import org.bouncycastle.crypto.digests.Blake2bDigest

/**
 * Base trait for KES implementations using Ed25519 signatures and Blake2b-256 hashing.
 *
 * This provides:
 * - Blake2b-256 hash function
 * - Ed25519 key generation, signing, and verification
 * - PRNG for seed doubling
 * - Tree height and witness computation
 */
trait KesEd25519Blake2b256 {

  protected val pkBytes: Int = 32
  protected val sigBytes: Int = 64
  protected val hashBytes: Int = 32

  /**
   * Blake2b-256 hash function.
   */
  protected def hash(input: Array[Byte]): Array[Byte] = {
    val digest = new Blake2bDigest(256)
    digest.update(input, 0, input.length)
    val out = new Array[Byte](32)
    digest.doFinal(out, 0)
    out
  }

  /**
   * Exponent base two.
   */
  protected def exp(n: Int): Int =
    1 << n

  /**
   * PRNG for seed doubling.
   * Uses Blake2b-256 with domain separation prefixes.
   */
  protected def prng(seed: Array[Byte]): (Array[Byte], Array[Byte]) = {
    val r1 = hash(0x00.toByte +: seed)
    val r2 = hash(0x01.toByte +: seed)
    (r1, r2)
  }

  /**
   * Generate Ed25519 keypair from seed.
   *
   * @param seed 32-byte seed
   * @return (secret key, public key) where secret key IS the seed
   */
  protected def sGenKeypair(seed: Array[Byte]): (Array[Byte], Array[Byte]) = {
    val sk = seed.clone()
    val h = sha512(sk)
    // Clamp
    h(0) = (h(0) & 0xf8).toByte
    h(31) = ((h(31) & 0x7f) | 0x40).toByte
    val pk = Ed25519Signing.publicKeyFromScalar(h.take(32))
    (sk, pk)
  }

  /**
   * Sign with Ed25519 using 32-byte seed.
   */
  protected def sSign(m: Array[Byte], sk: Array[Byte]): Array[Byte] = {
    val h = sha512(sk)
    // Clamp
    h(0) = (h(0) & 0xf8).toByte
    h(31) = ((h(31) & 0x7f) | 0x40).toByte
    val scalar = h.take(32)
    val noncePrefix = h.slice(32, 64)
    val pk = Ed25519Signing.publicKeyFromScalar(scalar)
    Ed25519Signing.sign(scalar, noncePrefix, pk, m)
  }

  /**
   * Verify Ed25519 signature.
   */
  protected def sVerify(m: Array[Byte], signature: Array[Byte], pk: Array[Byte]): Boolean =
    Ed25519Signing.verify(signature, m, pk)

  /**
   * Get the height of a binary tree.
   */
  def getTreeHeight(tree: KesBinaryTree): Int = {
    def loop(t: KesBinaryTree): Int = t match {
      case n: MerkleNode  => Seq(loop(n.left), loop(n.right)).max + 1
      case _: SigningLeaf => 1
      case Empty()        => 0
    }
    loop(tree) - 1
  }

  /**
   * Compute the witness (hash) of a tree node.
   */
  def witness(tree: KesBinaryTree): Array[Byte] = tree match {
    case MerkleNode(_, witnessLeft, witnessRight, _, _) => hash(witnessLeft ++ witnessRight)
    case SigningLeaf(_, vk)                             => hash(vk)
    case Empty()                                        => Array.fill(hashBytes)(0: Byte)
  }

  /**
   * SHA-512 helper for Ed25519 key expansion.
   */
  private def sha512(input: Array[Byte]): Array[Byte] = {
    val md = MessageDigest.getInstance("SHA-512")
    md.update(input)
    md.digest()
  }
}
