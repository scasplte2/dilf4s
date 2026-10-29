package xyz.kd5ujc.signing

import java.security.{MessageDigest, SecureRandom}

import cafe.cryptography.curve25519.{CompressedRistretto, Constants, Scalar}

/**
 * FROST(ristretto255, SHA-512) single-signer signature scheme.
 *
 * Implements the single-signer variant from FROST draft-irtf-cfrg-frost-15 Appendix C,
 * using the ristretto255 ciphersuite. This is compatible with FROST aggregate signatures
 * for threshold signing.
 *
 * Scheme:
 *   - Secret key:  32-byte canonical Scalar (reduced mod L from SHA-512(seed))
 *   - Public key:  32-byte CompressedRistretto point PK = [sk]G
 *   - H2(m) = SHA-512("FROST-RISTRETTO255-SHA512-v1chal" || m) mod L  (challenge)
 *   - H3(m) = SHA-512("FROST-RISTRETTO255-SHA512-v1nonce" || m) mod L (nonce gen)
 *   - Sign(sk, msg):
 *       PK = [sk]G
 *       k = H3(random_bytes(32) || sk)     // hedged nonce
 *       R = [k]G
 *       c = H2(R_compressed || PK_compressed || msg)
 *       z = k + sk * c mod L
 *       signature = R_compressed || z_bytes   (64 bytes)
 *   - Verify(sig, msg, pk):
 *       R = decompress(sig[0:32])
 *       z = deserialize_scalar(sig[32:64])
 *       c = H2(R_compressed || PK_compressed || msg)
 *       check: [z]G == R + [c]PK
 *
 * Key differences from Ed25519/standard Schnorr:
 *   - Domain-separated hashes with FROST context strings
 *   - Randomized (hedged) nonces — signatures are NOT deterministic
 *   - No nonce prefix stored with secret key
 *   - Compatible with FROST threshold aggregate signatures
 *
 * Dependencies: cafe.cryptography/curve25519-elisabeth only.
 */
class Ristretto255 extends SignatureScheme[Ristretto255.SecretKey, Ristretto255.PublicKey] {

  override val seedLength: Int = Ristretto255.SeedLength

  override def deriveSecretKeyFromSeed(seed: Array[Byte]): Ristretto255.SecretKey = {
    require(
      seed.length >= Ristretto255.SeedLength,
      s"Invalid seed length. Expected >= ${Ristretto255.SeedLength}, got: ${seed.length}"
    )
    // FROST key derivation: reduce SHA-512(seed) mod L
    val h = sha512(seed.slice(0, Ristretto255.SeedLength))
    val scalar = Scalar.fromBytesModOrderWide(h)
    Ristretto255.SecretKey(scalar.toByteArray)
  }

  override def sign(privateKey: Ristretto255.SecretKey, message: Array[Byte]): Array[Byte] = {
    val sk = Scalar.fromCanonicalBytes(privateKey.bytes)
    val pk = Constants.RISTRETTO_GENERATOR_TABLE.multiply(sk)
    val pkCompressed = pk.compress().toByteArray

    // Hedged nonce: k = H3(random_bytes(32) || sk)
    val randomBytes = new Array[Byte](32)
    Ristretto255.secureRandom.nextBytes(randomBytes)
    val k = h3(randomBytes, privateKey.bytes)

    // R = [k]G
    val rPoint = Constants.RISTRETTO_GENERATOR_TABLE.multiply(k)
    val rCompressed = rPoint.compress().toByteArray

    // Challenge: c = H2(R || PK || message)
    val c = h2(rCompressed, pkCompressed, message)

    // z = k + sk * c mod L
    val z = k.add(sk.multiply(c))

    // Signature = R || z (64 bytes)
    val sig = new Array[Byte](Ristretto255.SignatureLength)
    System.arraycopy(rCompressed, 0, sig, 0, 32)
    System.arraycopy(z.toByteArray, 0, sig, 32, 32)
    sig
  }

  override def verify(
    signature: Array[Byte],
    message:   Array[Byte],
    publicKey: Ristretto255.PublicKey
  ): Boolean = {
    if (signature.length != Ristretto255.SignatureLength || publicKey.bytes.length != Ristretto255.PublicKeyLength)
      return false

    try {
      // Decode R
      val rCompressed = signature.slice(0, 32)
      val rPoint = new CompressedRistretto(rCompressed).decompress()

      // Decode z — must be canonical
      val zBytes = signature.slice(32, 64)
      val z = Scalar.fromCanonicalBytes(zBytes)

      // Decode PK
      val pkPoint = new CompressedRistretto(publicKey.bytes).decompress()

      // Challenge: c = H2(R || PK || message)
      val c = h2(rCompressed, publicKey.bytes, message)

      // Check: [z]G == R + [c]PK
      val lhs = Constants.RISTRETTO_GENERATOR_TABLE.multiply(z)
      val rhs = rPoint.add(pkPoint.multiply(c))

      lhs.equals(rhs)
    } catch {
      case _: Exception => false
    }
  }

  override def getVerificationKey(privateKey: Ristretto255.SecretKey): Ristretto255.PublicKey = {
    val sk = Scalar.fromCanonicalBytes(privateKey.bytes)
    val pk = Constants.RISTRETTO_GENERATOR_TABLE.multiply(sk)
    Ristretto255.PublicKey(pk.compress().toByteArray)
  }

  // --- FROST domain-separated hash functions ---

  /**
   * H2: Challenge hash with FROST domain separator.
   * H2(m) = SHA-512("FROST-RISTRETTO255-SHA512-v1chal" || m) mod L
   */
  private def h2(r: Array[Byte], pk: Array[Byte], msg: Array[Byte]): Scalar = {
    val md = MessageDigest.getInstance("SHA-512")
    md.update(Ristretto255.H2_CONTEXT_STRING)
    md.update(r)
    md.update(pk)
    md.update(msg)
    Scalar.fromBytesModOrderWide(md.digest())
  }

  /**
   * H3: Nonce generation hash with FROST domain separator.
   * H3(m) = SHA-512("FROST-RISTRETTO255-SHA512-v1nonce" || m) mod L
   */
  private def h3(randomBytes: Array[Byte], sk: Array[Byte]): Scalar = {
    val md = MessageDigest.getInstance("SHA-512")
    md.update(Ristretto255.H3_CONTEXT_STRING)
    md.update(randomBytes)
    md.update(sk)
    Scalar.fromBytesModOrderWide(md.digest())
  }

  // --- SHA-512 helper ---

  private def sha512(input: Array[Byte]): Array[Byte] =
    MessageDigest.getInstance("SHA-512").digest(input)
}

object Ristretto255 {
  val SignatureLength: Int = 64
  val ScalarLength: Int = 32
  val PublicKeyLength: Int = 32
  val SeedLength: Int = 32

  // FROST(ristretto255, SHA-512) context strings
  private[signing] val H2_CONTEXT_STRING: Array[Byte] = "FROST-RISTRETTO255-SHA512-v1chal".getBytes("UTF-8")
  private[signing] val H3_CONTEXT_STRING: Array[Byte] = "FROST-RISTRETTO255-SHA512-v1nonce".getBytes("UTF-8")

  // Thread-safe SecureRandom for nonce generation
  private val secureRandom: SecureRandom = new SecureRandom()

  /**
   * Secret key = canonical 32-byte Scalar.
   *
   * Unlike the previous implementation, FROST keys do NOT include nonce material.
   * Nonces are generated fresh (hedged) for each signature.
   */
  case class SecretKey(bytes: Array[Byte]) extends SigningKey {
    require(bytes.length == ScalarLength, s"Invalid scalar length. Expected: $ScalarLength, got: ${bytes.length}")

    override def equals(obj: Any): Boolean = obj match {
      case that: SecretKey => java.util.Arrays.equals(this.bytes, that.bytes)
      case _               => false
    }
    override def hashCode(): Int = java.util.Arrays.hashCode(bytes)
  }

  case class PublicKey(bytes: Array[Byte]) extends VerificationKey {
    require(
      bytes.length == PublicKeyLength,
      s"Invalid public key length. Expected: $PublicKeyLength, got: ${bytes.length}"
    )

    override def equals(obj: Any): Boolean = obj match {
      case that: PublicKey => java.util.Arrays.equals(this.bytes, that.bytes)
      case _               => false
    }
    override def hashCode(): Int = java.util.Arrays.hashCode(bytes)
  }
}
