package xyz.kd5ujc.jlvm

import java.nio.charset.StandardCharsets.UTF_8

import xyz.kd5ujc.core.hash.{Hasher, Keccak256}
import xyz.kd5ujc.crypto.signing.Ecrecover

import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.crypto.params.{ECDomainParameters, ECPrivateKeyParameters}
import org.bouncycastle.crypto.signers.{ECDSASigner, HMacDSAKCalculator}
import scodec.bits.ByteVector

/**
 * Ethereum-style keys for tests: RFC 6979 ECDSA over secp256k1 (BouncyCastle), low `s`, with the recovery byte `v`
 * found by recovering. `Ecrecover` itself is checked against libsecp256k1 in the crypto tests.
 */
object EthKeys {

  private val params = CustomNamedCurves.getByName("secp256k1")
  private val domain = new ECDomainParameters(params.getCurve, params.getG, params.getN, params.getH)
  private val N = BigInt(params.getN)

  private def be32(i: BigInt): ByteVector = ByteVector(i.toByteArray).takeRight(32).padLeft(32)

  def keccak(b: ByteVector): ByteVector = Hasher[Keccak256].digest(b).bytes
  def keccak(s: String): ByteVector = keccak(ByteVector(s.getBytes(UTF_8)))

  def address(key: BigInt): ByteVector =
    Ecrecover.address(ByteVector.view(params.getG.multiply(key.bigInteger).normalize().getEncoded(false))).toOption.get

  /** `r ‖ s ‖ v` over a 32-byte digest, `v` in {27, 28}. */
  def sign(key: BigInt, digest: ByteVector): ByteVector = {
    val signer = new ECDSASigner(new HMacDSAKCalculator(new SHA256Digest()))
    signer.init(true, new ECPrivateKeyParameters(key.bigInteger, domain))
    val rs = signer.generateSignature(digest.toArray).map(BigInt(_)).toList match {
      case List(r, s) => be32(r) ++ be32(if (s > N / 2) N - s else s)
      case other      => sys.error(s"unexpected signature $other")
    }
    List(27, 28).map(v => rs :+ v.toByte).find(sig => Ecrecover.recover(digest, sig) == Right(Some(address(key)))).get
  }

  /** EIP-191 `personal_sign` of a 32-byte hash: Keccak-256 of "\u0019Ethereum Signed Message:\n32" ‖ hash. */
  val PersonalPrefix: ByteVector = ByteVector("\u0019Ethereum Signed Message:\n32".getBytes(UTF_8))
  def personal(hash: ByteVector): ByteVector = keccak(PersonalPrefix ++ hash)

  def hex(b: ByteVector): String = "0x" + b.toHex
}
