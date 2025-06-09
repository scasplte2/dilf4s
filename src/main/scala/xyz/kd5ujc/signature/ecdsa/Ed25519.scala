package xyz.kd5ujc.signature.ecdsa

import java.security.SecureRandom

import org.bouncycastle.crypto.generators.Ed25519KeyPairGenerator
import org.bouncycastle.crypto.params.{Ed25519KeyGenerationParameters, Ed25519PrivateKeyParameters, Ed25519PublicKeyParameters}
import org.bouncycastle.crypto.signers.Ed25519Signer

class Ed25519 {
  val SIGNATURE_SIZE: Int = 64
  val PUBLIC_KEY_SIZE: Int = 32
  val PRIVATE_KEY_SIZE: Int = 32

  def generatePrivateKey(random: SecureRandom, privateKey: Array[Byte]): Unit = {
    val generator = new Ed25519KeyPairGenerator()
    generator.init(new Ed25519KeyGenerationParameters(random))
    val keyPair = generator.generateKeyPair()
    val privKey = keyPair.getPrivate.asInstanceOf[Ed25519PrivateKeyParameters]
    System.arraycopy(privKey.getEncoded(), 0, privateKey, 0, PRIVATE_KEY_SIZE)
  }

  def generatePublicKey(privateKey: Array[Byte], privateKeyOffset: Int, publicKey: Array[Byte], publicKeyOffset: Int): Unit = {
    val privKey = new Ed25519PrivateKeyParameters(privateKey, privateKeyOffset)
    val pubKey = privKey.generatePublicKey()
    System.arraycopy(pubKey.getEncoded(), 0, publicKey, publicKeyOffset, PUBLIC_KEY_SIZE)
  }

  def sign(
    privateKey:       Array[Byte],
    privateKeyOffset: Int,
    message:          Array[Byte],
    messageOffset:    Int,
    messageLen:       Int,
    signature:        Array[Byte],
    signatureOffset:  Int
  ): Unit = {
    val signer = new Ed25519Signer()
    val privKey = new Ed25519PrivateKeyParameters(privateKey, privateKeyOffset)
    signer.init(true, privKey)
    signer.update(message, messageOffset, messageLen)
    val sig = signer.generateSignature()
    System.arraycopy(sig, 0, signature, signatureOffset, SIGNATURE_SIZE)
  }

  def verify(
    signature:       Array[Byte],
    signatureOffset: Int,
    publicKey:       Array[Byte],
    publicKeyOffset: Int,
    message:         Array[Byte],
    messageOffset:   Int,
    messageLen:      Int
  ): Boolean = {
    val verifier = new Ed25519Signer()
    val pubKey = new Ed25519PublicKeyParameters(publicKey, publicKeyOffset)
    verifier.init(false, pubKey)
    verifier.update(message, messageOffset, messageLen)
    verifier.verifySignature(signature.slice(signatureOffset, signatureOffset + SIGNATURE_SIZE))
  }
}
