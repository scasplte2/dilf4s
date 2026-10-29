package xyz.kd5ujc.signature.ecdsa.api

import xyz.kd5ujc.signature.signature.{Message, SignatureProof, SigningKey, VerificationKey}

trait SignatureScheme[F[_]] {
  def generateKeyPair: F[(SigningKey, VerificationKey)]
  def sign(secretKey:   SigningKey, message:      Message): F[SignatureProof]
  def verify(publicKey: VerificationKey, message: Message, signature: SignatureProof): F[Boolean]
}
