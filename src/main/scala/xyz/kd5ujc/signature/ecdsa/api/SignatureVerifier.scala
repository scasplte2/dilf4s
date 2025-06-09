package xyz.kd5ujc.signature.ecdsa.api

import xyz.kd5ujc.signature.signature.{Message, SignatureProof, VerificationKey}

trait SignatureVerifier[F[_]] {
  def verify(publicKey: VerificationKey, message: Message, signature: SignatureProof): F[Boolean]
}
