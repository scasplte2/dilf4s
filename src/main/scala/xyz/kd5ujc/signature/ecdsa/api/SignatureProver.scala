package xyz.kd5ujc.signature.ecdsa.api

import xyz.kd5ujc.signature.signature.{Message, SignatureProof, SigningKey}

trait SignatureProver[F[_]] {
  def sign(secretKey: SigningKey, message: Message): F[SignatureProof]
}
