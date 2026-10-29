package xyz.kd5ujc.signature.keygen

trait SigningKey
trait VerificationKey
trait SignatureProof

// Generic KeyPair trait
trait KeyPair[F[_], SK <: SigningKey, VK <: VerificationKey, PRF <: SignatureProof] {
  def signingKey: SK
  def verificationKey: PRF

  def sign(msg:   Array[Byte]): F[SignatureProof]
  def verify(msg: Array[Byte], signature: Array[Byte]): F[Boolean]
}

// SigningKey should extends SignatureProver
// VerificationKey should extend SignatureVerifier

// Ed25519 needs to implement the prover, verifier, and key generator
// Can we also implement SECP256k1?
