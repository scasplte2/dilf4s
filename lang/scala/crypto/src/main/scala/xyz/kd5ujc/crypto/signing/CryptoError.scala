package xyz.kd5ujc.crypto.signing

import xyz.kd5ujc.models.crypto_errors.CryptoErrorKind

enum CryptoError {
  case InvalidEncoding(subject: CryptoError.Subject, reason: CryptoError.EncodingReason)
  case InvalidKey(reason: CryptoError.KeyReason)
  case BackendFailure(operation: CryptoError.Operation, cause: Throwable)

  def kind: CryptoErrorKind = this match {
    case InvalidEncoding(_, _) => CryptoErrorKind.InvalidEncoding
    case InvalidKey(_)         => CryptoErrorKind.InvalidKey
    case BackendFailure(_, _)  => CryptoErrorKind.BackendFailure
  }

  def message: String = this match {
    case InvalidEncoding(subject, reason) => s"${subject.label}: ${reason.message}"
    case InvalidKey(reason)               => reason.message
    case BackendFailure(operation, cause) => s"${operation.label}: ${CryptoError.describe(cause)}"
  }
}

object CryptoError {
  private[crypto] def describe(cause: Throwable): String = Option(cause.getMessage).getOrElse(cause.getClass.getName)

  enum Scheme(val label: String, val keyLabel: String) {
    case Ed25519 extends Scheme("Ed25519", "Ed25519")
    case Ed25519Bip32 extends Scheme("Ed25519-BIP32", "Ed25519-BIP32")
    case Secp256k1Ecdsa extends Scheme("secp256k1-ECDSA", "secp256k1")
    case Bip340 extends Scheme("BIP-340", "BIP-340")
    case TaggedSchnorr extends Scheme("secp256k1-tagged-Schnorr", "secp256k1-tagged-Schnorr")
    case P256 extends Scheme("P-256-ECDSA", "P-256")
    case Ristretto255 extends Scheme("ristretto255-Schnorr", "ristretto255")
    case Bls extends Scheme("BLS12-381-PoP", "BLS")
  }

  enum Subject {
    case SigningKey(scheme: Scheme)
    case VerifyingKey(scheme: Scheme)
    case Signature(scheme: Scheme)
    case Bip340Message, Bip340AuxiliaryRandomness, NonceRandomness, RistrettoSeed, BlsAggregate, HashToCurveDst
    case EcrecoverDigest, EcrecoverSignature

    def label: String = this match {
      case SigningKey(Scheme.Secp256k1Ecdsa) => "secp256k1-ECDSA signing key"
      case SigningKey(scheme)                => s"${scheme.keyLabel} signing key"
      case VerifyingKey(Scheme.Bls)          => "BLS public key"
      case VerifyingKey(scheme)              => s"${scheme.keyLabel} verifying key"
      case Signature(scheme)                 => s"${scheme.keyLabel} signature"
      case Bip340Message                     => "BIP-340 message"
      case Bip340AuxiliaryRandomness         => "BIP-340 auxiliary randomness"
      case NonceRandomness                   => "nonce randomness"
      case RistrettoSeed                     => "ristretto255 seed"
      case BlsAggregate                      => "BLS aggregate"
      case HashToCurveDst                    => "hash-to-curve DST"
      case EcrecoverDigest                   => "ecrecover digest"
      case EcrecoverSignature                => "ecrecover signature"
    }
  }

  enum Operation(val label: String) {
    case SecKeyVerify extends Operation("secp256k1 secKeyVerify")
    case PubkeyParse extends Operation("secp256k1 pubkeyParse")
    case Verify extends Operation("secp256k1 verify")
    case PrivKeyTweakAdd extends Operation("secp256k1 privKeyTweakAdd")
    case PubKeyTweakAdd extends Operation("secp256k1 pubKeyTweakAdd")
    case SignSchnorr extends Operation("secp256k1 signSchnorr")
    case VerifySchnorr extends Operation("secp256k1 verifySchnorr")
    case BlsSecret extends Operation("blst from_bendian")
    case BlsG1Decode extends Operation("blst P1_Affine")
    case BlsG2Decode extends Operation("blst P2_Affine")
    case BlsPublicKey extends Operation("blst P1")
    case BlsSign extends Operation("blst sign")
    case BlsVerify extends Operation("blst core_verify")
    case BlsAggregate extends Operation("blst aggregate")
    case BlsPairing extends Operation("blst pairing")
    case BlsHashToCurve extends Operation("blst hash_to")
    case BlsMaster extends Operation("blst derive_master_eip2333")
    case BlsChild extends Operation("blst derive_child_eip2333")
  }

  enum DecodeFailure {
    case Foreign(cause: Throwable)
    case Backend(cause: CryptoError.BackendFailure)
    case Identity, ZeroScalar
  }

  enum EncodingReason {
    case Length(expected: Long, actual: Long)
    case NonCanonicalEcdsa
    case RistrettoLength(actual: Long)
    case RistrettoElement(cause: DecodeFailure)
    case RistrettoScalar(cause: DecodeFailure)
    case BlsDecode(cause: CryptoError.BackendFailure)
    case OutsideG2, EmptyAggregate, InvalidDst
    case Recovery(cause: Ecrecover.Error)

    def message: String = this match {
      case Length(expected, actual)                                      => s"expected $expected bytes, got $actual"
      case NonCanonicalEcdsa                                             => "r and s must be in range and s must be low (canonical)"
      case RistrettoLength(_) | RistrettoElement(_) | RistrettoScalar(_) => "expected R (non-identity element) || z (canonical scalar)"
      case BlsDecode(_)                                                  => "does not decode"
      case OutsideG2                                                     => "outside G2"
      case EmptyAggregate                                                => "at least one signature is required"
      case InvalidDst                                                    => "must be 1..255 printable ASCII characters"
      case Recovery(cause)                                               => cause.message
    }
  }

  enum KeyReason {
    case ScalarRange(scheme: Scheme)
    case InvalidPoint(scheme: Scheme)
    case CompressedPoint(scheme: Scheme)
    case Decode(scheme: Scheme, cause: DecodeFailure)
    case ScalarDecode(scheme: Scheme, cause: DecodeFailure)
    case Ed25519Bip32Scalar, BlsSubgroup
    case Eip2333SeedLength(actual: Long)
    case Eip2333Index(actual: Long)
    case Recovery(cause: Ecrecover.Error)

    def message: String = this match {
      case ScalarRange(Scheme.Bls) => "BLS secret key must be a scalar in [1, r)"
      case ScalarRange(Scheme.Ristretto255) | ScalarDecode(Scheme.Ristretto255, _) =>
        "ristretto255 secret key must be a canonical non-zero scalar"
      case ScalarRange(scheme)            => s"${scheme.keyLabel} secret key must be a scalar in [1, n)"
      case InvalidPoint(scheme)           => s"${scheme.keyLabel} verifying key is not a valid point"
      case CompressedPoint(scheme)        => s"${scheme.keyLabel} verifying key is not a compressed point"
      case Decode(Scheme.Ristretto255, _) => "ristretto255 verifying key is not a canonical non-identity element"
      case Decode(Scheme.Bls, _)          => "BLS public key does not decode"
      case Decode(scheme, _)              => s"${scheme.keyLabel} verifying key is not a valid point"
      case ScalarDecode(scheme, _)        => s"${scheme.keyLabel} secret key must be a scalar in [1, n)"
      case Ed25519Bip32Scalar             => "kL must be a multiple of 8 below 2^255"
      case BlsSubgroup                    => "BLS public key is the identity or outside G1"
      case Eip2333SeedLength(_)           => "EIP-2333 seeds are at least 32 bytes"
      case Eip2333Index(_)                => "EIP-2333 indices are 32-bit"
      case Recovery(cause)                => cause.message
    }
  }
}
