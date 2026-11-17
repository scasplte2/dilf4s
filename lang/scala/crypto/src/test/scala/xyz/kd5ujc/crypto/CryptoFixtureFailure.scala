package xyz.kd5ujc.crypto

import xyz.kd5ujc.crypto.keys.KeyError
import xyz.kd5ujc.crypto.signing.CryptoError
import xyz.kd5ujc.crypto.vrf.VrfError

final case class CryptoFixtureFailure(error: CryptoError | KeyError | VrfError) extends RuntimeException {
  override def getMessage: String = error.toString
}

private[crypto] enum VerificationVerdict {
  case Valid, Invalid, MalformedKey, MalformedSignature
}

private[crypto] enum FixtureInvariant {
  case Verification(expected: VerificationVerdict, actual: VerificationVerdict)
  case KeyDecode(
    expected: Option[xyz.kd5ujc.models.crypto_errors.CryptoErrorKind],
    actual:   Option[xyz.kd5ujc.models.crypto_errors.CryptoErrorKind]
  )
  case Derivation(expected: xyz.kd5ujc.models.crypto_errors.KeyErrorKind, actual: Option[xyz.kd5ujc.models.crypto_errors.KeyErrorKind])
  case Vrf(expected: Option[xyz.kd5ujc.models.crypto_errors.VrfErrorKind], actual: Option[xyz.kd5ujc.models.crypto_errors.VrfErrorKind])
  case MissingEdwardsPoint, MissingOffCurvePoint, MissingNonCanonicalPoint, NotSmallOrder, PopRejected, DerivationMismatch
}

private[crypto] final case class FixtureViolation(reason: FixtureInvariant) extends RuntimeException {
  override def getMessage: String = reason.toString
}

private[crypto] object FixtureViolation {
  def ensure(condition: Boolean, reason: FixtureInvariant): Unit = Either.cond(condition, (), FixtureViolation(reason)).toTry.get
}
