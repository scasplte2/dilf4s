package xyz.kd5ujc.crypto.keys

import xyz.kd5ujc.crypto.signing.CryptoError
import xyz.kd5ujc.models.crypto_errors.KeyErrorKind

enum KeyError {
  case InvalidEntropy(actualBytes: Long)
  case InvalidMnemonic(reason: KeyError.MnemonicReason)
  case InvalidWordList(language: Bip39.Language, reason: KeyError.WordListReason)
  case InvalidPassphrase(reason: KeyError.PassphraseReason)
  case InvalidDerivation(reason: KeyError.DerivationReason)
  case InvalidPath(reason: KeyError.PathReason)

  def kind: KeyErrorKind = this match {
    case InvalidEntropy(_)     => KeyErrorKind.InvalidEntropy
    case InvalidMnemonic(_)    => KeyErrorKind.InvalidMnemonic
    case InvalidWordList(_, _) => KeyErrorKind.InvalidWordList
    case InvalidPassphrase(_)  => KeyErrorKind.InvalidPassphrase
    case InvalidDerivation(_)  => KeyErrorKind.InvalidDerivation
    case InvalidPath(_)        => KeyErrorKind.InvalidPath
  }

  def message: String = this match {
    case InvalidEntropy(actual)                                             => s"entropy must be 16..32 bytes in steps of 4, got $actual"
    case InvalidMnemonic(reason)                                            => reason.message
    case InvalidWordList(language, KeyError.WordListReason.MissingResource) => s"missing resource for ${language.name}"
    case InvalidWordList(language, reason)                                  => s"${language.name}: ${reason.message}"
    case InvalidPassphrase(reason)                                          => reason.message
    case InvalidDerivation(reason)                                          => reason.message
    case InvalidPath(reason)                                                => reason.message
  }
}

object KeyError {
  enum MnemonicReason {
    case UnassignedCodePoint, UnknownWord, Checksum
    case WordCount(actual: Int)
    def message: String = this match {
      case UnassignedCodePoint => "the phrase holds a code point Unicode 15.0 does not assign"
      case UnknownWord         => "phrase contains a word not in the list"
      case Checksum            => "checksum mismatch"
      case WordCount(actual)   => s"expected 12, 15, 18, 21 or 24 words, got $actual"
    }
  }

  enum WordListReason {
    case Count(actual: Int)
    case UnassignedCodePoint, InvalidWords, MissingResource
    def message: String = this match {
      case Count(actual)       => s"expected 2048 words, got $actual"
      case UnassignedCodePoint => "a word holds a code point Unicode 15.0 does not assign"
      case InvalidWords        => "expected 2048 distinct words without separators"
      case MissingResource     => "missing resource"
    }
  }

  enum PassphraseReason {
    case UnassignedCodePoint
    def message: String = this match {
      case UnassignedCodePoint => "the passphrase holds a code point Unicode 15.0 does not assign"
    }
  }

  enum DerivationReason {
    case IndexOutOfRange(actual: Long)
    case ChainCodeLength(actual: Long)
    case SeedLength(actual: Long)
    case RootSecretLength(actual: Long)
    case HardenedOnly, TweakOutOfRange, PrivateKeyRequired, NonCanonicalPoint
    case InvalidScalar(cause: CryptoError)
    case Crypto(cause: CryptoError)
    case PointDecode(cause: Throwable)
    def message: String = this match {
      case IndexOutOfRange(actual)            => s"index $actual out of range"
      case ChainCodeLength(actual)            => s"a chain code is 32 bytes, got $actual"
      case SeedLength(_)                      => "seed must be 16..64 bytes"
      case RootSecretLength(_)                => "root secret must be 96 bytes"
      case HardenedOnly                       => "SLIP-10 Ed25519 supports hardened indices only"
      case TweakOutOfRange                    => "I_L is not below n (skip this index, per BIP32)"
      case PrivateKeyRequired                 => "hardened children need the private key"
      case InvalidScalar(_)                   => "derived key is not a valid scalar (skip this index, per BIP32)"
      case Crypto(cause)                      => cause.message
      case PointDecode(_) | NonCanonicalPoint => "verifying key is not a canonical point encoding"
    }
  }

  enum PathReason {
    case MissingMaster, TooDeep
    case Malformed(segment: Int)
    case LeadingZero(segment: Int)
    case TooManyDigits(segment: Int, actual: Int)
    case OutOfRange(segment: Int)

    def refusal: PathRefusal = this match {
      case MissingMaster       => PathRefusal.MissingMaster
      case TooDeep             => PathRefusal.TooDeep
      case Malformed(_)        => PathRefusal.Malformed
      case LeadingZero(_)      => PathRefusal.LeadingZero
      case TooManyDigits(_, _) => PathRefusal.TooManyDigits
      case OutOfRange(_)       => PathRefusal.OutOfRange
    }

    def message: String = this match {
      case MissingMaster             => "path must start with m"
      case TooDeep                   => s"path has more than ${ChildIndex.MaxDepth} segments"
      case Malformed(at)             => s"path segment $at is malformed"
      case LeadingZero(at)           => s"path segment $at has a leading zero"
      case TooManyDigits(at, actual) => s"path segment $at has $actual digits, more than ${ChildIndex.MaxSegmentDigits}"
      case OutOfRange(at)            => s"path segment $at is out of range"
    }
  }

  extension (error: InvalidPath) def refusal: PathRefusal = error.reason.refusal
}
