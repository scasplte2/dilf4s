package xyz.kd5ujc.crypto.signing

import cats.effect.IO

import xyz.kd5ujc.crypto.bls.Bls12381
import xyz.kd5ujc.crypto.keys.{Bip39, ChildIndex, Ed25519Bip32, KeyError, Secret, Slip10Ed25519, WordList}
import xyz.kd5ujc.crypto.signing.CryptoError.{DecodeFailure, EncodingReason, KeyReason, Operation, Scheme, Subject}
import xyz.kd5ujc.models.crypto_errors.{CryptoErrorKind, KeyErrorKind}

import scodec.bits.ByteVector
import weaver.SimpleIOSuite

object SigningErrorSuite extends SimpleIOSuite {
  pureTest("wrong widths retain the subject and exact expected and actual byte counts") {
    val empty = ByteVector.empty
    val cases = List(
      Ed25519.codec.signingKey(empty)     -> CryptoError.InvalidEncoding(Subject.SigningKey(Scheme.Ed25519), EncodingReason.Length(32, 0)),
      P256Ecdsa.codec.verifyingKey(empty) -> CryptoError.InvalidEncoding(Subject.VerifyingKey(Scheme.P256), EncodingReason.Length(33, 0)),
      Message32.from(empty)               -> CryptoError.InvalidEncoding(Subject.Bip340Message, EncodingReason.Length(32, 0)),
      Ed25519Bip32.codec.signingKey(empty) -> CryptoError
        .InvalidEncoding(Subject.SigningKey(Scheme.Ed25519Bip32), EncodingReason.Length(64, 0))
    )
    cases.foldLeft(expect.same(4, cases.size)) { case (acc, (result, expected)) => acc.and(expect.same(Left(expected), result)) }
  }

  pureTest("Ristretto distinguishes width, identity element and noncanonical scalar refusals") {
    val zero = ByteVector.fill(32)(0)
    val validElement = ByteVector.fromValidHex("e2f2ae0a6abc4e71a884a961c500515f58e30b6aa582dd8db6a65945e08d2d76")
    expect
      .same(
        Left(CryptoError.InvalidEncoding(Subject.Signature(Scheme.Ristretto255), EncodingReason.RistrettoLength(0))),
        Ristretto255Schnorr.codec.signature(ByteVector.empty)
      )
      .and(
        expect.same(
          Left(CryptoError.InvalidKey(KeyReason.Decode(Scheme.Ristretto255, DecodeFailure.Identity))),
          Ristretto255Schnorr.codec.verifyingKey(zero)
        )
      )
      .and(
        expect.same(
          Left(CryptoError.InvalidKey(KeyReason.ScalarDecode(Scheme.Ristretto255, DecodeFailure.ZeroScalar))),
          Ristretto255Schnorr.codec.signingKey(zero)
        )
      )
      .and(
        expect.same(
          Left(
            CryptoError.InvalidEncoding(Subject.Signature(Scheme.Ristretto255), EncodingReason.RistrettoElement(DecodeFailure.Identity))
          ),
          Ristretto255Schnorr.codec.signature(zero ++ zero)
        )
      )
      .and(Ristretto255Schnorr.codec.signature(validElement ++ ByteVector.fill(32)(0xff.toByte)) match {
        case Left(
              CryptoError
                .InvalidEncoding(Subject.Signature(Scheme.Ristretto255), EncodingReason.RistrettoScalar(DecodeFailure.Foreign(cause)))
            ) =>
          expect.same(classOf[IllegalArgumentException], cause.getClass)
        case other => failure(s"expected a retained noncanonical scalar cause, got $other")
      })
  }

  test("native point failures preserve their backend operation and cause through key derivation") {
    Secp256k1.load[IO].map { backend =>
      val invalid = ByteVector(2) ++ ByteVector.fill(32)(0xff.toByte)
      backend.ecdsaCodec.verifyingKey(invalid) match {
        case Left(error @ CryptoError.InvalidKey(KeyReason.Decode(Scheme.Secp256k1Ecdsa, DecodeFailure.Backend(cause)))) =>
          val derived: KeyError.InvalidDerivation = KeyError.InvalidDerivation(KeyError.DerivationReason.Crypto(error))
          expect
            .same(Operation.PubkeyParse, cause.operation)
            .and(expect.same(classOf[fr.acinq.secp256k1.Secp256k1Exception], cause.cause.getClass))
            .and(expect.same(CryptoErrorKind.InvalidKey, error.kind))
            .and(expect.same(KeyErrorKind.InvalidDerivation, derived.kind))
            .and(expect.same(KeyError.DerivationReason.Crypto(error), derived.reason))
        case other => failure(s"expected a retained secp256k1 decoding cause, got $other")
      }
    }
  }

  test("BLS keeps exact key and aggregate refusals") {
    Bls12381.load[IO].map { bls =>
      expect
        .same(Left(CryptoError.InvalidKey(KeyReason.Eip2333SeedLength(0))), bls.eip2333Master(ByteVector.empty))
        .and(expect.same(Left(CryptoError.InvalidEncoding(Subject.BlsAggregate, EncodingReason.EmptyAggregate)), bls.aggregate(Nil)))
        .and(
          expect
            .same(Left(CryptoError.InvalidEncoding(Subject.HashToCurveDst, EncodingReason.InvalidDst)), bls.hashToG2(ByteVector.empty, ""))
        )
    }
  }

  pureTest("ecrecover conversion retains typed refusals and the original foreign cause") {
    val cause = new IllegalArgumentException()
    val invalid = Ecrecover.Error.InvalidPublicKey(cause)
    val recovery = Ecrecover.Error.InvalidRecoveryId(29)
    expect
      .same(CryptoError.InvalidKey(KeyReason.Recovery(invalid)), invalid.toCryptoError)
      .and(expect.same(CryptoError.InvalidEncoding(Subject.EcrecoverSignature, EncodingReason.Recovery(recovery)), recovery.toCryptoError))
  }

  test("key derivation and BIP39 refusals retain exact reasons and counts") {
    WordList.load[IO](Bip39.Language.English).map {
      case Left(error) => failure(s"could not load fixture: ${error.message}")
      case Right(words) =>
        expect
          .same(Left(KeyError.InvalidEntropy(0)), Bip39.toMnemonic(Secret(ByteVector.empty), words))
          .and(expect.same(Left(KeyError.InvalidMnemonic(KeyError.MnemonicReason.WordCount(1))), Bip39.toEntropy("abandon", words)))
          .and(
            expect.same(
              Left(KeyError.InvalidWordList(Bip39.Language.English, KeyError.WordListReason.Count(0))),
              WordList(Bip39.Language.English, Vector.empty)
            )
          )
          .and(expect.same(Left(KeyError.InvalidDerivation(KeyError.DerivationReason.IndexOutOfRange(-1))), ChildIndex.soft(-1)))
          .and(
            expect.same(
              Left(KeyError.InvalidDerivation(KeyError.DerivationReason.SeedLength(0))),
              Slip10Ed25519.master(Secret(ByteVector.empty))
            )
          )
          .and(
            expect.same(
              Left(KeyError.InvalidDerivation(KeyError.DerivationReason.RootSecretLength(0))),
              Ed25519Bip32.root(Secret(ByteVector.empty))
            )
          )
    }
  }
}
