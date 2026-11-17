package xyz.kd5ujc.dilf4s.internal

import java.util.Optional

import xyz.kd5ujc.crypto.keys.{Bip39, KeyError}
import xyz.kd5ujc.crypto.signing.CryptoError
import xyz.kd5ujc.crypto.signing.CryptoError.{EncodingReason, KeyReason, Operation, Scheme, Subject}
import xyz.kd5ujc.dilf4s.error.{CryptoError => JCrypto, KeyError => JKey, PathRefusal => JPath, Preview => JPreview}
import xyz.kd5ujc.models.crypto_errors.{CryptoErrorKind, KeyErrorKind}

import weaver.FunSuite

object SigningErrorAdapterSuite extends FunSuite {
  test("all signing cases retain Java record kinds and diagnostics") {
    val cases: Vector[(CryptoError, JCrypto)] = Vector(
      CryptoError.InvalidEncoding(Subject.Signature(Scheme.Secp256k1Ecdsa), EncodingReason.Length(64, 0)) ->
      new JCrypto.InvalidEncoding("secp256k1 signature", "expected 64 bytes, got 0"),
      CryptoError.InvalidKey(KeyReason.ScalarRange(Scheme.P256)) -> new JCrypto.InvalidKey("P-256 secret key must be a scalar in [1, n)"),
      CryptoError.BackendFailure(Operation.Verify, new IllegalArgumentException("backend detail")) ->
      new JCrypto.BackendFailure("secp256k1 verify", "backend detail")
    )
    expect
      .same(CryptoErrorKind.values.toVector, cases.map(_._1.kind))
      .and(expect.same(cases.map(_._2), cases.map(c => Errors.cryptoError(c._1))))
  }

  test("all key cases retain Java record kinds, absent separators and path refusals") {
    val cases: Vector[(KeyError, JKey)] = Vector(
      KeyError.InvalidEntropy(0) -> new JKey.InvalidEntropy("entropy must be 16..32 bytes in steps of 4, got 0"),
      KeyError.InvalidMnemonic(KeyError.MnemonicReason.Checksum) -> new JKey.InvalidMnemonic("checksum mismatch"),
      KeyError.InvalidWordList(Bip39.Language.English, KeyError.WordListReason.Count(0)) ->
      new JKey.InvalidWordList("english: expected 2048 words, got 0", Optional.empty[JPreview]()),
      KeyError.InvalidPassphrase(KeyError.PassphraseReason.UnassignedCodePoint) ->
      new JKey.InvalidPassphrase("the passphrase holds a code point Unicode 15.0 does not assign"),
      KeyError.InvalidDerivation(KeyError.DerivationReason.PrivateKeyRequired) -> new JKey.InvalidDerivation(
        "hardened children need the private key"
      ),
      KeyError
        .InvalidPath(KeyError.PathReason.LeadingZero(2)) -> new JKey.InvalidPath(JPath.LEADING_ZERO, "path segment 2 has a leading zero")
    )
    expect
      .same(KeyErrorKind.values.toVector, cases.map(_._1.kind))
      .and(expect.same(cases.map(_._2), cases.map(c => Errors.keyError(c._1))))
  }
}
