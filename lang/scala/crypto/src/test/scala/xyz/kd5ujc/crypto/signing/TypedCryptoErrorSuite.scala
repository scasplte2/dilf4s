package xyz.kd5ujc.crypto.signing

import scala.compiletime.testing.{ErrorKind, typeCheckErrors}
import scala.compiletime.{erasedValue, summonFrom}
import scala.deriving.Mirror

import xyz.kd5ujc.crypto.keys.{Bip39, KeyError}

import weaver.FunSuite

object TypedCryptoErrorSuite extends FunSuite {
  private inline def typedField[T]: Boolean = inline erasedValue[T] match {
    case _: String                     => false
    case _: Int | _: Long | _: Boolean => true
    // Only foreign exceptions are opaque leaves; our domain sums are traversed below.
    case _: Throwable => true
    // Each owned sum is checked separately below to stay within compiler inline depth.
    case _: CryptoError | _: CryptoError.BackendFailure | _: CryptoError.Subject | _: CryptoError.Scheme                    => true
    case _: CryptoError.Operation | _: CryptoError.DecodeFailure | _: CryptoError.EncodingReason | _: CryptoError.KeyReason => true
    case _: KeyError.MnemonicReason | _: KeyError.WordListReason | _: KeyError.PassphraseReason                             => true
    case _: KeyError.DerivationReason | _: KeyError.PathReason | _: Ecrecover.Error | _: Bip39.Language                     => true
    case _: Option[t]                                                                                                       => typedField[t]
    case _: Vector[t]                                                                                                       => typedField[t]
    case _: List[t]                                                                                                         => typedField[t]
    case _: Map[k, v] => typedField[k] && typedField[v]
    case _ =>
      summonFrom {
        case m: Mirror.ProductOf[T] => typedFields[m.MirroredElemTypes]
        case m: Mirror.SumOf[T]     => typedCases[m.MirroredElemTypes]
        case _                      => false
      }
  }

  private inline def typedFields[T <: Tuple]: Boolean = inline erasedValue[T] match {
    case _: EmptyTuple => true
    case _: (h *: t)   => typedField[h] && typedFields[t]
  }

  private inline def typedProduct[T]: Boolean = summonFrom {
    case m: Mirror.ProductOf[T] => typedFields[m.MirroredElemTypes]
  }

  private inline def typedCases[T <: Tuple]: Boolean = inline erasedValue[T] match {
    case _: EmptyTuple              => true
    case _: (a *: b *: c *: d *: t) => typedProduct[a] && typedProduct[b] && typedProduct[c] && typedProduct[d] && typedCases[t]
    case _: (h *: t)                => typedProduct[h] && typedCases[t]
  }

  private inline def typedPayloads[T](using m: Mirror.SumOf[T]): Boolean = typedCases[m.MirroredElemTypes]

  test("payload checks reject strings, including strings inside containers") {
    expect
      .same(false, typedFields[String *: EmptyTuple])
      .and(expect.same(false, typedFields[Option[String] *: EmptyTuple]))
      .and(expect.same(false, typedFields[List[String] *: EmptyTuple]))
      .and(expect.same(false, typedFields[Vector[String] *: EmptyTuple]))
      .and(expect.same(false, typedFields[Map[Int, String] *: EmptyTuple]))
      .and(expect.same(false, typedFields[(Int, String) *: EmptyTuple]))
      .and(expect.same(false, typedFields[Any *: EmptyTuple]))
      .and(expect.same(true, typedFields[Long *: Int *: EmptyTuple]))
  }

  test("signing and key errors have no string payloads, including nested causes") {
    expect(typedPayloads[CryptoError], "CryptoError payloads must be typed")
      .and(expect(typedPayloads[KeyError], "KeyError payloads must be typed"))
      .and(expect(typedPayloads[Ecrecover.Error], "Ecrecover retains its decoder cause"))
      .and(expect(typedPayloads[Bip39.Language], "word-list languages are closed cases"))
      .and(expect(typedPayloads[CryptoError.Subject], "subjects are typed"))
      .and(expect(typedPayloads[CryptoError.Scheme], "schemes are typed"))
      .and(expect(typedPayloads[CryptoError.Operation], "backend operations are typed"))
      .and(expect(typedPayloads[CryptoError.DecodeFailure], "decoder failures retain causes"))
      .and(expect(typedPayloads[CryptoError.EncodingReason], "encoding reasons are typed"))
      .and(expect(typedPayloads[CryptoError.KeyReason], "key reasons are typed"))
      .and(expect(typedPayloads[KeyError.MnemonicReason], "mnemonic reasons are typed"))
      .and(expect(typedPayloads[KeyError.WordListReason], "word list reasons are typed"))
      .and(expect(typedPayloads[KeyError.PassphraseReason], "passphrase reasons are typed"))
      .and(expect(typedPayloads[KeyError.DerivationReason], "derivation reasons retain causes"))
      .and(expect(typedPayloads[KeyError.PathReason], "path reasons are typed"))
  }

  test("typed constructor positive controls compile") {
    val accepted = List(
      typeCheckErrors(
        "import xyz.kd5ujc.crypto.signing.CryptoError.*; InvalidEncoding(Subject.Bip340Message, EncodingReason.Length(32, 31))"
      ),
      typeCheckErrors("import xyz.kd5ujc.crypto.signing.CryptoError.*; InvalidKey(KeyReason.ScalarRange(Scheme.P256))"),
      typeCheckErrors("import xyz.kd5ujc.crypto.signing.CryptoError.*; BackendFailure(Operation.Verify, new IllegalArgumentException())"),
      typeCheckErrors("import xyz.kd5ujc.crypto.keys.KeyError.*; InvalidEntropy(15)"),
      typeCheckErrors("import xyz.kd5ujc.crypto.keys.KeyError.*; InvalidMnemonic(MnemonicReason.Checksum)"),
      typeCheckErrors(
        "import xyz.kd5ujc.crypto.keys.*; KeyError.InvalidWordList(Bip39.Language.English, KeyError.WordListReason.Count(0))"
      ),
      typeCheckErrors("import xyz.kd5ujc.crypto.keys.KeyError.*; InvalidPassphrase(PassphraseReason.UnassignedCodePoint)"),
      typeCheckErrors("import xyz.kd5ujc.crypto.keys.KeyError.*; InvalidDerivation(DerivationReason.PrivateKeyRequired)"),
      typeCheckErrors("import xyz.kd5ujc.crypto.keys.KeyError.*; InvalidPath(PathReason.Malformed(0))")
    )
    expect.same(9, accepted.size).and(expect.same(List.empty, accepted.flatten))
  }

  test("the compiler rejects every former string constructor") {
    val rejected = List(
      2 -> typeCheckErrors("""xyz.kd5ujc.crypto.signing.CryptoError.InvalidEncoding("key", "bad")"""),
      1 -> typeCheckErrors("""xyz.kd5ujc.crypto.signing.CryptoError.InvalidKey("bad")"""),
      2 -> typeCheckErrors("""xyz.kd5ujc.crypto.signing.CryptoError.BackendFailure("verify", "bad")"""),
      1 -> typeCheckErrors("""xyz.kd5ujc.crypto.keys.KeyError.InvalidEntropy("bad")"""),
      1 -> typeCheckErrors("""xyz.kd5ujc.crypto.keys.KeyError.InvalidMnemonic("bad")"""),
      2 -> typeCheckErrors("""xyz.kd5ujc.crypto.keys.KeyError.InvalidWordList("bad", None)"""),
      1 -> typeCheckErrors("""xyz.kd5ujc.crypto.keys.KeyError.InvalidPassphrase("bad")"""),
      1 -> typeCheckErrors("""xyz.kd5ujc.crypto.keys.KeyError.InvalidDerivation("bad")"""),
      1 -> typeCheckErrors("""xyz.kd5ujc.crypto.keys.KeyError.InvalidPath("bad")""")
    )
    rejected.foldLeft(expect.same(9, rejected.size)) {
      case (acc, (count, errors)) =>
        acc.and(expect.same(List.fill(count)(ErrorKind.Typer), errors.map(_.kind)))
    }
  }

  test("generated string error families cannot bypass native errors") {
    val rejected = List(
      typeCheckErrors("""xyz.kd5ujc.models.crypto_errors.CryptoError.InvalidKey("bad")"""),
      typeCheckErrors("""xyz.kd5ujc.models.crypto_errors.KeyError.InvalidMnemonic("bad")""")
    )
    rejected.foldLeft(expect.same(2, rejected.size))((acc, errors) => acc.and(expect.same(List(ErrorKind.Typer), errors.map(_.kind))))
  }
}
