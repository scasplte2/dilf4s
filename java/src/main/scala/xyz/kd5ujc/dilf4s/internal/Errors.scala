package xyz.kd5ujc.dilf4s.internal

import xyz.kd5ujc.jlvm.wire

/**
 * The reference's error families as the Java API's: one exhaustive match per family, so a kind added to a schema is
 * a compile error here until the Java family has it. Pure and total.
 */
object Errors {

  def preview(p: xyz.kd5ujc.core.canonical.Preview): xyz.kd5ujc.dilf4s.error.Preview =
    new xyz.kd5ujc.dilf4s.error.Preview(p.head, p.length, p.truncated)

  def jsonSyntaxReason(r: xyz.kd5ujc.models.codec_errors.JsonSyntaxReason): xyz.kd5ujc.dilf4s.error.JsonSyntaxReason =
    r match {
      case "expectedValue"          => xyz.kd5ujc.dilf4s.error.JsonSyntaxReason.EXPECTED_VALUE
      case "expectedMemberName"     => xyz.kd5ujc.dilf4s.error.JsonSyntaxReason.EXPECTED_MEMBER_NAME
      case "expectedColon"          => xyz.kd5ujc.dilf4s.error.JsonSyntaxReason.EXPECTED_COLON
      case "expectedCommaOrBracket" => xyz.kd5ujc.dilf4s.error.JsonSyntaxReason.EXPECTED_COMMA_OR_BRACKET
      case "expectedCommaOrBrace"   => xyz.kd5ujc.dilf4s.error.JsonSyntaxReason.EXPECTED_COMMA_OR_BRACE
      case "trailingContent"        => xyz.kd5ujc.dilf4s.error.JsonSyntaxReason.TRAILING_CONTENT
      case "unterminatedString"     => xyz.kd5ujc.dilf4s.error.JsonSyntaxReason.UNTERMINATED_STRING
      case "controlCharacter"       => xyz.kd5ujc.dilf4s.error.JsonSyntaxReason.CONTROL_CHARACTER
      case "invalidEscape"          => xyz.kd5ujc.dilf4s.error.JsonSyntaxReason.INVALID_ESCAPE
    }

  def nonCanonicalReason(r: xyz.kd5ujc.models.codec_errors.NonCanonicalReason): xyz.kd5ujc.dilf4s.error.NonCanonicalReason =
    r match {
      case "nonShortestArgument" => xyz.kd5ujc.dilf4s.error.NonCanonicalReason.NON_SHORTEST_ARGUMENT
      case "keyOrder"            => xyz.kd5ujc.dilf4s.error.NonCanonicalReason.KEY_ORDER
      case "nullMapValue"        => xyz.kd5ujc.dilf4s.error.NonCanonicalReason.NULL_MAP_VALUE
      case "bignumLeadingZero"   => xyz.kd5ujc.dilf4s.error.NonCanonicalReason.BIGNUM_LEADING_ZERO
      case "bignumInRange"       => xyz.kd5ujc.dilf4s.error.NonCanonicalReason.BIGNUM_IN_RANGE
      case "bignumNotBytes"      => xyz.kd5ujc.dilf4s.error.NonCanonicalReason.BIGNUM_NOT_BYTES
      case "rationalNotPair"     => xyz.kd5ujc.dilf4s.error.NonCanonicalReason.RATIONAL_NOT_PAIR
      case "rationalNotInteger"  => xyz.kd5ujc.dilf4s.error.NonCanonicalReason.RATIONAL_NOT_INTEGER
      case "rationalDenominator" => xyz.kd5ujc.dilf4s.error.NonCanonicalReason.RATIONAL_DENOMINATOR
      case "rationalNotReduced"  => xyz.kd5ujc.dilf4s.error.NonCanonicalReason.RATIONAL_NOT_REDUCED
    }

  def notJson(r: xyz.kd5ujc.models.codec_errors.NotJson): xyz.kd5ujc.dilf4s.error.NotJson =
    r match {
      case "bytes"                    => xyz.kd5ujc.dilf4s.error.NotJson.BYTES
      case "non-terminating rational" => xyz.kd5ujc.dilf4s.error.NotJson.NON_TERMINATING_RATIONAL
      case "over-long number"         => xyz.kd5ujc.dilf4s.error.NotJson.OVER_LONG_NUMBER
    }

  def expansionRefusal(r: xyz.kd5ujc.models.hash_errors.ExpansionRefusal): xyz.kd5ujc.dilf4s.error.ExpansionRefusal =
    r match {
      case "emptyDst"     => xyz.kd5ujc.dilf4s.error.ExpansionRefusal.EMPTY_DST
      case "outputLength" => xyz.kd5ujc.dilf4s.error.ExpansionRefusal.OUTPUT_LENGTH
    }

  def pathRefusal(r: xyz.kd5ujc.models.crypto_errors.PathRefusal): xyz.kd5ujc.dilf4s.error.PathRefusal =
    r match {
      case "missingMaster" => xyz.kd5ujc.dilf4s.error.PathRefusal.MISSING_MASTER
      case "malformed"     => xyz.kd5ujc.dilf4s.error.PathRefusal.MALFORMED
      case "leadingZero"   => xyz.kd5ujc.dilf4s.error.PathRefusal.LEADING_ZERO
      case "tooManyDigits" => xyz.kd5ujc.dilf4s.error.PathRefusal.TOO_MANY_DIGITS
      case "outOfRange"    => xyz.kd5ujc.dilf4s.error.PathRefusal.OUT_OF_RANGE
      case "tooDeep"       => xyz.kd5ujc.dilf4s.error.PathRefusal.TOO_DEEP
    }

  def rootComponent(r: xyz.kd5ujc.models.proofs_errors.VtRootComponent): xyz.kd5ujc.dilf4s.error.RootComponent =
    r match {
      case "root"    => xyz.kd5ujc.dilf4s.error.RootComponent.ROOT
      case "state"   => xyz.kd5ujc.dilf4s.error.RootComponent.STATE
      case "history" => xyz.kd5ujc.dilf4s.error.RootComponent.HISTORY
      case "parent"  => xyz.kd5ujc.dilf4s.error.RootComponent.PARENT
    }

  def canonicalError(e: xyz.kd5ujc.models.codec_errors.CanonicalError): xyz.kd5ujc.dilf4s.error.CanonicalError =
    e match {
      case xyz.kd5ujc.models.codec_errors.CanonicalError.UnpairedSurrogate => new xyz.kd5ujc.dilf4s.error.CanonicalError.UnpairedSurrogate()
      case xyz.kd5ujc.models.codec_errors.CanonicalError.DuplicateKey(key) =>
        new xyz.kd5ujc.dilf4s.error.CanonicalError.DuplicateKey(preview(key))
      case xyz.kd5ujc.models.codec_errors.CanonicalError.RationalTooWide(limit) =>
        new xyz.kd5ujc.dilf4s.error.CanonicalError.RationalTooWide(limit)
      case xyz.kd5ujc.models.codec_errors.CanonicalError.IntegerTooWide(limit) =>
        new xyz.kd5ujc.dilf4s.error.CanonicalError.IntegerTooWide(limit)
      case xyz.kd5ujc.models.codec_errors.CanonicalError.JsonSyntax(offset, reason) =>
        new xyz.kd5ujc.dilf4s.error.CanonicalError.JsonSyntax(offset, jsonSyntaxReason(reason))
      case xyz.kd5ujc.models.codec_errors.CanonicalError.InvalidNumber(length, offset, bound) =>
        new xyz.kd5ujc.dilf4s.error.CanonicalError.InvalidNumber(
          length.fold(java.util.OptionalLong.empty())(java.util.OptionalLong.of),
          offset.fold(java.util.OptionalLong.empty())(java.util.OptionalLong.of),
          bound.fold(java.util.Optional.empty[xyz.kd5ujc.dilf4s.error.NumberBound]())(x => java.util.Optional.of(numberBound(x)))
        )
      case xyz.kd5ujc.models.codec_errors.CanonicalError.NotJsonRepresentable(what) =>
        new xyz.kd5ujc.dilf4s.error.CanonicalError.NotJsonRepresentable(notJson(what))
      case xyz.kd5ujc.models.codec_errors.CanonicalError.TypeMismatch(reason) => new xyz.kd5ujc.dilf4s.error.TypeMismatch(reason)
      case xyz.kd5ujc.models.codec_errors.CanonicalError.UnexpectedEnd(offset) =>
        new xyz.kd5ujc.dilf4s.error.CanonicalError.UnexpectedEnd(offset)
      case xyz.kd5ujc.models.codec_errors.CanonicalError.TrailingBytes(offset) =>
        new xyz.kd5ujc.dilf4s.error.CanonicalError.TrailingBytes(offset)
      case xyz.kd5ujc.models.codec_errors.CanonicalError.DepthExceeded(limit) =>
        new xyz.kd5ujc.dilf4s.error.CanonicalError.DepthExceeded(limit)
      case xyz.kd5ujc.models.codec_errors.CanonicalError.NonCanonical(offset, reason) =>
        new xyz.kd5ujc.dilf4s.error.CanonicalError.NonCanonical(offset, nonCanonicalReason(reason))
      case xyz.kd5ujc.models.codec_errors.CanonicalError.Unsupported(offset, item) =>
        new xyz.kd5ujc.dilf4s.error.CanonicalError.Unsupported(offset, unsupportedItem(item))
      case xyz.kd5ujc.models.codec_errors.CanonicalError.InvalidUtf8(offset) =>
        new xyz.kd5ujc.dilf4s.error.CanonicalError.InvalidUtf8(offset)
    }

  def numberBound(e: xyz.kd5ujc.models.codec_errors.NumberBound): xyz.kd5ujc.dilf4s.error.NumberBound =
    e match {
      case xyz.kd5ujc.models.codec_errors.NumberBound.Digits(digits, limit) => new xyz.kd5ujc.dilf4s.error.NumberBound.Digits(digits, limit)
      case xyz.kd5ujc.models.codec_errors.NumberBound.Scale(scale, limit)   => new xyz.kd5ujc.dilf4s.error.NumberBound.Scale(scale, limit)
    }

  def unsupportedItem(e: xyz.kd5ujc.models.codec_errors.UnsupportedItem): xyz.kd5ujc.dilf4s.error.UnsupportedItem =
    e match {
      case xyz.kd5ujc.models.codec_errors.UnsupportedItem.FloatOrSimple(info) =>
        new xyz.kd5ujc.dilf4s.error.UnsupportedItem.FloatOrSimple(info)
      case xyz.kd5ujc.models.codec_errors.UnsupportedItem.SimpleValue(value) =>
        new xyz.kd5ujc.dilf4s.error.UnsupportedItem.SimpleValue(value)
      case xyz.kd5ujc.models.codec_errors.UnsupportedItem.IndefiniteLength => new xyz.kd5ujc.dilf4s.error.UnsupportedItem.IndefiniteLength()
      case xyz.kd5ujc.models.codec_errors.UnsupportedItem.ReservedInfo(info) =>
        new xyz.kd5ujc.dilf4s.error.UnsupportedItem.ReservedInfo(info)
      case xyz.kd5ujc.models.codec_errors.UnsupportedItem.Tag(tag)   => new xyz.kd5ujc.dilf4s.error.UnsupportedItem.Tag(tag.bigInteger)
      case xyz.kd5ujc.models.codec_errors.UnsupportedItem.NonTextKey => new xyz.kd5ujc.dilf4s.error.UnsupportedItem.NonTextKey()
    }

  def hashError(e: xyz.kd5ujc.models.hash_errors.HashError): xyz.kd5ujc.dilf4s.error.HashError =
    e match {
      case xyz.kd5ujc.models.hash_errors.HashError.InvalidLength(algorithm, expected, actual) =>
        new xyz.kd5ujc.dilf4s.error.HashError.InvalidLength(algorithm, expected, actual)
      case xyz.kd5ujc.models.hash_errors.HashError.InvalidHex(offset) => new xyz.kd5ujc.dilf4s.error.HashError.InvalidHex(offset)
      case xyz.kd5ujc.models.hash_errors.HashError.InvalidDomainTag(tag) =>
        new xyz.kd5ujc.dilf4s.error.HashError.InvalidDomainTag(preview(tag))
      case xyz.kd5ujc.models.hash_errors.HashError.InvalidExpansion(refusal, algorithm, limit) =>
        new xyz.kd5ujc.dilf4s.error.HashError.InvalidExpansion(expansionRefusal(refusal), algorithm, limit)
      case xyz.kd5ujc.models.hash_errors.HashError.UnknownAlgorithm(name) =>
        new xyz.kd5ujc.dilf4s.error.HashError.UnknownAlgorithm(preview(name))
    }

  def cryptoError(e: xyz.kd5ujc.models.crypto_errors.CryptoError): xyz.kd5ujc.dilf4s.error.CryptoError =
    e match {
      case xyz.kd5ujc.models.crypto_errors.CryptoError.InvalidEncoding(what, reason) =>
        new xyz.kd5ujc.dilf4s.error.CryptoError.InvalidEncoding(what, reason)
      case xyz.kd5ujc.models.crypto_errors.CryptoError.InvalidKey(reason) => new xyz.kd5ujc.dilf4s.error.CryptoError.InvalidKey(reason)
      case xyz.kd5ujc.models.crypto_errors.CryptoError.BackendFailure(operation, reason) =>
        new xyz.kd5ujc.dilf4s.error.CryptoError.BackendFailure(operation, reason)
    }

  def keyError(e: xyz.kd5ujc.models.crypto_errors.KeyError): xyz.kd5ujc.dilf4s.error.KeyError =
    e match {
      case xyz.kd5ujc.models.crypto_errors.KeyError.InvalidEntropy(reason)  => new xyz.kd5ujc.dilf4s.error.KeyError.InvalidEntropy(reason)
      case xyz.kd5ujc.models.crypto_errors.KeyError.InvalidMnemonic(reason) => new xyz.kd5ujc.dilf4s.error.KeyError.InvalidMnemonic(reason)
      case xyz.kd5ujc.models.crypto_errors.KeyError.InvalidWordList(reason, separator) =>
        new xyz.kd5ujc.dilf4s.error.KeyError.InvalidWordList(
          reason,
          separator.fold(java.util.Optional.empty[xyz.kd5ujc.dilf4s.error.Preview]())(x => java.util.Optional.of(preview(x)))
        )
      case xyz.kd5ujc.models.crypto_errors.KeyError.InvalidPassphrase(reason) =>
        new xyz.kd5ujc.dilf4s.error.KeyError.InvalidPassphrase(reason)
      case xyz.kd5ujc.models.crypto_errors.KeyError.InvalidDerivation(reason) =>
        new xyz.kd5ujc.dilf4s.error.KeyError.InvalidDerivation(reason)
      case xyz.kd5ujc.models.crypto_errors.KeyError.InvalidPath(refusal, reason) =>
        new xyz.kd5ujc.dilf4s.error.KeyError.InvalidPath(pathRefusal(refusal), reason)
    }

  def vrfError(e: xyz.kd5ujc.models.crypto_errors.VrfError): xyz.kd5ujc.dilf4s.error.VrfError =
    e match {
      case xyz.kd5ujc.models.crypto_errors.VrfError.Malformed(what, reason) => new xyz.kd5ujc.dilf4s.error.VrfError.Malformed(what, reason)
      case xyz.kd5ujc.models.crypto_errors.VrfError.InvalidPublicKey        => new xyz.kd5ujc.dilf4s.error.VrfError.InvalidPublicKey()
      case xyz.kd5ujc.models.crypto_errors.VrfError.SmallOrderPublicKey     => new xyz.kd5ujc.dilf4s.error.VrfError.SmallOrderPublicKey()
      case xyz.kd5ujc.models.crypto_errors.VrfError.InvalidGamma            => new xyz.kd5ujc.dilf4s.error.VrfError.InvalidGamma()
      case xyz.kd5ujc.models.crypto_errors.VrfError.NonCanonicalScalar      => new xyz.kd5ujc.dilf4s.error.VrfError.NonCanonicalScalar()
      case xyz.kd5ujc.models.crypto_errors.VrfError.ChallengeMismatch       => new xyz.kd5ujc.dilf4s.error.VrfError.ChallengeMismatch()
      case xyz.kd5ujc.models.crypto_errors.VrfError.KeyMismatch             => new xyz.kd5ujc.dilf4s.error.VrfError.KeyMismatch()
      case xyz.kd5ujc.models.crypto_errors.VrfError.HashToCurveFailed(reason) =>
        new xyz.kd5ujc.dilf4s.error.VrfError.HashToCurveFailed(reason)
    }

  def kesError(e: xyz.kd5ujc.models.crypto_errors.KesError): xyz.kd5ujc.dilf4s.error.KesError =
    e match {
      case xyz.kd5ujc.models.crypto_errors.KesError.Malformed(what, reason) => new xyz.kd5ujc.dilf4s.error.KesError.Malformed(what, reason)
      case xyz.kd5ujc.models.crypto_errors.KesError.PeriodOutOfRange(period, total) =>
        new xyz.kd5ujc.dilf4s.error.KesError.PeriodOutOfRange(period, total)
      case xyz.kd5ujc.models.crypto_errors.KesError.Exhausted(period)       => new xyz.kd5ujc.dilf4s.error.KesError.Exhausted(period)
      case xyz.kd5ujc.models.crypto_errors.KesError.InconsistentKey(reason) => new xyz.kd5ujc.dilf4s.error.KesError.InconsistentKey(reason)
    }

  def sigmaError(e: xyz.kd5ujc.models.crypto_errors.SigmaError): xyz.kd5ujc.dilf4s.error.SigmaError =
    e match {
      case xyz.kd5ujc.models.crypto_errors.SigmaError.InvalidProposition(reason) =>
        new xyz.kd5ujc.dilf4s.error.SigmaError.InvalidProposition(reason)
      case xyz.kd5ujc.models.crypto_errors.SigmaError.InvalidPoint(what, reason) =>
        new xyz.kd5ujc.dilf4s.error.SigmaError.InvalidPoint(what, reason)
      case xyz.kd5ujc.models.crypto_errors.SigmaError.ProofLength(expected, actual) =>
        new xyz.kd5ujc.dilf4s.error.SigmaError.ProofLength(expected, actual)
      case xyz.kd5ujc.models.crypto_errors.SigmaError.ResponseOutOfRange => new xyz.kd5ujc.dilf4s.error.SigmaError.ResponseOutOfRange()
    }

  def zkError(e: xyz.kd5ujc.models.crypto_errors.ZkError): xyz.kd5ujc.dilf4s.error.ZkError =
    e match {
      case xyz.kd5ujc.models.crypto_errors.ZkError.Malformed(reason)    => new xyz.kd5ujc.dilf4s.error.ZkError.Malformed(reason)
      case xyz.kd5ujc.models.crypto_errors.ZkError.InvalidPoint(reason) => new xyz.kd5ujc.dilf4s.error.ZkError.InvalidPoint(reason)
    }

  def groth16Error(e: xyz.kd5ujc.models.crypto_errors.Groth16Error): xyz.kd5ujc.dilf4s.error.Groth16Error =
    e match {
      case xyz.kd5ujc.models.crypto_errors.Groth16Error.Malformed(reason) => new xyz.kd5ujc.dilf4s.error.Groth16Error.Malformed(reason)
      case xyz.kd5ujc.models.crypto_errors.Groth16Error.InvalidProof(reason) =>
        new xyz.kd5ujc.dilf4s.error.Groth16Error.InvalidProof(reason)
      case xyz.kd5ujc.models.crypto_errors.Groth16Error.PairingCheckFailed => new xyz.kd5ujc.dilf4s.error.Groth16Error.PairingCheckFailed()
      case xyz.kd5ujc.models.crypto_errors.Groth16Error.Rejected(reason)   => new xyz.kd5ujc.dilf4s.error.Groth16Error.Rejected(reason)
    }

  def merkleError(e: xyz.kd5ujc.models.proofs_errors.MerkleError): xyz.kd5ujc.dilf4s.error.MerkleError =
    e match {
      case xyz.kd5ujc.models.proofs_errors.MerkleError.IndexOutOfRange(index, size) =>
        new xyz.kd5ujc.dilf4s.error.MerkleError.IndexOutOfRange(index.bigInteger, size.bigInteger)
      case xyz.kd5ujc.models.proofs_errors.MerkleError.InvalidSizes(oldSize, newSize) =>
        new xyz.kd5ujc.dilf4s.error.MerkleError.InvalidSizes(oldSize.bigInteger, newSize.bigInteger)
      case xyz.kd5ujc.models.proofs_errors.MerkleError.InvalidProof(reason) => new xyz.kd5ujc.dilf4s.error.MerkleError.InvalidProof(reason)
      case xyz.kd5ujc.models.proofs_errors.MerkleError.RootMismatch(expected, computed) =>
        new xyz.kd5ujc.dilf4s.error.MerkleError.RootMismatch(expected, computed)
      case xyz.kd5ujc.models.proofs_errors.MerkleError.Encoding(reason) => new xyz.kd5ujc.dilf4s.error.MerkleError.Encoding(reason)
      case xyz.kd5ujc.models.proofs_errors.MerkleError.TooLarge(limit)  => new xyz.kd5ujc.dilf4s.error.MerkleError.TooLarge(limit)
    }

  def poseidonError(e: xyz.kd5ujc.models.proofs_errors.PoseidonError): xyz.kd5ujc.dilf4s.error.PoseidonError =
    e match {
      case xyz.kd5ujc.models.proofs_errors.PoseidonError.InputCount(count, max) =>
        new xyz.kd5ujc.dilf4s.error.PoseidonError.InputCount(count, max)
      case xyz.kd5ujc.models.proofs_errors.PoseidonError.NonCanonical(bits) => new xyz.kd5ujc.dilf4s.error.PoseidonError.NonCanonical(bits)
    }

  def poseidonMerkleError(e: xyz.kd5ujc.models.proofs_errors.PoseidonMerkleError): xyz.kd5ujc.dilf4s.error.PoseidonMerkleError =
    e match {
      case xyz.kd5ujc.models.proofs_errors.PoseidonMerkleError.Position(positionBits, depth) =>
        new xyz.kd5ujc.dilf4s.error.PoseidonMerkleError.Position(positionBits, depth)
      case xyz.kd5ujc.models.proofs_errors.PoseidonMerkleError.Depth(depth) => new xyz.kd5ujc.dilf4s.error.PoseidonMerkleError.Depth(depth)
      case xyz.kd5ujc.models.proofs_errors.PoseidonMerkleError.OccupiedPosition(positionBits) =>
        new xyz.kd5ujc.dilf4s.error.PoseidonMerkleError.OccupiedPosition(positionBits)
      case xyz.kd5ujc.models.proofs_errors.PoseidonMerkleError.Encoding(reason) =>
        new xyz.kd5ujc.dilf4s.error.PoseidonMerkleError.Encoding(reason)
    }

  def trieError(e: xyz.kd5ujc.models.proofs_errors.TrieError): xyz.kd5ujc.dilf4s.error.TrieError =
    e match {
      case xyz.kd5ujc.models.proofs_errors.TrieError.InvalidKey(reason)   => new xyz.kd5ujc.dilf4s.error.TrieError.InvalidKey(reason)
      case xyz.kd5ujc.models.proofs_errors.TrieError.MissingNode(hash)    => new xyz.kd5ujc.dilf4s.error.TrieError.MissingNode(hash)
      case xyz.kd5ujc.models.proofs_errors.TrieError.InvalidNode(reason)  => new xyz.kd5ujc.dilf4s.error.TrieError.InvalidNode(reason)
      case xyz.kd5ujc.models.proofs_errors.TrieError.InvalidProof(reason) => new xyz.kd5ujc.dilf4s.error.TrieError.InvalidProof(reason)
      case xyz.kd5ujc.models.proofs_errors.TrieError.RootMismatch(expected, computed) =>
        new xyz.kd5ujc.dilf4s.error.TrieError.RootMismatch(expected, computed)
      case xyz.kd5ujc.models.proofs_errors.TrieError.Encoding(reason) => new xyz.kd5ujc.dilf4s.error.TrieError.Encoding(reason)
      case xyz.kd5ujc.models.proofs_errors.TrieError.NotCovered(what) => new xyz.kd5ujc.dilf4s.error.TrieError.NotCovered(what)
      case xyz.kd5ujc.models.proofs_errors.TrieError.TooLarge(limit)  => new xyz.kd5ujc.dilf4s.error.TrieError.TooLarge(limit)
    }

  def stateKeyError(e: xyz.kd5ujc.models.codec_errors.StateKeyError): xyz.kd5ujc.dilf4s.error.StateKeyError =
    e match {
      case xyz.kd5ujc.models.codec_errors.StateKeyError.Empty          => new xyz.kd5ujc.dilf4s.error.StateKeyError.Empty()
      case xyz.kd5ujc.models.codec_errors.StateKeyError.TooLong(limit) => new xyz.kd5ujc.dilf4s.error.StateKeyError.TooLong(limit)
      case xyz.kd5ujc.models.codec_errors.StateKeyError.NotUtf8        => new xyz.kd5ujc.dilf4s.error.StateKeyError.NotUtf8()
      case xyz.kd5ujc.models.codec_errors.StateKeyError.TooManySegments(key, limit) =>
        new xyz.kd5ujc.dilf4s.error.StateKeyError.TooManySegments(preview(key), limit)
      case xyz.kd5ujc.models.codec_errors.StateKeyError.InvalidSegment(key, segment) =>
        new xyz.kd5ujc.dilf4s.error.StateKeyError.InvalidSegment(preview(key), segment)
      case xyz.kd5ujc.models.codec_errors.StateKeyError.SegmentTooLong(key, segment, limit) =>
        new xyz.kd5ujc.dilf4s.error.StateKeyError.SegmentTooLong(preview(key), segment, limit)
      case xyz.kd5ujc.models.codec_errors.StateKeyError.NamespaceFull(namespace) =>
        new xyz.kd5ujc.dilf4s.error.StateKeyError.NamespaceFull(preview(namespace))
    }

  def storeError(e: xyz.kd5ujc.models.store_errors.StoreError): xyz.kd5ujc.dilf4s.error.StoreError =
    e match {
      case xyz.kd5ujc.models.store_errors.StoreError.Encoding(context, reason) =>
        new xyz.kd5ujc.dilf4s.error.StoreError.Encoding(context, reason)
      case xyz.kd5ujc.models.store_errors.StoreError.NotFound(keyBytes, prefix) =>
        new xyz.kd5ujc.dilf4s.error.StoreError.NotFound(keyBytes, preview(prefix))
      case xyz.kd5ujc.models.store_errors.StoreError.VersionConflict(expected, actual) =>
        new xyz.kd5ujc.dilf4s.error.StoreError.VersionConflict(expected, actual)
      case xyz.kd5ujc.models.store_errors.StoreError.UnknownVersion(requested, latest) =>
        new xyz.kd5ujc.dilf4s.error.StoreError.UnknownVersion(requested, latest)
      case xyz.kd5ujc.models.store_errors.StoreError.Pruned(requested, floor) =>
        new xyz.kd5ujc.dilf4s.error.StoreError.Pruned(requested, floor)
      case xyz.kd5ujc.models.store_errors.StoreError.Backend(operation, reason) =>
        new xyz.kd5ujc.dilf4s.error.StoreError.Backend(operation, reason)
      case xyz.kd5ujc.models.store_errors.StoreError.Corrupt(reason) => new xyz.kd5ujc.dilf4s.error.StoreError.Corrupt(reason)
    }

  def versionedTrieError(e: xyz.kd5ujc.models.proofs_errors.VersionedTrieError): xyz.kd5ujc.dilf4s.error.VersionedTrieError =
    e match {
      case xyz.kd5ujc.models.proofs_errors.VersionedTrieError.Key(error) =>
        new xyz.kd5ujc.dilf4s.error.VersionedTrieError.Key(stateKeyError(error))
      case xyz.kd5ujc.models.proofs_errors.VersionedTrieError.InvalidVersion(reason) =>
        new xyz.kd5ujc.dilf4s.error.VersionedTrieError.InvalidVersion(reason)
      case xyz.kd5ujc.models.proofs_errors.VersionedTrieError.Empty => new xyz.kd5ujc.dilf4s.error.VersionedTrieError.Empty()
      case xyz.kd5ujc.models.proofs_errors.VersionedTrieError.VersionNotIncreasing(head, next) =>
        new xyz.kd5ujc.dilf4s.error.VersionedTrieError.VersionNotIncreasing(head, next)
      case xyz.kd5ujc.models.proofs_errors.VersionedTrieError.VersionConflict(expected, actual) =>
        new xyz.kd5ujc.dilf4s.error.VersionedTrieError.VersionConflict(
          expected.fold(java.util.Optional.empty[String]())(x => java.util.Optional.of(x)),
          actual.fold(java.util.Optional.empty[String]())(x => java.util.Optional.of(x))
        )
      case xyz.kd5ujc.models.proofs_errors.VersionedTrieError.CommitRewrite(version) =>
        new xyz.kd5ujc.dilf4s.error.VersionedTrieError.CommitRewrite(version)
      case xyz.kd5ujc.models.proofs_errors.VersionedTrieError.NotRetained(version) =>
        new xyz.kd5ujc.dilf4s.error.VersionedTrieError.NotRetained(version)
      case xyz.kd5ujc.models.proofs_errors.VersionedTrieError.UnknownVersion(version) =>
        new xyz.kd5ujc.dilf4s.error.VersionedTrieError.UnknownVersion(version)
      case xyz.kd5ujc.models.proofs_errors.VersionedTrieError.RootMismatch(component, expected, computed) =>
        new xyz.kd5ujc.dilf4s.error.VersionedTrieError.RootMismatch(rootComponent(component), expected, computed)
      case xyz.kd5ujc.models.proofs_errors.VersionedTrieError.NotCovered(what) =>
        new xyz.kd5ujc.dilf4s.error.VersionedTrieError.NotCovered(what)
      case xyz.kd5ujc.models.proofs_errors.VersionedTrieError.Malformed(reason) =>
        new xyz.kd5ujc.dilf4s.error.VersionedTrieError.Malformed(reason)
      case xyz.kd5ujc.models.proofs_errors.VersionedTrieError.TooLarge(what, limit) =>
        new xyz.kd5ujc.dilf4s.error.VersionedTrieError.TooLarge(what, limit)
      case xyz.kd5ujc.models.proofs_errors.VersionedTrieError.TooDeep(what, depth, limit) =>
        new xyz.kd5ujc.dilf4s.error.VersionedTrieError.TooDeep(what, depth, limit)
      case xyz.kd5ujc.models.proofs_errors.VersionedTrieError.Store(error) =>
        new xyz.kd5ujc.dilf4s.error.VersionedTrieError.Store(storeError(error))
      case xyz.kd5ujc.models.proofs_errors.VersionedTrieError.Trie(error) =>
        new xyz.kd5ujc.dilf4s.error.VersionedTrieError.Trie(trieError(error))
    }

  def cryptoCause(e: xyz.kd5ujc.models.jlvm_errors.JlvmCryptoCause): xyz.kd5ujc.dilf4s.error.CryptoCause =
    e match {
      case xyz.kd5ujc.models.jlvm_errors.JlvmCryptoCause.Signing(error) =>
        new xyz.kd5ujc.dilf4s.error.CryptoCause.Signing(cryptoError(error))
      case xyz.kd5ujc.models.jlvm_errors.JlvmCryptoCause.Sigma(error) => new xyz.kd5ujc.dilf4s.error.CryptoCause.Sigma(sigmaError(error))
      case xyz.kd5ujc.models.jlvm_errors.JlvmCryptoCause.Zk(error)    => new xyz.kd5ujc.dilf4s.error.CryptoCause.Zk(zkError(error))
      case xyz.kd5ujc.models.jlvm_errors.JlvmCryptoCause.Groth16(error) =>
        new xyz.kd5ujc.dilf4s.error.CryptoCause.Groth16(groth16Error(error))
      case xyz.kd5ujc.models.jlvm_errors.JlvmCryptoCause.Vrf(error)    => new xyz.kd5ujc.dilf4s.error.CryptoCause.Vrf(vrfError(error))
      case xyz.kd5ujc.models.jlvm_errors.JlvmCryptoCause.Hash(error)   => new xyz.kd5ujc.dilf4s.error.CryptoCause.Hash(hashError(error))
      case xyz.kd5ujc.models.jlvm_errors.JlvmCryptoCause.Merkle(error) => new xyz.kd5ujc.dilf4s.error.CryptoCause.Merkle(merkleError(error))
      case xyz.kd5ujc.models.jlvm_errors.JlvmCryptoCause.Trie(error)   => new xyz.kd5ujc.dilf4s.error.CryptoCause.Trie(trieError(error))
      case xyz.kd5ujc.models.jlvm_errors.JlvmCryptoCause.Poseidon(error) =>
        new xyz.kd5ujc.dilf4s.error.CryptoCause.Poseidon(poseidonError(error))
      case xyz.kd5ujc.models.jlvm_errors.JlvmCryptoCause.PoseidonMerkle(error) =>
        new xyz.kd5ujc.dilf4s.error.CryptoCause.PoseidonMerkle(poseidonMerkleError(error))
    }

  def jlvmError(e: xyz.kd5ujc.models.jlvm_errors.JlvmError): xyz.kd5ujc.dilf4s.error.JlvmError =
    e match {
      case xyz.kd5ujc.models.jlvm_errors.JlvmError.Malformed(message) => new xyz.kd5ujc.dilf4s.error.JlvmError.Malformed(message)
      case xyz.kd5ujc.models.jlvm_errors.JlvmError.Type(op, message)  => new xyz.kd5ujc.dilf4s.error.JlvmError.Type(op.wire, message)
      case xyz.kd5ujc.models.jlvm_errors.JlvmError.Arity(op, message) => new xyz.kd5ujc.dilf4s.error.JlvmError.Arity(op.wire, message)
      case xyz.kd5ujc.models.jlvm_errors.JlvmError.Arithmetic(op, message) =>
        new xyz.kd5ujc.dilf4s.error.JlvmError.Arithmetic(op.wire, message)
      case xyz.kd5ujc.models.jlvm_errors.JlvmError.Domain(op, message) => new xyz.kd5ujc.dilf4s.error.JlvmError.Domain(op.wire, message)
      case xyz.kd5ujc.models.jlvm_errors.JlvmError.Crypto(op, message, cause) =>
        new xyz.kd5ujc.dilf4s.error.JlvmError.Crypto(
          op.wire,
          message,
          cause.fold(java.util.Optional.empty[xyz.kd5ujc.dilf4s.error.CryptoCause]())(x => java.util.Optional.of(cryptoCause(x)))
        )
      case xyz.kd5ujc.models.jlvm_errors.JlvmError.Limit(message) => new xyz.kd5ujc.dilf4s.error.JlvmError.Limit(message)
      case xyz.kd5ujc.models.jlvm_errors.JlvmError.OutOfGas(required, available) =>
        new xyz.kd5ujc.dilf4s.error.JlvmError.OutOfGas(required, available)
    }
}
