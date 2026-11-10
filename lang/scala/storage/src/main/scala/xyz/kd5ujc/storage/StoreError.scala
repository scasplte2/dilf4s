package xyz.kd5ujc.storage

import java.nio.file.Path

import xyz.kd5ujc.core.canonical.{CanonicalError, Cbor, DecodeFailure, Preview}
import xyz.kd5ujc.core.hash.HashError
import xyz.kd5ujc.models.codec_errors.StateKeyError

import scodec.bits.ByteVector

export xyz.kd5ujc.models.store_errors.StoreErrorKind

enum StoreError {
  case Encoding(context: StoreContext, reason: EncodingReason)
  case NotFound(keyBytes: Long, prefix: Preview)
  case VersionConflict(expected: Long, actual: Long)
  case UnknownVersion(requested: Long, latest: Long)
  case Pruned(requested: Long, floor: Long)
  case Backend(operation: BackendOperation, cause: Throwable)
  case Corrupt(reason: CorruptionReason)

  def kind: StoreErrorKind = this match {
    case Encoding(_, _)        => StoreErrorKind.Encoding
    case NotFound(_, _)        => StoreErrorKind.NotFound
    case VersionConflict(_, _) => StoreErrorKind.VersionConflict
    case UnknownVersion(_, _)  => StoreErrorKind.UnknownVersion
    case Pruned(_, _)          => StoreErrorKind.Pruned
    case Backend(_, _)         => StoreErrorKind.Backend
    case Corrupt(_)            => StoreErrorKind.Corrupt
  }
}

enum StoreContext {
  case Key, Value, Namespace, StateKey, VersionRecord, TrieNode, MerkleLogState

  def label: String = this match {
    case Key            => "key"
    case Value          => "value"
    case Namespace      => "namespace"
    case StateKey       => "state key"
    case VersionRecord  => "record"
    case TrieNode       => "node record"
    case MerkleLogState => "Merkle log state"
  }
}

enum EncodingReason {
  case FixedWidth(expected: Long, actual: Long)
  case InvalidUtf8, UnterminatedComponent, InvalidEscape
  case Canonical(cause: CanonicalError)
  case Decode(cause: DecodeFailure)
  case Hash(cause: HashError)
  case EncodedSize(cause: Cbor.TooLarge)
  case StateKey(cause: StateKeyError)
  case NamespaceCharacters(actual: Int, maximum: Int)
  case NamespaceBytes(name: Preview, minimum: Int, maximum: Int)

  def message: String = this match {
    case FixedWidth(expected, actual)           => s"expected $expected bytes, got $actual"
    case InvalidUtf8                            => "invalid UTF-8"
    case UnterminatedComponent                  => "unterminated component"
    case InvalidEscape                          => "invalid escape"
    case Canonical(cause)                       => cause.toString
    case Decode(cause)                          => cause.message
    case Hash(cause)                            => cause.toString
    case EncodedSize(cause)                     => s"its encoding is over ${cause.limit} bytes"
    case StateKey(cause)                        => cause.toString
    case NamespaceCharacters(actual, maximum)   => s"$actual characters, more than $maximum UTF-8 bytes"
    case NamespaceBytes(name, minimum, maximum) => s"$name must be $minimum..$maximum UTF-8 bytes"
  }
}

enum StoredRecord {
  case Version, TrieState, VersionIndex

  def label: String = this match {
    case Version      => "version record"
    case TrieState    => "trie state"
    case VersionIndex => "version index record"
  }
}

enum CorruptionReason {
  case Width(record: StoredRecord, actual: Long, expected: Long)
  case VersionRecordCount(actual: Int, expected: Int)
  case Canonical(context: StoreContext, cause: CanonicalError)
  case Decode(context: StoreContext, cause: DecodeFailure)
  case Hash(cause: HashError)
  case TrieBranchInvariant
  case NodeHashMismatch(expected: ByteVector, computed: ByteVector)
  case MissingPerfectNode(level: Int, index: Long)
  case SequenceRecordTooShort(actual: Long, minimum: Long)
  case MissingSequenceEntry(sequence: Long)
  case MissingHead(sequence: Long)
  case StoresOutOfStep(state: Long, history: Long, values: Long)
  case IndexOutOfStep(sequence: Long, currentPresent: Boolean, nextPresent: Boolean)

  def message: String = this match {
    case Width(record, actual, _)                => s"${record.label} has $actual bytes"
    case VersionRecordCount(actual, expected)    => s"expected $expected version records, got $actual"
    case Canonical(context, cause)               => s"${context.label}: $cause"
    case Decode(context, cause)                  => s"${context.label}: ${cause.message}"
    case Hash(cause)                             => cause.toString
    case TrieBranchInvariant                     => "node violates the branch invariant"
    case NodeHashMismatch(expected, computed)    => s"node ${expected.toHex} hashes to ${computed.toHex}"
    case MissingPerfectNode(level, index)        => s"missing perfect node ($level, $index)"
    case SequenceRecordTooShort(_, _)            => "sequence index record"
    case MissingSequenceEntry(sequence)          => s"no index entry for sequence $sequence"
    case MissingHead(sequence)                   => s"no head at sequence $sequence"
    case StoresOutOfStep(state, history, values) => s"stores out of step: state $state, history $history, values $values"
    case IndexOutOfStep(sequence, _, _)          => s"version index out of step at sequence $sequence"
  }
}

enum BackendOperation {
  case LoadLibrary, Compact, Get, MultiGet, BatchPut, BatchDelete, Write, Seek, Iterate, Status
  case Open(path: Path)

  def label: String = this match {
    case LoadLibrary => "load library"
    case Open(path)  => s"open $path"
    case Compact     => "compact"
    case Get         => "get"
    case MultiGet    => "multiGet"
    case BatchPut    => "batch put"
    case BatchDelete => "batch delete"
    case Write       => "write"
    case Seek        => "seek"
    case Iterate     => "iterate"
    case Status      => "status"
  }
}

/** A missing key is represented by its byte length and the hex of at most its first 16 bytes. */
def notFound(key: ByteVector): StoreError = StoreError.NotFound(key.size, Preview.of(key.take(16).toHex))
