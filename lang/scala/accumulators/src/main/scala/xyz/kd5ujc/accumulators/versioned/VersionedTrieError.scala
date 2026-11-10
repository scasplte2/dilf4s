package xyz.kd5ujc.accumulators.versioned

import xyz.kd5ujc.accumulators.trie.TrieError
import xyz.kd5ujc.core.canonical.{CanonicalError, DecodeFailure, Preview}
import xyz.kd5ujc.core.hash.HashError
import xyz.kd5ujc.models.codec_errors.StateKeyError
import xyz.kd5ujc.storage.StoreError

import scodec.bits.ByteVector

export xyz.kd5ujc.models.proofs_errors.{VersionKeyHex, VersionedTrieErrorKind, VtRootComponent}

/** Native failures retain structured causes; only the kind and documented protocol fields cross the wire. */
enum VersionedTrieError {
  case Key(error: StateKeyError)
  case InvalidVersion(reason: VersionRefusal)
  case Empty
  case VersionNotIncreasing(head: VersionKeyHex, next: VersionKeyHex)
  case VersionConflict(expected: Option[VersionKeyHex], actual: Option[VersionKeyHex])
  case CommitRewrite(version: VersionKeyHex)
  case NotRetained(version: VersionKeyHex)
  case UnknownVersion(version: VersionKeyHex)
  case RootMismatch(component: VtRootComponent, expected: Option[ByteVector], computed: Option[ByteVector])
  case NotCovered(query: ProofQuery)
  case Malformed(reason: MalformedReason)
  case TooLarge(subject: LimitSubject, limit: Long)
  case TooDeep(subject: LimitSubject, depth: Long, limit: Long)
  case Store(error: StoreError)
  case Trie(error: TrieError)

  def kind: VersionedTrieErrorKind = this match {
    case Key(_)                     => VersionedTrieErrorKind.Key
    case InvalidVersion(_)          => VersionedTrieErrorKind.InvalidVersion
    case Empty                      => VersionedTrieErrorKind.Empty
    case VersionNotIncreasing(_, _) => VersionedTrieErrorKind.VersionNotIncreasing
    case VersionConflict(_, _)      => VersionedTrieErrorKind.VersionConflict
    case CommitRewrite(_)           => VersionedTrieErrorKind.CommitRewrite
    case NotRetained(_)             => VersionedTrieErrorKind.NotRetained
    case UnknownVersion(_)          => VersionedTrieErrorKind.UnknownVersion
    case RootMismatch(_, _, _)      => VersionedTrieErrorKind.RootMismatch
    case NotCovered(_)              => VersionedTrieErrorKind.NotCovered
    case Malformed(_)               => VersionedTrieErrorKind.Malformed
    case TooLarge(_, _)             => VersionedTrieErrorKind.TooLarge
    case TooDeep(_, _, _)           => VersionedTrieErrorKind.TooDeep
    case Store(_)                   => VersionedTrieErrorKind.Store
    case Trie(_)                    => VersionedTrieErrorKind.Trie
  }
}

enum VersionKeyFormat { case Configured, UnsignedLong, Bytes }

enum VersionRefusal {
  case Width(format: VersionKeyFormat, actual: Int, maximum: Int)
  case EncodedWidth(expected: Int, actual: Long)
  case ValueRange(value: Long, maximum: Long)
  case EncodedRange(bytes: ByteVector, maximum: Long)

  def message: String = this match {
    case Width(VersionKeyFormat.Configured, actual, maximum)   => s"a version key's width must be 1..$maximum, not $actual"
    case Width(VersionKeyFormat.UnsignedLong, actual, maximum) => s"a Long version key is 1..$maximum bytes, not $actual"
    case Width(VersionKeyFormat.Bytes, actual, maximum)        => s"a bytes version key is 1..$maximum bytes, not $actual"
    case EncodedWidth(expected, actual)                        => s"a version key is $expected bytes, got $actual"
    case ValueRange(value, maximum)                            => s"version $value is outside 0..$maximum"
    case EncodedRange(bytes, maximum)                          => s"version ${bytes.toHex} exceeds $maximum"
  }
}

enum ProofPart {
  case Top, State, History
  def label: String = this match {
    case Top     => "top"
    case State   => "state"
    case History => "history"
  }
}

enum ProofQuery {
  case Key(key: Preview)
  case Namespace(namespace: Preview)
  case StateRange(from: Preview, until: Option[Preview])
  case TopSlot(slot: ByteVector)
  case Version(key: ByteVector)
  case HistoryFromHead
  case Versions(from: ByteVector, until: Option[ByteVector])
  case HistoryThrough(version: ByteVector)

  def message: String = this match {
    case Key(key)                => s"key ${key.short}"
    case Namespace(namespace)    => s"namespace ${namespace.short}"
    case StateRange(from, until) => s"range [${from.short}, ${until.fold("")(_.short)})"
    case TopSlot(slot)           => s"top slot ${slot.toHex}"
    case Version(key)            => s"version ${key.toHex}"
    case HistoryFromHead         => "history from the head version up"
    case Versions(from, until)   => s"versions [${from.toHex}, ${until.fold("")(_.toHex)})"
    case HistoryThrough(version) => s"history up to ${version.toHex}"
  }
}

enum LimitSubject {
  case Proof, Delta, SnapshotChunk, ValueCommitment
  case StateValue(key: Preview)

  def message: String = this match {
    case Proof           => "the proof"
    case Delta           => "the delta"
    case SnapshotChunk   => "a snapshot chunk"
    case ValueCommitment => "the value's commitment"
    case StateValue(key) => s"the value of ${key.short}"
  }
}

enum SnapshotPart {
  case State, History
  def label: String = this match {
    case State   => "state"
    case History => "history"
  }
}

enum SnapshotRefusal {
  case UntrustedHead, MissingInitialHead, RepeatedHead, AfterHeadVersion, StateAfterHistory, WrongHeadStateRoot, IncompleteHistory
  case ChunkSize(part: SnapshotPart, actual: Int, maximum: Int)
  case NullValue(key: Preview)
  case KeysOutOfOrder(previous: Preview, next: Preview)
  case VersionsOutOfOrder(previous: ByteVector, next: ByteVector)
  case VersionAfterHead(version: ByteVector)

  def message: String = this match {
    case UntrustedHead                      => "its head is not the trusted head"
    case MissingInitialHead                 => "it starts with its head"
    case RepeatedHead                       => "it has one head"
    case AfterHeadVersion                   => "nothing follows the head version"
    case StateAfterHistory                  => "state entries precede the history"
    case WrongHeadStateRoot                 => "the head version has another state root"
    case IncompleteHistory                  => "it ends before the head version"
    case ChunkSize(part, actual, maximum)   => s"a ${part.label} chunk holds 1..$maximum items, not $actual"
    case NullValue(key)                     => s"${key.short} is null"
    case KeysOutOfOrder(previous, next)     => s"key ${next.short} does not follow ${previous.short}"
    case VersionsOutOfOrder(previous, next) => s"version ${next.toHex} does not follow ${previous.toHex}"
    case VersionAfterHead(version)          => s"version ${version.toHex} is after the head"
  }
}

enum DigestSubject { case Encoded, Root, StateRoot }

enum MalformedReason {
  case Canonical(cause: CanonicalError)
  case Decode(cause: DecodeFailure)
  case Digest(cause: HashError, subject: DigestSubject = DigestSubject.Encoded)
  case ProofWeight(limit: Long)
  case MissingProof(part: ProofPart)
  case WrongTopSlot(slot: ByteVector)
  case MissingVersion, InvalidHeadHistory, HistoricalProofForCurrent, MissingAsOfStateRoot, WriteRemoveOverlap, DuplicateWrite
  case VersionMismatch(actual: ByteVector, expected: ByteVector)
  case WrongHistoricalStateRoot(version: ByteVector)
  case CommittedVersionClaimedAbsent(version: ByteVector)
  case AbsentVersionClaimedPresent(version: ByteVector)
  case AnswerAfterTarget(answer: ByteVector, target: ByteVector)
  case WrongAsOfHistory(target: ByteVector)
  case NullUpsert(key: Preview)
  case Snapshot(cause: SnapshotRefusal)

  def message: String = this match {
    case Canonical(cause)                       => cause.toString
    case Decode(cause)                          => cause.toString
    case Digest(cause, DigestSubject.Encoded)   => cause.toString
    case Digest(cause, DigestSubject.Root)      => s"root: $cause"
    case Digest(cause, DigestSubject.StateRoot) => s"a state root: $cause"
    case ProofWeight(limit)                     => s"the input weighs over $limit"
    case MissingProof(part)                     => s"the proof has no ${part.label} proof"
    case WrongTopSlot(slot)                     => s"top slot ${slot.toHex} does not hold the head's value"
    case MissingVersion                         => "the proof names no version"
    case InvalidHeadHistory                     => "the history trie does not end at the head version with the head's state root"
    case HistoricalProofForCurrent              => "a historical proof is not about the current state"
    case MissingAsOfStateRoot                   => "an as-of answer names its state root"
    case WriteRemoveOverlap                     => "a key is both written and removed"
    case DuplicateWrite                         => "a key is written twice"
    case VersionMismatch(actual, expected)      => s"the proof is about version ${actual.toHex}, not ${expected.toHex}"
    case WrongHistoricalStateRoot(version)      => s"history holds another state root for ${version.toHex}"
    case CommittedVersionClaimedAbsent(version) => s"version ${version.toHex} is committed; the proof claims it is not"
    case AbsentVersionClaimedPresent(version)   => s"version ${version.toHex} is not committed; the proof claims a state"
    case AnswerAfterTarget(answer, target)      => s"the answer ${answer.toHex} is after ${target.toHex}"
    case WrongAsOfHistory(target)               => s"the history up to ${target.toHex} is not the claimed answer"
    case NullUpsert(key)                        => s"${key.short} is upserted as null"
    case Snapshot(cause)                        => s"snapshot: ${cause.message}"
  }
}

/** Full hexadecimal of an already width-checked version key. */
def versionHex(key: ByteVector): VersionKeyHex = key.toHex

def fromTrie(e: TrieError): VersionedTrieError = e match {
  case TrieError.TooLarge(limit) => VersionedTrieError.TooLarge(LimitSubject.Proof, limit)
  case other                     => VersionedTrieError.Trie(other)
}
