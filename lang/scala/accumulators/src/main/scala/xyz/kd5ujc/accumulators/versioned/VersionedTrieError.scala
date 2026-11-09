package xyz.kd5ujc.accumulators.versioned

import xyz.kd5ujc.accumulators.trie.TrieError

import scodec.bits.ByteVector

/**
 * Typed failures of a versioned trie (docs/specs/versioned-trie.md), raised through `cats.mtl.Raise[F, VersionedTrieError]`
 * or returned in `Either` by the pure verifiers: generated from `shared/schemas/proofs-errors.cddl`, with the Rust and
 * TypeScript SDKs' kinds and fields. Versions appear as their version keys' full lowercase hex ([[VersionKeyHex]],
 * [[versionHex]]), so the type does not depend on the version type; a recomputed root's `component` is a closed
 * [[VtRootComponent]].
 */
export xyz.kd5ujc.models.proofs_errors.{VersionKeyHex, VersionedTrieError, VersionedTrieErrorKind, VtRootComponent}

/**
 * A version key as a refusal names it: its full lowercase hex, never truncated. Every key reaching a refusal has been
 * encoded at the version key's width (at most `Trie.MaxFixedBytes`, the history trie being `fixed(width)`), so the text
 * is at most twice that long (docs/specs/versioned-trie.md §2).
 */
def versionHex(key: ByteVector): VersionKeyHex = key.toHex

/**
 * A trie failure as the versioned trie reports it: a trie proof over `ProofInput.MaxProofWeight`, which no producer
 * emits, is this family's `TooLarge` with that limit (docs/specs/conformance.md §3), whether it was one embedded proof
 * or the whole record that passed the cap; every other trie failure is wrapped as `Trie`.
 */
def fromTrie(e: TrieError): VersionedTrieError = e match {
  case TrieError.TooLarge(limit) => VersionedTrieError.TooLarge("the proof", limit)
  case other                     => VersionedTrieError.Trie(other)
}
