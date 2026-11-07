/**
 * The trie's unchecked operations, for this package's own modules (the replica's trie map and the versioned trie),
 * which call them on data they own or have already checked: `trie.ts` fills this table as it loads, from functions and
 * private fields its public API does not expose. Internal: not exported from the package.
 */
import type { Result, Value } from "@kd5ujc/dilf4s-core";

import type * as B from "./bits.js";
import type { KeyMode, Listing, Lookup, TrieError, TrieHashing, TrieProof, VerifiedTrieProof } from "./trie.js";
import type { models } from "@kd5ujc/dilf4s-core";

export interface TrieInternals {
  /** A key's validity in a mode, for a plain key. */
  checkKey(mode: KeyMode, key: Uint8Array): Result<undefined, TrieError>;
  /** A leaf's hash over a segment's bits. */
  leafBits(h: TrieHashing, segment: B.Bits, valueHash: Uint8Array): Uint8Array;
  /** A branch's hash over a segment's bits; absent fields are omitted. */
  branchBits(h: TrieHashing, segment: B.Bits, valueHash: Uint8Array | undefined, left: Uint8Array | undefined, right: Uint8Array | undefined): Uint8Array;
  /** `valueHash(commit(v))` of a canonical value. */
  valueHash(h: TrieHashing, value: Value): Result<Uint8Array, TrieError>;
  /** `valueHash(commit(b))` of plain bytes. */
  bytesHash(h: TrieHashing, b: Uint8Array): Uint8Array;
  /** A proof from decoded records. */
  fromRecords(h: TrieHashing, records: models.trie.TrieProof): Result<TrieProof, TrieError>;
  /** A proof checked against a plain root. */
  check(h: TrieHashing, mode: KeyMode, root: Uint8Array, proof: TrieProof): Result<VerifiedTrieProof, TrieError>;
  /** A plain key's lookup in a verified proof. */
  lookup(v: VerifiedTrieProof, key: Uint8Array): Result<Lookup, TrieError>;
  /** The listing under a plain prefix. */
  prefix(v: VerifiedTrieProof, prefix: Uint8Array): Listing;
  /** The listing of `[from, until)`, plain bounds. */
  range(v: VerifiedTrieProof, from: Uint8Array, until: Uint8Array | undefined): Listing;
  /** The root of a trie of plain, owned entries. */
  trieRoot(h: TrieHashing, mode: KeyMode, entries: readonly (readonly [Uint8Array, Uint8Array])[]): Result<Uint8Array, TrieError>;
  /** Whether `x` is a `TrieProof` made by `trie.ts` (a private-brand check). */
  isProof(x: unknown): x is TrieProof;
  /** A trusted root as a `RootMismatch` names it: its hex when it has the digest's `width`, otherwise its length. */
  rootText(root: Uint8Array, width: number): string;
}

/** Filled by `trie.ts` when it loads, before any of this package's functions can run. */
export const TRIE = {} as TrieInternals;
