/**
 * The versioned trie (docs/specs/versioned-trie.md): state keys and namespaces, version keys, the top trie that binds a
 * head to one root, every check of Scala's `VersionedTrieVerifier`, proof / delta / snapshot-chunk decoding, the
 * chunk-by-chunk snapshot reader against a trusted head, and a replica that follows deltas.
 */
import { bitLength, cbor, describeFailure, err, models, ok, Preview, toHex, type HashAlgorithm, type Result, type Value } from "@kd5ujc/dilf4s-core";

import { ascii, bytesEqual, compareBytes, concatBytes, copyBytes, fromAscii, isAscii } from "./bytes.js";
import { decodeInput } from "./input.js";
import * as trie from "./trie.js";
import { TrieMap } from "./trie-map.js";

/**
 * Why a state key or namespace was rejected: the input as a `Preview`, or none for bytes refused before they were read
 * as text (docs/specs/conformance.md section 8).
 */
export interface StateKeyError {
  readonly input?: Preview;
  readonly reason: string;
}

/**
 * Typed failures of the versioned trie, by Scala's `VersionedTrieError` names. Versions appear as their version keys.
 * The store-only kinds (`Empty`, `VersionConflict`, `CommitRewrite`, `NotRetained`, `Store`) do not arise here.
 */
export type VersionedTrieError =
  | { readonly kind: "Key"; readonly error: StateKeyError }
  | { readonly kind: "InvalidVersion"; readonly reason: string }
  | { readonly kind: "VersionNotIncreasing"; readonly head: Uint8Array; readonly next: Uint8Array }
  | { readonly kind: "UnknownVersion"; readonly version: Uint8Array }
  | { readonly kind: "RootMismatch"; readonly component: string; readonly expected: string; readonly computed: string }
  | { readonly kind: "NotCovered"; readonly what: string }
  | { readonly kind: "Malformed"; readonly reason: string }
  | { readonly kind: "TooLarge"; readonly what: string; readonly limit: number }
  | { readonly kind: "TooDeep"; readonly what: string; readonly depth: number; readonly limit: number }
  | { readonly kind: "Trie"; readonly error: trie.TrieError };

type R<T> = Result<T, VersionedTrieError>;

const malformed = (reason: string): R<never> => err({ kind: "Malformed", reason });
const notCovered = (what: string): R<never> => err({ kind: "NotCovered", what });
const viaTrie = <T>(r: Result<T, trie.TrieError>): R<T> => (r.ok ? r : err({ kind: "Trie", error: r.error }));
// a trusted root of the wrong width is the caller's: named by its length, never hex-encoded whole (conformance.md section 8)
const mismatch = (component: string, expected: Uint8Array, computed: Uint8Array): R<never> =>
  err({ kind: "RootMismatch", component, expected: trie.rootText(expected, computed.length), computed: toHex(computed) });

// ------------------------------------------------------------------ keys (section 2)

export const MAX_SEGMENT_LENGTH = models.limits.STATE_KEY_MAX_SEGMENT_LENGTH;
export const MAX_SEGMENTS = models.limits.STATE_KEY_MAX_SEGMENTS;
export const MAX_KEY_LENGTH = models.limits.STATE_KEY_MAX_LENGTH;

const SEGMENT = /^[a-z0-9][a-z0-9._-]*$/;

/** A state key: `segment *("/" segment)`, each segment `[a-z0-9][a-z0-9._-]*`, within the length limits. */
export class StateKey {
  private constructor(readonly value: string) {
    Object.freeze(this);
  }

  /**
   * The length is checked before the text is split, so a refusal costs O(MAX_KEY_LENGTH) whatever the input; it names a
   * segment by its position, never its text.
   */
  static parse(s: string): Result<StateKey, StateKeyError> {
    const bad = (reason: string): Result<never, StateKeyError> => err({ input: Preview.of(s), reason });
    if (s.length === 0) return bad("empty key");
    // refused by its length alone: a preview would measure the whole text
    if (s.length > MAX_KEY_LENGTH) return err({ reason: `${s.length.toString()} characters, longer than ${MAX_KEY_LENGTH.toString()}` });
    const segments = s.split("/");
    if (segments.length > MAX_SEGMENTS) return bad(`more than ${MAX_SEGMENTS.toString()} segments`);
    const at = segments.findIndex((x) => x.length > MAX_SEGMENT_LENGTH || !SEGMENT.test(x));
    if (at < 0) return ok(new StateKey(s));
    const where = `segment ${at.toString()}`;
    return (segments[at]?.length ?? 0) > MAX_SEGMENT_LENGTH ? bad(`${where} is longer than ${MAX_SEGMENT_LENGTH.toString()} characters`) : bad(`${where} does not match [a-z0-9][a-z0-9._-]*`);
  }

  /**
   * The key of trie-key bytes; the grammar is ASCII, so other bytes are refused. Bytes longer than `MAX_KEY_LENGTH` are
   * refused by their length, before they are read.
   */
  static fromBytes(bytes: Uint8Array): Result<StateKey, StateKeyError> {
    if (bytes.length > MAX_KEY_LENGTH) return err({ reason: `${bytes.length.toString()} bytes, longer than ${MAX_KEY_LENGTH.toString()}` });
    return isAscii(bytes) ? StateKey.parse(fromAscii(bytes)) : err({ reason: "not an ASCII state key" });
  }

  /** The trie key: the UTF-8 (here ASCII) bytes. */
  bytes(): Uint8Array {
    return ascii(this.value);
  }

  /** The key as a diagnostic names it (docs/specs/conformance.md section 8). */
  preview(): Preview {
    return Preview.of(this.value);
  }

  toString(): string {
    return this.value;
  }
}

/** A namespace `ns`, naming the keys below `ns/`; its prefix is `utf8(ns + "/")`. */
export class StateNamespace {
  private constructor(readonly value: string) {
    Object.freeze(this);
  }

  static readonly MAX_LENGTH = models.limits.STATE_NAMESPACE_MAX_LENGTH;

  static parse(s: string): Result<StateNamespace, StateKeyError> {
    const k = StateKey.parse(s);
    if (!k.ok) return k;
    const slashes = s.split("/").length - 1;
    return s.length <= StateNamespace.MAX_LENGTH && slashes < MAX_SEGMENTS - 1 ? ok(new StateNamespace(s)) : err({ input: Preview.of(s), reason: "a namespace must leave room for one more segment" });
  }

  prefix(): Uint8Array {
    return ascii(`${this.value}/`);
  }

  /** The namespace as a diagnostic names it (docs/specs/conformance.md section 8). */
  preview(): Preview {
    return Preview.of(this.value);
  }

  contains(key: StateKey): boolean {
    return key.value.startsWith(`${this.value}/`);
  }

  toString(): string {
    return this.value;
  }
}

// ------------------------------------------------------------------ version keys

/**
 * How a version becomes a history-trie key: a fixed `width` and an unsigned big-endian encoding, so that comparing
 * versions and comparing their keys as unsigned bytes always agree. `width` is part of what a verifier trusts.
 */
export interface VersionKey<V> {
  readonly width: number;
  encode(version: V): R<Uint8Array>;
  decode(bytes: Uint8Array): R<V>;
}

function bigEndian(width: number, max: bigint): VersionKey<bigint> {
  return Object.freeze({
    width,
    encode: (v: bigint): R<Uint8Array> => {
      if (v < 0n || v > max) return err({ kind: "InvalidVersion", reason: `version of ${bitLength(v).toString()} bits is outside 0..${max.toString()}` });
      const out = new Uint8Array(width);
      let x = v;
      for (let i = width - 1; i >= 0; i--) {
        out[i] = Number(x & 0xffn);
        x >>= 8n;
      }
      return ok(out);
    },
    decode: (b: Uint8Array): R<bigint> => {
      if (b.length !== width) return err({ kind: "InvalidVersion", reason: `a version key is ${width.toString()} bytes, got ${b.length.toString()}` });
      let v = 0n;
      for (const x of b) v = (v << 8n) | BigInt(x);
      return v <= max ? ok(v) : err({ kind: "InvalidVersion", reason: `version ${toHex(b)} exceeds ${max.toString()}` });
    },
  });
}

export const VersionKey = {
  /**
   * Scala's `VersionKey.unsignedLong`: a non-negative 64-bit signed integer as `width` (1..8) bytes. At width 8 the
   * largest version is `2^63 - 1`, as for a JVM `Long`.
   */
  unsignedLong(width: number): R<VersionKey<bigint>> {
    if (!Number.isInteger(width) || width < 1 || width > 8) return err({ kind: "InvalidVersion", reason: `a Long version key is 1..8 bytes, not ${width.toString()}` });
    return ok(bigEndian(width, width === 8 ? 2n ** 63n - 1n : 2n ** BigInt(8 * width) - 1n));
  },

  /** The default, Scala's `VersionKey.long`: 8 bytes, `0..2^63 - 1` (the JLVM's `int_to_bytes [v, 8]`). */
  long: bigEndian(8, 2n ** 63n - 1n),
} as const;

// ------------------------------------------------------------------ the model

/** What one root commits to: the head version, the state root at it, and the history root. */
export interface Head<V> {
  readonly version: V;
  readonly stateRoot: Uint8Array;
  readonly historyRoot: Uint8Array;
}

/** A version asked about, with its state root if it was committed. */
export interface At<V> {
  readonly version: V;
  readonly stateRoot?: Uint8Array;
}

/** A decoded proof (section 5): the head, the whole top trie, and optional history and state parts. */
export interface VersionedTrieProof<V> {
  readonly head: Head<V>;
  readonly top: trie.TrieProof;
  readonly at?: At<V>;
  readonly history?: trie.TrieProof;
  readonly state?: trie.TrieProof;
}

/** One version's change: the head before it (none for the first commit), the head after it, and the writes. */
export interface Delta<V> {
  readonly parent?: Head<V>;
  readonly head: Head<V>;
  readonly upserts: readonly (readonly [StateKey, Value])[];
  readonly removes: readonly StateKey[];
}

/** One snapshot chunk (section 7). */
export type SnapshotChunk<V> =
  | { readonly kind: "head"; readonly head: Head<V> }
  | { readonly kind: "state"; readonly entries: readonly (readonly [StateKey, Value])[] }
  | { readonly kind: "history"; readonly versions: readonly (readonly [V, Uint8Array])[] };

/** A whole snapshot: the head, every current entry in key order, every committed version in order. */
export interface Snapshot<V> {
  readonly head: Head<V>;
  readonly entries: readonly (readonly [StateKey, Value])[];
  readonly history: readonly (readonly [V, Uint8Array])[];
}

/** The largest value, in canonical CBOR bytes, so that every entry fits one snapshot chunk. */
export const MAX_VALUE_BYTES = models.limits.VERSIONED_MAX_VALUE_BYTES;
/** The deepest value: every record carrying one (a map, an array and a map around it) stays within depth 256. */
export const MAX_VALUE_DEPTH = models.limits.VERSIONED_MAX_VALUE_DEPTH;
export const MAX_CHUNK_ENTRIES = models.limits.VERSIONED_MAX_CHUNK_ENTRIES;
/** The largest snapshot chunk, refused before it is decoded. */
export const MAX_CHUNK_BYTES = models.limits.VERSIONED_MAX_CHUNK_BYTES;

export const VERSION_SLOT = Uint8Array.of(0x00);
export const STATE_SLOT = Uint8Array.of(0x01);
export const HISTORY_SLOT = Uint8Array.of(0x02);

const slotName = (slot: Uint8Array): string => toHex(slot);

/** {@link VersionedTrie.checkValue}, which needs no profile. */
function checkValue(key: StateKey, value: Value): R<undefined> {
  if (value.depth > MAX_VALUE_DEPTH) return err({ kind: "TooDeep", what: `the value of ${key.preview().short()}`, depth: value.depth, limit: MAX_VALUE_DEPTH });
  return cbor.encodedLengthWithin(value, MAX_VALUE_BYTES).ok ? ok(undefined) : err({ kind: "TooLarge", what: `the value of ${key.preview().short()}`, limit: MAX_VALUE_BYTES });
}

/** An entry of a verified listing: a state key and its value hash. */
export type StateEntry = readonly [StateKey, Uint8Array];

let createState!: (p: trie.VerifiedTrieProof, h: trie.TrieHashing) => VerifiedState;

/** A state proof checked against its state root: what it shows about keys, namespaces and ranges. */
export class VerifiedState {
  readonly #proof: trie.VerifiedTrieProof;
  readonly #hashing: trie.TrieHashing;
  private constructor(proof: trie.VerifiedTrieProof, hashing: trie.TrieHashing) {
    this.#proof = proof;
    this.#hashing = hashing;
    Object.freeze(this);
  }

  static {
    createState = (p, h) => new VerifiedState(p, h);
  }

  /** The key's value hash, or `null` if it is proven absent; not covered is an error. */
  lookup(key: StateKey): R<Uint8Array | null> {
    const l = viaTrie(this.#proof.lookup(key.bytes()));
    if (!l.ok) return l;
    switch (l.value.kind) {
      case "present":
        return ok(l.value.valueHash);
      case "absent":
        return ok(null);
      case "notCovered":
        return notCovered(`key ${key.preview().short()}`);
    }
  }

  /**
   * Whether the key holds `value` (`null` or a null value: is absent), as Scala's `VersionedTrieVerifier.holds`: the
   * key is looked up first, and a claimed value is hashed only when the key is present and the value passes
   * {@link VersionedTrie.checkValue}. A value no commit could store is `false`, so a claim built with shared parts
   * costs at most the 1 MiB measure, never its expansion.
   */
  holds(key: StateKey, value: Value | null): R<boolean> {
    const l = this.lookup(key);
    if (!l.ok) return l;
    const claimed = value === null || value.kind === "null" ? null : value;
    if (l.value === null || claimed === null) return ok(l.value === null && claimed === null);
    if (!checkValue(key, claimed).ok) return ok(false);
    const h = this.#hashing.valueHash(claimed);
    return h.ok ? ok(bytesEqual(l.value, h.value)) : viaTrie(h);
  }

  /** Every entry under the namespace, complete. */
  namespace(ns: StateNamespace): R<readonly StateEntry[]> {
    return keyed(`namespace ${ns.preview().short()}`, this.#proof.prefix(ns.prefix()));
  }

  /** Every entry in `[from, until)`, complete; no `until` is unbounded. */
  range(from: StateKey, until?: StateKey): R<readonly StateEntry[]> {
    return keyed(`range [${from.preview().short()}, ${until?.preview().short() ?? ""})`, this.#proof.range(from.bytes(), until?.bytes()));
  }
}

function keyed(what: string, listing: trie.Listing): R<readonly StateEntry[]> {
  if (listing.kind === "notCovered") return notCovered(what);
  const out: StateEntry[] = [];
  for (const e of listing.entries) {
    const k = StateKey.fromBytes(e.key);
    if (!k.ok) return err({ kind: "Key", error: k.error });
    out.push([k.value, e.valueHash]);
  }
  return ok(out);
}

const sameEntries = (listing: readonly trie.TrieEntry[], expected: readonly trie.TrieEntry[]): boolean =>
  listing.length === expected.length && listing.every((x, i) => bytesEqual(x.key, expected[i]?.key ?? new Uint8Array(0)) && bytesEqual(x.valueHash, expected[i]?.valueHash ?? new Uint8Array(0)));

/**
 * A versioned trie's verification profile: the hash algorithm and the version key, both part of what a verifier
 * trusts along with the root. Every check is the one a JLVM program makes with the trie operations (section 9); the
 * verifier supplies every key, so no proof can redirect a query, and "not covered" is an error, never a negative answer.
 */
export class VersionedTrie<V> {
  readonly hashing: trie.TrieHashing;
  readonly topMode: trie.KeyMode;
  readonly stateMode: trie.KeyMode;
  readonly historyMode: trie.KeyMode;
  private constructor(
    algorithm: HashAlgorithm,
    readonly versionKey: VersionKey<V>,
    modes: readonly [trie.KeyMode, trie.KeyMode, trie.KeyMode],
  ) {
    this.hashing = trie.TrieHashing.of(algorithm);
    [this.topMode, this.stateMode, this.historyMode] = modes;
    Object.freeze(this);
  }

  static of<V>(algorithm: HashAlgorithm, versionKey: VersionKey<V>): R<VersionedTrie<V>> {
    const top = viaTrie(trie.KeyMode.fixed(1));
    const state = viaTrie(trie.KeyMode.variable(MAX_KEY_LENGTH));
    const history = viaTrie(trie.KeyMode.fixed(versionKey.width));
    if (!top.ok) return top;
    if (!state.ok) return state;
    if (!history.ok) return history;
    return ok(new VersionedTrie(algorithm, versionKey, [top.value, state.value, history.value]));
  }

  /** The hash a trie commits for a value: `valueHash(commit(v))`; one too large to commit is `Trie.Encoding`. */
  valueHash(value: Value): R<Uint8Array> {
    return viaTrie(this.hashing.valueHash(value));
  }

  /** The hash a trie commits for a byte string: the top trie's slots and the history trie's state roots. */
  bytesHash(b: Uint8Array): Uint8Array {
    return this.hashing.bytesHash(b);
  }

  /**
   * A value every wire format can carry: at most 253 levels deep and 1 MiB of canonical CBOR. The size is measured
   * without encoding, and refused at once when the value's weight (a lower bound on its length) is already over, so a
   * value built with shared parts costs O(1 MiB) at most. A refusal carries the limit, never a measured size.
   */
  checkValue(key: StateKey, value: Value): R<undefined> {
    return checkValue(key, value);
  }

  /** The root a head commits to: the top trie of `0x00 -> vkey`, `0x01 -> stateRoot`, `0x02 -> historyRoot`. */
  root(head: Head<V>): R<Uint8Array> {
    const vkey = this.versionKey.encode(head.version);
    if (!vkey.ok) return vkey;
    return viaTrie(
      trie.trieRoot(this.hashing, this.topMode, [
        [VERSION_SLOT, this.bytesHash(vkey.value)],
        [STATE_SLOT, this.bytesHash(head.stateRoot)],
        [HISTORY_SLOT, this.bytesHash(head.historyRoot)],
      ]),
    );
  }

  /** Whether two heads are equal: the same version key and roots. */
  sameHead(a: Head<V>, b: Head<V>): R<boolean> {
    const ka = this.versionKey.encode(a.version);
    if (!ka.ok) return ka;
    const kb = this.versionKey.encode(b.version);
    if (!kb.ok) return kb;
    return ok(bytesEqual(ka.value, kb.value) && bytesEqual(a.stateRoot, b.stateRoot) && bytesEqual(a.historyRoot, b.historyRoot));
  }

  // ---------------------------------------------------------------- wire forms

  private digest(b: Uint8Array): R<Uint8Array> {
    return b.length === this.hashing.size ? ok(copyBytes(b)) : malformed(`a digest is ${this.hashing.size.toString()} bytes, got ${b.length.toString()}`);
  }

  private fromWireHead(w: models.versionedTrie.VersionedTrieHead): R<Head<V>> {
    const version = this.versionKey.decode(w.v);
    if (!version.ok) return version;
    const s = this.digest(w.s);
    if (!s.ok) return s;
    const h = this.digest(w.h);
    if (!h.ok) return h;
    return ok(Object.freeze({ version: version.value, stateRoot: s.value, historyRoot: h.value }));
  }

  /** A head from its canonical value (`versioned-trie-head`); one heavier than `MAX_PROOF_WEIGHT` is `Malformed`. */
  decodeHead(value: Value): R<Head<V>> {
    const w = decodeInput(models.versionedTrie.VersionedTrieHead, value);
    return w.ok ? this.fromWireHead(w.value) : malformed(describeFailure(w.error));
  }

  /** A head's canonical value. */
  encodeHead(head: Head<V>): R<Value> {
    const v = this.versionKey.encode(head.version);
    if (!v.ok) return v;
    const e = models.versionedTrie.VersionedTrieHead.encode({ v: v.value, s: head.stateRoot, h: head.historyRoot });
    return e.ok ? e : malformed(e.error.kind);
  }

  /** A proof from its canonical value (`versioned-trie-proof`); one heavier than `MAX_PROOF_WEIGHT` is `Malformed`. */
  decodeProof(value: Value): R<VersionedTrieProof<V>> {
    const w = decodeInput(models.versionedTrie.VersionedTrieProof, value);
    if (!w.ok) return malformed(describeFailure(w.error));
    const head = this.fromWireHead(w.value.head);
    if (!head.ok) return head;
    const top = viaTrie(trie.TrieProof.fromRecords(this.hashing, w.value.top));
    if (!top.ok) return top;
    let at: At<V> | undefined;
    if (w.value.at !== undefined) {
      const version = this.versionKey.decode(w.value.at.v);
      if (!version.ok) return version;
      const s = w.value.at.s === undefined ? ok(undefined) : this.digest(w.value.at.s);
      if (!s.ok) return s;
      at = Object.freeze(s.value === undefined ? { version: version.value } : { version: version.value, stateRoot: s.value });
    }
    const part = (records: models.trie.TrieProof | undefined): R<trie.TrieProof | undefined> => (records === undefined ? ok(undefined) : viaTrie(trie.TrieProof.fromRecords(this.hashing, records)));
    const history = part(w.value.history);
    if (!history.ok) return history;
    const state = part(w.value.state);
    if (!state.ok) return state;
    return ok(
      Object.freeze({
        head: head.value,
        top: top.value,
        ...(at === undefined ? {} : { at }),
        ...(history.value === undefined ? {} : { history: history.value }),
        ...(state.value === undefined ? {} : { state: state.value }),
      }),
    );
  }

  /**
   * A delta from its canonical value (`versioned-trie-delta`); one heavier than `MAX_PROOF_WEIGHT` is `Malformed`,
   * refused before it is decoded (its values are its own, so the whole delta is bounded).
   */
  decodeDelta(value: Value): R<Delta<V>> {
    const w = decodeInput(models.versionedTrie.VersionedTrieDelta, value);
    if (!w.ok) return malformed(describeFailure(w.error));
    const parent = w.value.parent === undefined ? ok(undefined) : this.fromWireHead(w.value.parent);
    if (!parent.ok) return parent;
    const head = this.fromWireHead(w.value.head);
    if (!head.ok) return head;
    const upserts: (readonly [StateKey, Value])[] = [];
    for (const u of w.value.upserts) {
      const k = StateKey.parse(u.k);
      if (!k.ok) return err({ kind: "Key", error: k.error });
      upserts.push([k.value, u.v]);
    }
    const removes: StateKey[] = [];
    for (const r of w.value.removes) {
      const k = StateKey.parse(r);
      if (!k.ok) return err({ kind: "Key", error: k.error });
      removes.push(k.value);
    }
    return ok(Object.freeze({ ...(parent.value === undefined ? {} : { parent: parent.value }), head: head.value, upserts, removes }));
  }

  /** A snapshot chunk from canonical CBOR, refused by size before it is decoded; order is the reader's to check. */
  decodeChunk(bytes: Uint8Array): R<SnapshotChunk<V>> {
    if (bytes.length > MAX_CHUNK_BYTES) return err({ kind: "TooLarge", what: "a snapshot chunk", limit: MAX_CHUNK_BYTES });
    const value = cbor.decode(bytes);
    if (!value.ok) return malformed(value.error.kind);
    const w = models.versionedTrie.VersionedTrieSnapshotChunk.decode(value.value);
    if (!w.ok) return malformed(describeFailure(w.error));
    const c = w.value;
    if ("head" in c) {
      const head = this.fromWireHead(c.head);
      return head.ok ? ok({ kind: "head", head: head.value }) : head;
    }
    if ("state" in c) {
      const entries: (readonly [StateKey, Value])[] = [];
      for (const u of c.state) {
        const k = StateKey.parse(u.k);
        if (!k.ok) return err({ kind: "Key", error: k.error });
        entries.push([k.value, u.v]);
      }
      return ok({ kind: "state", entries });
    }
    const versions: (readonly [V, Uint8Array])[] = [];
    for (const x of c.history) {
      const v = this.versionKey.decode(x.v);
      if (!v.ok) return v;
      const s = this.digest(x.s);
      if (!s.ok) return s;
      versions.push([v.value, s.value]);
    }
    return ok({ kind: "history", versions });
  }

  // ---------------------------------------------------------------- verification (section 6)

  private verified(mode: trie.KeyMode, root: Uint8Array, what: string, proof: trie.TrieProof | undefined): R<trie.VerifiedTrieProof> {
    return proof === undefined ? malformed(`the proof has no ${what} proof`) : viaTrie(trie.check(this.hashing, mode, root, proof));
  }

  /** Rule 1: the head binds to `root`, and the proof's top trie holds the head's values in slots 0x00..0x02. */
  private bind(root: Uint8Array, proof: VersionedTrieProof<V>): R<Head<V>> {
    const claimed = this.root(proof.head);
    if (!claimed.ok) return claimed;
    if (!bytesEqual(claimed.value, root)) return mismatch("root", root, claimed.value);
    const top = this.verified(this.topMode, root, "top", proof.top);
    if (!top.ok) return top;
    const vkey = this.versionKey.encode(proof.head.version);
    if (!vkey.ok) return vkey;
    const slots: readonly (readonly [Uint8Array, Uint8Array])[] = [
      [VERSION_SLOT, vkey.value],
      [STATE_SLOT, proof.head.stateRoot],
      [HISTORY_SLOT, proof.head.historyRoot],
    ];
    for (const [slot, value] of slots) {
      const l = viaTrie(top.value.lookup(slot));
      if (!l.ok) return l;
      if (l.value.kind === "notCovered") return notCovered(`top slot ${slotName(slot)}`);
      if (l.value.kind !== "present" || !bytesEqual(l.value.valueHash, this.bytesHash(value))) return malformed(`top slot ${slotName(slot)} does not hold the head's value`);
    }
    return ok(proof.head);
  }

  private history(head: Head<V>, proof: VersionedTrieProof<V>): R<trie.VerifiedTrieProof> {
    return this.verified(this.historyMode, head.historyRoot, "history", proof.history);
  }

  private asked(proof: VersionedTrieProof<V>, u: V): R<At<V>> {
    const at = proof.at;
    if (at === undefined) return malformed("the proof names no version");
    const a = this.versionKey.encode(at.version);
    if (!a.ok) return a;
    const b = this.versionKey.encode(u);
    if (!b.ok) return b;
    return bytesEqual(a.value, b.value) ? ok(at) : malformed(`the proof is about version ${toHex(a.value)}, not ${toHex(b.value)}`);
  }

  /** The state root of `u` per the history trie: the root if committed, `null` if proven never committed. */
  private versionRoot(head: Head<V>, proof: VersionedTrieProof<V>, u: V): R<Uint8Array | null> {
    const at = this.asked(proof, u);
    if (!at.ok) return at;
    const hist = this.history(head, proof);
    if (!hist.ok) return hist;
    const vkey = this.versionKey.encode(u);
    if (!vkey.ok) return vkey;
    const l = viaTrie(hist.value.lookup(vkey.value));
    if (!l.ok) return l;
    const k = toHex(vkey.value);
    const s = at.value.stateRoot;
    switch (l.value.kind) {
      case "notCovered":
        return notCovered(`version ${k}`);
      case "present":
        if (s === undefined) return malformed(`version ${k} is committed; the proof claims it is not`);
        return bytesEqual(l.value.valueHash, this.bytesHash(s)) ? ok(s) : malformed(`history holds another state root for ${k}`);
      case "absent":
        return s === undefined ? ok(null) : malformed(`version ${k} is not committed; the proof claims a state`);
    }
  }

  /**
   * Rule 7: the head bound to `root`, with the head invariant: the history holds the head version with the head's
   * state root, and nothing after it. The history part is a range proof from the head version up.
   */
  verifyHead(root: Uint8Array, proof: VersionedTrieProof<V>): R<Head<V>> {
    const head = this.bind(root, proof);
    if (!head.ok) return head;
    const hist = this.history(head.value, proof);
    if (!hist.ok) return hist;
    const vkey = this.versionKey.encode(head.value.version);
    if (!vkey.ok) return vkey;
    const tail = hist.value.range(vkey.value);
    if (tail.kind === "notCovered") return notCovered("history from the head version up");
    return sameEntries(tail.entries, [{ key: vkey.value, valueHash: this.bytesHash(head.value.stateRoot) }])
      ? head
      : malformed("the history trie does not end at the head version with the head's state root");
  }

  /** Rule 2: a proof about the current state, checked against the head's state root. */
  verifyState(root: Uint8Array, proof: VersionedTrieProof<V>): R<VerifiedState> {
    const head = this.bind(root, proof);
    if (!head.ok) return head;
    if (proof.at !== undefined) return malformed("a historical proof is not about the current state");
    const state = this.verified(this.stateMode, head.value.stateRoot, "state", proof.state);
    return state.ok ? ok(createState(state.value, this.hashing)) : state;
  }

  /** Rule 3: version `u`'s state root if the head's history committed it, `null` if it provably never did. */
  verifyVersion(root: Uint8Array, proof: VersionedTrieProof<V>, u: V): R<Uint8Array | null> {
    const head = this.bind(root, proof);
    return head.ok ? this.versionRoot(head.value, proof, u) : head;
  }

  /** Rule 4: the state of a past version `u`, its root taken from the history trie. */
  verifyHistoricalKey(root: Uint8Array, proof: VersionedTrieProof<V>, u: V): R<VerifiedState> {
    const head = this.bind(root, proof);
    if (!head.ok) return head;
    const uk = this.versionKey.encode(u);
    if (!uk.ok) return uk;
    const s = this.versionRoot(head.value, proof, u);
    if (!s.ok) return s;
    if (s.value === null) return err({ kind: "UnknownVersion", version: uk.value });
    const state = this.verified(this.stateMode, s.value, "state", proof.state);
    return state.ok ? ok(createState(state.value, this.hashing)) : state;
  }

  /** Rule 6: whether `claimed` is exactly the committed versions in `[from, until)` with their state roots, in order. */
  verifyVersions(root: Uint8Array, proof: VersionedTrieProof<V>, from: V, until: V | null, claimed: readonly (readonly [V, Uint8Array])[]): R<boolean> {
    const head = this.bind(root, proof);
    if (!head.ok) return head;
    const hist = this.history(head.value, proof);
    if (!hist.ok) return hist;
    const fk = this.versionKey.encode(from);
    if (!fk.ok) return fk;
    const uk = until === null ? ok(undefined) : this.versionKey.encode(until);
    if (!uk.ok) return uk;
    const listing = hist.value.range(fk.value, uk.value);
    if (listing.kind === "notCovered") return notCovered(`versions [${toHex(fk.value)}, ${uk.value === undefined ? "" : toHex(uk.value)})`);
    const expected: trie.TrieEntry[] = [];
    for (const [v, s] of claimed) {
      const k = this.versionKey.encode(v);
      if (!k.ok) return k;
      expected.push({ key: k.value, valueHash: this.bytesHash(s) });
    }
    return ok(sameEntries(listing.entries, expected));
  }

  /**
   * Rule 5: the latest committed version `p <= t` and its state root, or `null` if every committed version is after
   * `t`. The history part lists `[vkey(p), vkey(t) || 0x00)` as exactly `p`.
   */
  verifyAsOf(root: Uint8Array, proof: VersionedTrieProof<V>, t: V): R<{ readonly version: V; readonly stateRoot: Uint8Array } | null> {
    const head = this.bind(root, proof);
    if (!head.ok) return head;
    const hist = this.history(head.value, proof);
    if (!hist.ok) return hist;
    const tkey = this.versionKey.encode(t);
    if (!tkey.ok) return tkey;
    const bound = concatBytes([tkey.value, Uint8Array.of(0)]);
    let found: { readonly pkey: Uint8Array; readonly version: V; readonly stateRoot: Uint8Array } | undefined;
    if (proof.at !== undefined) {
      const pkey = this.versionKey.encode(proof.at.version);
      if (!pkey.ok) return pkey;
      const s = proof.at.stateRoot;
      if (s === undefined) return malformed("an as-of answer names its state root");
      if (compareBytes(pkey.value, tkey.value) > 0) return malformed(`the answer ${toHex(pkey.value)} is after ${toHex(tkey.value)}`);
      found = { pkey: pkey.value, version: proof.at.version, stateRoot: s };
    }
    const listing = hist.value.range(found?.pkey ?? new Uint8Array(0), bound);
    if (listing.kind === "notCovered") return notCovered(`history up to ${toHex(tkey.value)}`);
    const expected = found === undefined ? [] : [{ key: found.pkey, valueHash: this.bytesHash(found.stateRoot) }];
    if (!sameEntries(listing.entries, expected)) return malformed(`the history up to ${toHex(tkey.value)} is not the claimed answer`);
    return ok(found === undefined ? null : { version: found.version, stateRoot: found.stateRoot });
  }
}

// ------------------------------------------------------------------ snapshots (section 7)

type Stage = "head" | "state" | "history" | "done";

/**
 * Checks a snapshot chunk by chunk against a head the caller already trusts, refusing it at the first chunk that
 * breaks a rule: one head chunk first, the trusted head; then state chunks with strictly increasing keys and no null,
 * oversized or overdeep value; then history chunks with strictly increasing versions, none after the trusted version,
 * which ends the snapshot with the trusted state root; each chunk holding 1 to 1024 items. The roots are checked by
 * {@link Replica.fromSnapshot} once the reader has finished.
 */
export class SnapshotReader<V> {
  private constructor(
    readonly profile: VersionedTrie<V>,
    readonly trusted: Head<V>,
    private readonly stage: Stage,
    private readonly lastKey: Uint8Array | undefined,
    private readonly lastVersion: Uint8Array | undefined,
    private readonly stateChunks: readonly (readonly (readonly [StateKey, Value])[])[],
    private readonly historyChunks: readonly (readonly (readonly [V, Uint8Array])[])[],
  ) {
    Object.freeze(this);
  }

  static start<V>(profile: VersionedTrie<V>, trusted: Head<V>): SnapshotReader<V> {
    return new SnapshotReader(profile, trusted, "head", undefined, undefined, [], []);
  }

  private next(changes: {
    readonly stage?: Stage;
    readonly lastKey?: Uint8Array;
    readonly lastVersion?: Uint8Array;
    readonly state?: readonly (readonly [StateKey, Value])[];
    readonly history?: readonly (readonly [V, Uint8Array])[];
  }): SnapshotReader<V> {
    return new SnapshotReader(
      this.profile,
      this.trusted,
      changes.stage ?? this.stage,
      changes.lastKey ?? this.lastKey,
      changes.lastVersion ?? this.lastVersion,
      changes.state === undefined ? this.stateChunks : [...this.stateChunks, changes.state],
      changes.history === undefined ? this.historyChunks : [...this.historyChunks, changes.history],
    );
  }

  /** Decodes and feeds one chunk of canonical CBOR. */
  feedBytes(bytes: Uint8Array): R<SnapshotReader<V>> {
    const c = this.profile.decodeChunk(bytes);
    return c.ok ? this.feed(c.value) : c;
  }

  feed(chunk: SnapshotChunk<V>): R<SnapshotReader<V>> {
    const bad = (reason: string): R<never> => malformed(`snapshot: ${reason}`);
    if (this.stage === "head") {
      if (chunk.kind !== "head") return bad("it starts with its head");
      const same = this.profile.sameHead(chunk.head, this.trusted);
      if (!same.ok) return same;
      return same.value ? ok(this.next({ stage: "state" })) : bad("its head is not the trusted head");
    }
    if (chunk.kind === "head") return bad("it has one head");
    if (this.stage === "state" && chunk.kind === "state") {
      const es = chunk.entries;
      if (es.length < 1 || es.length > MAX_CHUNK_ENTRIES) return bad(`a state chunk holds 1..${MAX_CHUNK_ENTRIES.toString()} items, not ${es.length.toString()}`);
      for (const [k, v] of es) {
        if (v.kind === "null") return bad(`${k.preview().short()} is null`);
        const c = this.profile.checkValue(k, v);
        if (!c.ok) return c;
      }
      let previous = this.lastKey;
      for (const [k] of es) {
        const b = k.bytes();
        if (previous !== undefined && compareBytes(previous, b) >= 0) return bad(`key ${Preview.of(toHex(b)).short()} does not follow ${Preview.of(toHex(previous)).short()}`);
        previous = b;
      }
      return ok(this.next({ ...(previous === undefined ? {} : { lastKey: previous }), state: es }));
    }
    if (this.stage === "done") return bad("nothing follows the head version");
    if (chunk.kind === "state") return bad("state entries precede the history");
    const vs = chunk.versions;
    if (vs.length < 1 || vs.length > MAX_CHUNK_ENTRIES) return bad(`a history chunk holds 1..${MAX_CHUNK_ENTRIES.toString()} items, not ${vs.length.toString()}`);
    const keys: Uint8Array[] = [];
    for (const [u] of vs) {
      const k = this.profile.versionKey.encode(u);
      if (!k.ok) return k;
      keys.push(k.value);
    }
    const hk = this.profile.versionKey.encode(this.trusted.version);
    if (!hk.ok) return hk;
    let previous = this.lastVersion;
    for (const k of keys) {
      if (previous !== undefined && compareBytes(previous, k) >= 0) return bad(`version ${Preview.of(toHex(k)).short()} does not follow ${Preview.of(toHex(previous)).short()}`);
      previous = k;
    }
    for (const [i, k] of keys.entries()) {
      const s = vs[i]?.[1] ?? new Uint8Array(0);
      if (compareBytes(k, hk.value) > 0) return bad(`version ${toHex(k)} is after the head`);
      if (bytesEqual(k, hk.value) && !bytesEqual(s, this.trusted.stateRoot)) return bad("the head version has another state root");
    }
    const last = keys[keys.length - 1];
    return ok(this.next({ stage: last !== undefined && bytesEqual(last, hk.value) ? "done" : "history", ...(last === undefined ? {} : { lastVersion: last }), history: vs }));
  }

  /** The snapshot, once its history has reached the head version. */
  finish(): R<Snapshot<V>> {
    if (this.stage !== "done") return malformed("snapshot: it ends before the head version");
    return ok(Object.freeze({ head: this.trusted, entries: this.stateChunks.flat(), history: this.historyChunks.flat() }));
  }
}

// ------------------------------------------------------------------ replicas

interface StateSlot {
  readonly key: StateKey;
  readonly value: Value;
}

interface VersionSlot<V> {
  readonly version: V;
  readonly stateRoot: Uint8Array;
}

const copyHead = <V>(h: Head<V>): Head<V> =>
  Object.freeze({ version: h.version, stateRoot: Uint8Array.from(h.stateRoot), historyRoot: Uint8Array.from(h.historyRoot) });

const chunked = <T>(xs: readonly T[], n: number): T[][] => {
  const out: T[][] = [];
  for (let i = 0; i < xs.length; i += n) out.push(xs.slice(i, i + n));
  return out;
};

/** Whether sorted `keys` hold one key twice: compared, never hashed. */
const repeats = (keys: readonly Uint8Array[]): boolean => keys.some((k, i) => i > 0 && compareBytes(keys[i - 1] ?? k, k) === 0);

/** Whether sorted `keys` hold `key`, by binary search. */
function holds(keys: readonly Uint8Array[], key: Uint8Array): boolean {
  let [lo, hi] = [0, keys.length];
  while (lo < hi) {
    const mid = (lo + hi) >> 1;
    const c = compareBytes(keys[mid] ?? key, key);
    if (c === 0) return true;
    if (c < 0) lo = mid + 1;
    else hi = mid;
  }
  return false;
}

/**
 * A replica of a versioned trie, held in memory and checked locally (section 7). It starts empty, or from a snapshot
 * checked against a head the caller already trusts, and follows deltas, recomputing both roots, so a forged root, a
 * dropped write or a gap is caught by the replica itself. The state and the history are each one persistent trie that
 * holds the entries and caches every node's hash: a delta re-hashes only the paths of the keys it changes, O(changed
 * keys x key length) whatever the state's size, and shares the rest with the replica it was applied to. No key a peer
 * sends is ever put in a `Map` or `Set`: keys are walked bit by bit, or compared.
 */
export class Replica<V> {
  // the replica owns every root it holds: heads and roots come in from callers and go out as copies, so no caller can
  // change a verified replica through a shared array
  readonly #head: Head<V> | null;

  private constructor(
    readonly profile: VersionedTrie<V>,
    head: Head<V> | null,
    private readonly state: TrieMap<StateSlot>,
    private readonly versions: TrieMap<VersionSlot<V>>,
  ) {
    this.#head = head === null ? null : copyHead(head);
    Object.freeze(this);
  }

  /** The head, or `null` before the first delta (a copy). */
  get head(): Head<V> | null {
    return this.#head === null ? null : copyHead(this.#head);
  }

  static empty<V>(profile: VersionedTrie<V>): Replica<V> {
    return new Replica(profile, null, TrieMap.empty(profile.hashing, profile.stateMode), TrieMap.empty(profile.hashing, profile.historyMode));
  }

  /** The commitment root of the head, or `null` before the first delta. */
  root(): R<Uint8Array | null> {
    return this.#head === null ? ok(null) : this.profile.root(this.#head);
  }

  /** The current value of `key`, if any: one walk down its path. */
  get(key: StateKey): Value | undefined {
    return this.state.get(key.bytes())?.value;
  }

  /** The number of current entries. */
  get size(): number {
    return this.state.size;
  }

  /** Every current entry, in key order. */
  entries(): readonly (readonly [StateKey, Value])[] {
    return this.state.values().map((s) => [s.key, s.value] as const);
  }

  /** Every committed version with its state root, in order. */
  history(): readonly (readonly [V, Uint8Array])[] {
    return this.versions.values().map((s) => [s.version, Uint8Array.from(s.stateRoot)] as const);
  }

  /**
   * A replica of `snapshot`, which must pass every check of {@link SnapshotReader} against `trusted` and reproduce its
   * state and history roots. The snapshot's own head is only a claim.
   */
  static fromSnapshot<V>(profile: VersionedTrie<V>, trusted: Head<V>, snapshot: Snapshot<V>): R<Replica<V>> {
    const chunks: SnapshotChunk<V>[] = [
      { kind: "head", head: trusted },
      ...chunked(snapshot.entries, MAX_CHUNK_ENTRIES).map((entries) => ({ kind: "state" as const, entries })),
      ...chunked(snapshot.history, MAX_CHUNK_ENTRIES).map((versions) => ({ kind: "history" as const, versions })),
    ];
    let reader = SnapshotReader.start(profile, trusted);
    for (const c of chunks) {
      const r = reader.feed(c);
      if (!r.ok) return r;
      reader = r.value;
    }
    const checked = reader.finish();
    if (!checked.ok) return checked;
    // the reader has checked that keys and versions strictly increase, which the builder requires
    const entries: (readonly [Uint8Array, StateSlot, Uint8Array])[] = [];
    for (const [key, value] of checked.value.entries) {
      const h = profile.valueHash(value);
      if (!h.ok) return h;
      entries.push([key.bytes(), { key, value }, h.value]);
    }
    const state = viaTrie(TrieMap.fromSorted(profile.hashing, profile.stateMode, entries));
    if (!state.ok) return state;
    if (!bytesEqual(state.value.root, trusted.stateRoot)) return mismatch("state", trusted.stateRoot, state.value.root);
    const versions: (readonly [Uint8Array, VersionSlot<V>, Uint8Array])[] = [];
    for (const [version, stateRoot] of checked.value.history) {
      const vkey = profile.versionKey.encode(version);
      if (!vkey.ok) return vkey;
      versions.push([vkey.value, { version, stateRoot: Uint8Array.from(stateRoot) }, profile.bytesHash(stateRoot)]);
    }
    const history = viaTrie(TrieMap.fromSorted(profile.hashing, profile.historyMode, versions));
    if (!history.ok) return history;
    if (!bytesEqual(history.value.root, trusted.historyRoot)) return mismatch("history", trusted.historyRoot, history.value.root);
    return ok(new Replica(profile, trusted, state.value, history.value));
  }

  /** Reads a snapshot's chunks of canonical CBOR against `trusted`, then restores a replica from it. */
  static restore<V>(profile: VersionedTrie<V>, trusted: Head<V>, chunks: Iterable<Uint8Array>): R<Replica<V>> {
    let reader = SnapshotReader.start(profile, trusted);
    for (const c of chunks) {
      const r = reader.feedBytes(c);
      if (!r.ok) return r;
      reader = r.value;
    }
    const snapshot = reader.finish();
    return snapshot.ok ? Replica.fromSnapshot(profile, trusted, snapshot.value) : snapshot;
  }

  /**
   * The replica after `delta`, which must chain from this head, raise the version, write no key twice nor both write
   * and remove one, carry only values every wire format can, and whose claimed head must be what the replica computes.
   * This replica is unchanged either way.
   */
  applyDelta(delta: Delta<V>): R<Replica<V>> {
    const p = this.profile;
    const parent = this.root();
    if (!parent.ok) return parent;
    const claimed = delta.parent === undefined ? ok(null) : p.root(delta.parent);
    if (!claimed.ok) return claimed;
    const same = parent.value === null || claimed.value === null ? parent.value === claimed.value : bytesEqual(parent.value, claimed.value);
    if (!same) return err({ kind: "RootMismatch", component: "parent", expected: parent.value === null ? "none" : toHex(parent.value), computed: claimed.value === null ? "none" : toHex(claimed.value) });
    const nk = p.versionKey.encode(delta.head.version);
    if (!nk.ok) return nk;
    if (this.#head !== null) {
      const hk = p.versionKey.encode(this.#head.version);
      if (!hk.ok) return hk;
      if (compareBytes(nk.value, hk.value) <= 0) return err({ kind: "VersionNotIncreasing", head: hk.value, next: nk.value });
    }
    const written = delta.upserts.map(([k]) => k.bytes()).sort(compareBytes);
    if (delta.removes.some((k) => holds(written, k.bytes()))) return malformed("a key is both written and removed");
    if (repeats(written)) return malformed("a key is written twice");
    for (const [k, v] of delta.upserts) {
      if (v.kind === "null") return malformed(`${k.preview().short()} is upserted as null`);
      const c = p.checkValue(k, v);
      if (!c.ok) return c;
    }
    // new tries sharing every untouched subtree with this replica's, which stay as they are
    let state = this.state;
    for (const k of delta.removes) {
      const next = viaTrie(state.delete(k.bytes()));
      if (!next.ok) return next;
      state = next.value;
    }
    for (const [key, value] of delta.upserts) {
      const h = p.valueHash(value);
      if (!h.ok) return h;
      const next = viaTrie(state.set(key.bytes(), { key, value }, h.value));
      if (!next.ok) return next;
      state = next.value;
    }
    const s = state.root;
    if (!bytesEqual(s, delta.head.stateRoot)) return mismatch("state", delta.head.stateRoot, s);
    const versions = viaTrie(this.versions.set(nk.value, { version: delta.head.version, stateRoot: s }, p.bytesHash(s)));
    if (!versions.ok) return versions;
    const h = versions.value.root;
    if (!bytesEqual(h, delta.head.historyRoot)) return mismatch("history", delta.head.historyRoot, h);
    return ok(new Replica(p, delta.head, state, versions.value));
  }
}
