/**
 * The versioned trie (docs/specs/versioned-trie.md): state keys and namespaces, version keys, the top trie that binds a
 * head to one root, every check of Scala's `VersionedTrieVerifier`, proof / delta / snapshot-chunk decoding, the
 * chunk-by-chunk snapshot reader against a trusted head, and a replica that follows deltas.
 *
 * Every exported function and method checks its arguments once before it reads them: an argument of the wrong type is
 * core's `TypeMismatch` (check.ts). A plain object a caller passes (a head, a proof, a delta, a chunk, a snapshot) has
 * each field read once into a frozen copy this module owns, its bytes copied from their own memory; past that check
 * the verifiers work on owned data only. A version key the caller supplies is the caller's code: what it returns is
 * checked before it is used.
 */
import { cbor, decodeUtf8, describeFailure, err, isValue, models, ok, Preview, utf8Length, type HashAlgorithm, type Result, type TypeMismatch, type Value } from "@kd5ujc/dilf4s-core";
import { mint, plainBytes, takeBytes, toHexOf } from "@kd5ujc/dilf4s-core/internal";

import { ascii, bytesEqual, compareBytes, concatBytes, copyBytes, fromAscii, isAscii } from "./bytes.js";
import { algorithmArg, bytesArg, expected, expectedAt, foldIterable, isObject, NOT_A_BIGINT, NOT_A_NUMBER, NOT_A_STRING, readArray, readPair, TOKEN, valueArg, type Token } from "./check.js";
import { bitsOf, decodeInput } from "./input.js";
import * as trie from "./trie.js";
import { TRIE } from "./trie-internal.js";
import { TrieMap } from "./trie-map.js";

/**
 * Why a state key or namespace was refused, in the order of docs/specs/versioned-trie.md section 2: generated from
 * `shared/schemas/codec-errors.cddl` (`models.codecErrors`), with the reference's and the Rust SDK's kinds and fields.
 */
export type StateKeyError = models.codecErrors.StateKeyError;

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

/** A versioned-trie refusal, or an argument of the wrong type: what the exported functions here may refuse with. */
export type VersionedRefusal = VersionedTrieError | TypeMismatch;

type R<T> = Result<T, VersionedTrieError>;
/** The result of a function a caller reaches: its refusals, or a `TypeMismatch`. */
type C<T> = Result<T, VersionedRefusal>;

const malformed = (reason: string): R<never> => err({ kind: "Malformed", reason });
const notCovered = (what: string): R<never> => err({ kind: "NotCovered", what });
const viaTrie = <T>(r: Result<T, trie.TrieError>): R<T> => (r.ok ? r : err({ kind: "Trie", error: r.error }));
// a trusted root of the wrong width is the caller's: named by its length, never hex-encoded whole (conformance.md section 8)
const mismatch = (component: string, expected: Uint8Array, computed: Uint8Array): R<never> =>
  err({ kind: "RootMismatch", component, expected: TRIE.rootText(expected, computed.length), computed: toHexOf(computed) });

// ------------------------------------------------------------------ keys (section 2)

export const MAX_SEGMENT_LENGTH = models.limits.STATE_KEY_MAX_SEGMENT_LENGTH;
export const MAX_SEGMENTS = models.limits.STATE_KEY_MAX_SEGMENTS;
export const MAX_KEY_LENGTH = models.limits.STATE_KEY_MAX_LENGTH;

const SEGMENT = /^[a-z0-9][a-z0-9._-]*$/;

const KEY = mint();
const NAMESPACE = mint();
const PROFILE = mint();

let isKey!: (x: unknown) => x is StateKey;
let createKey!: (s: string) => StateKey;

/** `x` if it is a {@link StateKey} made by `StateKey.parse` or `fromBytes` (a private-brand check). */
const keyArg = (x: unknown): Result<StateKey, TypeMismatch> => (isKey(x) ? ok(x) : err(expected("a StateKey")));

/** A state key: `segment *("/" segment)`, each segment `[a-z0-9][a-z0-9._-]*`, within the length limits. */
export class StateKey {
  private constructor(
    token: Token,
    readonly value: string,
  ) {
    if (token === TOKEN) KEY.stamp(this);
    Object.freeze(this);
  }

  static {
    isKey = (x): x is StateKey => KEY.has(x);
    createKey = (s) => new StateKey(TOKEN, s);
  }

  /**
   * Refusals come in the order of versioned-trie.md section 2: empty; over `MAX_KEY_LENGTH` UTF-8 bytes; over
   * `MAX_SEGMENTS` segments; then each segment in order, its grammar before its length. The length is measured before
   * the text is split, and only within `MAX_KEY_LENGTH` code units (a UTF-8 length is never shorter), so a refusal costs
   * O(MAX_KEY_LENGTH) whatever the input; it names a segment by its position, never its text. What is not a string
   * (from a JavaScript caller, or a cast) is a `TypeMismatch`, never converted.
   */
  static parse(s: string): Result<StateKey, StateKeyError | TypeMismatch> {
    return typeof (s as unknown) === "string" ? parseKey(s) : err(NOT_A_STRING);
  }

  /**
   * The key of trie-key bytes (UTF-8). Bytes longer than `MAX_KEY_LENGTH` are refused by their length, before they are
   * read; then bytes that are not well-formed UTF-8; then {@link StateKey.parse}. What is not a `Uint8Array` is a
   * `TypeMismatch`; the bytes are read through a plain view of their own memory, their own `length` not believed.
   */
  static fromBytes(bytes: Uint8Array): Result<StateKey, StateKeyError | TypeMismatch> {
    const b = bytesArg(bytes);
    return b.ok ? keyFromBytes(b.value) : b;
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

/** {@link StateKey.parse} of a string. */
function parseKey(s: string): Result<StateKey, StateKeyError> {
  if (s.length === 0) return err({ kind: "Empty" });
  // refused by its length alone: a preview would measure the whole text
  if (s.length > MAX_KEY_LENGTH) return err({ kind: "TooLong", limit: MAX_KEY_LENGTH });
  const utf8 = utf8Length(s);
  if (!utf8.ok || utf8.value > MAX_KEY_LENGTH) return err({ kind: "TooLong", limit: MAX_KEY_LENGTH });
  const segments = s.split("/");
  if (segments.length > MAX_SEGMENTS) return err({ kind: "TooManySegments", key: Preview.of(s), limit: MAX_SEGMENTS });
  // the grammar is ASCII, so a segment that passes it has as many characters as bytes
  const at = segments.findIndex((x) => !SEGMENT.test(x) || x.length > MAX_SEGMENT_LENGTH);
  if (at < 0) return ok(createKey(s));
  return SEGMENT.test(segments[at] ?? "")
    ? err({ kind: "SegmentTooLong", key: Preview.of(s), segment: at, limit: MAX_SEGMENT_LENGTH })
    : err({ kind: "InvalidSegment", key: Preview.of(s), segment: at });
}

/** {@link StateKey.fromBytes} of plain bytes. */
function keyFromBytes(bytes: Uint8Array): Result<StateKey, StateKeyError> {
  if (bytes.length > MAX_KEY_LENGTH) return err({ kind: "TooLong", limit: MAX_KEY_LENGTH });
  if (isAscii(bytes)) return parseKey(fromAscii(bytes));
  const text = decodeUtf8(bytes);
  return text === undefined ? err({ kind: "NotUtf8" }) : parseKey(text);
}

let isNamespace!: (x: unknown) => x is StateNamespace;

/** A namespace `ns`, naming the keys below `ns/`; its prefix is `utf8(ns + "/")`. */
export class StateNamespace {
  private constructor(
    token: Token,
    readonly value: string,
  ) {
    if (token === TOKEN) NAMESPACE.stamp(this);
    Object.freeze(this);
  }

  static {
    isNamespace = (x): x is StateNamespace => NAMESPACE.has(x);
  }

  static readonly MAX_LENGTH = models.limits.STATE_NAMESPACE_MAX_LENGTH;

  /** A namespace: a state key leaving room for one more segment (`NamespaceFull`); what is not a string is a `TypeMismatch`. */
  static parse(s: string): Result<StateNamespace, StateKeyError | TypeMismatch> {
    if (typeof (s as unknown) !== "string") return err(NOT_A_STRING);
    const k = parseKey(s);
    if (!k.ok) return k;
    const slashes = s.split("/").length - 1;
    return s.length <= StateNamespace.MAX_LENGTH && slashes < MAX_SEGMENTS - 1 ? ok(new StateNamespace(TOKEN, s)) : err({ kind: "NamespaceFull", namespace: Preview.of(s) });
  }

  prefix(): Uint8Array {
    return ascii(`${this.value}/`);
  }

  /** The namespace as a diagnostic names it (docs/specs/conformance.md section 8). */
  preview(): Preview {
    return Preview.of(this.value);
  }

  /** Whether `key` lies below this namespace; what is not a `StateKey` is a `TypeMismatch`. */
  contains(key: StateKey): Result<boolean, TypeMismatch> {
    const k = keyArg(key);
    return k.ok ? ok(k.value.value.startsWith(`${this.value}/`)) : k;
  }

  toString(): string {
    return this.value;
  }
}

// ------------------------------------------------------------------ version keys

/**
 * How a version becomes a history-trie key: a fixed `width` and an unsigned big-endian encoding, so that comparing
 * versions and comparing their keys as unsigned bytes always agree. `width` is part of what a verifier trusts. A
 * version key may be the caller's own; this module checks what it gives (a `Result`, and bytes of `width`) before using
 * it, as a `TypeMismatch` otherwise.
 */
export interface VersionKey<V> {
  readonly width: number;
  encode(version: V): C<Uint8Array>;
  decode(bytes: Uint8Array): C<V>;
}

/** The version keys this module made: their results need no check. */
const OWN_KEYS = new WeakSet();

function bigEndian(width: number, max: bigint): VersionKey<bigint> {
  const key: VersionKey<bigint> = Object.freeze({
    width,
    encode: (v: bigint): C<Uint8Array> => {
      if (typeof (v as unknown) !== "bigint") return err(NOT_A_BIGINT);
      if (v < 0n || v > max) return err({ kind: "InvalidVersion", reason: `version of ${bitsOf(v).toString()} bits is outside 0..${max.toString()}` });
      const out = new Uint8Array(width);
      let x = v;
      for (let i = width - 1; i >= 0; i--) {
        out[i] = Number(x & 0xffn);
        x >>= 8n;
      }
      return ok(out);
    },
    decode: (bytes: Uint8Array): C<bigint> => {
      const b = plainBytes(bytes);
      if (b === undefined) return err(expected("a Uint8Array"));
      if (b.length !== width) return err({ kind: "InvalidVersion", reason: `a version key is ${width.toString()} bytes, got ${b.length.toString()}` });
      let v = 0n;
      for (const x of b) v = (v << 8n) | BigInt(x);
      return v <= max ? ok(v) : err({ kind: "InvalidVersion", reason: `version ${toHexOf(b)} exceeds ${max.toString()}` });
    },
  });
  OWN_KEYS.add(key);
  return key;
}

export const VersionKey = {
  /**
   * Scala's `VersionKey.unsignedLong`: a non-negative 64-bit signed integer as `width` (1..8) bytes. At width 8 the
   * largest version is `2^63 - 1`, as for a JVM `Long`. A width that is not a number is a `TypeMismatch`.
   */
  unsignedLong(width: number): C<VersionKey<bigint>> {
    if (typeof (width as unknown) !== "number") return err(NOT_A_NUMBER);
    if (!Number.isInteger(width) || width < 1 || width > 8) return err({ kind: "InvalidVersion", reason: `a Long version key is 1..8 bytes, not ${width.toString()}` });
    return ok(bigEndian(width, width === 8 ? 2n ** 63n - 1n : 2n ** BigInt(8 * width) - 1n));
  },

  /** The default, Scala's `VersionKey.long`: 8 bytes, `0..2^63 - 1` (the JLVM's `int_to_bytes [v, 8]`). */
  long: bigEndian(8, 2n ** 63n - 1n),
} as const;

/** What a caller's version key method gave, as a result this module can trust: a `Result`, its error passed on. */
function keyResult<T>(r: unknown, value: (v: unknown) => T | undefined): C<T> {
  const bad = expected("a version key's result to be a Result of its type");
  if (!isObject(r)) return err(bad);
  const isOk: unknown = Reflect.get(r, "ok");
  if (isOk === true) {
    const v = value(Reflect.get(r, "value"));
    return v === undefined ? err(bad) : ok(v);
  }
  if (isOk !== false) return err(bad);
  const e: unknown = Reflect.get(r, "error");
  return isObject(e) && typeof Reflect.get(e, "kind") === "string" ? err(e as VersionedRefusal) : err(bad);
}

/**
 * A caller's version key, checked once (an object whose `width` is an integer from 1 to `MAX_KEY_LENGTH` and whose
 * `encode` and `decode` are functions, each read once), as a frozen key whose every result is checked: an encoding
 * must be bytes of `width` (copied), a decoding any value. This module's own keys are used as they are.
 */
function versionKeyArg<V>(x: unknown): Result<VersionKey<V>, TypeMismatch> {
  if (isObject(x) && OWN_KEYS.has(x)) return ok(x as VersionKey<V>);
  const bad = expected("a VersionKey");
  if (!isObject(x)) return err(bad);
  const width: unknown = Reflect.get(x, "width");
  const encode: unknown = Reflect.get(x, "encode");
  const decode: unknown = Reflect.get(x, "decode");
  if (typeof width !== "number" || !Number.isInteger(width) || width < 1 || width > MAX_KEY_LENGTH) return err(bad);
  if (typeof encode !== "function" || typeof decode !== "function") return err(bad);
  return ok(
    Object.freeze({
      width,
      encode: (v: V): C<Uint8Array> =>
        keyResult(Reflect.apply(encode, x, [v]), (b) => {
          const own = takeBytes(b);
          return own?.length === width ? own : undefined;
        }),
      decode: (b: Uint8Array): C<V> => keyResult(Reflect.apply(decode, x, [copyBytes(b)]), (v) => v as V),
    }),
  );
}

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

/** The top trie's slots (copies: the module keeps its own, which no caller can change). */
export const VERSION_SLOT = Uint8Array.of(0x00);
export const STATE_SLOT = Uint8Array.of(0x01);
export const HISTORY_SLOT = Uint8Array.of(0x02);
const SLOTS = { version: Uint8Array.of(0x00), state: Uint8Array.of(0x01), history: Uint8Array.of(0x02) } as const;

const slotName = (slot: Uint8Array): string => toHexOf(slot);

/** A caller's head, its fields read once into a frozen copy: the version (checked when encoded) and two roots (copied). */
function headArg<V>(x: unknown, what: string): Result<Head<V>, TypeMismatch> {
  if (!isObject(x)) return err(expected(`${what} to be a Head`));
  const version: unknown = Reflect.get(x, "version");
  const stateRoot = takeBytes(Reflect.get(x, "stateRoot"));
  const historyRoot = takeBytes(Reflect.get(x, "historyRoot"));
  if (stateRoot === undefined || historyRoot === undefined) return err(expected(`${what}'s roots to be Uint8Arrays`));
  return ok(Object.freeze({ version: version as V, stateRoot, historyRoot }));
}

/** An optional `TrieProof` field: absent (`undefined`), or one made by `trie.ts`. */
function optionalProof(x: unknown, what: string): Result<trie.TrieProof | undefined, TypeMismatch> {
  return x === undefined ? ok(undefined) : TRIE.isProof(x) ? ok(x) : err(expected(`${what} to be a TrieProof`));
}

/** A caller's proof, its fields read once into a frozen copy: a head, the top `TrieProof`, and the optional parts. */
function proofArg<V>(x: unknown): Result<VersionedTrieProof<V>, TypeMismatch> {
  if (!isObject(x)) return err(expected("a VersionedTrieProof"));
  const head = headArg<V>(Reflect.get(x, "head"), "the proof's head");
  if (!head.ok) return head;
  const top: unknown = Reflect.get(x, "top");
  if (!TRIE.isProof(top)) return err(expected("the proof's top to be a TrieProof"));
  const history = optionalProof(Reflect.get(x, "history"), "the proof's history");
  if (!history.ok) return history;
  const state = optionalProof(Reflect.get(x, "state"), "the proof's state");
  if (!state.ok) return state;
  const atField: unknown = Reflect.get(x, "at");
  let at: At<V> | undefined;
  if (atField !== undefined) {
    if (!isObject(atField)) return err(expected("the proof's at to be an At"));
    const version: unknown = Reflect.get(atField, "version");
    const s: unknown = Reflect.get(atField, "stateRoot");
    const stateRoot = s === undefined ? undefined : takeBytes(s);
    if (s !== undefined && stateRoot === undefined) return err(expected("the proof's at.stateRoot to be a Uint8Array"));
    at = Object.freeze(stateRoot === undefined ? { version: version as V } : { version: version as V, stateRoot });
  }
  return ok(
    Object.freeze({
      head: head.value,
      top,
      ...(at === undefined ? {} : { at }),
      ...(history.value === undefined ? {} : { history: history.value }),
      ...(state.value === undefined ? {} : { state: state.value }),
    }),
  );
}

/** A caller's entries: an array of `[StateKey, Value]` arrays, read by index, each item once. */
const entriesArg = (x: unknown, what: string): Result<(readonly [StateKey, Value])[], TypeMismatch> =>
  readArray(x, what, (e, i) => {
    const pair = readPair(e);
    if (pair === undefined) return err(expectedAt("a [StateKey, Value] array", i));
    const [k, v] = pair;
    return isKey(k) && isValue(v) ? ok([k, v] as const) : err(expectedAt("a [StateKey, Value] array", i));
  });

/** A caller's versions: an array of `[V, Uint8Array]` arrays, read by index, each root copied. */
const versionsArg = <V>(x: unknown, what: string): Result<(readonly [V, Uint8Array])[], TypeMismatch> =>
  readArray(x, what, (e, i) => {
    const pair = readPair(e);
    const root = pair === undefined ? undefined : takeBytes(pair[1]);
    return pair === undefined || root === undefined ? err(expectedAt("a [version, Uint8Array] array", i)) : ok([pair[0] as V, root] as const);
  });

/** {@link VersionedTrie.checkValue} of a key and a value already checked to be of their types. */
function checkValue(key: StateKey, value: Value): R<undefined> {
  const what = `the value of ${key.preview().short()}`;
  if (value.depth > MAX_VALUE_DEPTH) return err({ kind: "TooDeep", what, depth: value.depth, limit: MAX_VALUE_DEPTH });
  const length = cbor.encodedLengthWithin(value, MAX_VALUE_BYTES);
  if (length.ok) return ok(undefined);
  return length.error.kind === "TooLarge" ? err({ kind: "TooLarge", what, limit: MAX_VALUE_BYTES }) : malformed(`${what}: ${length.error.reason}`);
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

  /** The key's value hash, or `null` if it is proven absent; not covered is an error. What is not a `StateKey` is a `TypeMismatch`. */
  lookup(key: StateKey): C<Uint8Array | null> {
    const k = keyArg(key);
    return k.ok ? this.#lookup(k.value) : k;
  }

  #lookup(key: StateKey): R<Uint8Array | null> {
    const l = viaTrie(TRIE.lookup(this.#proof, key.bytes()));
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
   * costs at most the 1 MiB measure, never its expansion. A key that is not a `StateKey`, or a value that is neither
   * `null` nor a canonical value (an object posing as a null value included), is a `TypeMismatch`, never a claim.
   */
  holds(key: StateKey, value: Value | null): C<boolean> {
    const k = keyArg(key);
    if (!k.ok) return k;
    const v = value === null ? ok(null) : valueArg(value);
    if (!v.ok) return v;
    const l = this.#lookup(k.value);
    if (!l.ok) return l;
    const claimed = v.value === null || v.value.kind === "null" ? null : v.value;
    if (l.value === null || claimed === null) return ok(l.value === null && claimed === null);
    if (!checkValue(k.value, claimed).ok) return ok(false);
    const h = TRIE.valueHash(this.#hashing, claimed);
    return h.ok ? ok(bytesEqual(l.value, h.value)) : viaTrie(h);
  }

  /** Every entry under the namespace, complete; what is not a `StateNamespace` is a `TypeMismatch`. */
  namespace(ns: StateNamespace): C<readonly StateEntry[]> {
    if (!isNamespace(ns)) return err(expected("a StateNamespace"));
    return keyed(`namespace ${ns.preview().short()}`, TRIE.prefix(this.#proof, ns.prefix()));
  }

  /** Every entry in `[from, until)`, complete; no `until` is unbounded. Bounds that are not `StateKey`s are a `TypeMismatch`. */
  range(from: StateKey, until?: StateKey): C<readonly StateEntry[]> {
    const f = keyArg(from);
    if (!f.ok) return f;
    const u = until === undefined ? ok(undefined) : keyArg(until);
    if (!u.ok) return u;
    return keyed(`range [${f.value.preview().short()}, ${u.value?.preview().short() ?? ""})`, TRIE.range(this.#proof, f.value.bytes(), u.value?.bytes()));
  }
}

function keyed(what: string, listing: trie.Listing): R<readonly StateEntry[]> {
  if (listing.kind === "notCovered") return notCovered(what);
  const out: StateEntry[] = [];
  for (const e of listing.entries) {
    const k = keyFromBytes(e.key);
    if (!k.ok) return err({ kind: "Key", error: k.error });
    out.push([k.value, e.valueHash]);
  }
  return ok(out);
}

const sameEntries = (listing: readonly trie.TrieEntry[], expected: readonly trie.TrieEntry[]): boolean =>
  listing.length === expected.length && listing.every((x, i) => bytesEqual(x.key, expected[i]?.key ?? new Uint8Array(0)) && bytesEqual(x.valueHash, expected[i]?.valueHash ?? new Uint8Array(0)));

let isProfile!: (x: unknown) => x is VersionedTrie<unknown>;

/** `x` if it is a {@link VersionedTrie} made by `VersionedTrie.of` (a private-brand check). */
const profileArg = <V>(x: unknown): Result<VersionedTrie<V>, TypeMismatch> => (isProfile(x) ? ok(x as VersionedTrie<V>) : err(expected("a VersionedTrie")));

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
    token: Token,
    hashing: trie.TrieHashing,
    readonly versionKey: VersionKey<V>,
    modes: readonly [trie.KeyMode, trie.KeyMode, trie.KeyMode],
  ) {
    this.hashing = hashing;
    [this.topMode, this.stateMode, this.historyMode] = token === TOKEN ? modes : [modes[0], modes[0], modes[0]];
    if (token === TOKEN) PROFILE.stamp(this);
    Object.freeze(this);
  }

  static {
    isProfile = (x): x is VersionedTrie<unknown> => PROFILE.has(x);
  }

  /**
   * The profile over `algorithm` and `versionKey`. An algorithm that is not a `HashAlgorithm`, or a version key that is
   * not an object with an integer `width` (1 to `MAX_KEY_LENGTH`) and `encode` and `decode` functions, is a
   * `TypeMismatch`; a caller's version key is wrapped so that what it returns is checked (see {@link VersionKey}).
   */
  static of<V>(algorithm: HashAlgorithm, versionKey: VersionKey<V>): C<VersionedTrie<V>> {
    const a = algorithmArg(algorithm);
    if (!a.ok) return a;
    const k = versionKeyArg<V>(versionKey);
    if (!k.ok) return k;
    const hashing = trie.TrieHashing.of(a.value);
    const top = trie.KeyMode.fixed(1);
    const state = trie.KeyMode.variable(MAX_KEY_LENGTH);
    const history = trie.KeyMode.fixed(k.value.width);
    // every argument is this module's own, checked above: none of these refuses
    if (!hashing.ok || !top.ok || !state.ok || !history.ok) return malformed("the profile's tries");
    return ok(new VersionedTrie(TOKEN, hashing.value, k.value, [top.value, state.value, history.value]));
  }

  /** The hash a trie commits for a value: `valueHash(commit(v))`; one too large to commit is `Trie.TooLarge`. */
  valueHash(value: Value): C<Uint8Array> {
    const v = valueArg(value);
    return v.ok ? viaTrie(TRIE.valueHash(this.hashing, v.value)) : v;
  }

  /** The hash a trie commits for a byte string: the top trie's slots and the history trie's state roots. */
  bytesHash(b: Uint8Array): Result<Uint8Array, TypeMismatch> {
    const bytes = bytesArg(b);
    return bytes.ok ? ok(TRIE.bytesHash(this.hashing, bytes.value)) : bytes;
  }

  #bytesHash(b: Uint8Array): Uint8Array {
    return TRIE.bytesHash(this.hashing, b);
  }

  /**
   * A value every wire format can carry: at most 253 levels deep and 1 MiB of canonical CBOR. The size is measured
   * without encoding, and refused at once when the value's weight (a lower bound on its length) is already over, so a
   * value built with shared parts costs O(1 MiB) at most. A refusal carries the limit, never a measured size. A key that
   * is not a `StateKey`, or a value that is not a canonical value, is a `TypeMismatch`, before either is read.
   */
  checkValue(key: StateKey, value: Value): C<undefined> {
    const k = keyArg(key);
    if (!k.ok) return k;
    const v = valueArg(value);
    return v.ok ? checkValue(k.value, v.value) : v;
  }

  /** The root a head commits to: the top trie of `0x00 -> vkey`, `0x01 -> stateRoot`, `0x02 -> historyRoot`. */
  root(head: Head<V>): C<Uint8Array> {
    const h = headArg<V>(head, "the head");
    return h.ok ? this.#root(h.value) : h;
  }

  #root(head: Head<V>): C<Uint8Array> {
    const vkey = this.versionKey.encode(head.version);
    if (!vkey.ok) return vkey;
    return viaTrie(
      TRIE.trieRoot(this.hashing, this.topMode, [
        [SLOTS.version, this.#bytesHash(vkey.value)],
        [SLOTS.state, this.#bytesHash(head.stateRoot)],
        [SLOTS.history, this.#bytesHash(head.historyRoot)],
      ]),
    );
  }

  /** Whether two heads are equal: the same version key and roots. Heads are checked as {@link root} checks one. */
  sameHead(a: Head<V>, b: Head<V>): C<boolean> {
    const x = headArg<V>(a, "the first head");
    if (!x.ok) return x;
    const y = headArg<V>(b, "the second head");
    return y.ok ? this.#sameHead(x.value, y.value) : y;
  }

  #sameHead(a: Head<V>, b: Head<V>): C<boolean> {
    const ka = this.versionKey.encode(a.version);
    if (!ka.ok) return ka;
    const kb = this.versionKey.encode(b.version);
    if (!kb.ok) return kb;
    return ok(bytesEqual(ka.value, kb.value) && bytesEqual(a.stateRoot, b.stateRoot) && bytesEqual(a.historyRoot, b.historyRoot));
  }

  // ---------------------------------------------------------------- wire forms

  #digest(b: Uint8Array): R<Uint8Array> {
    return b.length === this.hashing.size ? ok(copyBytes(b)) : malformed(`a digest is ${this.hashing.size.toString()} bytes, got ${b.length.toString()}`);
  }

  #fromWireHead(w: models.versionedTrie.VersionedTrieHead): C<Head<V>> {
    const version = this.versionKey.decode(w.v);
    if (!version.ok) return version;
    const s = this.#digest(w.s);
    if (!s.ok) return s;
    const h = this.#digest(w.h);
    if (!h.ok) return h;
    return ok(Object.freeze({ version: version.value, stateRoot: s.value, historyRoot: h.value }));
  }

  /** A head from its canonical value (`versioned-trie-head`); one heavier than `MAX_PROOF_WEIGHT` is `Malformed`. */
  decodeHead(value: Value): C<Head<V>> {
    const v = valueArg(value);
    if (!v.ok) return v;
    const w = decodeInput(models.versionedTrie.VersionedTrieHead, v.value);
    return w.ok ? this.#fromWireHead(w.value) : malformed(describeFailure(w.error));
  }

  /** A head's canonical value; a head is checked as {@link root} checks one. */
  encodeHead(head: Head<V>): C<Value> {
    const h = headArg<V>(head, "the head");
    if (!h.ok) return h;
    const v = this.versionKey.encode(h.value.version);
    if (!v.ok) return v;
    const e = models.versionedTrie.VersionedTrieHead.encode({ v: v.value, s: h.value.stateRoot, h: h.value.historyRoot });
    return e.ok ? e : malformed(e.error.kind);
  }

  /** A proof from its canonical value (`versioned-trie-proof`); one heavier than `MAX_PROOF_WEIGHT` is `Malformed`. */
  decodeProof(value: Value): C<VersionedTrieProof<V>> {
    const v = valueArg(value);
    if (!v.ok) return v;
    const w = decodeInput(models.versionedTrie.VersionedTrieProof, v.value);
    if (!w.ok) return malformed(describeFailure(w.error));
    const head = this.#fromWireHead(w.value.head);
    if (!head.ok) return head;
    const top = viaTrie(TRIE.fromRecords(this.hashing, w.value.top));
    if (!top.ok) return top;
    let at: At<V> | undefined;
    if (w.value.at !== undefined) {
      const version = this.versionKey.decode(w.value.at.v);
      if (!version.ok) return version;
      const s = w.value.at.s === undefined ? ok(undefined) : this.#digest(w.value.at.s);
      if (!s.ok) return s;
      at = Object.freeze(s.value === undefined ? { version: version.value } : { version: version.value, stateRoot: s.value });
    }
    const part = (records: models.trie.TrieProof | undefined): R<trie.TrieProof | undefined> => (records === undefined ? ok(undefined) : viaTrie(TRIE.fromRecords(this.hashing, records)));
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
  decodeDelta(value: Value): C<Delta<V>> {
    const v = valueArg(value);
    if (!v.ok) return v;
    const w = decodeInput(models.versionedTrie.VersionedTrieDelta, v.value);
    if (!w.ok) return malformed(describeFailure(w.error));
    const parent = w.value.parent === undefined ? ok(undefined) : this.#fromWireHead(w.value.parent);
    if (!parent.ok) return parent;
    const head = this.#fromWireHead(w.value.head);
    if (!head.ok) return head;
    const upserts: (readonly [StateKey, Value])[] = [];
    for (const u of w.value.upserts) {
      const k = parseKey(u.k);
      if (!k.ok) return err({ kind: "Key", error: k.error });
      upserts.push([k.value, u.v]);
    }
    const removes: StateKey[] = [];
    for (const r of w.value.removes) {
      const k = parseKey(r);
      if (!k.ok) return err({ kind: "Key", error: k.error });
      removes.push(k.value);
    }
    return ok(Object.freeze({ ...(parent.value === undefined ? {} : { parent: parent.value }), head: head.value, upserts, removes }));
  }

  /**
   * A snapshot chunk from canonical CBOR, refused by size before it is decoded; order is the reader's to check. What is
   * not a `Uint8Array` is a `TypeMismatch`; the bytes are measured by their internal length.
   */
  decodeChunk(bytes: Uint8Array): C<SnapshotChunk<V>> {
    const b = bytesArg(bytes);
    return b.ok ? this.#decodeChunk(b.value) : b;
  }

  #decodeChunk(bytes: Uint8Array): C<SnapshotChunk<V>> {
    if (bytes.length > MAX_CHUNK_BYTES) return err({ kind: "TooLarge", what: "a snapshot chunk", limit: MAX_CHUNK_BYTES });
    const value = cbor.decode(bytes);
    if (!value.ok) return malformed(value.error.kind);
    const w = models.versionedTrie.VersionedTrieSnapshotChunk.decode(value.value);
    if (!w.ok) return malformed(describeFailure(w.error));
    const c = w.value;
    if ("head" in c) {
      const head = this.#fromWireHead(c.head);
      return head.ok ? ok({ kind: "head", head: head.value }) : head;
    }
    if ("state" in c) {
      const entries: (readonly [StateKey, Value])[] = [];
      for (const u of c.state) {
        const k = parseKey(u.k);
        if (!k.ok) return err({ kind: "Key", error: k.error });
        entries.push([k.value, u.v]);
      }
      return ok({ kind: "state", entries });
    }
    const versions: (readonly [V, Uint8Array])[] = [];
    for (const x of c.history) {
      const v = this.versionKey.decode(x.v);
      if (!v.ok) return v;
      const s = this.#digest(x.s);
      if (!s.ok) return s;
      versions.push([v.value, s.value]);
    }
    return ok({ kind: "history", versions });
  }

  // ---------------------------------------------------------------- verification (section 6)

  #verified(mode: trie.KeyMode, root: Uint8Array, what: string, proof: trie.TrieProof | undefined): R<trie.VerifiedTrieProof> {
    return proof === undefined ? malformed(`the proof has no ${what} proof`) : viaTrie(TRIE.check(this.hashing, mode, root, proof));
  }

  /** The checked arguments every rule starts from: a root (a plain view) and a proof (a frozen copy). */
  #args(root: unknown, proof: unknown): Result<readonly [Uint8Array, VersionedTrieProof<V>], TypeMismatch> {
    const r = bytesArg(root);
    if (!r.ok) return r;
    const p = proofArg<V>(proof);
    return p.ok ? ok([r.value, p.value]) : p;
  }

  /** Rule 1: the head binds to `root`, and the proof's top trie holds the head's values in slots 0x00..0x02. */
  #bind(root: Uint8Array, proof: VersionedTrieProof<V>): C<Head<V>> {
    const claimed = this.#root(proof.head);
    if (!claimed.ok) return claimed;
    if (!bytesEqual(claimed.value, root)) return mismatch("root", root, claimed.value);
    const top = this.#verified(this.topMode, root, "top", proof.top);
    if (!top.ok) return top;
    const vkey = this.versionKey.encode(proof.head.version);
    if (!vkey.ok) return vkey;
    const slots: readonly (readonly [Uint8Array, Uint8Array])[] = [
      [SLOTS.version, vkey.value],
      [SLOTS.state, proof.head.stateRoot],
      [SLOTS.history, proof.head.historyRoot],
    ];
    for (const [slot, value] of slots) {
      const l = viaTrie(TRIE.lookup(top.value, slot));
      if (!l.ok) return l;
      if (l.value.kind === "notCovered") return notCovered(`top slot ${slotName(slot)}`);
      if (l.value.kind !== "present" || !bytesEqual(l.value.valueHash, this.#bytesHash(value))) return malformed(`top slot ${slotName(slot)} does not hold the head's value`);
    }
    return ok(proof.head);
  }

  #history(head: Head<V>, proof: VersionedTrieProof<V>): R<trie.VerifiedTrieProof> {
    return this.#verified(this.historyMode, head.historyRoot, "history", proof.history);
  }

  #asked(proof: VersionedTrieProof<V>, u: V): C<At<V>> {
    const at = proof.at;
    if (at === undefined) return malformed("the proof names no version");
    const a = this.versionKey.encode(at.version);
    if (!a.ok) return a;
    const b = this.versionKey.encode(u);
    if (!b.ok) return b;
    return bytesEqual(a.value, b.value) ? ok(at) : malformed(`the proof is about version ${toHexOf(a.value)}, not ${toHexOf(b.value)}`);
  }

  /** The state root of `u` per the history trie: the root if committed, `null` if proven never committed. */
  #versionRoot(head: Head<V>, proof: VersionedTrieProof<V>, u: V): C<Uint8Array | null> {
    const at = this.#asked(proof, u);
    if (!at.ok) return at;
    const hist = this.#history(head, proof);
    if (!hist.ok) return hist;
    const vkey = this.versionKey.encode(u);
    if (!vkey.ok) return vkey;
    const l = viaTrie(TRIE.lookup(hist.value, vkey.value));
    if (!l.ok) return l;
    const k = toHexOf(vkey.value);
    const s = at.value.stateRoot;
    switch (l.value.kind) {
      case "notCovered":
        return notCovered(`version ${k}`);
      case "present":
        if (s === undefined) return malformed(`version ${k} is committed; the proof claims it is not`);
        return bytesEqual(l.value.valueHash, this.#bytesHash(s)) ? ok(s) : malformed(`history holds another state root for ${k}`);
      case "absent":
        return s === undefined ? ok(null) : malformed(`version ${k} is not committed; the proof claims a state`);
    }
  }

  /**
   * Rule 7: the head bound to `root`, with the head invariant: the history holds the head version with the head's
   * state root, and nothing after it. The history part is a range proof from the head version up. A root that is not a
   * `Uint8Array`, or a proof that is not a `VersionedTrieProof` (its fields read once each), is a `TypeMismatch`.
   */
  verifyHead(root: Uint8Array, proof: VersionedTrieProof<V>): C<Head<V>> {
    const a = this.#args(root, proof);
    if (!a.ok) return a;
    const [r, p] = a.value;
    const head = this.#bind(r, p);
    if (!head.ok) return head;
    const hist = this.#history(head.value, p);
    if (!hist.ok) return hist;
    const vkey = this.versionKey.encode(head.value.version);
    if (!vkey.ok) return vkey;
    const tail = TRIE.range(hist.value, vkey.value, undefined);
    if (tail.kind === "notCovered") return notCovered("history from the head version up");
    return sameEntries(tail.entries, [{ key: vkey.value, valueHash: this.#bytesHash(head.value.stateRoot) }])
      ? head
      : malformed("the history trie does not end at the head version with the head's state root");
  }

  /** Rule 2: a proof about the current state, checked against the head's state root. Arguments as {@link verifyHead}. */
  verifyState(root: Uint8Array, proof: VersionedTrieProof<V>): C<VerifiedState> {
    const a = this.#args(root, proof);
    if (!a.ok) return a;
    const [r, p] = a.value;
    const head = this.#bind(r, p);
    if (!head.ok) return head;
    if (p.at !== undefined) return malformed("a historical proof is not about the current state");
    const state = this.#verified(this.stateMode, head.value.stateRoot, "state", p.state);
    return state.ok ? ok(createState(state.value, this.hashing)) : state;
  }

  /**
   * Rule 3: version `u`'s state root if the head's history committed it, `null` if it provably never did. Arguments as
   * {@link verifyHead}; `u` is checked by the version key.
   */
  verifyVersion(root: Uint8Array, proof: VersionedTrieProof<V>, u: V): C<Uint8Array | null> {
    const a = this.#args(root, proof);
    if (!a.ok) return a;
    const [r, p] = a.value;
    const head = this.#bind(r, p);
    return head.ok ? this.#versionRoot(head.value, p, u) : head;
  }

  /** Rule 4: the state of a past version `u`, its root taken from the history trie. Arguments as {@link verifyVersion}. */
  verifyHistoricalKey(root: Uint8Array, proof: VersionedTrieProof<V>, u: V): C<VerifiedState> {
    const a = this.#args(root, proof);
    if (!a.ok) return a;
    const [r, p] = a.value;
    const head = this.#bind(r, p);
    if (!head.ok) return head;
    const uk = this.versionKey.encode(u);
    if (!uk.ok) return uk;
    const s = this.#versionRoot(head.value, p, u);
    if (!s.ok) return s;
    if (s.value === null) return err({ kind: "UnknownVersion", version: uk.value });
    const state = this.#verified(this.stateMode, s.value, "state", p.state);
    return state.ok ? ok(createState(state.value, this.hashing)) : state;
  }

  /**
   * Rule 6: whether `claimed` is exactly the committed versions in `[from, until)` with their state roots, in order.
   * Arguments as {@link verifyVersion}; `claimed` must be an array of `[version, Uint8Array]` arrays (read by index).
   */
  verifyVersions(root: Uint8Array, proof: VersionedTrieProof<V>, from: V, until: V | null, claimed: readonly (readonly [V, Uint8Array])[]): C<boolean> {
    const a = this.#args(root, proof);
    if (!a.ok) return a;
    const [r, p] = a.value;
    const given = versionsArg<V>(claimed, "an array of [version, Uint8Array] claims");
    if (!given.ok) return given;
    const head = this.#bind(r, p);
    if (!head.ok) return head;
    const hist = this.#history(head.value, p);
    if (!hist.ok) return hist;
    const fk = this.versionKey.encode(from);
    if (!fk.ok) return fk;
    const uk = until === null ? ok(undefined) : this.versionKey.encode(until);
    if (!uk.ok) return uk;
    const listing = TRIE.range(hist.value, fk.value, uk.value);
    if (listing.kind === "notCovered") return notCovered(`versions [${toHexOf(fk.value)}, ${uk.value === undefined ? "" : toHexOf(uk.value)})`);
    const expected: trie.TrieEntry[] = [];
    for (const [v, s] of given.value) {
      const k = this.versionKey.encode(v);
      if (!k.ok) return k;
      expected.push({ key: k.value, valueHash: this.#bytesHash(s) });
    }
    return ok(sameEntries(listing.entries, expected));
  }

  /**
   * Rule 5: the latest committed version `p <= t` and its state root, or `null` if every committed version is after
   * `t`. The history part lists `[vkey(p), vkey(t) || 0x00)` as exactly `p`. Arguments as {@link verifyVersion}.
   */
  verifyAsOf(root: Uint8Array, proof: VersionedTrieProof<V>, t: V): C<{ readonly version: V; readonly stateRoot: Uint8Array } | null> {
    const a = this.#args(root, proof);
    if (!a.ok) return a;
    const [r, p] = a.value;
    const head = this.#bind(r, p);
    if (!head.ok) return head;
    const hist = this.#history(head.value, p);
    if (!hist.ok) return hist;
    const tkey = this.versionKey.encode(t);
    if (!tkey.ok) return tkey;
    const bound = concatBytes([tkey.value, Uint8Array.of(0)]);
    let found: { readonly pkey: Uint8Array; readonly version: V; readonly stateRoot: Uint8Array } | undefined;
    if (p.at !== undefined) {
      const pkey = this.versionKey.encode(p.at.version);
      if (!pkey.ok) return pkey;
      const s = p.at.stateRoot;
      if (s === undefined) return malformed("an as-of answer names its state root");
      if (compareBytes(pkey.value, tkey.value) > 0) return malformed(`the answer ${toHexOf(pkey.value)} is after ${toHexOf(tkey.value)}`);
      found = { pkey: pkey.value, version: p.at.version, stateRoot: s };
    }
    const listing = TRIE.range(hist.value, found?.pkey ?? new Uint8Array(0), bound);
    if (listing.kind === "notCovered") return notCovered(`history up to ${toHexOf(tkey.value)}`);
    const expected = found === undefined ? [] : [{ key: found.pkey, valueHash: this.#bytesHash(found.stateRoot) }];
    if (!sameEntries(listing.entries, expected)) return malformed(`the history up to ${toHexOf(tkey.value)} is not the claimed answer`);
    return ok(found === undefined ? null : { version: found.version, stateRoot: found.stateRoot });
  }
}

// ------------------------------------------------------------------ snapshots (section 7)

type Stage = "head" | "state" | "history" | "done";

/** A caller's chunk, its fields read once into a frozen copy: its kind, then its head, entries or versions. */
function chunkArg<V>(x: unknown): Result<SnapshotChunk<V>, TypeMismatch> {
  if (!isObject(x)) return err(expected("a SnapshotChunk"));
  const kind: unknown = Reflect.get(x, "kind");
  switch (kind) {
    case "head": {
      const head = headArg<V>(Reflect.get(x, "head"), "the chunk's head");
      return head.ok ? ok(Object.freeze({ kind, head: head.value })) : head;
    }
    case "state": {
      const entries = entriesArg(Reflect.get(x, "entries"), "the chunk's entries");
      return entries.ok ? ok(Object.freeze({ kind, entries: entries.value })) : entries;
    }
    case "history": {
      const versions = versionsArg<V>(Reflect.get(x, "versions"), "the chunk's versions");
      return versions.ok ? ok(Object.freeze({ kind, versions: versions.value })) : versions;
    }
    default:
      return err(expected("a SnapshotChunk of kind head, state or history"));
  }
}

let startReader!: <V>(profile: VersionedTrie<V>, trusted: Head<V>) => SnapshotReader<V>;
let feedChunk!: <V>(r: SnapshotReader<V>, chunk: SnapshotChunk<V>) => C<SnapshotReader<V>>;
let finishReader!: <V>(r: SnapshotReader<V>) => R<Snapshot<V>>;

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

  static {
    startReader = <V>(profile: VersionedTrie<V>, trusted: Head<V>) => new SnapshotReader(profile, trusted, "head", undefined, undefined, [], []);
    feedChunk = (r, chunk) => r.#feed(chunk);
    finishReader = (r) => r.#finish();
  }

  /**
   * A reader against `trusted` under `profile`. A profile that is not a `VersionedTrie`, or a head that is not a `Head`
   * (its fields read once, its roots copied), is a `TypeMismatch`.
   */
  static start<V>(profile: VersionedTrie<V>, trusted: Head<V>): Result<SnapshotReader<V>, TypeMismatch> {
    const p = profileArg<V>(profile);
    if (!p.ok) return p;
    const h = headArg<V>(trusted, "the trusted head");
    return h.ok ? ok(startReader(p.value, h.value)) : h;
  }

  #next(changes: {
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

  /** Decodes and feeds one chunk of canonical CBOR; what is not a `Uint8Array` is a `TypeMismatch`. */
  feedBytes(bytes: Uint8Array): C<SnapshotReader<V>> {
    const c = this.profile.decodeChunk(bytes);
    return c.ok ? this.#feed(c.value) : c;
  }

  /** Feeds one chunk; one that is not a `SnapshotChunk` (its fields read once into a copy) is a `TypeMismatch`. */
  feed(chunk: SnapshotChunk<V>): C<SnapshotReader<V>> {
    const c = chunkArg<V>(chunk);
    return c.ok ? this.#feed(c.value) : c;
  }

  #feed(chunk: SnapshotChunk<V>): C<SnapshotReader<V>> {
    const bad = (reason: string): R<never> => malformed(`snapshot: ${reason}`);
    const key = this.profile.versionKey;
    if (this.stage === "head") {
      if (chunk.kind !== "head") return bad("it starts with its head");
      const same = this.profile.sameHead(chunk.head, this.trusted);
      if (!same.ok) return same;
      return same.value ? ok(this.#next({ stage: "state" })) : bad("its head is not the trusted head");
    }
    if (chunk.kind === "head") return bad("it has one head");
    if (this.stage === "state" && chunk.kind === "state") {
      const es = chunk.entries;
      if (es.length < 1 || es.length > MAX_CHUNK_ENTRIES) return bad(`a state chunk holds 1..${MAX_CHUNK_ENTRIES.toString()} items, not ${es.length.toString()}`);
      for (const [k, v] of es) {
        if (v.kind === "null") return bad(`${k.preview().short()} is null`);
        const c = checkValue(k, v);
        if (!c.ok) return c;
      }
      let previous = this.lastKey;
      for (const [k] of es) {
        const b = k.bytes();
        if (previous !== undefined && compareBytes(previous, b) >= 0) return bad(`key ${Preview.of(toHexOf(b)).short()} does not follow ${Preview.of(toHexOf(previous)).short()}`);
        previous = b;
      }
      return ok(this.#next({ ...(previous === undefined ? {} : { lastKey: previous }), state: es }));
    }
    if (this.stage === "done") return bad("nothing follows the head version");
    if (chunk.kind === "state") return bad("state entries precede the history");
    const vs = chunk.versions;
    if (vs.length < 1 || vs.length > MAX_CHUNK_ENTRIES) return bad(`a history chunk holds 1..${MAX_CHUNK_ENTRIES.toString()} items, not ${vs.length.toString()}`);
    const keys: Uint8Array[] = [];
    for (const [u] of vs) {
      const k = key.encode(u);
      if (!k.ok) return k;
      keys.push(k.value);
    }
    const hk = key.encode(this.trusted.version);
    if (!hk.ok) return hk;
    let previous = this.lastVersion;
    for (const k of keys) {
      if (previous !== undefined && compareBytes(previous, k) >= 0) return bad(`version ${Preview.of(toHexOf(k)).short()} does not follow ${Preview.of(toHexOf(previous)).short()}`);
      previous = k;
    }
    for (const [i, k] of keys.entries()) {
      const s = vs[i]?.[1] ?? new Uint8Array(0);
      if (compareBytes(k, hk.value) > 0) return bad(`version ${toHexOf(k)} is after the head`);
      if (bytesEqual(k, hk.value) && !bytesEqual(s, this.trusted.stateRoot)) return bad("the head version has another state root");
    }
    const last = keys[keys.length - 1];
    return ok(this.#next({ stage: last !== undefined && bytesEqual(last, hk.value) ? "done" : "history", ...(last === undefined ? {} : { lastVersion: last }), history: vs }));
  }

  /** The snapshot, once its history has reached the head version. */
  finish(): R<Snapshot<V>> {
    return this.#finish();
  }

  #finish(): R<Snapshot<V>> {
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

const copyHead = <V>(h: Head<V>): Head<V> => Object.freeze({ version: h.version, stateRoot: copyBytes(h.stateRoot), historyRoot: copyBytes(h.historyRoot) });

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

/** A caller's snapshot, its fields read once into a frozen copy: a head, entries and versions. */
function snapshotArg<V>(x: unknown): Result<Snapshot<V>, TypeMismatch> {
  if (!isObject(x)) return err(expected("a Snapshot"));
  const head = headArg<V>(Reflect.get(x, "head"), "the snapshot's head");
  if (!head.ok) return head;
  const entries = entriesArg(Reflect.get(x, "entries"), "the snapshot's entries");
  if (!entries.ok) return entries;
  const history = versionsArg<V>(Reflect.get(x, "history"), "the snapshot's history");
  return history.ok ? ok(Object.freeze({ head: head.value, entries: entries.value, history: history.value })) : history;
}

/** A caller's delta, its fields read once into a frozen copy: an optional parent head, a head, upserts and removes. */
function deltaArg<V>(x: unknown): Result<Delta<V>, TypeMismatch> {
  if (!isObject(x)) return err(expected("a Delta"));
  const parentField: unknown = Reflect.get(x, "parent");
  const parent = parentField === undefined ? ok(undefined) : headArg<V>(parentField, "the delta's parent");
  if (!parent.ok) return parent;
  const head = headArg<V>(Reflect.get(x, "head"), "the delta's head");
  if (!head.ok) return head;
  const upserts = entriesArg(Reflect.get(x, "upserts"), "the delta's upserts");
  if (!upserts.ok) return upserts;
  const removes = readArray(Reflect.get(x, "removes"), "the delta's removes", (k, i) => (isKey(k) ? ok(k) : err(expectedAt("a StateKey", i))));
  if (!removes.ok) return removes;
  return ok(Object.freeze({ ...(parent.value === undefined ? {} : { parent: parent.value }), head: head.value, upserts: upserts.value, removes: removes.value }));
}

let replicaOf!: <V>(profile: VersionedTrie<V>, head: Head<V>, state: TrieMap<StateSlot>, versions: TrieMap<VersionSlot<V>>) => Replica<V>;

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

  /** An empty replica under `profile`; one that is not a `VersionedTrie` is a `TypeMismatch`. */
  static empty<V>(profile: VersionedTrie<V>): Result<Replica<V>, TypeMismatch> {
    const p = profileArg<V>(profile);
    return p.ok ? ok(new Replica(p.value, null, TrieMap.empty(p.value.hashing, p.value.stateMode), TrieMap.empty(p.value.hashing, p.value.historyMode))) : p;
  }

  /** The commitment root of the head, or `null` before the first delta. */
  root(): C<Uint8Array | null> {
    return this.#head === null ? ok(null) : this.profile.root(this.#head);
  }

  /** The current value of `key`, if any: one walk down its path. What is not a `StateKey` is a `TypeMismatch`. */
  get(key: StateKey): Result<Value | undefined, TypeMismatch> {
    const k = keyArg(key);
    return k.ok ? ok(this.state.get(k.value.bytes())?.value) : k;
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
    return this.versions.values().map((s) => [s.version, copyBytes(s.stateRoot)] as const);
  }

  /**
   * A replica of `snapshot`, which must pass every check of {@link SnapshotReader} against `trusted` and reproduce its
   * state and history roots. The snapshot's own head is only a claim. A profile, head or snapshot that is not of its
   * type (a snapshot's fields read once each, its arrays by index) is a `TypeMismatch`.
   */
  static fromSnapshot<V>(profile: VersionedTrie<V>, trusted: Head<V>, snapshot: Snapshot<V>): C<Replica<V>> {
    const p = profileArg<V>(profile);
    if (!p.ok) return p;
    const t = headArg<V>(trusted, "the trusted head");
    if (!t.ok) return t;
    const s = snapshotArg<V>(snapshot);
    return s.ok ? fromSnapshot(p.value, t.value, s.value) : s;
  }

  /**
   * Reads a snapshot's chunks of canonical CBOR against `trusted`, then restores a replica from it. `chunks` is read
   * once, as `for...of` reads it, each chunk fed as it is read (its iterator closed on a refusal); a chunk that is not a
   * `Uint8Array`, or what `for...of` would throw a `TypeError` for, is a `TypeMismatch`.
   */
  static restore<V>(profile: VersionedTrie<V>, trusted: Head<V>, chunks: Iterable<Uint8Array>): C<Replica<V>> {
    const start = SnapshotReader.start(profile, trusted);
    if (!start.ok) return start;
    const fed = foldIterable<SnapshotReader<V>, VersionedRefusal>(chunks, start.value, (reader, c) => reader.feedBytes(c as Uint8Array));
    if (!fed.ok) return fed;
    const snapshot = finishReader(fed.value);
    return snapshot.ok ? fromSnapshot(fed.value.profile, fed.value.trusted, snapshot.value) : snapshot;
  }

  /**
   * The replica after `delta`, which must chain from this head, raise the version, write no key twice nor both write
   * and remove one, carry only values every wire format can, and whose claimed head must be what the replica computes.
   * This replica is unchanged either way. A delta that is not a `Delta` (its fields read once each, its arrays by index,
   * its keys `StateKey`s and its values canonical values) is a `TypeMismatch`.
   */
  applyDelta(delta: Delta<V>): C<Replica<V>> {
    const d = deltaArg<V>(delta);
    return d.ok ? this.#apply(d.value) : d;
  }

  #apply(delta: Delta<V>): C<Replica<V>> {
    const p = this.profile;
    const parent = this.root();
    if (!parent.ok) return parent;
    const claimed = delta.parent === undefined ? ok(null) : p.root(delta.parent);
    if (!claimed.ok) return claimed;
    const same = parent.value === null || claimed.value === null ? parent.value === claimed.value : bytesEqual(parent.value, claimed.value);
    if (!same) return err({ kind: "RootMismatch", component: "parent", expected: parent.value === null ? "none" : toHexOf(parent.value), computed: claimed.value === null ? "none" : toHexOf(claimed.value) });
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
      const c = checkValue(k, v);
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
      const h = viaTrie(TRIE.valueHash(p.hashing, value));
      if (!h.ok) return h;
      const next = viaTrie(state.set(key.bytes(), { key, value }, h.value));
      if (!next.ok) return next;
      state = next.value;
    }
    const s = state.root;
    if (!bytesEqual(s, delta.head.stateRoot)) return mismatch("state", delta.head.stateRoot, s);
    const versions = viaTrie(this.versions.set(nk.value, { version: delta.head.version, stateRoot: s }, TRIE.bytesHash(p.hashing, s)));
    if (!versions.ok) return versions;
    const h = versions.value.root;
    if (!bytesEqual(h, delta.head.historyRoot)) return mismatch("history", delta.head.historyRoot, h);
    return ok(new Replica(p, delta.head, state, versions.value));
  }

  static {
    replicaOf = (profile, head, state, versions) => new Replica(profile, head, state, versions);
  }
}

/** {@link Replica.fromSnapshot} of checked, owned arguments. */
function fromSnapshot<V>(profile: VersionedTrie<V>, trusted: Head<V>, snapshot: Snapshot<V>): C<Replica<V>> {
  const chunks: SnapshotChunk<V>[] = [
    { kind: "head", head: trusted },
    ...chunked(snapshot.entries, MAX_CHUNK_ENTRIES).map((entries) => ({ kind: "state" as const, entries })),
    ...chunked(snapshot.history, MAX_CHUNK_ENTRIES).map((versions) => ({ kind: "history" as const, versions })),
  ];
  let reader = startReader(profile, trusted);
  for (const c of chunks) {
    const r = feedChunk(reader, c);
    if (!r.ok) return r;
    reader = r.value;
  }
  const checked = finishReader(reader);
  if (!checked.ok) return checked;
  // the reader has checked that keys and versions strictly increase, which the builder requires
  const entries: (readonly [Uint8Array, StateSlot, Uint8Array])[] = [];
  for (const [key, value] of checked.value.entries) {
    const h = viaTrie(TRIE.valueHash(profile.hashing, value));
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
    versions.push([vkey.value, { version, stateRoot: copyBytes(stateRoot) }, TRIE.bytesHash(profile.hashing, stateRoot)]);
  }
  const history = viaTrie(TrieMap.fromSorted(profile.hashing, profile.historyMode, versions));
  if (!history.ok) return history;
  if (!bytesEqual(history.value.root, trusted.historyRoot)) return mismatch("history", trusted.historyRoot, history.value.root);
  return ok(replicaOf(profile, trusted, state.value, history.value));
}
