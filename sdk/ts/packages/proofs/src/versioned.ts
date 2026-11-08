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
import { isRevokedProxy, mint, ownBytes, ownCopy, readGuarded, REVOKED, toHexOf, typeMismatch } from "@kd5ujc/dilf4s-core/internal";

import { ascii, bytesEqual, compareBytes, concatBytes, copyBytes, fromAscii, isAscii } from "./bytes.js";
import { algorithmArg, bytesArg, expected, expectedAt, foldIterable, isMeasuredBytes, isObject, isSized, measured, NOT_A_BIGINT, NOT_A_NUMBER, NOT_A_STRING, NOT_BYTES, Oversized, readArray, readPair, TOKEN, valueArg, type Measured, type Token } from "./check.js";
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
 * Typed failures of the versioned trie: generated from `shared/schemas/proofs-errors.cddl` (`models.proofsErrors`),
 * with the reference's and the Rust SDK's kinds and fields. Versions appear as their version keys' full lowercase hex,
 * never as bytes. The store-only kinds (`Empty`, `VersionConflict`, `CommitRewrite`, `NotRetained`, `Store`) do not
 * arise here: this SDK has no store.
 */
export type VersionedTrieError = models.proofsErrors.VersionedTrieError;

/** Which recomputed root a `RootMismatch` names. */
type Component = models.proofsErrors.VtRootComponent;

/** A versioned-trie refusal, or an argument of the wrong type: what the exported functions here may refuse with. */
export type VersionedRefusal = VersionedTrieError | TypeMismatch;

type R<T> = Result<T, VersionedTrieError>;
/** The result of a function a caller reaches: its refusals, or a `TypeMismatch`. */
type C<T> = Result<T, VersionedRefusal>;

const malformed = (reason: string): R<never> => err({ kind: "Malformed", reason });
const notCovered = (what: string): R<never> => err({ kind: "NotCovered", what });
const viaTrie = <T>(r: Result<T, trie.TrieError>): R<T> => (r.ok ? r : err({ kind: "Trie", error: r.error }));
// a trusted root of the wrong width is the caller's: named by its length, never hex-encoded whole (conformance.md section 8)
const mismatch = (component: Component, expected: Measured, computed: Uint8Array): R<never> =>
  err({ kind: "RootMismatch", component, expected: rootText(expected, computed.length), computed: toHexOf(computed) });

/**
 * A version key as a refusal names it: its full lowercase hex, never truncated. Every key reaching a refusal is checked
 * at its version key's width, at most `trie.MAX_FIXED_BYTES` (1024), so the text is at most 2048 characters
 * (versioned-trie.md section 2).
 */
const versionHex = (key: Uint8Array): models.proofsErrors.VersionKeyHex => toHexOf(key);

/** A root as a refusal names it: its hex at the digest's width, otherwise its length (an oversized one's, uncopied). */
const rootText = (root: Measured, width: number): string => (isSized(root, width) ? toHexOf(root) : `a root of ${root.length.toString()} bytes`);

// ------------------------------------------------------------------ keys (section 2)

export const MAX_SEGMENT_LENGTH = models.limits.STATE_KEY_MAX_SEGMENT_LENGTH;
export const MAX_SEGMENTS = models.limits.STATE_KEY_MAX_SEGMENTS;
export const MAX_KEY_LENGTH = models.limits.STATE_KEY_MAX_LENGTH;

const SEGMENT = /^[a-z0-9][a-z0-9._-]*$/;

const KEY = mint();
const NAMESPACE = mint();
const PROFILE = mint();
const STATE = mint();
const READER = mint();
const REPLICA = mint();

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
    // without the token none of the caller's arguments is kept: every field is a fixed default
    if (token !== TOKEN) this.value = "";
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
    // within the key's bound: longer bytes are refused by their length, uncopied
    const b = ownBytes(bytes, MAX_KEY_LENGTH);
    if (b === undefined) return err(NOT_BYTES);
    return typeof b === "number" ? err({ kind: "TooLong", limit: MAX_KEY_LENGTH }) : keyFromBytes(b);
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
Object.freeze(StateKey);
Object.freeze(StateKey.prototype);

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
    // without the token none of the caller's arguments is kept: every field is a fixed default
    if (token !== TOKEN) this.value = "";
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
    if (!isNamespace(this)) return err(expected("a StateNamespace"));
    const k = keyArg(key);
    return k.ok ? ok(k.value.value.startsWith(`${this.value}/`)) : k;
  }

  toString(): string {
    return this.value;
  }
}
Object.freeze(StateNamespace);
Object.freeze(StateNamespace.prototype);

// ------------------------------------------------------------------ version keys

/**
 * How a version becomes a history-trie key: a fixed `width` and an unsigned big-endian encoding, so that comparing
 * versions and comparing their keys as unsigned bytes always agree. `width` is part of what a verifier trusts. A
 * version key may be the caller's own; this module checks what it gives (a `Result`, and bytes of `width`) before using
 * it, as a `TypeMismatch` otherwise.
 */
export interface VersionKey<V> {
  readonly width: number;
  /** The version's key: exactly `width` bytes; a version of the wrong type is a `TypeMismatch`. */
  encode(version: V): C<Uint8Array>;
  /** The version of a key; bytes that are not a `Uint8Array` are a `TypeMismatch`. */
  decode(bytes: Uint8Array): C<V>;
}

/** The version keys this module made: their results need no check. */
const OWN_KEYS = new WeakSet();
/** This module's `VersionKey.bytes` keys, with their widths: their versions are bytes the module copies ({@link ownVersion}). */
const BYTE_KEYS = new WeakMap<object, number>();

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
      // within the key's width: longer bytes are refused by their length, uncopied
      const b = ownBytes(bytes, width);
      if (b === undefined) return err(expected("a Uint8Array"));
      const got = typeof b === "number" ? b : b.length;
      if (typeof b === "number" || got !== width) return err({ kind: "InvalidVersion", reason: `a version key is ${width.toString()} bytes, got ${got.toString()}` });
      let v = 0n;
      for (const x of b) v = (v << 8n) | BigInt(x);
      return v <= max ? ok(v) : err({ kind: "InvalidVersion", reason: `version ${toHexOf(b)} exceeds ${max.toString()}` });
    },
  });
  OWN_KEYS.add(key);
  return key;
}

export const VersionKey = Object.freeze({
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

  /**
   * A version that is its own key: `width` bytes (1 to `trie.MAX_FIXED_BYTES`), in unsigned byte order; Scala's
   * `VersionKey.bytes` and the Rust SDK's `BytesKey`. A width that is not a number is a `TypeMismatch`; a version that
   * is not bytes is one too, and bytes of another length are `InvalidVersion`, refused by their length, uncopied.
   */
  bytes(width: number): C<VersionKey<Uint8Array>> {
    if (typeof (width as unknown) !== "number") return err(NOT_A_NUMBER);
    if (!Number.isInteger(width) || width < 1 || width > trie.MAX_FIXED_BYTES) return err({ kind: "InvalidVersion", reason: `a bytes version key is 1..${trie.MAX_FIXED_BYTES.toString()} bytes, not ${width.toString()}` });
    return ok(bytesKey(width));
  },
} as const);

/** {@link VersionKey.bytes}: encoding and decoding alike copy bytes of exactly `width`. */
function bytesKey(width: number): VersionKey<Uint8Array> {
  const own = (bytes: Uint8Array): C<Uint8Array> => {
    // a version read in longer than the width is kept as its length alone (ownVersion), refused here as the bytes were
    const b = Oversized.is(bytes) ? bytes.length : ownCopy(bytes, width);
    if (b === undefined) return err(NOT_BYTES);
    const got = typeof b === "number" ? b : b.length;
    return typeof b === "number" || got !== width ? err({ kind: "InvalidVersion", reason: `a version key is ${width.toString()} bytes, got ${got.toString()}` }) : ok(b);
  };
  const key: VersionKey<Uint8Array> = Object.freeze({ width, encode: own, decode: own });
  OWN_KEYS.add(key);
  BYTE_KEYS.set(key, width);
  return key;
}

/** A caller's version key result, checked: a `Result` of its type, its error copied into one of this module's. */
function keyResult<T>(r: unknown, value: (v: unknown) => T | undefined): C<T> {
  const bad = expected("a version key's result to be a Result of its type");
  if (!isObject(r)) return err(bad);
  const isOk: unknown = readGuarded(r, "ok");
  // every read after the first guarded: a getter run by an earlier one may have revoked what is read next, and a
  // REVOKED value, kind or reason is refused, never kept
  if (isOk === true) {
    const read: unknown = readGuarded(r, "value");
    const v = read === REVOKED ? undefined : value(read);
    return v === undefined ? err(bad) : ok(v);
  }
  if (isOk !== false) return err(bad);
  // the caller's error is never handed on as it is: its kind and reason are read once, into a refusal of this module's
  const e: unknown = readGuarded(r, "error");
  // asked once: the kind's getter may revoke the error, whose reason is then REVOKED, never taken for absent
  const isError = isObject(e);
  const kind: unknown = isError ? readGuarded(e, "kind") : undefined;
  const reason: unknown = isError ? readGuarded(e, "reason") : undefined;
  if (e === REVOKED || kind === REVOKED || reason === REVOKED) return err(bad);
  if (kind === "TypeMismatch" && typeof reason === "string") return err(typeMismatch(reason));
  return err({ kind: "InvalidVersion", reason: kind === "InvalidVersion" && typeof reason === "string" ? reason : "the version key refused the version" });
}

/**
 * A caller's version key, checked once (an object whose `width` is an integer from 1 to `trie.MAX_FIXED_BYTES`, the
 * widest the history trie keys, and whose
 * `encode` and `decode` are functions, each read once), as a frozen key whose every result is checked: an encoding
 * must be bytes of `width` (copied), a decoding any value. This module's own keys are used as they are.
 */
function versionKeyArg<V>(x: unknown): Result<VersionKey<V>, TypeMismatch> {
  if (isObject(x) && OWN_KEYS.has(x)) return ok(x as VersionKey<V>);
  const bad = expected("a VersionKey");
  if (!isObject(x)) return err(bad);
  const width: unknown = readGuarded(x, "width");
  const encode: unknown = readGuarded(x, "encode");
  const decode: unknown = readGuarded(x, "decode");
  if (typeof width !== "number" || !Number.isInteger(width) || width < 1 || width > trie.MAX_FIXED_BYTES) return err(bad);
  if (typeof encode !== "function" || typeof decode !== "function" || isRevokedProxy(encode) || isRevokedProxy(decode)) return err(bad);
  return ok(
    Object.freeze({
      width,
      encode: (v: V): C<Uint8Array> =>
        // checked again at each call: caller code may have revoked a Proxy of the function since
        isRevokedProxy(encode)
          ? err(bad)
          : keyResult(Reflect.apply(encode, x, [v]), (b) => {
              // measured before it is copied: an encoding of another width is refused, never copied whole
              const own = ownCopy(b, width);
              return typeof own !== "number" && own?.length === width ? own : undefined;
            }),
      decode: (b: Uint8Array): C<V> => {
        // exposed as the profile's versionKey: its input is a caller's too, so it passes core's boundary within the
        // key's width (owned, measured by its internal length) before the caller's decoder sees a copy of its own
        const own = ownCopy(b, width);
        if (own === undefined) return err(expected("a Uint8Array"));
        const got = typeof own === "number" ? own : own.length;
        if (typeof own === "number" || got !== width) return err({ kind: "InvalidVersion", reason: `a version key is ${width.toString()} bytes, got ${got.toString()}` });
        return isRevokedProxy(decode) ? err(bad) : keyResult(Reflect.apply(decode, x, [own]), (v) => v as V);
      },
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

/**
 * The top trie's slots: each the one byte of its key, as a number (as Rust's `u8`s), since a module's bytes would be
 * shared with every caller and no typed array of any elements can be frozen. The module keeps its own keys.
 */
export const VERSION_SLOT = 0x00;
/** The state root's slot. */
export const STATE_SLOT = 0x01;
/** The history root's slot. */
export const HISTORY_SLOT = 0x02;
const SLOTS = Object.freeze({ version: Uint8Array.of(VERSION_SLOT), state: Uint8Array.of(STATE_SLOT), history: Uint8Array.of(HISTORY_SLOT) });

const slotName = (slot: Uint8Array): string => toHexOf(slot);

/**
 * A digest a caller passes (a root, a head's or a claim's), copied through core's boundary within the profile's `size`,
 * as a {@link Measured} value (an `Oversized` past the bound, never copied); `undefined` for what is not a `Uint8Array`.
 * The boundary refuses nothing past the bound: a digest is hashed, compared, encoded or returned only once a size check
 * has narrowed it to bytes, at the stage the wire form checks it.
 */
function digestField(x: unknown, size: number): Measured | undefined {
  const own = ownCopy(x, size);
  return own === undefined ? undefined : measured(own);
}

/** A head as a caller passes it, its roots measured: a {@link Head} only once both are sized ({@link VersionedTrie}'s `#sizedHead`). */
interface MeasuredHead<V> {
  readonly version: V;
  readonly stateRoot: Measured;
  readonly historyRoot: Measured;
}

/** A version asked about, as a caller's proof gives it: its state root measured. */
interface MeasuredAt<V> {
  readonly version: V;
  readonly stateRoot?: Measured;
}

/** A caller's proof as read: its head and its `at` measured, its tries this package's own. */
interface MeasuredProof<V> {
  readonly head: MeasuredHead<V>;
  readonly top: trie.TrieProof;
  readonly at?: MeasuredAt<V>;
  readonly history?: trie.TrieProof;
  readonly state?: trie.TrieProof;
}

/** A caller's delta as read: its heads measured. */
interface MeasuredDelta<V> {
  readonly parent?: MeasuredHead<V>;
  readonly head: MeasuredHead<V>;
  readonly upserts: readonly (readonly [StateKey, Value])[];
  readonly removes: readonly StateKey[];
}

/** A snapshot chunk as read: a head, or versions, with their roots measured. */
type MeasuredChunk<V> =
  | { readonly kind: "head"; readonly head: MeasuredHead<V> }
  | { readonly kind: "state"; readonly entries: readonly (readonly [StateKey, Value])[] }
  | { readonly kind: "history"; readonly versions: readonly (readonly [V, Measured])[] };

/** A caller's snapshot as read: its head and its versions' roots measured. */
interface MeasuredSnapshot<V> {
  readonly head: MeasuredHead<V>;
  readonly entries: readonly (readonly [StateKey, Value])[];
  readonly history: readonly (readonly [V, Measured])[];
}

/** The profile's own checks of measured heads, for the reader and the replica. */
let sameHeadOf!: <V>(p: VersionedTrie<V>, a: MeasuredHead<V>, b: MeasuredHead<V>) => C<boolean>;
let rootOfHead!: <V>(p: VersionedTrie<V>, h: MeasuredHead<V>) => C<Uint8Array>;
let sizedHeadOf!: <V>(p: VersionedTrie<V>, h: MeasuredHead<V>) => R<Head<V>>;

/**
 * A version of `key`'s as it is read in or handed out. One of a `VersionKey.bytes` key is the caller's bytes until it
 * is copied: it is copied once, as it is read, through core's boundary (from any realm, by its internal slots, so no
 * Proxy trap, subclass accessor or iterator runs), and again as it is handed out, as every root is, so nothing the
 * caller does to an array it passed or was given changes one kept. One longer than the key's width is kept as its
 * length alone (an `Oversized`, never copied), which the key refuses as it refused the bytes. A `bigint` needs no copy,
 * and a caller's own key's version is the caller's own value, which the key checks when it encodes it.
 */
function ownVersion<V>(key: VersionKey<V>, v: V): V {
  const width = BYTE_KEYS.get(key);
  if (width === undefined) return v;
  const own = ownCopy(v, width);
  return (typeof own === "object" ? own : typeof own === "number" ? measured(own) : v) as V;
}

/** A caller's head, its fields read once into a frozen copy: the version (checked when encoded) and two roots (copied). */
function headArg<V>(x: unknown, what: string, p: VersionedTrie<V>): Result<MeasuredHead<V>, TypeMismatch> {
  if (!isObject(x)) return err(expected(`${what} to be a Head`));
  // each read guarded: a getter run by an earlier read may have revoked the head; a byte version copied as read
  const version: unknown = ownVersion(p.versionKey, readGuarded(x, "version"));
  // each root owned within the profile's size, else Oversized (never copied), which its size check refuses
  const stateRoot = digestField(readGuarded(x, "stateRoot"), p.hashing.size);
  const historyRoot = digestField(readGuarded(x, "historyRoot"), p.hashing.size);
  if (stateRoot === undefined || historyRoot === undefined) return err(expected(`${what}'s roots to be Uint8Arrays`));
  return ok(Object.freeze({ version: version as V, stateRoot, historyRoot }));
}

/** An optional `TrieProof` field: absent (`undefined`), or one made by `trie.ts`. */
function optionalProof(x: unknown, what: string): Result<trie.TrieProof | undefined, TypeMismatch> {
  return x === undefined ? ok(undefined) : TRIE.isProof(x) ? ok(x) : err(expected(`${what} to be a TrieProof`));
}

/** A caller's proof, its fields read once into a frozen copy: a head, the top `TrieProof`, and the optional parts. */
function proofArg<V>(x: unknown, p: VersionedTrie<V>): Result<MeasuredProof<V>, TypeMismatch> {
  if (!isObject(x)) return err(expected("a VersionedTrieProof"));
  const head = headArg<V>(readGuarded(x, "head"), "the proof's head", p);
  if (!head.ok) return head;
  const top: unknown = readGuarded(x, "top");
  if (!TRIE.isProof(top)) return err(expected("the proof's top to be a TrieProof"));
  const history = optionalProof(readGuarded(x, "history"), "the proof's history");
  if (!history.ok) return history;
  const state = optionalProof(readGuarded(x, "state"), "the proof's state");
  if (!state.ok) return state;
  const atField: unknown = readGuarded(x, "at");
  let at: MeasuredAt<V> | undefined;
  if (atField !== undefined) {
    if (!isObject(atField)) return err(expected("the proof's at to be an At"));
    const version: unknown = ownVersion(p.versionKey, readGuarded(atField, "version"));
    const s: unknown = readGuarded(atField, "stateRoot");
    if (version === REVOKED || s === REVOKED) return err(expected("the proof's at to be an At"));
    const stateRoot = s === undefined ? undefined : digestField(s, p.hashing.size);
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
const entriesArg = <E = never>(x: unknown, what: string, bound?: (length: number) => E | undefined): Result<(readonly [StateKey, Value])[], E | TypeMismatch> =>
  readArray<readonly [StateKey, Value], E | TypeMismatch>(
    x,
    what,
    (e, i) => {
    const pair = readPair(e);
    if (pair === undefined) return err(expectedAt("a [StateKey, Value] array", i));
    const k: unknown = readGuarded(pair, 0);
    const v: unknown = readGuarded(pair, 1);
    return isKey(k) && isValue(v) ? ok([k, v] as const) : err(expectedAt("a [StateKey, Value] array", i));
    },
    bound,
  );

/** A caller's versions: an array of `[V, Uint8Array]` arrays, read by index, each root copied. */
const versionsArg = <V, E = never>(x: unknown, what: string, p: VersionedTrie<V>, bound?: (length: number) => E | undefined): Result<(readonly [V, Measured])[], E | TypeMismatch> =>
  readArray<readonly [V, Measured], E | TypeMismatch>(
    x,
    what,
    (e, i) => {
    const pair = readPair(e);
    // the version first, then the root, copied as read: nothing read after it can change it
    const version: unknown = pair === undefined ? undefined : ownVersion(p.versionKey, readGuarded(pair, 0));
    // owned within the profile's size, else Oversized (never copied), which the reader's size check refuses
    const root = pair === undefined ? undefined : digestField(readGuarded(pair, 1), p.hashing.size);
    // a version is any value, checked when encoded: REVOKED is refused here, never kept as one
    return pair === undefined || root === undefined || version === REVOKED ? err(expectedAt("a [version, Uint8Array] array", i)) : ok([version as V, root] as const);
    },
    bound,
  );

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
const NOT_A_STATE = expected("a VerifiedState");

/** A state proof checked against its state root: what it shows about keys, namespaces and ranges. */
export class VerifiedState {
  readonly #proof: trie.VerifiedTrieProof;
  readonly #hashing: trie.TrieHashing;
  private constructor(token: Token, proof: trie.VerifiedTrieProof, hashing: trie.TrieHashing) {
    this.#proof = proof;
    this.#hashing = hashing;
    // without the token no method answers: a state built from outside never stands for a verified proof
    if (token === TOKEN) STATE.stamp(this);
    Object.freeze(this);
  }

  static {
    createState = (p, h) => new VerifiedState(TOKEN, p, h);
  }

  /** The key's value hash, or `null` if it is proven absent; not covered is an error. What is not a `StateKey` is a `TypeMismatch`. */
  lookup(key: StateKey): C<Uint8Array | null> {
    if (!STATE.has(this)) return err(NOT_A_STATE);
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
    if (!STATE.has(this)) return err(NOT_A_STATE);
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
    if (!STATE.has(this)) return err(NOT_A_STATE);
    if (!isNamespace(ns)) return err(expected("a StateNamespace"));
    return keyed(`namespace ${ns.preview().short()}`, TRIE.prefix(this.#proof, ns.prefix()));
  }

  /** Every entry in `[from, until)`, complete; no `until` is unbounded. Bounds that are not `StateKey`s are a `TypeMismatch`. */
  range(from: StateKey, until?: StateKey): C<readonly StateEntry[]> {
    if (!STATE.has(this)) return err(NOT_A_STATE);
    const f = keyArg(from);
    if (!f.ok) return f;
    const u = until === undefined ? ok(undefined) : keyArg(until);
    if (!u.ok) return u;
    return keyed(`range [${f.value.preview().short()}, ${u.value?.preview().short() ?? ""})`, TRIE.range(this.#proof, f.value.bytes(), u.value?.bytes()));
  }
}
Object.freeze(VerifiedState);
Object.freeze(VerifiedState.prototype);

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
/** The refusal of a profile method called on a profile built by calling the constructor from outside. */
const NOT_A_PROFILE = expected("a VersionedTrie");

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
    // without the token none of the caller's arguments is read or kept: the fields are this module's own defaults
    const own = token === TOKEN;
    this.hashing = own ? hashing : TRIE.defaultHashing;
    if (!own) this.versionKey = VersionKey.long as VersionKey<V>;
    [this.topMode, this.stateMode, this.historyMode] = own ? modes : [TRIE.defaultMode, TRIE.defaultMode, TRIE.defaultMode];
    if (own) PROFILE.stamp(this);
    Object.freeze(this);
  }

  static {
    isProfile = (x): x is VersionedTrie<unknown> => PROFILE.has(x);
    sameHeadOf = (p, a, b) => p.#sameHead(a, b);
    rootOfHead = (p, h) => p.#root(h);
    sizedHeadOf = (p, h) => p.#sizedHead(h);
  }

  /**
   * The profile over `algorithm` and `versionKey`. An algorithm that is not a `HashAlgorithm`, or a version key that is
   * not an object with an integer `width` (1 to `trie.MAX_FIXED_BYTES`) and `encode` and `decode` functions, is a
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
    if (!PROFILE.has(this)) return err(NOT_A_PROFILE);
    const v = valueArg(value);
    return v.ok ? viaTrie(TRIE.valueHash(this.hashing, v.value)) : v;
  }

  /** The hash a trie commits for a byte string: the top trie's slots and the history trie's state roots. */
  bytesHash(b: Uint8Array): Result<Uint8Array, TypeMismatch> {
    if (!PROFILE.has(this)) return err(NOT_A_PROFILE);
    // a byte string of any length is committed
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
    if (!PROFILE.has(this)) return err(NOT_A_PROFILE);
    const k = keyArg(key);
    if (!k.ok) return k;
    const v = valueArg(value);
    return v.ok ? checkValue(k.value, v.value) : v;
  }

  /** The root a head commits to: the top trie of `0x00 -> vkey`, `0x01 -> stateRoot`, `0x02 -> historyRoot`. */
  root(head: Head<V>): C<Uint8Array> {
    if (!PROFILE.has(this)) return err(NOT_A_PROFILE);
    const h = headArg<V>(head, "the head", this);
    return h.ok ? this.#root(h.value) : h;
  }

  #root(head: MeasuredHead<V>): C<Uint8Array> {
    const vkey = this.versionKey.encode(head.version);
    if (!vkey.ok) return vkey;
    const sized = this.#sizedHead(head);
    return sized.ok ? this.#rootOf(vkey.value, sized.value) : sized;
  }

  /**
   * A measured head as a {@link Head}: both roots of the profile's digest size, or `Malformed` as the wire form refuses
   * them (after the version). The one way from a caller's head to one that is committed, encoded, compared or kept.
   */
  #sizedHead(head: MeasuredHead<V>): R<Head<V>> {
    const size = this.hashing.size;
    const { stateRoot, historyRoot } = head;
    if (!isSized(stateRoot, size)) return malformed(`a digest is ${size.toString()} bytes, got ${stateRoot.length.toString()}`);
    if (!isSized(historyRoot, size)) return malformed(`a digest is ${size.toString()} bytes, got ${historyRoot.length.toString()}`);
    return ok(Object.freeze({ version: head.version, stateRoot, historyRoot }));
  }

  /** The root of a head whose version is already encoded (`vkey`, bytes the module owns): no version is read again. */
  #rootOf(vkey: Uint8Array, head: Head<V>): R<Uint8Array> {
    return viaTrie(
      TRIE.trieRoot(this.hashing, this.topMode, [
        [SLOTS.version, this.#bytesHash(vkey)],
        [SLOTS.state, this.#bytesHash(head.stateRoot)],
        [SLOTS.history, this.#bytesHash(head.historyRoot)],
      ]),
    );
  }

  /** Whether two heads are equal: the same version key and roots. Heads are checked as {@link root} checks one. */
  sameHead(a: Head<V>, b: Head<V>): C<boolean> {
    if (!PROFILE.has(this)) return err(NOT_A_PROFILE);
    const x = headArg<V>(a, "the first head", this);
    if (!x.ok) return x;
    const y = headArg<V>(b, "the second head", this);
    return y.ok ? this.#sameHead(x.value, y.value) : y;
  }

  #sameHead(ma: MeasuredHead<V>, mb: MeasuredHead<V>): C<boolean> {
    const ka = this.versionKey.encode(ma.version);
    if (!ka.ok) return ka;
    const kb = this.versionKey.encode(mb.version);
    if (!kb.ok) return kb;
    const a = this.#sizedHead(ma);
    if (!a.ok) return a;
    const b = this.#sizedHead(mb);
    if (!b.ok) return b;
    return ok(bytesEqual(ka.value, kb.value) && bytesEqual(a.value.stateRoot, b.value.stateRoot) && bytesEqual(a.value.historyRoot, b.value.historyRoot));
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
    if (!PROFILE.has(this)) return err(NOT_A_PROFILE);
    const v = valueArg(value);
    if (!v.ok) return v;
    const w = decodeInput(models.versionedTrie.VersionedTrieHead, v.value);
    return w.ok ? this.#fromWireHead(w.value) : malformed(describeFailure(w.error));
  }

  /** A head's canonical value; a head is checked as {@link root} checks one. */
  encodeHead(head: Head<V>): C<Value> {
    if (!PROFILE.has(this)) return err(NOT_A_PROFILE);
    const h = headArg<V>(head, "the head", this);
    if (!h.ok) return h;
    const v = this.versionKey.encode(h.value.version);
    if (!v.ok) return v;
    const sized = this.#sizedHead(h.value);
    if (!sized.ok) return sized;
    const e = models.versionedTrie.VersionedTrieHead.encode({ v: v.value, s: sized.value.stateRoot, h: sized.value.historyRoot });
    return e.ok ? e : malformed(e.error.kind);
  }

  /** A proof from its canonical value (`versioned-trie-proof`); one heavier than `MAX_PROOF_WEIGHT` is `Malformed`. */
  decodeProof(value: Value): C<VersionedTrieProof<V>> {
    // a profile built by calling the constructor from outside holds the caller's hashing and modes: it never mints a proof
    if (!PROFILE.has(this)) return err(NOT_A_PROFILE);
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
    if (!PROFILE.has(this)) return err(NOT_A_PROFILE);
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
    if (!PROFILE.has(this)) return err(NOT_A_PROFILE);
    // within the chunk bound: a longer chunk is refused by its length, uncopied
    const b = ownBytes(bytes, MAX_CHUNK_BYTES);
    if (b === undefined) return err(NOT_BYTES);
    return typeof b === "number" ? err({ kind: "TooLarge", what: "a snapshot chunk", limit: MAX_CHUNK_BYTES }) : this.#decodeChunk(b);
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
  #args(root: unknown, proof: unknown): Result<readonly [Measured, MeasuredProof<V>], TypeMismatch> {
    // the trusted root copied first: the proof's getters, and a caller's version key later, are caller code
    const r = digestField(root, this.hashing.size);
    if (r === undefined) return err(NOT_BYTES);
    const p = proofArg<V>(proof, this);
    return p.ok ? ok([r, p.value]) : p;
  }

  /**
   * The trusted root (copied) and the version asked about, encoded once into bytes the module owns, both before the
   * proof's fields are read: a proof's getter (caller code) cannot change the query, which is only ever used as these
   * bytes from then on.
   */
  #asking(root: unknown, u: V): C<readonly [Measured, Uint8Array]> {
    const r = digestField(root, this.hashing.size);
    if (r === undefined) return err(NOT_BYTES);
    const k = this.versionKey.encode(u);
    return k.ok ? ok([r, k.value]) : k;
  }

  /**
   * Rule 1: the head binds to `root`, and the proof's top trie holds the head's values in slots 0x00..0x02. The head,
   * with its version's key, encoded once here and used as these bytes from then on.
   */
  #bind(root: Measured, proof: MeasuredProof<V>): C<{ readonly head: Head<V>; readonly vkey: Uint8Array }> {
    // the head's version encoded once: its root, its slot and every history check after use these bytes, so a version
    // (a caller's object, a Proxy) is never read again
    const encoded = this.versionKey.encode(proof.head.version);
    if (!encoded.ok) return encoded;
    const vkey = encoded.value;
    // a head binds only with roots of the profile's size (the wire form refuses others after the version)
    const sized = this.#sizedHead(proof.head);
    if (!sized.ok) return sized;
    const head = sized.value;
    const claimed = this.#rootOf(vkey, head);
    if (!claimed.ok) return claimed;
    // a trusted root past the digest's size (Oversized) matches no head's root
    if (!isMeasuredBytes(root) || !bytesEqual(claimed.value, root)) return mismatch("root", root, claimed.value);
    const top = this.#verified(this.topMode, root, "top", proof.top);
    if (!top.ok) return top;
    const slots: readonly (readonly [Uint8Array, Uint8Array])[] = [
      [SLOTS.version, vkey],
      [SLOTS.state, head.stateRoot],
      [SLOTS.history, head.historyRoot],
    ];
    for (const [slot, value] of slots) {
      const l = viaTrie(TRIE.lookup(top.value, slot));
      if (!l.ok) return l;
      if (l.value.kind === "notCovered") return notCovered(`top slot ${slotName(slot)}`);
      if (l.value.kind !== "present" || !bytesEqual(l.value.valueHash, this.#bytesHash(value))) return malformed(`top slot ${slotName(slot)} does not hold the head's value`);
    }
    return ok({ head, vkey });
  }

  #history(head: Head<V>, proof: MeasuredProof<V>): R<trie.VerifiedTrieProof> {
    return this.#verified(this.historyMode, head.historyRoot, "history", proof.history);
  }

  /** The proof's `at`, if it names the version asked about, whose key (`ukey`) the rule encoded before reading the proof. */
  #asked(proof: MeasuredProof<V>, ukey: Uint8Array): C<MeasuredAt<V>> {
    const at = proof.at;
    if (at === undefined) return malformed("the proof names no version");
    const a = this.versionKey.encode(at.version);
    if (!a.ok) return a;
    return bytesEqual(a.value, ukey) ? ok(at) : malformed(`the proof is about version ${toHexOf(a.value)}, not ${toHexOf(ukey)}`);
  }

  /** The state root of the version keyed `ukey` per the history trie: the root if committed, `null` if proven never committed. */
  #versionRoot(head: Head<V>, proof: MeasuredProof<V>, ukey: Uint8Array): C<Uint8Array | null> {
    const at = this.#asked(proof, ukey);
    if (!at.ok) return at;
    const hist = this.#history(head, proof);
    if (!hist.ok) return hist;
    const l = viaTrie(TRIE.lookup(hist.value, ukey));
    if (!l.ok) return l;
    const k = toHexOf(ukey);
    const s = at.value.stateRoot;
    switch (l.value.kind) {
      case "notCovered":
        return notCovered(`version ${k}`);
      case "present":
        if (s === undefined) return malformed(`version ${k} is committed; the proof claims it is not`);
        // only a state root of the profile's size is hashed, and returned: a committed root has that size
        if (!isSized(s, this.hashing.size)) return malformed(`a digest is ${this.hashing.size.toString()} bytes, got ${s.length.toString()}`);
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
    if (!PROFILE.has(this)) return err(NOT_A_PROFILE);
    const a = this.#args(root, proof);
    if (!a.ok) return a;
    const [r, p] = a.value;
    const bound = this.#bind(r, p);
    if (!bound.ok) return bound;
    const { head, vkey } = bound.value;
    const hist = this.#history(head, p);
    if (!hist.ok) return hist;
    const tail = TRIE.range(hist.value, vkey, undefined);
    if (tail.kind === "notCovered") return notCovered("history from the head version up");
    return sameEntries(tail.entries, [{ key: vkey, valueHash: this.#bytesHash(head.stateRoot) }])
      ? ok(head)
      : malformed("the history trie does not end at the head version with the head's state root");
  }

  /** Rule 2: a proof about the current state, checked against the head's state root. Arguments as {@link verifyHead}. */
  verifyState(root: Uint8Array, proof: VersionedTrieProof<V>): C<VerifiedState> {
    if (!PROFILE.has(this)) return err(NOT_A_PROFILE);
    const a = this.#args(root, proof);
    if (!a.ok) return a;
    const [r, p] = a.value;
    const bound = this.#bind(r, p);
    if (!bound.ok) return bound;
    if (p.at !== undefined) return malformed("a historical proof is not about the current state");
    const state = this.#verified(this.stateMode, bound.value.head.stateRoot, "state", p.state);
    return state.ok ? ok(createState(state.value, this.hashing)) : state;
  }

  /**
   * Rule 3: version `u`'s state root if the head's history committed it, `null` if it provably never did. Arguments as
   * {@link verifyHead}; `u` is checked by the version key.
   */
  verifyVersion(root: Uint8Array, proof: VersionedTrieProof<V>, u: V): C<Uint8Array | null> {
    if (!PROFILE.has(this)) return err(NOT_A_PROFILE);
    const q = this.#asking(root, u);
    if (!q.ok) return q;
    const [r, ukey] = q.value;
    const p = proofArg<V>(proof, this);
    if (!p.ok) return p;
    const bound = this.#bind(r, p.value);
    return bound.ok ? this.#versionRoot(bound.value.head, p.value, ukey) : bound;
  }

  /** Rule 4: the state of a past version `u`, its root taken from the history trie. Arguments as {@link verifyVersion}. */
  verifyHistoricalKey(root: Uint8Array, proof: VersionedTrieProof<V>, u: V): C<VerifiedState> {
    if (!PROFILE.has(this)) return err(NOT_A_PROFILE);
    const q = this.#asking(root, u);
    if (!q.ok) return q;
    const [r, ukey] = q.value;
    const p = proofArg<V>(proof, this);
    if (!p.ok) return p;
    const bound = this.#bind(r, p.value);
    if (!bound.ok) return bound;
    const s = this.#versionRoot(bound.value.head, p.value, ukey);
    if (!s.ok) return s;
    if (s.value === null) return err({ kind: "UnknownVersion", version: versionHex(ukey) });
    const state = this.#verified(this.stateMode, s.value, "state", p.value.state);
    return state.ok ? ok(createState(state.value, this.hashing)) : state;
  }

  /**
   * Rule 6: whether `claimed` is exactly the committed versions in `[from, until)` with their state roots, in order.
   * Arguments as {@link verifyVersion}; `claimed` must be an array of `[version, Uint8Array]` arrays (read by index).
   */
  verifyVersions(root: Uint8Array, proof: VersionedTrieProof<V>, from: V, until: V | null, claimed: readonly (readonly [V, Uint8Array])[]): C<boolean> {
    if (!PROFILE.has(this)) return err(NOT_A_PROFILE);
    // the trusted root copied and the query encoded before anything else is read: the claims' and the proof's getters
    // are caller code, and from here the query is only these bytes
    const q = this.#asking(root, from);
    if (!q.ok) return q;
    const [r, fk] = q.value;
    const uk = until === null ? ok(undefined) : this.versionKey.encode(until);
    if (!uk.ok) return uk;
    // each claim's version encoded as soon as it is read, before its root or a later claim is read (their getters, or
    // the proof's, may change a version object read before), and its root copied: only these bytes are compared
    const claims = readArray(claimed, "an array of [version, Uint8Array] claims", (e, i): C<trie.TrieEntry | null> => {
      const bad = err(expectedAt("a [version, Uint8Array] array", i));
      const pair = readPair(e);
      if (pair === undefined) return bad;
      const version: unknown = readGuarded(pair, 0);
      if (version === REVOKED) return bad;
      const k = this.versionKey.encode(version as V);
      if (!k.ok) return k;
      // hashed only if it is a digest of the profile's size: any other claim of a root matches no listing
      const s = digestField(readGuarded(pair, 1), this.hashing.size);
      if (s === undefined) return bad;
      return ok(isSized(s, this.hashing.size) ? { key: k.value, valueHash: this.#bytesHash(s) } : null);
    });
    if (!claims.ok) return claims;
    const sizedClaims = claims.value.filter((c): c is trie.TrieEntry => c !== null);
    // a claim whose root is not a digest of the profile's size is never the listing (decided after the proof's checks)
    const unmatched = sizedClaims.length !== claims.value.length;
    const p = proofArg<V>(proof, this);
    if (!p.ok) return p;
    const bound = this.#bind(r, p.value);
    if (!bound.ok) return bound;
    const hist = this.#history(bound.value.head, p.value);
    if (!hist.ok) return hist;
    const listing = TRIE.range(hist.value, fk, uk.value);
    if (listing.kind === "notCovered") return notCovered(`versions [${toHexOf(fk)}, ${uk.value === undefined ? "" : toHexOf(uk.value)})`);
    return ok(!unmatched && sameEntries(listing.entries, sizedClaims));
  }

  /**
   * Rule 5: the latest committed version `p <= t` and its state root, or `null` if every committed version is after
   * `t`. The history part lists `[vkey(p), vkey(t) || 0x00)` as exactly `p`. Arguments as {@link verifyVersion}.
   */
  verifyAsOf(root: Uint8Array, proof: VersionedTrieProof<V>, t: V): C<{ readonly version: V; readonly stateRoot: Uint8Array } | null> {
    if (!PROFILE.has(this)) return err(NOT_A_PROFILE);
    const q = this.#asking(root, t);
    if (!q.ok) return q;
    const [r, tkey] = q.value;
    const pa = proofArg<V>(proof, this);
    if (!pa.ok) return pa;
    const p = pa.value;
    const b = this.#bind(r, p);
    if (!b.ok) return b;
    const hist = this.#history(b.value.head, p);
    if (!hist.ok) return hist;
    const bound = concatBytes([tkey, Uint8Array.of(0)]);
    let found: { readonly pkey: Uint8Array; readonly version: V; readonly stateRoot: Measured } | undefined;
    if (p.at !== undefined) {
      const pkey = this.versionKey.encode(p.at.version);
      if (!pkey.ok) return pkey;
      const s = p.at.stateRoot;
      if (s === undefined) return malformed("an as-of answer names its state root");
      if (compareBytes(pkey.value, tkey) > 0) return malformed(`the answer ${toHexOf(pkey.value)} is after ${toHexOf(tkey)}`);
      found = { pkey: pkey.value, version: p.at.version, stateRoot: s };
    }
    const listing = TRIE.range(hist.value, found?.pkey ?? new Uint8Array(0), bound);
    if (listing.kind === "notCovered") return notCovered(`history up to ${toHexOf(tkey)}`);
    const notTheAnswer = malformed(`the history up to ${toHexOf(tkey)} is not the claimed answer`);
    if (found === undefined) return sameEntries(listing.entries, []) ? ok(null) : notTheAnswer;
    // only a state root of the profile's size is hashed, and returned: an answer with any other is never the history's
    const answer = found.stateRoot;
    if (!isSized(answer, this.hashing.size)) return notTheAnswer;
    if (!sameEntries(listing.entries, [{ key: found.pkey, valueHash: this.#bytesHash(answer) }])) return notTheAnswer;
    return ok({ version: found.version, stateRoot: answer });
  }
}
Object.freeze(VersionedTrie);
Object.freeze(VersionedTrie.prototype);

// ------------------------------------------------------------------ snapshots (section 7)

type Stage = "head" | "state" | "history" | "done";

/**
 * A chunk's size bound, checked on its length (read once) before any item is read, as {@link SnapshotReader.feed}
 * refuses a chunk outside it: a caller's array is never walked past `MAX_CHUNK_ENTRIES`.
 */
const chunkBound =
  (kind: "state" | "history") =>
  (length: number): VersionedTrieError | undefined =>
    length >= 1 && length <= MAX_CHUNK_ENTRIES ? undefined : { kind: "Malformed", reason: `snapshot: a ${kind} chunk holds 1..${MAX_CHUNK_ENTRIES.toString()} items, not ${length.toString()}` };

/** A caller's chunk, its fields read once into a frozen copy: its kind, then its head, entries or versions. */
function chunkArg<V>(x: unknown, p: VersionedTrie<V>): C<MeasuredChunk<V>> {
  if (!isObject(x)) return err(expected("a SnapshotChunk"));
  const kind: unknown = readGuarded(x, "kind");
  switch (kind) {
    case "head": {
      const head = headArg<V>(readGuarded(x, "head"), "the chunk's head", p);
      return head.ok ? ok(Object.freeze({ kind, head: head.value })) : head;
    }
    case "state": {
      const entries = entriesArg(readGuarded(x, "entries"), "the chunk's entries", chunkBound("state"));
      return entries.ok ? ok(Object.freeze({ kind, entries: entries.value })) : entries;
    }
    case "history": {
      const versions = versionsArg<V, VersionedTrieError>(readGuarded(x, "versions"), "the chunk's versions", p, chunkBound("history"));
      return versions.ok ? ok(Object.freeze({ kind, versions: versions.value })) : versions;
    }
    default:
      return err(expected("a SnapshotChunk of kind head, state or history"));
  }
}

const NOT_A_READER = expected("a SnapshotReader");
let startReader!: <V>(profile: VersionedTrie<V>, trusted: Head<V>) => SnapshotReader<V>;
let feedChunk!: <V>(r: SnapshotReader<V>, chunk: MeasuredChunk<V>) => C<SnapshotReader<V>>;
let finishReader!: <V>(r: SnapshotReader<V>) => R<Snapshot<V>>;
/** The keys of a reader's history versions, in order, each encoded once as it was fed: no version is encoded again. */
let readerKeys!: <V>(r: SnapshotReader<V>) => readonly Uint8Array[];

/**
 * Checks a snapshot chunk by chunk against a head the caller already trusts, refusing it at the first chunk that
 * breaks a rule: one head chunk first, the trusted head; then state chunks with strictly increasing keys and no null,
 * oversized or overdeep value; then history chunks with strictly increasing versions, none after the trusted version,
 * which ends the snapshot with the trusted state root; each chunk holding 1 to 1024 items. The roots are checked by
 * {@link Replica.fromSnapshot} once the reader has finished.
 */
export class SnapshotReader<V> {
  // runtime-private: the trusted head's roots, the keys and the chunks the reader has accepted are its own (shared with
  // the readers it was fed from), reachable through no property; `trusted` and `finish` hand out copies
  readonly #trusted: Head<V>;
  readonly #stage: Stage;
  readonly #lastKey: Uint8Array | undefined;
  readonly #lastVersion: Uint8Array | undefined;
  readonly #stateChunks: readonly (readonly (readonly [StateKey, Value])[])[];
  readonly #historyChunks: readonly (readonly (readonly [V, Uint8Array])[])[];
  readonly #historyKeys: readonly (readonly Uint8Array[])[];
  readonly #headKey: Uint8Array | undefined;

  private constructor(
    token: Token,
    readonly profile: VersionedTrie<V>,
    trusted: Head<V>,
    stage: Stage,
    lastKey: Uint8Array | undefined,
    lastVersion: Uint8Array | undefined,
    stateChunks: readonly (readonly (readonly [StateKey, Value])[])[],
    historyChunks: readonly (readonly (readonly [V, Uint8Array])[])[],
    historyKeys: readonly (readonly Uint8Array[])[],
    headKey: Uint8Array | undefined,
  ) {
    this.#trusted = trusted;
    this.#stage = stage;
    this.#lastKey = lastKey;
    this.#lastVersion = lastVersion;
    this.#stateChunks = stateChunks;
    this.#historyChunks = historyChunks;
    this.#historyKeys = historyKeys;
    this.#headKey = headKey;
    if (token === TOKEN) READER.stamp(this);
    Object.freeze(this);
  }

  /** The head the reader checks the snapshot against (a copy). */
  get trusted(): Head<V> {
    return copyHead(this.#trusted, this.profile.versionKey);
  }

  static {
    startReader = <V>(profile: VersionedTrie<V>, trusted: Head<V>) => new SnapshotReader(TOKEN, profile, trusted, "head", undefined, undefined, [], [], [], undefined);
    feedChunk = (r, chunk) => r.#feed(chunk);
    finishReader = (r) => r.#finish();
    readerKeys = (r) => r.#historyKeys.flat();
  }

  /**
   * A reader against `trusted` under `profile`. A profile that is not a `VersionedTrie`, or a head that is not a `Head`
   * (its fields read once, its roots copied), is a `TypeMismatch`.
   */
  static start<V>(profile: VersionedTrie<V>, trusted: Head<V>): C<SnapshotReader<V>> {
    const p = profileArg<V>(profile);
    if (!p.ok) return p;
    const h = headArg<V>(trusted, "the trusted head", p.value);
    if (!h.ok) return h;
    // the reader keeps the trusted head: only one with roots of the profile's size (Malformed otherwise)
    const sized = sizedHeadOf(p.value, h.value);
    return sized.ok ? ok(startReader(p.value, sized.value)) : sized;
  }

  #next(changes: {
    readonly stage?: Stage;
    readonly lastKey?: Uint8Array;
    readonly lastVersion?: Uint8Array;
    readonly state?: readonly (readonly [StateKey, Value])[];
    readonly history?: readonly (readonly [V, Uint8Array])[];
    readonly keys?: readonly Uint8Array[];
    readonly headKey?: Uint8Array;
  }): SnapshotReader<V> {
    return new SnapshotReader(
      TOKEN,
      this.profile,
      this.#trusted,
      changes.stage ?? this.#stage,
      changes.lastKey ?? this.#lastKey,
      changes.lastVersion ?? this.#lastVersion,
      changes.state === undefined ? this.#stateChunks : [...this.#stateChunks, changes.state],
      changes.history === undefined ? this.#historyChunks : [...this.#historyChunks, changes.history],
      changes.keys === undefined ? this.#historyKeys : [...this.#historyKeys, changes.keys],
      changes.headKey ?? this.#headKey,
    );
  }

  /** Decodes and feeds one chunk of canonical CBOR; what is not a `Uint8Array` is a `TypeMismatch`. */
  feedBytes(bytes: Uint8Array): C<SnapshotReader<V>> {
    if (!READER.has(this)) return err(NOT_A_READER);
    const c = this.profile.decodeChunk(bytes);
    return c.ok ? this.#feed(c.value) : c;
  }

  /** Feeds one chunk; one that is not a `SnapshotChunk` (its fields read once into a copy) is a `TypeMismatch`. */
  feed(chunk: SnapshotChunk<V>): C<SnapshotReader<V>> {
    if (!READER.has(this)) return err(NOT_A_READER);
    const c = chunkArg<V>(chunk, this.profile);
    return c.ok ? this.#feed(c.value) : c;
  }

  #feed(chunk: MeasuredChunk<V>): C<SnapshotReader<V>> {
    const bad = (reason: string): R<never> => malformed(`snapshot: ${reason}`);
    const key = this.profile.versionKey;
    if (this.#stage === "head") {
      if (chunk.kind !== "head") return bad("it starts with its head");
      // the trusted head itself (as a restore from a snapshot feeds it) is the same head, its version never read again
      const same = chunk.head === this.#trusted ? ok(true) : sameHeadOf(this.profile, chunk.head, this.#trusted);
      if (!same.ok) return same;
      return same.value ? ok(this.#next({ stage: "state" })) : bad("its head is not the trusted head");
    }
    if (chunk.kind === "head") return bad("it has one head");
    if (this.#stage === "state" && chunk.kind === "state") {
      const es = chunk.entries;
      if (es.length < 1 || es.length > MAX_CHUNK_ENTRIES) return bad(`a state chunk holds 1..${MAX_CHUNK_ENTRIES.toString()} items, not ${es.length.toString()}`);
      for (const [k, v] of es) {
        if (v.kind === "null") return bad(`${k.preview().short()} is null`);
        const c = checkValue(k, v);
        if (!c.ok) return c;
      }
      let previous = this.#lastKey;
      for (const [k] of es) {
        const b = k.bytes();
        if (previous !== undefined && compareBytes(previous, b) >= 0) return bad(`key ${Preview.of(toHexOf(b)).short()} does not follow ${Preview.of(toHexOf(previous)).short()}`);
        previous = b;
      }
      return ok(this.#next({ ...(previous === undefined ? {} : { lastKey: previous }), state: es }));
    }
    if (this.#stage === "done") return bad("nothing follows the head version");
    if (chunk.kind === "state") return bad("state entries precede the history");
    const vs = chunk.versions;
    if (vs.length < 1 || vs.length > MAX_CHUNK_ENTRIES) return bad(`a history chunk holds 1..${MAX_CHUNK_ENTRIES.toString()} items, not ${vs.length.toString()}`);
    const keys: Uint8Array[] = [];
    for (const [u] of vs) {
      const k = key.encode(u);
      if (!k.ok) return k;
      keys.push(k.value);
    }
    // every state root of the hashing's size, after the versions (as the wire form orders them), narrowed to bytes before
    // any is compared or kept
    const size = this.profile.hashing.size;
    const sized: (readonly [V, Uint8Array])[] = [];
    for (const [u, s] of vs) {
      if (!isSized(s, size)) return bad(`a state root is ${size.toString()} bytes, got ${s.length.toString()}`);
      sized.push([u, s]);
    }
    // the trusted head's version encoded once, for the first history chunk, and carried: never read again
    const hk = this.#headKey === undefined ? key.encode(this.#trusted.version) : ok(this.#headKey);
    if (!hk.ok) return hk;
    let previous = this.#lastVersion;
    for (const k of keys) {
      if (previous !== undefined && compareBytes(previous, k) >= 0) return bad(`version ${versionHex(k)} does not follow ${versionHex(previous)}`);
      previous = k;
    }
    for (const [i, k] of keys.entries()) {
      const s = sized[i]?.[1] ?? new Uint8Array(0);
      if (compareBytes(k, hk.value) > 0) return bad(`version ${versionHex(k)} is after the head`);
      if (bytesEqual(k, hk.value) && !bytesEqual(s, this.#trusted.stateRoot)) return bad("the head version has another state root");
    }
    const last = keys[keys.length - 1];
    return ok(
      this.#next({ stage: last !== undefined && bytesEqual(last, hk.value) ? "done" : "history", ...(last === undefined ? {} : { lastVersion: last }), history: sized, keys, headKey: hk.value }),
    );
  }

  /** The snapshot, once its history has reached the head version. */
  finish(): C<Snapshot<V>> {
    if (!READER.has(this)) return err(NOT_A_READER);
    const done = this.#finish();
    if (!done.ok) return done;
    // the caller's own copy: changing it changes nothing the reader holds, or gives a later finish
    const { head, entries, history } = done.value;
    return ok(
      Object.freeze({
        head: copyHead(head, this.profile.versionKey),
        entries: entries.map(([k, v]) => Object.freeze([k, v] as const)),
        history: history.map(([u, s]) => Object.freeze([ownVersion(this.profile.versionKey, u), copyBytes(s)] as const)),
      }),
    );
  }

  #finish(): R<Snapshot<V>> {
    if (this.#stage !== "done") return malformed("snapshot: it ends before the head version");
    return ok(Object.freeze({ head: this.#trusted, entries: this.#stateChunks.flat(), history: this.#historyChunks.flat() }));
  }
}
Object.freeze(SnapshotReader);
Object.freeze(SnapshotReader.prototype);

// ------------------------------------------------------------------ replicas

interface StateSlot {
  readonly key: StateKey;
  readonly value: Value;
}

interface VersionSlot<V> {
  readonly version: V;
  readonly stateRoot: Uint8Array;
}

const copyHead = <V>(h: Head<V>, key: VersionKey<V>): Head<V> => Object.freeze({ version: ownVersion(key, h.version), stateRoot: copyBytes(h.stateRoot), historyRoot: copyBytes(h.historyRoot) });

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
function snapshotArg<V>(x: unknown, p: VersionedTrie<V>): Result<MeasuredSnapshot<V>, TypeMismatch> {
  if (!isObject(x)) return err(expected("a Snapshot"));
  const head = headArg<V>(readGuarded(x, "head"), "the snapshot's head", p);
  if (!head.ok) return head;
  const entries = entriesArg(readGuarded(x, "entries"), "the snapshot's entries");
  if (!entries.ok) return entries;
  const history = versionsArg<V>(readGuarded(x, "history"), "the snapshot's history", p);
  return history.ok ? ok(Object.freeze({ head: head.value, entries: entries.value, history: history.value })) : history;
}

/** A caller's delta, its fields read once into a frozen copy: an optional parent head, a head, upserts and removes. */
function deltaArg<V>(x: unknown, p: VersionedTrie<V>): Result<MeasuredDelta<V>, TypeMismatch> {
  if (!isObject(x)) return err(expected("a Delta"));
  const parentField: unknown = readGuarded(x, "parent");
  const parent = parentField === undefined ? ok(undefined) : headArg<V>(parentField, "the delta's parent", p);
  if (!parent.ok) return parent;
  const head = headArg<V>(readGuarded(x, "head"), "the delta's head", p);
  if (!head.ok) return head;
  const upserts = entriesArg(readGuarded(x, "upserts"), "the delta's upserts");
  if (!upserts.ok) return upserts;
  const removes = readArray(readGuarded(x, "removes"), "the delta's removes", (k, i) => (isKey(k) ? ok(k) : err(expectedAt("a StateKey", i))));
  if (!removes.ok) return removes;
  return ok(Object.freeze({ ...(parent.value === undefined ? {} : { parent: parent.value }), head: head.value, upserts: upserts.value, removes: removes.value }));
}

const NOT_A_REPLICA = expected("a Replica");
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
  // runtime-private, as the head is: the tries' nodes and slots are the replica's own, shared with the replicas it was
  // built from and the ones built from it, and reachable through no property
  readonly #state: TrieMap<StateSlot>;
  readonly #versions: TrieMap<VersionSlot<V>>;

  private constructor(
    token: Token,
    readonly profile: VersionedTrie<V>,
    head: Head<V> | null,
    state: TrieMap<StateSlot>,
    versions: TrieMap<VersionSlot<V>>,
  ) {
    this.#state = state;
    this.#versions = versions;
    // without the token the caller's head is never read, and the replica's views answer as an empty one's
    this.#head = token !== TOKEN || head === null ? null : copyHead(head, profile.versionKey);
    if (token === TOKEN) REPLICA.stamp(this);
    Object.freeze(this);
  }

  /** The head, or `null` before the first delta (a copy). */
  get head(): Head<V> | null {
    return this.#head === null ? null : copyHead(this.#head, this.profile.versionKey);
  }

  /** An empty replica under `profile`; one that is not a `VersionedTrie` is a `TypeMismatch`. */
  static empty<V>(profile: VersionedTrie<V>): Result<Replica<V>, TypeMismatch> {
    const p = profileArg<V>(profile);
    return p.ok ? ok(new Replica(TOKEN, p.value, null, TrieMap.empty(p.value.hashing, p.value.stateMode), TrieMap.empty(p.value.hashing, p.value.historyMode))) : p;
  }

  /** The commitment root of the head, or `null` before the first delta. */
  root(): C<Uint8Array | null> {
    if (!REPLICA.has(this)) return err(NOT_A_REPLICA);
    return this.#head === null ? ok(null) : this.profile.root(this.#head);
  }

  /** The current value of `key`, if any: one walk down its path. What is not a `StateKey` is a `TypeMismatch`. */
  get(key: StateKey): Result<Value | undefined, TypeMismatch> {
    if (!REPLICA.has(this)) return err(NOT_A_REPLICA);
    const k = keyArg(key);
    return k.ok ? ok(this.#state.get(k.value.bytes())?.value) : k;
  }

  /** The number of current entries. */
  get size(): number {
    return REPLICA.has(this) ? this.#state.size : 0;
  }

  /** Every current entry, in key order (none for a replica built from outside). */
  entries(): readonly (readonly [StateKey, Value])[] {
    return REPLICA.has(this) ? this.#state.values().map((s) => [s.key, s.value] as const) : [];
  }

  /** Every committed version with its state root, in order (none for a replica built from outside). */
  history(): readonly (readonly [V, Uint8Array])[] {
    return REPLICA.has(this) ? this.#versions.values().map((s) => [ownVersion(this.profile.versionKey, s.version), copyBytes(s.stateRoot)] as const) : [];
  }

  /**
   * A replica of `snapshot`, which must pass every check of {@link SnapshotReader} against `trusted` and reproduce its
   * state and history roots. The snapshot's own head is only a claim. A profile, head or snapshot that is not of its
   * type (a snapshot's fields read once each, its arrays by index) is a `TypeMismatch`.
   */
  static fromSnapshot<V>(profile: VersionedTrie<V>, trusted: Head<V>, snapshot: Snapshot<V>): C<Replica<V>> {
    const p = profileArg<V>(profile);
    if (!p.ok) return p;
    const t = headArg<V>(trusted, "the trusted head", p.value);
    if (!t.ok) return t;
    const s = snapshotArg<V>(snapshot, p.value);
    if (!s.ok) return s;
    // the trusted head is kept: only one with roots of the profile's size, after every argument's type
    const sized = sizedHeadOf(p.value, t.value);
    return sized.ok ? fromSnapshot(p.value, sized.value, s.value) : sized;
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
    // the reader has checked every chunk: the replica is built from it, its versions' keys as it encoded them
    return fromReader(fed.value);
  }

  /**
   * The replica after `delta`, which must chain from this head, raise the version, write no key twice nor both write
   * and remove one, carry only values every wire format can, and whose claimed head must be what the replica computes.
   * This replica is unchanged either way. A delta that is not a `Delta` (its fields read once each, its arrays by index,
   * its keys `StateKey`s and its values canonical values) is a `TypeMismatch`.
   */
  applyDelta(delta: Delta<V>): C<Replica<V>> {
    if (!REPLICA.has(this)) return err(NOT_A_REPLICA);
    const d = deltaArg<V>(delta, this.profile);
    return d.ok ? this.#apply(d.value) : d;
  }

  #apply(delta: MeasuredDelta<V>): C<Replica<V>> {
    const p = this.profile;
    const parent = this.root();
    if (!parent.ok) return parent;
    const claimed = delta.parent === undefined ? ok(null) : rootOfHead(p, delta.parent);
    if (!claimed.ok) return claimed;
    const same = parent.value === null || claimed.value === null ? parent.value === claimed.value : bytesEqual(parent.value, claimed.value);
    if (!same) return err({ kind: "RootMismatch", component: "parent", expected: parent.value === null ? "none" : toHexOf(parent.value), computed: claimed.value === null ? "none" : toHexOf(claimed.value) });
    const nk = p.versionKey.encode(delta.head.version);
    if (!nk.ok) return nk;
    if (this.#head !== null) {
      const hk = p.versionKey.encode(this.#head.version);
      if (!hk.ok) return hk;
      if (compareBytes(nk.value, hk.value) <= 0) return err({ kind: "VersionNotIncreasing", head: versionHex(hk.value), next: versionHex(nk.value) });
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
    let state = this.#state;
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
    // the claimed roots compared as bytes: one past the digest's size (Oversized) matches no root
    const claimedState = delta.head.stateRoot;
    if (!isMeasuredBytes(claimedState) || !bytesEqual(s, claimedState)) return mismatch("state", claimedState, s);
    const versions = viaTrie(this.#versions.set(nk.value, { version: delta.head.version, stateRoot: s }, TRIE.bytesHash(p.hashing, s)));
    if (!versions.ok) return versions;
    const h = versions.value.root;
    const claimedHistory = delta.head.historyRoot;
    if (!isMeasuredBytes(claimedHistory) || !bytesEqual(h, claimedHistory)) return mismatch("history", claimedHistory, h);
    return ok(new Replica(TOKEN, p, Object.freeze({ version: delta.head.version, stateRoot: s, historyRoot: h }), state, versions.value));
  }

  static {
    replicaOf = (profile, head, state, versions) => new Replica(TOKEN, profile, head, state, versions);
  }
}
Object.freeze(Replica);
Object.freeze(Replica.prototype);

/** {@link Replica.fromSnapshot} of checked, owned arguments. */
function fromSnapshot<V>(profile: VersionedTrie<V>, trusted: Head<V>, snapshot: MeasuredSnapshot<V>): C<Replica<V>> {
  const chunks: MeasuredChunk<V>[] = [
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
  return fromReader(reader);
}

/**
 * The replica of a reader's snapshot, once it has finished: each history version keyed by the bytes the reader
 * encoded it to as it was fed, so no version (a caller's object) is encoded, or read, again.
 */
function fromReader<V>(reader: SnapshotReader<V>): C<Replica<V>> {
  const { profile, trusted } = reader;
  const checked = finishReader(reader);
  if (!checked.ok) return checked;
  const keys = readerKeys(reader);
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
  for (const [i, [version, stateRoot]] of checked.value.history.entries()) {
    const vkey = keys[i];
    if (vkey === undefined) return malformed("snapshot: a history version has no key");
    versions.push([vkey, { version, stateRoot: copyBytes(stateRoot) }, TRIE.bytesHash(profile.hashing, stateRoot)]);
  }
  const history = viaTrie(TrieMap.fromSorted(profile.hashing, profile.historyMode, versions));
  if (!history.ok) return history;
  if (!bytesEqual(history.value.root, trusted.historyRoot)) return mismatch("history", trusted.historyRoot, history.value.root);
  return ok(replicaOf(profile, trusted, state.value, history.value));
}
