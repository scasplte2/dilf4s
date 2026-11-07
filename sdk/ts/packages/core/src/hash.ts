/**
 * Hash functions, domain tags and tagged hashing (docs/specs/canonical-encoding.md section 4).
 *
 * Every exported function checks what a caller passes past the types (from JavaScript, or a cast) once, in O(1), before
 * it reads it: an argument of the wrong type is a `TypeMismatch`, never a throw or an argument read as another type.
 * Caller bytes are read through a plain view of their own memory (`plainBytes`), so a subclass's `length`, accessors or
 * iterator are never believed. The `...Of` functions are the unchecked forms, for this SDK's own data
 * (`@kd5ujc/dilf4s-core/internal`).
 */
import { blake2b } from "@noble/hashes/blake2.js";
import { sha256, sha512 } from "@noble/hashes/sha2.js";
import { keccak_256, sha3_256, sha3_512 } from "@noble/hashes/sha3.js";

import { copyBytes, plainBytes, takeBytes } from "./bytes.js";
import { checkedLimit } from "./budget.js";
import { encodeIntoWithin, Feeding, MAX_ENCODED_BYTES, type EncodeError } from "./cbor.js";
import type { TypeMismatch } from "./error.js";
import type { ExpansionRefusal, HashError, HashErrorKind } from "./generated/hash_errors.js";
import { firstNonDigit, fromHex } from "./hex.js";
import { mint } from "./mint.js";
import { Preview } from "./preview.js";
import { err, ok, type Err, type Result } from "./result.js";
import { encodeUtf8 } from "./utf8.js";
import type { Value } from "./value.js";

/**
 * A hash refusal, by kind (docs/specs/conformance.md section 6): generated from `shared/schemas/hash-errors.cddl`, as the
 * reference's and the Rust SDK's are. Caller text is held as a {@link Preview}, never as itself.
 */
export type { ExpansionRefusal, HashError, HashErrorKind };

/** The members of {@link HashError} of the kinds `K`: what a function that refuses only those returns. */
export type HashErrorOf<K extends HashErrorKind> = Extract<HashError, { readonly kind: K }>;

/**
 * Why an algorithm argument was refused: a name that is not one of {@link HASH_ALGORITHMS} (`UnknownAlgorithm`,
 * previewed), or, from a JavaScript caller or a cast, one that is not a string at all (`TypeMismatch`).
 */
export type AlgorithmError = HashErrorOf<"UnknownAlgorithm"> | TypeMismatch;

/** The hash functions the specs use, by the names the specs and vectors give them. */
export type HashAlgorithm = "SHA-256" | "SHA-512" | "SHA3-256" | "SHA3-512" | "Keccak-256" | "BLAKE2b-256" | "BLAKE2b-512";

/** Every {@link HashAlgorithm}, frozen: a caller cannot add a name to it. */
export const HASH_ALGORITHMS: readonly HashAlgorithm[] = Object.freeze(["SHA-256", "SHA-512", "SHA3-256", "SHA3-512", "Keccak-256", "BLAKE2b-256", "BLAKE2b-512"]);

/** Whether `x` is one of {@link HASH_ALGORITHMS}, in O(1): compared with each name, never converted. */
export function isAlgorithm(x: unknown): x is HashAlgorithm {
  switch (x) {
    case "SHA-256":
    case "SHA-512":
    case "SHA3-256":
    case "SHA3-512":
    case "Keccak-256":
    case "BLAKE2b-256":
    case "BLAKE2b-512":
      return true;
    default:
      return false;
  }
}

/** A refusal of an argument of the wrong type (`TypeMismatch`), saying what was expected. */
export const typeMismatch = (reason: string): TypeMismatch => Object.freeze({ kind: "TypeMismatch", reason });

const NOT_A_STRING = typeMismatch("expected a string");
const NOT_BYTES = typeMismatch("expected a Uint8Array");
const NOT_PARTS = typeMismatch("expected an array of Uint8Arrays");
const NOT_A_TAG = typeMismatch("expected a DomainTag");
const NOT_A_LENGTH = typeMismatch("the output length is not a number");

/**
 * The refusal of `x` as an algorithm, or `undefined` if it is one: a string that names none is `UnknownAlgorithm` with
 * the name previewed; anything else is a `TypeMismatch`, never read or converted.
 */
function refuseAlgorithm(x: unknown): Err<AlgorithmError> | undefined {
  if (isAlgorithm(x)) return undefined;
  return typeof x === "string" ? err({ kind: "UnknownAlgorithm", name: Preview.of(x) }) : err(NOT_A_STRING);
}

/** An incremental hash of this package's own (noble's): it throws on input that is not bytes and on a second digest. */
export interface RawHash {
  /** Absorbs plain bytes (noble throws on anything else). */
  update(data: Uint8Array): unknown;
  /** Finishes the hash: a second call throws. */
  digest(): Uint8Array;
  /** An independent copy of the state so far. */
  clone(): RawHash;
}

/** A fresh incremental hash under `algorithm`, unchecked: for this SDK's own algorithm and bytes. */
export function startHashOf(algorithm: HashAlgorithm): RawHash {
  switch (algorithm) {
    case "SHA-256":
      return sha256.create();
    case "SHA-512":
      return sha512.create();
    case "SHA3-256":
      return sha3_256.create();
    case "SHA3-512":
      return sha3_512.create();
    case "Keccak-256":
      return keccak_256.create();
    // the output length is a BLAKE2b parameter, so BLAKE2b-256 is not a truncated BLAKE2b-512
    case "BLAKE2b-256":
      return blake2b.create({ dkLen: 32 });
    case "BLAKE2b-512":
      return blake2b.create({ dkLen: 64 });
  }
}

/** The hash of the concatenation of `parts`, unchecked: for this SDK's own algorithm and plain bytes. */
export function hashPartsOf(algorithm: HashAlgorithm, parts: readonly Uint8Array[]): Uint8Array {
  const h = startHashOf(algorithm);
  for (const p of parts) h.update(p);
  return h.digest();
}

/** The hash of `data`, unchecked: for this SDK's own algorithm and plain bytes. */
export function digestOf(algorithm: HashAlgorithm, data: Uint8Array): Uint8Array {
  const h = startHashOf(algorithm);
  h.update(data);
  return h.digest();
}

/** The output size of `algorithm` in bytes, unchecked. */
export const outputSizeOf = (algorithm: HashAlgorithm): number => (algorithm === "SHA-512" || algorithm === "SHA3-512" || algorithm === "BLAKE2b-512" ? 64 : 32);

/** The input block size of `algorithm` in bytes (a sponge's rate), unchecked. */
export function blockSizeOf(algorithm: HashAlgorithm): number {
  switch (algorithm) {
    case "SHA-256":
      return 64;
    case "SHA3-512":
      return 72;
    case "SHA3-256":
    case "Keccak-256":
      return 136;
    case "SHA-512":
    case "BLAKE2b-256":
    case "BLAKE2b-512":
      return 128;
  }
}

/** {@link expansionLimit}, unchecked. */
const expansionLimitOf = (algorithm: HashAlgorithm): number => Math.min(255 * outputSizeOf(algorithm), 65535);

/**
 * An incremental hash: absorb parts, then read the digest of what it has absorbed so far. Its methods check what a
 * caller passes, as every function here does.
 */
export interface Hashing {
  /** Absorbs `data` (read through a plain view of its memory); what is not a `Uint8Array` is a `TypeMismatch`, unread. */
  update(data: Uint8Array): Result<Hashing, TypeMismatch>;
  /** The digest of everything absorbed so far: it may be read again, and absorbing may go on after it. */
  digest(): Uint8Array;
}

/** {@link Hashing} over a hash of this package's: the hash itself never meets caller input it would throw on. */
class CheckedHashing implements Hashing {
  readonly #hash: RawHash;
  constructor(hash: RawHash) {
    this.#hash = hash;
    Object.freeze(this);
  }

  /** {@link Hashing.update}: the data checked and read through a plain view. */
  update(data: Uint8Array): Result<Hashing, TypeMismatch> {
    const view = plainBytes(data);
    if (view === undefined) return err(NOT_BYTES);
    this.#hash.update(view);
    return ok(this);
  }

  /** {@link Hashing.digest}. */
  digest(): Uint8Array {
    // a copy is finished, never the hash itself, so this may be called again and absorbing may go on
    return this.#hash.clone().digest();
  }
}

/**
 * A fresh incremental hash under `algorithm`. An algorithm that is not a string is a `TypeMismatch`; a string that is
 * not one of {@link HASH_ALGORITHMS} is `UnknownAlgorithm`.
 */
export function startHash(algorithm: HashAlgorithm): Result<Hashing, AlgorithmError> {
  return refuseAlgorithm(algorithm) ?? ok(new CheckedHashing(startHashOf(algorithm)));
}

/** The output size of `algorithm` in bytes; refused as {@link startHash} refuses an algorithm. */
export function outputSize(algorithm: HashAlgorithm): Result<number, AlgorithmError> {
  return refuseAlgorithm(algorithm) ?? ok(outputSizeOf(algorithm));
}

/**
 * The input block size of `algorithm` in bytes (a sponge's rate): `expand_message_xmd`'s `s_in_bytes`. Refused as
 * {@link startHash} refuses an algorithm.
 */
export function blockSize(algorithm: HashAlgorithm): Result<number, AlgorithmError> {
  return refuseAlgorithm(algorithm) ?? ok(blockSizeOf(algorithm));
}

/**
 * The algorithm with this name, or `UnknownAlgorithm` with the name previewed. A JavaScript caller's name that is not a
 * string is a `TypeMismatch`, never converted.
 */
export function parseHashAlgorithm(name: string): Result<HashAlgorithm, AlgorithmError> {
  return isAlgorithm(name) ? ok(name) : (refuseAlgorithm(name) ?? err(NOT_A_STRING));
}

/**
 * A digest of `algorithm` from its bytes, which must be exactly its output size (a fresh copy of the array's own memory,
 * measured by its internal length: its own `length` is not believed). Refused as {@link startHash} refuses an algorithm; bytes that are not
 * a `Uint8Array` are a `TypeMismatch`.
 */
export function digestFromBytes(algorithm: HashAlgorithm, bytes: Uint8Array): Result<Uint8Array, HashErrorOf<"InvalidLength" | "UnknownAlgorithm"> | TypeMismatch> {
  const refused = refuseAlgorithm(algorithm);
  if (refused !== undefined) return refused;
  const view = plainBytes(bytes);
  if (view === undefined) return err(NOT_BYTES);
  const expected = outputSizeOf(algorithm);
  // measured before it is copied: bytes of any other length are refused unread
  return view.length === expected ? ok(copyBytes(view)) : err({ kind: "InvalidLength", algorithm, expected, actual: view.length });
}

/**
 * A digest of `algorithm` from its hex (docs/specs/canonical-encoding.md section 4): an optional `0x`, then exactly
 * twice the output size in ASCII hex digits, either case. `InvalidHex` at the first character that is not a hex digit
 * (or, for an odd number of digits, the text's length), `InvalidLength` for any other number of digits. Digits past the
 * width are refused reading at most one past it, before the text is decoded (conformance.md section 8). Refused as
 * {@link startHash} refuses an algorithm; text that is not a string is a `TypeMismatch`, unread.
 */
export function digestFromHex(algorithm: HashAlgorithm, text: string): Result<Uint8Array, HashErrorOf<"InvalidLength" | "InvalidHex" | "UnknownAlgorithm"> | TypeMismatch> {
  const refused = refuseAlgorithm(algorithm);
  if (refused !== undefined) return refused;
  if (typeof (text as unknown) !== "string") return err(NOT_A_STRING);
  const from = text.startsWith("0x") ? 2 : 0;
  const digits = text.length - from;
  const expected = outputSizeOf(algorithm);
  if (digits > 2 * expected) {
    const bad = firstNonDigit(text, from, from + 2 * expected + 1);
    return err(bad >= 0 ? { kind: "InvalidHex", offset: bad } : { kind: "InvalidLength", algorithm, expected, actual: Math.ceil(digits / 2) });
  }
  const bytes = fromHex(text.slice(from));
  if (!bytes.ok) return bytes.error.kind === "InvalidHex" ? err({ kind: "InvalidHex", offset: bytes.error.offset + from }) : bytes;
  return bytes.value.length === expected ? ok(bytes.value) : err({ kind: "InvalidLength", algorithm, expected, actual: bytes.value.length });
}

/** The most bytes {@link expandMessageXmd} expands to with `algorithm`: `min(255 * output size, 65535)`. */
export function expansionLimit(algorithm: HashAlgorithm): Result<number, AlgorithmError> {
  return refuseAlgorithm(algorithm) ?? ok(expansionLimitOf(algorithm));
}

const OVERSIZE_PREFIX = encodeUtf8("H2C-OVERSIZE-DST-");

/**
 * RFC 9380 section 5.3.1 `expand_message_xmd`: `length` uniform bytes from `message` under the domain separation tag
 * `dst`. A DST over 255 bytes is first reduced to `H("H2C-OVERSIZE-DST-" || DST)` (section 5.3.3).
 *
 * Every argument's type is checked first, in O(1): an algorithm as {@link startHash} checks it, then a message or DST
 * that is not a `Uint8Array`, or a length that is not a number, is a `TypeMismatch`. Then, before anything is hashed,
 * `InvalidExpansion` for an empty DST (`emptyDst`), then for a length that is not an integer in `1..`
 * {@link expansionLimit} (`outputLength`). The message is read once, through a plain view of its memory; the DST is
 * copied from its own memory first. Neither array's own `length` is believed.
 */
export function expandMessageXmd(
  algorithm: HashAlgorithm,
  message: Uint8Array,
  dst: Uint8Array,
  length: number,
): Result<Uint8Array, HashErrorOf<"InvalidExpansion" | "UnknownAlgorithm"> | TypeMismatch> {
  const refusedAlgorithm = refuseAlgorithm(algorithm);
  if (refusedAlgorithm !== undefined) return refusedAlgorithm;
  const msg = plainBytes(message);
  if (msg === undefined) return err(NOT_BYTES);
  const own = takeBytes(dst);
  if (own === undefined) return err(NOT_BYTES);
  if (typeof (length as unknown) !== "number") return err(NOT_A_LENGTH);
  const limit = expansionLimitOf(algorithm);
  const refused = (refusal: ExpansionRefusal) => err<HashErrorOf<"InvalidExpansion">>({ kind: "InvalidExpansion", refusal, algorithm, limit });
  if (own.length === 0) return refused("emptyDst");
  if (!Number.isSafeInteger(length) || length < 1 || length > limit) return refused("outputLength");
  const tag = own.length > 255 ? hashPartsOf(algorithm, [OVERSIZE_PREFIX, own]) : own;
  const suffix = new Uint8Array(tag.length + 1);
  suffix.set(tag);
  suffix[tag.length] = tag.length;
  const b0 = hashPartsOf(algorithm, [new Uint8Array(blockSizeOf(algorithm)), msg, Uint8Array.of(length >> 8, length & 0xff), Uint8Array.of(0), suffix]);
  const size = outputSizeOf(algorithm);
  const ell = Math.ceil(length / size);
  const out = new Uint8Array(ell * size);
  let prev = hashPartsOf(algorithm, [b0, Uint8Array.of(1), suffix]);
  out.set(prev, 0);
  for (let i = 2; i <= ell; i++) {
    const last = prev;
    prev = hashPartsOf(algorithm, [b0.map((b, j) => b ^ (last[j] ?? 0)), Uint8Array.of(i), suffix]);
    out.set(prev, (i - 1) * size);
  }
  return ok(out.slice(0, length));
}

/**
 * The hash of the concatenation of `parts`. Refused as {@link startHash} refuses an algorithm; `parts` that is not an
 * array, or a part that is not a `Uint8Array`, is a `TypeMismatch`. The array is read by index (its length once, each
 * part once), and each part through a plain view of its memory, as it is absorbed.
 */
export function hashParts(algorithm: HashAlgorithm, parts: readonly Uint8Array[]): Result<Uint8Array, AlgorithmError> {
  const refused = refuseAlgorithm(algorithm);
  if (refused !== undefined) return refused;
  const count: unknown = Array.isArray(parts) ? parts.length : undefined;
  if (typeof count !== "number") return err(NOT_PARTS);
  const h = startHashOf(algorithm);
  for (let i = 0; i < count; i++) {
    const part = plainBytes(parts[i]);
    if (part === undefined) return err(typeMismatch(`expected a Uint8Array at index ${i.toString()}`));
    h.update(part);
  }
  return ok(h.digest());
}

/**
 * The hash of `data`. Refused as {@link startHash} refuses an algorithm; data that is not a `Uint8Array` is a
 * `TypeMismatch`. It is read once, through a plain view of its memory.
 */
export function digest(algorithm: HashAlgorithm, data: Uint8Array): Result<Uint8Array, AlgorithmError> {
  const refused = refuseAlgorithm(algorithm);
  if (refused !== undefined) return refused;
  const view = plainBytes(data);
  return view === undefined ? err(NOT_BYTES) : ok(digestOf(algorithm, view));
}

/** The DomainTag grammar of section 4. Without the `m` flag, `$` matches only at the very end, as Java's `matches`. */
const DOMAIN_TAG = /^[a-z0-9]+(\/[a-z0-9][a-z0-9._-]*)*\/v[0-9]+$/;

let isTag!: (x: unknown) => x is DomainTag;

/** The key `DomainTag`'s constructor takes, private to this module: without it, what a constructor call builds is no tag. */
const TAG_KEY: unique symbol = Symbol("dilf4s domain tag");
const TAG = mint();

/** A versioned domain-separation tag, such as `dilf4s/mpt/leaf/v1`. */
export class DomainTag {
  // #private state makes the class nominal: only DomainTag.of can produce one, so no unchecked tag reaches a hash
  readonly #value: string;
  private constructor(key: typeof TAG_KEY, value: string) {
    this.#value = value;
    // a JavaScript caller can call a private constructor; without the key it builds an object no check accepts
    if (key === TAG_KEY) TAG.stamp(this);
    Object.freeze(this);
  }

  static {
    isTag = (x): x is DomainTag => TAG.has(x);
  }

  /** The tag text. */
  get value(): string {
    return this.#value;
  }

  /**
   * The tag, if it matches the grammar (`InvalidDomainTag`, previewed). A JavaScript caller's tag that is not a string is
   * a `TypeMismatch`, never converted (a regular expression would run its `toString`).
   */
  static of(tag: string): Result<DomainTag, HashErrorOf<"InvalidDomainTag"> | TypeMismatch> {
    if (typeof (tag as unknown) !== "string") return err(NOT_A_STRING);
    return DOMAIN_TAG.test(tag) ? ok(new DomainTag(TAG_KEY, tag)) : err({ kind: "InvalidDomainTag", tag: Preview.of(tag) });
  }
}

/**
 * Whether `x` is a {@link DomainTag} made by `DomainTag.of`, in O(1): a private-brand check, which runs no Proxy trap
 * and which an object built on its prototype, or by calling the constructor from outside, fails.
 */
export const isDomainTag = (x: unknown): x is DomainTag => isTag(x);

/**
 * `taggedHash_H(tag, msg) = H(H(tag) || H(tag) || msg)`, the BIP-340 construction over any `H`. Building one hashes the
 * tag once. A domain tag is ASCII by its grammar.
 */
export class TaggedHash {
  readonly #tagHash: Uint8Array;
  private constructor(
    readonly algorithm: HashAlgorithm,
    readonly tag: DomainTag,
  ) {
    this.#tagHash = digestOf(algorithm, encodeUtf8(tag.value));
    Object.freeze(this);
  }

  /**
   * The tagged hash of `tag` under `algorithm`. Refused as {@link startHash} refuses an algorithm; a tag that is not a
   * `DomainTag` made by `DomainTag.of` (see {@link isDomainTag}) is a `TypeMismatch`.
   */
  static of(algorithm: HashAlgorithm, tag: DomainTag): Result<TaggedHash, AlgorithmError> {
    const refused = refuseAlgorithm(algorithm);
    if (refused !== undefined) return refused;
    return isTag(tag) ? ok(new TaggedHash(algorithm, tag)) : err(NOT_A_TAG);
  }

  /** An incremental hash with the prefix `H(tag) || H(tag)` absorbed: absorb a message, then digest, for its tagged hash. */
  start(): Hashing {
    return new CheckedHashing(this.#start());
  }

  /** `start`, bound to this instance: borrowed onto another receiver, it fails at `#tagHash` before anything is hashed. */
  #start(): RawHash {
    const t = this.#tag();
    const h = startHashOf(this.algorithm);
    h.update(t);
    h.update(t);
    return h;
  }

  /** A copy of the tag hash: a hash, whose methods any caller can replace, is never handed this instance's own state. */
  #tag(): Uint8Array {
    return Uint8Array.from(this.#tagHash);
  }

  /** The tagged hash of `message`; what is not a `Uint8Array` is a `TypeMismatch`. Read once, through a plain view. */
  hash(message: Uint8Array): Result<Uint8Array, TypeMismatch> {
    const view = plainBytes(message);
    if (view === undefined) return err(NOT_BYTES);
    const h = this.#start();
    h.update(view);
    return ok(h.digest());
  }

  /**
   * The content hash of a canonical value: the tagged hash of its canonical encoding. {@link contentWithin} at
   * `cbor.MAX_ENCODED_BYTES`, as `cbor.encode`: an encoding longer is `TooLarge`.
   */
  content(value: Value): Result<Uint8Array, EncodeError> {
    return this.contentWithin(value, MAX_ENCODED_BYTES);
  }

  /**
   * The content hash of a value whose canonical encoding is at most `maxBytes` long, with the bounds and refusals of
   * `cbor.encodeWithin` (a value that is not a canonical value, or a limit that is not a number, is a `TypeMismatch`,
   * before anything is hashed), the encoding streamed into the hash: memory as `STAGED_BYTES` states.
   */
  contentWithin(value: Value, maxBytes: number): Result<Uint8Array, EncodeError> {
    const limit = checkedLimit(value, maxBytes);
    if (!limit.ok) return limit;
    // the private factory, never the public `start`: a value's own bytes reach only a hash this package created
    const h = this.#start();
    const e = encodeIntoWithin(new Feeding(h), value, limit.value);
    return e.ok ? ok(h.digest()) : e;
  }
}

/** `taggedHash(tag, message)` with `algorithm`, refused as {@link TaggedHash.of} and {@link TaggedHash.hash} refuse. */
export function taggedHash(algorithm: HashAlgorithm, tag: DomainTag, message: Uint8Array): Result<Uint8Array, AlgorithmError> {
  const t = TaggedHash.of(algorithm, tag);
  return t.ok ? t.value.hash(message) : t;
}

/** The content hash of a canonical value with `algorithm`, as {@link TaggedHash.of} and {@link TaggedHash.content}. */
export function contentHash(algorithm: HashAlgorithm, tag: DomainTag, value: Value): Result<Uint8Array, AlgorithmError | EncodeError> {
  const t = TaggedHash.of(algorithm, tag);
  return t.ok ? t.value.content(value) : t;
}
