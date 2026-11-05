/** Hash functions, domain tags and tagged hashing (docs/specs/canonical-encoding.md section 4). */
import { blake2b } from "@noble/hashes/blake2.js";
import { sha256, sha512 } from "@noble/hashes/sha2.js";
import { keccak_256, sha3_256, sha3_512 } from "@noble/hashes/sha3.js";

import { encodeIntoWithin, Feeding, MAX_ENCODED_BYTES } from "./cbor.js";
import type { TooLarge } from "./error.js";
import type { ExpansionRefusal, HashError, HashErrorKind } from "./generated/hash_errors.js";
import { firstNonDigit, fromHex } from "./hex.js";
import { Preview } from "./preview.js";
import { err, ok, type Result } from "./result.js";
import { encodeUtf8 } from "./utf8.js";
import type { Value } from "./value.js";

/**
 * A hash refusal, by kind (docs/specs/conformance.md section 6): generated from `shared/schemas/hash-errors.cddl`, as the
 * reference's and the Rust SDK's are. Caller text is held as a {@link Preview}, never as itself.
 */
export type { ExpansionRefusal, HashError, HashErrorKind };

/** The members of {@link HashError} of the kinds `K`: what a function that refuses only those returns. */
export type HashErrorOf<K extends HashErrorKind> = Extract<HashError, { readonly kind: K }>;

/** The hash functions the specs use, by the names the specs and vectors give them. */
export type HashAlgorithm = "SHA-256" | "SHA-512" | "SHA3-256" | "SHA3-512" | "Keccak-256" | "BLAKE2b-256" | "BLAKE2b-512";

export const HASH_ALGORITHMS: readonly HashAlgorithm[] = ["SHA-256", "SHA-512", "SHA3-256", "SHA3-512", "Keccak-256", "BLAKE2b-256", "BLAKE2b-512"];

/** An incremental hash: absorb parts, then produce the digest (once). It holds a block of input at most. */
export interface Hashing {
  update(data: Uint8Array): Hashing;
  digest(): Uint8Array;
}

/** A fresh incremental hash under `algorithm`. */
export function startHash(algorithm: HashAlgorithm): Hashing {
  return start(algorithm);
}

function start(algorithm: HashAlgorithm): Hashing {
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

/** The output size of `algorithm` in bytes. */
export function outputSize(algorithm: HashAlgorithm): number {
  return algorithm === "SHA-512" || algorithm === "SHA3-512" || algorithm === "BLAKE2b-512" ? 64 : 32;
}

/** The input block size of `algorithm` in bytes (a sponge's rate): `expand_message_xmd`'s `s_in_bytes`. */
export function blockSize(algorithm: HashAlgorithm): number {
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

/** The algorithm with this name, or `UnknownAlgorithm` with the name previewed. */
export function parseHashAlgorithm(name: string): Result<HashAlgorithm, HashErrorOf<"UnknownAlgorithm">> {
  const found = HASH_ALGORITHMS.find((a) => a === name);
  return found === undefined ? err({ kind: "UnknownAlgorithm", name: Preview.of(name) }) : ok(found);
}

/** The algorithm with this name, if it is one of {@link HASH_ALGORITHMS}: {@link parseHashAlgorithm} without the refusal. */
export function hashAlgorithm(name: string): HashAlgorithm | undefined {
  const r = parseHashAlgorithm(name);
  return r.ok ? r.value : undefined;
}

/** A digest of `algorithm` from its bytes, which must be exactly its output size (a fresh copy). */
export function digestFromBytes(algorithm: HashAlgorithm, bytes: Uint8Array): Result<Uint8Array, HashErrorOf<"InvalidLength">> {
  const expected = outputSize(algorithm);
  return bytes.length === expected ? ok(Uint8Array.from(bytes)) : err({ kind: "InvalidLength", algorithm, expected, actual: bytes.length });
}

/**
 * A digest of `algorithm` from its hex (docs/specs/canonical-encoding.md section 4): an optional `0x`, then exactly
 * twice the output size in ASCII hex digits, either case. `InvalidHex` at the first character that is not a hex digit
 * (or, for an odd number of digits, the text's length), `InvalidLength` for any other number of digits. Digits past the
 * width are refused reading at most one past it, before the text is decoded (conformance.md section 8).
 */
export function digestFromHex(algorithm: HashAlgorithm, text: string): Result<Uint8Array, HashErrorOf<"InvalidLength" | "InvalidHex">> {
  const from = text.startsWith("0x") ? 2 : 0;
  const digits = text.length - from;
  const expected = outputSize(algorithm);
  if (digits > 2 * expected) {
    const bad = firstNonDigit(text, from, from + 2 * expected + 1);
    return err(bad >= 0 ? { kind: "InvalidHex", offset: bad } : { kind: "InvalidLength", algorithm, expected, actual: Math.ceil(digits / 2) });
  }
  const bytes = fromHex(text.slice(from));
  return bytes.ok ? digestFromBytes(algorithm, bytes.value) : err({ kind: "InvalidHex", offset: bytes.error.offset + from });
}

/** The most bytes {@link expandMessageXmd} expands to with `algorithm`: `min(255 * output size, 65535)`. */
export function expansionLimit(algorithm: HashAlgorithm): number {
  return Math.min(255 * outputSize(algorithm), 65535);
}

const OVERSIZE_PREFIX = encodeUtf8("H2C-OVERSIZE-DST-");

/**
 * RFC 9380 section 5.3.1 `expand_message_xmd`: `length` uniform bytes from `message` under the domain separation tag
 * `dst`. A DST over 255 bytes is first reduced to `H("H2C-OVERSIZE-DST-" || DST)` (section 5.3.3). Refused as
 * `InvalidExpansion`, before anything is hashed, for an empty DST (`emptyDst`), then for a length that is not an integer
 * in `1..`{@link expansionLimit} (`outputLength`).
 */
export function expandMessageXmd(
  algorithm: HashAlgorithm,
  message: Uint8Array,
  dst: Uint8Array,
  length: number,
): Result<Uint8Array, HashErrorOf<"InvalidExpansion">> {
  const limit = expansionLimit(algorithm);
  const refused = (refusal: ExpansionRefusal) => err<HashErrorOf<"InvalidExpansion">>({ kind: "InvalidExpansion", refusal, algorithm, limit });
  if (dst.length === 0) return refused("emptyDst");
  if (!Number.isSafeInteger(length) || length < 1 || length > limit) return refused("outputLength");
  const tag = dst.length > 255 ? hashParts(algorithm, [OVERSIZE_PREFIX, dst]) : dst;
  const suffix = new Uint8Array(tag.length + 1);
  suffix.set(tag);
  suffix[tag.length] = tag.length;
  const b0 = hashParts(algorithm, [new Uint8Array(blockSize(algorithm)), message, Uint8Array.of(length >> 8, length & 0xff), Uint8Array.of(0), suffix]);
  const size = outputSize(algorithm);
  const ell = Math.ceil(length / size);
  const out = new Uint8Array(ell * size);
  let prev = hashParts(algorithm, [b0, Uint8Array.of(1), suffix]);
  out.set(prev, 0);
  for (let i = 2; i <= ell; i++) {
    const last = prev;
    prev = hashParts(algorithm, [b0.map((b, j) => b ^ (last[j] ?? 0)), Uint8Array.of(i), suffix]);
    out.set(prev, (i - 1) * size);
  }
  return ok(out.slice(0, length));
}

/** The hash of the concatenation of `parts`. */
export function hashParts(algorithm: HashAlgorithm, parts: readonly Uint8Array[]): Uint8Array {
  const h = start(algorithm);
  for (const p of parts) h.update(p);
  return h.digest();
}

/** The hash of `data`. */
export function digest(algorithm: HashAlgorithm, data: Uint8Array): Uint8Array {
  return hashParts(algorithm, [data]);
}

/** The DomainTag grammar of section 4. Without the `m` flag, `$` matches only at the very end, as Java's `matches`. */
const DOMAIN_TAG = /^[a-z0-9]+(\/[a-z0-9][a-z0-9._-]*)*\/v[0-9]+$/;

/** A versioned domain-separation tag, such as `dilf4s/mpt/leaf/v1`. */
export class DomainTag {
  // #private state makes the class nominal: only DomainTag.of can produce one, so no unchecked tag reaches a hash
  readonly #value: string;
  private constructor(value: string) {
    this.#value = value;
    Object.freeze(this);
  }

  /** The tag text. */
  get value(): string {
    return this.#value;
  }

  /** The tag, if it matches the grammar. */
  static of(tag: string): Result<DomainTag, HashErrorOf<"InvalidDomainTag">> {
    return DOMAIN_TAG.test(tag) ? ok(new DomainTag(tag)) : err({ kind: "InvalidDomainTag", tag: Preview.of(tag) });
  }
}

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
    this.#tagHash = digest(algorithm, encodeUtf8(tag.value));
    Object.freeze(this);
  }

  static of(algorithm: HashAlgorithm, tag: DomainTag): TaggedHash {
    return new TaggedHash(algorithm, tag);
  }

  /** An incremental hash with the prefix `H(tag) || H(tag)` absorbed: absorb a message, then digest, for its tagged hash. */
  start(): Hashing {
    return this.#start();
  }

  /** `start`, bound to this instance: borrowed onto another receiver, it fails at `#tagHash` before anything is hashed. */
  #start(): Hashing {
    const t = this.#tag();
    return start(this.algorithm).update(t).update(t);
  }

  /** A copy of the tag hash: a hash, whose methods any caller can replace, is never handed this instance's own state. */
  #tag(): Uint8Array {
    return Uint8Array.from(this.#tagHash);
  }

  /** The tagged hash of `message`. */
  hash(message: Uint8Array): Uint8Array {
    const t = this.#tag();
    return hashParts(this.algorithm, [t, t, message]);
  }

  /**
   * The content hash of a canonical value: the tagged hash of its canonical encoding. {@link contentWithin} at
   * `cbor.MAX_ENCODED_BYTES`, as `cbor.encode`: an encoding longer is `TooLarge`.
   */
  content(value: Value): Result<Uint8Array, TooLarge> {
    return this.contentWithin(value, MAX_ENCODED_BYTES);
  }

  /**
   * The content hash of a value whose canonical encoding is at most `maxBytes` long, with the bounds and refusals of
   * `cbor.encodeWithin`, the encoding streamed into the hash: memory as `STAGED_BYTES` states.
   */
  contentWithin(value: Value, maxBytes: number): Result<Uint8Array, TooLarge> {
    // the private factory, never the public `start`: a value's own bytes reach only a hash this package created
    const h = this.#start();
    const e = encodeIntoWithin(new Feeding(h), value, maxBytes);
    return e.ok ? ok(h.digest()) : e;
  }
}

/** `taggedHash(tag, message)` with `algorithm`. */
export function taggedHash(algorithm: HashAlgorithm, tag: DomainTag, message: Uint8Array): Uint8Array {
  return TaggedHash.of(algorithm, tag).hash(message);
}

/** The content hash of a canonical value with `algorithm`, as {@link TaggedHash.content}. */
export function contentHash(algorithm: HashAlgorithm, tag: DomainTag, value: Value): Result<Uint8Array, TooLarge> {
  return TaggedHash.of(algorithm, tag).content(value);
}
