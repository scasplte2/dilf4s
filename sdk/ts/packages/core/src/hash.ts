/** Hash functions, domain tags and tagged hashing (docs/specs/canonical-encoding.md section 4). */
import { blake2b } from "@noble/hashes/blake2.js";
import { sha256, sha512 } from "@noble/hashes/sha2.js";
import { keccak_256, sha3_256, sha3_512 } from "@noble/hashes/sha3.js";

import { encode } from "./cbor.js";
import { err, ok, type Result } from "./result.js";
import { encodeUtf8 } from "./utf8.js";
import type { Value } from "./value.js";

/** The hash functions the specs use, by the names the specs and vectors give them. */
export type HashAlgorithm = "SHA-256" | "SHA-512" | "SHA3-256" | "SHA3-512" | "Keccak-256" | "BLAKE2b-256" | "BLAKE2b-512";

export const HASH_ALGORITHMS: readonly HashAlgorithm[] = ["SHA-256", "SHA-512", "SHA3-256", "SHA3-512", "Keccak-256", "BLAKE2b-256", "BLAKE2b-512"];

/** An incremental hash: absorb parts, then produce the digest. */
interface Hashing {
  update(data: Uint8Array): Hashing;
  digest(): Uint8Array;
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

/** The algorithm with this name, if it is one of {@link HASH_ALGORITHMS}. */
export function hashAlgorithm(name: string): HashAlgorithm | undefined {
  return HASH_ALGORITHMS.find((a) => a === name);
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

export interface InvalidDomainTag {
  readonly kind: "InvalidDomainTag";
  readonly tag: string;
}

/** The DomainTag grammar of section 4. Without the `m` flag, `$` matches only at the very end, as Java's `matches`. */
const DOMAIN_TAG = /^[a-z0-9]+(\/[a-z0-9][a-z0-9._-]*)*\/v[0-9]+$/;

/** A versioned domain-separation tag, such as `dilf4s/mpt/leaf/v1`. */
export class DomainTag {
  private constructor(readonly value: string) {
    Object.freeze(this);
  }

  /** The tag, if it matches the grammar. */
  static of(tag: string): Result<DomainTag, InvalidDomainTag> {
    return DOMAIN_TAG.test(tag) ? ok(new DomainTag(tag)) : err({ kind: "InvalidDomainTag", tag });
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

  /** The tagged hash of `message`. */
  hash(message: Uint8Array): Uint8Array {
    return hashParts(this.algorithm, [this.#tagHash, this.#tagHash, message]);
  }

  /** The content hash of a canonical value: the tagged hash of its canonical encoding. */
  content(value: Value): Uint8Array {
    return this.hash(encode(value));
  }
}

/** `taggedHash(tag, message)` with `algorithm`. */
export function taggedHash(algorithm: HashAlgorithm, tag: DomainTag, message: Uint8Array): Uint8Array {
  return TaggedHash.of(algorithm, tag).hash(message);
}

/** The content hash of a canonical value with `algorithm`. */
export function contentHash(algorithm: HashAlgorithm, tag: DomainTag, value: Value): Uint8Array {
  return TaggedHash.of(algorithm, tag).content(value);
}
