/**
 * Tagged hashing with this package's literal tags (`canonical-encoding.md` section 4). Core's `TaggedHash` takes a
 * `DomainTag`, which only a fallible factory builds; these literals are fixed, so the construction is spelled out here
 * and a test checks every literal against the grammar and against core's `TaggedHash`.
 */
import type { HashAlgorithm } from "@kd5ujc/dilf4s-core";
import { digestOf, hashPartsOf } from "@kd5ujc/dilf4s-core/internal";
import { utf8ToBytes } from "@noble/hashes/utils.js";

export const TAGS = Object.freeze({
  schnorr: "dilf4s/sig/bip340/v1",
  kesSeed: "dilf4s/kes/seed/v1",
  kesLeaf: "dilf4s/kes/leaf/v1",
  kesNode: "dilf4s/kes/node/v1",
  kesProduct: "dilf4s/kes/product/v1",
} as const);

/**
 * `m => H(H(tag) || H(tag) || m)`, hashing the tag once. Each call hands the hash a copy of the tag hash, never the
 * kept array: a hash's `update` can be replaced on its prototype, and a replacement that kept and rewrote the array
 * would change every later hash under this tag.
 */
export function literalTaggedHash(algorithm: HashAlgorithm, tag: string): (message: Uint8Array) => Uint8Array {
  const t = digestOf(algorithm, utf8ToBytes(tag));
  return (message) => {
    const copy = Uint8Array.from(t);
    return hashPartsOf(algorithm, [copy, copy, message]);
  };
}
