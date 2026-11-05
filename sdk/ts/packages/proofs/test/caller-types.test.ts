import { describe, expect, it } from "vitest";

import { merkle, trie, versioned } from "../src/index.js";
import { expectErr, expectOk, int } from "./vectors.js";

/** What a JavaScript caller, or a cast, can pass past the types. */
const bad = (x: unknown): never => x as never;

describe("a committed value that is not a canonical value is refused through core's TypeMismatch, never thrown on", () => {
  const notAValue = "expected a canonical value";

  it("a Merkle leaf and a trie value hash refuse it as Encoding; a value still hashes", () => {
    const tagged = merkle.TaggedMerkleHashing.of("SHA-256");
    const hashing = trie.TrieHashing.of("SHA-256");
    for (const x of [5, null, "a", { kind: "int" }]) {
      expect(expectErr(tagged.leaf(bad(x)), "a leaf")).toEqual({ kind: "Encoding", reason: notAValue });
      expect(expectErr(hashing.valueHash(bad(x)), "a value hash")).toEqual({ kind: "Encoding", reason: notAValue });
    }
    expect(expectOk(tagged.leaf(int(1n)), "a value").length).toBe(32);
    expect(expectOk(hashing.valueHash(int(1n)), "a value").length).toBe(32);
  });

  it("a versioned value's check refuses it as Malformed, naming the key", () => {
    const vt = expectOk(versioned.VersionedTrie.of("SHA-256", expectOk(versioned.VersionKey.unsignedLong(8), "version key")), "profile");
    const key = expectOk(versioned.StateKey.parse("fiber/1"), "state key");
    for (const x of [5, null, "a"]) {
      expect(expectErr(vt.checkValue(key, bad(x)), "not a value")).toEqual({ kind: "Malformed", reason: `the value of fiber/1: ${notAValue}` });
    }
    expect(vt.checkValue(key, int(1n)).ok).toBe(true);
  });
});
