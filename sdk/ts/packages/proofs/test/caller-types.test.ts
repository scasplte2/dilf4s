import { describe, expect, it } from "vitest";

import { Value } from "@kd5ujc/dilf4s-core";

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

describe("caller bytes and claims in the trie and the versioned trie are checked, and read by their own length", () => {
  /** `xs` as a Uint8Array whose own `length` lies. */
  const lying = (xs: readonly number[], length: number): Uint8Array => Object.defineProperty(Uint8Array.from(xs), "length", { get: () => length });
  const hashing = trie.TrieHashing.of("SHA-256");
  const variable = expectOk(trie.KeyMode.variable(8), "mode");
  const fixed2 = expectOk(trie.KeyMode.fixed(2), "mode");

  it("a key that is not bytes is InvalidKey, and a key's length is its own", () => {
    for (const x of [5, "ab", null, [1, 2]]) expect(expectErr(fixed2.checkKey(bad(x)), "not bytes")).toEqual({ kind: "InvalidKey", reason: "expected a Uint8Array" });
    expect(fixed2.checkKey(lying([1, 2], 3)).ok, "two bytes claiming three").toBe(true);
    expect(expectErr(variable.checkKey(lying(Array.from({ length: 9 }, () => 0), 1)), "nine bytes claiming one")).toMatchObject({ kind: "InvalidKey" });
  });

  it("a claimed value that is not a value is Encoding, before its weight is read", () => {
    const refused = { kind: "Encoding", reason: "a claimed value is not a canonical value" };
    const empty = hashing.empty;
    for (const x of [5, undefined, "a", { kind: "int", weight: 1n }]) {
      expect(expectErr(trie.trieVerify(hashing, variable, empty, Uint8Array.of(1), bad(x), Value.null), "a claim")).toEqual(refused);
      expect(expectErr(trie.triePrefixVerify(hashing, variable, empty, new Uint8Array(0), [[Uint8Array.of(1), bad(x)]], Value.null), "an entry")).toEqual(refused);
    }
  });

  it("a root is named by its own length", () => {
    const root = new Uint8Array(32).fill(0xab);
    expect(trie.rootText(lying([...root], 3), 32)).toBe("ab".repeat(32));
    expect(trie.rootText(lying([1, 2, 3], 32), 32)).toBe("a root of 3 bytes");
    expect(trie.rootText(bad(5), 32)).toBe("a root of 0 bytes");
  });

  it("state keys and namespaces refuse what is not text or bytes, and read bytes by their own length", () => {
    for (const x of [5, null, undefined, ["a"]]) {
      expect(expectErr(versioned.StateKey.parse(bad(x)), "not text")).toEqual({ kind: "TypeMismatch", reason: "expected a string" });
      expect(expectErr(versioned.StateNamespace.parse(bad(x)), "not text")).toEqual({ kind: "TypeMismatch", reason: "expected a string" });
    }
    for (const x of [5, "fiber/1", null, [0x61]]) expect(expectErr(versioned.StateKey.fromBytes(bad(x)), "not bytes")).toEqual({ kind: "TypeMismatch", reason: "expected a Uint8Array" });
    const fiber = [...new TextEncoder().encode("fiber/1")];
    expect(expectOk(versioned.StateKey.fromBytes(lying(fiber, 1_000)), "seven bytes claiming 1000").value).toBe("fiber/1");
    expect(expectOk(versioned.StateKey.fromBytes(lying(fiber, 1)), "seven bytes claiming one").value).toBe("fiber/1");
  });
});
