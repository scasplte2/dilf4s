import { runInNewContext } from "node:vm";

import { describe, expect, it } from "vitest";

import {
  blockSize,
  contentHash,
  digest,
  digestFromBytes,
  digestFromHex,
  DomainTag,
  expandMessageXmd,
  expansionLimit,
  fromHex,
  HASH_ALGORITHMS,
  hashParts,
  outputSize,
  parseHashAlgorithm,
  Preview,
  startHash,
  TaggedHash,
  taggedHash,
  toHex,
  type ExpansionRefusal,
  type HashAlgorithm,
  type Hashing,
  Value,
} from "../src/index.js";
import { isDomainTag as isDomainTagOf, MAX_OUTPUT_SIZE, takeBytesWithin } from "../src/internal.js";
import { expectOk, int } from "./vectors.js";

/** What a JavaScript caller, or a cast, can pass past the types. */
const bad = (x: unknown): never => x as never;
const mismatch = (reason: string): unknown => ({ ok: false, error: { kind: "TypeMismatch", reason } });
const notBytes = mismatch("expected a Uint8Array");
const notAString = mismatch("expected a string");
/** A test label for anything a caller may pass: an object is not read, so a Proxy's traps do not run for it. */
const label = (x: unknown): string => (typeof x === "object" && x !== null ? "an object" : `${typeof x} ${String(x)}`);

const sha: HashAlgorithm = "SHA-256";
const tag = expectOk(DomainTag.of("dilf4s/test/hash/v1"), "tag");
const message = Uint8Array.of(1, 2, 3, 4, 5);

/** A Uint8Array subclass whose `length` says 32 whatever it holds. */
class Liar extends Uint8Array {
  override get length(): number {
    return 32;
  }
}

/** A Proxy of real bytes, recording every trap it runs. */
function trapped(): { readonly proxy: Uint8Array; readonly traps: string[] } {
  const traps: string[] = [];
  const proxy = new Proxy(Uint8Array.of(1, 2, 3), {
    get: (t, name) => {
      traps.push(String(name));
      return Reflect.get(t, name) as unknown;
    },
  });
  return { proxy, traps };
}

/** A view of a SharedArrayBuffer holding `bytes`. */
function shared(bytes: Uint8Array): Uint8Array {
  const view = new Uint8Array(new SharedArrayBuffer(bytes.length));
  view.set(bytes);
  return view;
}

/** Everything that is not bytes, each a fresh instance: null, undefined, a number, an object posing as bytes. */
const notBytesCases = (): unknown[] => [null, undefined, 5, "abc", { length: 5, 0: 1, 1: 2 }, { [Symbol.toStringTag]: "Uint8Array", length: 5 }, [1, 2, 3], new Uint16Array(3), new DataView(new ArrayBuffer(3))];

describe("the hash API refuses an argument of the wrong type as TypeMismatch, never a throw or a misreading", () => {
  it("bytes that are not a Uint8Array, at every byte argument", () => {
    const h = expectOk(startHash(sha), "a hash");
    for (const x of notBytesCases()) {
      expect(digest(sha, bad(x)), label(x)).toEqual(notBytes);
      expect(digestFromBytes(sha, bad(x)), label(x)).toEqual(notBytes);
      expect(expandMessageXmd(sha, bad(x), Uint8Array.of(1), 32), label(x)).toEqual(notBytes);
      expect(expandMessageXmd(sha, message, bad(x), 32), label(x)).toEqual(notBytes);
      expect(expectOk(TaggedHash.of(sha, tag), "tagged").hash(bad(x)), label(x)).toEqual(notBytes);
      expect(taggedHash(sha, tag, bad(x)), label(x)).toEqual(notBytes);
      expect(h.update(bad(x)), label(x)).toEqual(notBytes);
      expect(toHex(bad(x)), label(x)).toEqual(notBytes);
      expect(hashParts(sha, [message, bad(x)]), label(x)).toEqual(mismatch("expected a Uint8Array at index 1"));
    }
    // the hash refused nothing into itself: it is still the hash of nothing
    expect(h.digest()).toEqual(expectOk(digest(sha, new Uint8Array(0)), "empty"));
    for (const x of [null, undefined, 5, "abc", { length: 1, 0: message }, message]) expect(hashParts(sha, bad(x)), label(x)).toEqual(mismatch("expected an array of Uint8Arrays"));
  });

  it("an algorithm that is not a string is a TypeMismatch; a string naming none is UnknownAlgorithm", () => {
    const hostile = { toString: (): string => "SHA-256" };
    const unknown = { ok: false, error: { kind: "UnknownAlgorithm", name: Preview.of("SHA-1") } };
    for (const a of [null, undefined, 256, hostile, new String("SHA-256")]) {
      expect(startHash(bad(a)), label(a)).toEqual(notAString);
      expect(outputSize(bad(a)), label(a)).toEqual(notAString);
      expect(blockSize(bad(a)), label(a)).toEqual(notAString);
      expect(expansionLimit(bad(a)), label(a)).toEqual(notAString);
      expect(parseHashAlgorithm(bad(a)), label(a)).toEqual(notAString);
      expect(digest(bad(a), message), label(a)).toEqual(notAString);
      expect(hashParts(bad(a), [message]), label(a)).toEqual(notAString);
      expect(digestFromBytes(bad(a), new Uint8Array(32)), label(a)).toEqual(notAString);
      expect(digestFromHex(bad(a), "00"), label(a)).toEqual(notAString);
      expect(expandMessageXmd(bad(a), message, Uint8Array.of(1), 32), label(a)).toEqual(notAString);
      expect(TaggedHash.of(bad(a), tag), label(a)).toEqual(notAString);
      expect(taggedHash(bad(a), tag, message), label(a)).toEqual(notAString);
      expect(contentHash(bad(a), tag, int(1n)), label(a)).toEqual(notAString);
    }
    for (const r of [startHash(bad("SHA-1")), outputSize(bad("SHA-1")), blockSize(bad("SHA-1")), expansionLimit(bad("SHA-1")), parseHashAlgorithm("SHA-1"), digest(bad("SHA-1"), message)]) {
      expect(r).toEqual(unknown);
    }
    // outputSize("MD5") was 32: an unknown name has no size
    expect(outputSize(bad("MD5")).ok).toBe(false);
    expect(expectOk(outputSize("SHA3-512"), "a size")).toBe(64);
    expect(expectOk(blockSize("SHA3-512"), "a block")).toBe(72);
    expect(expectOk(expansionLimit("SHA-512"), "a limit")).toBe(16320);
  });

  it("text that is not a string, unread: no toString runs", () => {
    const hostile = {
      toString: (): string => {
        throw new Error("read");
      },
    };
    for (const x of [null, undefined, 5, hostile, ["00"]]) {
      expect(fromHex(bad(x)), label(x)).toEqual(notAString);
      expect(digestFromHex(sha, bad(x)), label(x)).toEqual(notAString);
      expect(DomainTag.of(bad(x)), label(x)).toEqual(notAString);
    }
  });

  it("a tag that is not a DomainTag, and a value that is not a canonical value", () => {
    const fakeTag: unknown = Object.create(DomainTag.prototype);
    for (const t of [null, undefined, 5, "dilf4s/test/hash/v1", { value: "dilf4s/test/hash/v1" }, fakeTag]) {
      expect(TaggedHash.of(sha, bad(t)), label(t)).toEqual(mismatch("expected a DomainTag"));
      expect(taggedHash(sha, bad(t), message), label(t)).toEqual(mismatch("expected a DomainTag"));
      expect(contentHash(sha, bad(t), int(1n)), label(t)).toEqual(mismatch("expected a DomainTag"));
    }
    const t = expectOk(TaggedHash.of(sha, tag), "tagged");
    for (const v of [null, undefined, 5, { kind: "null" }, { kind: "int", value: 1n }]) {
      expect(contentHash(sha, tag, bad(v)), label(v)).toEqual(mismatch("expected a canonical value"));
      expect(t.content(bad(v)), label(v)).toEqual(mismatch("expected a canonical value"));
      expect(t.contentWithin(bad(v), 100), label(v)).toEqual(mismatch("expected a canonical value"));
    }
    expect(t.contentWithin(int(1n), bad("100"))).toEqual(mismatch("the byte limit is not a number"));
    expect(expectOk(contentHash(sha, tag, int(1n)), "hashes")).toEqual(expectOk(t.content(int(1n)), "hashes"));
  });

  it("an output length that is not a number is a TypeMismatch; one that is a number out of range is outputLength", () => {
    const refused = (refusal: ExpansionRefusal) => ({ ok: false, error: { kind: "InvalidExpansion", refusal, algorithm: sha, limit: 8160 } });
    for (const n of ["32", 32n, null, undefined, { valueOf: () => 32 }]) expect(expandMessageXmd(sha, message, Uint8Array.of(1), bad(n)), label(n)).toEqual(mismatch("the output length is not a number"));
    for (const n of [Number.NaN, 1.5, 0, -1, 8161, Number.POSITIVE_INFINITY]) expect(expandMessageXmd(sha, message, Uint8Array.of(1), n), String(n)).toEqual(refused("outputLength"));
    expect(expandMessageXmd(sha, message, new Uint8Array(0), 32)).toEqual(refused("emptyDst"));
  });

  it("a Uint8Array subclass that lies about its length is read by its internal length, at every byte argument", () => {
    const liar = new Liar(Uint8Array.of(1, 2, 3, 4, 5));
    expect(liar.length, "the lie").toBe(32);
    const plain = Uint8Array.of(1, 2, 3, 4, 5);
    expect(digest(sha, liar)).toEqual(digest(sha, plain));
    expect(hashParts(sha, [liar, liar])).toEqual(hashParts(sha, [plain, plain]));
    expect(taggedHash(sha, tag, liar)).toEqual(taggedHash(sha, tag, plain));
    expect(toHex(liar)).toEqual({ ok: true, value: "0102030405" });
    // a message read by its own length: not 32 bytes past what it holds
    expect(expandMessageXmd(sha, liar, Uint8Array.of(9), 32)).toEqual(expandMessageXmd(sha, plain, Uint8Array.of(9), 32));
    expect(expandMessageXmd(sha, plain, liar, 32)).toEqual(expandMessageXmd(sha, plain, plain, 32));
    expect(digestFromBytes(sha, liar)).toEqual({ ok: false, error: { kind: "InvalidLength", algorithm: sha, expected: 32, actual: 5 } });
    const h = expectOk(startHash(sha), "a hash");
    expectOk(h.update(liar), "absorbs");
    expect(h.digest()).toEqual(expectOk(digest(sha, plain), "plain"));
    // an own accessor is not believed either, and an empty DST claiming bytes is empty
    const own = Uint8Array.of(7, 8, 9);
    Object.defineProperty(own, "length", { get: () => 0 });
    expect(expandMessageXmd(sha, plain, own, 32)).toEqual(expandMessageXmd(sha, plain, Uint8Array.of(7, 8, 9), 32));
    expect(expandMessageXmd(sha, plain, new Liar(0), 32)).toEqual({ ok: false, error: { kind: "InvalidExpansion", refusal: "emptyDst", algorithm: sha, limit: 8160 } });
  });

  it("another realm's Uint8Array, and a Buffer, are bytes", () => {
    const foreign = runInNewContext("Uint8Array.of(1, 2, 3, 4, 5)") as Uint8Array;
    expect(foreign instanceof Uint8Array).toBe(false);
    for (const b of [foreign, Buffer.from([1, 2, 3, 4, 5])]) {
      expect(digest(sha, b)).toEqual(digest(sha, message));
      expect(hashParts(sha, [b])).toEqual(digest(sha, message));
      expect(taggedHash(sha, tag, b)).toEqual(taggedHash(sha, tag, message));
      expect(expandMessageXmd(sha, b, b, 32)).toEqual(expandMessageXmd(sha, message, message, 32));
      expect(toHex(b)).toEqual({ ok: true, value: "0102030405" });
    }
    const foreign32 = runInNewContext("new Uint8Array(32).fill(5)") as Uint8Array;
    const d = expectOk(digestFromBytes(sha, foreign32), "a digest");
    expect(Object.getPrototypeOf(d), "a fresh plain array").toBe(Uint8Array.prototype);
    expect(Array.from(d)).toEqual(Array<number>(32).fill(5));
  });

  it("a Proxy of a Uint8Array is not bytes, and none of its traps runs", () => {
    const { proxy, traps } = trapped();
    expect(digest(sha, proxy)).toEqual(notBytes);
    expect(hashParts(sha, [proxy])).toEqual(mismatch("expected a Uint8Array at index 0"));
    expect(digestFromBytes(sha, proxy)).toEqual(notBytes);
    expect(expandMessageXmd(sha, proxy, Uint8Array.of(1), 32)).toEqual(notBytes);
    expect(taggedHash(sha, tag, proxy)).toEqual(notBytes);
    expect(toHex(proxy)).toEqual(notBytes);
    expect(expectOk(startHash(sha), "a hash").update(proxy)).toEqual(notBytes);
    expect(traps).toEqual([]);
  });

  it("a view of a SharedArrayBuffer is bytes; a digest from it is a copy in unshared memory", () => {
    const s = shared(message);
    expect(digest(sha, s)).toEqual(digest(sha, message));
    expect(hashParts(sha, [s, s])).toEqual(hashParts(sha, [message, message]));
    expect(expandMessageXmd(sha, s, s, 32)).toEqual(expandMessageXmd(sha, message, message, 32));
    expect(toHex(s)).toEqual({ ok: true, value: "0102030405" });
    const d32 = shared(new Uint8Array(32).fill(9));
    const d = expectOk(digestFromBytes(sha, d32), "a digest");
    d32.fill(0);
    expect(d.buffer instanceof ArrayBuffer, "unshared").toBe(true);
    expect(Array.from(d), "the bytes at the call").toEqual(Array<number>(32).fill(9));
  });

  it("an incremental hash may be read more than once, and absorbing goes on after a digest", () => {
    const h = expectOk(startHash(sha), "a hash");
    expectOk(h.update(Uint8Array.of(1, 2)), "absorbs");
    const first = h.digest();
    expect(h.digest(), "the same digest again").toEqual(first);
    expectOk(h.update(Uint8Array.of(3)), "absorbs after a digest");
    expect(h.digest()).toEqual(expectOk(digest(sha, Uint8Array.of(1, 2, 3)), "1, 2, 3"));
    const t = expectOk(expectOk(TaggedHash.of(sha, tag), "tagged").start(), "starts");
    expectOk(t.update(message), "absorbs");
    expect(t.digest()).toEqual(expectOk(taggedHash(sha, tag, message), "tagged"));
  });

  it("a DomainTag built by calling its constructor from outside is not one: no tag outside the grammar reaches a hash", () => {
    const forged = Reflect.construct(DomainTag, [Symbol("dilf4s domain tag"), "Not A Tag!"]) as unknown;
    const keyless = Reflect.construct(DomainTag, [undefined, "Not A Tag!"]) as unknown;
    for (const t of [forged, keyless]) {
      expect(TaggedHash.of(sha, bad(t))).toEqual(mismatch("expected a DomainTag"));
      expect(taggedHash(sha, bad(t), message)).toEqual(mismatch("expected a DomainTag"));
    }
  });

  it("a Proxy reporting a length no array has is refused, never looped over", { timeout: 5_000 }, () => {
    for (const length of [Number.POSITIVE_INFINITY, 2 ** 53, 1.5, -1, Symbol("length")]) {
      const parts = new Proxy([], { get: (t, name) => (name === "length" ? length : typeof name === "string" && /^[0-9]+$/.test(name) ? message : (Reflect.get(t, name) as unknown)) });
      expect(hashParts(sha, parts), String(length)).toEqual(mismatch("expected an array of Uint8Arrays"));
    }
  });

  it("a revoked Proxy is refused, never thrown on", () => {
    const { proxy, revoke } = Proxy.revocable([], {});
    revoke();
    expect(hashParts(sha, bad(proxy))).toEqual(mismatch("expected an array of Uint8Arrays"));
    expect(hashParts(sha, bad([message, proxy]))).toEqual(mismatch("expected a Uint8Array at index 1"));
    // revoked by its own trap as its length is read: the parts' reads are refused, never thrown on
    const { proxy: revoking, revoke: revokeParts } = Proxy.revocable([message, message], {
      get: (t, name) => {
        const v: unknown = Reflect.get(t, name);
        revokeParts();
        return v;
      },
    });
    expect(hashParts(sha, bad(revoking))).toEqual(mismatch("expected a Uint8Array at index 0"));
    expect(digest(sha, bad(proxy))).toEqual(notBytes);
  });

  it("a TaggedHash built by calling its constructor from outside reads nothing it is given and hashes nothing", () => {
    const tag = {
      get value(): never {
        throw new Error("never read");
      },
    };
    const forged = Reflect.construct(TaggedHash as unknown as new (...a: unknown[]) => object, [Symbol("key"), "SHA-512", tag]) as TaggedHash;
    expect([forged.algorithm, forged.tag.value], "its own defaults, never the caller's").toEqual(["SHA-256", ""]);
    const notATaggedHash = mismatch("expected a TaggedHash");
    expect(forged.hash(message), "hash").toEqual(notATaggedHash);
    expect(forged.content(int(1n)), "content").toEqual(notATaggedHash);
    expect(forged.contentWithin(int(1n), 64), "contentWithin").toEqual(notATaggedHash);
    expect(forged.start(), "start: no stream over a tag outside the grammar").toEqual(notATaggedHash);
    // borrowed onto another receiver, start refuses it the same way
    // eslint-disable-next-line @typescript-eslint/unbound-method -- the point is a method borrowed onto another receiver
    expect(Reflect.apply(TaggedHash.prototype.start, { algorithm: "SHA-256" }, []), "borrowed start").toEqual(notATaggedHash);
    expect(isDomainTagOf(forged.tag), "its tag is no DomainTag").toBe(false);
    const fakeTag = Reflect.construct(DomainTag as unknown as new (...a: unknown[]) => object, [Symbol("key"), "Not A Tag"]) as DomainTag;
    expect(fakeTag.value, "a forged tag keeps nothing").toBe("");
  });

  it("a hash stream built from its constructor, or a Proxy of one, absorbs nothing and never throws", () => {
    const genuine = expectOk(startHash(sha), "a stream");
    const Stream = genuine.constructor as new (...a: unknown[]) => Hashing;
    let ran = 0;
    const fake = { update: () => (ran += 1), clone: () => ({ digest: () => ((ran += 1), new Uint8Array(32).fill(7)) }) };
    const notAHashing = mismatch("expected a Hashing made by startHash or TaggedHash.start");
    for (const forged of [new Stream({}), new Stream(fake), new Stream(Symbol("key"), fake), new Proxy(genuine, {})]) {
      expect(forged.update(message), "update").toEqual(notAHashing);
      expect(forged.digest(), "digest").toEqual(new Uint8Array(0));
    }
    expect(ran, "no method of the caller's ran").toBe(0);
    expect(genuine.digest(), "the genuine stream absorbed nothing either").toEqual(expectOk(digest(sha, new Uint8Array(0)), "empty"));
  });

  it("content checks its receiver before reading anything of it: null, a revoked Proxy, a forged receiver", () => {
    // eslint-disable-next-line @typescript-eslint/unbound-method -- the point is a method borrowed onto another receiver
    const content = TaggedHash.prototype.content;
    const { proxy, revoke } = Proxy.revocable({}, {});
    revoke();
    const forged = { contentWithin: () => ({ ok: true, value: new Uint8Array(32) }) };
    for (const [name, receiver] of [["null", null], ["undefined", undefined], ["a revoked Proxy", proxy], ["a forged receiver", forged]] as const) {
      expect(Reflect.apply(content, receiver, [int(1n)]), name).toEqual(mismatch("expected a TaggedHash"));
    }
  });

  it("a bounded argument longer than its width is never copied: its view is kept, and refused by its length", () => {
    const big = new Uint8Array(65);
    const kept = takeBytesWithin(big, MAX_OUTPUT_SIZE);
    big[0] = 9;
    expect(kept?.[0], "a view of the caller's memory, not a copy").toBe(9);
    const small = new Uint8Array(64);
    const owned = takeBytesWithin(small, MAX_OUTPUT_SIZE);
    small[0] = 9;
    expect(owned?.[0], "a copy, owned").toBe(0);
    expect(MAX_OUTPUT_SIZE, "the longest digest").toBe(64);
  });

  it("an exception from caller code propagates, with no verdict, and an iterator opened is closed (canonical-encoding.md §1)", () => {
    // a getter that throws: the caller's own exception, unchanged
    const throwing: unknown[] = [message];
    Object.defineProperty(throwing, 1, {
      get: (): never => {
        throw new RangeError("the caller's own");
      },
    });
    expect(() => hashParts(sha, bad(throwing)), "a throwing getter").toThrow(new RangeError("the caller's own"));
    // a Proxy whose handler is a revoked Proxy: the engine throws while running the caller's handler, at the length read
    const { proxy: handler, revoke } = Proxy.revocable({}, {});
    revoke();
    const broken = new Proxy([message], handler);
    expect(() => hashParts(sha, bad(broken)), "a revoked handler").toThrow(TypeError);
    // an iterable whose entry's getter throws: the exception propagates, and the iterator is closed first
    let closed = 0;
    const entry: unknown[] = ["a"];
    Object.defineProperty(entry, 1, {
      get: (): never => {
        throw new RangeError("the caller's own");
      },
    });
    const iterable = {
      [Symbol.iterator]: () => ({ next: () => ({ done: false, value: entry }), return: () => ((closed += 1), { done: true }) }),
    };
    expect(() => Value.map(bad(iterable)), "a throwing entry").toThrow(new RangeError("the caller's own"));
    expect(closed, "the iterator was closed").toBe(1);
  });

  it("the algorithm list cannot be extended by a caller", () => {
    expect(Object.isFrozen(HASH_ALGORITHMS)).toBe(true);
    expect(() => (HASH_ALGORITHMS as HashAlgorithm[]).push(bad("MD5"))).toThrow(TypeError);
    expect(outputSize(bad("MD5")).ok).toBe(false);
  });
});
