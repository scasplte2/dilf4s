import { describe, expect, it } from "vitest";

import { models } from "@kd5ujc/dilf4s-core";

import { trie } from "../src/index.js";
import * as B from "../src/bits.js";
import { bytesEqual, compareBytes } from "../src/bytes.js";
import * as L from "../src/listing.js";
import { expectOk, int, hexOf } from "./vectors.js";

const T = expectOk(trie.TrieHashing.of("SHA-256"), "TrieHashing.of");

// ------------------------------------------------------------------ the walks as they were before (trie.md section 4)

/** Work done the old way: bytes built for reconstructed paths, and bits compared from bit 0 at every node. */
interface OldWork {
  built: number;
  compared: number;
}

function concat(parts: readonly B.Bits[], w: OldWork): B.Bits {
  const n = parts.reduce((t, p) => t + p.length, 0);
  const out = new Uint8Array((n + 7) >> 3);
  w.built += out.length;
  let at = 0;
  for (const p of parts) {
    for (let i = 0; i < p.length; i++) if (B.bit(p, i)) out[(at + i) >> 3] = (out[(at + i) >> 3] ?? 0) | (0x80 >> ((at + i) & 7));
    at += p.length;
  }
  return { bytes: out, length: n };
}

const append = (s: B.Bits, b: boolean, w: OldWork): B.Bits => concat([s, b ? { bytes: Uint8Array.of(0x80), length: 1 } : { bytes: Uint8Array.of(0), length: 1 }], w);

function common(a: B.Bits, b: B.Bits, w: OldWork): number {
  const c = B.commonPrefixFrom(a, 0, b, 0);
  w.compared += c + 1;
  return c;
}

function lt(a: B.Bits, b: B.Bits, w: OldWork): boolean {
  const c = common(a, b, w);
  if (c === a.length) return a.length < b.length;
  if (c === b.length) return false;
  return !B.bit(a, c);
}

const lteq = (a: B.Bits, b: B.Bits, w: OldWork): boolean => (a.length === b.length && common(a, b, w) === a.length) || lt(a, b, w);
const startsWith = (s: B.Bits, p: B.Bits, w: OldWork): boolean => p.length <= s.length && common(s, p, w) === p.length;
const intersects = (prefix: B.Bits, from: B.Bits, until: B.Bits | undefined, w: OldWork): boolean =>
  !(!startsWith(from, prefix, w) && lt(prefix, from, w)) && !(until !== undefined && lteq(until, prefix, w));
const within = (key: B.Bits, from: B.Bits, until: B.Bits | undefined, w: OldWork): boolean => lteq(from, key, w) && (until === undefined || lt(key, until, w));

type Entries = L.KeyedHash[];
const entry = (key: B.Bits, valueHash: Uint8Array): L.KeyedHash => ({ key: B.toBytes(key), valueHash });

function oldGather(top: L.WalkNode, at: B.Bits, w: OldWork): Entries | undefined {
  const out: Entries = [];
  const todo: (readonly [L.WalkNode, B.Bits])[] = [[top, at]];
  for (let t = todo.pop(); t !== undefined; t = todo.pop()) {
    const [node, path] = t;
    if (node.kind === "stub") return undefined;
    const here = concat([path, node.segment], w);
    if (node.kind === "leaf") {
      out.push(entry(here, node.value));
      continue;
    }
    if (node.value !== undefined) out.push(entry(here, node.value));
    if (node.right !== undefined) todo.push([node.right, append(here, true, w)]);
    if (node.left !== undefined) todo.push([node.left, append(here, false, w)]);
  }
  return out;
}

function oldPrefix(top: L.WalkNode | undefined, prefix: Uint8Array, w: OldWork): Entries | undefined {
  const p = B.bitsOf(prefix);
  let node = top;
  let consumed: B.Bits = B.EMPTY_BITS;
  let at = 0;
  for (;;) {
    if (node === undefined) return [];
    if (node.kind === "stub") return undefined;
    const remaining = B.drop(p, at);
    if (startsWith(node.segment, remaining, w)) return oldGather(node, consumed, w);
    if (node.kind === "leaf" || !startsWith(remaining, node.segment, w)) return [];
    at += node.segment.length;
    const b = B.bit(p, at);
    consumed = append(concat([consumed, node.segment], w), b, w);
    node = b ? node.right : node.left;
    at += 1;
  }
}

function oldRange(top: L.WalkNode | undefined, from: Uint8Array, until: Uint8Array | undefined, w: OldWork): Entries | undefined {
  const lo = B.bitsOf(from);
  const hi = until === undefined ? undefined : B.bitsOf(until);
  if (top === undefined || !intersects(B.EMPTY_BITS, lo, hi, w)) return [];
  const out: Entries = [];
  const todo: (readonly [L.WalkNode, B.Bits])[] = [[top, B.EMPTY_BITS]];
  for (let t = todo.pop(); t !== undefined; t = todo.pop()) {
    const [node, at] = t;
    if (node.kind === "stub") return undefined;
    const here = concat([at, node.segment], w);
    if (node.kind === "leaf") {
      if (within(here, lo, hi, w)) out.push(entry(here, node.value));
      continue;
    }
    if (node.value !== undefined && within(here, lo, hi, w)) out.push(entry(here, node.value));
    const right = append(here, true, w);
    const left = append(here, false, w);
    if (node.right !== undefined && intersects(right, lo, hi, w)) todo.push([node.right, right]);
    if (node.left !== undefined && intersects(left, lo, hi, w)) todo.push([node.left, left]);
  }
  return out;
}

const same = (a: Entries, b: readonly L.KeyedHash[]): boolean =>
  a.length === b.length && a.every((x, i) => bytesEqual(x.key, b[i]?.key ?? new Uint8Array(0)) && bytesEqual(x.valueHash, b[i]?.valueHash ?? new Uint8Array(0)));
const oldLists = (listing: Entries | undefined, claimed: readonly L.KeyedHash[]): boolean | undefined => (listing === undefined ? undefined : same(listing, claimed));

const hexed = (es: readonly L.KeyedHash[] | undefined): unknown => es?.map((e) => [hexOf(e.key), hexOf(e.valueHash)]) ?? "notCovered";

// ------------------------------------------------------------------ random partial tries

/** A seeded generator (mulberry32), so every run checks the same cases. */
function rng(seed: number): () => number {
  let a = seed;
  return () => {
    a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

const ALPHABET = [0x00, 0x01, 0x7f, 0x80, 0xff];

function key(r: () => number, maxLen: number): Uint8Array {
  const n = Math.floor(r() * (maxLen + 1));
  return Uint8Array.from({ length: n }, () => ALPHABET[Math.floor(r() * ALPHABET.length)] ?? 0);
}

const valueOf = (i: number): Uint8Array => Uint8Array.from({ length: 32 }, (_, j) => (i * 31 + j) & 0xff);

/** The trie holding `keys` (deduplicated), with each subtree a stub with probability `stubs`. */
function partial(keys: readonly Uint8Array[], stubs: number, r: () => number): L.WalkNode | undefined {
  const sorted = [...keys].sort(compareBytes).filter((k, i, a) => i === 0 || !bytesEqual(a[i - 1] ?? new Uint8Array(0), k));
  if (sorted.length === 0) return undefined;
  const bits = sorted.map((k) => B.bitsOf(k));
  const at = (i: number): B.Bits => bits[i] ?? B.EMPTY_BITS;
  const build = (lo: number, hi: number, offset: number): L.WalkNode => {
    if (r() < stubs) return { kind: "stub" };
    if (hi - lo === 1) return { kind: "leaf", segment: B.drop(at(lo), offset), value: valueOf(lo) };
    const pos = offset + B.commonPrefixFrom(at(lo), offset, at(hi - 1), offset);
    const here = at(lo).length === pos;
    const start = here ? lo + 1 : lo;
    let split = start;
    while (split < hi && !B.bit(at(split), pos)) split++;
    return {
      kind: "branch",
      segment: B.slice(at(lo), offset, pos),
      value: here ? valueOf(lo) : undefined,
      left: split > start ? build(start, split, pos + 1) : undefined,
      right: hi > split ? build(split, hi, pos + 1) : undefined,
    };
  };
  return build(0, sorted.length, 0);
}

/** Claims near the truth: itself, empty, one short at either end, reversed, one extra, a wrong value, a longer key. */
function claimsNear(truth: Entries, other: Entries): Entries[] {
  const first = truth[0];
  return [
    truth,
    [],
    truth.slice(1),
    truth.slice(0, -1),
    [...truth].reverse(),
    [...truth, ...other.slice(0, 1)],
    [...other.slice(0, 1), ...truth],
    truth.map((e) => ({ key: e.key, valueHash: valueOf(-1) })),
    first === undefined ? [] : [{ key: Uint8Array.from([...first.key, 0]), valueHash: first.valueHash }, ...truth.slice(1)],
    other,
  ];
}

describe("listing walks answer as the old ones did", { timeout: 60_000 }, () => {
  it("for random partial tries, bounds and claims", () => {
    const r = rng(90);
    for (let round = 0; round < 400; round++) {
      const keys = Array.from({ length: Math.floor(r() * 20) }, () => key(r, 4));
      const top = partial(keys, round % 3 === 0 ? 0 : 0.15, r);
      const whole = oldGather(top ?? { kind: "stub" }, B.EMPTY_BITS, { built: 0, compared: 0 }) ?? [];
      const bound = (): Uint8Array => (r() < 0.5 && keys.length > 0 ? (keys[Math.floor(r() * keys.length)] ?? new Uint8Array(0)) : key(r, 5));
      for (let q = 0; q < 6; q++) {
        const from = bound();
        const until = r() < 0.3 ? undefined : r() < 0.15 ? from : bound();
        const w = { built: 0, compared: 0 };
        const range = oldRange(top, from, until, w);
        const prefix = oldPrefix(top, from, w);
        const what = `round ${round.toString()}, [${hexOf(from)}, ${until === undefined ? "" : hexOf(until)})`;
        expect(hexed(L.listRange(top, from, until)), `${what}: range`).toEqual(hexed(range));
        expect(hexed(L.listPrefix(top, from)), `${what}: prefix`).toEqual(hexed(prefix));
        for (const c of claimsNear(range ?? [], whole)) expect(L.listsRange(top, from, until, c), `${what}: range check`).toBe(oldLists(range, c));
        for (const c of claimsNear(prefix ?? [], whole)) expect(L.listsPrefix(top, from, c), `${what}: prefix check`).toBe(oldLists(prefix, c));
      }
    }
  });
});

// ------------------------------------------------------------------ the deep chain (issue #91): work, not time

const v = expectOk(T.valueHash(int(1n)), "encodes");
const seven: B.Bits = { bytes: Uint8Array.of(0), length: 7 };
const zeros = (k: number): Uint8Array => new Uint8Array(k);

/** Keys `00^k` for `k = 0..n`: a chain of `n` branches (a 7-bit zero segment, the first empty) and a leaf. */
function chain(n: number): L.WalkNode {
  let node: L.WalkNode = { kind: "leaf", segment: seven, value: v };
  for (let k = n - 1; k >= 0; k--) node = { kind: "branch", segment: k === 0 ? B.EMPTY_BITS : seven, value: v, left: node, right: undefined };
  return node;
}

function chainRecords(n: number): models.trie.TrieRecord[] {
  const recs: models.trie.TrieRecord[] = [{ s: { n: 0n, b: new Uint8Array(0) }, v, c: "left" }];
  for (let k = 1; k < n; k++) recs.push({ s: { n: 7n, b: Uint8Array.of(0) }, v, c: "left" });
  recs.push({ s: { n: 7n, b: Uint8Array.of(0) }, v });
  return recs;
}

/** The proof's weight in bits, as gas meters it. */
const proofBits = (n: number): number => 8 * Number(expectOk(models.trie.TrieProof.encode(chainRecords(n)), "records").weight);

const listing = (from: number, to: number): Entries => Array.from({ length: to - from + 1 }, (_, i) => ({ key: zeros(from + i), valueHash: v }));

interface Deep {
  readonly name: string;
  readonly run: (top: L.WalkNode, work: L.Work) => boolean | undefined;
  readonly expected: boolean;
  readonly boundBits: number;
  readonly claimBits: number;
}

const keyBits = (es: Entries): number => es.reduce((t, e) => t + 8 * e.key.length, 0);

/** Issue #90's constructions: long bounds and a long prefix with an empty claim (false), and true claims. */
function deepCases(n: number): Deep[] {
  const long = zeros(n - 1);
  const past = Uint8Array.from([...long, 1]);
  const tail = listing(n - 1, n);
  const all = listing(0, n);
  return [
    { name: "long from", run: (t, w) => L.listsRange(t, long, undefined, [], w), expected: false, boundBits: 8 * long.length, claimBits: 0 },
    { name: "long until", run: (t, w) => L.listsRange(t, new Uint8Array(0), past, [], w), expected: false, boundBits: 8 * past.length, claimBits: 0 },
    { name: "long from and until", run: (t, w) => L.listsRange(t, long, past, [], w), expected: false, boundBits: 8 * (long.length + past.length), claimBits: 0 },
    { name: "long prefix", run: (t, w) => L.listsPrefix(t, long, [], w), expected: false, boundBits: 8 * long.length, claimBits: 0 },
    { name: "long from, the tail claimed", run: (t, w) => L.listsRange(t, long, undefined, tail, w), expected: true, boundBits: 8 * long.length, claimBits: keyBits(tail) },
    { name: "from 0x, everything claimed", run: (t, w) => L.listsRange(t, new Uint8Array(0), undefined, all, w), expected: true, boundBits: 0, claimBits: keyBits(all) },
    { name: "prefix 0x, everything claimed", run: (t, w) => L.listsPrefix(t, new Uint8Array(0), all, w), expected: true, boundBits: 0, claimBits: keyBits(all) },
  ];
}

describe("listing checks on a deep chain", { timeout: 60_000 }, () => {
  it("do work linear in the proof plus the bounds and the claim; the old walks were quadratic", () => {
    const n = 600;
    const top = chain(n);
    const bits = proofBits(n);
    for (const c of deepCases(n)) {
      const work = { steps: 0 };
      expect(c.run(top, work), c.name).toBe(c.expected);
      const bound = 2 * (bits + c.boundBits + c.claimBits);
      expect(work.steps, `${c.name}: steps within 2 * (proof + bounds + claim) = ${bound.toString()}`).toBeLessThanOrEqual(bound);
    }
    const old = { built: 0, compared: 0 };
    expect(oldRange(top, zeros(n - 1), undefined, old)?.length).toBe(2);
    expect(old.built + old.compared, "the old walk of a long from").toBeGreaterThan(2 * (bits + 8 * (n - 1)));
    const oldPrefixWork = { built: 0, compared: 0 };
    expect(oldPrefix(top, zeros(n - 1), oldPrefixWork)?.length).toBe(2);
    expect(oldPrefixWork.built, "the old walk of a long prefix builds its path at every level").toBeGreaterThan(2 * (bits + 8 * (n - 1)) / 8);
  });

  it("twice as deep takes twice the work, without recursion, at 20000 levels", () => {
    const [small, large] = [10000, 20000].map((n) => {
      const top = chain(n);
      // claiming every key claims Θ(n^2) bits, which the check compares: the doubling holds for linear claims
      return new Map(
        deepCases(n)
          .filter((c) => c.claimBits <= 16 * n)
          .map((c) => {
            const work = { steps: 0 };
            return [c.name, { verdict: c.run(top, work), steps: work.steps }] as const;
          }),
      );
    });
    for (const [name, a] of small ?? new Map<string, { verdict: boolean | undefined; steps: number }>()) {
      const b = large?.get(name);
      expect(b?.verdict, name).toBe(a.verdict);
      expect(b?.steps ?? Infinity, `${name}: ${a.steps.toString()} steps at 10000`).toBeLessThanOrEqual(2 * a.steps + 64);
    }
  });

  it("trie_range_verify and trie_prefix_verify answer through the public API", () => {
    const n = 2000;
    const mode = expectOk(trie.KeyMode.variable(65535), "mode");
    const recs = chainRecords(n);
    const root = expectOk(trie.trieRoot(T, mode, Array.from({ length: n + 1 }, (_, k) => [zeros(k), v] as const)), "root");
    const proof = expectOk(models.trie.TrieProof.encode(recs), "records");
    const one = int(1n);
    expect(expectOk(trie.trieRangeVerify(T, mode, root, zeros(n - 1), null, [], proof), "verdict")).toBe(false);
    expect(expectOk(trie.trieRangeVerify(T, mode, root, zeros(n - 1), null, [[zeros(n - 1), one], [zeros(n), one]], proof), "verdict")).toBe(true);
    expect(expectOk(trie.trieRangeVerify(T, mode, root, new Uint8Array(0), Uint8Array.from([...zeros(n - 1), 1]), [], proof), "verdict")).toBe(false);
    expect(expectOk(trie.triePrefixVerify(T, mode, root, zeros(n - 1), [], proof), "verdict")).toBe(false);
    expect(expectOk(trie.triePrefixVerify(T, mode, root, zeros(n - 1), [[zeros(n - 1), one], [zeros(n), one]], proof), "verdict")).toBe(true);
  });
});
