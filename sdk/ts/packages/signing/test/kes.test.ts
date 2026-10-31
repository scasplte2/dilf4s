import { inspect } from "node:util";

import { describe, expect, test } from "vitest";

import { digest, hashAlgorithm, models, toHex, type HashAlgorithm, type Result } from "@kd5ujc/dilf4s-core";
import { sha256 } from "@noble/hashes/sha2.js";

import { CardanoSumKes, KesProduct, KesSum, type CardanoSumKesKey, type KesError, type KesProductKey, type KesSumKey } from "../src/index.js";
import { concat } from "../src/internal/bytes.js";
import { expectOk, h, loadVectors, readDocBytes, tsv } from "./vectors.js";

const vectors = loadVectors("kes_v1.json", models.kesVectors.KesVectors);
const kind = <T>(r: Result<T, KesError>): string => (r.ok ? "ok" : r.error.kind);
const beef = h("beef");

/** One construction behind a common face, for the conformance replay. */
interface Face {
  readonly totalPeriods: number;
  generate(seed: Uint8Array): Result<FaceKey, KesError>;
  load(raw: Uint8Array, period: number): Result<FaceKey, KesError>;
  verify(vk: Uint8Array, period: number, message: Uint8Array, sig: Uint8Array): Result<boolean, KesError>;
}
interface FaceKey {
  readonly period: number;
  readonly bytes: Uint8Array;
  readonly verifyingKey: Uint8Array;
  sign(message: Uint8Array): Uint8Array;
  evolve(): Result<FaceKey, KesError>;
}

function faceOf(v: models.kesVectors.KesVector): Face {
  const p = v.parameters;
  const hash = (name: string): HashAlgorithm => {
    const a = hashAlgorithm(name);
    if (a === undefined) throw new Error(`unknown hash ${name}`);
    return a;
  };
  const sumP = p as models.kesVectors.KesSumParameters;
  const productP = p as models.kesVectors.KesProductParameters;
  const cardanoP = p as models.kesVectors.KesCardanoParameters;
  const wrap = <K extends { period: number; bytes: Uint8Array; verifyingKey: Uint8Array; evolve(): Result<K, KesError> }>(
    sign: (k: K, m: Uint8Array) => Uint8Array,
  ) => {
    const key = (k: K): FaceKey => ({
      period: k.period,
      bytes: k.bytes,
      verifyingKey: k.verifyingKey,
      sign: (m) => sign(k, m),
      evolve: () => {
        const n = k.evolve();
        return n.ok ? { ok: true, value: key(n.value) } : n;
      },
    });
    return (r: Result<K, KesError>): Result<FaceKey, KesError> => (r.ok ? { ok: true, value: key(r.value) } : r);
  };
  switch (v.construction) {
    case "dilf4s-sum": {
      const s = expectOk(KesSum.of(hash(sumP.hash), Number(sumP.height)), "scheme");
      const w = wrap<KesSumKey>((k, m) => expectOk(k.sign(m), "sign"));
      return { totalPeriods: s.totalPeriods, generate: (x) => w(s.generate(x)), load: (x, t) => w(s.load(x, t)), verify: (...a) => s.verify(...a) };
    }
    case "dilf4s-product": {
      const s = expectOk(KesProduct.of(hash(productP.hash), Number(productP.superHeight), Number(productP.subHeight)), "scheme");
      const w = wrap<KesProductKey>((k, m) => expectOk(k.sign(m), "sign"));
      return { totalPeriods: s.totalPeriods, generate: (x) => w(s.generate(x)), load: (x, t) => w(s.load(x, t)), verify: (...a) => s.verify(...a) };
    }
    case "cardano-sum": {
      const s = expectOk(CardanoSumKes.of(Number(cardanoP.depth)), "scheme");
      const w = wrap<CardanoSumKesKey>((k, m) => k.sign(m));
      return { totalPeriods: s.totalPeriods, generate: (x) => w(s.generate(x)), load: (x, t) => w(s.load(x, t)), verify: (...a) => s.verify(...a) };
    }
  }
}

describe("kes_v1.json", { timeout: 60_000 }, () => {
  test("covers the three constructions", () => {
    expect(new Set(vectors.vectors.map((v) => v.construction))).toEqual(new Set(["dilf4s-sum", "dilf4s-product", "cardano-sum"]));
  });

  vectors.vectors.forEach((v, i) => {
    test(`#${String(i)} ${v.construction} ${JSON.stringify(v.parameters, (_, x: unknown) => (typeof x === "bigint" ? Number(x) : x))}: every period's key and signature, verification, loading and evolution`, () => {
      const face = faceOf(v);
      const msg = h(v.message);
      expect(v.periods.length).toBe(face.totalPeriods);
      let key = expectOk(face.generate(h(v.seed)), "generate");
      expect(toHex(key.verifyingKey)).toBe(v.verifyingKey);
      v.periods.forEach((p, t) => {
        expect(Number(p.period)).toBe(t);
        expect(key.period).toBe(t);
        expect(toHex(key.bytes), `key at ${String(t)}`).toBe(p.key);
        expect(toHex(key.sign(msg)), `signature at ${String(t)}`).toBe(p.signature);
        expect(toHex(key.verifyingKey)).toBe(v.verifyingKey);
        for (let q = 0; q < face.totalPeriods; q++)
          expect(face.verify(h(v.verifyingKey), q, msg, h(p.signature)), `verify at ${String(q)}`).toEqual({ ok: true, value: q === t });
        expect(face.verify(h(v.verifyingKey), t, concat(msg, Uint8Array.of(0)), h(p.signature))).toEqual({ ok: true, value: false });
        const loaded = expectOk(face.load(h(p.key), t), "load");
        expect(toHex(loaded.sign(msg))).toBe(p.signature);
        if (t + 1 < face.totalPeriods) {
          const next = expectOk(key.evolve(), "evolve");
          expect(toHex(expectOk(loaded.evolve(), "evolve loaded").bytes)).toBe(toHex(next.bytes));
          // a key does not load at another period
          expect(face.load(h(p.key), t + 1).ok).toBe(false);
          key = next;
        } else {
          expect(kind(key.evolve())).toBe("Exhausted");
        }
      });
    });
  });
});

describe("Cardano Sum6 KES fixtures", { timeout: 300_000 }, () => {
  const dir = "kes/cardano/";
  const sum6 = CardanoSumKes.sum6;

  test("fixture files match their recorded SHA-256 sums", () => {
    const sums = tsv(`${dir}SHA256SUMS`).map((r) => (r[0] ?? "").split("  "));
    expect(sums.length).toBe(3);
    for (const [sum, name] of sums) expect(toHex(sha256(readDocBytes(dir + (name ?? ""))))).toBe(sum);
  });

  test("cardano-cli key-gen-KES: the depth-5 tree regenerated from r1 has root vk1, and vk = H(vk0 || vk1)", () => {
    const depth5 = expectOk(CardanoSumKes.of(5), "depth 5");
    const rows = tsv(`${dir}keygen.tsv`);
    expect(rows.length).toBe(3);
    for (const [, r1, vk1, vk0, vk] of rows) {
      expect(toHex(expectOk(depth5.generate(h(r1 ?? "")), "gen").verifyingKey)).toBe(vk1);
      expect(toHex(digest("BLAKE2b-256", concat(h(vk0 ?? ""), h(vk1 ?? ""))))).toBe(vk);
    }
  });

  test("17,336 verdicts on four preprod headers and their mutations", () => {
    const originals = new Map(tsv(`${dir}originals.tsv`).map((r) => [r[0] ?? "", r]));
    const flip = (b: Uint8Array, at: number, bit: number): Uint8Array => {
      const c = b.slice();
      c[at] = (c[at] ?? 0) ^ (1 << bit);
      return c;
    };
    const mutate = (r: string[], recipe: string): [Uint8Array, number, Uint8Array, Uint8Array] => {
      const [root, period, msg, sig] = [h(r[1] ?? ""), Number(r[2]), h(r[3] ?? ""), h(r[4] ?? "")];
      const parts = recipe.split("/");
      const [a, b, c, d] = parts;
      if (a === "period") return [root, Number(b), msg, sig];
      if (b === "xor") {
        const [o, bit] = [Number(c), Number(d)];
        if (a === "root") return [flip(root, o, bit), period, msg, sig];
        if (a === "signature") return [root, period, msg, flip(sig, o, bit)];
        if (a === "message") return [root, period, flip(msg, o, bit), sig];
      }
      if (a === "signature" && b === "swap-pair") {
        const off = 64 + 64 * (Number(c) - 1);
        return [root, period, msg, concat(sig.subarray(0, off), sig.subarray(off + 32, off + 64), sig.subarray(off, off + 32), sig.subarray(off + 64))];
      }
      if (recipe === "message/empty") return [root, period, new Uint8Array(0), sig];
      if (recipe === "message/truncate-last") return [root, period, msg.subarray(0, msg.length - 1), sig];
      if (recipe === "message/append-zero") return [root, period, concat(msg, Uint8Array.of(0)), sig];
      return [root, period, msg, sig];
    };
    const obs = tsv(`${dir}observations.tsv`);
    expect(obs.length).toBe(17336);
    expect(obs.filter((o) => o[2] === "VERIFIED").length).toBe(4);
    const mismatches = obs.flatMap((o) => {
      const original = originals.get(o[0] ?? "");
      if (original === undefined) return [`no original ${o[0] ?? ""}`];
      const [root, period, msg, sig] = mutate(original, o[1] ?? "");
      const r = sum6.verify(root, period, msg, sig);
      const verdict = r.ok && r.value ? "VERIFIED" : "REJECTED";
      return verdict === o[2] ? [] : [o.slice(0, 3).join(" ")];
    });
    expect(mismatches.slice(0, 3)).toEqual([]);
  });
});

describe("KES properties and typed errors", { timeout: 60_000 }, () => {
  test("Sum3: every period signs, verifies only there, and the last period cannot evolve", () => {
    const sum3 = expectOk(CardanoSumKes.of(3), "sum3");
    let k = expectOk(sum3.generate(new Uint8Array(32).fill(3)), "k0");
    const vk = k.verifyingKey;
    for (let t = 0; t < 8; t++) {
      const sig = k.sign(h("cafe"));
      expect(sig.length).toBe(sum3.signatureLength);
      for (let p = 0; p < 8; p++) expect(sum3.verify(vk, p, h("cafe"), sig)).toEqual({ ok: true, value: p === t });
      expect(toHex(k.verifyingKey)).toBe(toHex(vk));
      if (t < 7) k = expectOk(k.evolve(), "evolve");
    }
    expect(k.evolve()).toEqual({ ok: false, error: { kind: "Exhausted", period: 7 } });
  });

  test("Sum6: a raw key loads at its period and continues; layout sizes and erased seeds match cardano-base", () => {
    const sum6 = CardanoSumKes.sum6;
    const k0 = expectOk(sum6.generate(new Uint8Array(32).fill(1)), "k0");
    const k40 = expectOk(k0.evolveTo(40), "k40");
    const reloaded = expectOk(sum6.load(k40.bytes, 40), "reload");
    expect(k0.bytes.length).toBe(608);
    expect(sum6.signatureLength).toBe(448);
    expect(toHex(reloaded.sign(h("01")))).toBe(toHex(k40.sign(h("01"))));
    expect(toHex(expectOk(reloaded.evolve(), "e").bytes)).toBe(toHex(expectOk(k40.evolve(), "e").bytes));
    expect(toHex(k40.bytes.subarray(512, 544))).toBe("00".repeat(32));
    const text = [String(k0), JSON.stringify({ k0 }), inspect(k0)].join(" ");
    expect(text).not.toContain(toHex(k0.bytes).slice(0, 16));
    expect(text).toContain("redacted");
  });

  test("Sum6: wrong periods and a corrupted r1 are refused", () => {
    const sum6 = CardanoSumKes.sum6;
    const k0 = expectOk(sum6.generate(new Uint8Array(32).fill(5)), "k0");
    const k31 = expectOk(k0.evolveTo(31), "k31");
    const badR1 = k31.bytes;
    badR1[512] = (badR1[512] ?? 0) ^ 1;
    expect(kind(sum6.load(k0.bytes, 32))).toBe("InconsistentKey");
    expect(kind(sum6.load(k0.bytes, 1))).toBe("InconsistentKey");
    expect(kind(sum6.load(expectOk(k31.evolve(), "k32").bytes, 31))).toBe("InconsistentKey");
    const corrupted = expectOk(sum6.load(badR1, 31), "loads: r1 is checked on activation");
    expect(kind(corrupted.evolve())).toBe("InconsistentKey");
    for (let p = 0; p < 64; p += 9) expect(sum6.load(expectOk(k0.evolveTo(p), "to").bytes, p).ok).toBe(true);
    expect(kind(k31.evolveTo(30))).toBe("PeriodOutOfRange");
    expect(kind(k31.evolveTo(64))).toBe("PeriodOutOfRange");
  });

  test("malformed inputs and out-of-range periods are typed errors", () => {
    const sum6 = CardanoSumKes.sum6;
    const k = expectOk(sum6.generate(new Uint8Array(32).fill(2)), "k");
    const vk = k.verifyingKey;
    const sig = k.sign(new Uint8Array(0));
    expect(kind(sum6.verify(vk.subarray(1), 0, new Uint8Array(0), sig))).toBe("Malformed");
    expect(kind(sum6.verify(vk, 0, new Uint8Array(0), sig.subarray(1)))).toBe("Malformed");
    expect(sum6.verify(vk, 64, new Uint8Array(0), sig)).toEqual({ ok: false, error: { kind: "PeriodOutOfRange", period: 64, total: 64 } });
    expect(kind(sum6.generate(new Uint8Array(31)))).toBe("Malformed");
    expect(kind(sum6.load(k.bytes, 64))).toBe("PeriodOutOfRange");
    expect(kind(sum6.load(k.bytes.subarray(1), 0))).toBe("Malformed");
    expect(kind(CardanoSumKes.of(8))).toBe("Malformed");
    expect(kind(KesSum.of("SHA-256", 31))).toBe("Malformed");
    expect(kind(KesProduct.of("SHA-256", 16, 15))).toBe("Malformed");
  });

  test("a loaded key owns its bytes: clearing the caller's Buffer afterwards changes nothing", () => {
    // Buffer#slice is a view, so a loader that keeps slices of its input would share the caller's memory
    const sum = expectOk(KesSum.of("BLAKE2b-256", 1), "sum");
    const prod = expectOk(KesProduct.of("BLAKE2b-256", 1, 1), "prod");
    const cardano = expectOk(CardanoSumKes.of(2), "cardano");
    const seed = new Uint8Array(32).fill(9);
    const cases: readonly [string, Uint8Array, (raw: Uint8Array) => { bytes: Uint8Array; sign: () => Uint8Array }, (sig: Uint8Array) => boolean][] = [
      [
        "sum",
        expectOk(sum.generate(seed), "sum k").bytes,
        (raw) => {
          const k = expectOk(sum.load(raw, 0), "load");
          return { bytes: k.bytes, sign: () => expectOk(k.sign(beef), "sign") };
        },
        (sig) => expectOk(sum.verify(expectOk(sum.generate(seed), "vk").verifyingKey, 0, beef, sig), "verify"),
      ],
      [
        "product",
        expectOk(prod.generate(seed), "prod k").bytes,
        (raw) => {
          const k = expectOk(prod.load(raw, 0), "load");
          return { bytes: k.bytes, sign: () => expectOk(k.sign(beef), "sign") };
        },
        (sig) => expectOk(prod.verify(expectOk(prod.generate(seed), "vk").verifyingKey, 0, beef, sig), "verify"),
      ],
      [
        "cardano",
        expectOk(cardano.generate(seed), "cardano k").bytes,
        (raw) => {
          const k = expectOk(cardano.load(raw, 0), "load");
          return { bytes: k.bytes, sign: () => k.sign(beef) };
        },
        (sig) => expectOk(cardano.verify(expectOk(cardano.generate(seed), "vk").verifyingKey, 0, beef, sig), "verify"),
      ],
    ];
    for (const [name, encoded, load, verifies] of cases) {
      const buffer = Buffer.from(encoded);
      const key = load(buffer);
      buffer.fill(0);
      expect(toHex(key.bytes), name).toBe(toHex(encoded));
      expect(verifies(key.sign()), name).toBe(true);
    }
  });

  test("KesSum and KesProduct: every period round-trips; the super leaf is erased; corrupted keys are refused", () => {
    const sum = expectOk(KesSum.of("SHA-512", 3), "sum");
    let k = expectOk(sum.generate(new Uint8Array(32).fill(8)), "k0");
    expect(k.verifyingKey.length).toBe(64);
    for (let t = 0; t < 8; t++) {
      const sig = expectOk(k.sign(beef), "sign");
      expect(k.bytes.length).toBe(sum.keyLength);
      for (let p = 0; p < 8; p++) expect(sum.verify(k.verifyingKey, p, beef, sig)).toEqual({ ok: true, value: p === t });
      expect(toHex(expectOk(sum.load(k.bytes, t), "load").bytes)).toBe(toHex(k.bytes));
      if (t < 7) k = expectOk(k.evolve(), "evolve");
    }
    expect(kind(k.evolve())).toBe("Exhausted");
    expect(kind(sum.evolve(k))).toBe("Exhausted");
    expect(toHex(sum.verifyingKey(k))).toBe(toHex(k.verifyingKey));
    expect(toHex(expectOk(sum.sign(k, beef), "scheme sign"))).toBe(toHex(expectOk(k.sign(beef), "key sign")));

    const prod = expectOk(KesProduct.of("BLAKE2b-256", 2, 2), "prod");
    const p0 = expectOk(prod.generate(new Uint8Array(32).fill(4)), "p0");
    const p5 = expectOk(p0.evolveTo(5), "p5");
    expect(toHex(p5.verifyingKey)).toBe(toHex(p0.verifyingKey));
    expect(prod.verify(p0.verifyingKey, 5, beef, expectOk(p5.sign(beef), "sign"))).toEqual({ ok: true, value: true });
    expect(prod.verify(p0.verifyingKey, 4, beef, expectOk(p5.sign(beef), "sign"))).toEqual({ ok: true, value: false });
    // the super tree's active leaf is erased: its seed bytes are zero in the encoding
    expect(toHex(p0.bytes.subarray(0, 32))).toBe("00".repeat(32));
    const forged = p5.bytes;
    forged[forged.length - 1] = (forged[forged.length - 1] ?? 0) ^ 1;
    expect(kind(prod.load(forged, 5))).toBe("InconsistentKey");
    expect(kind(p5.evolveTo(3))).toBe("PeriodOutOfRange");
    expect(kind(p5.evolveTo(16))).toBe("PeriodOutOfRange");

    const k0 = expectOk(sum.generate(new Uint8Array(32).fill(8)), "k0");
    const badLeaf = k0.bytes;
    badLeaf[40] = (badLeaf[40] ?? 0) ^ 1;
    expect(kind(sum.load(badLeaf, 0))).toBe("InconsistentKey");
    expect(kind(sum.load(k0.bytes, 2))).toBe("InconsistentKey");
  });
});
