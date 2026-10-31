import { describe, expect, test } from "vitest";

import { models, toHex } from "@kd5ujc/dilf4s-core";
import { expand_message_xmd, hash_to_field } from "@noble/curves/abstract/hash-to-curve.js";
import { sha512, sha256 } from "@noble/hashes/sha2.js";

import { ecvrfDraft03, ecvrfEll2, ecvrfTai, type Vrf, type VrfError, type VrfSuiteName } from "../src/index.js";
import { draft03Challenge, draft03Equation, encodeToCurveEll2, fromUniformDraft03, hashToCurveDraft03 } from "../src/vrf.js";
import { bigToLE, concat, leToBig } from "../src/internal/bytes.js";
import { decodeDraft03, isSmallOrder, L, P, Point } from "../src/internal/edwards.js";
import { expectOk, h, loadVectors, readDoc, readDocBytes, tsv, utf8 } from "./vectors.js";

const vectors = loadVectors("vrf_v1.json", models.vrfVectors.VrfVectors);
const suites: Record<string, Vrf<VrfSuiteName>> = {
  "ECVRF-EDWARDS25519-SHA512-TAI": ecvrfTai as Vrf<VrfSuiteName>,
  "ECVRF-EDWARDS25519-SHA512-ELL2": ecvrfEll2 as Vrf<VrfSuiteName>,
  "ECVRF-ED25519-SHA512-Elligator2 (draft-03)": ecvrfDraft03 as Vrf<VrfSuiteName>,
};
const suite = (name: string): Vrf<VrfSuiteName> => {
  const s = suites[name];
  if (s === undefined) throw new Error(`unknown suite ${name}`);
  return s;
};
const kindOf = (r: { ok: true } | { ok: false; error: VrfError }): string => (r.ok ? "valid" : r.error.kind);

const big = (s: string | undefined): bigint => BigInt(`0x${(s ?? "").replace(/^0x/, "")}`);

/** Affine (x, y) as a compressed encoding. */
const affine = (x: bigint, y: bigint): string => {
  const b = bigToLE(y, 32);
  b[31] = (b[31] ?? 0) | (x & 1n ? 0x80 : 0);
  return toHex(b);
};

describe("vrf_v1.json", { timeout: 60_000 }, () => {
  test("covers the three suites", () => {
    expect(vectors.suites.map((s) => s.suite)).toEqual(Object.keys(suites));
  });

  for (const s of vectors.suites) {
    test(`${s.suite}: keys from seeds, proofs and outputs byte for byte, and verification`, () => {
      const v = suite(s.suite);
      expect(s.keys.length).toBe(3);
      for (const key of s.keys) {
        const kp = expectOk(v.fromSeed(h(key.seed)), "seed");
        expect(toHex(kp.signing.bytes)).toBe(key.signingKey);
        expect(toHex(kp.verifying.bytes)).toBe(key.verifyingKey);
        const sk = expectOk(v.signingKey(h(key.signingKey)), "sk");
        expect(key.proofs.length).toBe(3);
        for (const p of key.proofs) {
          const proved = expectOk(v.prove(sk, h(p.alpha)), "prove");
          expect(toHex(proved.proof.bytes)).toBe(p.pi);
          expect(toHex(proved.output.bytes)).toBe(p.beta);
          expect(toHex(expectOk(v.verifyBytes(h(key.verifyingKey), h(p.alpha), h(p.pi)), "verify").bytes)).toBe(p.beta);
        }
      }
    });
  }

  test("rejections: every verdict and error kind", () => {
    expect(vectors.rejections.length).toBeGreaterThan(30);
    const mismatches = vectors.rejections.flatMap((r) => {
      const actual = kindOf(suite(r.suite).verifyBytes(h(r.verifyingKey), h(r.alpha), h(r.pi)));
      return actual === r.result ? [] : [`${r.suite}/${r.name}: expected ${r.result}, got ${actual}`];
    });
    expect(mismatches).toEqual([]);
  });
});

describe("RFC 9380 J.5.2, edwards25519_XMD:SHA-512_ELL2_NU_", { timeout: 60_000 }, () => {
  interface Section {
    header: { dst?: string; DST?: string };
    vectors: Record<string, string>[];
  }
  const doc = JSON.parse(readDoc("vrf/rfc9380.json")) as Record<string, Section>;

  test("expand_message_xmd (SHA-512) agrees with noble", () => {
    const sec = doc["expand_message_xmd_sha512"];
    expect(sec?.vectors.length).toBeGreaterThan(0);
    for (const v of sec?.vectors ?? []) {
      const out = expand_message_xmd(utf8(v["msg"] ?? ""), utf8(sec?.header.DST ?? ""), Number(v["len_in_bytes"]), sha512);
      expect(toHex(out)).toBe(v["uniform_bytes"]);
    }
  });

  test("hash_to_field and encode_to_curve (5 vectors)", () => {
    const sec = doc["edwards25519_XMD_SHA512_ELL2_NU"];
    const dst = utf8(sec?.header.dst ?? "");
    expect(sec?.vectors.length).toBe(5);
    for (const v of sec?.vectors ?? []) {
      const msg = utf8(v["msg"] ?? "");
      const [u] = hash_to_field(msg, 1, { DST: dst, p: P, m: 1, k: 128, expand: "xmd", hash: sha512 });
      expect(u?.[0]).toBe(big(v["u[0]"]));
      const pt = encodeToCurveEll2(msg, dst);
      expect(pt === undefined ? "" : toHex(pt.toBytes())).toBe(affine(big(v["P.x"]), big(v["P.y"])));
    }
  });
});

describe("RFC 9381 and draft-03 examples", { timeout: 60_000 }, () => {
  type Row = Record<string, string>;
  const rfc = JSON.parse(readDoc("vrf/rfc9381.json")) as Record<string, Row[]>;
  const draft = JSON.parse(readDoc("vrf/draft03.json")) as Record<string, Row[]>;
  const get = (r: Row, k: string): string => r[k] ?? "";

  const examples = (v: Vrf<VrfSuiteName>, rows: Row[], extra: (r: Row) => void): void => {
    expect(rows.length).toBe(3);
    for (const r of rows) {
      const sk = expectOk(v.signingKey(v === (ecvrfDraft03 as Vrf<VrfSuiteName>) ? h(get(r, "SK") + get(r, "PK")) : h(get(r, "SK"))), "sk");
      const alpha = h(get(r, "alpha"));
      expect(toHex(v.publicKey(sk).bytes)).toBe(get(r, "PK"));
      const proved = expectOk(v.prove(sk, alpha), "prove");
      expect(toHex(proved.proof.bytes)).toBe(get(r, "pi"));
      expect(toHex(proved.output.bytes)).toBe(get(r, "beta"));
      const vk = expectOk(v.verifyingKey(h(get(r, "PK"))), "vk");
      const pi = expectOk(v.proof(h(get(r, "pi"))), "pi");
      expect(toHex(expectOk(v.verify(vk, alpha, pi), "verify").bytes)).toBe(get(r, "beta"));
      expect(kindOf(v.verify(vk, concat(alpha, Uint8Array.of(0)), pi))).toBe("ChallengeMismatch");
      const tampered = pi.bytes;
      tampered[40] = (tampered[40] ?? 0) ^ 1;
      expect(kindOf(v.verifyBytes(vk.bytes, alpha, tampered))).not.toBe("valid");
      extra(r);
    }
  };

  test("RFC 9381 B.3: TAI examples 16-18 (with intermediate H)", () => {
    examples(ecvrfTai as Vrf<VrfSuiteName>, rfc["tai"] ?? [], () => undefined);
  });

  test("RFC 9381 B.4: ELL2 examples 19-21 (with intermediate u and H)", () => {
    const dst = concat(utf8("ECVRF_edwards25519_XMD:SHA-512_ELL2_NU_"), Uint8Array.of(4));
    examples(ecvrfEll2 as Vrf<VrfSuiteName>, rfc["ell2"] ?? [], (r) => {
      const msg = concat(h(get(r, "PK")), h(get(r, "alpha")));
      const [u] = hash_to_field(msg, 1, { DST: dst, p: P, m: 1, k: 128, expand: "xmd", hash: sha512 });
      expect(u?.[0]).toBe(leToBig(h(get(r, "u"))));
      expect(toHex(encodeToCurveEll2(msg, dst)?.toBytes() ?? new Uint8Array(0))).toBe(get(r, "H"));
    });
  });

  test("draft-irtf-cfrg-vrf-03 A.4: Elligator2 examples (with intermediate H)", () => {
    examples(ecvrfDraft03 as Vrf<VrfSuiteName>, draft["examples"] ?? [], (r) => {
      expect(toHex(hashToCurveDraft03(h(get(r, "PK")), h(get(r, "alpha")))?.toBytes() ?? new Uint8Array(0))).toBe(get(r, "H"));
    });
  });
});

describe("draft-03 libsodium oracle corpus (normative)", { timeout: 300_000 }, () => {
  const dir = "vrf/draft03-corpus/";

  test("corpus files match their recorded SHA-256 sums", () => {
    const sums = tsv(`${dir}SHA256SUMS`).map((r) => (r[0] ?? "").split("  "));
    expect(sums.length).toBe(6);
    for (const [sum, name] of sums) expect(toHex(sha256(readDocBytes(dir + (name ?? ""))))).toBe(sum);
  });

  const outcome = (row: string[]): string => {
    if (row.includes("NULL")) return "MALFORMED";
    const r = ecvrfDraft03.verifyBytes(h(row[1] ?? ""), h(row[3] ?? ""), h(row[2] ?? ""));
    if (r.ok) return `VALID:${toHex(r.value.bytes)}`;
    return r.error.kind === "Malformed" ? "MALFORMED" : "INVALID";
  };

  const replay = (name: string, rowsFile: string, expectedFile: string, count: number, valid: number, malformed: number): void => {
    test(`${name}: ${String(count)} oracle verdicts`, () => {
      const rows = tsv(dir + rowsFile);
      const expected = tsv(dir + expectedFile).map((r) => r.join("\t"));
      expect(rows.length).toBe(count);
      expect(expected.length).toBe(count);
      expect(expected.filter((e) => e.includes("\tVALID:")).length).toBe(valid);
      expect(expected.filter((e) => e.endsWith("\tMALFORMED")).length).toBe(malformed);
      const mismatches = rows.flatMap((r, i) => {
        const actual = `${r[0] ?? ""}\t${outcome(r)}`;
        return actual === expected[i] ? [] : [`${actual} != ${expected[i] ?? ""}`];
      });
      expect(mismatches.slice(0, 3)).toEqual([]);
    });
  };

  replay("original corpus", "vectors.tsv", "expected.tsv", 2048, 3, 27);
  replay("additional corpus", "additional.tsv", "additional-results.tsv", 2338, 3, 0);

  const b = (p: { toBytes(): Uint8Array }): string => toHex(p.toBytes());

  /** The four helper kinds, evaluated with the production functions (the oracle helper's output formats). */
  const helper = (row: string[]): string => {
    const input = h(row[2] ?? "");
    switch (row[1] ?? "") {
      case "d": {
        const p = decodeDraft03(input);
        return p === undefined ? "INVALID" : `${isSmallOrder(p) ? "SMALL:" : "POINT:"}${b(p)}`;
      }
      case "u": {
        const p = fromUniformDraft03(input);
        return p === undefined ? "INVALID" : b(p);
      }
      case "e": {
        const p = decodeDraft03(input);
        const q = decodeDraft03(h(row[3] ?? ""));
        if (p === undefined || q === undefined) return "INVALID";
        return b(draft03Equation(p, q, leToBig(h(row[4] ?? "")), leToBig(h(row[5] ?? "")) % L));
      }
      default: {
        const pi = h(row[3] ?? "");
        const alpha = h(row[4] ?? "");
        const [gb, cb, sb] = [pi.subarray(0, 32), pi.subarray(32, 48), pi.subarray(48)];
        const y = decodeDraft03(input);
        const gamma = decodeDraft03(gb);
        const s = leToBig(sb);
        if (y === undefined || isSmallOrder(y) || gamma === undefined || s >= L) return "INVALID";
        const hp = hashToCurveDraft03(y.toBytes(), alpha);
        if (hp === undefined) return "INVALID";
        const c = leToBig(cb);
        const u = draft03Equation(y, Point.BASE, c, s);
        const v = draft03Equation(gamma, hp, c, s);
        const beta = sha512(concat(Uint8Array.of(4, 3), gamma.clearCofactor().toBytes()));
        return b(hp) + b(u) + b(v) + toHex(draft03Challenge(input, hp.toBytes(), gb, u.toBytes(), v.toBytes())) + toHex(beta);
      }
    }
  };

  for (const [op, count] of [
    ["d", 612],
    ["u", 519],
    ["e", 240],
    ["t", 2055],
  ] as const) {
    test(`helper '${op}': ${String(count)} intermediate-step comparisons`, () => {
      const all = tsv(`${dir}helpers.tsv`);
      const expected = tsv(`${dir}helper-results.tsv`).map((r) => r.join("\t"));
      expect(all.length).toBe(3426);
      const rows = all.flatMap((r, i) => (r[1] === op ? [[r, expected[i] ?? ""] as const] : []));
      expect(rows.length).toBe(count);
      const mismatches = rows.flatMap(([r, e]) => (`${r[0] ?? ""}\t${helper(r)}` === e ? [] : [`${r[0] ?? ""}: ${helper(r)} != ${e}`]));
      expect(mismatches.slice(0, 2)).toEqual([]);
    });
  }
});

describe("VRF properties and typed errors", { timeout: 60_000 }, () => {
  test("prove/verify round-trips for every suite", () => {
    for (const v of Object.values(suites)) {
      for (let i = 0; i < 4; i++) {
        const kp = expectOk(v.fromSeed(sha256(Uint8Array.of(i, 7))), "seed");
        const alpha = sha512(Uint8Array.of(i)).subarray(0, i * 9);
        const proved = expectOk(v.prove(kp.signing, alpha), "prove");
        expect(proved.proof.bytes.length).toBe(80);
        expect(proved.output.bytes.length).toBe(64);
        expect(toHex(expectOk(v.verify(kp.verifying, alpha, proved.proof), "verify").bytes)).toBe(toHex(proved.output.bytes));
      }
    }
  });

  test("suites sharing suite byte 0x04 never accept each other's proofs", () => {
    const kp = expectOk(ecvrfEll2.fromSeed(new Uint8Array(32).fill(5)), "kp");
    const { proof } = expectOk(ecvrfEll2.prove(kp.signing, h("01")), "prove");
    expect(ecvrfDraft03.verifyBytes(kp.verifying.bytes, h("01"), proof.bytes).ok).toBe(false);
  });

  test("malformed envelopes, small-order keys, non-canonical encodings and mismatched keys are typed errors", () => {
    const kp = expectOk(ecvrfTai.fromSeed(new Uint8Array(32).fill(9)), "kp");
    const pi = expectOk(ecvrfTai.prove(kp.signing, new Uint8Array(0)), "prove").proof.bytes;
    const vk = kp.verifying.bytes;
    const identity = h("0100000000000000000000000000000000000000000000000000000000000000");
    const negZero = h("0100000000000000000000000000000000000000000000000000000000000080");
    const nonCanonical = h("edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f");
    const empty = new Uint8Array(0);
    expect(kindOf(ecvrfTai.verifyBytes(vk.subarray(1), empty, pi))).toBe("Malformed");
    expect(kindOf(ecvrfTai.verifyBytes(vk, empty, pi.subarray(1)))).toBe("Malformed");
    // the proof's length is checked before the key's
    const both = ecvrfTai.verifyBytes(vk.subarray(1), empty, pi.subarray(1));
    expect(both.ok ? "" : both.error.kind === "Malformed" ? both.error.what : "").toContain("proof");
    expect(kindOf(ecvrfTai.verifyBytes(identity, empty, pi))).toBe("SmallOrderPublicKey");
    expect(kindOf(ecvrfTai.verifyBytes(negZero, empty, pi))).toBe("InvalidPublicKey");
    expect(kindOf(ecvrfTai.verifyBytes(nonCanonical, empty, pi))).toBe("InvalidPublicKey");
    expect(kindOf(ecvrfTai.verifyBytes(vk, empty, concat(pi.subarray(0, 48), bigToLE(L, 32))))).toBe("NonCanonicalScalar");
    const badGamma = pi.slice();
    badGamma.set(h("0200000000000000000000000000000000000000000000000000000000000000"), 0);
    expect(kindOf(ecvrfTai.verifyBytes(vk, empty, badGamma))).toBe("InvalidGamma");
    expect(kindOf(ecvrfDraft03.signingKey(concat(new Uint8Array(32).fill(1), new Uint8Array(32).fill(2))))).toBe("KeyMismatch");
    expect(kindOf(ecvrfDraft03.signingKey(new Uint8Array(32)))).toBe("Malformed");
    expect(kindOf(ecvrfTai.signingKey(new Uint8Array(64)))).toBe("Malformed");
    // draft-03 decodes -0 to the identity, which is of small order
    expect(kindOf(ecvrfDraft03.verifyingKey(negZero))).toBe("SmallOrderPublicKey");
    expect(kindOf(ecvrfDraft03.verifyingKey(nonCanonical))).toBe("InvalidPublicKey");
    expect(kindOf(ecvrfTai.fromSeed(new Uint8Array(31)))).toBe("Malformed");
    const redacted = [String(kp.signing), JSON.stringify(kp)].join(" ");
    expect(redacted).not.toContain("0909");
  });
});
