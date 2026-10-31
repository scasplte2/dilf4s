import { describe, expect, test } from "vitest";

import { toHex } from "@kd5ujc/dilf4s-core";
import { bls12_381 } from "@noble/curves/bls12-381.js";

import { blsPop, type Signature, type VerifyingKey } from "../src/index.js";
import { R } from "../src/bls.js";
import { bigToBE } from "../src/internal/bytes.js";
import { expectOk, h, readDoc, utf8 } from "./vectors.js";

interface Case {
  readonly name: string;
  readonly input: unknown;
  readonly output: unknown;
}
const corpus = JSON.parse(readDoc("bls/eth2-bls12-381-tests-v0.1.2.json")) as Record<string, Case[]>;
const cases = (category: string, count: number): Case[] => {
  const cs = corpus[category] ?? [];
  expect(cs.length, category).toBe(count);
  return cs;
};

// loose decoders for the corpus: an input that does not decode stands for a value that never verifies
const vkOf = (hex: string): VerifyingKey<"BLS12-381-PoP"> | undefined => {
  const r = blsPop.verifyingKey(h(hex));
  return r.ok ? r.value : undefined;
};
const sigOf = (hex: string): Signature<"BLS12-381-PoP"> | undefined => {
  const r = blsPop.signature(h(hex));
  return r.ok ? r.value : undefined;
};

describe("ethereum/bls12-381-tests v0.1.2", { timeout: 120_000 }, () => {
  test("sign (10)", () => {
    for (const c of cases("sign", 10)) {
      const input = c.input as { privkey: string; message: string };
      const sk = blsPop.signingKey(h(input.privkey));
      const actual = sk.ok ? `0x${toHex(blsPop.sign(sk.value, h(input.message)).bytes)}` : null;
      expect(actual, c.name).toBe(c.output);
    }
  });

  test("verify (29)", () => {
    for (const c of cases("verify", 29)) {
      const input = c.input as { pubkey: string; message: string; signature: string };
      const r = blsPop.verifyBytes(h(input.pubkey), h(input.message), h(input.signature));
      expect(r.ok && r.value, c.name).toBe(c.output);
    }
  });

  test("aggregate (6)", () => {
    for (const c of cases("aggregate", 6)) {
      const sigs = (c.input as string[]).map((s) => sigOf(s));
      const defined = sigs.filter((s) => s !== undefined);
      const r = defined.length === sigs.length ? blsPop.aggregate(defined) : undefined;
      expect(r?.ok === true ? `0x${toHex(r.value.bytes)}` : null, c.name).toBe(c.output);
    }
  });

  test("fast_aggregate_verify (12)", () => {
    for (const c of cases("fast_aggregate_verify", 12)) {
      const input = c.input as { pubkeys: string[]; message: string; signature: string };
      const keys = input.pubkeys.map(vkOf);
      const sig = sigOf(input.signature);
      const ok = sig !== undefined && keys.every((k) => k !== undefined) && blsPop.fastAggregateVerify(keys, h(input.message), sig);
      expect(ok, c.name).toBe(c.output);
    }
  });

  test("aggregate_verify (5)", () => {
    for (const c of cases("aggregate_verify", 5)) {
      const input = c.input as { pubkeys: string[]; messages: string[]; signature: string };
      const keys = input.pubkeys.map(vkOf);
      const sig = sigOf(input.signature);
      const pairs = keys.flatMap((k, i) => (k === undefined ? [] : [[k, h(input.messages[i] ?? "")] as const]));
      const ok = sig !== undefined && pairs.length === keys.length && blsPop.aggregateVerify(pairs, sig);
      expect(ok, c.name).toBe(c.output);
    }
  });

  test("batch_verify (4)", () => {
    for (const c of cases("batch_verify", 4)) {
      const input = c.input as { pubkeys: string[]; messages: string[]; signatures: string[] };
      const triples = input.pubkeys.flatMap((k, i) => {
        const vk = vkOf(k);
        const sig = sigOf(input.signatures[i] ?? "");
        return vk === undefined || sig === undefined ? [] : [[vk, h(input.messages[i] ?? ""), sig] as const];
      });
      const ok = triples.length === input.pubkeys.length && blsPop.batchVerify(triples);
      expect(ok, c.name).toBe(c.output);
    }
  });

  test("deserialization_G1 (13) through KeyValidate, deserialization_G2 (15)", () => {
    for (const c of cases("deserialization_G1", 13)) {
      const r = blsPop.verifyingKey(h((c.input as { pubkey: string }).pubkey));
      // the identity deserialises, but KeyValidate rejects it as a public key
      const expected = c.name === "deserialization_succeeds_infinity_with_true_b_flag" ? false : c.output;
      expect(r.ok, c.name).toBe(expected);
      if (!r.ok) expect(["InvalidKey", "InvalidEncoding"]).toContain(r.error.kind);
    }
    for (const c of cases("deserialization_G2", 15)) {
      const r = blsPop.signature(h((c.input as { signature: string }).signature));
      expect(r.ok, c.name).toBe(c.output);
      if (!r.ok) expect(r.error.kind, c.name).toBe("InvalidEncoding");
    }
  });

  test("hash_to_G2 (4), RFC 9380 BLS12381G2_XMD:SHA-256_SSWU_RO_", () => {
    const dst = utf8("QUUX-V01-CS02-with-BLS12381G2_XMD:SHA-256_SSWU_RO_");
    for (const c of cases("hash_to_G2", 4)) {
      const out = c.output as { x: string; y: string };
      // the vectors write Fp2 as "c0,c1"; the uncompressed encoding puts c1 first
      const coord = (s: string) => s.split(",").map((p) => p.slice(2)).reverse().join("");
      const r = expectOk(blsPop.hashToG2(utf8((c.input as { msg: string }).msg), dst), c.name);
      expect(toHex(r), c.name).toBe(coord(out.x) + coord(out.y));
    }
  });
});

describe("BLS12-381-PoP", { timeout: 60_000 }, () => {
  test("tessellation-bls known answer, proof of possession, and typed key errors", () => {
    const sk = expectOk(blsPop.signingKey(h("328388aff0d4a5b7dc9205abd374e7e98f3cd9f3418edb4eafda5fb16473d216")), "sk");
    const vk = blsPop.publicKey(sk);
    expect(toHex(vk.bytes)).toBe("b53d21a4cfd562c469cc81514d4ce5a6b577d8403d32a394dc265dd190b47fa9f829fdd7963afdf972e5e77854051f6f");
    expect(toHex(blsPop.sign(sk, new Uint8Array(32).fill(0xab)).bytes)).toBe(
      "ae82747ddeefe4fd64cf9cedb9b04ae3e8a43420cd255e3c7cd06a8d88b7c7f8638543719981c5d16fa3527c468c25f0026704a6951bde891360c7e8d12ddee0559004ccdbe6046b55bae1b257ee97f7cdb955773d7cf29adf3ccbb9975e4eb9",
    );
    const pop = blsPop.popProve(sk);
    const other = blsPop.publicKey(expectOk(blsPop.signingKey(bigToBE(7n, 32)), "sk7"));
    expect(blsPop.popVerify(vk, pop)).toBe(true);
    expect(blsPop.popVerify(other, pop), "a PoP is bound to its key").toBe(false);
    expect(blsPop.verify(vk, vk.bytes, pop), "a PoP is not a message signature").toBe(false);
    const zero = blsPop.signingKey(new Uint8Array(32));
    expect(zero.ok ? "ok" : zero.error.kind).toBe("InvalidKey");
    const atR = blsPop.signingKey(bigToBE(R, 32));
    expect(atR.ok ? "ok" : atR.error.kind).toBe("InvalidKey");
    const short = blsPop.signingKey(new Uint8Array(31));
    expect(short.ok ? "ok" : short.error.kind).toBe("InvalidEncoding");
    expect(blsPop.hashToG2(new Uint8Array(1), new Uint8Array(0)).ok, "empty DST").toBe(false);
  });

  test("the identity signature decodes but never verifies; the identity key is InvalidKey", () => {
    const identitySig = h(`c0${"00".repeat(95)}`);
    const sig = expectOk(blsPop.signature(identitySig), "identity decodes");
    const sk = expectOk(blsPop.signingKey(bigToBE(5n, 32)), "sk");
    const vk = blsPop.publicKey(sk);
    expect(blsPop.verify(vk, new Uint8Array(0), sig)).toBe(false);
    expect(blsPop.fastAggregateVerify([vk], new Uint8Array(0), sig)).toBe(false);
    expect(blsPop.aggregateVerify([[vk, new Uint8Array(0)]], sig)).toBe(false);
    const idKey = blsPop.verifyingKey(h(`c0${"00".repeat(47)}`));
    expect(idKey.ok ? "ok" : idKey.error.kind).toBe("InvalidKey");
    // s + (-s) aggregates to the identity, encoded with the infinity flag
    const s = blsPop.sign(sk, utf8("m"));
    const neg = expectOk(blsPop.signature(bls12_381.longSignatures.Signature.toBytes(bls12_381.longSignatures.Signature.fromBytes(s.bytes).negate())), "neg");
    expect(toHex(expectOk(blsPop.aggregate([s, neg]), "aggregate").bytes)).toBe(toHex(identitySig));
    expect(blsPop.aggregate([]).ok).toBe(false);
    expect(blsPop.fastAggregateVerify([], utf8("m"), s)).toBe(false);
    expect(blsPop.aggregateVerify([], s)).toBe(false);
    expect(blsPop.batchVerify([])).toBe(false);
  });

  test("EIP-2333 test case 0 (master and child keys)", () => {
    const seed = h(
      "c55257c360c07c72029aebc1b53c05ed0362ada38ead3e3e9efa3708e53495531f09a6987599d18264c1e1c92f2cf141630c7a3c4ab7c81b2f001698e7463b04",
    );
    const master = expectOk(blsPop.deriveMasterSk(seed), "master");
    expect(toHex(master.bytes)).toBe(toHex(bigToBE(6083874454709270928345386274498605044986640685124978867557563392430687146096n, 32)));
    const child = expectOk(blsPop.deriveChildSk(master, 0), "child");
    expect(toHex(child.bytes)).toBe(toHex(bigToBE(20397789859736650942317412262472558107875392172444076792671091975210932703118n, 32)));
    expect(blsPop.deriveMasterSk(new Uint8Array(31)).ok).toBe(false);
    expect(blsPop.deriveChildSk(master, 2 ** 32).ok).toBe(false);
  });
});
