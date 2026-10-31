import { inspect } from "node:util";

import { ed25519 as nobleEd } from "@noble/curves/ed25519.js";
import { sha512 } from "@noble/hashes/sha2.js";
import { concatBytes } from "@noble/hashes/utils.js";
import { describe, expect, test } from "vitest";

import { toHex } from "@kd5ujc/dilf4s-core";

import { ed25519, ed25519Bip32 } from "../src/index.js";
import { signExtended } from "../src/ed25519.js";
import { L } from "../src/internal/edwards.js";
import { bigToLE, leToBig } from "../src/internal/bytes.js";
import { expectOk, h, readDoc, utf8 } from "./vectors.js";

interface BramblVector {
  readonly inputs: { readonly secretKey: string; readonly message: string };
  readonly outputs: { readonly verificationKey: string; readonly signature: string };
}

const seed42 = new Uint8Array(32).fill(0x42);
const message = utf8("zip215");

describe("Ed25519", { timeout: 60_000 }, () => {
  test("RFC 8032 known answers (BramblSc Ed25519.json)", () => {
    const vectors = JSON.parse(readDoc("Ed25519.json")) as BramblVector[];
    expect(vectors.length).toBeGreaterThan(0);
    for (const v of vectors) {
      const kp = expectOk(ed25519.fromSeed(h(v.inputs.secretKey)), "seed");
      expect(toHex(kp.verifying.bytes)).toBe(v.outputs.verificationKey);
      const sig = ed25519.sign(kp.signing, h(v.inputs.message));
      expect(toHex(sig.bytes)).toBe(v.outputs.signature);
      expect(ed25519.verify(kp.verifying, h(v.inputs.message), sig)).toBe(true);
      expect(ed25519.verifyLibsodium(kp.verifying, h(v.inputs.message), sig)).toBe(true);
    }
  });

  const kp = expectOk(ed25519.fromSeed(seed42), "seed");
  const vk = kp.verifying.bytes;

  test("an off-curve key is InvalidKey, a short one InvalidEncoding, a non-canonical one decodes", () => {
    const offCurve = ed25519.verifyingKey(h("0200000000000000000000000000000000000000000000000000000000000000"));
    expect(offCurve.ok ? "ok" : offCurve.error.kind).toBe("InvalidKey");
    const short = ed25519.verifyingKey(new Uint8Array(31));
    expect(short.ok ? "ok" : short.error.kind).toBe("InvalidEncoding");
    // y = p + 1: a non-canonical encoding of the identity
    expect(ed25519.verifyingKey(h("eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f")).ok).toBe(true);
    const sig = ed25519.signature(new Uint8Array(63));
    expect(sig.ok ? "ok" : sig.error.kind).toBe("InvalidEncoding");
  });

  test("ZIP-215 accepts a non-canonical R; libsodium rejects it", () => {
    // r = 0: R is the identity, encoded non-canonically as y = p + 1; S = k a
    const rBytes = h("eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f");
    const s = signNonce(rBytes, 0n);
    expect(expectOk(ed25519.verifyBytes(vk, message, s), "decode")).toBe(true);
    expect(expectOk(ed25519.verifyLibsodiumBytes(vk, message, s), "decode")).toBe(false);
  });

  test("ZIP-215 accepts a mixed-order R (valid only cofactored); libsodium rejects it", () => {
    const torsion = ed25519Point(h("26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05"));
    expect(torsion.isSmallOrder() && !torsion.is0()).toBe(true);
    const r = 12345678901234567890n;
    const rPrime = Point.BASE.multiply(r).add(torsion).toBytes();
    const s = signNonce(rPrime, r);
    expect(ed25519.verify(kp.verifying, message, expectOk(ed25519.signature(s), "sig"))).toBe(true);
    expect(expectOk(ed25519.verifyLibsodiumBytes(vk, message, s), "decode")).toBe(false);
  });

  test("S + L is rejected by both rules; S = L is rejected", () => {
    const sig = ed25519.sign(kp.signing, message).bytes;
    const bumped = sig.slice();
    bumped.set(bigToLE(leToBig(sig.subarray(32)) + L, 32), 32);
    expect(expectOk(ed25519.verifyBytes(vk, message, bumped), "decode")).toBe(false);
    expect(expectOk(ed25519.verifyLibsodiumBytes(vk, message, bumped), "decode")).toBe(false);
    const sIsL = sig.slice();
    sIsL.set(bigToLE(L, 32), 32);
    expect(expectOk(ed25519.verifyBytes(vk, message, sIsL), "decode")).toBe(false);
  });

  test("a small-order key verifies under ZIP-215 when the equation holds, never under libsodium", () => {
    // A = identity, R = identity, S = 0: [8](0 - R - kA) = 0
    const identity = h("0100000000000000000000000000000000000000000000000000000000000000");
    const sig = new Uint8Array(64);
    sig.set(identity, 0);
    expect(expectOk(ed25519.verifyBytes(identity, message, sig), "decode")).toBe(true);
    expect(expectOk(ed25519.verifyLibsodiumBytes(identity, message, sig), "decode")).toBe(false);
  });

  test("an R that does not decode verifies false (not an error)", () => {
    const sig = ed25519.sign(kp.signing, message).bytes;
    sig.set(h("0200000000000000000000000000000000000000000000000000000000000000"), 0);
    expect(ed25519.verifyBytes(vk, message, sig)).toEqual({ ok: true, value: false });
  });

  test("signing keys are redacted", () => {
    const text = [String(kp.signing), JSON.stringify(kp.signing), JSON.stringify(kp), inspect(kp.signing)].join(" ");
    expect(text).not.toContain("42424242");
    expect(text).toContain("redacted");
    expect(kp.signing.equals(expectOk(ed25519.signingKey(seed42), "sk"))).toBe(true);
    const out = kp.signing.bytes;
    out[0] = 0;
    expect(kp.signing.bytes[0]).toBe(0x42);
  });

  function signNonce(rBytes: Uint8Array, r: bigint): Uint8Array {
    const a = expandScalar(seed42);
    const k = leToBig(sha512(concatBytes(rBytes, vk, message))) % L;
    const out = new Uint8Array(64);
    out.set(rBytes, 0);
    out.set(bigToLE((r + k * a) % L, 32), 32);
    return out;
  }
});

describe("Ed25519-BIP32", { timeout: 60_000 }, () => {
  // The BramblSc vectors use unclamped kL (which the codec refuses), so they exercise the signing equation only;
  // vector 3's published signature verifies but was not produced by the nonce rule (as in the Scala suite).
  test("signing known answers (BramblSc ExtendedEd25519.json)", () => {
    const vectors = JSON.parse(readDoc("ExtendedEd25519.json")) as BramblVector[];
    vectors.forEach((v, i) => {
      const skb = h(v.inputs.secretKey);
      const msg = h(v.inputs.message);
      const sig = signExtended(skb.subarray(0, 32), skb.subarray(32, 64), msg);
      const vkb = h(v.outputs.verificationKey).subarray(0, 32);
      expect(toHex(Point.BASE.multiply(leToBig(skb.subarray(0, 32)) % L).toBytes())).toBe(toHex(vkb));
      expect(expectOk(ed25519Bip32.verifyBytes(vkb, msg, sig), "decode")).toBe(true);
      if (i === 2) expect(expectOk(ed25519Bip32.verifyBytes(vkb, msg, h(v.outputs.signature)), "decode")).toBe(true);
      else expect(toHex(sig)).toBe(v.outputs.signature);
    });
    const unclamped = ed25519Bip32.signingKey(h(vectors[0]?.inputs.secretKey ?? "").subarray(0, 64));
    expect(unclamped.ok ? "ok" : unclamped.error.kind).toBe("InvalidKey");
  });
});

const Point = nobleEd.Point;
const ed25519Point = (b: Uint8Array) => Point.fromBytes(b, true);

function expandScalar(seed: Uint8Array): bigint {
  const a = sha512(seed).slice(0, 32);
  a[0] = (a[0] ?? 0) & 248;
  a[31] = ((a[31] ?? 0) & 127) | 64;
  return leToBig(a) % L;
}
