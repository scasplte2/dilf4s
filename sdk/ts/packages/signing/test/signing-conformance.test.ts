import { describe, expect, test } from "vitest";

import { models, toHex, type Result } from "@kd5ujc/dilf4s-core";

import {
  bip340,
  blsPop,
  ed25519,
  Message32,
  p256Ecdsa,
  ristretto255Schnorr,
  secp256k1Ecdsa,
  taggedSchnorr,
  type SigningError,
} from "../src/index.js";
import { expectOk, h, loadVectors } from "./vectors.js";

const vectors = loadVectors("signing_v1.json", models.signingVectors.SigningVectors);
const randomness = h(vectors.randomness);

/** A scheme seen through bytes: derive from a seed, sign a message, verify encodings. */
interface Adapter {
  fromSeed(seed: Uint8Array): { readonly sk: Uint8Array; readonly vk: Uint8Array; sign(message: Uint8Array): Uint8Array };
  verifyBytes(vk: Uint8Array, message: Uint8Array, sig: Uint8Array): Result<boolean, SigningError>;
  verifyingKey(vk: Uint8Array): Result<unknown, SigningError>;
}

const m32 = (m: Uint8Array): Message32 => expectOk(Message32.of(m), "32-byte message");

const adapters: Record<string, Adapter> = {
  Ed25519: {
    fromSeed: (seed) => {
      const kp = expectOk(ed25519.fromSeed(seed), "seed");
      return { sk: kp.signing.bytes, vk: kp.verifying.bytes, sign: (m) => ed25519.sign(kp.signing, m).bytes };
    },
    verifyBytes: ed25519.verifyBytes,
    verifyingKey: ed25519.verifyingKey,
  },
  "Ed25519-libsodium": { fromSeed: () => expect.unreachable(), verifyBytes: ed25519.verifyLibsodiumBytes, verifyingKey: ed25519.verifyingKey },
  "P-256-ECDSA": {
    fromSeed: (seed) => {
      const kp = expectOk(p256Ecdsa.fromSeed(seed), "seed");
      return { sk: kp.signing.bytes, vk: kp.verifying.bytes, sign: (m) => p256Ecdsa.sign(kp.signing, m).bytes };
    },
    verifyBytes: p256Ecdsa.verifyBytes,
    verifyingKey: p256Ecdsa.verifyingKey,
  },
  "secp256k1-ECDSA": {
    fromSeed: (seed) => {
      const kp = expectOk(secp256k1Ecdsa.fromSeed(seed), "seed");
      return { sk: kp.signing.bytes, vk: kp.verifying.bytes, sign: (m) => secp256k1Ecdsa.sign(kp.signing, m).bytes };
    },
    verifyBytes: secp256k1Ecdsa.verifyBytes,
    verifyingKey: secp256k1Ecdsa.verifyingKey,
  },
  "BIP-340": {
    fromSeed: (seed) => {
      const kp = expectOk(bip340.fromSeed(seed), "seed");
      return { sk: kp.signing.bytes, vk: kp.verifying.bytes, sign: (m) => expectOk(bip340.sign(kp.signing, m32(m), randomness), "sign").bytes };
    },
    verifyBytes: (vk, m, sig) => bip340.verifyBytes(vk, m32(m), sig),
    verifyingKey: bip340.verifyingKey,
  },
  "secp256k1-tagged-Schnorr": {
    fromSeed: (seed) => {
      const kp = expectOk(taggedSchnorr.fromSeed(seed), "seed");
      return { sk: kp.signing.bytes, vk: kp.verifying.bytes, sign: (m) => expectOk(taggedSchnorr.sign(kp.signing, m, randomness), "sign").bytes };
    },
    verifyBytes: taggedSchnorr.verifyBytes,
    verifyingKey: taggedSchnorr.verifyingKey,
  },
  "ristretto255-Schnorr": {
    fromSeed: (seed) => {
      const kp = expectOk(ristretto255Schnorr.fromSeed(seed), "seed");
      return {
        sk: kp.signing.bytes,
        vk: kp.verifying.bytes,
        sign: (m) => expectOk(ristretto255Schnorr.sign(kp.signing, m, randomness), "sign").bytes,
      };
    },
    verifyBytes: ristretto255Schnorr.verifyBytes,
    verifyingKey: ristretto255Schnorr.verifyingKey,
  },
  "BLS12-381-PoP": {
    fromSeed: (seed) => {
      const kp = expectOk(blsPop.fromSeed(seed), "seed");
      return { sk: kp.signing.bytes, vk: kp.verifying.bytes, sign: (m) => blsPop.sign(kp.signing, m).bytes };
    },
    verifyBytes: blsPop.verifyBytes,
    verifyingKey: blsPop.verifyingKey,
  },
};

const adapter = (scheme: string): Adapter => {
  const a = adapters[scheme];
  if (a === undefined) throw new Error(`no adapter for ${scheme}`);
  return a;
};

describe("signing_v1.json", { timeout: 60_000 }, () => {
  test("covers every scheme of signing.md section 1", () => {
    expect(vectors.signing.map((s) => s.scheme)).toEqual([
      "Ed25519",
      "P-256-ECDSA",
      "secp256k1-ECDSA",
      "BIP-340",
      "secp256k1-tagged-Schnorr",
      "ristretto255-Schnorr",
      "BLS12-381-PoP",
    ]);
  });

  for (const scheme of vectors.signing) {
    test(`${scheme.scheme}: keys from seeds, byte-for-byte signatures, and every signature verifies`, { timeout: 60_000 }, () => {
      const a = adapter(scheme.scheme);
      expect(scheme.keys.length).toBe(3);
      for (const key of scheme.keys) {
        const derived = a.fromSeed(h(key.seed));
        expect(toHex(derived.sk)).toBe(key.signingKey);
        expect(toHex(derived.vk)).toBe(key.verifyingKey);
        expect(key.signatures.length).toBe(3);
        for (const s of key.signatures) {
          expect(toHex(derived.sign(h(s.message))), `${scheme.scheme} re-sign`).toBe(s.signature);
          expect(a.verifyBytes(h(key.verifyingKey), h(s.message), h(s.signature))).toEqual({ ok: true, value: true });
        }
      }
    });
  }

  test("verification verdicts (valid, invalid, malformedKey, malformedSignature)", { timeout: 60_000 }, () => {
    expect(vectors.verification.length).toBeGreaterThan(400);
    const mismatches: string[] = [];
    for (const c of vectors.verification) {
      const a = adapter(c.scheme);
      const r = a.verifyBytes(h(c.verifyingKey), h(c.message), h(c.signature));
      // the key is decoded first, so an error is the key's exactly when the key alone fails to decode
      const actual = r.ok ? (r.value ? "valid" : "invalid") : a.verifyingKey(h(c.verifyingKey)).ok ? "malformedSignature" : "malformedKey";
      if (actual !== c.result) mismatches.push(`${c.scheme}/${c.name}: expected ${c.result}, got ${actual} ${r.ok ? "" : JSON.stringify(r.error)}`);
    }
    expect(mismatches).toEqual([]);
  });

  test("BLS proofs of possession reproduce and verify", { timeout: 30_000 }, () => {
    expect(vectors.blsPop.length).toBe(3);
    for (const v of vectors.blsPop) {
      const sk = expectOk(blsPop.signingKey(h(v.signingKey)), "sk");
      expect(toHex(blsPop.publicKey(sk).bytes)).toBe(v.verifyingKey);
      const proof = blsPop.popProve(sk);
      expect(toHex(proof.bytes)).toBe(v.proof);
      const vk = expectOk(blsPop.verifyingKey(h(v.verifyingKey)), "vk");
      expect(blsPop.popVerify(vk, expectOk(blsPop.signature(h(v.proof)), "proof"))).toBe(true);
      expect(blsPop.verify(vk, h(v.verifyingKey), proof), "a PoP is not a message signature").toBe(false);
    }
  });
});
