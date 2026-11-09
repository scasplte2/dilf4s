import { describe, expect, test } from "vitest";

import { models, type Result } from "@kd5ujc/dilf4s-core";

import {
  bip32Secp256k1,
  bip340,
  blsPop,
  type ChildIndex,
  ed25519,
  ed25519Bip32,
  icarus,
  Message32,
  p256Ecdsa,
  parsePath,
  ristretto255Schnorr,
  secp256k1Ecdsa,
  mnemonicToSeed,
  slip10Ed25519,
  taggedSchnorr,
  toEntropy,
  toMnemonic,
  WordList,
  type KeyError,
  type KeyRefusal,
  type CryptoRefusal,
  type PathRefusal,
  type CryptoError,
} from "../src/index.js";
import { expectOk, h, loadVectors, hexOf } from "./vectors.js";

const vectors = loadVectors("signing_v1.json", models.signingVectors.SigningVectors);
const randomness = h(vectors.randomness);

/** A scheme seen through bytes: derive from a seed, sign a message, verify encodings. */
interface Adapter {
  fromSeed(seed: Uint8Array): { readonly sk: Uint8Array; readonly vk: Uint8Array; sign(message: Uint8Array): Uint8Array };
  verifyBytes(vk: Uint8Array, message: Uint8Array, sig: Uint8Array): Result<boolean, CryptoRefusal>;
  verifyingKey(vk: Uint8Array): Result<unknown, CryptoRefusal>;
}

const m32 = (m: Uint8Array): Message32 => expectOk(Message32.of(m), "32-byte message");

const adapters: Record<string, Adapter> = {
  Ed25519: {
    fromSeed: (seed) => {
      const kp = expectOk(ed25519.fromSeed(seed), "seed");
      return { sk: kp.signing.bytes, vk: kp.verifying.bytes, sign: (m) => expectOk(ed25519.sign(kp.signing, m), "sign").bytes };
    },
    verifyBytes: ed25519.verifyBytes,
    verifyingKey: ed25519.verifyingKey,
  },
  "Ed25519-libsodium": { fromSeed: () => expect.unreachable(), verifyBytes: ed25519.verifyLibsodiumBytes, verifyingKey: ed25519.verifyingKey },
  "P-256-ECDSA": {
    fromSeed: (seed) => {
      const kp = expectOk(p256Ecdsa.fromSeed(seed), "seed");
      return { sk: kp.signing.bytes, vk: kp.verifying.bytes, sign: (m) => expectOk(p256Ecdsa.sign(kp.signing, m), "sign").bytes };
    },
    verifyBytes: p256Ecdsa.verifyBytes,
    verifyingKey: p256Ecdsa.verifyingKey,
  },
  "secp256k1-ECDSA": {
    fromSeed: (seed) => {
      const kp = expectOk(secp256k1Ecdsa.fromSeed(seed), "seed");
      return { sk: kp.signing.bytes, vk: kp.verifying.bytes, sign: (m) => expectOk(secp256k1Ecdsa.sign(kp.signing, m), "sign").bytes };
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
      return { sk: kp.signing.bytes, vk: kp.verifying.bytes, sign: (m) => expectOk(blsPop.sign(kp.signing, m), "sign").bytes };
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
        expect(hexOf(derived.sk)).toBe(key.signingKey);
        expect(hexOf(derived.vk)).toBe(key.verifyingKey);
        expect(key.signatures.length).toBe(3);
        for (const s of key.signatures) {
          expect(hexOf(derived.sign(h(s.message))), `${scheme.scheme} re-sign`).toBe(s.signature);
          expect(a.verifyBytes(h(key.verifyingKey), h(s.message), h(s.signature))).toEqual({ ok: true, value: true });
        }
      }
    });
  }

  test("verification verdicts: valid, invalid, or the part refused and its kind", { timeout: 60_000 }, () => {
    expect(vectors.verification.length).toBeGreaterThan(400);
    const mismatches: string[] = [];
    for (const c of vectors.verification) {
      const a = adapter(c.scheme);
      const r = a.verifyBytes(h(c.verifyingKey), h(c.message), h(c.signature));
      // the key is decoded first, so an error is the key's exactly when the key alone fails to decode
      const actual: models.signingVectors.VerificationResult = r.ok
        ? r.value
          ? "valid"
          : "invalid"
        : r.error.kind === "TypeMismatch"
          ? "invalid"
          : { part: a.verifyingKey(h(c.verifyingKey)).ok ? "signature" : "verifyingKey", kind: r.error.kind };
      // a vector's inputs are bytes, so a TypeMismatch is a failure of this test, never a verdict
      if (!r.ok && r.error.kind === "TypeMismatch") mismatches.push(`${c.scheme}/${c.name}: ${r.error.reason}`);
      if (JSON.stringify(actual) !== JSON.stringify(c.result)) mismatches.push(`${c.scheme}/${c.name}: expected ${JSON.stringify(c.result)}, got ${JSON.stringify(actual)}`);
    }
    expect(mismatches).toEqual([]);
  });

  /** What a `keyRefusals` row's call gives. */
  const keyCall = (c: models.signingVectors.KeyRefusal): Result<unknown, KeyRefusal> => {
    const path = (): Result<readonly ChildIndex[], KeyRefusal> => parsePath(c.path ?? "");
    const publicPath = <K>(start: K, step: (k: K, i: ChildIndex) => Result<K, KeyRefusal>): Result<unknown, KeyRefusal> => {
      const p = path();
      if (!p.ok) return p;
      let at: Result<K, KeyRefusal> = { ok: true, value: start };
      for (const i of p.value) at = at.ok ? step(at.value, i) : at;
      return at;
    };
    switch (c.check) {
      case "bip39-entropy":
        return toMnemonic(h(c.entropy ?? ""));
      case "bip39-phrase":
        return toEntropy(c.phrase ?? "");
      case "bip39-seed": {
        const parsed = toEntropy(c.phrase ?? "");
        return parsed.ok ? mnemonicToSeed(parsed.value.mnemonic, c.passphrase ?? "") : parsed;
      }
      case "word-list":
        return WordList.of(c.words ?? []);
      case "slip10": {
        const p = path();
        return p.ok ? slip10Ed25519.derive(h(c.seed ?? ""), p.value) : p;
      }
      case "bip32": {
        const p = path();
        return p.ok ? bip32Secp256k1.derive(h(c.seed ?? ""), p.value) : p;
      }
      case "bip32-public": {
        const vk = expectOk(secp256k1Ecdsa.verifyingKey(h(c.verifyingKey ?? "")), "vk");
        return publicPath({ key: vk, chainCode: h(c.chainCode ?? "") }, (k, i) => {
          const r = bip32Secp256k1.publicChild(k.key, k.chainCode, i);
          return r.ok ? { ok: true, value: { key: r.value.key, chainCode: r.value.chainCode } } : r;
        });
      }
      case "icarus-public": {
        const vk = expectOk(ed25519Bip32.verifyingKey(h(c.verifyingKey ?? "")), "vk");
        return publicPath({ key: vk, chainCode: h(c.chainCode ?? "") }, (k, i) => {
          const r = icarus.publicChild(k.key, k.chainCode, i);
          return r.ok ? { ok: true, value: { key: r.value.key, chainCode: r.value.chainCode } } : r;
        });
      }
    }
  };

  test("key-derivation refusals: each row's call is refused with its KeyError kind", () => {
    expect(vectors.keyRefusals.length).toBeGreaterThanOrEqual(16);
    const mismatches = vectors.keyRefusals.flatMap((c) => {
      const r = keyCall(c);
      const actual = r.ok ? "accepted" : r.error.kind;
      return actual === c.kind ? [] : [`${c.check}/${c.name}: expected ${c.kind}, got ${actual}`];
    });
    expect(mismatches).toEqual([]);
  });

  test("coverage (conformance.md section 6): every CryptoError and KeyError kind has a row by kind", () => {
    // both types are generated from the schema with their kinds (models.cryptoErrors), and these Records over them do
    // not compile until a kind added there is listed here
    const signing: Record<CryptoError["kind"], "rows" | "exempt"> = {
      InvalidEncoding: "rows",
      InvalidKey: "rows",
      // a JVM provider's own failure: no SDK has one
      BackendFailure: "exempt",
    };
    const keys: Record<KeyError["kind"], "keyRefusals" | "paths"> = {
      InvalidEntropy: "keyRefusals",
      InvalidMnemonic: "keyRefusals",
      InvalidWordList: "keyRefusals",
      InvalidPassphrase: "keyRefusals",
      InvalidDerivation: "keyRefusals",
      InvalidPath: "paths",
    };
    const decoded = new Set<string>(vectors.keyDecoding.map((c) => c.result));
    const verified = new Set<string>(vectors.verification.flatMap((c) => (typeof c.result === "string" ? [] : [c.result.kind])));
    expect(Object.entries(signing).filter(([k, v]) => (v === "rows") !== (decoded.has(k) && verified.has(k)) || (v === "exempt" && (decoded.has(k) || verified.has(k))))).toEqual([]);
    const refused = new Set<string>(vectors.keyRefusals.map((c) => c.kind));
    expect(Object.entries(keys).filter(([k, v]) => (v === "paths" ? !vectors.paths.some((p) => typeof p.result === "string") : !refused.has(k)))).toEqual([]);
  });

  // a signing-key decoder per scheme, and the verifying-key decoder of each (Ed25519-BIP32 keys verify as Ed25519)
  const signingKeyOf: Record<string, (b: Uint8Array) => Result<unknown, CryptoRefusal>> = {
    Ed25519: ed25519.signingKey,
    "Ed25519-BIP32": ed25519Bip32.signingKey,
    "P-256-ECDSA": p256Ecdsa.signingKey,
    "secp256k1-ECDSA": secp256k1Ecdsa.signingKey,
    "BIP-340": bip340.signingKey,
    "secp256k1-tagged-Schnorr": taggedSchnorr.signingKey,
    "ristretto255-Schnorr": ristretto255Schnorr.signingKey,
    "BLS12-381-PoP": blsPop.signingKey,
  };

  test("key decoding: a wrong length is InvalidEncoding, a well-sized non-key InvalidKey", () => {
    expect(vectors.keyDecoding.length).toBeGreaterThan(90);
    const mismatches: string[] = [];
    for (const c of vectors.keyDecoding) {
      const a = adapters[c.scheme];
      const decode = c.part === "signingKey" ? signingKeyOf[c.scheme] : a && ((b: Uint8Array) => a.verifyingKey(b));
      if (decode === undefined) {
        mismatches.push(`no ${c.part} decoder for ${c.scheme}`);
        continue;
      }
      const r = decode(h(c.bytes));
      const actual = r.ok ? "accepted" : r.error.kind;
      if (actual !== c.result) mismatches.push(`${c.scheme} ${c.part} ${c.name}: expected ${c.result}, got ${actual}`);
    }
    expect(mismatches).toEqual([]);
  });

  test("paths are strict, and a refused one is InvalidPath with the row's refusal", () => {
    // the schema's refusals and the SDK's are the same set: each is assignable to the other
    type SchemaRefusal = Exclude<models.signingVectors.PathResult, models.signingVectors.PathIndices>;
    const toSdk = (r: SchemaRefusal): PathRefusal => r;
    const toSchema = (r: PathRefusal): SchemaRefusal => r;
    expect(vectors.paths.length).toBeGreaterThanOrEqual(54);
    const seen = new Set<PathRefusal>();
    for (const c of vectors.paths) {
      const r = parsePath(c.path);
      const actual = r.ok ? { indices: r.value.map((i) => BigInt(i.serialized)) } : r.error.kind === "InvalidPath" ? toSchema(r.error.refusal) : r.error.kind;
      expect(actual, JSON.stringify(c.path.slice(0, 40))).toEqual(c.result);
      if (typeof c.result === "string") seen.add(toSdk(c.result));
    }
    // coverage (conformance.md section 6): a Record over the SDK's refusals, so a refusal added to PathRefusal does not
    // compile until it is listed here, and then needs a row
    const refusals: Record<PathRefusal, true> = {
      missingMaster: true,
      malformed: true,
      leadingZero: true,
      tooManyDigits: true,
      outOfRange: true,
      tooDeep: true,
    };
    const pinned = new Set<string>(seen);
    expect(Object.keys(refusals).filter((r) => !pinned.has(r))).toEqual([]);
  });

  test("BLS proofs of possession reproduce and verify", { timeout: 30_000 }, () => {
    expect(vectors.blsPop.length).toBe(3);
    for (const v of vectors.blsPop) {
      const sk = expectOk(blsPop.signingKey(h(v.signingKey)), "sk");
      expect(hexOf(expectOk(blsPop.publicKey(sk), "publicKey").bytes)).toBe(v.verifyingKey);
      const proof = expectOk(blsPop.popProve(sk), "popProve");
      expect(hexOf(proof.bytes)).toBe(v.proof);
      const vk = expectOk(blsPop.verifyingKey(h(v.verifyingKey)), "vk");
      expect(expectOk(blsPop.popVerify(vk, expectOk(blsPop.signature(h(v.proof)), "proof")), "popVerify")).toBe(true);
      expect(expectOk(blsPop.verify(vk, h(v.verifyingKey), proof), "verify"), "a PoP is not a message signature").toBe(false);
    }
  });
});
