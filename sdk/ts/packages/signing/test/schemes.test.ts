import { describe, expect, test } from "vitest";

import { DomainTag, TaggedHash, toHex } from "@kd5ujc/dilf4s-core";
import { p256 } from "@noble/curves/nist.js";
import { secp256k1 } from "@noble/curves/secp256k1.js";
import { sha256 } from "@noble/hashes/sha2.js";

import {
  bip340,
  Message32,
  p256Ecdsa,
  ristretto255Schnorr,
  secp256k1Ecdsa,
  taggedSchnorr,
  type EcdsaScheme,
  type EcdsaSchemeName,
} from "../src/index.js";
import { bigToBE, bigToLE, beToBig, leToBig } from "../src/internal/bytes.js";
import { L } from "../src/internal/edwards.js";
import { literalTaggedHash, TAGS } from "../src/internal/tags.js";
import { expectOk, h, utf8 } from "./vectors.js";

const kind = (r: { ok: boolean; error?: { kind: string } }): string => (r.ok ? "ok" : (r.error?.kind ?? "?"));

describe("ECDSA", { timeout: 60_000 }, () => {
  test("P-256: RFC 6979 A.2.5 known answers (SHA-256), with s normalised low", () => {
    const n = p256.Point.Fn.ORDER;
    const sk = expectOk(p256Ecdsa.signingKey(h("c9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721")), "sk");
    const low = (sig: string): string => {
      const b = h(sig);
      const s = beToBig(b.subarray(32));
      return toHex(b.subarray(0, 32)) + toHex(bigToBE(s > n / 2n ? n - s : s, 32));
    };
    expect(toHex(p256Ecdsa.publicKey(sk).bytes)).toBe("0360fed4ba255a9d31c961eb74c6356d68c049b8923b61fa6ce669622e60f29fb6");
    expect(toHex(p256Ecdsa.sign(sk, utf8("sample")).bytes)).toBe(
      low("efd48b2aacb6a8fd1140dd9cd45e81d69d2c877b56aaf991c34d0ea84eaf3716f7cb1c942d657c41d436c7a1b6e29f65f3e900dbb9aff4064dc4ab2f843acda8"),
    );
    expect(toHex(p256Ecdsa.sign(sk, utf8("test")).bytes)).toBe(
      low("f1abb023518351cd71d881567b1ea663ed3efcf6c5132b354f28d3b0b7d38367019f4113742a2b14bd25926b49c649155f267e60d3814b4c0cc84250e46f0083"),
    );
  });

  test("secp256k1: RFC 6979 known answer (key 1, \"Satoshi Nakamoto\")", () => {
    const sk = expectOk(secp256k1Ecdsa.signingKey(bigToBE(1n, 32)), "sk");
    expect(toHex(secp256k1Ecdsa.sign(sk, utf8("Satoshi Nakamoto")).bytes)).toBe(
      "934b1ea10a4b3c1757e2b0c017d0b6143ce3c9a7e6a4a49860d7a6ab210ee3d82442ce9d2b916064108014783e923ec36b49743e2ffa1c4496f01a512aafd9e5",
    );
  });

  const cases: [EcdsaScheme<EcdsaSchemeName>, bigint][] = [
    [p256Ecdsa as EcdsaScheme<EcdsaSchemeName>, p256.Point.Fn.ORDER],
    [secp256k1Ecdsa as EcdsaScheme<EcdsaSchemeName>, secp256k1.Point.Fn.ORDER],
  ];
  for (const [scheme, n] of cases) {
    test(`${scheme.name}: high-S is InvalidEncoding; out-of-range keys and bad points are typed errors`, () => {
      const kp = expectOk(scheme.fromSeed(bigToBE(1234567n, 32)), "seed");
      const sig = scheme.sign(kp.signing, h("0102")).bytes;
      expect(scheme.verify(kp.verifying, h("0102"), expectOk(scheme.signature(sig), "sig"))).toBe(true);
      const highS = sig.slice();
      highS.set(bigToBE(n - beToBig(sig.subarray(32)), 32), 32);
      expect(kind(scheme.signature(highS))).toBe("InvalidEncoding");
      expect(kind(scheme.verifyBytes(kp.verifying.bytes, h("0102"), highS))).toBe("InvalidEncoding");
      const rZero = sig.slice();
      rZero.fill(0, 0, 32);
      expect(kind(scheme.signature(rZero))).toBe("InvalidEncoding");
      expect(kind(scheme.signature(sig.subarray(0, 63)))).toBe("InvalidEncoding");
      expect(kind(scheme.signingKey(bigToBE(n, 32)))).toBe("InvalidKey");
      expect(kind(scheme.signingKey(new Uint8Array(32)))).toBe("InvalidKey");
      expect(kind(scheme.signingKey(new Uint8Array(31)))).toBe("InvalidEncoding");
      expect(kind(scheme.verifyingKey(new Uint8Array(32)))).toBe("InvalidEncoding");
      // uncompressed keys are not keys of this scheme, even when the point is valid
      const uncompressed = scheme.name === "P-256-ECDSA" ? p256.getPublicKey(kp.signing.bytes, false) : secp256k1.getPublicKey(kp.signing.bytes, false);
      expect(kind(scheme.verifyingKey(uncompressed))).toBe("InvalidEncoding");
      const badPrefix = kp.verifying.bytes;
      badPrefix[0] = 0x04;
      expect(kind(scheme.verifyingKey(badPrefix))).toBe("InvalidKey");
      // x = 5 is not on secp256k1 (5^3 + 7 = 132 is a non-residue); for P-256 pick an x with no point
      expect(kind(scheme.verifyingKey(h(`02${"ff".repeat(32)}`)))).toBe("InvalidKey");
      expect(scheme.verify(kp.verifying, h("0103"), expectOk(scheme.signature(sig), "sig"))).toBe(false);
    });
  }
});

describe("BIP-340 and tagged Schnorr", { timeout: 60_000 }, () => {
  const vectors = [
    [
      bigToBE(3n, 32),
      new Uint8Array(32),
      new Uint8Array(32),
      "f9308a019258c31049344f85f89d5229b531c845836f99b08601f113bce036f9",
      "e907831f80848d1069a5371b402410364bdf1c5f8307b0084c55f1ce2dca821525f66a4a85ea8b71e482a74f382d2ce5ebeee8fdb2172f477df4900d310536c0",
    ],
    [
      h("b7e151628aed2a6abf7158809cf4f3c762e7160f38b4da56a784d9045190cfef"),
      bigToBE(1n, 32),
      h("243f6a8885a308d313198a2e03707344a4093822299f31d0082efa98ec4e6c89"),
      "dff1d77f2a671c5f36183726db2341be58feae1da2deced843240f7b502ba659",
      "6896bd60eeae296db48a229ff71dfe071bde413e6d43f917dc8dcf8c78de33418906d11ac976abccb20b091292bff4ea897efcb639ea871cfa95f6de339e4b0a",
    ],
  ] as const;

  test("BIP-340 official vectors 0 and 1", () => {
    for (const [skb, aux, msg, vkHex, sigHex] of vectors) {
      const sk = expectOk(bip340.signingKey(skb), "sk");
      const m = expectOk(Message32.of(msg), "msg");
      expect(toHex(bip340.publicKey(sk).bytes)).toBe(vkHex);
      expect(toHex(expectOk(bip340.sign(sk, m, aux), "sign").bytes)).toBe(sigHex);
      expect(bip340.verifyBytes(h(vkHex), m, h(sigHex))).toEqual({ ok: true, value: true });
    }
  });

  test("messages are exactly 32 bytes, aux is 32 bytes, and the schemes never cross-verify", () => {
    expect(kind(Message32.of(new Uint8Array(31)))).toBe("InvalidEncoding");
    const seed = bigToBE(3n, 32);
    const std = expectOk(bip340.fromSeed(seed), "std");
    const tagged = expectOk(taggedSchnorr.fromSeed(seed), "tagged");
    expect(toHex(std.verifying.bytes)).toBe(toHex(tagged.verifying.bytes));
    const msg = new Uint8Array(32).fill(9);
    const m = expectOk(Message32.of(msg), "m");
    const stdSig = expectOk(bip340.sign(std.signing, m, new Uint8Array(32)), "std").bytes;
    const taggedSig = expectOk(taggedSchnorr.sign(tagged.signing, msg, new Uint8Array(32)), "tagged").bytes;
    expect(taggedSchnorr.verifyBytes(tagged.verifying.bytes, msg, stdSig)).toEqual({ ok: true, value: false });
    expect(bip340.verifyBytes(std.verifying.bytes, m, taggedSig)).toEqual({ ok: true, value: false });
    expect(taggedSchnorr.verifyBytes(tagged.verifying.bytes, msg, taggedSig)).toEqual({ ok: true, value: true });
    expect(kind(bip340.sign(std.signing, m, new Uint8Array(31)))).toBe("InvalidEncoding");
    expect(kind(taggedSchnorr.sign(tagged.signing, msg, new Uint8Array(33)))).toBe("InvalidEncoding");
  });

  test("the tagged digest is BIP-340 over taggedHash_SHA256(dilf4s/sig/bip340/v1, m)", () => {
    const kp = expectOk(taggedSchnorr.fromSeed(bigToBE(77n, 32)), "kp");
    const msg = utf8("any length at all");
    const tag = expectOk(DomainTag.of(TAGS.schnorr), "tag");
    const digest = TaggedHash.of("SHA-256", tag).hash(msg);
    const sig = expectOk(taggedSchnorr.sign(kp.signing, msg, new Uint8Array(32).fill(0x42)), "sig");
    const asStd = expectOk(bip340.signingKey(kp.signing.bytes), "sk");
    expect(toHex(expectOk(bip340.sign(asStd, expectOk(Message32.of(digest), "m"), new Uint8Array(32).fill(0x42)), "std").bytes)).toBe(toHex(sig.bytes));
  });

  test("every literal tag satisfies the DomainTag grammar and matches core's TaggedHash", () => {
    for (const t of Object.values(TAGS)) {
      const tag = expectOk(DomainTag.of(t), t);
      for (const alg of ["SHA-256", "BLAKE2b-256", "SHA3-512"] as const)
        expect(toHex(literalTaggedHash(alg, t)(utf8("x")))).toBe(toHex(TaggedHash.of(alg, tag).hash(utf8("x"))));
    }
  });

  test("x-only keys must lift; signatures are length-checked only", () => {
    // x = 5: 5^3 + 7 is not a square mod p
    expect(kind(bip340.verifyingKey(bigToBE(5n, 32)))).toBe("InvalidKey");
    expect(kind(bip340.verifyingKey(new Uint8Array(33)))).toBe("InvalidEncoding");
    expect(kind(taggedSchnorr.signature(new Uint8Array(63)))).toBe("InvalidEncoding");
    expect(taggedSchnorr.signature(new Uint8Array(64).fill(0xff)).ok).toBe(true);
    expect(kind(bip340.signingKey(new Uint8Array(32)))).toBe("InvalidKey");
    const kp = expectOk(bip340.fromSeed(bigToBE(9n, 32)), "kp");
    const m = expectOk(Message32.of(sha256(utf8("r"))), "m");
    const a = bip340.signRandom(kp.signing, m);
    const b = bip340.signRandom(kp.signing, m);
    expect(toHex(a.bytes)).not.toBe(toHex(b.bytes));
    expect(bip340.verify(kp.verifying, m, a) && bip340.verify(kp.verifying, m, b)).toBe(true);
  });
});

describe("ristretto255 Schnorr", { timeout: 60_000 }, () => {
  test("RFC 9591 E.2: group key and the threshold signature verify", () => {
    const sk = expectOk(ristretto255Schnorr.signingKey(h("1b25a55e463cfd15cf14a5d3acc3d15053f08da49c8afcf3ab265f2ebc4f970b")), "sk");
    const pk = "e2a62f39eede11269e3bd5a7d97554f5ca384f9f6d3dd9c3c0d05083c7254f57";
    const sig = h(
      "fc45655fbc66bbffad654ea4ce5fdae253a49a64ace25d9adb62010dd9fb25552164141787162e5b4cab915b4aa45d94655dbb9ed7c378a53b980a0be220a802",
    );
    expect(toHex(ristretto255Schnorr.publicKey(sk).bytes)).toBe(pk);
    expect(ristretto255Schnorr.verifyBytes(h(pk), h("74657374"), sig)).toEqual({ ok: true, value: true });
    expect(ristretto255Schnorr.verifyBytes(h(pk), h("74657375"), sig)).toEqual({ ok: true, value: false });
  });

  test("identity elements, non-canonical scalars, zero keys and short randomness are rejected", () => {
    const identity = new Uint8Array(32);
    const kp = expectOk(ristretto255Schnorr.fromSeed(new Uint8Array(32).fill(1)), "kp");
    const sig = expectOk(ristretto255Schnorr.sign(kp.signing, new Uint8Array(0), new Uint8Array(32).fill(2)), "sig").bytes;
    expect(ristretto255Schnorr.verify(kp.verifying, new Uint8Array(0), expectOk(ristretto255Schnorr.signature(sig), "sig"))).toBe(true);
    expect(kind(ristretto255Schnorr.verifyingKey(identity))).toBe("InvalidKey");
    const identityR = sig.slice();
    identityR.fill(0, 0, 32);
    expect(kind(ristretto255Schnorr.signature(identityR))).toBe("InvalidEncoding");
    const zPlusL = sig.slice();
    zPlusL.set(bigToLE(leToBig(sig.subarray(32)) + L, 32), 32);
    expect(kind(ristretto255Schnorr.signature(zPlusL))).toBe("InvalidEncoding");
    expect(kind(ristretto255Schnorr.signingKey(new Uint8Array(32)))).toBe("InvalidKey");
    expect(kind(ristretto255Schnorr.signingKey(bigToLE(L, 32)))).toBe("InvalidKey");
    expect(kind(ristretto255Schnorr.sign(kp.signing, new Uint8Array(0), new Uint8Array(1)))).toBe("InvalidEncoding");
    const a = ristretto255Schnorr.signRandom(kp.signing, h("01"));
    const b = ristretto255Schnorr.signRandom(kp.signing, h("01"));
    expect(toHex(a.bytes)).not.toBe(toHex(b.bytes));
    expect(ristretto255Schnorr.verify(kp.verifying, h("01"), a) && ristretto255Schnorr.verify(kp.verifying, h("01"), b)).toBe(true);
  });
});
