// Ethereum's signer recovery (jlvm.md section 6): the signer's address for signatures noble makes, a known Ethereum
// account, each refusal and each signature that recovers no key, in the order the Rust SDK's `ecrecover.rs` checks them,
// and arguments read through core's byte boundary.
import { runInNewContext } from "node:vm";

import { describe, expect, test } from "vitest";

import { secp256k1 } from "@noble/curves/secp256k1.js";
import { keccak_256 } from "@noble/hashes/sha3.js";

import { ECRECOVER_ADDRESS_BYTES, ecrecover } from "../src/index.js";
import { bigToBE, beToBig, concat } from "../src/internal/bytes.js";
import { h, hexOf } from "./vectors.js";

const N = secp256k1.Point.Fn.ORDER;
const P = secp256k1.Point.Fp.ORDER;

/** The address of a secret key, from its uncompressed public key. */
const addressOf = (sk: Uint8Array): string => hexOf(keccak_256(secp256k1.getPublicKey(sk, false).subarray(1)).subarray(12));

/** A low-s signature `r || s || v` (v in {27, 28}) over a 32-byte digest, by noble. */
function sign(sk: Uint8Array, digest: Uint8Array): Uint8Array {
  const sig = secp256k1.sign(digest, sk, { prehash: false, format: "recovered", lowS: true });
  // noble's recovered format is `recovery || r || s`
  return concat(sig.subarray(1, 65), Uint8Array.of(27 + (sig[0] ?? 0)));
}

const kind = (r: { ok: boolean; error?: { kind: string } }): string => (r.ok ? "ok" : (r.error?.kind ?? "?"));

describe("ecrecover", () => {
  const sk = h("4c0883a69102937d6231471b5dbb6204fe5129617082792ae468d01a3f362318");
  const digest = keccak_256(new TextEncoder().encode("dilf4s ecrecover"));

  test("recovers the signer's address, for v in {27, 28} and {0, 1}", () => {
    // this key's account, as Ethereum's documentation gives it
    expect(addressOf(sk)).toBe("2c7536e3605d9c16a7a3d7b1898e529396a65c23");
    for (let i = 0; i < 16; i++) {
      const d = keccak_256(Uint8Array.of(i));
      const sig = sign(sk, d);
      const got = ecrecover(d, sig);
      expect(got.ok && got.value !== undefined ? hexOf(got.value) : kind(got), `digest ${i.toString()}`).toBe(addressOf(sk));
      const zeroBased = Uint8Array.from(sig);
      zeroBased[64] = (sig[64] ?? 27) - 27;
      const again = ecrecover(d, zeroBased);
      expect(again.ok && again.value !== undefined ? hexOf(again.value) : kind(again), `digest ${i.toString()}, v - 27`).toBe(addressOf(sk));
    }
    const r = ecrecover(digest, sign(sk, digest));
    expect(r.ok && r.value?.length).toBe(ECRECOVER_ADDRESS_BYTES);
  });

  test("the other parity recovers another key, never an error", () => {
    const sig = sign(sk, digest);
    sig[64] = sig[64] === 27 ? 28 : 27;
    const r = ecrecover(digest, sig);
    expect(r.ok, "well formed").toBe(true);
    expect(r.ok && r.value !== undefined ? hexOf(r.value) : "none").not.toBe(addressOf(sk));
  });

  test("malformed: the digest's size, then the signature's, then v", () => {
    const sig = sign(sk, digest);
    const short = ecrecover(digest.subarray(1), sig.subarray(1));
    expect(short.ok ? "ok" : short.error).toMatchObject({ kind: "InvalidEncoding", what: "ecrecover digest" });
    const longSig = ecrecover(digest, concat(sig, Uint8Array.of(0)));
    expect(longSig.ok ? "ok" : longSig.error).toMatchObject({ kind: "InvalidEncoding", what: "ecrecover signature" });
    for (const v of [2, 26, 29, 30, 255]) {
      const bad = Uint8Array.from(sig);
      bad[64] = v;
      const r = ecrecover(digest, bad);
      expect(r.ok ? "ok" : r.error, `v = ${v.toString()}`).toMatchObject({ kind: "InvalidEncoding", what: "ecrecover signature" });
    }
  });

  test("a well-formed signature that recovers no key is undefined", () => {
    const sig = sign(sk, digest);
    const [r, s] = [beToBig(sig.subarray(0, 32)), beToBig(sig.subarray(32, 64))];
    const v = Uint8Array.of(sig[64] ?? 27);
    const cases: readonly (readonly [string, bigint, bigint])[] = [
      ["r zero", 0n, s],
      ["s zero", r, 0n],
      ["r the order", N, s],
      ["s the order", r, N],
      ["r past the order, below p", N + 1n, s],
      ["r all ones", 2n ** 256n - 1n, s],
      ["high s (EIP-2's twin)", r, N - s],
      ["s just above half the order", r, N / 2n + 1n],
    ];
    for (const [name, rr, ss] of cases) {
      const got = ecrecover(digest, concat(bigToBE(rr, 32), bigToBE(ss, 32), v));
      expect(got.ok ? (got.value === undefined ? "none" : hexOf(got.value)) : kind(got), name).toBe("none");
    }
    // half the order itself is low
    const half = ecrecover(digest, concat(bigToBE(r, 32), bigToBE(N / 2n, 32), v));
    expect(half.ok && half.value !== undefined, "s = n / 2 is low").toBe(true);
    // an r below n whose x^3 + 7 is not a square is no x-coordinate
    let x = 1n;
    const isSquare = (a: bigint): boolean => secp256k1.Point.Fp.pow(a, (P - 1n) / 2n) === 1n;
    while (isSquare((x * x * x + 7n) % P)) x++;
    const notX = ecrecover(digest, concat(bigToBE(x, 32), bigToBE(s, 32), v));
    expect(notX.ok ? (notX.value === undefined ? "none" : "some") : kind(notX), `r = ${x.toString()} is no x-coordinate`).toBe("none");
  });

  test("arguments that are not bytes are a TypeMismatch", () => {
    const sig = sign(sk, digest);
    expect(kind(ecrecover("00" as never, sig))).toBe("TypeMismatch");
    expect(kind(ecrecover(digest, [1, 2] as never))).toBe("TypeMismatch");
    const { proxy, revoke } = Proxy.revocable(new Uint8Array(32), {});
    revoke();
    expect(kind(ecrecover(proxy, sig)), "a revoked Proxy").toBe("TypeMismatch");
    expect(kind(ecrecover(new Proxy(digest, {}), sig)), "a Proxy of bytes").toBe("TypeMismatch");
  });

  test("bytes are measured by their internal length; another realm's and shared bytes are bytes", () => {
    const sig = sign(sk, digest);
    class Liar extends Uint8Array {
      override get length(): number {
        return 32;
      }
    }
    const lying = ecrecover(new Liar(digest.subarray(1)), sig);
    expect(lying.ok ? "ok" : lying.error, "31 bytes claiming 32").toMatchObject({ kind: "InvalidEncoding", what: "ecrecover digest" });
    const recovered = ecrecover(new Liar(digest), new Liar(sig));
    expect(recovered.ok && recovered.value !== undefined ? hexOf(recovered.value) : kind(recovered), "a lying length is ignored").toBe(addressOf(sk));
    const foreign = runInNewContext(`Uint8Array.of(${[...sig].join(",")})`) as Uint8Array;
    const shared = new Uint8Array(new SharedArrayBuffer(32));
    shared.set(digest);
    const r = ecrecover(shared, foreign);
    expect(r.ok && r.value !== undefined ? hexOf(r.value) : kind(r), "shared and foreign bytes").toBe(addressOf(sk));
  });
});
