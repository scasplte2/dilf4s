import { describe, expect, it } from "vitest";

import { ed25519, Message32, secp256k1Ecdsa } from "../src/index.js";
import { expectOk } from "./vectors.js";

/** `xs` as a Uint8Array whose own `length` lies. */
const lying = (xs: Uint8Array, length: number): Uint8Array => Object.defineProperty(Uint8Array.from(xs), "length", { get: () => length });

describe("caller bytes are read by their own length, never a subclass's or an own accessor's", () => {
  it("a length-checked key, signature or message is checked, and kept, by its own length", () => {
    const sk = expectOk(ed25519.signingKey(new Uint8Array(32).fill(7)), "a signing key");
    const vk = ed25519.publicKey(sk).bytes;
    expect([...expectOk(ed25519.verifyingKey(lying(vk, 31)), "32 bytes claiming 31").bytes]).toEqual([...vk]);
    expect(ed25519.verifyingKey(lying(vk.subarray(0, 31), 32)).ok, "31 bytes claiming 32").toBe(false);
    expect(expectOk(Message32.of(lying(new Uint8Array(32), 31)), "32 bytes claiming 31").bytes.length).toBe(32);
    expect(Message32.of(lying(new Uint8Array(31), 32)).ok, "31 bytes claiming 32").toBe(false);
    const ecdsaKey = new Uint8Array(32);
    ecdsaKey[31] = 1;
    expect(expectOk(secp256k1Ecdsa.signingKey(lying(ecdsaKey, 33)), "32 bytes claiming 33").bytes.length).toBe(32);
  });
});
