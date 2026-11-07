import { runInNewContext } from "node:vm";

import { describe, expect, it } from "vitest";

import {
  bip32Secp256k1,
  bip340,
  blsPop,
  CardanoSumKes,
  ChildIndex,
  ecvrfDraft03,
  ecvrfEll2,
  ecvrfTai,
  ed25519,
  ed25519Bip32,
  ExtendedKey,
  generateMnemonic,
  icarus,
  KesProduct,
  KesProductKey,
  KesSum,
  KesSumKey,
  CardanoSumKesKey,
  Message32,
  Mnemonic,
  mnemonicToSeed,
  p256Ecdsa,
  parsePath,
  ristretto255Schnorr,
  secp256k1Ecdsa,
  Secret,
  Signature,
  SigningKey,
  slip10Ed25519,
  taggedSchnorr,
  toEntropy,
  toMnemonic,
  VerifyingKey,
  VrfOutput,
  VrfProof,
  WordList,
  type SchemeName,
  type Vrf,
  type VrfSuiteName,
} from "../src/index.js";
import { signedOf } from "../src/internal/check.js";
import { expectErr, expectOk } from "./vectors.js";

/** What a JavaScript caller, or a cast, can pass past the types. */
const bad = (x: unknown): never => x as never;
const mismatch = (reason: string): unknown => ({ ok: false, error: { kind: "TypeMismatch", reason } });
const notBytes = mismatch("expected a Uint8Array");
const label = (x: unknown): string => (typeof x === "object" && x !== null ? "an object" : `${typeof x} ${String(x)}`);

/** A Uint8Array subclass whose `length` says `claimed` whatever it holds. */
function liar(bytes: Iterable<number>, claimed: number): Uint8Array {
  class Liar extends Uint8Array {
    override get length(): number {
      return claimed;
    }
  }
  return new Liar([...bytes]);
}

/** A Proxy of `target`, recording every trap that runs. */
function trapped<T extends object>(target: T): { readonly proxy: T; readonly traps: string[] } {
  const traps: string[] = [];
  const handler: ProxyHandler<T> = {};
  for (const name of ["get", "has", "ownKeys", "getOwnPropertyDescriptor", "getPrototypeOf", "set", "defineProperty"] as const) {
    Reflect.set(handler, name, (...args: unknown[]) => {
      traps.push(name);
      return Reflect.apply(Reflect.get(Reflect, name) as (...a: unknown[]) => unknown, undefined, args);
    });
  }
  return { proxy: new Proxy(target, handler), traps };
}

const shared = (bytes: readonly number[]): Uint8Array => {
  const v = new Uint8Array(new SharedArrayBuffer(bytes.length));
  v.set(bytes);
  return v;
};
const foreign = (bytes: Iterable<number>): Uint8Array => runInNewContext(`Uint8Array.of(${[...bytes].join(",")})`) as Uint8Array;

/** Messages that are no bytes: null, undefined, a number, text, objects posing as bytes. */
const notBytesCases = (): unknown[] => [null, undefined, 0, "", {}, { length: 0 }, { [Symbol.toStringTag]: "Uint8Array", length: 0 }, [], new Uint16Array(0)];

const seed = new Uint8Array(32).fill(7);
const empty = new Uint8Array(0);
const aux = new Uint8Array(32);

/** One scheme's sign and verify, as the test drives each: signing the empty message, then verifying a given one. */
interface Driven {
  readonly name: string;
  readonly signEmpty: () => unknown;
  readonly verify: (message: unknown, signature: unknown) => unknown;
  readonly verifyBytes: (message: unknown, signature: unknown) => unknown;
  readonly signWith: (message: unknown) => unknown;
}

function driven(): Driven[] {
  const ed = expectOk(ed25519.fromSeed(seed), "ed");
  const ed32Root = expectOk(icarus.fromEntropy(new Uint8Array(16)), "icarus");
  const ed32vk = expectOk(ed25519Bip32.publicKey(ed32Root.key), "vk");
  const p = expectOk(p256Ecdsa.fromSeed(seed), "p256");
  const k1 = expectOk(secp256k1Ecdsa.fromSeed(seed), "k1");
  const t = expectOk(taggedSchnorr.fromSeed(seed), "tagged");
  const r = expectOk(ristretto255Schnorr.fromSeed(seed), "ristretto");
  const b = expectOk(blsPop.fromSeed(seed), "bls");
  type Sig = Signature<SchemeName>;
  const bytesOf = (s: unknown): Uint8Array => (s as Sig).bytes;
  return [
    { name: "Ed25519", signEmpty: () => expectOk(ed25519.sign(ed.signing, empty), "sign"), verify: (m, s) => ed25519.verify(ed.verifying, bad(m), bad(s)), verifyBytes: (m, s) => ed25519.verifyBytes(ed.verifying.bytes, bad(m), bytesOf(s)), signWith: (m) => ed25519.sign(ed.signing, bad(m)) },
    { name: "Ed25519 libsodium", signEmpty: () => expectOk(ed25519.sign(ed.signing, empty), "sign"), verify: (m, s) => ed25519.verifyLibsodium(ed.verifying, bad(m), bad(s)), verifyBytes: (m, s) => ed25519.verifyLibsodiumBytes(ed.verifying.bytes, bad(m), bytesOf(s)), signWith: (m) => ed25519.sign(ed.signing, bad(m)) },
    { name: "Ed25519-BIP32", signEmpty: () => expectOk(ed25519Bip32.sign(ed32Root.key, empty), "sign"), verify: (m, s) => ed25519Bip32.verify(ed32vk, bad(m), bad(s)), verifyBytes: (m, s) => ed25519Bip32.verifyBytes(ed32vk.bytes, bad(m), bytesOf(s)), signWith: (m) => ed25519Bip32.sign(ed32Root.key, bad(m)) },
    { name: "P-256", signEmpty: () => expectOk(p256Ecdsa.sign(p.signing, empty), "sign"), verify: (m, s) => p256Ecdsa.verify(p.verifying, bad(m), bad(s)), verifyBytes: (m, s) => p256Ecdsa.verifyBytes(p.verifying.bytes, bad(m), bytesOf(s)), signWith: (m) => p256Ecdsa.sign(p.signing, bad(m)) },
    { name: "secp256k1", signEmpty: () => expectOk(secp256k1Ecdsa.sign(k1.signing, empty), "sign"), verify: (m, s) => secp256k1Ecdsa.verify(k1.verifying, bad(m), bad(s)), verifyBytes: (m, s) => secp256k1Ecdsa.verifyBytes(k1.verifying.bytes, bad(m), bytesOf(s)), signWith: (m) => secp256k1Ecdsa.sign(k1.signing, bad(m)) },
    { name: "tagged Schnorr", signEmpty: () => expectOk(taggedSchnorr.sign(t.signing, empty, aux), "sign"), verify: (m, s) => taggedSchnorr.verify(t.verifying, bad(m), bad(s)), verifyBytes: (m, s) => taggedSchnorr.verifyBytes(t.verifying.bytes, bad(m), bytesOf(s)), signWith: (m) => taggedSchnorr.sign(t.signing, bad(m), aux) },
    { name: "ristretto255", signEmpty: () => expectOk(ristretto255Schnorr.sign(r.signing, empty, aux), "sign"), verify: (m, s) => ristretto255Schnorr.verify(r.verifying, bad(m), bad(s)), verifyBytes: (m, s) => ristretto255Schnorr.verifyBytes(r.verifying.bytes, bad(m), bytesOf(s)), signWith: (m) => ristretto255Schnorr.sign(r.signing, bad(m), aux) },
    { name: "BLS", signEmpty: () => expectOk(blsPop.sign(b.signing, empty), "sign"), verify: (m, s) => blsPop.verify(b.verifying, bad(m), bad(s)), verifyBytes: (m, s) => blsPop.verifyBytes(b.verifying.bytes, bad(m), bytesOf(s)), signWith: (m) => blsPop.sign(b.signing, bad(m)) },
  ];
}

describe("a message that is not bytes is a TypeMismatch, never read as empty and never verified", { timeout: 60_000 }, () => {
  it("verifying a signature of the empty message against what is not bytes is refused, in every scheme", () => {
    for (const d of driven()) {
      const sig = d.signEmpty();
      expect(d.verify(empty, sig), `${d.name}: the empty message verifies`).toEqual({ ok: true, value: true });
      for (const m of notBytesCases()) {
        // the reverted attempt read a non-bytes message as empty: this was { ok: true, value: true }
        expect(d.verify(m, sig), `${d.name}: ${label(m)}`).toEqual(notBytes);
        expect(d.verifyBytes(m, sig), `${d.name} bytes: ${label(m)}`).toEqual(notBytes);
        expect(d.signWith(m), `${d.name} sign: ${label(m)}`).toEqual(notBytes);
      }
    }
  });

  it("BIP-340 takes only a Message32 made by Message32.of: an object with its bytes is not one", () => {
    const kp = expectOk(bip340.fromSeed(seed), "bip340");
    const m = expectOk(Message32.of(new Uint8Array(32)), "m32");
    const sig = expectOk(bip340.sign(kp.signing, m, aux), "sign");
    expect(bip340.verify(kp.verifying, m, sig)).toEqual({ ok: true, value: true });
    const fakes: unknown[] = [null, new Uint8Array(32), { bytes: new Uint8Array(32) }, Object.create(Message32.prototype), Reflect.construct(Message32, [Symbol("token"), new Uint8Array(32)])];
    for (const f of fakes) {
      expect(bip340.verify(kp.verifying, bad(f), sig), label(f)).toEqual(mismatch("expected a Message32"));
      expect(bip340.verifyBytes(kp.verifying.bytes, bad(f), sig.bytes), label(f)).toEqual(mismatch("expected a Message32"));
      expect(bip340.sign(kp.signing, bad(f), aux), label(f)).toEqual(mismatch("expected a Message32"));
      expect(bip340.signRandom(kp.signing, bad(f)), label(f)).toEqual(mismatch("expected a Message32"));
    }
    for (const x of notBytesCases()) expect(Message32.of(bad(x)), label(x)).toEqual(notBytes);
    expect(expectErr(Message32.of(liar(new Uint8Array(31), 32)), "31 bytes claiming 32").kind).toBe("InvalidEncoding");
  });

  it("VRF: an alpha that is not bytes is refused, never an output; a key or proof not of the suite is refused", () => {
    const check = <S extends VrfSuiteName>(vrf: Vrf<S>): void => {
      const kp = expectOk(vrf.fromSeed(seed), "vrf");
      const proved = expectOk(vrf.prove(kp.signing, empty), "prove");
      expect(vrf.verify(kp.verifying, empty, proved.proof).ok, vrf.name).toBe(true);
      for (const a of notBytesCases()) {
        expect(vrf.verify(kp.verifying, bad(a), proved.proof), `${vrf.name}: ${label(a)}`).toEqual(notBytes);
        expect(vrf.verifyBytes(kp.verifying.bytes, bad(a), proved.proof.bytes), `${vrf.name}: ${label(a)}`).toEqual(notBytes);
        expect(vrf.prove(kp.signing, bad(a)), `${vrf.name}: ${label(a)}`).toEqual(notBytes);
      }
      // an oversized key or proof is refused by its length (never copied whole first); a well-sized one still verifies
      expect(expectErr(vrf.verifyBytes(kp.verifying.bytes, empty, new Uint8Array(8 << 20)), "an 8 MiB proof").kind).toBe("Malformed");
      expect(expectErr(vrf.verifyBytes(new Uint8Array(1 << 20), empty, proved.proof.bytes), "a 1 MiB key").kind).toBe("Malformed");
      expect(vrf.verifyBytes(kp.verifying.bytes, empty, proved.proof.bytes).ok, "the well-sized pair").toBe(true);
      expect(vrf.verify(kp.verifying, empty, bad({ suite: vrf.name, bytes: proved.proof.bytes }))).toEqual(mismatch(`expected a ${vrf.name} VrfProof`));
      expect(expectErr(vrf.verify(bad({ scheme: vrf.name, bytes: kp.verifying.bytes }), empty, proved.proof), "fake key").kind).toBe("TypeMismatch");
      expect(vrf.publicKey(bad({ bytes: seed }))).toEqual(mismatch(`expected a ${vrf.name} SigningKey`));
    };
    check(ecvrfTai);
    check(ecvrfEll2);
    check(ecvrfDraft03);
    const tai = expectOk(ecvrfTai.fromSeed(seed), "tai");
    const ell2Proof = expectOk(ecvrfEll2.prove(expectOk(ecvrfEll2.fromSeed(seed), "ell2").signing, empty), "ell2").proof;
    expect(ecvrfTai.verify(tai.verifying, empty, bad(ell2Proof))).toEqual(mismatch("expected a ECVRF-EDWARDS25519-SHA512-TAI VrfProof"));
  });

  it("KES: a message that is not bytes is refused at signing and verifying, and keys are this package's", () => {
    const sum = expectOk(KesSum.of("SHA-256", 2), "sum");
    const product = expectOk(KesProduct.of("SHA-256", 1, 1), "product");
    const cardano = expectOk(CardanoSumKes.of(1), "cardano");
    const sk = expectOk(sum.generate(seed), "sk");
    const pk = expectOk(product.generate(seed), "pk");
    const ck = expectOk(cardano.generate(seed), "ck");
    const cases = [
      [sum, sk, expectOk(sk.sign(empty), "sig")],
      [product, pk, expectOk(pk.sign(empty), "sig")],
      [cardano, ck, expectOk(ck.sign(empty), "sig")],
    ] as const;
    for (const [scheme, key, sig] of cases) {
      expect(scheme.verify(key.verifyingKey, 0, empty, sig)).toEqual({ ok: true, value: true });
      for (const m of notBytesCases()) {
        expect(scheme.verify(key.verifyingKey, 0, bad(m), sig), label(m)).toEqual(notBytes);
        expect(key.sign(bad(m)), label(m)).toEqual(notBytes);
      }
      expect(scheme.verify(key.verifyingKey, bad("0"), empty, sig)).toEqual(mismatch("expected a number"));
      expect(scheme.verify(bad(null), 0, empty, sig)).toEqual(notBytes);
      expect(scheme.generate(bad(null))).toEqual(notBytes);
      expect(scheme.load(bad(null), 0)).toEqual(notBytes);
      expect(key.evolveTo(bad("1"))).toEqual(mismatch("expected a number"));
    }
    expect(sum.sign(bad({ period: 0, bytes: sk.bytes }), empty)).toEqual(mismatch("expected a KesSumKey"));
    expect(sum.verifyingKey(bad(pk))).toEqual(mismatch("expected a KesSumKey"));
    expect(cardano.sign(bad(sk), empty)).toEqual(mismatch("expected a CardanoSumKesKey"));
    for (const h of [null, "MD5", 256]) expect(KesSum.of(bad(h), 2), label(h)).toEqual(mismatch("expected a HashAlgorithm"));
    expect(KesSum.of("SHA-256", bad("2"))).toEqual(mismatch("expected a number"));
    expect(CardanoSumKes.of(bad(null))).toEqual(mismatch("expected a number"));
  });

  it("BLS aggregates refuse keys, messages and signatures that are not of their types, never a verdict", () => {
    const b = expectOk(blsPop.fromSeed(seed), "bls");
    const sig = expectOk(blsPop.sign(b.signing, empty), "sig");
    expect(blsPop.fastAggregateVerify([b.verifying], empty, sig)).toEqual({ ok: true, value: true });
    expect(blsPop.aggregateVerify([[b.verifying, empty]], sig)).toEqual({ ok: true, value: true });
    expect(blsPop.batchVerify([[b.verifying, empty, sig]])).toEqual({ ok: true, value: true });
    for (const m of notBytesCases()) {
      expect(blsPop.fastAggregateVerify([b.verifying], bad(m), sig), label(m)).toEqual(notBytes);
      expect(expectErr(blsPop.aggregateVerify([[b.verifying, bad(m)]], sig), label(m)).kind).toBe("TypeMismatch");
      expect(expectErr(blsPop.batchVerify([[b.verifying, bad(m), sig]]), label(m)).kind).toBe("TypeMismatch");
    }
    expect(expectErr(blsPop.fastAggregateVerify([bad({ bytes: b.verifying.bytes })], empty, sig), "fake key").kind).toBe("TypeMismatch");
    expect(expectErr(blsPop.aggregate([bad(null)]), "null signature").kind).toBe("TypeMismatch");
    expect(expectErr(blsPop.aggregate(bad(null)), "no array").kind).toBe("TypeMismatch");
    expect(blsPop.popVerify(b.verifying, bad({ bytes: sig.bytes }))).toEqual(mismatch("expected a BLS12-381-PoP Signature"));
    expect(blsPop.hashToG2(bad(null), Uint8Array.of(1))).toEqual(notBytes);
    expect(blsPop.hashToG2(empty, bad("dst"))).toEqual(notBytes);
    expect(blsPop.deriveChildSk(b.signing, bad("1"))).toEqual(mismatch("expected a number"));
    expect(blsPop.deriveMasterSk(bad(null))).toEqual(notBytes);
  });
});

describe("keys, signatures and secrets are this package's, of the scheme at hand", () => {
  const ed = expectOk(ed25519.fromSeed(seed), "ed");
  const sig = expectOk(ed25519.sign(ed.signing, empty), "sig");

  it("an object with a key's bytes, one built on its prototype, or one built by calling its constructor, is not a key", () => {
    const fakes: unknown[] = [
      { scheme: "Ed25519", bytes: ed.signing.bytes },
      Object.create(SigningKey.prototype),
      Reflect.construct(SigningKey, [Symbol("token"), "Ed25519", ed.signing.bytes]),
      Reflect.construct(SigningKey, [undefined, "Ed25519", ed.signing.bytes]),
    ];
    for (const f of fakes) {
      expect(ed25519.publicKey(bad(f)), "publicKey").toEqual(mismatch("expected a Ed25519 SigningKey"));
      expect(ed25519.sign(bad(f), empty), "sign").toEqual(mismatch("expected a Ed25519 SigningKey"));
      expect(ed.signing.equals(bad(f)), "equals").toBe(false);
    }
    const vkFakes: unknown[] = [{ scheme: "Ed25519", bytes: ed.verifying.bytes }, Reflect.construct(VerifyingKey, [Symbol("token"), "Ed25519", ed.verifying.bytes])];
    for (const f of vkFakes) {
      expect(ed25519.verify(bad(f), empty, sig)).toEqual(mismatch("expected a Ed25519 VerifyingKey"));
      expect(ed.verifying.equals(bad(f))).toBe(false);
    }
    const sigFake = Reflect.construct(Signature, [Symbol("token"), "Ed25519", sig.bytes]) as unknown;
    expect(ed25519.verify(ed.verifying, empty, bad(sigFake))).toEqual(mismatch("expected a Ed25519 Signature"));
    expect(sig.equals(bad(sigFake))).toBe(false);
    // a constructor called from outside never reads what it is given
    expect(() => Reflect.construct(SigningKey, [Symbol("token"), "Ed25519", null]) as unknown).not.toThrow();
  });

  it("a Proxy of a key is not a key, and none of its traps runs", () => {
    const { proxy, traps } = trapped(ed.signing);
    expect(ed25519.sign(proxy, empty)).toEqual(mismatch("expected a Ed25519 SigningKey"));
    const { proxy: message, traps: mtraps } = trapped(Uint8Array.of(1));
    expect(ed25519.verify(ed.verifying, message, sig)).toEqual(notBytes);
    expect([...traps, ...mtraps]).toEqual([]);
  });

  it("a key or signature of another scheme is refused, never verified under rules it was not decoded by", () => {
    const k1 = expectOk(bip340.fromSeed(seed), "bip340");
    const tagged = expectOk(taggedSchnorr.signingKey(k1.signing.bytes), "same scalar");
    const taggedVk = expectOk(taggedSchnorr.publicKey(tagged), "vk");
    const m32 = expectOk(Message32.of(new Uint8Array(32)), "m32");
    const bipSig = expectOk(bip340.sign(k1.signing, m32, aux), "sig");
    expect(taggedSchnorr.verify(taggedVk, new Uint8Array(32), bad(bipSig))).toEqual(mismatch("expected a secp256k1-tagged-Schnorr Signature"));
    expect(taggedSchnorr.verify(bad(k1.verifying), new Uint8Array(32), bad(bipSig))).toEqual(mismatch("expected a secp256k1-tagged-Schnorr VerifyingKey"));
    expect(ed25519Bip32.verify(bad(ed.verifying), empty, bad(sig))).toEqual(mismatch("expected a Ed25519-BIP32 VerifyingKey"));
  });

  it("secrets: what is not bytes is refused, a Secret built from outside is not one, and equality with a non-secret is false", () => {
    for (const x of notBytesCases()) expect(Secret.of(bad(x)), label(x)).toEqual(notBytes);
    const s = expectOk(Secret.of(Uint8Array.of(1, 2)), "secret");
    const fake = Reflect.construct(Secret, [Uint8Array.of(1, 2)]) as unknown;
    expect(s.equals(bad(fake))).toBe(false);
    expect(s.equals(bad({ bytes: Uint8Array.of(1, 2), length: 2 }))).toBe(false);
    expect(slip10Ed25519.master(bad(fake))).toEqual(mismatch("expected a Uint8Array or a Secret"));
    // a secret holds what its bytes hold, not what their length says
    expect(expectOk(Secret.of(liar([1, 2, 3], 1)), "lying").length).toBe(3);
  });
});

describe("bytes are read by their internal length; another realm's and shared bytes are bytes", () => {
  const ed = expectOk(ed25519.fromSeed(seed), "ed");

  it("a lying message is signed and verified as what it holds", () => {
    const plain = Uint8Array.of(1, 2, 3);
    const lying = liar([1, 2, 3], 0);
    const sig = expectOk(ed25519.sign(ed.signing, lying), "sign");
    expect(sig.equals(expectOk(ed25519.sign(ed.signing, plain), "plain"))).toBe(true);
    expect(ed25519.verify(ed.verifying, plain, sig)).toEqual({ ok: true, value: true });
    expect(ed25519.verify(ed.verifying, liar([1, 2, 3], 2), sig)).toEqual({ ok: true, value: true });
    expect(ed25519.verify(ed.verifying, liar([1, 2], 3), sig)).toEqual({ ok: true, value: false });
  });

  it("a key or signature lying about its length is measured by its internal length", () => {
    expect(expectErr(ed25519.signingKey(liar(new Uint8Array(31), 32)), "31 bytes").kind).toBe("InvalidEncoding");
    expect(expectOk(ed25519.signingKey(liar(seed, 0)), "32 bytes").equals(ed.signing)).toBe(true);
    const sig = expectOk(ed25519.sign(ed.signing, empty), "sig");
    expect(ed25519.verifyBytes(liar(ed.verifying.bytes, 1), empty, liar(sig.bytes, 1))).toEqual({ ok: true, value: true });
  });

  it("another realm's bytes and shared bytes are bytes", () => {
    const sig = expectOk(ed25519.sign(ed.signing, foreign([1, 2])), "foreign");
    expect(ed25519.verify(ed.verifying, shared([1, 2]), sig)).toEqual({ ok: true, value: true });
    expect(ed25519.verifyBytes(foreign(ed.verifying.bytes), foreign([1, 2]), foreign(sig.bytes))).toEqual({ ok: true, value: true });
    expect(expectOk(ed25519.sign(ed.signing, shared([1, 2])), "shared").equals(sig)).toBe(true);
  });

  it("a message to sign is read once, as a copy only this thread can change, wherever it lives", () => {
    const s = shared([1, 2, 3]);
    const own = signedOf(s);
    expect(own?.buffer instanceof SharedArrayBuffer, "copied out of the shared buffer").toBe(false);
    s.fill(9);
    expect(Array.from(own ?? []), "the bytes as they were when read").toEqual([1, 2, 3]);
    // a SharedArrayBuffer given ArrayBuffer's prototype looks local: no memory is trusted, every message is copied
    const disguised = shared([1, 2, 3]);
    Object.setPrototypeOf(disguised.buffer, ArrayBuffer.prototype);
    const copied = signedOf(disguised);
    disguised.fill(9);
    expect(Array.from(copied ?? []), "a disguised shared buffer is copied too").toEqual([1, 2, 3]);
    const local = Uint8Array.of(1, 2, 3);
    expect(signedOf(local)?.buffer === local.buffer, "local memory is copied as well").toBe(false);
    const other = foreign([1, 2, 3]);
    expect(signedOf(other)?.buffer instanceof ArrayBuffer, "another realm's memory is copied into this realm's").toBe(true);
  });
});

describe("BIP-39 and derivation refuse arguments of the wrong type", () => {
  it("entropy is read by its internal length: two bytes claiming sixteen are refused, never a two-word mnemonic", () => {
    const lying = liar([1, 2], 16);
    expect(expectErr(toMnemonic(lying), "two bytes").kind).toBe("InvalidEntropy");
    expect(expectOk(toMnemonic(liar(new Uint8Array(16), 2)), "sixteen bytes").words.length).toBe(12);
    for (const x of [null, 5, "entropy", { length: 16 }]) expect(toMnemonic(bad(x)), label(x)).toEqual(mismatch("expected a Uint8Array or a Secret"));
    expect(toMnemonic(new Uint8Array(16), bad({ words: [] }))).toEqual(mismatch("expected a WordList"));
  });

  it("a phrase that is not text is refused unread; a mnemonic built from outside is not one", () => {
    let converted = false;
    const hostile = {
      toString: (): string => {
        converted = true;
        return "abandon ".repeat(11) + "about";
      },
    };
    expect(toEntropy(bad(hostile))).toEqual(mismatch("expected a string"));
    expect(converted, "toString never ran").toBe(false);
    expect(mnemonicToSeed(bad(null))).toEqual(mismatch("expected a Mnemonic"));
    const m = expectOk(toMnemonic(new Uint8Array(16)), "m");
    expect(mnemonicToSeed(m, bad(5))).toEqual(mismatch("expected a string"));
    const forged = Reflect.construct(Mnemonic, [Symbol("token"), ["abandon"], WordList.english]) as unknown;
    expect(mnemonicToSeed(bad(forged))).toEqual(mismatch("expected a Mnemonic"));
    expect(mnemonicToSeed(bad({ words: m.words }))).toEqual(mismatch("expected a Mnemonic"));
    expect(generateMnemonic(bad("16"))).toEqual(mismatch("expected a number"));
    expect(generateMnemonic(16, WordList.english, bad(5))).toEqual(mismatch("expected a function"));
    expect(generateMnemonic(16, WordList.english, () => bad(null))).toEqual(mismatch("expected random bytes"));
    expect(expectErr(generateMnemonic(16, WordList.english, () => new Uint8Array(8)), "short randomness").kind).toBe("InvalidEntropy");
  });

  it("paths, indices, extended keys and chain codes of the wrong type", () => {
    expect(parsePath(bad(5))).toEqual(mismatch("expected a string"));
    expect(ChildIndex.soft(bad("1"))).toEqual(mismatch("expected a number"));
    expect(ChildIndex.hardened(bad(1n))).toEqual(mismatch("expected a number"));
    const master = expectOk(slip10Ed25519.master(seed), "master");
    const forgedIndex = Reflect.construct(ChildIndex, [Symbol("token"), 0, true]) as unknown;
    expect(slip10Ed25519.child(master, bad(forgedIndex))).toEqual(mismatch("expected a ChildIndex"));
    expect(slip10Ed25519.child(master, bad({ value: 0, hardened: true, serialized: 0x80000000 }))).toEqual(mismatch("expected a ChildIndex"));
    expect(slip10Ed25519.derive(seed, bad([forgedIndex]))).toEqual(mismatch("expected a ChildIndex at index 0"));
    expect(slip10Ed25519.child(bad({ key: master.key, chainCode: master.chainCode }), expectOk(ChildIndex.hardened(0), "0'"))).toEqual(mismatch("expected an ExtendedKey of Ed25519"));
    expect(bip32Secp256k1.child(bad(master), expectOk(ChildIndex.soft(0), "0"))).toEqual(mismatch("expected an ExtendedKey of secp256k1-ECDSA"));
    expect(ExtendedKey.of(bad({ bytes: seed }), new Uint8Array(32))).toEqual(mismatch("expected a SigningKey"));
    expect(ExtendedKey.of(master.key, bad(null))).toEqual(mismatch("expected a Uint8Array chain code"));
    const k1 = expectOk(bip32Secp256k1.master(seed), "k1");
    const vk = expectOk(secp256k1Ecdsa.publicKey(k1.key), "vk");
    expect(bip32Secp256k1.publicChild(vk, bad("cc"), expectOk(ChildIndex.soft(0), "0"))).toEqual(mismatch("expected a Uint8Array chain code"));
    expect(bip32Secp256k1.publicChild(bad({ bytes: vk.bytes }), k1.chainCode, expectOk(ChildIndex.soft(0), "0"))).toEqual(mismatch("expected a secp256k1-ECDSA VerifyingKey"));
    expect(icarus.fromEntropy(new Uint8Array(16), bad(5))).toEqual(mismatch("expected a string"));
    expect(icarus.root(bad(null))).toEqual(mismatch("expected a Uint8Array or a Secret"));
    // a chain code is read by its internal length and copied
    const cc = liar(new Uint8Array(32).fill(4), 3);
    expect(expectOk(ExtendedKey.of(master.key, cc), "32 bytes").chainCode).toEqual(new Uint8Array(32).fill(4));
  });
});

describe("a KES key built by calling its constructor from outside never signs, and never evolves into a key of this package's", () => {
  it("sum, product and Cardano keys", () => {
    const sum = expectOk(KesSum.of("SHA-256", 2), "sum");
    const product = expectOk(KesProduct.of("SHA-256", 1, 1), "product");
    const cardano = expectOk(CardanoSumKes.of(1), "cardano");
    let ran = 0;
    // operations of the caller's own, which a forged key would run if any method trusted it
    const ops = { sign: () => ((ran += 1), new Uint8Array(96)), evolve: () => ((ran += 1), { ok: true, value: {} }), root: () => new Uint8Array(32), encode: () => new Uint8Array(0) };
    const forged = [
      ["KesSumKey", Reflect.construct(KesSumKey, [Symbol("token"), sum, ops, { leaf: true }, 0]) as unknown, sum],
      ["KesProductKey", Reflect.construct(KesProductKey, [Symbol("token"), product, ops, { superTree: {}, subTree: {}, nextSeed: new Uint8Array(32), superSig: new Uint8Array(0) }, 0]) as unknown, product],
      ["CardanoSumKesKey", Reflect.construct(CardanoSumKesKey, [Symbol("token"), cardano, new Uint8Array(128), 0]) as unknown, cardano],
    ] as const;
    for (const [name, key, scheme] of forged) {
      const k = key as { sign(m: Uint8Array): unknown; evolve(): unknown; evolveTo(t: number): unknown };
      expect(k.evolve(), `${name}.evolve`).toEqual(mismatch(`expected a ${name}`));
      expect(k.sign(empty), `${name}.sign`).toEqual(mismatch(`expected a ${name}`));
      expect(k.evolveTo(1), `${name}.evolveTo`).toEqual(mismatch(`expected a ${name}`));
      // a target at the key's own period evolved nothing, and handed the forged key back as a success
      expect(k.evolveTo(0), `${name}.evolveTo its own period`).toEqual(mismatch(`expected a ${name}`));
      expect((scheme as { sign(k: unknown, m: Uint8Array): unknown }).sign(key, empty), `${name} scheme sign`).toEqual(mismatch(`expected a ${name}`));
    }
    expect(ran, "no operation of the caller's ran").toBe(0);
  });
});

describe("a KES scheme built by calling its constructor from outside mints no key and computes with nothing of the caller's", () => {
  it("sum, product and Cardano schemes", () => {
    const sum = Reflect.construct(KesSum, [Symbol("token"), "SHA-256", 0]) as KesSum;
    const product = Reflect.construct(KesProduct, [Symbol("token"), "SHA-256", 0, 0]) as KesProduct;
    const cardano = Reflect.construct(CardanoSumKes, [Symbol("token"), -1]) as CardanoSumKes;
    expect(sum.generate(seed)).toEqual(mismatch("expected a KesSum"));
    expect(sum.load(new Uint8Array(sum.keyLength), 0)).toEqual(mismatch("expected a KesSum"));
    expect(sum.verify(new Uint8Array(32), 0, empty, new Uint8Array(sum.signatureLength))).toEqual(mismatch("expected a KesSum"));
    expect(product.generate(seed)).toEqual(mismatch("expected a KesProduct"));
    // a depth of -1 used to recurse without end: the caller's depth is never computed with
    expect(cardano.generate(seed)).toEqual(mismatch("expected a CardanoSumKes"));
    // nor does it sign or evolve a genuine key on the caller's behalf
    const genuine = expectOk(expectOk(KesSum.of("SHA-256", 0), "sum").generate(seed), "key");
    expect(sum.sign(genuine, empty)).toEqual(mismatch("expected a KesSum"));
    expect(sum.evolve(genuine)).toEqual(mismatch("expected a KesSum"));
    expect(sum.evolveTo(genuine, 0)).toEqual(mismatch("expected a KesSum"));
    expect(sum.verifyingKey(genuine)).toEqual(mismatch("expected a KesSum"));
    expect(() => Reflect.construct(KesSum, [Symbol("token"), {}, -1]) as unknown).not.toThrow();
  });
});

describe("a shared refusal cannot be changed into a verdict", () => {
  it("mutating a returned refusal leaves the next call's refusal as it was", () => {
    const ed = expectOk(ed25519.fromSeed(seed), "ed");
    const sig = expectOk(ed25519.sign(ed.signing, empty), "sig");
    const first = ed25519.verify(bad(null), empty, sig);
    expect(() => Object.assign(first, { ok: true, value: true })).toThrow(TypeError);
    expect(ed25519.verify(bad(null), empty, sig)).toEqual(mismatch("expected a Ed25519 VerifyingKey"));
    const noMessage = ed25519.verifyBytes(ed.verifying.bytes, bad(null), sig.bytes);
    expect(Object.isFrozen(noMessage)).toBe(true);
    expect(Object.isFrozen(ed25519.verify(ed.verifying, empty, sig))).toBe(true);
  });
});

describe("a revoked Proxy is refused as a TypeMismatch at every array argument, never thrown on", () => {
  const revoked = (): never => {
    const { proxy, revoke } = Proxy.revocable([], {});
    revoke();
    return bad(proxy);
  };

  it("BLS aggregates, derivation paths and word lists", () => {
    const b = expectOk(blsPop.fromSeed(seed), "bls");
    const sig = expectOk(blsPop.sign(b.signing, empty), "sig");
    expect(blsPop.aggregate(revoked())).toEqual(mismatch("expected an array of BLS12-381-PoP Signatures"));
    expect(blsPop.fastAggregateVerify(revoked(), empty, sig)).toEqual(mismatch("expected an array of BLS12-381-PoP VerifyingKeys"));
    expect(blsPop.aggregateVerify(revoked(), sig)).toEqual(mismatch("expected an array of a [VerifyingKey, Uint8Array] pair"));
    expect(blsPop.batchVerify(bad([revoked()]))).toEqual(mismatch("expected a [VerifyingKey, Uint8Array, Signature] triple at index 0"));
    expect(slip10Ed25519.derive(seed, revoked())).toEqual(mismatch("expected an array of ChildIndex"));
    expect(WordList.of(revoked())).toEqual({ ok: false, error: { kind: "InvalidWordList", reason: "the words are not an array" } });
    const { proxy: random, revoke } = Proxy.revocable((n: number) => new Uint8Array(n), {});
    revoke();
    expect(generateMnemonic(16, WordList.english, random)).toEqual(mismatch("expected a function"));
  });
});

/** A Proxy of `target` that revokes itself as its first read returns: every later read of it would throw. */
function revokingAfterFirstRead<T extends object>(target: T): T {
  const { proxy, revoke } = Proxy.revocable(target, {
    get: (t, name) => {
      const v: unknown = Reflect.get(t, name);
      revoke();
      return v;
    },
  });
  return proxy;
}

describe("an array or tuple a getter revokes while it is read is refused, never thrown on", () => {
  it("BLS aggregates and tuples, derivation paths and word lists", () => {
    const b = expectOk(blsPop.fromSeed(seed), "bls");
    const sig = expectOk(blsPop.sign(b.signing, empty), "sig");
    expect(blsPop.aggregate(revokingAfterFirstRead([sig, sig]))).toEqual(mismatch("expected an array of BLS12-381-PoP Signatures"));
    expect(blsPop.fastAggregateVerify(revokingAfterFirstRead([b.verifying]), empty, sig)).toEqual(mismatch("expected an array of BLS12-381-PoP VerifyingKeys"));
    expect(blsPop.aggregateVerify(bad([revokingAfterFirstRead([b.verifying, empty])]), sig)).toEqual(mismatch("expected a [VerifyingKey, Uint8Array] pair at index 0"));
    expect(blsPop.batchVerify(bad([revokingAfterFirstRead([b.verifying, empty, sig])]))).toEqual(mismatch("expected a [VerifyingKey, Uint8Array, Signature] triple at index 0"));
    const index = expectOk(ChildIndex.hardened(0), "index");
    expect(slip10Ed25519.derive(seed, revokingAfterFirstRead([index]))).toEqual(mismatch("expected an array of ChildIndex"));
    expect(WordList.of(revokingAfterFirstRead([...Array<string>(2048).keys()].map((i) => `w${i.toString()}`)))).toEqual({ ok: false, error: { kind: "InvalidWordList", reason: "word 0 is not a string" } });
  });
});

describe("a key, index or output built by calling its constructor from outside keeps none of the caller's arguments", () => {
  const forge = (c: unknown, args: readonly unknown[]): never => Reflect.construct(c as new (...a: unknown[]) => object, [Symbol("token"), ...args]) as never;

  it("keys, signatures, indices, word lists, VRF proofs and outputs: their printing never runs caller code", () => {
    const scheme = Symbol("scheme");
    for (const c of [SigningKey, VerifyingKey, Signature]) {
      const k: { scheme: unknown; toString(): string } = forge(c, [scheme, new Uint8Array(32)]);
      expect(k.scheme, c.name).toBe("");
      expect(() => k.toString(), `${c.name}.toString`).not.toThrow();
    }
    const index: ChildIndex = forge(ChildIndex, [{ valueOf: () => 1 }, true]);
    expect([index.value, index.hardened, index.serialized], "an index's defaults").toEqual([0, false, 0]);
    const list: WordList = forge(WordList, [["a"], Symbol("separator")]);
    expect(list.separator, "a word list's default").toBe(" ");
    const proof: VrfProof<"ECVRF-EDWARDS25519-SHA512-TAI"> = forge(VrfProof, [Symbol("suite"), new Uint8Array(80)]);
    expect([proof.suite, proof.bytes], "a VRF proof's defaults").toEqual(["", new Uint8Array(0)]);
    // an output reads nothing it is given either: bytes that are not bytes are never copied
    const output: VrfOutput<"ECVRF-EDWARDS25519-SHA512-TAI"> = forge(VrfOutput, [Symbol("suite"), null]);
    expect([output.suite, output.bytes], "a VRF output's defaults").toEqual(["", new Uint8Array(0)]);
  });

  it("KES keys built from outside have no verifying key, and print without reading the caller's scheme", () => {
    const hostile = {
      get hash(): never {
        throw new Error("never read");
      },
      get depth(): never {
        throw new Error("never read");
      },
    };
    const ops = { root: (): never => { throw new Error("never run"); }, encode: () => new Uint8Array(0) };
    const keys: { verifyingKey: Uint8Array; period: number; toString(): string }[] = [
      forge(KesSumKey, [hostile, ops, {}, 5]),
      forge(KesProductKey, [hostile, ops, { superTree: {}, subTree: {}, nextSeed: new Uint8Array(32), superSig: new Uint8Array(0) }, 5]),
      forge(CardanoSumKesKey, [hostile, new Uint8Array(128), 5]),
    ];
    for (const k of keys) {
      expect([k.verifyingKey, k.period], "no verifying key, period 0").toEqual([new Uint8Array(0), 0]);
      expect(k.toString()).toMatch(/not a key of this package/);
    }
  });
});

describe("a BLS query is owned before caller code runs: a key's getter cannot change the message verified", () => {
  it("fastAggregateVerify copies the message before it reads the keys", { timeout: 60_000 }, () => {
    const b = expectOk(blsPop.fromSeed(seed), "bls");
    const sig = expectOk(blsPop.sign(b.signing, Uint8Array.of(1)), "a signature over [1]");
    expect(blsPop.fastAggregateVerify([b.verifying], Uint8Array.of(2), sig), "[2] was not signed").toEqual({ ok: true, value: false });
    const message = Uint8Array.of(2);
    const keys: unknown[] = [];
    Object.defineProperty(keys, 0, {
      get: () => {
        message[0] = 1;
        return b.verifying;
      },
    });
    keys.length = 1;
    expect(blsPop.fastAggregateVerify(bad(keys), message, sig), "the message as given, [2]").toEqual({ ok: true, value: false });
  });
});
