// Signing's classes, their prototypes, its exported objects and the refusals it shares are frozen when their modules
// load (canonical-encoding.md section 1, "Immutable models"), as core's are (core/test/frozen.test.ts, whose walk and
// write attempts this repeats).
import { readdirSync } from "node:fs";
import { runInNewContext } from "node:vm";
import { describe, expect, it } from "vitest";

import * as nobleBls from "@noble/curves/bls12-381.js";
import * as nobleEd from "@noble/curves/ed25519.js";
import * as nobleNist from "@noble/curves/nist.js";
import * as nobleSecp from "@noble/curves/secp256k1.js";
import * as nobleWeierstrass from "@noble/curves/abstract/weierstrass.js";
import * as nobleBlake2 from "@noble/hashes/blake2.js";
import * as nobleHkdf from "@noble/hashes/hkdf.js";
import * as nobleHmac from "@noble/hashes/hmac.js";
import * as noblePbkdf2 from "@noble/hashes/pbkdf2.js";
import * as nobleSha2 from "@noble/hashes/sha2.js";
import * as nobleUtils from "@noble/hashes/utils.js";

import {
  bip32Secp256k1,
  bip340,
  blsPop,
  CardanoSumKes,
  ChildIndex,
  ecvrfDraft03,
  ecvrfTai,
  ed25519,
  KesProduct,
  KesSum,
  Message32,
  p256Ecdsa,
  ristretto255Schnorr,
  secp256k1Ecdsa,
  Secret,
  Signature,
  SigningKey,
  slip10Ed25519,
  taggedSchnorr,
  toMnemonic,
  VerifyingKey,
  type KeyPair,
  type SchemeName,
} from "../src/index.js";
import type { Result } from "@kd5ujc/dilf4s-core";

import { expectOk } from "./vectors.js";

/** `x` as a `T`, past the types, as a JavaScript caller passes it. */
const bad = (x: unknown): never => x as never;

/** The realm's own prototypes (`Object.prototype`, `Map.prototype`, the iterators'...), where the walk stops. */
const intrinsics: ReadonlySet<unknown> = new Set([
  Object.getPrototypeOf([].values()),
  Object.getPrototypeOf(Object.getPrototypeOf([].values())),
  Object.getPrototypeOf(Uint8Array),
  Object.getPrototypeOf(Uint8Array.prototype),
  ...Object.getOwnPropertyNames(globalThis).flatMap((name): unknown[] => {
    const g: unknown = Reflect.get(globalThis, name);
    const p: unknown = typeof g === "function" ? Reflect.get(g, "prototype") : undefined;
    return typeof p === "object" && p !== null ? [p, Object.getPrototypeOf(p) as unknown] : [];
  }),
]);

/** Whether `f` is a class: its `prototype` is fixed, as a class's is and a plain function's is not. */
const isClass = (f: unknown): boolean => typeof f === "function" && Object.getOwnPropertyDescriptor(f, "prototype")?.writable === false;

/**
 * Every object and class reachable from `roots` through own properties (values and accessors), prototypes and a
 * class's superclass, up to the realm's own and `stops`: iterative, with a visited set. A plain function is not entered.
 */
function reachable(roots: readonly unknown[], stops: ReadonlySet<unknown> = new Set()): readonly object[] {
  const seen = new Set<object>();
  const todo = [...roots];
  while (todo.length > 0) {
    const x = todo.pop();
    const entered = (typeof x === "object" && x !== null) || isClass(x);
    if (!entered || seen.has(x as object) || intrinsics.has(x) || stops.has(x)) continue;
    const o = x as object;
    seen.add(o);
    todo.push(Object.getPrototypeOf(o));
    for (const key of Reflect.ownKeys(o)) {
      const d = Object.getOwnPropertyDescriptor(o, key);
      if (d !== undefined) todo.push(d.value, Reflect.get(d, "get"), Reflect.get(d, "set"));
    }
  }
  return [...seen];
}

/**
 * noble's objects, which the package calls (and `internal/edwards.ts` re-exports noble's `Point`): a dependency's, and
 * as mutable as the realm's built-ins, which this guarantee leaves out as it does them.
 */
const noble: ReadonlySet<unknown> = new Set(
  reachable([nobleBls, nobleEd, nobleNist, nobleSecp, nobleWeierstrass, nobleBlake2, nobleHkdf, nobleHmac, noblePbkdf2, nobleSha2, nobleUtils].flatMap((m) => Object.values(m as Record<string, unknown>))),
);

const seed = new Uint8Array(32).fill(7);
const message = Uint8Array.of(1, 2, 3);
const ed = expectOk(ed25519.fromSeed(seed), "an Ed25519 key pair");
const edSignature = expectOk(ed25519.sign(ed.signing, message), "an Ed25519 signature");

/**
 * One instance of each class a caller is given, and what they hold. A key pair, or a proof with its output, is a fresh
 * object of the one caller's (as a result is), so the walk starts from what it holds.
 */
function instances(): readonly unknown[] {
  const derivations: readonly { readonly name: string; fromSeed(seed: Uint8Array): Result<KeyPair<SchemeName>, unknown> }[] = [ed25519, p256Ecdsa, secp256k1Ecdsa, bip340, taggedSchnorr, ristretto255Schnorr, blsPop];
  const pairs = derivations.map((s) => expectOk(s.fromSeed(seed), `a ${s.name} key pair`));
  const vrf = expectOk(ecvrfTai.fromSeed(seed), "a VRF key pair");
  const proved = expectOk(ecvrfTai.prove(vrf.signing, message), "a VRF proof and output");
  const draft03 = expectOk(ecvrfDraft03.fromSeed(seed), "a draft-03 VRF key pair");
  const sum = expectOk(KesSum.of("SHA-256", 2), "a sum scheme");
  const product = expectOk(KesProduct.of("SHA-256", 1, 1), "a product scheme");
  return [
    ...[...pairs, vrf, draft03].flatMap((p) => [p.signing, p.verifying]),
    proved.proof,
    proved.output,
    edSignature,
    expectOk(Secret.of(seed), "a secret"),
    expectOk(Message32.of(seed), "a 32-byte message"),
    expectOk(ChildIndex.hardened(0), "an index"),
    expectOk(slip10Ed25519.master(seed), "a SLIP-10 master key"),
    expectOk(bip32Secp256k1.master(seed), "a BIP32 master key"),
    expectOk(toMnemonic(seed.subarray(0, 16)), "a mnemonic"),
    sum,
    expectOk(sum.generate(seed), "a sum key"),
    product,
    expectOk(product.generate(seed), "a product key"),
    expectOk(CardanoSumKes.sum6.generate(seed), "a Cardano key"),
  ];
}

describe("signing's classes, their prototypes and its exported objects are frozen", { timeout: 60_000 }, () => {
  it("every object and class reachable from any of signing's modules, as shipped (entry points or not), and its instances", async () => {
    // each module's exports, as a caller importing the shipped file by its URL gets them
    const base = new URL("../src/", import.meta.url);
    const files = readdirSync(base, { recursive: true, encoding: "utf8" }).filter((f) => f.endsWith(".ts"));
    expect(files, "signing's modules").toEqual(expect.arrayContaining(["keys.ts", "internal/tags.ts", "wordlists/english.ts"]));
    const modules = await Promise.all(files.map(async (f) => (await import(/* @vite-ignore */ new URL(f, base).href)) as Record<string, unknown>));
    const reached = reachable([...modules.flatMap((m) => Object.values(m)), ...instances()], noble);
    expect(reached.filter(isClass).length, "signing's classes").toBeGreaterThanOrEqual(18);
    // a module namespace is not frozen, but no code can write to it
    const namespace = (o: object): boolean => Object.prototype.toString.call(o) === "[object Module]";
    expect(reached.filter((o) => !Object.isFrozen(o) && !namespace(o)).map((o) => (isClass(o) ? `class ${(o as { name: string }).name}` : Object.prototype.toString.call(o)))).toEqual([]);
  });
});

type Mode = "strict" | "sloppy";

/** The outcome of `body` (an expression over `t`, `k` and `x`) run as script in `mode`: `ok:` its value, or `throw:` the error's name. */
function attempt(mode: Mode, body: string, t: object, k: PropertyKey, x: unknown): string {
  const script = `${mode === "strict" ? '"use strict";' : ""}(function () { try { return "ok:" + String(${body}); } catch (e) { return "throw:" + e.name; } })()`;
  return runInNewContext(script, { t, k, x }) as string;
}

/**
 * Every way a caller might change `t[k]` to `x`, and what each gives in each mode. A frozen object refuses each one:
 * strict code gets a TypeError for an assignment, sloppy code has it ignored, and `Reflect` answers false in both.
 */
const writes: readonly { readonly name: string; readonly body: string; readonly strict: string; readonly sloppy: string }[] = [
  { name: "assignment", body: "(t[k] = x, 'done')", strict: "throw:TypeError", sloppy: "ok:done" },
  { name: "defineProperty", body: "Object.defineProperty(t, k, { value: x }) && 'done'", strict: "throw:TypeError", sloppy: "throw:TypeError" },
  { name: "Reflect.set", body: "Reflect.set(t, k, x)", strict: "ok:false", sloppy: "ok:false" },
  { name: "Reflect.defineProperty", body: "Reflect.defineProperty(t, k, { value: x })", strict: "ok:false", sloppy: "ok:false" },
  { name: "delete", body: "delete t[k]", strict: "throw:TypeError", sloppy: "ok:false" },
  { name: "setPrototypeOf", body: "Object.setPrototypeOf(t, { [k]: x }) && 'done'", strict: "throw:TypeError", sloppy: "throw:TypeError" },
];

describe("no write to a key class or its prototype changes a verdict", () => {
  it("VerifyingKey, SigningKey, Signature and Secret, and their prototypes, in strict and sloppy code", () => {
    const other = expectOk(ed25519.fromSeed(new Uint8Array(32).fill(8)), "another key pair");
    const probe = (): readonly unknown[] => [
      ed25519.verify(ed.verifying, message, edSignature),
      ed25519.verify(ed.verifying, Uint8Array.of(9), edSignature),
      ed25519.verify(other.verifying, message, edSignature),
      ed.verifying.equals(ed.verifying),
      ed.verifying.equals(other.verifying),
      ed.signing.equals(other.signing),
      edSignature.equals(edSignature),
      ed.verifying.toString(),
      Secret.of(seed).ok,
    ];
    const before = probe();
    expect(before.slice(0, 7), "the verdicts").toEqual([{ ok: true, value: true }, { ok: true, value: false }, { ok: true, value: false }, true, false, false, true]);
    const always = (): boolean => true;
    const targets: readonly (readonly [string, object, PropertyKey, unknown])[] = [
      ["VerifyingKey.prototype.equals", VerifyingKey.prototype, "equals", always],
      ["VerifyingKey.prototype.toString", VerifyingKey.prototype, "toString", () => "evil"],
      ["VerifyingKey.prototype.bytes", VerifyingKey.prototype, "bytes", new Uint8Array(32)],
      ["SigningKey.prototype.equals", SigningKey.prototype, "equals", always],
      ["Signature.prototype.equals", Signature.prototype, "equals", always],
      ["Secret.of", Secret, "of", () => ({ ok: false })],
      ["VerifyingKey.prototype", VerifyingKey, "prototype", {}],
      ["a verifying key's scheme", ed.verifying, "scheme", "Ed25519-BIP32"],
      ["ed25519.verify", ed25519, "verify", () => ({ ok: true, value: true })],
    ];
    for (const mode of ["strict", "sloppy"] as const) {
      for (const [what, t, k, x] of targets) {
        expect(Object.getOwnPropertyDescriptor(t, k), `${what} is its own`).toBeDefined();
        for (const w of writes) expect(attempt(mode, w.body, t, k, x), `${w.name} on ${what}, ${mode}`).toBe(w[mode]);
      }
    }
    expect(probe(), "what the keys and the scheme answer").toEqual(before);
  });
});

describe("a refusal signing gives more than one caller cannot change what the next is given", () => {
  const key = expectOk(KesSum.of("SHA-256", 1), "a sum scheme");
  const probes: readonly (readonly [string, () => { readonly ok: boolean; readonly error?: unknown }])[] = [
    ["a verifying key of the wrong type", () => ed25519.verify(bad(null), message, edSignature)],
    ["a message that is not bytes", () => ed25519.verify(ed.verifying, bad("m"), edSignature)],
    ["a signing key of the wrong type", () => ed25519.sign(bad({}), message)],
    ["a BIP-340 key of the wrong type", () => bip340.sign(bad(null), expectOk(Message32.of(seed), "m32"), seed)],
    ["not a Message32", () => bip340.verify(expectOk(bip340.fromSeed(seed), "kp").verifying, bad(seed), bad(null))],
    ["not a KesSum", () => KesSum.prototype.generate.call(bad({}), seed)],
    ["not a KES key", () => key.sign(bad(null), message)],
    ["not bytes to decode", () => ed25519.verifyingKey(bad(1))],
    ["a ristretto255 key of the wrong type", () => ristretto255Schnorr.sign(bad(1), message, seed)],
  ];

  for (const [what, probe] of probes) {
    it(what, () => {
      const first = probe();
      const again = probe();
      expect(first.ok, "a refusal").toBe(false);
      const shared = !again.ok && again.error === first.error;
      expect(!shared || Object.isFrozen(first.error), "an error given to every caller is frozen").toBe(true);
      expect(again !== first || Object.isFrozen(first), "a result given to every caller is frozen").toBe(true);
      const before = structuredClone(first.error);
      for (const mode of ["strict", "sloppy"] as const) {
        for (const w of writes) {
          attempt(mode, w.body, first.error as object, "kind", "Poisoned");
          attempt(mode, w.body, first, "ok", true);
        }
        attempt(mode, "(t.reason = x, 'done')", first.error as object, "reason", "poisoned");
      }
      expect(again.error, "what the next caller is given").toEqual(before);
      expect(probe()).toEqual({ ok: false, error: before });
    });
  }
});
