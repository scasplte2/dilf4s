/** Indices and paths (`signing.md` 3), SLIP-10 Ed25519 (2.2), BIP32 secp256k1 (2.3) and BIP32-Ed25519 Icarus (2.4). */
import { err, ok, type Result } from "@kd5ujc/dilf4s-core";
import { secp256k1 } from "@noble/curves/secp256k1.js";
import { hmac } from "@noble/hashes/hmac.js";
import { pbkdf2 } from "@noble/hashes/pbkdf2.js";
import { sha512 } from "@noble/hashes/sha2.js";
import { utf8ToBytes } from "@noble/hashes/utils.js";

import { wellFormed } from "./bip39.js";
import { ed25519, ed25519Bip32 } from "./ed25519.js";
import type { KeyError } from "./errors.js";
import { INSPECT, Secret, type SchemeName, type SigningKey, type VerifyingKey } from "./keys.js";
import { secp256k1Ecdsa } from "./ecdsa.js";
import { attempt } from "./internal/attempt.js";
import { beToBig, bigToBE, bigToLE, concat, copy, leToBig, u32be, u32le } from "./internal/bytes.js";
import { decodeStrict, mulBase } from "./internal/edwards.js";
import { verifyingKeyOf } from "./scheme.js";
import { INTERNAL, type Internal } from "./internal/token.js";

const invalid = (reason: string): KeyError => ({ kind: "InvalidDerivation", reason });
const HARDENED = 0x80000000;

/** A child index below 2^31; hardened indices are serialised with bit 31 set. */
export class ChildIndex {
  private constructor(
    readonly value: number,
    readonly hardened: boolean,
  ) {
    Object.freeze(this);
  }

  static soft(i: number): Result<ChildIndex, KeyError> {
    return Number.isInteger(i) && i >= 0 && i < HARDENED ? ok(new ChildIndex(i, false)) : err(invalid(`index ${String(i)} out of range`));
  }

  static hardened(i: number): Result<ChildIndex, KeyError> {
    return Number.isInteger(i) && i >= 0 && i < HARDENED ? ok(new ChildIndex(i, true)) : err(invalid(`index ${String(i)} out of range`));
  }

  /** The serialised index: `value`, plus 2^31 when hardened. */
  get serialized(): number {
    return this.hardened ? this.value + HARDENED : this.value;
  }
}

// ASCII digits (leading zeros allowed, as in Scala and Rust), then an optional hardening mark
const SEGMENT = /^([0-9]+)(['h]?)$/;

/**
 * `m/44'/0'/0'/0/7` (or `h` for `'`), relative to the master key; `m` alone is the master. Nothing is trimmed, and an
 * empty segment or any character but ASCII digits and a final `'` or `h` is `InvalidDerivation` (`signing.md` 3).
 */
export function parsePath(path: string): Result<readonly ChildIndex[], KeyError> {
  const [head, ...rest] = path.split("/");
  if (head !== "m") return err(invalid("path must start with m"));
  const out: ChildIndex[] = [];
  for (const seg of rest) {
    const m = SEGMENT.exec(seg);
    if (m === null) return err(invalid(`bad path segment '${seg}'`));
    const i = Number(m[1]);
    const c = m[2] === "" ? ChildIndex.soft(i) : ChildIndex.hardened(i);
    if (!c.ok) return c;
    out.push(c.value);
  }
  return ok(out);
}

const CHAIN_CODE = "a chain code is 32 bytes";

/** An extended private key: a signing key and a 32-byte chain code. Never printed. */
export class ExtendedKey<S extends SchemeName> {
  readonly #chainCode: Uint8Array;
  constructor(
    _token: Internal,
    readonly key: SigningKey<S>,
    chainCode: Uint8Array,
  ) {
    this.#chainCode = copy(chainCode);
    Object.freeze(this);
  }

  /** An extended key from a decoded signing key and a 32-byte chain code. */
  static of<S extends SchemeName>(key: SigningKey<S>, chainCode: Uint8Array): Result<ExtendedKey<S>, KeyError> {
    return chainCode.length === 32 ? ok(new ExtendedKey(INTERNAL, key, chainCode)) : err(invalid(CHAIN_CODE));
  }

  get chainCode(): Uint8Array {
    return copy(this.#chainCode);
  }

  toString(): string {
    return `ExtendedKey(${this.key.scheme}, <redacted>)`;
  }

  toJSON(): string {
    return this.toString();
  }

  [INSPECT](): string {
    return this.toString();
  }
}

/** A public child: the verifying key and its chain code. */
export interface PublicChild<S extends SchemeName> {
  readonly key: VerifyingKey<S>;
  readonly chainCode: Uint8Array;
}

const extended = <S extends SchemeName>(key: SigningKey<S>, chainCode: Uint8Array): ExtendedKey<S> => new ExtendedKey(INTERNAL, key, chainCode);
const bytesOf = (s: Uint8Array | Secret): Uint8Array => (s instanceof Secret ? s.bytes : s);
const hmac512 = (key: Uint8Array, data: Uint8Array): Uint8Array => hmac(sha512, key, data);

function seedRange(seed: Uint8Array): Result<Uint8Array, KeyError> {
  return seed.length >= 16 && seed.length <= 64 ? ok(seed) : err(invalid("seed must be 16..64 bytes"));
}

function foldPath<S extends SchemeName>(
  start: Result<ExtendedKey<S>, KeyError>,
  path: readonly ChildIndex[],
  child: (k: ExtendedKey<S>, i: ChildIndex) => Result<ExtendedKey<S>, KeyError>,
): Result<ExtendedKey<S>, KeyError> {
  let acc = start;
  for (const i of path) {
    if (!acc.ok) return acc;
    acc = child(acc.value, i);
  }
  return acc;
}

// ------------------------------------------------------------------ SLIP-10 Ed25519

const ED25519_SEED = utf8ToBytes("ed25519 seed");

function slip10Split(i: Uint8Array): Result<ExtendedKey<"Ed25519">, KeyError> {
  const k = ed25519.signingKey(i.subarray(0, 32));
  return k.ok ? ok(extended(k.value, i.subarray(32, 64))) : err(invalid(k.error.kind));
}

/** SLIP-10 for Ed25519: hardened derivation only; the 32-byte key is an RFC 8032 seed. */
export const slip10Ed25519 = Object.freeze({
  master: (seed: Uint8Array | Secret): Result<ExtendedKey<"Ed25519">, KeyError> => {
    const s = seedRange(bytesOf(seed));
    return s.ok ? slip10Split(hmac512(ED25519_SEED, s.value)) : s;
  },
  child: (parent: ExtendedKey<"Ed25519">, index: ChildIndex): Result<ExtendedKey<"Ed25519">, KeyError> =>
    index.hardened
      ? slip10Split(hmac512(parent.chainCode, concat(Uint8Array.of(0), parent.key.bytes, u32be(index.serialized))))
      : err(invalid("SLIP-10 Ed25519 supports hardened indices only")),
  derive: (seed: Uint8Array | Secret, path: readonly ChildIndex[]): Result<ExtendedKey<"Ed25519">, KeyError> =>
    foldPath(slip10Ed25519.master(seed), path, slip10Ed25519.child),
});

// ------------------------------------------------------------------ BIP32 secp256k1

const BITCOIN_SEED = utf8ToBytes("Bitcoin seed");
const N = secp256k1.Point.Fn.ORDER;
const SKIP = "derived key is not a valid scalar (skip this index, per BIP32)";

function scalarKey(d: bigint): Result<SigningKey<"secp256k1-ECDSA">, KeyError> {
  const k = d >= 1n && d < N ? secp256k1Ecdsa.signingKey(bigToBE(d, 32)) : undefined;
  return k?.ok === true ? ok(k.value) : err(invalid(SKIP));
}

/** BIP32 for secp256k1 (private CKDpriv and public CKDpub). */
export const bip32Secp256k1 = Object.freeze({
  master: (seed: Uint8Array | Secret): Result<ExtendedKey<"secp256k1-ECDSA">, KeyError> => {
    const s = seedRange(bytesOf(seed));
    if (!s.ok) return s;
    const i = hmac512(BITCOIN_SEED, s.value);
    const k = scalarKey(beToBig(i.subarray(0, 32)));
    return k.ok ? ok(extended(k.value, i.subarray(32))) : k;
  },
  child: (parent: ExtendedKey<"secp256k1-ECDSA">, index: ChildIndex): Result<ExtendedKey<"secp256k1-ECDSA">, KeyError> => {
    const k = parent.key.bytes;
    const data = index.hardened
      ? concat(Uint8Array.of(0), k, u32be(index.serialized))
      : concat(secp256k1Ecdsa.publicKey(parent.key).bytes, u32be(index.serialized));
    const i = hmac512(parent.chainCode, data);
    const il = beToBig(i.subarray(0, 32));
    if (il >= N) return err(invalid(SKIP));
    const child = scalarKey((il + beToBig(k)) % N);
    return child.ok ? ok(extended(child.value, i.subarray(32))) : child;
  },
  /** CKDpub: `K' = K + [I_L]G` from the parent verifying key and chain code; soft indices only. */
  publicChild: (
    key: VerifyingKey<"secp256k1-ECDSA">,
    chainCode: Uint8Array,
    index: ChildIndex,
  ): Result<PublicChild<"secp256k1-ECDSA">, KeyError> => {
    if (index.hardened) return err(invalid("hardened children need the private key"));
    if (chainCode.length !== 32) return err(invalid(CHAIN_CODE));
    const i = hmac512(chainCode, concat(key.bytes, u32be(index.serialized)));
    const il = beToBig(i.subarray(0, 32));
    // only I_L >= n is skipped (BIP32, signing.md 2.3), as in CKDpriv; I_L = 0 adds nothing
    if (il >= N) return err(invalid(SKIP));
    const parent = attempt(() => secp256k1.Point.fromBytes(key.bytes));
    const child = il === 0n ? parent : parent?.add(secp256k1.Point.BASE.multiply(il));
    if (child === undefined || child.is0()) return err(invalid("derived key is the point at infinity (skip this index, per BIP32)"));
    return ok({ key: verifyingKeyOf("secp256k1-ECDSA", child.toBytes(true)), chainCode: i.slice(32) });
  },
  derive: (seed: Uint8Array | Secret, path: readonly ChildIndex[]): Result<ExtendedKey<"secp256k1-ECDSA">, KeyError> =>
    foldPath(bip32Secp256k1.master(seed), path, bip32Secp256k1.child),
});

// ------------------------------------------------------------------ BIP32-Ed25519 (Icarus, CIP-0003)

const TWO_256 = 1n << 256n;

/** `8 * LE(z[0:28])`. */
const times8 = (z: Uint8Array): bigint => 8n * leToBig(z.subarray(0, 28));

function icarusChild(parent: ExtendedKey<"Ed25519-BIP32">, index: ChildIndex): Result<ExtendedKey<"Ed25519-BIP32">, KeyError> {
  const sk = parent.key.bytes;
  const kL = sk.subarray(0, 32);
  const kR = sk.subarray(32, 64);
  const idx = u32le(index.serialized);
  const a = ed25519Bip32.publicKey(parent.key).bytes;
  const cc = parent.chainCode;
  const [zData, ccData] = index.hardened
    ? [concat(Uint8Array.of(0x00), kL, kR, idx), concat(Uint8Array.of(0x01), kL, kR, idx)]
    : [concat(Uint8Array.of(0x02), a, idx), concat(Uint8Array.of(0x03), a, idx)];
  const z = hmac512(cc, zData);
  // fixed-width little-endian additions, mod 2^256
  const childKL = bigToLE((times8(z) + leToBig(kL)) % TWO_256, 32);
  const childKR = bigToLE((leToBig(z.subarray(32)) + leToBig(kR)) % TWO_256, 32);
  const key = ed25519Bip32.signingKey(concat(childKL, childKR));
  return key.ok ? ok(extended(key.value, hmac512(cc, ccData).subarray(32))) : err(invalid(`derived kL is invalid: ${key.error.kind}`));
}

function icarusRoot(secret: Uint8Array | Secret): Result<ExtendedKey<"Ed25519-BIP32">, KeyError> {
  const b = copy(bytesOf(secret));
  if (b.length !== 96) return err(invalid("root secret must be 96 bytes"));
  b[0] = (b[0] ?? 0) & 0xf8;
  b[31] = ((b[31] ?? 0) & 0x1f) | 0x40;
  const key = ed25519Bip32.signingKey(b.subarray(0, 64));
  return key.ok ? ok(extended(key.value, b.subarray(64))) : err(invalid(key.error.kind));
}

/** BIP32-Ed25519 as Icarus / CIP-0003: root keys, private children and soft public children. */
export const icarus = Object.freeze({
  /** A root from 96 bytes `kL || kR || chainCode`, clamped. */
  root: icarusRoot,
  /** `PBKDF2-HMAC-SHA512(passphrase, entropy, 4096, 96)`, clamped. */
  fromEntropy: (entropy: Uint8Array | Secret, passphrase = ""): Result<ExtendedKey<"Ed25519-BIP32">, KeyError> =>
    wellFormed(passphrase)
      ? icarusRoot(pbkdf2(sha512, utf8ToBytes(passphrase), bytesOf(entropy), { c: 4096, dkLen: 96 }))
      : err(invalid("passphrase is not well-formed Unicode")),
  child: icarusChild,
  /** `A' = A + [8 LE(Z[0:28])]B`, soft indices only; `A` must be the canonical encoding. */
  publicChild: (key: VerifyingKey<"Ed25519-BIP32">, chainCode: Uint8Array, index: ChildIndex): Result<PublicChild<"Ed25519-BIP32">, KeyError> => {
    if (index.hardened) return err(invalid("hardened children need the private key"));
    if (chainCode.length !== 32) return err(invalid(CHAIN_CODE));
    const a = key.bytes;
    const point = decodeStrict(a);
    if (point === undefined) return err(invalid("verifying key is not a canonical point encoding"));
    const idx = u32le(index.serialized);
    const z = hmac512(chainCode, concat(Uint8Array.of(0x02), a, idx));
    const child = mulBase(times8(z)).add(point);
    return ok({ key: verifyingKeyOf("Ed25519-BIP32", child.toBytes()), chainCode: hmac512(chainCode, concat(Uint8Array.of(0x03), a, idx)).slice(32) });
  },
  derive: (root: ExtendedKey<"Ed25519-BIP32">, path: readonly ChildIndex[]): Result<ExtendedKey<"Ed25519-BIP32">, KeyError> =>
    foldPath(ok(root), path, icarusChild),
});

