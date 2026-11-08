/**
 * Indices and paths (`signing.md` 3), SLIP-10 Ed25519 (2.2), BIP32 secp256k1 (2.3) and BIP32-Ed25519 Icarus (2.4).
 *
 * Every function checks what a caller passes past the types once, in O(1) per argument (a path: once per index, by
 * index), before reading it: an argument of the wrong type is core's `TypeMismatch` (internal/check.ts). Keys,
 * extended keys and indices must be this package's own, of the scheme at hand.
 */
import { err, models, ok, type Result, type TypeMismatch } from "@kd5ujc/dilf4s-core";
import { mint, ownBytes, UNBOUNDED } from "@kd5ujc/dilf4s-core/internal";
import { secp256k1 } from "@noble/curves/secp256k1.js";
import { hmac } from "@noble/hashes/hmac.js";
import { pbkdf2 } from "@noble/hashes/pbkdf2.js";
import { sha512 } from "@noble/hashes/sha2.js";
import { utf8ToBytes } from "@noble/hashes/utils.js";

import { wellFormed } from "./bip39.js";
import { ed25519, ed25519Bip32 } from "./ed25519.js";
import type { InvalidPath, KeyError, PathRefusal } from "./errors.js";
import { INSPECT, secretBytes, signingKeyBytes, signingKeySchemeOf, verifyingKeyBytes, type Secret, type SchemeName, type SigningKey, type VerifyingKey } from "./keys.js";
import { secp256k1Ecdsa } from "./ecdsa.js";
import { attempt } from "./internal/attempt.js";
import { beToBig, bigToBE, bigToLE, concat, copy, leToBig, u32be, u32le } from "./internal/bytes.js";
import { expected, expectedAt, NOT_A_NUMBER, NOT_A_STRING, readArray } from "./internal/check.js";
import { decodeStrict, mulBase } from "./internal/edwards.js";
import { extendedPublicKey } from "./ed25519.js";
import { verifyingKeyOf } from "./scheme.js";
import { INTERNAL, type Internal } from "./internal/token.js";

/** A derivation refusal, or an argument of the wrong type. */
export type KeyRefusal = KeyError | TypeMismatch;

const invalid = (reason: string): KeyError => ({ kind: "InvalidDerivation", reason });
const HARDENED = 0x80000000;

const INDEX = mint();
let indexOf!: (i: number, hardened: boolean) => Result<ChildIndex, KeyRefusal>;

/** `x` if it is a {@link ChildIndex} made here (a brand check: no Proxy trap or caller code runs). */
const indexArg = (x: unknown): Result<ChildIndex, TypeMismatch> => (INDEX.has(x) ? ok(x as ChildIndex) : err(expected("a ChildIndex")));

/** A child index below 2^31; hardened indices are serialised with bit 31 set. */
export class ChildIndex {
  private constructor(
    token: Internal,
    readonly value: number,
    readonly hardened: boolean,
  ) {
    // without the token none of the caller's arguments is kept: every field is a fixed default
    if (token !== INTERNAL) [this.value, this.hardened] = [0, false];
    if (token === INTERNAL) INDEX.stamp(this);
    Object.freeze(this);
  }

  /** A soft index: an integer in `[0, 2^31)` (`InvalidDerivation`); what is not a number is a `TypeMismatch`. */
  static soft(i: number): Result<ChildIndex, KeyRefusal> {
    return indexOf(i, false);
  }

  /** A hardened index: an integer in `[0, 2^31)` (`InvalidDerivation`); what is not a number is a `TypeMismatch`. */
  static hardened(i: number): Result<ChildIndex, KeyRefusal> {
    return indexOf(i, true);
  }

  static {
    indexOf = (i, hardened) => {
      if (typeof (i as unknown) !== "number") return err(NOT_A_NUMBER);
      return Number.isInteger(i) && i >= 0 && i < HARDENED ? ok(new ChildIndex(INTERNAL, i, hardened)) : err(invalid(`index ${String(i)} out of range`));
    };
  }

  /** The serialised index: `value`, plus 2^31 when hardened. */
  get serialized(): number {
    return this.hardened ? this.value + HARDENED : this.value;
  }
}

// ASCII digits, then an optional hardening mark; a leading zero is refused separately
const SEGMENT = /^([0-9]+)(['h]?)$/;

/** The most digits a segment may have: 2^31 - 1 has ten. */
export const MAX_SEGMENT_DIGITS = models.limits.PATH_MAX_SEGMENT_DIGITS;

/** The most segments a path may have after `m`: BIP32 serialises the depth in one byte. */
export const MAX_DEPTH = models.limits.PATH_MAX_DEPTH;

/**
 * `m/44'/0'/0'/0/7` (or `h` for `'`), relative to the master key; `m` alone is the master. A segment is `0` or a
 * nonzero ASCII digit and more digits, then an optional `'` or `h`, so each index has one spelling. Nothing is trimmed,
 * and an empty segment, a leading zero or any other character is `InvalidPath`, typed by the first rule broken (a
 * `PathRefusal`; `signing.md` 3). A segment of more than `MAX_SEGMENT_DIGITS` digits is refused before any conversion,
 * and the scan stops at segment `MAX_DEPTH`, so parsing is linear in the path and never holds more than `MAX_DEPTH`
 * indices. The message names a segment by its position (0 is the first after `m`) and, past the digit bound, its
 * length; never by its text. A path that is not a string is a `TypeMismatch`, unread.
 */
export function parsePath(path: string): Result<readonly ChildIndex[], InvalidPath | TypeMismatch> {
  if (typeof (path as unknown) !== "string") return err(NOT_A_STRING);
  if (path === "m") return ok([]);
  if (!path.startsWith("m/")) return err(refuse("missingMaster", "path must start with m"));
  const out: ChildIndex[] = [];
  for (let from = 2; ; ) {
    const at = out.length;
    if (at >= MAX_DEPTH) return err(refuse("tooDeep", `path has more than ${String(MAX_DEPTH)} segments`));
    const slash = path.indexOf("/", from);
    const c = parseSegment(path.slice(from, slash < 0 ? path.length : slash), at);
    if (!c.ok) return c;
    out.push(c.value);
    if (slash < 0) return ok(out);
    from = slash + 1;
  }
}

const refuse = (refusal: PathRefusal, reason: string): InvalidPath => ({ kind: "InvalidPath", refusal, reason });

function parseSegment(seg: string, at: number): Result<ChildIndex, InvalidPath> {
  const m = SEGMENT.exec(seg);
  if (m === null) return err(refuse("malformed", `path segment ${String(at)} is malformed`));
  const digits = m[1] ?? "";
  if (digits !== "0" && digits.startsWith("0")) return err(refuse("leadingZero", `path segment ${String(at)} has a leading zero`));
  if (digits.length > MAX_SEGMENT_DIGITS)
    return err(refuse("tooManyDigits", `path segment ${String(at)} has ${String(digits.length)} digits, more than ${String(MAX_SEGMENT_DIGITS)}`));
  // at most ten digits: exact as a double, and the index constructors check the range
  const c = indexOf(Number(digits), m[2] !== "");
  return c.ok ? c : err(refuse("outOfRange", `path segment ${String(at)} is out of range`));
}

const CHAIN_CODE = "a chain code is 32 bytes";

const EXTENDED = mint();

/** An extended key's parts, read from one this package made: its signing key, that key's bytes, and the chain code. */
interface Parts<S extends SchemeName> {
  readonly key: SigningKey<S>;
  readonly secret: Uint8Array;
  readonly chainCode: Uint8Array;
}

let partsOf!: <S extends SchemeName>(x: ExtendedKey<S>) => Parts<S>;

/** An extended private key: a signing key and a 32-byte chain code. Never printed. */
export class ExtendedKey<S extends SchemeName> {
  readonly #chainCode: Uint8Array;
  constructor(
    token: Internal,
    readonly key: SigningKey<S>,
    chainCode: Uint8Array,
  ) {
    this.#chainCode = token === INTERNAL ? copy(chainCode) : new Uint8Array(0);
    if (token === INTERNAL) EXTENDED.stamp(this);
    Object.freeze(this);
  }

  static {
    partsOf = <S extends SchemeName>(x: ExtendedKey<S>) => {
      const secret = signingKeyBytes(x.key, x.key.scheme);
      return { key: x.key, secret: secret.ok ? secret.value : new Uint8Array(0), chainCode: x.#chainCode };
    };
  }

  /**
   * An extended key from a decoded signing key and a 32-byte chain code (copied from its own memory). A key that is not
   * a `SigningKey` of this package's, or a chain code that is not a `Uint8Array`, is a `TypeMismatch`.
   */
  static of<S extends SchemeName>(key: SigningKey<S>, chainCode: Uint8Array): Result<ExtendedKey<S>, KeyRefusal> {
    if (signingKeySchemeOf(key) === undefined) return err(expected("a SigningKey"));
    // within its width: a longer one is refused uncopied
    const view = ownBytes(chainCode, 32);
    if (view === undefined) return err(expected("a Uint8Array chain code"));
    return typeof view !== "number" && view.length === 32 ? ok(new ExtendedKey(INTERNAL, key, view)) : err(invalid(CHAIN_CODE));
  }

  get chainCode(): Uint8Array {
    return copy(this.#chainCode);
  }

  toString(): string {
    // a key built from outside holds the caller's key, which is never read
    return EXTENDED.has(this) ? `ExtendedKey(${this.key.scheme}, <redacted>)` : "ExtendedKey(<not a key of this package>)";
  }

  toJSON(): string {
    return this.toString();
  }

  [INSPECT](): string {
    return this.toString();
  }
}

/** The parts of `x` if it is an `ExtendedKey` of this package's whose key is of `scheme`. */
function extendedArg<S extends SchemeName>(x: unknown, scheme: S): Result<Parts<S>, TypeMismatch> {
  if (!EXTENDED.has(x)) return err(expected(`an ExtendedKey of ${scheme}`));
  const parts = partsOf(x as ExtendedKey<S>);
  return parts.key.scheme === scheme ? ok(parts) : err(expected(`an ExtendedKey of ${scheme}`));
}

/** A caller's path: an array of `ChildIndex`es, read by index, each once. */
const pathArg = (path: unknown): Result<ChildIndex[], TypeMismatch> =>
  readArray(path, "an array of ChildIndex", (i, n) => (INDEX.has(i) ? ok(i as ChildIndex) : err(expectedAt("a ChildIndex", n))));

/** A public child: the verifying key and its chain code. */
export interface PublicChild<S extends SchemeName> {
  readonly key: VerifyingKey<S>;
  readonly chainCode: Uint8Array;
}

const extended = <S extends SchemeName>(key: SigningKey<S>, chainCode: Uint8Array): ExtendedKey<S> => new ExtendedKey(INTERNAL, key, chainCode);
const hmac512 = (key: Uint8Array, data: Uint8Array): Uint8Array => hmac(sha512, key, data);

/**
 * Secret input: a `Secret` or a `Uint8Array` (copied through core's boundary), or a `TypeMismatch`. With `max` (a seed's
 * or a root's bound), longer input is refused as `too` words it, by its length, uncopied.
 */
const secretArg = (x: unknown, max: number, too: string): Result<Uint8Array, KeyRefusal> => {
  const b = secretBytes(x, max);
  if (b === undefined) return err(expected("a Uint8Array or a Secret"));
  return typeof b === "number" ? err(invalid(too)) : ok(b);
};
const SEED_RANGE = "seed must be 16..64 bytes";

function seedRange(seed: Uint8Array): Result<Uint8Array, KeyError> {
  return seed.length >= 16 && seed.length <= 64 ? ok(seed) : err(invalid(SEED_RANGE));
}

/** The derivation along a checked path from a start key, each step by `child`. */
function foldPath<S extends SchemeName>(
  start: Result<Parts<S>, KeyRefusal>,
  path: readonly ChildIndex[],
  child: (k: Parts<S>, i: ChildIndex) => Result<Parts<S>, KeyRefusal>,
): Result<ExtendedKey<S>, KeyRefusal> {
  let acc = start;
  for (const i of path) {
    if (!acc.ok) return acc;
    acc = child(acc.value, i);
  }
  return acc.ok ? ok(extended(acc.value.key, acc.value.chainCode)) : acc;
}

/** The parts of a key this package just derived (its own bytes). */
function partsFrom<S extends SchemeName>(key: SigningKey<S>, chainCode: Uint8Array): Parts<S> {
  const secret = signingKeyBytes(key, key.scheme);
  return { key, secret: secret.ok ? secret.value : new Uint8Array(0), chainCode };
}

/** `child` as a public function: the parent and the index are checked first. */
function checkedChild<S extends SchemeName>(scheme: S, child: (k: Parts<S>, i: ChildIndex) => Result<Parts<S>, KeyRefusal>) {
  return (parent: ExtendedKey<S>, index: ChildIndex): Result<ExtendedKey<S>, KeyRefusal> => {
    const p = extendedArg(parent, scheme);
    if (!p.ok) return p;
    const i = indexArg(index);
    if (!i.ok) return i;
    const c = child(p.value, i.value);
    return c.ok ? ok(extended(c.value.key, c.value.chainCode)) : c;
  };
}

// ------------------------------------------------------------------ SLIP-10 Ed25519

const ED25519_SEED = utf8ToBytes("ed25519 seed");

function slip10Split(i: Uint8Array): Result<Parts<"Ed25519">, KeyRefusal> {
  const k = ed25519.signingKey(i.subarray(0, 32));
  return k.ok ? ok(partsFrom(k.value, i.slice(32, 64))) : err(invalid(k.error.kind));
}

const slip10Master = (seed: unknown): Result<Parts<"Ed25519">, KeyRefusal> => {
  const b = secretArg(seed, 64, SEED_RANGE);
  if (!b.ok) return b;
  const s = seedRange(b.value);
  return s.ok ? slip10Split(hmac512(ED25519_SEED, s.value)) : s;
};

const slip10Child = (parent: Parts<"Ed25519">, index: ChildIndex): Result<Parts<"Ed25519">, KeyRefusal> =>
  index.hardened ? slip10Split(hmac512(parent.chainCode, concat(Uint8Array.of(0), parent.secret, u32be(index.serialized)))) : err(invalid("SLIP-10 Ed25519 supports hardened indices only"));

/** SLIP-10 for Ed25519: hardened derivation only; the 32-byte key is an RFC 8032 seed. */
export const slip10Ed25519 = Object.freeze({
  /** The master key of a 16- to 64-byte seed; a seed that is neither a `Uint8Array` nor a `Secret` is a `TypeMismatch`. */
  master: (seed: Uint8Array | Secret): Result<ExtendedKey<"Ed25519">, KeyRefusal> => foldPath(slip10Master(seed), [], slip10Child),
  /** A hardened child; a parent or index not of this package's is a `TypeMismatch`. */
  child: checkedChild("Ed25519", slip10Child),
  /** The key at `path` from the seed; a path that is not an array of `ChildIndex`es is a `TypeMismatch`. */
  derive: (seed: Uint8Array | Secret, path: readonly ChildIndex[]): Result<ExtendedKey<"Ed25519">, KeyRefusal> => {
    const p = pathArg(path);
    return p.ok ? foldPath(slip10Master(seed), p.value, slip10Child) : p;
  },
});

// ------------------------------------------------------------------ BIP32 secp256k1

const BITCOIN_SEED = utf8ToBytes("Bitcoin seed");
const N = secp256k1.Point.Fn.ORDER;
const SKIP = "derived key is not a valid scalar (skip this index, per BIP32)";

function scalarKey(d: bigint): Result<SigningKey<"secp256k1-ECDSA">, KeyError> {
  const k = d >= 1n && d < N ? secp256k1Ecdsa.signingKey(bigToBE(d, 32)) : undefined;
  return k?.ok === true ? ok(k.value) : err(invalid(SKIP));
}

const bip32Master = (seed: unknown): Result<Parts<"secp256k1-ECDSA">, KeyRefusal> => {
  const b = secretArg(seed, 64, SEED_RANGE);
  if (!b.ok) return b;
  const s = seedRange(b.value);
  if (!s.ok) return s;
  const i = hmac512(BITCOIN_SEED, s.value);
  const k = scalarKey(beToBig(i.subarray(0, 32)));
  return k.ok ? ok(partsFrom(k.value, i.slice(32))) : k;
};

function bip32Child(parent: Parts<"secp256k1-ECDSA">, index: ChildIndex): Result<Parts<"secp256k1-ECDSA">, KeyRefusal> {
  const k = parent.secret;
  const data = index.hardened ? concat(Uint8Array.of(0), k, u32be(index.serialized)) : concat(secp256k1.getPublicKey(k, true), u32be(index.serialized));
  const i = hmac512(parent.chainCode, data);
  const il = beToBig(i.subarray(0, 32));
  if (il >= N) return err(invalid(SKIP));
  const child = scalarKey((il + beToBig(k)) % N);
  return child.ok ? ok(partsFrom(child.value, i.slice(32))) : child;
}

/** A soft public child's checked arguments: the key's own bytes, a 32-byte chain code (copied) and a soft index. */
function publicChildArgs(scheme: SchemeName, key: unknown, chainCode: unknown, index: unknown): Result<readonly [Uint8Array, Uint8Array, ChildIndex], KeyRefusal> {
  const k = verifyingKeyBytes(key, scheme);
  if (!k.ok) return k;
  const view = ownBytes(chainCode, 32);
  if (view === undefined) return err(expected("a Uint8Array chain code"));
  const i = indexArg(index);
  if (!i.ok) return i;
  if (i.value.hardened) return err(invalid("hardened children need the private key"));
  return typeof view !== "number" && view.length === 32 ? ok([k.value, Uint8Array.from(view), i.value]) : err(invalid(CHAIN_CODE));
}

/** BIP32 for secp256k1 (private CKDpriv and public CKDpub). */
export const bip32Secp256k1 = Object.freeze({
  /** The master key of a 16- to 64-byte seed; a seed that is neither a `Uint8Array` nor a `Secret` is a `TypeMismatch`. */
  master: (seed: Uint8Array | Secret): Result<ExtendedKey<"secp256k1-ECDSA">, KeyRefusal> => foldPath(bip32Master(seed), [], bip32Child),
  /** CKDpriv; a parent or index not of this package's is a `TypeMismatch`. */
  child: checkedChild("secp256k1-ECDSA", bip32Child),
  /**
   * CKDpub: `K' = K + [I_L]G` from the parent verifying key and chain code; soft indices only. A key, chain code or
   * index of the wrong type is a `TypeMismatch`.
   */
  publicChild: (key: VerifyingKey<"secp256k1-ECDSA">, chainCode: Uint8Array, index: ChildIndex): Result<PublicChild<"secp256k1-ECDSA">, KeyRefusal> => {
    const a = publicChildArgs("secp256k1-ECDSA", key, chainCode, index);
    if (!a.ok) return a;
    const [kb, cc, idx] = a.value;
    const i = hmac512(cc, concat(kb, u32be(idx.serialized)));
    const il = beToBig(i.subarray(0, 32));
    // only I_L >= n is skipped (BIP32, signing.md 2.3), as in CKDpriv; I_L = 0 adds nothing
    if (il >= N) return err(invalid(SKIP));
    const parent = attempt(() => secp256k1.Point.fromBytes(kb));
    const child = il === 0n ? parent : parent?.add(secp256k1.Point.BASE.multiply(il));
    if (child === undefined || child.is0()) return err(invalid("derived key is the point at infinity (skip this index, per BIP32)"));
    return ok({ key: verifyingKeyOf("secp256k1-ECDSA", child.toBytes(true)), chainCode: i.slice(32) });
  },
  /** The key at `path` from the seed; a path that is not an array of `ChildIndex`es is a `TypeMismatch`. */
  derive: (seed: Uint8Array | Secret, path: readonly ChildIndex[]): Result<ExtendedKey<"secp256k1-ECDSA">, KeyRefusal> => {
    const p = pathArg(path);
    return p.ok ? foldPath(bip32Master(seed), p.value, bip32Child) : p;
  },
});

// ------------------------------------------------------------------ BIP32-Ed25519 (Icarus, CIP-0003)

const TWO_256 = 1n << 256n;

/** `8 * LE(z[0:28])`. */
const times8 = (z: Uint8Array): bigint => 8n * leToBig(z.subarray(0, 28));

function icarusChild(parent: Parts<"Ed25519-BIP32">, index: ChildIndex): Result<Parts<"Ed25519-BIP32">, KeyRefusal> {
  const sk = parent.secret;
  const kL = sk.subarray(0, 32);
  const kR = sk.subarray(32, 64);
  const idx = u32le(index.serialized);
  const a = extendedPublicKey(kL);
  const cc = parent.chainCode;
  const [zData, ccData] = index.hardened
    ? [concat(Uint8Array.of(0x00), kL, kR, idx), concat(Uint8Array.of(0x01), kL, kR, idx)]
    : [concat(Uint8Array.of(0x02), a, idx), concat(Uint8Array.of(0x03), a, idx)];
  const z = hmac512(cc, zData);
  // fixed-width little-endian additions, mod 2^256
  const childKL = bigToLE((times8(z) + leToBig(kL)) % TWO_256, 32);
  const childKR = bigToLE((leToBig(z.subarray(32)) + leToBig(kR)) % TWO_256, 32);
  const key = ed25519Bip32.signingKey(concat(childKL, childKR));
  return key.ok ? ok(partsFrom(key.value, hmac512(cc, ccData).slice(32))) : err(invalid(`derived kL is invalid: ${key.error.kind}`));
}

/** A root from 96 owned bytes `kL || kR || chainCode`, clamped. */
function icarusRoot(b: Uint8Array): Result<Parts<"Ed25519-BIP32">, KeyRefusal> {
  if (b.length !== 96) return err(invalid("root secret must be 96 bytes"));
  b[0] = (b[0] ?? 0) & 0xf8;
  b[31] = ((b[31] ?? 0) & 0x1f) | 0x40;
  const key = ed25519Bip32.signingKey(b.subarray(0, 64));
  return key.ok ? ok(partsFrom(key.value, b.slice(64))) : err(invalid(key.error.kind));
}

/** BIP32-Ed25519 as Icarus / CIP-0003: root keys, private children and soft public children. */
export const icarus = Object.freeze({
  /** A root from 96 bytes `kL || kR || chainCode`, clamped (a copy); what is neither bytes nor a `Secret` is a `TypeMismatch`. */
  root: (secret: Uint8Array | Secret): Result<ExtendedKey<"Ed25519-BIP32">, KeyRefusal> => {
    const b = secretArg(secret, 96, "root secret must be 96 bytes");
    return b.ok ? foldPath(icarusRoot(copy(b.value)), [], icarusChild) : b;
  },
  /**
   * `PBKDF2-HMAC-SHA512(passphrase, entropy, 4096, 96)`, clamped. Entropy that is neither bytes nor a `Secret`, or a
   * passphrase that is not a string, is a `TypeMismatch`.
   */
  fromEntropy: (entropy: Uint8Array | Secret, passphrase = ""): Result<ExtendedKey<"Ed25519-BIP32">, KeyRefusal> => {
    // PBKDF2's salt: entropy of any length is stretched
    const e = secretArg(entropy, UNBOUNDED, "");
    if (!e.ok) return e;
    if (typeof (passphrase as unknown) !== "string") return err(NOT_A_STRING);
    return wellFormed(passphrase) ? foldPath(icarusRoot(pbkdf2(sha512, utf8ToBytes(passphrase), e.value, { c: 4096, dkLen: 96 })), [], icarusChild) : err(invalid("passphrase is not well-formed Unicode"));
  },
  /** A child; a parent or index not of this package's is a `TypeMismatch`. */
  child: checkedChild("Ed25519-BIP32", icarusChild),
  /** `A' = A + [8 LE(Z[0:28])]B`, soft indices only; `A` must be the canonical encoding. Arguments as BIP32's `publicChild`. */
  publicChild: (key: VerifyingKey<"Ed25519-BIP32">, chainCode: Uint8Array, index: ChildIndex): Result<PublicChild<"Ed25519-BIP32">, KeyRefusal> => {
    const args = publicChildArgs("Ed25519-BIP32", key, chainCode, index);
    if (!args.ok) return args;
    const [a, cc, i] = args.value;
    const point = decodeStrict(a);
    if (point === undefined) return err(invalid("verifying key is not a canonical point encoding"));
    const idx = u32le(i.serialized);
    const z = hmac512(cc, concat(Uint8Array.of(0x02), a, idx));
    const child = mulBase(times8(z)).add(point);
    return ok({ key: verifyingKeyOf("Ed25519-BIP32", child.toBytes()), chainCode: hmac512(cc, concat(Uint8Array.of(0x03), a, idx)).slice(32) });
  },
  /** The key at `path` from the root; a root or path not of this package's is a `TypeMismatch`. */
  derive: (root: ExtendedKey<"Ed25519-BIP32">, path: readonly ChildIndex[]): Result<ExtendedKey<"Ed25519-BIP32">, KeyRefusal> => {
    const r = extendedArg(root, "Ed25519-BIP32");
    if (!r.ok) return r;
    const p = pathArg(path);
    return p.ok ? foldPath(ok(r.value), p.value, icarusChild) : p;
  },
});
