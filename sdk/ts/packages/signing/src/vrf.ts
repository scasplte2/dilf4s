/**
 * ECVRF over edwards25519 with SHA-512 (`vrf.md`): RFC 9381 TAI and ELL2, and draft-03 Elligator2 as the Cardano fork
 * of libsodium implements it. The suites are distinct types, and none accepts another's proofs.
 */
import { err, ok, type Result } from "@kd5ujc/dilf4s-core";
import { ed25519_hasher } from "@noble/curves/ed25519.js";
import { utf8ToBytes } from "@noble/hashes/utils.js";

import type { VrfError } from "./errors.js";
import { SigningKey, VerifyingKey, type KeyPair } from "./keys.js";
import { attempt } from "./internal/attempt.js";
import { bigToLE, concat, copy, equal, hex, leToBig, mod, powMod } from "./internal/bytes.js";
import {
  decodeDraft03,
  decodeStrict,
  expandSeed,
  hashToScalar,
  isSmallOrder,
  L,
  mulBase,
  mulPublic,
  mulSecret,
  P,
  Point,
  sha512Parts,
  type EdPoint,
} from "./internal/edwards.js";
import { INTERNAL } from "./internal/token.js";

export type VrfSuiteName = "ECVRF-EDWARDS25519-SHA512-TAI" | "ECVRF-EDWARDS25519-SHA512-ELL2" | "ECVRF-ED25519-SHA512-Elligator2 (draft-03)";

/** An 80-byte proof `Gamma (32) || c (16) || s (32)`; its fields are checked only by verification. */
export class VrfProof<S extends VrfSuiteName> {
  readonly #bytes: Uint8Array;
  constructor(
    _token: typeof INTERNAL,
    readonly suite: S,
    bytes: Uint8Array,
  ) {
    this.#bytes = copy(bytes);
    Object.freeze(this);
  }

  get bytes(): Uint8Array {
    return copy(this.#bytes);
  }

  toString(): string {
    return `VrfProof(${this.suite}, ${hex(this.#bytes)})`;
  }
}

/** The 64-byte output `beta`: only ever produced by proving or by a successful verification. */
export class VrfOutput<S extends VrfSuiteName> {
  readonly #bytes: Uint8Array;
  constructor(
    _token: typeof INTERNAL,
    readonly suite: S,
    bytes: Uint8Array,
  ) {
    this.#bytes = copy(bytes);
    Object.freeze(this);
  }

  get bytes(): Uint8Array {
    return copy(this.#bytes);
  }

  toString(): string {
    return `VrfOutput(${this.suite}, ${hex(this.#bytes)})`;
  }
}

/** Proving and verification for one suite. */
export interface Vrf<S extends VrfSuiteName> {
  readonly name: S;
  /** 32 bytes (a seed) for the RFC 9381 suites; 64 bytes `seed || pk` for draft-03, whose `pk` must be the seed's. */
  readonly signingKey: (bytes: Uint8Array) => Result<SigningKey<S>, VrfError>;
  /** A key that decodes under the suite's rule and is not of small order. */
  readonly verifyingKey: (bytes: Uint8Array) => Result<VerifyingKey<S>, VrfError>;
  /** 80 bytes. */
  readonly proof: (bytes: Uint8Array) => Result<VrfProof<S>, VrfError>;
  readonly publicKey: (key: SigningKey<S>) => VerifyingKey<S>;
  readonly prove: (key: SigningKey<S>, alpha: Uint8Array) => Result<{ readonly proof: VrfProof<S>; readonly output: VrfOutput<S> }, VrfError>;
  readonly verify: (key: VerifyingKey<S>, alpha: Uint8Array, proof: VrfProof<S>) => Result<VrfOutput<S>, VrfError>;
  /** Over raw encodings: a wrong length is `Malformed` (the proof's is checked first), anything else a typed rejection. */
  readonly verifyBytes: (key: Uint8Array, alpha: Uint8Array, proof: Uint8Array) => Result<VrfOutput<S>, VrfError>;
  /** A key pair from a 32-byte seed (draft-03 keys become `seed || pk`). */
  readonly fromSeed: (seed: Uint8Array) => Result<KeyPair<S>, VrfError>;
}

const malformed = (what: string, expected: number, got: number): VrfError => ({
  kind: "Malformed",
  what,
  reason: `expected ${String(expected)} bytes, got ${String(got)}`,
});

// ------------------------------------------------------------------ field arithmetic for draft-03 Elligator 2 (public data only)

const J = 486662n;
const fmod = (a: bigint): bigint => mod(a, P);
/** `a^-1`, and `0` for `0` (RFC 9380 inv0). */
const inv0 = (a: bigint): bigint => powMod(a, P - 2n, P);

/**
 * libsodium's `ge25519_from_uniform` (Cardano draft-03): bit 255 selects the sign of `x`, the rest is the Elligator 2
 * representative; the point is mapped through `y = (u - 1)/(u + 1)` and the cofactor cleared.
 */
export function fromUniformDraft03(r32: Uint8Array): EdPoint | undefined {
  if (r32.length !== 32) return undefined;
  const sign = ((r32[31] ?? 0) & 0x80) >> 7;
  const cleared = Uint8Array.from(r32);
  cleared[31] = (cleared[31] ?? 0) & 0x7f;
  const r = fmod(leToBig(cleared));
  const u0 = fmod(-J * inv0(1n + 2n * r * r));
  const w = (u: bigint): bigint => fmod(u * (u * u + J * u + 1n));
  const u = powMod(w(u0), (P - 1n) / 2n, P) === P - 1n ? fmod(-u0 - J) : u0;
  const y = fmod(u + 1n) === 0n || w(u) === 0n ? 1n : fmod((u - 1n) * inv0(u + 1n));
  const enc = bigToLE(y, 32);
  enc[31] = (enc[31] ?? 0) | (sign << 7);
  return decodeDraft03(enc)?.clearCofactor();
}

/** Cardano draft-03 hash-to-curve: `SHA-512(04 || 01 || PK || alpha)[0:32]` with bit 255 cleared. */
export function hashToCurveDraft03(pk: Uint8Array, alpha: Uint8Array): EdPoint | undefined {
  const r = sha512Parts(Uint8Array.of(0x04, 0x01), pk, alpha).slice(0, 32);
  r[31] = (r[31] ?? 0) & 0x7f;
  return fromUniformDraft03(r);
}

/** `encode_to_curve` of RFC 9380 suite `edwards25519_XMD:SHA-512_ELL2_NU_`, with noble's hasher. */
export function encodeToCurveEll2(msg: Uint8Array, dst: Uint8Array): EdPoint | undefined {
  return attempt(() => ed25519_hasher.encodeToCurve(msg, { DST: dst }));
}

// ------------------------------------------------------------------ suite profiles

interface Profile<S extends VrfSuiteName> {
  readonly name: S;
  readonly suite: number;
  readonly skLength: number;
  readonly decode: (b: Uint8Array) => EdPoint | undefined;
  readonly hashToCurve: (pk: Uint8Array, alpha: Uint8Array) => EdPoint | undefined;
  readonly challenge: (pk: Uint8Array, h: Uint8Array, gamma: Uint8Array, u: Uint8Array, v: Uint8Array) => Uint8Array;
  /** `U = s B - c Y` and `V = s H - c Gamma`, as the suite's reference evaluates them (`p` is `Y` or `Gamma`). */
  readonly equation: (p: EdPoint, q: EdPoint, c: bigint, s: bigint) => EdPoint;
  readonly outputSuffix: Uint8Array;
}

/** RFC 9381: strict decoding, `Y` in the challenge, trailing zero octets, the literal `s B - c Y`. */
function rfc9381<S extends VrfSuiteName>(name: S, suite: number, hashToCurve: Profile<S>["hashToCurve"]): Profile<S> {
  return {
    name,
    suite,
    skLength: 32,
    decode: decodeStrict,
    hashToCurve,
    challenge: (pk, h, gamma, u, v) => sha512Parts(Uint8Array.of(suite, 0x02), pk, h, gamma, u, v, Uint8Array.of(0)).slice(0, 16),
    equation: (p, q, c, s) => mulPublic(q, s).subtract(mulPublic(p, c)),
    outputSuffix: Uint8Array.of(0),
  };
}

const TAI = rfc9381("ECVRF-EDWARDS25519-SHA512-TAI", 0x03, (pk, alpha) => {
  // the first counter whose hash strictly decodes; that point times the cofactor
  for (let ctr = 0; ctr < 256; ctr++) {
    const pt = decodeStrict(sha512Parts(Uint8Array.of(0x03, 0x01), pk, alpha, Uint8Array.of(ctr, 0x00)).slice(0, 32));
    if (pt !== undefined) return pt.clearCofactor();
  }
  return undefined;
});

const ELL2_DST = concat(utf8ToBytes("ECVRF_edwards25519_XMD:SHA-512_ELL2_NU_"), Uint8Array.of(0x04));
const ELL2 = rfc9381("ECVRF-EDWARDS25519-SHA512-ELL2", 0x04, (pk, alpha) => encodeToCurveEll2(concat(pk, alpha), ELL2_DST));

/**
 * draft-03 as libsodium: canonical-`y` decoding that admits negative zero, no `Y` in the challenge, no trailing
 * octets, and `U = (-c mod L) Y + s B` (the integer `L - c`, which differs from `s B - c Y` on mixed-order points).
 */
const DRAFT03: Profile<"ECVRF-ED25519-SHA512-Elligator2 (draft-03)"> = {
  name: "ECVRF-ED25519-SHA512-Elligator2 (draft-03)",
  suite: 0x04,
  skLength: 64,
  decode: decodeDraft03,
  hashToCurve: hashToCurveDraft03,
  challenge: (_pk, h, gamma, u, v) => sha512Parts(Uint8Array.of(0x04, 0x02), h, gamma, u, v).slice(0, 16),
  equation: (p, q, c, s) => mulPublic(p, mod(L - c, L)).add(mulPublic(q, s)),
  outputSuffix: new Uint8Array(0),
};

/** Exposed for the intermediate-step tests (the libsodium helper corpus). */
export const draft03Equation = DRAFT03.equation;
export const draft03Challenge = DRAFT03.challenge;

const publicFromSeed = (seed: Uint8Array): Uint8Array => mulBase(expandSeed(seed).scalar).toBytes();

function vrf<S extends VrfSuiteName>(profile: Profile<S>): Vrf<S> {
  const { name } = profile;
  const seedOf = (key: SigningKey<S>): Uint8Array => key.bytes.subarray(0, 32);
  const output = (gamma: EdPoint): VrfOutput<S> =>
    new VrfOutput(INTERNAL, name, sha512Parts(Uint8Array.of(profile.suite, 0x03), gamma.clearCofactor().toBytes(), profile.outputSuffix));

  function decodeKey(bytes: Uint8Array): Result<EdPoint, VrfError> {
    if (bytes.length !== 32) return err(malformed(`${name} public key`, 32, bytes.length));
    const pt = profile.decode(bytes);
    if (pt === undefined) return err({ kind: "InvalidPublicKey" });
    return isSmallOrder(pt) ? err({ kind: "SmallOrderPublicKey" }) : ok(pt);
  }

  const signingKey = (bytes: Uint8Array): Result<SigningKey<S>, VrfError> => {
    if (bytes.length !== profile.skLength) return err(malformed(`${name} signing key`, profile.skLength, bytes.length));
    // draft-03 keys are libsodium's `seed || pk`: the stored public key must be the seed's
    return bytes.length === 32 || equal(publicFromSeed(bytes.subarray(0, 32)), bytes.subarray(32))
      ? ok(new SigningKey(INTERNAL, name, bytes))
      : err({ kind: "KeyMismatch" });
  };

  const publicKey = (key: SigningKey<S>): VerifyingKey<S> => new VerifyingKey(INTERNAL, name, publicFromSeed(seedOf(key)));

  const verifyBytes = (key: Uint8Array, alpha: Uint8Array, proof: Uint8Array): Result<VrfOutput<S>, VrfError> => {
    if (proof.length !== 80) return err(malformed(`${name} proof`, 80, proof.length));
    const y = decodeKey(key);
    if (!y.ok) return y;
    const gb = proof.subarray(0, 32);
    const gamma = profile.decode(gb);
    if (gamma === undefined) return err({ kind: "InvalidGamma" });
    const cb = proof.subarray(32, 48);
    const c = leToBig(cb);
    const s = leToBig(proof.subarray(48));
    if (s >= L) return err({ kind: "NonCanonicalScalar" });
    const h = profile.hashToCurve(y.value.toBytes(), alpha);
    if (h === undefined) return err({ kind: "HashToCurveFailed", reason: "no curve point" });
    const u = profile.equation(y.value, Point.BASE, c, s);
    const v = profile.equation(gamma, h, c, s);
    return equal(profile.challenge(key, h.toBytes(), gb, u.toBytes(), v.toBytes()), cb) ? ok(output(gamma)) : err({ kind: "ChallengeMismatch" });
  };

  return Object.freeze({
    name,
    signingKey,
    verifyingKey: (bytes: Uint8Array) => {
      const k = decodeKey(bytes);
      return k.ok ? ok(new VerifyingKey(INTERNAL, name, bytes)) : k;
    },
    proof: (bytes: Uint8Array) => (bytes.length === 80 ? ok(new VrfProof(INTERNAL, name, bytes)) : err(malformed(`${name} proof`, 80, bytes.length))),
    publicKey,
    prove: (key: SigningKey<S>, alpha: Uint8Array) => {
      const { scalar: x, prefix } = expandSeed(seedOf(key));
      const pk = mulBase(x).toBytes();
      const h = profile.hashToCurve(pk, alpha);
      if (h === undefined) return err<VrfError>({ kind: "HashToCurveFailed", reason: "no curve point" });
      const hb = h.toBytes();
      const gamma = mulSecret(h, x);
      const gb = gamma.toBytes();
      const k = hashToScalar(prefix, hb);
      const c = profile.challenge(pk, hb, gb, mulBase(k).toBytes(), mulSecret(h, k).toBytes());
      const s = mod(leToBig(c) * x + k, L);
      return ok({ proof: new VrfProof(INTERNAL, name, concat(gb, c, bigToLE(s, 32))), output: output(gamma) });
    },
    verify: (key: VerifyingKey<S>, alpha: Uint8Array, proof: VrfProof<S>) => verifyBytes(key.bytes, alpha, proof.bytes),
    verifyBytes,
    fromSeed: (seed: Uint8Array) => {
      if (seed.length !== 32) return err(malformed("VRF seed", 32, seed.length));
      const sk = signingKey(profile.skLength === 32 ? seed : concat(seed, publicFromSeed(seed)));
      return sk.ok ? ok({ signing: sk.value, verifying: publicKey(sk.value) }) : sk;
    },
  });
}

export const ecvrfTai: Vrf<"ECVRF-EDWARDS25519-SHA512-TAI"> = vrf(TAI);
export const ecvrfEll2: Vrf<"ECVRF-EDWARDS25519-SHA512-ELL2"> = vrf(ELL2);
export const ecvrfDraft03: Vrf<"ECVRF-ED25519-SHA512-Elligator2 (draft-03)"> = vrf(DRAFT03);
