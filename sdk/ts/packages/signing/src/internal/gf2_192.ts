/**
 * GF(2^192) modulo the pentanomial `x^192 + x^7 + x^2 + x + 1` (`docs/specs/sigma.md` section 1), in which a threshold
 * node splits its challenge among its children: Rust's `sigma/gf2_192.rs`. Bit `i` is the coefficient of `x^i`; the
 * 24-byte encoding is little-endian, so byte `j` holds bits `8j .. 8j+7` (Ergo's `GF2_192`).
 *
 * An element is six 32-bit words where Rust has three 64-bit ones: JavaScript's bitwise operators work on 32 bits, and
 * a `bigint` would allocate at every shift. The verifier only multiplies by a byte ({@link mulByte}), so a threshold of
 * `n` children over `n - k + 1` coefficients costs `n (n - k + 1)` byte multiplies of 8 steps each.
 */

/** An element: six 32-bit words, least significant first, each in `[0, 2^32)`. */
export type Gf2_192 = readonly [number, number, number, number, number, number];

/** The bytes of an element's encoding. */
export const BYTES = 24;

/** `x^7 + x^2 + x + 1`: `x^192` reduced. */
const PENTANOMIAL = 0x87;

export const ZERO: Gf2_192 = Object.freeze([0, 0, 0, 0, 0, 0] as const);

/** The 32-bit word of `bytes` from `at`, little-endian. */
const word = (bytes: Uint8Array, at: number): number =>
  ((bytes[at] ?? 0) | ((bytes[at + 1] ?? 0) << 8) | ((bytes[at + 2] ?? 0) << 16) | ((bytes[at + 3] ?? 0) << 24)) >>> 0;

/** The element encoded by the 24 little-endian bytes of `bytes` (which has exactly 24). */
export function fromBytes(bytes: Uint8Array): Gf2_192 {
  return [word(bytes, 0), word(bytes, 4), word(bytes, 8), word(bytes, 12), word(bytes, 16), word(bytes, 20)];
}

/** The 24-byte little-endian encoding. */
export function toBytes(e: Gf2_192): Uint8Array {
  const out = new Uint8Array(BYTES);
  e.forEach((w, i) => {
    out[4 * i] = w & 0xff;
    out[4 * i + 1] = (w >>> 8) & 0xff;
    out[4 * i + 2] = (w >>> 16) & 0xff;
    out[4 * i + 3] = w >>> 24;
  });
  return out;
}

/**
 * The element with value `b` (a byte): a point at which threshold children's challenges are evaluated, as a wide
 * element (evaluation itself multiplies by the byte with {@link mulByte}).
 */
export const fromByte = (b: number): Gf2_192 => [b & 0xff, 0, 0, 0, 0, 0];

/** Addition, which is subtraction: XOR. */
export function add([a0, a1, a2, a3, a4, a5]: Gf2_192, [b0, b1, b2, b3, b4, b5]: Gf2_192): Gf2_192 {
  return [(a0 ^ b0) >>> 0, (a1 ^ b1) >>> 0, (a2 ^ b2) >>> 0, (a3 ^ b3) >>> 0, (a4 ^ b4) >>> 0, (a5 ^ b5) >>> 0];
}

/** Multiplication by `x`: shift left one bit, folding `x^192` back in as `x^7 + x^2 + x + 1` by mask, not by branch. */
function timesX([w0, w1, w2, w3, w4, w5]: Gf2_192): Gf2_192 {
  const carry = w5 >>> 31;
  return [
    ((w0 << 1) ^ (-carry & PENTANOMIAL)) >>> 0,
    ((w1 << 1) | (w0 >>> 31)) >>> 0,
    ((w2 << 1) | (w1 >>> 31)) >>> 0,
    ((w3 << 1) | (w2 >>> 31)) >>> 0,
    ((w4 << 1) | (w3 >>> 31)) >>> 0,
    ((w5 << 1) | (w4 >>> 31)) >>> 0,
  ];
}

/** Bit `i` (`0..192`). */
export const bit = (e: Gf2_192, i: number): boolean => (((e[Math.floor(i / 32)] ?? 0) >>> (i % 32)) & 1) === 1;

/** The bits of `e` where `mask` has ones: an all-ones mask (`-1`) keeps the element, a zero mask clears it. */
function masked([w0, w1, w2, w3, w4, w5]: Gf2_192, mask: number): Gf2_192 {
  return [(w0 & mask) >>> 0, (w1 & mask) >>> 0, (w2 & mask) >>> 0, (w3 & mask) >>> 0, (w4 & mask) >>> 0, (w5 & mask) >>> 0];
}

/** The bit positions of a general multiply, highest first. */
const ALL_BITS: readonly number[] = Object.freeze(Array.from({ length: 192 }, (_, i) => 191 - i));

/**
 * Shift-and-add over the bits of `b`, from the highest: each step multiplies the accumulator by `x`. The verifier only
 * ever multiplies by a byte ({@link mulByte}); this is the reference that path is tested against.
 */
export const mul = (a: Gf2_192, b: Gf2_192): Gf2_192 => ALL_BITS.reduce((acc, i) => add(timesX(acc), masked(a, bit(b, i) ? -1 : 0)), ZERO);

/**
 * The same shift-and-add over only the 8 bits of `b`: the general multiply's first 184 steps shift a zero
 * accumulator, so skipping them changes nothing. Always 8 steps, and each bit selects `a` by mask, not by branch, as in
 * Rust and Scala (where the prover multiplies values derived from its secrets). JavaScript gives no constant-time
 * guarantee, and the verifier multiplies only public data: challenges and the proof's coefficients.
 *
 * The steps of {@link timesX} and {@link masked} on six local words, allocating only the result: the same fold over
 * element tuples allocates three per step and takes 15 times as long (the 65,025 byte multiplies of a 255-child
 * threshold take about 25 ms here, against 380 ms). The tests check it against {@link mul} at every byte.
 */
export function mulByte(a: Gf2_192, b: number): Gf2_192 {
  const [a0, a1, a2, a3, a4, a5] = a;
  let [r0, r1, r2, r3, r4, r5] = ZERO;
  for (let i = 7; i >= 0; i--) {
    const mask = -((b >>> i) & 1);
    // times x, each word from the old value of the word below it, then a masked by bit i
    const carry = r5 >>> 31;
    r5 = ((r5 << 1) | (r4 >>> 31)) ^ (a5 & mask);
    r4 = ((r4 << 1) | (r3 >>> 31)) ^ (a4 & mask);
    r3 = ((r3 << 1) | (r2 >>> 31)) ^ (a3 & mask);
    r2 = ((r2 << 1) | (r1 >>> 31)) ^ (a2 & mask);
    r1 = ((r1 << 1) | (r0 >>> 31)) ^ (a1 & mask);
    r0 = (r0 << 1) ^ (-carry & PENTANOMIAL) ^ (a0 & mask);
  }
  return [r0 >>> 0, r1 >>> 0, r2 >>> 0, r3 >>> 0, r4 >>> 0, r5 >>> 0];
}

/** The polynomial with these coefficients (lowest degree first) at the point `x` (a byte), by Horner's rule. */
export const evaluate = (coefficients: readonly Gf2_192[], x: number): Gf2_192 => coefficients.reduceRight((acc, c) => add(mulByte(acc, x), c), ZERO);
