/**
 * Java's `BigInteger.bitLength`, as the weight counts it. A refusal names an integer by this, never by its digits:
 * decimal rendering is superlinear, and nothing pays for it. Linear in the integer's size (its hexadecimal form).
 */
export const bitLength = (n: bigint): number => {
  const m = n < 0n ? -n - 1n : n;
  if (m < 0x100000000n) return 32 - Math.clz32(Number(m));
  const hex = m.toString(16);
  return (hex.length - 1) * 4 + 32 - Math.clz32(Number.parseInt(hex.charAt(0), 16));
};

/** `log2(5)` in millionths, rounded down: the bits per factor of five. */
const MICRO_BITS_PER_FIVE = 2_321_928;

/**
 * `[a, b]` when `d = 2^a 5^b`, so that `1/d` terminates (`undefined` otherwise), and the big-number divisions and
 * exponentiations it took: at most four, whatever `d`. The twos are the low zero bits; the power of five is estimated
 * from the bit length (`5^b` has `floor(b log2 5) + 1` bits, and the neighbours absorb any rounding) and checked by
 * exponentiation, never found one division per factor (14,306 of them for `5^14306`, the widest a rational's
 * denominator is).
 */
export function twosAndFives(d: bigint): readonly [readonly [number, number] | undefined, number] {
  const twos = bitLength(d & -d) - 1;
  const rest = d >> BigInt(twos);
  if (rest === 1n) return [[twos, 0], 0];
  if (rest % 5n !== 0n) return [undefined, 1];
  const estimate = Math.floor(((bitLength(rest) - 1) * 1_000_000) / MICRO_BITS_PER_FIVE);
  // b log2 5 lies in [bits - 1, bits), under half a factor wide, so b is the estimate's ceiling but for rounding
  return [estimate + 1, estimate, estimate - 1].reduce<readonly [readonly [number, number] | undefined, number]>(
    ([found, steps], b) => (found !== undefined || b <= 0 ? [found, steps] : [5n ** BigInt(b) === rest ? [twos, b] : undefined, steps + 1]),
    [undefined, 1],
  );
}
