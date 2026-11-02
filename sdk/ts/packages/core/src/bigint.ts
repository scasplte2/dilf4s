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
