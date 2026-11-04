/**
 * The public `cbor` namespace: everything `cbor.ts` exports except its streaming internals (`Absorber`, `Feeding`,
 * `STAGED_BYTES`, `encodeIntoWithin`), which feed a hash this package creates. A test checks the two lists agree.
 */
export { decode, encode, encodedLengthWithin, encodeWithin, magnitudeBytes, MAX_ENCODED_BYTES, MAX_RATIONAL_BITS } from "./cbor.js";
