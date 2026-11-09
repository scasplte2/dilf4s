/**
 * `@kd5ujc/dilf4s-core/internal`: the helpers this SDK's other packages share with core, for checking caller input once
 * at their own public boundaries and then working on plain, owned data. This entry point exists for the sibling
 * `@kd5ujc/dilf4s-*` packages alone: it is not a supported API for any other caller, it may change or go away in any
 * release without notice, and nothing here checks its arguments (the `...Of` forms are the unchecked counterparts of
 * the public functions of the same name).
 */
// the boundary for caller byte arguments: proofs and signing take their bytes through these alone (a test scans for it)
export { bytesLength, copyBytes, isBytes, isLocalBuffer, isStableBuffer, ownAny, ownBytes, ownCopy, ownCopyAny, ownCopyPrefix, UNBOUNDED } from "./bytes.js";
export { bitLengthOf } from "./bigint.js";
export { blockSizeOf, digestOf, hashPartsOf, isAlgorithm, isDomainTag, MAX_OUTPUT_SIZE, outputSizeOf, startHashOf, typeMismatch, type RawHash } from "./hash.js";
export { isArrayGuarded, isRevokedProxy, readGuarded, REVOKED } from "./guard.js";
export { toHexOf } from "./hex.js";
export { mint, type Mint } from "./mint.js";
// the JLVM's operations read a byte string they own without copying it: a fixed-width argument charged per argument,
// not by its size, is measured before signing or proofs copy it, so a longer one is never copied (jlvm.md section 7)
export { bytesView } from "./value.js";
// the JLVM works on core's values alone: it builds its object literals from keys checked once at compilation
// (`MapShape`), and lists a map's keys and values in code-point order of the keys (`codePointKeys`, `codePointValues`:
// the order is found in time linear in the map's weight); `treeStats` and `unsharedNodes` read a map's
// tree, for its tests alone. None of them mints a value from anything but values, reads a map core did not mint, or
// hands out a map's own entries or key bytes
export { codePointKeys, codePointValues, MapShape, treeStats, unsharedNodes, type TreeStats } from "./value.js";
export { close, isArrayLength, isResult, NOT_A_RESULT, NOT_ITERABLE, open, openWith, readStep, walksAsArray, type Reader, type Step } from "./iterator.js";
// the JLVM measures and orders the text it holds, and orders encodings, as core does: one form of each, never a copy
export { compareBytes, compareCodePoints, utf8LengthOf } from "./utf8.js";
