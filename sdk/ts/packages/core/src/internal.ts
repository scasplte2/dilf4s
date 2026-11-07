/**
 * `@kd5ujc/dilf4s-core/internal`: the helpers this SDK's other packages share with core, for checking caller input once
 * at their own public boundaries and then working on plain, owned data. Not part of the public API: it may change
 * with any release, and nothing here checks its arguments (the `...Of` forms are the unchecked counterparts of the
 * public functions of the same name).
 */
export { bytesLength, copyBytes, isBytes, isLocalBuffer, plainBytes, takeBytes } from "./bytes.js";
export { bitLengthOf } from "./bigint.js";
export { blockSizeOf, digestOf, hashPartsOf, isAlgorithm, isDomainTag, outputSizeOf, startHashOf, typeMismatch, type RawHash } from "./hash.js";
export { toHexOf } from "./hex.js";
export { close, isResult, NOT_A_RESULT, NOT_ITERABLE, open, openWith, walksAsArray, type Reader } from "./iterator.js";
