/**
 * The server's bounds. Every input is an agent's, so each is bounded before anything reads it, and every output is
 * bounded before it is built: a call costs at most what these allow, whatever it holds.
 */
import { DEFAULT_GAS_LIMIT } from "@kd5ujc/dilf4s-jlvm";

/** The most a text input (a program, data, a value) may measure (`measure.ts`): 1 MiB. */
export const MAX_TEXT_BYTES = 2 ** 20;

/** The most the host bindings of one evaluation may measure together, their names included: 1 MiB. */
export const MAX_BINDINGS_BYTES = 2 ** 20;

/** The most host bindings one evaluation takes. */
export const MAX_BINDINGS = 256;

/**
 * The longest rendering a result carries: a value's canonical CBOR (in bytes, before hex), or its surface JSON (in
 * UTF-8 bytes), 1 MiB each. A longer one is omitted, and said to be; its digest is still given.
 */
export const MAX_OUTPUT_BYTES = 2 ** 20;

/**
 * The gas cap of a server given none: the spec's default limit, 2^24 (jlvm.md section 7, Ethereum's per-transaction
 * cap). A call asking for more is refused, never clamped, since the limit is part of what the call states.
 */
export const DEFAULT_GAS_CAP = DEFAULT_GAS_LIMIT;

/**
 * The highest gas cap a server can be given: 2^32, the highest limit any conformance case runs to completion at, far
 * below `MAX_GAS_LIMIT` (2^53 - 1), the largest limit any implementation accepts.
 */
export const GAS_CAP_CEILING = 2 ** 32;
