/**
 * Every way a parse or an evaluation can fail (`JlvmError.scala`). The error, its kinds and its site are generated from
 * `shared/schemas/jlvm-errors.cddl`; evaluation failures are reported with the gas they used.
 */
import { err, type Err, type models } from "@kd5ujc/dilf4s-core";
import { isResult, readGuarded } from "@kd5ujc/dilf4s-core/internal";

import { opFromWire, type CryptoOp } from "./op.js";

/** A parse or evaluation failure (generated). */
export type JlvmError = models.jlvmErrors.JlvmError;

/** The kind of a {@link JlvmError} (generated). */
export type JlvmErrorKind = models.jlvmErrors.JlvmErrorKind;

/** Where an operation-level error arose: an operation's own wire name, or `var` (generated). */
export type JlvmSite = models.jlvmErrors.JlvmSite;

/**
 * A cryptographic or proof operation (jlvm.md section 6) that this package does not run yet (`sigma_verify`,
 * `schnorr_verify` and `groth16_verify`, whose primitives the TypeScript SDK lacks). The evaluation reached it: its
 * base, its arguments and its size charge were all paid, in the order section 7 gives, and it stopped there, where an
 * implementation calls the operation. It is not a language error, and no conforming evaluation ends with it.
 */
export interface Unimplemented {
  readonly kind: "Unimplemented";
  /** The operation reached. */
  readonly op: CryptoOp;
}

/** What an evaluation can end with instead of a value: a language error, or an operation not yet implemented. */
export type EvalError = JlvmError | Unimplemented;

export const malformed = (message: string): JlvmError => ({ kind: "Malformed", message });
export const typeError = (op: JlvmSite, message: string): JlvmError => ({ kind: "Type", op, message });
export const arity = (op: JlvmSite, message: string): JlvmError => ({ kind: "Arity", op, message });
export const arithmetic = (op: JlvmSite, message: string): JlvmError => ({ kind: "Arithmetic", op, message });
export const domain = (op: JlvmSite, message: string): JlvmError => ({ kind: "Domain", op, message });
export const limit = (message: string): JlvmError => ({ kind: "Limit", message });

/** A failed result holding `e`. */
export const fail = (e: JlvmError): Err<JlvmError> => err(e);

/** What {@link siteOf} gives for something that is not a JLVM error: fixed, so the input is never echoed. */
export const NOT_A_JLVM_ERROR = "<not a JLVM error>";

/**
 * Where an operation-level error arose: its normative `op`; `undefined` for `Malformed`, `Limit` and `OutOfGas`.
 * What a caller passes past the types is read once per field, guarded, and something that is not a JLVM error (not an
 * object, a revoked Proxy, one revoked while it is read, an unknown kind, or an `op` that is no site) is
 * {@link NOT_A_JLVM_ERROR}. An exception the caller's own code raises (a getter, a trap) propagates.
 */
export function siteOf(e: JlvmError): JlvmSite | undefined | typeof NOT_A_JLVM_ERROR {
  const given: unknown = e;
  if (!isResult(given)) return NOT_A_JLVM_ERROR;
  const kind: unknown = readGuarded(given, "kind");
  switch (kind) {
    case "Type":
    case "Arity":
    case "Arithmetic":
    case "Domain":
    case "Crypto": {
      const op: unknown = readGuarded(given, "op");
      return isSite(op) ? op : NOT_A_JLVM_ERROR;
    }
    case "Malformed":
    case "Limit":
    case "OutOfGas":
      return undefined;
    default:
      return NOT_A_JLVM_ERROR;
  }
}

/** Whether `x` is a site: an operation's own wire name, or `var`. */
function isSite(x: unknown): x is JlvmSite {
  return x === "var" || (typeof x === "string" && opFromWire(x) !== undefined);
}
