/**
 * `shared/vectors/zk_jlvm_v1.json` (docs/specs/zk-jlvm.md), as the reference's `ZkJlvmVectorsSuite` and Rust's
 * `zk_jlvm.rs` run it: each call's statement, computed here from its texts with this package's JLVM, core's value
 * commitment and tagged hashing, is the file's, and its public values are that statement's canonical CBOR; each real SP1
 * Groth16 proof the file pins verifies under the guest's key, by the proofs package's verifier and through the JLVM's
 * `groth16_verify`, and claims nothing else (other public values, another key).
 */
import { cbor, DomainTag, models, taggedHash, Value, valueCommitment } from "@kd5ujc/dilf4s-core";
import { zk } from "@kd5ujc/dilf4s-proofs";
import { describe, expect, it } from "vitest";

import { Bindings, evaluate, parse, parseValue, siteOf, type JlvmError, VERSION, LATEST_SCHEDULE } from "../src/index.js";
import { bytes, expectOk, hex, loadVectors } from "./vectors.js";

type Call = models.zkJlvmVectors.ZkJlvmCall;
type StatementHex = models.zkJlvmVectors.ZkJlvmStatementHex;

const file = loadVectors("zk_jlvm_v1.json", models.zkJlvmVectors.ZkJlvmVectors);

const tagged = (tag: string, message: Uint8Array): Uint8Array => expectOk(taggedHash("SHA-256", expectOk(DomainTag.of(tag), tag), message), tag);
const commit = (v: Value): Uint8Array => expectOk(valueCommitment(v), "a value commitment");
const concat = (parts: readonly Uint8Array[]): Uint8Array => {
  const out = new Uint8Array(parts.reduce((n, p) => n + p.length, 0));
  parts.reduce((at, p) => (out.set(p, at), at + p.length), 0);
  return out;
};

/** The error record of the vectors (`jlvm-vectors.cddl`): the kind, the site and the cause's family and kind. */
function record(e: JlvmError): models.jlvmVectors.JlvmError {
  const op = siteOf(e);
  const cause = e.kind === "Crypto" && e.cause !== undefined ? { [e.cause.kind.charAt(0).toLowerCase() + e.cause.kind.slice(1)]: e.cause.error.kind } : undefined;
  return { error: e.kind, ...(op === undefined ? {} : { op }), ...(cause === undefined ? {} : { cause }) } as models.jlvmVectors.JlvmError;
}

/** The UTF-8 byte order of two names. */
function byUtf8(a: string, b: string): number {
  const [x, y] = [new TextEncoder().encode(a), new TextEncoder().encode(b)];
  for (let i = 0; i < Math.min(x.length, y.length); i++) if (x[i] !== y[i]) return (x[i] ?? 0) - (y[i] ?? 0);
  return x.length - y.length;
}

/** The statement of `call`, from the spec's definitions alone, as the file writes it (digests in hex). */
function statement(call: Call): StatementHex {
  const program = expectOk(parse(call.program), "the program");
  const data = expectOk(parseValue(call.data), "the data");
  const env = [...(call.env ?? [])].map(([k, v]) => [k, expectOk(parseValue(v), k)] as const);
  const o = expectOk(evaluate(program, data, { env: expectOk(Bindings.of(env), "the bindings"), gasLimit: Number(call.gasLimit) }), "evaluates");
  const each = [...env].sort(([a], [b]) => byUtf8(a, b)).map(([k, v]) => tagged("dilf4s/zk/jlvm/binding/v1", concat([expectOk(cbor.encode(expectOk(Value.text(k), k)), k), commit(v)])));
  return {
    language: BigInt(VERSION),
    schedule: BigInt(LATEST_SCHEDULE.version),
    program: hex(expectOk(program.id(), "the program's identity")),
    input: hex(tagged("dilf4s/zk/jlvm/input/v1", commit(data))),
    bindings: hex(tagged("dilf4s/zk/jlvm/bindings/v1", concat(each))),
    gasLimit: call.gasLimit,
    gasUsed: BigInt(o.gasUsed),
    outcome: o.result.ok ? { result: hex(tagged("dilf4s/zk/jlvm/result/v1", commit(o.result.value))) } : record(o.result.error),
  };
}

/** The statement's hex form, from its canonical value. */
function asHex(s: models.zkJlvm.ZkJlvmStatement): StatementHex {
  const { language, schedule, gasLimit, gasUsed, outcome } = s;
  return { language, schedule, program: hex(s.program), input: hex(s.input), bindings: hex(s.bindings), gasLimit, gasUsed, outcome: "result" in outcome ? { result: hex(outcome.result) } : outcome };
}

describe("zk_jlvm_v1.json", { timeout: 120_000 }, () => {
  it("each statement is its call's, and the public values are its canonical CBOR", () => {
    expect(file.calls.length, "zk_jlvm_v1.json has calls").toBeGreaterThan(0);
    for (const c of file.calls) {
      expect(statement(c), `${c.name}: the statement`).toEqual(c.statement);
      const decoded = expectOk(models.zkJlvm.ZkJlvmStatement.decode(expectOk(cbor.decode(bytes(c.publicValues)), "canonical CBOR")), "a statement");
      expect(asHex(decoded), `${c.name}: the public values decode to the statement`).toEqual(c.statement);
      expect(hex(expectOk(cbor.encode(expectOk(models.zkJlvm.ZkJlvmStatement.encode(decoded), "encodes")), "CBOR")), `${c.name}: canonical, once`).toBe(c.publicValues);
    }
  });

  const run = (vkey: string, publicValues: string, proof: string): string => {
    const program = expectOk(parse(`{"groth16_verify": ["0x${vkey}", "0x${publicValues}", "0x${proof}"]}`), "the program");
    const o = expectOk(evaluate(program, Value.null), "evaluates");
    return o.result.ok ? hex(expectOk(cbor.encode(o.result.value), "encodes")) : o.result.error.kind;
  };
  const [TRUE, FALSE] = ["f5", "f4"];

  it("each pinned proof verifies under the guest's key, by the verifier and through groth16_verify, and only for its public values", () => {
    const proved = file.calls.flatMap((c) => (c.proofBytes === undefined ? [] : [[c, c.proofBytes] as const]));
    expect(proved.length, 'zk_jlvm_v1.json pins no Groth16 proof: prove one on euler (sdk/rust/zk/README.md, "On euler")').toBeGreaterThan(0);
    expect(file.programVKey).toBe("00648a8f144b8ec371cd9846461c43d262432438fb4b1e39beafa810be3e850b");
    for (const [c, proof] of proved) {
      expect(zk.groth16.sp1.verify(bytes(file.programVKey), bytes(c.publicValues), bytes(proof)), `${c.name}: the verifier`).toEqual({ ok: true, value: undefined });
      expect(run(file.programVKey, c.publicValues, proof), `${c.name}: groth16_verify`).toBe(TRUE);
      const flipped = bytes(c.publicValues);
      flipped[flipped.length - 1] = (flipped[flipped.length - 1] ?? 0) ^ 1;
      expect(run(file.programVKey, hex(flipped), proof), `${c.name}: not other bytes`).toBe(FALSE);
      const key = bytes(file.programVKey);
      key[31] = (key[31] ?? 0) ^ 1;
      expect(run(hex(key), c.publicValues, proof), `${c.name}: not under another key`).toBe(FALSE);
    }
  });
});
