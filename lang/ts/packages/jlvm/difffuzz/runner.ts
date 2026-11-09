/**
 * The TypeScript runner of the JLVM differential fuzzing harness (`tools/difffuzz/README.md`), run with node on the
 * built packages: `node --experimental-strip-types packages/jlvm/difffuzz/runner.ts [cases.jsonl]`. It reads cases as
 * JSON lines, from the file or standard input, and writes one record line per case to standard output, so a driver
 * can stream to it. It is not part of the published package.
 *
 * A case is a `jlvm_v1.json` input case, every text as the hex of its UTF-8: `name`; `program` (surface text) or
 * `form` (a canonical form's CBOR, read by `decode`); `data`; `env`, `[name, text]` pairs; `built`, `[name,
 * built-value]` pairs; `gasLimit`. It runs as the reference's `JlvmVectors.run` runs a vector case: the program, then
 * the data, then each binding in order; the first that fails is the outcome, with no gas. Then the bindings built with
 * shared parts are added, and the program is evaluated with the case's limit (2^24 when absent).
 *
 * A record holds what the vectors pin, and what is as normative: `program`, the canonical CBOR of the program read
 * (its identity, jlvm.md section 1); `outcome`, the vectors' `jlvm-outcome`; `available`, an `OutOfGas` error's
 * limit; `gasUsed`; and `warnings`, each path preview as `[head, length]`. A line that cannot be read is a fault: the runner says why and exits non-zero.
 */
import { createReadStream } from "node:fs";
import { createInterface } from "node:readline";

import { ArrayValue, cbor, decodeUtf8, fromHex, json, MapValue, models, toHex, type Result, type TypeMismatch, type Value } from "@kd5ujc/dilf4s-core";
import { Bindings, decode, DEFAULT_GAS_LIMIT, evaluate, parse, parseValue, siteOf, type JlvmError, type Outcome } from "@kd5ujc/dilf4s-jlvm";

type Outcomes = models.jlvmVectors.JlvmOutcome;
type Cause = models.jlvmVectors.JlvmCause;

interface Case {
  readonly name: string;
  readonly program: { readonly surface: string } | { readonly form: Uint8Array };
  readonly data: string;
  readonly env: readonly (readonly [string, string])[];
  readonly built: readonly (readonly [string, models.jlvmVectors.BuiltValue])[];
  readonly gasLimit: bigint | undefined;
}

const fault = <T>(error: string): Result<T, string> => ({ ok: false, error });
const ok = <T>(value: T): Result<T, string> => ({ ok: true, value });

function text(v: Value | undefined, what: string): Result<string, string> {
  return v?.kind === "text" ? ok(v.value) : fault(`${what} is not text`);
}

/** Hex of UTF-8 back to text: ill-formed UTF-8 is refused, never replaced. */
function utf8(v: Value | undefined, what: string): Result<string, string> {
  const hex = text(v, what);
  if (!hex.ok) return hex;
  const bytes = fromHex(hex.value);
  if (!bytes.ok) return fault(`${what}: not hex`);
  const s = decodeUtf8(bytes.value);
  return s === undefined ? fault(`${what}: ill-formed UTF-8`) : ok(s);
}

/** `[name, x]` pairs, in order. */
function pairs(v: Value | undefined, what: string): Result<(readonly [string, Value])[], string> {
  if (v === undefined) return ok([]);
  if (v.kind !== "array") return fault(`${what} is not an array`);
  const out: (readonly [string, Value])[] = [];
  for (const p of v.items) {
    const [k, x] = p.kind === "array" && p.items.length === 2 ? p.items : [];
    if (k === undefined || x === undefined) return fault(`${what}: a pair is an array of two`);
    const name = utf8(k, what);
    if (!name.ok) return name;
    out.push([name.value, x]);
  }
  return ok(out);
}

function read(line: string): Result<Case, string> {
  const parsed = json.parse(line);
  if (!parsed.ok || parsed.value.kind !== "map") return fault("a case is a canonical JSON map");
  const m = parsed.value;
  const name = text(m.get("name"), "name");
  if (!name.ok) return name;
  const [p, f] = [m.get("program"), m.get("form")];
  let program: Case["program"];
  if (p !== undefined && f === undefined) {
    const surface = utf8(p, "program");
    if (!surface.ok) return surface;
    program = { surface: surface.value };
  } else if (p === undefined && f !== undefined) {
    const hex = text(f, "form");
    const bytes = hex.ok ? fromHex(hex.value) : undefined;
    if (bytes?.ok !== true) return fault("form is not hex");
    program = { form: bytes.value };
  } else return fault(`${name.value}: exactly one of program and form`);
  const data = utf8(m.get("data"), "data");
  if (!data.ok) return data;
  const envPairs = pairs(m.get("env"), "env");
  if (!envPairs.ok) return envPairs;
  const env: (readonly [string, string])[] = [];
  for (const [k, v] of envPairs.value) {
    const t = utf8(v, "env");
    if (!t.ok) return t;
    env.push([k, t.value]);
  }
  const builtPairs = pairs(m.get("built"), "built");
  if (!builtPairs.ok) return builtPairs;
  const built: (readonly [string, models.jlvmVectors.BuiltValue])[] = [];
  for (const [k, v] of builtPairs.value) {
    const b = models.jlvmVectors.BuiltValue.decode(v);
    if (!b.ok) return fault(`built ${k}: not a built-value`);
    built.push([k, b.value]);
  }
  const g = m.get("gasLimit");
  if (g !== undefined && g.kind !== "int") return fault("gasLimit is not an integer");
  return ok({ name: name.value, program, data: data.value, env, built, gasLimit: g?.value });
}

/** Every text `"$"` in `template` replaced by `inner`, which is shared, never copied. */
function substitute(template: Value, inner: Value): Result<Value, string> {
  switch (template.kind) {
    case "text":
      return ok(template.value === "$" ? inner : template);
    case "array": {
      const items: Value[] = [];
      for (const x of template.items) {
        const s = substitute(x, inner);
        if (!s.ok) return s;
        items.push(s.value);
      }
      const a = ArrayValue.of(items);
      return a.ok ? ok(a.value) : fault("a template array");
    }
    case "map": {
      const entries: (readonly [string, Value])[] = [];
      for (const [k, x] of template.entries()) {
        const s = substitute(x, inner);
        if (!s.ok) return s;
        entries.push([k, s.value]);
      }
      const m = MapValue.of(entries);
      return m.ok ? ok(m.value) : fault("a template map");
    }
    case "null":
    case "bool":
    case "int":
    case "rational":
    case "bytes":
      return ok(template);
  }
}

const cborValue = (hex: string): Result<Value, string> => {
  const bytes = fromHex(hex);
  const v = bytes.ok ? cbor.decode(bytes.value) : undefined;
  return v?.ok === true ? ok(v.value) : fault("not canonical CBOR hex");
};

/** A `built-value`, built with sharing, as `test/vectors.ts` builds one. A recipe that does not build is a fault. */
function built(b: models.jlvmVectors.BuiltValue): Result<Value, string> {
  const leaf = cborValue(b.leaf);
  if (!leaf.ok) return leaf;
  let v = leaf.value;
  for (const step of b.steps ?? []) {
    if ("copies" in step) {
      const a = ArrayValue.of(Array.from({ length: Number(step.copies) }, () => v));
      if (!a.ok) return fault("copies");
      v = a.value;
    } else {
      const t = cborValue(step.template);
      if (!t.ok) return t;
      for (let i = 0n; i < step.times; i++) {
        const s = substitute(t.value, v);
        if (!s.ok) return s;
        v = s.value;
      }
    }
  }
  return ok(v);
}

/** A Crypto error's cause as the vectors record it, typed by the vectors' schema: its family and that family's kind. */
function cause(e: JlvmError): Cause | undefined {
  if (e.kind !== "Crypto" || e.cause === undefined) return undefined;
  const c = e.cause;
  switch (c.kind) {
    case "Signing":
      return { signing: c.error.kind };
    case "Sigma":
      return { sigma: c.error.kind };
    case "Zk":
      return { zk: c.error.kind };
    case "Groth16":
      return { groth16: c.error.kind };
    case "Vrf":
      return { vrf: c.error.kind };
    case "Hash":
      return { hash: c.error.kind };
    case "Merkle":
      return { merkle: c.error.kind };
    case "Trie":
      return { trie: c.error.kind };
    case "Poseidon":
      return { poseidon: c.error.kind };
    case "PoseidonMerkle":
      return { poseidonMerkle: c.error.kind };
  }
}

function errorOutcome(e: JlvmError): Outcomes {
  const site = siteOf(e);
  const op = site === undefined || site === "<not a JLVM error>" ? undefined : site;
  return { error: e.kind, op, cause: cause(e) };
}

const failed = (e: JlvmError): Outcome => ({ gasUsed: 0, result: { ok: false, error: e }, warnings: [] });

/** A parse failure as an outcome, with no gas; `TypeMismatch` (an argument that is not text) is the runner's fault. */
const refused = (form: string | undefined, e: JlvmError | TypeMismatch): Result<{ readonly form: string | undefined; readonly outcome: Outcome }, string> =>
  e.kind === "TypeMismatch" ? fault(`not text: ${e.reason}`) : ok({ form, outcome: failed(e) });

/** The program read, its canonical form, and the outcome; a fault for a broken recipe or an argument out of range. */
function run(c: Case): Result<{ readonly form: string | undefined; readonly outcome: Outcome }, string> {
  const read = "surface" in c.program ? parse(c.program.surface) : decode(c.program.form);
  if (!read.ok) return refused(undefined, read.error);
  const program = read.value;
  const encoded = program.encode();
  const form = encoded.ok ? toHex(encoded.value) : undefined;
  const hex = form?.ok === true ? form.value : undefined;
  const data = parseValue(c.data);
  if (!data.ok) return refused(hex, data.error);
  const env: (readonly [string, Value])[] = [];
  for (const [k, v] of c.env) {
    const x = parseValue(v);
    if (!x.ok) return refused(hex, x.error);
    env.push([k, x.value]);
  }
  for (const [k, b] of c.built) {
    const v = built(b);
    if (!v.ok) return fault(`${c.name}: binding ${k}: ${v.error}`);
    env.push([k, v.value]);
  }
  const bindings = Bindings.of(env);
  if (!bindings.ok) return fault(`${c.name}: the bindings`);
  const o = evaluate(program, data.value, { env: bindings.value, gasLimit: c.gasLimit === undefined ? DEFAULT_GAS_LIMIT : Number(c.gasLimit) });
  return o.ok ? ok({ form: hex, outcome: o.value }) : fault(`${c.name}: ${o.error.reason}`);
}

function record(c: Case): Result<string, string> {
  const r = run(c);
  if (!r.ok) return r;
  const o = r.value.outcome;
  let outcome: Outcomes;
  if (o.result.ok) {
    const bytes = cbor.encode(o.result.value);
    const hex = bytes.ok ? toHex(bytes.value) : undefined;
    if (hex?.ok !== true) return fault(`${c.name}: a result with no encoding`);
    outcome = { cbor: hex.value };
  } else outcome = errorOutcome(o.result.error);
  const warnings = o.warnings.map((w) => [w.path.head, w.path.length] as const);
  const available = !o.result.ok && o.result.error.kind === "OutOfGas" ? o.result.error.available : undefined;
  return ok(JSON.stringify({ name: c.name, program: r.value.form, outcome, available, gasUsed: o.gasUsed, warnings }));
}

async function main(): Promise<number> {
  const path = process.argv[2];
  const input = path === undefined ? process.stdin : createReadStream(path, "utf8");
  const lines = createInterface({ input, crlfDelay: Infinity });
  for await (const line of lines) {
    if (line === "") continue;
    const c = read(line);
    const r = c.ok ? record(c.value) : c;
    if (!r.ok) {
      process.stderr.write(`runner: ${r.error}\n`);
      lines.close();
      return 1;
    }
    process.stdout.write(`${r.value}\n`);
  }
  return 0;
}

process.exitCode = await main();
