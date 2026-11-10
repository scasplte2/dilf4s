# JLVM differential fuzzing

Three implementations run every JLVM contract: Scala (the reference and primary node backend), Rust (the second node
backend and the SP1 guest) and TypeScript. If two node backends disagree on a call's outcome, error or gas, consensus
splits. This harness generates cases from a seed, runs each one on all three, and compares the records exactly. When
two records differ it shrinks the case, keeping the same mismatch, and writes it out.

```
tools/difffuzz/run.sh --seed 42 --cases 20000
```

This builds the Rust runner (release) and the TypeScript packages, compiles the Scala driver, and runs it from the
repository root. It needs `cargo`, `npm`, `sbt` and Java 21. The run prints its seed and a coverage table. It exits 0
when no case mismatched. Otherwise it exits 1 and leaves the shrunk cases in `target/difffuzz/findings.jsonl`.

## What is compared

Each runner turns a case into one record. A record holds what the vectors pin (`docs/specs/jlvm.md` section 3,
`conformance.md`), and two fields that are just as normative:

| Field | What it is |
|---|---|
| `program` | The canonical CBOR of the program the text parsed to, or the form decoded to: its identity (jlvm.md section 1). It is absent when parsing failed. |
| `outcome` | `jlvm-vectors.cddl`'s `jlvm-outcome`: a value's canonical CBOR, or an error's kind, its `op` for `Type`, `Arity`, `Arithmetic`, `Domain` and `Crypto`, and a `Crypto` error's `cause` (`{family: kind}`). |
| `available` | An `OutOfGas` error's `available`, which is the limit (section 3). The vectors leave it out. |
| `gasUsed` | The gas used. Out of gas, this is the whole limit. |
| `warnings` | Each `AmbiguousPath` warning's preview, `[head, length]`, in the order recorded. The vectors pin the warning kinds and complete preview contents, including the truncation flag derived from these fields. |

Messages and `OutOfGas.required` are informative, so they are never compared. The summary's tables count, for each
operation at the root, the values, `false`s, its own refusals and other errors; and for each cryptographic operation,
`true`, `false`, other values, and `Crypto` refusals with and without a cause. A runner that crashes, hangs (no
record within 60 s), or writes a line that is not exactly a record is a fault of the case it was on. Its other cases
still run.

For a case derived from another at that case's exact cost, the reference is also checked against its own rule (jlvm.md
section 7). At the cost, the outcome is unchanged. One gas below it, the outcome is `OutOfGas` and the gas used is
the whole limit.

## Runners

| Language | Runner | How it is run |
|---|---|---|
| Scala | `lang/scala/jlvm/src/test/scala/xyz/kd5ujc/jlvm/difffuzz/Runner.scala` (`Evaluate.record`) | in the driver's process; `Runner` is the same function as a main reading a file or standard input |
| Rust | `lang/rust/difffuzz` (`dilf4s-difffuzz`), on `dilf4s-jlvm` and `dilf4s-core` only | a child process, release build |
| TypeScript | `lang/ts/packages/jlvm/difffuzz/runner.ts`, on the built packages (not part of the published package) | `node --experimental-strip-types`, two child processes |

Every runner reads JSON lines and writes one record line per case, in order. It flushes after each record, so the
driver streams to it. A case line is a `jlvm_v1.json` input case with every text written as the hex of its UTF-8, so
no transport parser reads the program before the JLVM's own parser does:

```
{"name": "...", "program": "<hex of the surface text>" | "form": "<hex of canonical CBOR>", "data": "<hex>",
 "env": [["<hex name>", "<hex text>"], ...], "built": [["<hex name>", <built-value>], ...], "gasLimit": 123}
```

A case with `form` is read by `decode` instead of the surface parser. The runner records the refusal's kind, or the
program decoded and its evaluation. The bindings are applied in order, as in `JlvmVectors.run`. The program is read
first, then the data, then each binding. The first that fails is the outcome, with no gas.

## The generator

It is written in Scala, beside the reference. Three reasons:

- It can ask the reference, in process, what a case costs. That is how it places gas limits at the exact cost and one
  below it.
- It takes the closed list of operations from the generated `JlvmOp`, through a total match, so a new operation does
  not compile until the generator covers it.
- It writes findings in the shape that `OpCoverageCases` turns into vector rows.

Case `i` of seed `s` is a pure function of `(s, i)`, through SplitMix64, so a run is reproduced by `--seed s` and one
case by `--seed s --case i`. The sources, by weight in parts per thousand:

- **grammar** (300): programs rooted at each operation alike, built from its argument lists and kinds
  (`Grammar.sig`). Arguments are nested expressions, reads that hit, miss and partly hit (`a.b.5`, `a.b.c` under an
  array, `a..b`, computed paths of text and of exact segments, integers past 64 bits), and bound names, some of them
  bound to null. Values are placed inline, quoted, in the data or in a host binding, and carry edge values:
  - integers around 2^31, 2^53, 2^63, 2^64, 2^4096 and far beyond (`8e9999`);
  - decimals at the scale and digit limits (`1e-9999`, 10,000 digits), and spellings such as `1E+2`, `-0` and `0.10`;
  - astral characters, combining marks, the `White_Space` set and its look-alikes, and text around the preview's 32
    code points;
  - hex around the byte-string form;
  - empty and nested arrays and maps, with nesting close to the depth bound;
  - repeated member names, refused escapes and lone surrogates, now and then.
- **crypto** (140): each cryptographic operation on vector rows' arguments: `signing_v1`, `sigma_v1`, `zk_v1`,
  `vrf_v1` and `poseidon_v1`, the `jlvm_v1` cases' own, and Ethereum signatures for `ecrecover`. They are used as they are (true, or a
  value), with a byte flipped, cut or added anywhere (false, or a refusal), with another row's argument (false), with
  an argument of another kind, or reordered.
- **corpus** (275) and **vector** (50): the `jlvm_v1.json` cases, mutated or not. A mutation swaps an operation for
  another of its arity group, an alias or a removed name, perturbs or respells a literal, drops, duplicates or swaps
  arguments, or changes the data. Cases with built bindings or raised limits are drawn rarely.
- **form** (152): canonical forms, edited at the value level (an unknown operation, a wrong version, a malformed path,
  a spread of a non-variadic operation, an extra or missing key) or at the byte level (a flipped bit, a cut, a
  non-minimal or indefinite head, a tag); random trees in `jlvm.cddl`'s vocabulary; and forms written as bytes around
  the bounds (operations nested around 127 and the CBOR depth of 256, a literal nested past it, more arguments than
  `Program.MaxSize`), some with a node refused for its shape inside, so the order of the refusals is exercised. All
  are read by `decode`.
- **deep** (20) and **text** (60): nesting around the 127-operation bound, and edits of the text itself (whitespace,
  a BOM, cuts, duplicated spans).
- **weight** (10) and **bulk** (3): the result bound (2^24). A host binding of shared copies weighing about 2^23,
  joined with itself, lands a few copies either side of the bound without building anything large. Doubling text,
  bytes, arrays and maps to the bound builds about 2^24 bytes, so it is rare.

A fifth of all cases take a limit at their exact cost or one below it. A few take a small or random limit instead.

## Shrinking

A mismatch is reduced by delta debugging, and each step is checked on all three runners:

- **Program:** each node of the program's tree is replaced by one of its children or by something trivial, chunks of
  arrays and members of maps are removed, and long text and numbers are shortened.
- **Inputs:** the data and each binding are reduced the same way, and the gas limit is removed.
- **Text and bytes:** the program text or the form's bytes lose chunks, when the tree cannot be read or reprinting it
  loses the mismatch.

A candidate is kept only when it is strictly smaller and has the same signature, which is every `(language, field)`
pair that differs from the reference. So shrinking never wanders onto another bug. A finding gets at most
`--shrink-budget` candidates (3,000), none once `--minutes` has run out, and at most two cases per signature are shrunk
in a run (`--per-signature`).

Planted bugs show what this finds. Each was found within a few thousand cases, and each shrank to one operation:

| Bug | Signature | Shrunk to |
|---|---|---|
| TypeScript `trim` charging 2 per unit of weight instead of 1 | `ts:gas` | `{"trim": ""}` |
| TypeScript `upper` refusing as `lower` | `ts:outcome` | `{"upper": null}` |
| The same `trim` bug, at a limit one gas past Scala's cost | `ts:available ts:outcome` | `{"==": [{"var": ""}, {"trim": "a"}]}`, limit 289 |
| Rust `trim` keeping U+3000 | `rust:outcome` | `{"trim": "\u3000"}` |

## Options

| Option | Meaning |
|---|---|
| `--seed n` | the seed (default: from the clock, printed) |
| `--cases n` | how many cases (default 10,000) |
| `--minutes m` | stop after `m` minutes, whatever the count |
| `--case i` | only case `i` of the seed, printed with every runner's record (with the run's other generator options) |
| `--source name` | only one source: `grammar`, `crypto`, `corpus`, `vector`, `deep`, `bulk`, `weight`, `text`, `form` |
| `--replay file` | run the cases of a file: `findings.jsonl` (its `shrunk` rows), JSON lines of case rows, or a file with `cases` |
| `--vectors` | every `jlvm_v1.json` case, each record also checked against its row |
| `--dump file` | write every case, its reference record and message (for inspecting coverage) |
| `--out dir` | where findings, the summary and the runners' standard error go (default `target/difffuzz`) |
| `--batch n`, `--ts-workers n`, `--rust-workers n` | throughput |
| `--shrink-budget n`, `--per-signature n` | shrinking |
| `--rust path\|none`, `--ts path\|none`, `--node path` | the runners |

## A finding

1. Decide from `docs/specs/jlvm.md` which implementation is wrong. Scala settles a dispute unless the spec says
   otherwise.
2. Fix it, then add the shrunk case to `OpCoverageCases`, in a list of its own added to `run`'s cases.
3. Regenerate: `tools/sbt "jlvm/Test/runMain xyz.kd5ujc.jlvm.OpCoverageCases"`, then
   `tools/sbt "jlvm/Test/runMain xyz.kd5ujc.jlvm.JlvmVectorGen"`. All three languages then check the case on every build.
4. If the fix touches the Rust JLVM, the SP1 guest's key moves (`lang/rust/zk/README.md`).

`--replay target/difffuzz/findings.jsonl` reruns the findings. A replay writes what still mismatches to
`replayed-findings.jsonl` beside them, never over the file it reads.

## CI

- **Pull requests** that touch a JLVM, the vectors or the harness run the `difffuzz` job in `.github/workflows/ci.yml`.
  It runs after the Scala, Rust and TypeScript jobs, on what they built: the Scala job's classpath bundle (`bundle.sh`),
  the Rust job's release binary and the TypeScript job's `dist`. It uses a fixed seed and a fixed count sized for about
  five minutes.
- **Nightly**, `.github/workflows/difffuzz-nightly.yml` runs a seed taken from the date for about 45 minutes. It uploads
  the findings and the summary, and fails when anything mismatched. GitHub runs scheduled workflows only from the
  default branch.
