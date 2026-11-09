# @kd5ujc/dilf4s-jlvm

The dilf4s JLVM in TypeScript: the strict, deterministic, gas-metered expression language of
[`docs/specs/jlvm.md`](../../../../docs/specs/jlvm.md) (language version 1, gas schedule version 1), giving the same
values, errors and gas as the Scala reference (`jlvm/`) and the Rust crate (`sdk/rust/jlvm`). The conformance vectors
`shared/vectors/jlvm_v1.json` are its gate.

```ts
import { parse, parseValue, evaluate } from "@kd5ujc/dilf4s-jlvm";

const program = parse('{"+": [1, {"var": "x"}]}');
const data = parseValue('{"x": 2}');
if (program.ok && data.ok) {
  const outcome = evaluate(program.value, data.value, { gasLimit: 10_000 });
  // { ok: true, value: { gasUsed, result: { ok: true, value: 3 }, warnings: [] } }
}
```

- `parse` reads a program's JSON surface (aliases included), and `render` writes it back exactly. `fromCanonical` and
  `decode` read its canonical form (`shared/schemas/jlvm.cddl`). A `Program` is bounded in size and depth, has one
  canonical form, and its `id()` is `taggedHash("dilf4s/jlvm/program/v1", cbor(program))`.
- `compile` checks the special forms. `evaluate` runs a program or compiled program against data, with host bindings
  (`env`, a canonical map) and a gas limit. It reports the gas used whether it succeeds or fails.
- Every failure is a typed result: the generated `JlvmError` (kind, `op`, `cause`), or `TypeMismatch` for an argument
  of the wrong type. Nothing throws.

## Design

- **One iterative machine.** Evaluation runs on an explicit continuation stack, ported from the Rust crate
  (`machine.ts`), so nothing recurses on the program or the data. Parsing, compiling, rendering and the canonical
  codec are iterative too.
- **Gas before work.** Every charge is taken before the work it pays for, as section 7 of the spec says. The schedule
  is the spec's table (`gas.ts`); a test checks it row for row against `jlvm.md`. Amounts are `number`s, saturating at
  2^53 above every limit, which is at most 2^53 - 1 (`MAX_GAS_LIMIT`, the spec's): a larger limit is `Limit`.
- **Values.** The machine works on core's canonical values alone, as the Scala and Rust machines work on theirs: data,
  bindings, literals and every array and map it builds. A core map is a persistent AVL tree in canonical key order
  (core's `tree.ts`, the order Rust's `Map` keeps), so `set`, `unset` and `merge` cost O(log n) per entry, as gas
  charges them: a host's map is updated in place of a copy, each update building the nodes on its search path and
  sharing the rest, and the tree keeps the size, weight and depth exact. An object literal's keys are put in
  canonical order once, at compilation (core's `MapShape`), so building it at run time compares no key. `keys`,
  `values` and `entries` list in code-point order through the map's code-point view, linear in its weight. What
  leaves the machine is the value it computed, as it is.
- **Host bindings** are `Bindings` (which can bind a name to null: it then reads as null, where an unbound name is
  `Malformed`, as in Scala and Rust) or a core `MapValue` (which binds none to null).
- **One table per class.** Strict operations (`ops.ts`) and cryptographic ones (`crypto.ts`) each have a handler table
  typed as a record over the generated `JlvmOp`, so a missing handler does not compile.

## Work outside gas

Gas prices the validating nodes' work (the Scala and Rust implementations); this package is a client, whose gas has
only to match theirs. None of its map work goes past what gas charges for it:

- **Every update** (`set`, `unset`, each entry `merge` sets) is O(log n) in the map's size, a host map's included, and
  keeps the weight exact through any updates, a saturated one (2^63 or more, which takes shared parts) too: the tree
  holds each subtree's exact sum and saturates only when the weight is read. No update lists a map.
- **Every listing** in code-point order (`keys`, `values`, `entries`) is charged by weight, and the code-point view is
  linear in the weight: core merges the map's runs of one UTF-8 length by their common prefixes, reading fewer than
  10 pairs of bytes per unit of weight (`codePointOrder` in core's `tree.ts` proves the bound, and a test counts it).
  A host map or a literal keeps its view once found; a map an update made finds it at each listing, and keeps nothing,
  so versions sharing their nodes share their memory too.
- **The result** is a core value already: nothing is converted after the last charge.

## Section 6: cryptographic and proof operations

`crypto.ts` checks each operation's arguments as the reference does (arity, kinds, fixed widths, canonical BN254
scalars, Merkle heads, trie modes and keys, the claims' weight), in the reference's order, then calls `signing`,
`proofs` and core's hashing: signatures, `ecrecover`, sigma protocols (`signing`'s `sigma`), ECVRF, Poseidon and its
Merkle tree, the BN254 group operations and SP1's Groth16 verifier (`proofs`' `zk` and `zk.groth16`, on noble's curve),
Merkle trees and tries. A refusal a family made carries that family's typed error as its `cause`; the operations' own
shape checks carry none. Gas is charged before a handler runs, so a refusal's gas is the vectors' too.

Every operation runs: a handler's only outcomes are a value and a `JlvmError`, so a stub does not compile, and the
conformance test runs every case, skipping none.

- `sigma_verify` decodes the proposition first (`sigma.Proposition.fromValue`), then checks the proof's and the
  message's kinds, then verifies (`sigma.verify`); `schnorr_verify` is `sigma.verifyDlogBytes` on its three byte
  strings.
- `groth16_verify` checks the key's width and the arguments' kinds, then verifies under SP1's verifying key (circuit
  v6.1.0), decoded and checked once when the module loads, as the Rust backend holds it. A `Malformed` refusal is a
  `Crypto` error caused by `Groth16`; a proof that SP1's framing rejects, that is invalid, or that fails the pairing
  is `false`.

## Tests

The package's tests are in `test/`:

- `conformance.test.ts`: the vectors, the alias rows and the error-kind coverage.
- `dispatch.test.ts`: one handler per operation, each running, and the spec's gas table.
- `zk-jlvm.test.ts`: `shared/vectors/zk_jlvm_v1.json`: each call's statement and public values, and the pinned SP1
  Groth16 proof through `groth16_verify`.
- `crypto.test.ts`: the section 6 vectors rerun on arguments the machine built (maps and arrays from expressions).
- `metering.test.ts`: the denial-of-service regressions.
- `depth.test.ts`: no native stack growth at the depth limit.
- `linear.test.ts`: steps linear in gas, and an update's nodes against the map's size.
- `host-maps.test.ts`: host maps updated without listing or copying them, exact through any updates, and programs
  refused before they are read.
- `api.test.ts`: typed refusals, host bindings and canonical round trips.
- `render.test.ts`: exact rendering.
