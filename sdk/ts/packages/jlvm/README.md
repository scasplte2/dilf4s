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
  2^53 above every limit, which is at most `Number.MAX_SAFE_INTEGER`.
- **Values.** Data, bindings and literals are core's canonical values. The arrays and maps the machine builds are its
  own (`jvalue.ts`): a map is a persistent AVL tree, so `set`, `unset` and `merge` cost O(log n) per entry, as gas
  charges them. A host's map is updated in place of a copy: the tree lays its entries and tombstones over it, and
  reads the rest through the map's own search, keeping the size, weight and depth exact. What leaves the machine is
  converted to a core value once.
- **Host bindings** are `Bindings` (which can bind a name to null: it then reads as null, where an unbound name is
  `Malformed`, as in Scala and Rust) or a core `MapValue` (which binds none to null).
- **One table per class.** Strict operations (`ops.ts`) and cryptographic ones (`crypto.ts`) each have a handler table
  typed as a record over the generated `JlvmOp`, so a missing handler does not compile.

## Work outside gas

Gas prices the validating nodes' work (the Scala and Rust implementations); this package is a client, whose gas has
only to match theirs. Three pieces of its work are bounded otherwise:

- **Converting the result** to a core value happens after the last charge. It is bounded by the result weight bound,
  2^24, times a log factor for sorting map keys, not by the gas used.
- **A host map whose weight saturates** (2^63 or more, which takes shared parts) is converted into the machine's own
  tree when an update first hides one of its entries, so its weight can be recounted: O(n log n) in that map, once per
  evaluation. Any other host map is converted only after half of it was hidden by charged updates.

- **A host map's depths:** when an update first hides one of a host map's deepest values, the map's values are
  counted by depth, one pass over it, once per evaluation, so the depth stays exact. Every later update reads that
  count in O(1) of the map's size.

Every other update is O(log n), and every listing is charged by weight. A persistent core `MapValue` carrying its
entries' weights and depths would remove all three.

## Section 6: cryptographic and proof operations

`crypto.ts` checks each operation's arguments as the reference does (arity, kinds, fixed widths, canonical BN254
scalars, Merkle heads, trie modes and keys, the claims' weight), in the reference's order, then calls `signing`,
`proofs` and core's hashing: signatures, `ecrecover`, ECVRF, Poseidon and its Merkle tree, the BN254 group operations
(`proofs`' `zk`, on noble's curve), Merkle trees and tries. A refusal a family made carries that family's typed error as
its `cause`; the operations' own shape checks carry none. Gas is charged before a handler runs, so a refusal's gas is
the vectors' too.

Three operations are not implemented, because the TypeScript SDK has no primitive for them: `sigma_verify` and
`schnorr_verify` (sigma protocols, `sigma.md`) and `groth16_verify` (SP1's Groth16 verifier, `zk.md` section 2). An
evaluation that reaches one pays for it (its base, its arguments and its size charge, as section 7 orders them) and
ends with `{ kind: "Unimplemented", op }`, from one place, `crypto.ts`'s table. The conformance test skips exactly the
cases that reach one of these three, and lists them by operation; any other operation reached as `Unimplemented` fails
it.

## Tests

The package's tests are in `test/`:

- `conformance.test.ts`: the vectors, the alias rows and the error-kind coverage.
- `dispatch.test.ts`: one handler per operation, which three are unimplemented, and the spec's gas table.
- `crypto.test.ts`: the section 6 vectors rerun on arguments the machine built (its own maps and arrays).
- `metering.test.ts`: the denial-of-service regressions.
- `depth.test.ts`: no native stack growth at the depth limit.
- `linear.test.ts`: steps linear in gas, and the persistent map's work per update.
- `overlay.test.ts`: host maps updated without listing them, and programs refused before they are read.
- `api.test.ts`: typed refusals, host bindings and canonical round trips.
- `render.test.ts`: exact rendering.
