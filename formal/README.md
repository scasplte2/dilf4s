# formal: a Lean 4 model of a JLVM slice

A spike towards a Lean 4 executable specification of the JLVM (docs/specs/jlvm.md): the semantics and proofs in Lean,
with the Lean model as an oracle for the production implementations. It covers a slice of the language, proves two
theorems about it, and runs the pinned vectors through it. Lean core only (no Batteries, no Mathlib), toolchain
pinned in `lean-toolchain`.

```
lake build                              # builds the model and checks every proof
lake exe vectorcheck ../shared/vectors  # runs the vectors (exit code 1 on any mismatch)
```

## What is modelled

| File | What |
|---|---|
| `Jlvm/Value.lean` | The canonical value model (canonical-encoding.md section 1): null, booleans, one exact number kind (`Rat`), text, bytes, arrays, maps in canonical key order. Weight (saturating), depth, elements, truthiness, kinds, and the data model's invariants (`WF`). |
| `Jlvm/Cbor.lean` | The canonical CBOR encoder (section 2), and the strict decoder with the normative refusal order. The decoder recurses on the depth it still allows, so it needs no fuel. |
| `Jlvm/Json.lean` | The strict JSON parser (section 3), refusal order included: digit, exponent and scale bounds counted on the text, a repeated member name refused as it is read, unpaired surrogates refused (a member name's when its object closes), depth 256. Then the JLVM's surface values (`0x`-hex strings are bytes). `Lean.Json` is not used: it keeps the last of repeated names, bounds neither depth nor digits, and loses member order. |
| `Jlvm/Program.lean` | The program surface (jlvm.md section 1) for every operation name, keyword, alias and removed name; the size, nesting and canonical-depth bounds; compilation, with the shapes of `if` and the callbacks. |
| `Jlvm/Eval.lean` | Metered evaluation (sections 2, 4, 5 and 7) of the slice: literals, arrays and map expressions, `var` (data paths, bindings, defaults, computed paths, callback frames and `AmbiguousPath` warnings), `if`, `and`, `or`, `default`, `!`, `!!`, `==`, `!=`, `===`, `!==`, `<`, `<=`, `>`, `>=`, `+`, `-`, `*` (exact rationals, with their gas), `cat` (text, numbers rendered as exact decimals, arrays, bytes), `merge`, `map` and `all`. Gas schedule v1 for these. |
| `Jlvm/Theorems.lean` | The theorems. |
| `VectorCheck.lean` | The vector run. |

Not modelled: every other operation, `let`, the depth of the rendered surface (`Parser.surfaceDepth`), program
rendering and identity, and decoding a program's canonical form.

## Theorems

No `sorry`, and no axiom beyond Lean's standard three (`propext`, `Classical.choice`, `Quot.sound`). `lake build`
prints each theorem's axioms, and CI checks both.

- **Termination.** The evaluator is defined by well-founded recursion on the program, with no `partial` and no fuel.
  The measure is the size of the node, then the elements a callback has left: a callback runs its body, a subterm,
  once per element of a finite list. Lean's kernel accepts the definition only with that proof, so every evaluation
  terminates. The JSON parser alone takes fuel, twice its input's length.
- **Theorem 1, gas within the limit** (`gas_within_limit`). Take any program, data, bindings and gas limit below
  `2^63 - 1`. The gas an evaluation uses never exceeds the limit, and an evaluation that runs out of gas reports the
  whole limit as used (jlvm.md section 7, zk-jlvm.md section 2).
- **Theorem 2, gas bounds paid work** (`steps_bounded_by_gas`). `steps` counts the node evaluations that paid their
  charge, each at least 10 gas, so there are at most `gasUsed / 10` of them, and so at most `limit / 10`, whatever
  the program and its data. Every node pays before it evaluates any part, except a computed path (`{"var": e}`),
  which pays once `e` is evaluated (jlvm.md section 7). So an evaluation that fails inside computed paths may also
  have entered the computed-path nodes around the failure without paying for them. There is one chain of them, at
  most the program's nesting (256), which is bounded before anything runs. That second bound is **not** proved here:
  50 nested computed paths around `true` enter 51 nodes for 10 gas, and `vectorcheck` checks this as a regression.

Both theorems are properties of the evaluation monad `Ev`. A computation in it carries a proof that it keeps the meter
sound (`Sound`). Only `pure`, `bind`, `charge`, `chargeStep`, `fail` and `warn` build one, and each is proved sound
once. The evaluator is written only with these, so the compiler checks that every evaluation is sound. `chargeStep`
takes a proof that its charge is at least 10, and every node starts with one.

The limit must be below `2^63 - 1` because gas saturates there. At a limit of exactly `2^63 - 1`, a charge that
saturates still fits the limit, so once saturated, further charges are free. That is true of the reference
(`Gas.scala`) too.

## How it is checked against the vectors

`vectorcheck` reads the vector files with the model's own strict JSON parser, then checks two files.

**`canonical_encoding_v1.json`:**
- every `json` row: parse, then encode to canonical CBOR, or refuse with the row's kind;
- every accepted row's encoding, decoded back to the value;
- every `cborRejections` row, refused with its kind.

**`jlvm_v1.json`**, every case in scope:
- the outcome: the result's canonical CBOR, or the error's kind and `op`;
- the gas used and the number of warnings, exactly.

A case is in scope when it has no `built` binding and its program text names only the slice's operations. Every key
of a single-key object outside a `{"literal": ...}` is checked, aliases included. The test reads the program's text,
never its expected outcome. A program whose text is not JSON is in scope, since its outcome is the parser's alone.

Results on base `958dc34e`:
- encoding: 263 of 263 checks pass;
- `jlvm_v1.json`: 3339 cases, 847 in scope. Of those, 456 expect a value and 391 an error, 10 of them `OutOfGas`.
  All 847 pass.
- The run takes about 2 s of CPU.
