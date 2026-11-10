# dilf4s proved JLVM calls (v1)

Status: normative. Schema: `shared/schemas/zk-jlvm.cddl` (the statement). Conformance vectors:
`shared/vectors/zk_jlvm_v1.json` (schema `shared/schemas/zk-jlvm-vectors.cddl`): calls with their statements and public
values, the guest's verifying key, and a real SP1 Groth16 proof.

A JLVM call is proved by running it in an SP1 guest that commits one **statement**: what was run, on what, and what
came of it. An SP1 Groth16 proof of the guest's public values, checked with `groth16_verify` (`jlvm.md` §6, `zk.md`
§2) under the guest's verifying key, then shows that the JLVM gave that outcome on that call. The proof is a
validity proof, not a hiding one (§4).

## 1. The call

A call is exactly the five things a JLVM result is a function of (`jlvm.md`):

- the program;
- the input data;
- the host bindings, each a name and a value;
- the gas limit;
- the language version and the gas schedule, which are the guest's: language 1, the latest schedule (1).

The guest `dilf4s-zk-jlvm-call` (`lang/rust/zk/guest/src/bin/jlvm_call.rs`) reads the call in canonical form:

- the program's canonical CBOR, the encoding of its canonical form (`jlvm.md` §1);
- the input data's canonical CBOR (`canonical-encoding.md` §2);
- the host bindings as a list, never a map, each a name and its value's canonical CBOR;
- the gas limit.

It decodes each strictly: the program as `jlvm.md` §1, "Reading a canonical form", gives it, and the data and each
value with the canonical CBOR decoder, a value as it is (a byte string is bytes, with no `0x` text to convert). It
then evaluates with the Rust JLVM (`dilf4s-jlvm`, its `Native` backend on the SP1-patched cryptography) and commits
the statement. The guest links no JSON parser. Whoever runs the guest writes the call: the host (`dilf4s-zk-jlvm`)
writes it from surface texts (`jlvm.md` §1, as `jlvm_v1.json` holds them), each parsed as the conformance vectors
are run and then encoded.

- **A call with no statement.** The guest halts with exit code 1 when the program's bytes are not a program's
  canonical form (any refusal of `jlvm.md` §1, "Reading a canonical form"), the data's or a binding's bytes are not
  canonical CBOR, or a binding is named twice; with 2 when the JLVM does not start. Neither commits anything. SP1's
  Groth16 wrapper carries the exit code as a public input, and `groth16_verify` refuses any exit code but 0 (`zk.md`
  §2), so no such run has a proof that verifies.
- **Every evaluation has one.** A program that decodes but does not compile, an error, and running out of gas are
  outcomes like a value (§2), committed with exit code 0. So is a gas limit over 2^53 − 1, which the guest reads as
  any 64-bit unsigned integer: it is `Limit` with no gas used, whatever the program (`jlvm.md` §7).
- **The input is transport.** The statement holds the program's identity, the tagged hash of its canonical form
  (`jlvm.md` §1), and digests of the data and the bindings (§2), never the bytes the guest read. A program has one
  canonical form and a value one canonical encoding, so any texts that parse to one program and one input give one
  call, one identity and one evaluation, and no other bytes decode to them.
- **Why canonical form.** The guest does less work, and links less code, than when it read surface text and parsed it
  (measured in execute mode, SP1 v6.10.0, 2026-11-08, on the calls of `zk_jlvm_v1.json`):
  - the proved call `contract multisig_wallet: 2 of 3 owners sign` runs in 4,087,928 cycles, not 4,357,118 (6.2%
    fewer); the other contracts' calls, 5.0% to 6.0% fewer; `metakit gas vector 203`, 2.2% fewer; the two calls of
    one operation on a literal, 0.4% more (about 6,500 cycles over 1.77 million);
  - the ELF is 2,210,192 bytes, not 2,233,880, without the JSON parser;
  - the statements, and so the public values, are the same.

## 2. The statement

The public values are exactly the canonical CBOR (`canonical-encoding.md` §2) of one `zk-jlvm-statement`: nothing
before or after it, and no SP1 framing (the guest writes them with `commit_slice`). A verifier decodes them and
re-encodes the decoded statement; bytes that do not re-encode to themselves are not a statement.

```cddl
;# import jlvm-vectors as jv
; dilf4s SP1 JLVM call statement, v1 (docs/specs/zk-jlvm.md). Normative for shape. The SP1 guest dilf4s-zk-jlvm-call
; commits exactly the canonical CBOR of one statement as its public values. Digests are SHA-256 tagged hashes
; (canonical-encoding.md section 4) of value commitments (merkle.md section 2), never raw values.

zk-jlvm-statement = {
  "version": 1,
  ; the JLVM language version the call ran under (jlvm.md)
  "language": uint,
  ; the gas schedule version the call ran under (jlvm.md section 7)
  "schedule": uint,
  ; the program's identity, taggedHash_SHA-256("dilf4s/jlvm/program/v1", cbor(program)) (jlvm.md section 1)
  "program": bytes .size 32,
  ; taggedHash_SHA-256("dilf4s/zk/jlvm/input/v1", commit(data))
  "input": bytes .size 32,
  ; the host bindings: taggedHash_SHA-256("dilf4s/zk/jlvm/bindings/v1", e1 || ... || en) over the bindings in name
  ; order, each ei = taggedHash_SHA-256("dilf4s/zk/jlvm/binding/v1", cbor(name) || commit(value))
  "bindings": bytes .size 32,
  "gasLimit": uint,
  "gasUsed": uint,
  "outcome": zk-jlvm-outcome,
}

; A value by its digest, or an error by its kind and normative fields, as jlvm_v1.json records it (jlvm.md section 3).
zk-jlvm-outcome = zk-jlvm-result / jv.jlvm-error

; taggedHash_SHA-256("dilf4s/zk/jlvm/result/v1", commit(value))
zk-jlvm-result = { "result": bytes .size 32 }
```

`jv.jlvm-error` is the error record of `jlvm-vectors.cddl`: `{"error": kind, ? "op": site, ? "cause": {family:
kind}}`, the error's normative fields as `jlvm.md` §3 defines them and as `jlvm_v1.json` records them. An error's
message is informative and never committed. `OutOfGas` is pinned by its kind and `gasUsed`, which is then the limit.

| Field | Value |
|---|---|
| `language` | the JLVM language version, 1 |
| `schedule` | the gas schedule's version, 1 |
| `program` | the program's identity, `taggedHash_SHA-256("dilf4s/jlvm/program/v1", cbor(program))` |
| `input` | `taggedHash_SHA-256("dilf4s/zk/jlvm/input/v1", commit(data))` |
| `bindings` | `taggedHash_SHA-256("dilf4s/zk/jlvm/bindings/v1", e₁ ‖ … ‖ eₙ)`, the bindings in name order (UTF-8 byte order), each `eᵢ = taggedHash_SHA-256("dilf4s/zk/jlvm/binding/v1", cbor(nameᵢ) ‖ commit(valueᵢ))`; no bindings is the tagged hash of nothing |
| `gasLimit` | the call's gas limit, as the guest read it, even one over the maximum |
| `gasUsed` | the gas the evaluation used (`jlvm.md` §7); 0 for a program that does not compile, and for a gas limit over the maximum |
| `outcome` | `{"result": taggedHash_SHA-256("dilf4s/zk/jlvm/result/v1", commit(value))}`, or the error record |

`commit(v)` is the value commitment of `merkle.md` §2: `0x00 ‖ b` for a byte string `b`, `0x01 ‖ cbor(v)` for any
other value. Tagged hashing is `canonical-encoding.md` §4, over SHA-256.

**Why this shape.**
- **Everything the result depends on is committed**, so a prover cannot vary any part of the call, the language or
  the schedule and keep the statement. The guest's verifying key fixes the code that computed it (§3).
- **Digests, not values.** The input, the bindings and a result can each weigh up to 2^24 (`jlvm.md` §2). Their
  digests keep the public values small and constant in size, whatever the call. A verifier who knows the value
  computes its digest and compares.
- **Bindings are not a map.** A canonical map drops a null value, and a binding to null (which reads as null) differs
  from an unbound name (which is `Malformed`, `jlvm.md` §4.1). Each binding is hashed on its own, its name's CBOR
  first (self-delimiting), so no two binding sets share a digest, and no binding value's depth limits the call.
- **`gasLimit` is committed** although a value's outcome does not depend on it past `gasUsed`: with it the statement
  says exactly which call ran, and an `OutOfGas` outcome is checkable from the statement alone.
- **Warnings** (`jlvm.md` §4.1) change neither the result nor the gas, and are not committed.

## 3. Proof and verification

The proof is SP1's Groth16 wrapper (`zk.md` §2, `SP1VerifierGroth16.sol`, circuit v6.1.0) of the guest's run, made
with SP1 v6.10.0. Its public inputs are `[programVKey, sha256(publicValues) & (2^253 − 1), exitCode, vkRoot, nonce]`.

- **`programVKey`** is the guest's verifying-key hash, SP1's `bytes32` of the key `setup` derives from the guest's
  ELF. `zk_jlvm_v1.json` pins it with the ELF's SHA-256. A verifier must take it from there (or recompute it from the
  ELF), never from the prover: another program could commit any bytes.
- **`exitCode`** must be 0 (§1). **`vkRoot`** is SP1's recursion root, a constant of the circuit.
- **`nonce`** is a public input the prover chooses (`ProveRequest::with_proof_nonce`), not part of the statement:
  two proofs of one call may differ in it. Use the statement, never the proof bytes, as a call's identity.
- **Verifying** a proved call is `groth16_verify([programVKey, publicValues, proofBytes])` (`jlvm.md` §6), then
  decoding `publicValues` as a statement and comparing its fields with the call the verifier has in mind.

The guest's ELF must not depend on where it was built, so that CI, a developer and the prover derive one
`programVKey`: the host's build remaps the checkout and `CARGO_HOME` out of the paths the ELF embeds
(`lang/rust/zk/host/build.rs`), and the guest's workspace root (`lang/rust/Cargo.toml`) contains every path dependency, so
cargo hashes them by relative path (`lang/rust/zk/README.md`, "Layout"). Any change to the guest or the code it links (the JLVM, the cryptography, the SP1
patches) changes the ELF and the key. The gates then fail, naming both keys, until the call is proved again under
the new key and the proof pinned: the pinned proof must always verify under the pinned key, and the pinned key must
always be the guest's (`lang/rust/zk/README.md`).

## 4. Informative notes

- **Not hiding.** The digests bind, they do not hide: whoever can guess a low-entropy input, binding or result can
  check the guess against the statement. A call that needs hiding puts a random salt in its data.
- **Cost.** The guest's cycles are the prover's cost, not gas: gas prices the validating nodes that evaluate the call
  (`jlvm.md` §7) and is the same in the guest as anywhere.
- **Reference values.** `zk_jlvm_v1.json` computes each statement natively in Rust and in Scala, from the
  `jlvm_v1.json` row it names, and checks the guest commits the same bytes (`lang/rust/zk/host/tests`).
