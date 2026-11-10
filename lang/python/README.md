# dilf4s for Python

PyO3 bindings over the dilf4s Rust SDK (`lang/rust`): the canonical data model, signatures, proof verifiers and the
JLVM, with the same bytes and the same refusals as every other dilf4s language. Every shared vector the Rust SDK gates
runs through this package's API in its tests.

- `dilf4s.core`: canonical values, CBOR, the JSON mapping, tagged hashing and value commitments.
- `dilf4s.signing`: signature verification for every scheme, signing and key derivation, VRFs, KES and sigma proofs.
- `dilf4s.proofs`: Merkle trees, tries, versioned tries, Poseidon, BN254 and Groth16 (SP1's included).
- `dilf4s.jlvm`: the JLVM, a deterministic, gas-metered expression language for contracts.
- `dilf4s.errors`: every refusal, one module per error family of `shared/schemas`.

Every call that can refuse returns `Ok(value)` or `Err(error)`, frozen and matched by class, and never raises for a
refusal. An argument of the wrong type is `Err(TypeMismatch(...))`.

## Building

One abi3 wheel serves CPython 3.12 and later:

```sh
uv sync --locked
uv run --no-sync maturin build --release --locked --out dist
uv pip install --no-deps dist/*.whl
uv run --no-sync pytest
```

## Known limitations

The package is a client SDK; it never runs as a node. A crafted argument that names one large buffer many times is,
in general, read once per distinct object: `bytes` are borrowed, a `bytearray` or `memoryview` is copied once per
call, and collections are checked before anything is copied in proportion to their references. One path found in
review still copies a buffer separately for each argument:

- **Versioned-trie arguments** (`VersionedTrie.verify_versions`'s root and range ends, and a `VersionedHead`'s
  fields): these are read separately, so one buffer passed as all of them is copied once per argument.

Merkle digest lists retain only the prefix through the first invalid-width digest. Later entries are still
type-checked, preserving `TypeMismatch` precedence, but allocate no digest placeholders. Input references are
still collected for boundary validation.

A caller that passes each buffer once, rather than one object many times, is unaffected.
