# dilf4s SP1 guest harness

An SP1 v6 guest that runs the Rust SDK (`dilf4s-proofs`, `dilf4s-signing` without `sign`, and `dilf4s-jlvm`) on
`riscv64im-succinct-zkvm-elf`, built with every SP1 patch. A host executes it over the shared vectors and checks the
outcomes. The host uses execute mode only: nothing is proved.

The SDK's own CI build targets `riscv64imac-unknown-none-elf`. That target has `target_os = "none"`, so it never
compiles the patches' `target_os = "zkvm"` code. This harness compiles that code and runs it.

## Layout

There are three cargo roots, each with its own `[workspace]` and `Cargo.lock`:

| Directory | What it is |
|---|---|
| `cases/` | `dilf4s-zk-cases`, a `no_std` crate. It holds what is computed (`run`, plus the issue #33 leaf forms). With the `vectors` feature it also reads each case and its expected outcome from `shared/vectors`. Both the guest and the host build it. |
| `guest/` | The SP1 program. Its `Cargo.toml` has the `[patch.crates-io]` section, so the patches reach only the guest. |
| `host/` | Builds the guest with `sp1-build`, `--locked`, from `build.rs`. Executes it with `sp1-sdk`'s light prover. Has no patches, so its native side builds the same crates.io releases that `sdk/rust` uses. |

The roots are kept separate on purpose. If the patches applied to the host, the native comparison would compile the
patched crates too. Any change a patch makes off the zkVM would then appear on both sides and hide.

The host's native build does not have exactly `dilf4s-signing`'s k256 features. `sp1-sdk` depends on k256 too, and
feature unification turns on `std`, `serde` and `precomputed-tables`. These features cannot change results. They do
change speed, which is why the native issue #33 timings come from `cases/` on its own: its k256 has signing's
features.

## Running

You need these first:

- **The SP1 toolchain v6.10.0.**
  - Install it with the sp1up script at the `v6.10.0` commit, `d83ed91f96701209613720708f36ffce0e98ed33`: `sp1up --version v6.10.0`. CI fetches the script by that SHA, not the tag.
  - That installs `cargo-prove` v6.10.0 (commit `d83ed91`). It in turn installs the toolchain it pins, `succinct-1.96.0-64bit-v3`, which reports as `rustc 1.96.0-dev`.
  - `./check-toolchain.sh` fails on anything else.
- **`protoc` with the well-known types.** `sp1-sdk`'s build compiles protobuf files that import `google/protobuf/empty.proto`.
  - Ubuntu: `apt-get install protobuf-compiler libprotobuf-dev`.
  - Fedora: `dnf install protobuf-compiler protobuf-devel`.
- **Rust 1.99.1 for the host and `cases`**, as `sdk/rust` pins it (and CI). Their MSRV is 1.96, the succinct
  toolchain's.

Then:

```sh
cd sdk/rust/zk
./check-toolchain.sh
cargo test --locked --release --manifest-path host/Cargo.toml -- --nocapture
cargo run --release --manifest-path cases/Cargo.toml --features vectors --example sigma_leaves
```

CI runs the same commands in the `sp1-guest` job of `.github/workflows/ci.yml`.

## What the tests check

`host/tests/guest_matches_native.rs` covers thirteen families:

| Family | Cases |
|---|---|
| Ed25519 and libsodium | Every `verification` row and committed signature |
| ristretto255 | Every `verification` row and committed signature |
| secp256k1 | ECDSA, BIP-340 and tagged Schnorr |
| P-256 | ECDSA |
| BLS | Including the proofs of possession |
| ecrecover | Every committed secp256k1 signature at both parities, plus go-ethereum's vector and its refusals |
| Sigma | Both vector files |
| BN254 and SP1 Groth16 | Every add, mul, pairing and `sp1Groth16` case |
| Poseidon | Hashes and Merkle roots |
| VRF (TAI, ELL2, draft-03) | Every committed proof and rejection row of `vrf_v1.json`, one family per suite; also prints the guest cycles per verification |
| JLVM | 264 cases: every case that calls a cryptographic operation on real inputs, and every 50th other case |

For each family the test asserts four things:

1. **The guest ran on the zkVM.**
   - It reports `cfg!(target_os = "zkvm")`, and its exit code is 0.
   - Every precompile its patches use was called. For example, `ED_DECOMPRESS` for Ed25519 and `BN254_FP_MUL` for BN254.
   - The execution report's syscall counts prove the patched paths ran, not only that they compiled.
2. **Guest and native outcomes are identical.** They are compared byte for byte, with errors compared through `Debug`, on every case: there are no exceptions.
3. **The native outcome is the one the vector records.** So the comparison cannot pass on a bug that guest and native share.
4. **ecrecover recovers the signer exactly once.** For each committed secp256k1 signature, exactly one parity recovers the signer.

`host/tests/guest_lock.rs` checks the patches themselves:

- `guest/Cargo.lock` resolves every patched crate to its `sp1-patches` tag and to nothing else. Cargo only warns when a `[patch]` entry does not apply, so this needs a test.
- k256 pulls in the ecdsa fork.
- `sp1-lib` and `sp1-zkvm` are both 6.10.0.

## The patches

The patches are pinned in `guest/Cargo.toml` at their SP1 6.10.0 tags. One exception is bls12_381:
`dilf4s-signing` already depends on that patch's source directly. Commits are shown as resolved in
`guest/Cargo.lock`.

| Crate | Tag | Commit | Native build uses |
|---|---|---|---|
| k256 0.13.4 | `elliptic-curves` `patch-k256-13.4-sp1-6.10.0` | `cd63f2b5` | crates.io |
| ecdsa 0.16.9, pulled in by the k256 patch | `signatures` `sp1-skip-verify-on-recovery` | `1880299a` | crates.io |
| p256 0.13.2, and its primeorder | `elliptic-curves` `patch-p256-13.2-sp1-6.10.0` | `f074fbff` | crates.io |
| curve25519-dalek 4.1.3 | `curve25519-dalek` `patch-4.1.3-sp1-6.10.0` | `338cf5b3` | crates.io |
| bls12_381 0.8.0 | `bls12_381` `patch-0.8.0-sp1-6.10.0` | `d347c2b1` | the same patch |
| sha2 0.10.9 | `RustCrypto-hashes` `patch-sha2-0.10.9-sp1-6.10.0` | `e48b656e` | crates.io |
| sha3 0.10.8 | `RustCrypto-hashes` `patch-sha3-0.10.8-sp1-6.10.0` | `0a16ae7a` | crates.io |
| substrate-bn 0.6.0 | `bn` `patch-0.6.0-sp1-6.10.0-substrate-bn` | `ab4d0085` | crates.io |
| crypto-bigint 0.5.5 | `RustCrypto-bigint` `patch-0.5.5-sp1-6.10.0` | `e67beeeb` | crates.io |

The guest also builds two copies of `ecdsa`:

- the fork, for k256;
- crates.io's 0.16.9, for p256, whose patch does not fork it.

Never `[patch]` `ecdsa` itself. The fork adds a required `VerifyPrimitive::HIGH_S_ALLOWED`, which p256 does not
define.

### What changes behaviour rather than speed

Each patch was read once against the crates.io release that native builds resolve. In the list below:

- **(zkVM)** means the change applies only under `target_os = "zkvm"`.
- **(everywhere)** means the patch changes the crate on every target. It differs from what native builds use, because
  they take crates.io.

Paths are in the patch repositories.

#### curve25519-dalek: rejects non-canonical `y` (zkVM)

`CompressedEdwardsY::decompress` (`edwards.rs:252-352`) works like this:

1. It asks a host hook whether the precompile can decompress the point.
2. If the hook says no, it returns `None` for any `y` in `[p, 2^255)`, with no proof. The patch documents this at
   `edwards.rs:247-248`.

Upstream reduces such a `y` mod `p`, and ZIP-215 accepts it.

- 24 such encodings decode to points. One of them is `y = p + 1`, a second encoding of the identity.
- The honest hook refuses them all, because the precompile's AIR range-checks `y < p`.
- dilf4s never reaches this refusal: `decode_zip215` (`signing/src/ed25519.rs`) reduces `y` before decompressing. See
  "Non-canonical Edwards `y`" below.

Everything else here is unchanged:

- `x = 0` with the sign bit set is accepted on both sides.
- Ristretto decoding is not patched.
- Variable-base and double-base multiplication are now variable-time but give the same results.

#### k256 / p256: same accept and reject sets (zkVM)

On the zkVM the point types are `sp1-lib`'s affine types. Addition is complete: the identity, `P + P` and `P − P` are
handled before the precompile.

The following give the same results as upstream:

- decoding and decompression;
- BIP-340's even `y`;
- ECDSA verification, which still refuses a high `s`;
- Schnorr verification.

Field and scalar `invert`/`sqrt` come from host hints, and the guest re-checks every one. The non-residues used to
prove "no root" (3 and 5 for k256, 3 and 7 for p256) are non-residues, so a prover cannot claim a root is missing when
one exists.

Two things change:

- **Timing.** Scalar multiplication, `mul_by_generator` and `lincomb` are variable-time, built on the add/double
  precompiles (`sp1-lib` `ecdsa/projective.rs:137-169`).
- **The public `sqrt` and `sqrt_ratio`** now return the smaller root, on every target. Decompression picks the root by
  parity again, so it is unaffected.

The API also has gaps on the zkVM. These fail to compile:

- `ProjectivePoint::GENERATOR` and `IDENTITY`;
- `&P * &s` and `AffinePoint * Scalar`;
- `lincomb_ext`;
- the `hash2curve` and `bits` features.

In p256, `From<&SecretKey> for Scalar` is `unimplemented!()`.

#### The ecdsa fork: no observable difference (everywhere k256 is patched)

- **What changed.** `recover_from_prehash` no longer calls `verify_prehash` on the key it recovered (`recovery.rs:320`).
  In its place it refuses a high `s` when the new `HIGH_S_ALLOWED` is false, which k256 sets (`recovery.rs:322-331`).
- **Why the outcome is the same.** The recovered `Q = r⁻¹(s·R − z·G)` always satisfies the verification equation.
  The skipped check could therefore only refuse a high `s`, and the fork still refuses that.
- **What is lost.** Defence in depth: an arithmetic bug would now return a wrong key instead of an error.
- **Two cautions.** The fork's tag is mutable, and k256's `Cargo.toml` at the tag says "REVERT BEFORE MERGING". The
  ecrecover family covers recovery at both parities, and high and low `s`.

#### substrate-bn (everywhere)

- **Short field inputs are accepted.** `Fq::from_slice` and `Fr::from_slice` accept 0 to 32 bytes and left-pad them;
  upstream requires exactly 32 (`arith.rs:202-220`). Every dilf4s call passes exactly 32 bytes:
  - `proofs/src/zk/mod.rs:75`: exact words;
  - `proofs/src/zk/groth16.rs:150`: `[u8; 32]` inputs;
  - `proofs/src/zk/groth16.rs:169-170`: the halves of a 64-byte `A`.
- **Unchanged:**
  - refusal of values `≥ p`;
  - the curve and G2-subgroup checks;
  - `pairing_batch` and the group arithmetic.
- **Representation.** Field elements are plain rather than Montgomery, which changes `Debug` and `random`.
- **API changes:**
  - `From<AffineG1> for G1` became `Into`.
  - `GroupError` gained `InvalidInputLength`.
  - The new `From<G2> for AffineG2` is wrong for `z ≠ 1`, so do not use it.
- **Precompiles.** The patch uses the BN254 field precompiles (`BN254_FP*`), not the point precompiles `BN254_ADD` and
  `BN254_DOUBLE`.
- **Alternative source.** `substrate-bn-succinct-rs` 0.6.1 on crates.io has the same `src/` as this tag. dilf4s does
  not use it natively: off the zkVM it multiplies by schoolbook and long division, which made the zk vectors 34×
  slower (issue #67).

#### bls12_381 (zkVM)

- **What is unchanged:**
  - decoding accepts and refuses what upstream does;
  - pairings agree.
- **A hint can force a panic.** In this tag (`patch-0.8.0-sp1-6.10.0`), `Fp2::invert` decodes its hint with `unwrap()`
  before checking it (`fp2.rs:238-251, 703`). A prover can therefore turn any pairing into a panic: exit code 1, an
  unprovable run. The `-v2` tag (`patch-0.8.0-sp1-6.10.0-v2`, `3a1789f6`) fixes it.
- **Hashing.** This tag also moved `hash_to_curve` to digest 0.10. Its output is unchanged.
- **Why dilf4s stays on this tag for now.** The `-v2` tag fixes the panic, but that is not all it changes:
  - It reverts `hash_to_curve` to upstream 0.8.0's draft-12 code on digest 0.9, which needs a second `sha2` (0.9) in
    `dilf4s-signing`.
  - It changes the API: `hash_to_curve` takes `impl AsRef<[u8]>`, and the expander types change. This breaks the
    RFC 9380 `expand_message_xmd` check in `signing/tests/vrf_vectors.rs`.
  - Everything else is the fix and lifetime spelling.

#### sha2, sha3, crypto-bigint (zkVM)

- **sha2.** Only SHA-256's compression function goes to the precompile; SHA-512 is untouched.
- **sha3.** Keccak-256 and SHA3-256 are unchanged. TurboSHAKE is wrong in the guest: the precompile ignores the round
  count, so it runs 24 rounds instead of 12 (`state.rs:51-61`). dilf4s does not use TurboSHAKE.
- **crypto-bigint.** `U256` residues hold plain values in the guest, which changes what `as_montgomery` and
  `from_montgomery` return. `mul_mod_special` with `c = 0` panics. k256, p256, elliptic-curve, BLS and BN do not use
  these paths.

### Prover hints

Execute mode runs the honest hooks, so the tests show behaviour under honest execution only.

As read in the code, no hint can change a value a patch returns. A bad hint stops the guest with exit code 3 (an
invalid hint). The exceptions are:

- **The bls12_381 `unwrap()` above.** It exits with code 1.
- **The status bytes of the k256 and p256 `sqrt` hooks and the curve25519 decompress hook**, read with `.expect`. An
  empty hint could panic with exit code 1. This was not confirmed.
- **The curve25519 decompress refusal.** It trusts the hook for whether to try the precompile at all.
  - A dishonest "yes" makes the run unprovable.
  - A dishonest "no" refuses without proof only a non-canonical `y`, which dilf4s never passes. For a canonical `y`
    the guest must prove there is no root.

## Non-canonical Edwards `y`

The patched curve25519-dalek refuses a non-canonical Edwards `y` in the guest; upstream reduces it, as ZIP-215 requires.

Until `dilf4s-signing` reduced `y` itself in `decode_zip215`, 152 of the 415 Ed25519 rows of `signing_v1.json`
diverged:

- the small-order keys a10 to a13 gave `InvalidKey` in the guest;
- the R encodings r10 to r13 gave `false` in the guest;
- natively, all of them verify under `Ed25519`.

The guest now equals native code on every row. The challenge still hashes `R` and `A` as given.

`jlvm_v1.json` pins the same encodings through `ed25519_verify` ("non-canonical" cases), which the JLVM family runs.

## Issue #33: one linear combination or two multiplications per sigma leaf

`host/tests/sigma_leaves.rs` and the `sigma_leaves` example of `cases/` compute each leaf commitment `z·B − e·P` both
ways. The leaves come from the valid single-leaf proofs of both sigma vector files. They are checked to agree with
each other and with native code. Guest costs are each form's run minus a baseline run; per-leaf numbers follow.

| Leaf | Native, two muls | Native, lincomb | Guest cycles, two muls | Guest cycles, lincomb | Guest gas, two muls | Guest gas, lincomb |
|---|---|---|---|---|---|---|
| Generator base (dlog) | 110 µs | 85 µs | 25,973 | 27,257 | 44,816 | 47,882 |
| Any base (DH tuple) | 110 µs | 85 µs | 26,218 | 27,564 | 45,145 | 48,298 |

Native timings use k256 with `precomputed-tables` off, as `dilf4s-signing` builds it, on a loaded 16-core x86-64.

The two targets point in opposite directions:

- **Natively**, `lincomb` takes about 0.77× the time of two multiplications.
- **In the guest**, it takes about 1.05× the cycles and 1.07× the gas.

Under the patch, `lincomb` is a two-point double-and-add on the same precompiles as a single multiplication, so it
saves nothing. `dilf4s-signing` keeps two scalar multiplications per leaf on every target (see `push_leaf` in
`signing/src/sigma.rs`).

## VRF verification in the guest

Each VRF family prints the guest's cycles per verification of a committed proof. A cycle here is an instruction or a
syscall; the figure is the run less an empty run, divided by the 9 proofs per suite.

| Suite | Cycles per verification |
|---|---|
| ECVRF-EDWARDS25519-SHA512-TAI | 1,719,548 |
| ECVRF-EDWARDS25519-SHA512-ELL2 | 1,745,443 |
| ECVRF-ED25519-SHA512-Elligator2 (draft-03) | 1,646,979 |

The figures include the one-exponentiation Elligator maps. None of the suites calls the `ED_ADD` precompile:
verification combines its points with curve25519-dalek's `vartime_multiscalar_mul`, which SP1's patch leaves in
software. Only decompression (`ED_DECOMPRESS`) is accelerated.
