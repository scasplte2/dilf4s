# dilf4s SP1 guest harness

SP1 v6 guests that run the Rust SDK (`dilf4s-proofs`, `dilf4s-signing` without `sign`, and `dilf4s-jlvm`) on
`riscv64im-succinct-zkvm-elf`, built with every SP1 patch:

- `dilf4s-zk-guest`, the harness. The host executes it over the shared vectors and checks the outcomes.
- `dilf4s-zk-jlvm-call`, one JLVM call. It commits the call's statement (`docs/specs/zk-jlvm.md`), and is the program
  that is proved: see "Proving a JLVM call".

Tests and CI use execute mode only: nothing is proved there.

The SDK's own CI build targets `riscv64imac-unknown-none-elf`. That target has `target_os = "none"`, so it never
compiles the patches' `target_os = "zkvm"` code. This harness compiles that code and runs it.

## Layout

There are three cargo roots, each with its own `[workspace]` and `Cargo.lock`:

| Directory | What it is |
|---|---|
| `cases/` | `dilf4s-zk-cases`, a `no_std` crate. It holds what is computed (`run`, the issue #33 leaf forms, and `call`: a JLVM call's statement). With the `vectors` feature it also reads each case and its expected outcome from `shared/vectors`. Both the guest and the host build it. |
| `guest/` | The SP1 programs, `src/main.rs` (the harness) and `src/bin/jlvm_call.rs`. Its workspace root is `sdk/` (`sdk/Cargo.toml`), which holds the `[patch.crates-io]` section, so the patches reach only the guest, and its lock, `sdk/Cargo.lock`. |
| `host/` | Builds the guest with `sp1-build`, `--locked`, from `build.rs`. Executes it with `sp1-sdk`'s light prover, and proves a JLVM call (`src/bin/dilf4s-zk-jlvm.rs`). Has no patches, so its native side builds the same crates.io releases that `sdk/rust` uses. |

**The guest's ELF is reproducible.** The JLVM call's verifying key is a hash of its ELF, and
`shared/vectors/zk_jlvm_v1.json` pins it, so CI, a developer and the prover must build the same bytes wherever they
check out the repository and whatever their `CARGO_HOME`:

- `host/build.rs` remaps the checkout to `/dilf4s` and `CARGO_HOME` to `/cargo` in the paths the ELF embeds (panic
  locations).
- The guest's workspace root is `sdk/` because it contains every path dependency (`sdk/rust/{core,proofs,signing,jlvm}`
  and `cases/`). Cargo hashes a path package into its symbol names by its path relative to the workspace root when it
  is inside it, and by its absolute path otherwise, which made the ELF differ between checkouts.

Two checkouts at different paths, one with a fresh `CARGO_HOME`, built byte-identical ELFs.

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
cargo test --locked --release --manifest-path host/Cargo.toml -- --nocapture --test-threads=1
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
| VRF (TAI, ELL2, draft-03) | Every committed proof and rejection row of `vrf_v1.json`, one family per suite; also prints the guest's cycles and gas per verification |
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

- `sdk/Cargo.lock` resolves every patched crate to its `sp1-patches` tag and to nothing else. Cargo only warns when a `[patch]` entry does not apply, so this needs a test.
- k256 pulls in the ecdsa fork.
- `sp1-lib` and `sp1-zkvm` are both 6.10.0.

`host/tests/jlvm_call.rs` checks the JLVM call guest (`docs/specs/zk-jlvm.md`), in execute mode:

- **Guest against native.** For each call of `shared/vectors/zk_jlvm_v1.json`, the guest exits 0 and commits exactly
  the canonical CBOR of the statement native code computes, and that statement agrees with the call's `jlvm_v1.json`
  row (outcome and gas). The calls are a success that calls `ecrecover` twice (the one proved), a `Crypto` error from
  `ecrecover`, `OutOfGas` with a host binding, a sigma threshold, a binding read, a `Type` and an `Arithmetic` error,
  and a `Crypto` error with a cause. The proved call must reach the secp256k1 and Keccak precompiles.
- **No statement.** A call whose program's bytes are not a program's canonical form (not CBOR, no program, trailing
  bytes), whose data or binding is not canonical CBOR, or that names a binding twice, halts with exit code 1 and
  commits nothing.
- **Injectivity.** A binding to null and an unbound name, two names, the bytes `0x6161` and the text `"a"`, and two
  gas limits all give different statements; a program written with other whitespace gives the same one.
- **The vector file is current, strictly** (the policy, user decision 2026-11-08):
  - the guest's verifying key (SP1 `setup` on the light prover, about 20 s) must be the key `zk_jlvm_v1.json` pins.
    Any change to the code the guest links (`sdk/rust/{core,proofs,signing,jlvm}`, `cases/`, `guest/`,
    `sdk/Cargo.lock`) changes it, and the gate fails with both keys and the command that proves the call again. Give
    each checkout its own `CARGO_TARGET_DIR`: the host's build script watches the guest's sources by absolute path,
    so a target dir shared with another checkout can hand back that checkout's guest, and its key;
  - the pinned proof of the proved call must verify under the pinned key, with `dilf4s-proofs`' SP1 Groth16 verifier.
    It is never dropped: `vector` refuses to write a file without it, or with one that no longer verifies;
  - the rest of the file must be what the native statements give: a changed call is fixed with `vector`, which keeps
    the proof while it verifies. This is also checked on its own under the pinned key
    (`the_committed_calls_are_current_under_the_pinned_key`), so a stale statement shows even while a change to the
    guest has moved its key and the strict check fails first.
- **A proof fixture** (ignored by default): `DILF4S_ZK_PROOF=<dir>/proof-groth16.bin cargo test --release
  --manifest-path host/Cargo.toml --test jlvm_call -- --ignored` checks a fresh proof before it goes into the file.

`sdk/rust/jlvm/tests/zk_jlvm.rs` and the Scala `ZkJlvmVectorsSuite` recompute each statement from the spec alone and
verify the pinned proof through `groth16_verify` (and that it proves nothing else); both fail if there is none. So
the `rust` and `scala` CI jobs check the proof too.

**Proving is not CI's job.** Only euler proves (below). GitHub's free hosted runner cannot: a core proof of
`{"+": [1, 2]}` was OOM-killed there at a 15 GB cap, with every SP1 worker pool at 1 (measured 2026-11-08); on
cavendish it peaks at 24 GB (core) and 30 GB (compressed). A change that moves the key is therefore proved again on
euler, and its PR carries the new `zk_jlvm_v1.json`. When two such PRs are open, the one that merges second proves
again after its rebase.

## Proving a JLVM call

`host/src/bin/dilf4s-zk-jlvm.rs` proves a call with `sp1-sdk` v6.10.0 on a local prover, never a network one (the
SDK's `network` feature is off):

```sh
cargo run --release --manifest-path host/Cargo.toml --bin dilf4s-zk-jlvm -- execute --case "<name>"   # or --call <file>
cargo run --release --manifest-path host/Cargo.toml --bin dilf4s-zk-jlvm -- prove --case "<name>" --mode core|compressed|groth16 --out <dir>
cargo run --release --manifest-path host/Cargo.toml --bin dilf4s-zk-jlvm -- vector [--proof "<name>=<dir>/proof-groth16.bin"]
```

- **A call** is a `zk_jlvm_v1.json` call by name, or a JSON file `{"program": text, "data": text, "env": {name: text},
  "gasLimit": n}` of JLVM surface texts. The host parses the texts and writes the guest's stdin in canonical form
  (`Call::of_surface`, `docs/specs/zk-jlvm.md` §1): the guest reads no surface text.
- **`prove`** writes `proof-<mode>.bin` (`SP1ProofWithPublicValues::save`), `public-values.hex`, `vkey.txt`,
  `elf-sha256.txt` and, for Groth16, `proof-bytes.hex` (`SP1ProofWithPublicValues::bytes`, what `groth16_verify`
  takes). It executes the call first, and checks the proof with SP1's own verifier and its public values against the
  execution.
- **Modes.** In sp1-sdk v6.10.0 a request's mode decides how far it goes (`ProveRequest::core`, `compressed`,
  `groth16`, `src/prover/prove.rs`), and each request proves from the start: `groth16` is the core proof, compressed,
  shrunk, wrapped and proved in gnark in one request (`sp1-prover` `worker/prover/recursion.rs`, `run_groth16`).
  The SDK has no call that wraps an earlier compressed proof.
- **Provers.** Built with `--features cuda`, `prove` uses the CUDA prover (`ProverClient::builder().cuda()`), unless
  `--cpu`. The whole request, Groth16 included, then runs in `sp1-gpu-server` (`sp1-cuda` `client.rs`,
  `Request::ProveWithMode`), which `sp1-cuda` downloads to `$HOME/.sp1/bin` and starts (`server.rs`). The CUDA
  toolkit is not needed to build.
  - **The server needs no Docker.** The v6.10.0 release binary (`sp1_gpu_server_v6.10.0_x86_64.tar.gz`, inspected,
    not run) is a Go and cgo build with gnark linked in, and no reference to Docker or icicle.
  - **It needs the CUDA runtime, `libcudart.so.12`,** dynamically linked, besides the driver's `libcuda.so` that WSL
    provides in `/usr/lib/wsl/lib`. The runbook checks for it first.
- **The CPU prover** runs gnark in Docker, or, with `--features native-gnark`, built from Go with cgo and bindgen
  (which needs libclang) (`sp1-recursion-gnark-ffi` `build.rs`, `src/ffi/`).
- **`vector`** rewrites `shared/vectors/zk_jlvm_v1.json`: each call's native statement and public values, the
  guest's key, and each `--proof` (checked to verify under that key and to state its call) or the committed proof.
  It refuses to write the file without a proof of the proved call that verifies.

The circuit artifacts (about 8 GB, `sp1-prover` `build.rs`, `try_install_circuit_artifacts`) download on first use
to `$SP1_GROTH16_CIRCUIT_PATH`, by default `$HOME/.sp1/circuits/groth16/v6.1.0`, unless that directory has its
`.complete` marker.

### On euler (CUDA, RTX 5090)

euler is the user's own machine; dilf4s uses its defaults, without a duplicate toolchain:
- `~/.sp1` is dilf4s's: SP1 v6.10.0 (`cargo-prove`, the `succinct` toolchain, `sp1-gpu-server`, which `sp1-cuda`
  downloads to `$HOME/.sp1/bin` on first use) and the one set of Groth16 circuit artifacts, `~/.sp1/circuits/groth16/
  v6.1.0` (about 8 GB; its `groth16_vk.bin` is byte for byte the one sp1-verifier 6.10.0 ships).
- `~/.rustup` and `~/.cargo` are the user's own: rustup adds only the 1.99.1 toolchain `rust-toolchain.toml` pins, and
  `sp1up` replaces the `succinct` toolchain in place.
- The code is euler's one checkout of dilf4s, `~/repos/dilf4s`: no second checkout or worktree. It builds into one
  `CARGO_TARGET_DIR` inside it, `target` (the guest into its `elf-compilation`). Logs go to `/tmp/dilf4s-logs`, and
  the proof to `/tmp/dilf4s-out` until its vector is committed.

No `sudo`, apt or Docker: the CUDA prover needs none. The GPU must be free: ollama runs there, and a loaded model (up
to 22 GB) leaves too little of the 5090's 32 GB, so check first and never unload someone else's model. Run one proof
at a time. Wrap each remote command in `timeout` (`nvidia-smi` can hang right after a boot), and run long steps under
`setsid nohup` with a log: euler's SSH drops for seconds at a time. Each check below stops the run; report what it
says rather than working around it.

```sh
# 0. on cavendish: ship the branch to prove (no remote is needed on euler). BRANCH names it, here and on euler
BRANCH=${BRANCH:?set BRANCH to the branch to prove}
git bundle create /tmp/dilf4s.bundle "$BRANCH"
scp /tmp/dilf4s.bundle euler:/tmp/dilf4s.bundle

# 1. on euler, in every shell: the branch, the PATH (non-interactive shells lack ~/.cargo/bin) and one target dir
BRANCH=${BRANCH:?set BRANCH to the branch to prove, as in step 0}
mkdir -p /tmp/dilf4s-logs /tmp/dilf4s-out
export PATH="$HOME/.sp1/bin:$HOME/.cargo/bin:/usr/lib/wsl/lib:/home/linuxbrew/.linuxbrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
export PROTOC=/home/linuxbrew/.linuxbrew/bin/protoc CARGO_TARGET_DIR="$HOME/repos/dilf4s/target"
timeout 15 nvidia-smi --query-gpu=memory.used,utilization.gpu --format=csv || { echo "the GPU does not answer: stop"; exit 1; }
[ "$(curl -s -m 5 localhost:11434/api/ps)" = '{"models":[]}' ] || { echo "ollama holds a model on the GPU: stop"; exit 1; }
pgrep -x sp1-gpu-server && { echo "another sp1-gpu-server holds /tmp/sp1-cuda-0: stop"; exit 1; }
command -v git curl tar gcc >/dev/null || { echo "missing git, curl, tar or gcc: stop"; exit 1; }
[ -f "$(dirname "$(readlink -f "$PROTOC")")/../include/google/protobuf/empty.proto" ] \
  || { echo "protoc has no well-known types: stop"; exit 1; }
ldconfig -p | grep -q 'libcudart.so.12 ' || { echo "no libcudart.so.12, which sp1-gpu-server links: stop"; exit 1; }

# 2. once: SP1 v6.10.0 into ~/.sp1, by the sp1up script at the v6.10.0 commit (Rust 1.99.1 comes with the first build)
curl -sSfL https://raw.githubusercontent.com/succinctlabs/sp1/d83ed91f96701209613720708f36ffce0e98ed33/sp1up/sp1up -o /tmp/sp1up
timeout 1800 bash /tmp/sp1up --version v6.10.0 && rm /tmp/sp1up

# 3. the code, in the one checkout: only if it has no tracked changes and no rebase or merge in progress. The bundle's
#    head is checked out detached, so the same commands work on every run, and the head is checked
cd ~/repos/dilf4s || { echo "no ~/repos/dilf4s: stop"; exit 1; }
[ -z "$(git status --porcelain | grep -v '^??')" ] && [ ! -d .git/rebase-merge ] && [ ! -d .git/rebase-apply ] \
  && [ ! -f .git/MERGE_HEAD ] || { echo "~/repos/dilf4s has work in progress: stop"; exit 1; }
git fetch /tmp/dilf4s.bundle "$BRANCH" && git checkout --detach FETCH_HEAD \
  || { echo "the bundle did not check out: stop"; exit 1; }
git log --oneline -1   # must be the head the bundle was made from
rm /tmp/dilf4s.bundle
cd sdk/rust/zk && ./check-toolchain.sh || { echo "the toolchain is not SP1 v6.10.0's: stop"; exit 1; }

# 4. euler must build the guest CI builds, or the proof is of another program. EXPECT is that key without 0x: after a
#    change to the guest, the one the failing gate names as "the guest built here"; otherwise the pinned programVKey
EXPECT=${EXPECT:-$(sed -n 's/.*"programVKey": "\([0-9a-f]*\)".*/\1/p' ../../../shared/vectors/zk_jlvm_v1.json)}
V=$(timeout 7200 cargo run --release --features cuda --manifest-path host/Cargo.toml --bin dilf4s-zk-jlvm -- vkey \
  | sed -n 's/^vkey 0x//p')
echo "vkey $V, expected $EXPECT"
[ -n "$V" ] && [ "$V" = "$EXPECT" ] || { echo "euler's guest key is not the expected one: stop"; exit 1; }

# 5. prove (the tiny call first, the first time), then check the proof with dilf4s's verifier
C="contract multisig_wallet: 2 of 3 owners sign"
timeout 14400 cargo run --release --features cuda --manifest-path host/Cargo.toml --bin dilf4s-zk-jlvm -- \
  prove --case "$C" --mode groth16 --out /tmp/dilf4s-out || { echo "the proof failed: stop (and run step 7)"; exit 1; }
DILF4S_ZK_PROOF=/tmp/dilf4s-out/proof-groth16.bin timeout 3600 cargo test --release --features cuda \
  --manifest-path host/Cargo.toml --test jlvm_call -- --ignored --nocapture a_proof_fixture \
  || { echo "dilf4s does not verify the proof: stop (and run step 7)"; exit 1; }

# 6. the vector; copy it back, and commit it on cavendish
timeout 3600 cargo run --release --features cuda --manifest-path host/Cargo.toml --bin dilf4s-zk-jlvm -- \
  vector --proof "$C=/tmp/dilf4s-out/proof-groth16.bin" || { echo "the vector was not written: stop (and run step 7)"; exit 1; }
#    on cavendish: scp "euler:repos/dilf4s/shared/vectors/zk_jlvm_v1.json" shared/vectors/

# 7. clean up: nothing may keep running. sp1-cuda starts its server as a child that is killed when the prover exits
#    (kill_on_drop, server.rs); stop a leftover only if it is this run's binary. The toolchains, the checkout and its
#    target dir stay, so the next proof is quick; the proof files go once the vector is committed.
for pid in $(pgrep -x sp1-gpu-server); do
  [ "$(readlink -f "/proc/$pid/exe")" = "$(readlink -f "$HOME/.sp1/bin/sp1-gpu-server")" ] && kill "$pid"
done
rm -rf /tmp/dilf4s-out
du -sh ~/repos/dilf4s/target ~/.sp1
```

## The patches

The patches are pinned in `sdk/Cargo.toml`, the guest's workspace root, at their SP1 6.10.0 tags. One exception is
bls12_381: `dilf4s-signing` already depends on that patch's source directly. Commits are shown as resolved in
`sdk/Cargo.lock`.

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

Each VRF family prints the guest's cost per verification of a committed proof: the run less an empty run, divided by
the 9 proofs per suite. A cycle is an instruction or a syscall. Gas is SP1's proving-cost estimate, which also counts
the precompiles' rows, so it is the fairer figure once verification leans on precompiles.

| Step | TAI cycles | TAI gas | ELL2 cycles | ELL2 gas | draft-03 cycles | draft-03 gas |
|---|---|---|---|---|---|---|
| Before: `vartime_multiscalar_mul` for `U` and `V` | 1,719,548 | 1,637,457 | 1,745,443 | 1,660,900 | 1,646,979 | 1,558,030 |
| 1. `U` as one double-base multiplication | 1,076,338 | 1,061,968 | 1,099,594 | 1,083,023 | 1,003,108 | 984,971 |
| 2. `V` as two variable-base multiplications | 433,495 | 485,839 | 454,043 | 504,671 | 359,717 | 411,454 |
| 3. No re-encoding to decode or to hash `Y` | 278,937 | 320,966 | 338,074 | 380,955 | 321,027 | 370,155 |

Each row includes the ones above it; all of them give the same outcomes as native code on every VRF row.

1. **`U = s B - c Y`** is `vartime_double_scalar_mul_basepoint` (`equation_base` in `signing/src/vrf.rs`), on every
   target. SP1's patch runs it on `ED_ADD`, about 700 calls per verification. Natively it costs no more than the
   multiscalar multiplication: about 34 µs against 36 µs with RFC 9381's 128-bit `c`, and the same 35 µs with
   draft-03's full-width `-c`.
2. **`V = s H - c Gamma`** is two variable-base multiplications in the guest (`lincomb`), which the patch also runs
   on `ED_ADD`. The patch leaves `vartime_multiscalar_mul` in software. Natively `V` stays one multiscalar
   multiplication: about 35 µs, where the two constant-time multiplications take about 68 µs. The choice is gated
   on `target_os = "zkvm"` and `target_vendor = "succinct"`.
3. **Re-encoding.** Each `compress` is a field inversion in software, about 38,000 cycles in the guest (about 4 µs
   natively). Strict decoding used to decode and then re-encode to compare; it now checks `y < p` and refuses the
   sign bit at `y = ±1`, the only points with `x = 0`, which refuses exactly the same encodings
   (`strict_decoding_refuses_exactly_what_re_encoding_does`). Hash-to-curve hashes the key's bytes in place of
   re-encoding `Y`: a key that decodes and is not of small order re-encodes to itself, under both suites' rules
   (`a_decoded_key_re_encodes_to_itself`). This saves three inversions per verification for ELL2, one for draft-03,
   and for TAI one more per hash-to-curve attempt.
Natively, a whole verification got faster too: TAI from about 109 µs to 96 µs, ELL2 from 108 µs to 97 µs, draft-03
from 99 µs to 97 µs (the fastest of 600 passes over 16 proofs, several interleaved runs, one pinned core of a
16-core x86-64 under a load average of 17 to 28; the multiplication timings above come from the same method). The
load makes single runs vary by 10% or more, so read these as ratios.

**Declined: `sys_bigint` for the Elligator maps (issue #69).** Both maps spend one exponentiation, `x^((p - 5) / 8)`:
251 squarings and 11 multiplications. Running those 262 multiplications on the `UINT256_MUL` precompile through
`sys_bigint` was measured on top of row 3. It saved about 11,200 cycles (3.3–3.5%) and 16,100 gas (4.2–4.4%) per ELL2
or draft-03 verification, and nothing for TAI. That needed an `unsafe` FFI call and an exception to
`unsafe_code = "forbid"`, so the maintainer declined it. `dilf4s-signing` keeps fiat-crypto, with no unsafe code, on
every target.

The four inversions that remain encode `H`, `U`, `V` and `8 Gamma`, about 150,000 cycles, roughly half of what is
left. Each encoding is needed: the challenge hashes the first three, and `beta` hashes the last. The remaining option
is hinted point compression. In the guest, the patch's `From<EdwardsPoint> for Ed25519AffinePoint` normalises a
point with a prover-hinted, checked `Z` inverse in place of the software inversion. curve25519-dalek 4 keeps the
coordinates private, so using it would mean a guest-only `sp1-lib` dependency in `dilf4s-signing`, coupled to an API
that only the patch has. It is not done.
