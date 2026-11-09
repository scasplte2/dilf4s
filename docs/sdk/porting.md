# Porting dilf4s to a new language

This is for whoever starts the next port: Go (native), Java (a layer over the Scala library) or Python (bindings over
the Rust SDK). It says what "done" means, in what order to build, which vectors gate each step, the invariants you
must keep, and the rules the TypeScript and Rust ports learned in review. `parity.md` is the checklist: a port is
done when it has a column there and every row in it is gated.

Paths use `parity.md`'s shorthand: `rs/` is `sdk/rust/`, `ts/` is `sdk/ts/packages/`, and `<module>/m/` and
`<module>/t/` are the Scala module's `src/main/scala/xyz/kd5ujc/<module>/` and `src/test/scala/xyz/kd5ujc/<module>/`.

Read before you write code, in this order: `docs/specs/canonical-encoding.md`, `docs/specs/conformance.md`,
`shared/schemas/README.md`, then the spec of the phase you are on. The specs are normative. When this document and a
spec disagree, the spec wins, and this document has a bug.

## 1. Phases and their gates

Build in this order. Each phase is one or more PRs, and each must pass its gates before the next phase starts,
because later phases load their vectors through earlier phases' code.

### How every phase loads its vectors

1. Read the file from `shared/vectors/` as text.
2. Parse it with **your own** canonical JSON parser, from phase 2. Never use the language's stock JSON library: Go's
   `encoding/json`, Python's `json` and Java's usual parsers read numbers as doubles, and most let the last of two
   duplicate keys win.
3. Decode it into the **generated** vector type (`<name>-vectors.cddl`, from phase 1). A vector naming a kind your
   schema does not know then fails to load.
4. Build each `built-value` (a leaf and steps; `conformance.md` §6, "Inputs") with sharing. Never encode, render or
   copy one whole: some rows describe values of 64 MiB or more in a few bytes. See `rs/core/tests/common/mod.rs:46`
   and `rs/zk/cases/src/vectors.rs:585`.
5. Compare each outcome by its kind and its normative fields only (`conformance.md` §3). Never compare a message.
6. Check kind coverage over the generated kind list: every kind has a row, or is one of `conformance.md` §6's
   exemptions.

The reference loaders to copy are `rs/core/tests/common/mod.rs:22` and `ts/core/test/vectors.ts:41`.

### Phase 1: a `models-gen` emitter

**Build.** An emitter in `models-gen` beside `TsEmitter` and `RustEmitter`, for every construct of the profile
(`shared/schemas/README.md`, "The profile"):
- records, choices and arrays, with codecs to and from the canonical value;
- error families, as the language's error type, each with a closed kind type;
- named-text choices (`jlvm-op`), as closed types with `wire` and `fromWire`;
- constants (`limits.cddl`), in every width they fit, so no site casts;
- vector types;
- the four named pattern validators (`Profile.Patterns`), implemented in your core;
- `preview` fields, typed as your core's `Preview`;
- schema comments, as doc comments.

**Gates.**
- The committed output (for Go, `sdk/go/core/generated/`) equals the emitter's. This is a new `SdkSourcesSuite`
  case, like "committed Rust models match shared/schemas"
  (`models-gen/src/test/scala/xyz/kd5ujc/modelsgen/SdkSourcesSuite.scala:63`). `SdkSources` gains a field, and
  `SdkMain` (`models-gen/src/main/scala/xyz/kd5ujc/modelsgen/SdkMain.scala:101`) gains an output directory.
- A fixture module compiles every profile feature, like `sdk/rust/fixtures` (`SdkSources.RustFixtureSchemas`).
- The emitter refuses the same schemas the others refuse (`SchemaFeaturesSuite`, `CanonicalEmitterSuite`). For
  example, a name that collides after case conversion is an error in every language
  (`models-gen/src/test/scala/xyz/kd5ujc/modelsgen/SdkSourcesSuite.scala:101`).

**CI.** The staleness check runs in the `scala` job, which runs on every change. The fixture module compiles in the
new language's job.

**Java and Python.** Java wraps Scala, and Python wraps Rust, so their values come from the wrapped library. They
still need generated error families, kinds, limits and vector types in their own language: Java records and sealed
interfaces, Python frozen classes with a `.pyi`. Otherwise the kinds are hand-copied, and a kind added to a schema
compiles everywhere but there. Generate the mapping from the wrapped library's generated types too.

### Phase 2: core

**Build.** `canonical-encoding.md` whole: the value model with its width caps and cached weight and depth; CBOR
encode and decode in the refusal order of §2; `encodeWithin` and `encodedLengthWithin`; the JSON parser in §3's
order; `renderWithin`; tagged hashing over all seven algorithms; domain tags; digests from hex; `expand_message_xmd`;
the value commitment; content hashes and commitments within a caller's byte limit (`conformance.md` §4; Rust
lacks them, `parity.md` GAP-21); `Preview`. Implement the encoding ceiling (`MaxEncodedBytes`) as the reference does until
`parity.md` GAP-1 is decided: `conformance.md` §4 allows an SDK without one, as Rust is, but then a content hash
past the ceiling is a digest where the reference refuses.

**Vectors that must pass.**
- `canonical_encoding_v1.json`: all 11 sections. Run `encodedLengthWithin` on every CBOR `limits` row as well as
  `encodeWithin`; the existing SDKs do not yet (`parity.md` GAP-20).
- `diagnostics_v1.json`.
- `merkle_v1.json` `valueCommitments`.
- `versioned_trie_v1.json` `stateKeys`, through the generated state-key codec's `schema` outcome.
- Every vector file round-trips through its generated type (`rs/core/tests/vector_schemas.rs:67`).
- Kind coverage: `CanonicalError`, `HashError` and the expansion refusals.

**CI job shape.** One job per language, behind the `changes` filter in `.github/workflows/ci.yml`. For Go:

```yaml
go:
  needs: changes
  if: needs.changes.outputs.go == 'true'   # filter: sdk/go/**, shared/**, .github/workflows/ci.yml
  runs-on: ubuntu-latest
  defaults: { run: { working-directory: sdk/go } }
  steps:
    - uses: actions/checkout@v4
    - uses: actions/setup-go@v5            # go-version-file: sdk/go/go.mod
    - run: test -z "$(gofmt -l .)"
    - run: go vet ./...
    - run: go run honnef.co/go/tools/cmd/staticcheck@<pinned> ./...
    - run: CGO_ENABLED=0 go test ./...
    - run: CGO_ENABLED=0 go test -tags purego ./...   # the libraries' assembly-free paths
    - run: GOARCH=386 CGO_ENABLED=0 go test ./...      # int is 32 bits there
    - run: CGO_ENABLED=1 go test -race ./...           # the race detector needs cgo; the library never does
    - run: ./scripts/fuzz-smoke.sh                     # each decoder's Fuzz target, about 30 s each
    - run: ./scripts/forbidden.sh                      # no unsafe, reflect, "C", panic or recover in library code
    - run: git diff --exit-code -- go.sum
```

`go test -fuzz` runs one target per invocation, so the smoke script loops over them. Commit each decoder's seed corpus
under `testdata/fuzz/`, taken from the vectors, so that a plain `go test` replays it.

### Phase 3: signing

**Build.** `signing.md` (every scheme, with its encodings and refusals; BIP-39, SLIP-10, BIP32, BIP32-Ed25519;
paths), `vrf.md` (three suites), `kes.md` (three constructions), `sigma.md` (decoding and verification; proving if
the port is a client that proves; see `parity.md` GAP-3), and ecrecover (`jlvm.md` §6). Every operation on a secret
(signing, key derivation, VRF proving, KES signing) runs on a constant-time backend. Where the language has no vetted
one, the port verifies only and says so; in Go that is secp256k1 and BLS12-381 today (section 4).

**Vectors that must pass.**
- `signing_v1.json`: all 10 sections.
- `vrf_v1.json`: all 3 sections.
- `kes_v1.json`: both sections.
- `sigma_v1.json` and `sigma_dilf4s_v1.json`, every case; and `sigma_refusals_v1.json`, all 3 sections.
- The reference corpora of `parity.md` section 6: RFC 8032, BIP32-Ed25519, BIP-39, eth2 BLS, VRF (RFC 9380 and 9381,
  draft-03, and the libsodium oracle corpus), and Cardano KES.
- Kind coverage: `CryptoError`, `KeyError`, `PathRefusal`, `VrfError`, `KesError` and `SigmaError`.
- A size test against `crypto.cddl` (`ts/signing/test/sizes.test.ts:23`). Rust lacks one (GAP-13); don't copy that.

**CI.** The same job. The BLS, VRF-corpus and sigma rows are heavy, so give them generous timeouts.

### Phase 4: proofs

**Build.** `merkle.md`, `trie.md` (with the linear listing checks of §4), `versioned-trie.md` (keys, version keys,
proofs, deltas, the snapshot reader, the replica), and `zk.md` (BN254, Groth16, SP1, Poseidon and its tree).

**Vectors that must pass.**
- `merkle_v1.json`, `trie_v1.json`, `versioned_trie_v1.json`, `poseidon_v1.json` (`builds` through a library builder,
  not a test-local one: GAP-9) and `zk_v1.json`, every section.
- `refusals_v1.json`, every check.
- Kind coverage: `MerkleError`, `TrieError`, `VersionedTrieError`, `StateKeyError`, `PoseidonError`,
  `PoseidonMerkleError`, `ZkError` and `Groth16Error`, with the exemptions of `conformance.md` §6.
- Work counts: listing checks linear in proof weight plus bounds (`trie.md` §4), and no peer key in a hash table
  (below).

### Phase 5: the JLVM

**Build.** `jlvm.md` whole: the surface parser and exact rendering; the canonical form, its identity, and reading it
back in §1's order; compilation; the iterative machine; all 93 operations; gas schedule v1, charged before the work.

**Vectors that must pass.**
- `jlvm_v1.json` `cases`: value CBOR, or kind, `op` and `cause`; gas; warning count. Also `aliases` and `decodings`.
- `zk_jlvm_v1.json`: recompute every statement, and verify the pinned Groth16 proof through `groth16_verify`.
- The spec's gas table equals your schedule, row for row (`rs/jlvm/tests/gas_table.rs:74`).
- Every generated operation has exactly one handler (`ts/jlvm/test/dispatch.test.ts:33`).
- Port the metering regressions (`rs/jlvm/tests/metering.rs`, `ts/jlvm/test/metering.test.ts`). They count work;
  they never time it.

## 2. The invariants

Read each one where it is stated. The summaries here are pointers, not substitutes.

| Invariant | Where it is stated | The rule, in short |
|---|---|---|
| Caller bytes | `canonical-encoding.md` §1, "Caller bytes" (line 117) | Every caller byte argument passes once through one boundary. It is measured by its internal length, and kept as a view only when nothing can change it; otherwise it is copied once. After the boundary, nothing reads the caller's object again. |
| Caller code | `canonical-encoding.md` §1, "Caller code" (line 135) | An exception from the caller's own code (a getter, an iterator, a callback) propagates unchanged, after any iterator the SDK opened is closed. The SDK's guarantees are about the data, however it is shaped. |
| Immutable models | `canonical-encoding.md` §1, "Immutable models" (line 144) | Generated models, codecs, classes and shared refusals cannot be changed at run time by any caller, before or after import. |
| The boundary refuses nothing by size, and over-long is its own type | `canonical-encoding.md` §1, line 120 onwards | The boundary refuses only what is not bytes, as `TypeMismatch`. Past its bound, a field is not copied and is not refused there. It flows on as an `Oversized` value (TS: `ts/proofs/src/check.ts:46`) that nothing hashes, compares, encodes or returns. It is refused at the stage the specs pin for a shorter wrong-sized one. |
| Bounded work | `conformance.md` §4; `canonical-encoding.md` §2 and §3 | A refusal costs no more than the spec says: weight is checked from the cache in O(1), before decoding (`conformance.md` §5), and digits are counted on the text before any conversion. |
| No recursion on input shape | no general statement; see `trie.md` §5 (line 171, explicit stack), `sigma.md` §3 (line 115, worklist) and `jlvm.md` §7, "Drivers" (line 827) | Walk caller data with explicit stacks. The ports did so everywhere (PRs #9, #40, #114). A spec sentence would help (`parity.md` GAP-15). |
| Refusal order | `canonical-encoding.md` §2, "Refusal order" (line 261) and §3 (line 315); `conformance.md` §5, "Decoding order" (line 140) and "Codec order" (line 154); `versioned-trie.md` §2 and §5; `sigma.md` §3; `signing.md` §3; `jlvm.md` §1, "Reading a canonical form" (line 97) | One pass, in the stated order: the first fault met is the refusal. The vectors pin every pair of faults that an order could answer differently. |
| Typed refusals, nothing thrown | `conformance.md` §6, "Generated error types" (line 209); `shared/schemas/README.md`, error families | Every refusal is a kind of a generated family. Nothing throws or panics, and no message is normative (`conformance.md` §2). |
| Wrong-typed arguments | `canonical-encoding.md` §1, line 64 | Where the type system can be bypassed (JavaScript; Python; Java through erasure or `null`), a wrong-typed argument is `TypeMismatch` in the result, checked in O(1) before it is read. Where the compiler rules it out (Scala, Rust, Go), there is nothing to check, but `nil` and zero values are values (section 4). |
| Results bounded at 2^24; host data passes through | `jlvm.md` §2, "Bounds" (line 158, and line 172 onwards) | Every value an operation builds is checked against the result weight, a map it updates included (`set`, `unset`, `merge`). Input data and host bindings are not: a read, or an operation returning an argument or a part of one, passes it through as it is. |
| A null host binding is a binding | `zk-jlvm.md` §2, "Bindings are not a map" (line 116); the `dilf4s host bindings:` rows of `jlvm_v1.json` | A name bound to null reads as null; an unbound name is `Malformed`. So bindings are never a canonical map, which would drop the null. `jlvm.md` §4.1 does not state this (GAP-15). |
| Full lowercase hex version keys | `versioned-trie.md` §2, "A version in a refusal" (line 70); `conformance.md` §3 and §8 | A version in a refusal is its key's full lowercase hex, never truncated and never the bytes. The key's width is checked against the configured width (at most 1024) before any refusal names it. |
| Gas charged before the work | `jlvm.md` §7, "Every charge is taken before the work it pays for" (line 695), and "Charge points" (line 700) | A charge that would pass the limit stops the evaluation, with the whole limit used and none of the unpaid work done. |
| Determinism | `jlvm.md` (the five inputs, and §4.1 line 314); `jlvm.md` §2, "Text" (line 193); `trie.md` §6 (line 193); `storage.md` §3 (line 69); `zk.md` §4 (line 114); `versioned-trie.md` §7 (line 300) | A result depends on the program, data, bindings, language and schedule alone: no clock, no locale (case mapping is ASCII only), no hash-map iteration order. Caller- or peer-chosen keys are compared, never hashed. |
| Limits | `shared/schemas/limits.cddl`; `conformance.md` §6, "Limits" (line 337) | Every limit is read from the generated constant at its check. `parity.md` section 4 lists each site. |

## 3. Lessons from review

Each rule below is a bug class that reached Codex review more than once, or that review caught at a high severity.
The PR numbers are on GitHub; the bodies quote the findings.

| Rule | What happened | Where the fix lives |
|---|---|---|
| **Own caller bytes once, before any read.** Measure, then keep a view only of memory nothing else can change; otherwise copy once. Never read the caller's memory twice. | A worker thread flipping a shared key byte made `trieVerify` prove a present key absent in about 70% of calls. A `SharedArrayBuffer` given `ArrayBuffer.prototype` was read in place. (#122) | `ts/core/src/bytes.ts:121`; the source scan `ts/core/test/byte-boundary.test.ts` |
| **Anything hashed in several passes is copied first.** | TAI VRF hashes `alpha` once per try-and-increment counter. A worker alternating `alpha` made a proof that neither message selects verify. BLS `fastAggregateVerify` read the message after the keys, so a key getter could swap it. (#122) | `ts/signing/src/vrf.ts:317`; `jlvm.md` §7 (hash the prefix once, then finish copies) |
| **Never hash a stand-in.** An over-long field is a type of its own, not a placeholder value. | An over-long versioned root became `size + 1` zero bytes, which were hashed. A crafted history committing to `bytesHash(33 zero bytes)` made `verifyVersions` true for a 100-byte claim. (#122) | `ts/proofs/src/check.ts:46` (`Oversized`) |
| **Keep aggregates exact and incremental. Never rescan a container per update.** | Stale depth bounds in the TS JLVM's persistent map made updates quadratic within gas (#123). Listing checks rebuilt each path from bit 0: a 4,000-deep chain took 9 to 16 s for gas that pays for about 40 ms (#97). `intersect` built every sort key eagerly, about 432 MB within the gas limit (#81). | `ts/jlvm/src/jvalue.ts:146` and `:166` (per-node max depth); `trie.md` §4, "Listing checks" (line 101) |
| **Apply a bound to exactly what the spec bounds, where it says.** | `missing_some` bounded the absent keys it discards, so a met minimum could be `Limit`; it returns `[]` however heavy they are. (#123; three `dilf4s lookups:` rows) | `jlvm.md` §2, line 172 onwards |
| **Every operation that builds a value checks the result bound, including removals.** | `unset` had no result bound in any of the three languages. (#123; five `dilf4s result bound:` rows) | `jlvm/m/PureOps.scala:97`, `rs/jlvm/src/ops.rs:95`, `ts/jlvm/src/ops.ts:689` |
| **Refuse in the normative order, and pin each pair of faults.** | TS read `var` paths after measuring, and Scala had no weight guard, so a canonical form's refusals differed (#123; `decodings` rows). The spec's bignum order disagreed with all three decoders, so the spec was fixed and both cases pinned (#110). Rust's versioned-proof decoding checked meaning before shape (#89). | `jlvm.md` §1 (line 97); `canonical-encoding.md` §2 |
| **DoS tests count work. They never time it.** | Timed gates were flaky on a loaded machine and hid regressions. Every gate now counts comparisons, hash calls or steps, through injected kernels, and fails on a planted regression. (#81, #84, #98) | `rs/jlvm/tests/metering.rs`; `ts/jlvm/test/linear.test.ts` |
| **Never key a hash map by caller- or peer-chosen data.** | Java `String` hash collisions made decoding 4,096 colliding keys take 124 to 137 ms. V8 hashes integer-like keys without a seed, so 32k of them took 6.7 s to insert (#86). Poseidon positions 2^64 apart hash alike in V8 (#98; the last `builds` row). | `trie.md` §6 (line 193); `storage.md` §3 (line 69); `zk.md` §4 (line 114) |
| **Run the vectors on every backend and build of the code that ships.** | SP1's patched curve25519-dalek refused a non-canonical `y` that ZIP-215 accepts, so 152 of 415 Ed25519 rows verified differently in the guest (#77). | `rs/zk/host/tests/guest_matches_native.rs` |
| **No caller can change shared state.** | Pushing `'evil'` into `JlvmOp.shape.texts` made an unknown operation parse. Replacing `JlvmProgram`'s version-field codec let a version-2 program decode (#125). | `canonical-encoding.md` §1, "Immutable models" |
| **A value built outside its constructor is refused, not trusted.** | A key, proof or `Program` built by calling a constructor from JavaScript passed type checks (#122, #123). In Go, the zero value of an exported struct is the same hazard. | `canonical-encoding.md` §1, line 96 onwards |
| **Weights and gas saturate; nothing wraps.** | A `CBytes` weight wrapped negative (#120, high). | `jlvm.md` §2 ("The total saturates at `2^63 - 1`") |
| **Bound text-to-number work before converting, and name caller numbers by bit length.** | A 262,000-digit integer took about 1 s on JDK 21 (#48). Refusals echoed caller digits (#44). | `canonical-encoding.md` §3; `conformance.md` §2 and §8 |
| **Bytes are flat.** | On the JVM, a loop could build a byte rope about 5,600 levels deep: one 15.5M-gas case took 212 s and overflowed the stack (#102). | `jlvm.md` §5.5, "Byte strings are flat" (line 454) |
| **Charge for comparisons that grow.** | A read inside callbacks searched every frame for its first key and paid one flat charge, so long shared prefixes bought unpaid work (#46). | `jlvm.md` §7, "Charge points" |
| **Check the weight before decoding anything shared.** | A canonical form of 2^60 leaves, built with shared parts, is refused in 32 ms, before it is expanded (#123). Shared values weigh up to 2^63 − 1 (#36, #39). | `conformance.md` §5; `jlvm.md` §1, step 2 |
| **Decode surrogate pairs by arithmetic.** | Rust combined surrogate pairs with OR instead of addition, so U+20000 parsed as U+10000 (#9). | `canonical-encoding.md` §3 |

## 4. Safe constructions per target

The user's rule for these targets: "these may not have functional approaches available, so using the most
appropriate, safe constructions in those languages is approved". Here is what that means in each language.

### Go (native)

- **Results.** Return `(T, error)`. Each generated family is a set of typed error structs that implement `error` and
  `Kind()`, so callers use `errors.As`. Never return a bare `errors.New` for a refusal: its kind would be lost.
- **No panics, `unsafe`, cgo or reflection in the library.** An index out of range is a panic, so bound every
  index. `recover` is not control flow. Enforce all of this with a script in CI (phase 2), not by review.
- **Encapsulation.** Unexported fields, with accessors. A struct's zero value is constructible by anyone, so every
  method refuses a zero value, the way TS refuses a forged instance.
- **Bytes are copied at the boundary, both ways.** A `[]byte` aliases its caller's array, and `append` may write
  into it. Use `bytes.Clone` on the way in for anything kept or read twice, and on the way out for anything owned.
- **Explicit stacks.** Nothing recurses on caller data: decoders, encoders, the machine, the trie walk.
- **Fuzzing.** `go test -fuzz` for every decoder (CBOR, JSON, each codec, trie proofs, versioned records, sigma
  propositions, JLVM canonical forms), seeded from the vectors.
- **Lint.** `go vet` and staticcheck, with no suppressions in library code.
- **No goroutines in the library.** Values are immutable after construction, so they are safe to share.
- **Arithmetic.** Use `math/bits` (`Add64`, `Mul64`) for checked and saturating arithmetic; Go integers wrap silently.
- **Secrets in constant time.** Signing, key derivation, VRF proving and KES signing handle secrets, so each must
  run on a constant-time path. Read the library's own claim before trusting it: functions named `VarTime*`
  (`filippo.io/edwards25519`) or `*NonConst` (decred) are variable time by name, and gnark-crypto disclaims
  constant time and side-channel resistance altogether. Verification handles only public data and may use any.

#### Go crypto libraries, and where their defaults differ from our specs

Maintenance was checked on pkg.go.dev on 2026-11-08, from the newest release each page showed (each page also
said a newer one exists):
- releases in 2026: `filippo.io/edwards25519`, `btcec/v2`, `dcrd/dcrec/secp256k1/v4`, `gnark-crypto`,
  `golang.org/x/crypto` and `golang.org/x/text`;
- in 2025: `gtank/ristretto255` and `filippo.io/nistec`;
- in 2024: `hdevalence/ed25519consensus` (small and stable; use it as a cross-check only) and go-iden3-crypto's
  `v0` line (a later major exists).

Pin exact versions in `go.mod`, and check the rows named here before you trust a default.

| Primitive | Library | Where the default differs from the spec | What you must do | Vector rows that catch it |
|---|---|---|---|---|
| SHA-256, SHA-512, HMAC, PBKDF2, HKDF | stdlib (`crypto/sha256`, `crypto/sha512`, `crypto/hmac`, `crypto/pbkdf2` and `crypto/hkdf` from Go 1.24) | none known | | `canonical_encoding_v1.json` `digests`, `taggedHash`, `expansions`; `signing_v1.json` `bip39` |
| SHA3-256, SHA3-512 | `crypto/sha3` (Go 1.24) or `golang.org/x/crypto/sha3` | none | | `digests` |
| Keccak-256 (pre-standard padding) | `golang.org/x/crypto/sha3.NewLegacyKeccak256` | `New256` is SHA3-256, which is not Keccak-256 (`canonical-encoding.md` §4) | use `NewLegacyKeccak256` for `Keccak-256`, `keccak256` and ecrecover addresses | `digests` and `taggedHash` Keccak-256 rows; JLVM `keccak256` and `ecrecover` cases |
| BLAKE2b-256, BLAKE2b-512 | `golang.org/x/crypto/blake2b` | none: `New256` is BLAKE2b at a 32-byte digest length, not a truncated BLAKE2b-512 | | `digests`; Cardano KES; sigma challenges |
| `expand_message_xmd` | none: build it on `hash.Hash` | | the over-long-DST rule (§4) and its refusal order | `expansions` |
| Ed25519 verify, ZIP-215 | `filippo.io/edwards25519` | stdlib `crypto/ed25519.Verify` is not ZIP-215. In its source it is cofactorless and compares `R` with the re-encoding of the computed point, so it refuses a non-canonical `R` that ZIP-215 accepts. | decode `A` and `R` with `Point.SetBytes`, which "accepts all non-canonical encodings of valid points"; read `S` with `Scalar.SetCanonicalBytes`; compute `k` with `SetUniformBytes(SHA-512(R ‖ A ‖ M))`; accept iff `[8]([S]B − R − [k]A)` is the identity. Check against `github.com/hdevalence/ed25519consensus`, which implements ZIP-215 on the same library. | `signing_v1.json` `verification` Ed25519 rows (#77: a non-ZIP-215 verifier differed on 152 of 415) |
| Ed25519 libsodium rule (Cardano KES leaves) | `filippo.io/edwards25519` | neither stdlib nor ZIP-215 matches | `S < L`; a canonical `A` (re-encoding equals input) not of small order; `[S]B − [k]A` computed cofactorless, encoding exactly to `R`, and not of small order (`signing.md` §1.1) | `verification` Ed25519-libsodium rows; `kes_v1.json`; the Cardano corpus |
| Ed25519 sign; Ed25519-BIP32 sign | stdlib `crypto/ed25519.Sign`; `filippo.io/edwards25519` | `ed25519.Sign` takes Go's 64-byte `PrivateKey` (seed ‖ public key), not the spec's 32-byte seed, and it and `NewKeyFromSeed` panic on any other length. Neither takes `kL ‖ kR`. `SetCanonicalBytes` refuses a `kL` at or above `L`. | check the seed is 32 bytes (else `InvalidEncoding`), expand it with `NewKeyFromSeed`, and keep the 64-byte form internal: the secret key's encoding stays the seed (§1). For BIP32, reduce `kL` with `SetUniformBytes(kL ‖ 0^32)`, use only the constant-time `ScalarBaseMult`, and follow §1.2. | `signing_v1.json` `signing`, `ed25519Bip32` |
| VRF: TAI, ELL2, draft-03 | none: build them on `filippo.io/edwards25519` and its `field` package | `SetBytes` is lenient, but TAI and ELL2 need RFC 8032 strict decoding, and draft-03 needs its own | TAI and ELL2: accept a point only when its re-encoding equals the input, which refuses a non-canonical `y` and `x = 0` with the sign bit set. draft-03: refuse `y ≥ p`, and accept that sign bit as the positive point (`vrf.md` §2). Hash `alpha`'s prefix once per verification (`jlvm.md` §7). | `vrf_v1.json`; RFC 9381, RFC 9380 and draft-03 examples; the libsodium oracle corpus |
| ristretto255 Schnorr | `github.com/gtank/ristretto255` | `Element.SetCanonicalBytes` is strict, but the identity decodes | refuse the identity for `PK` and `R`; refuse a zero secret scalar | `verification` and `keyDecoding` ristretto255 rows |
| secp256k1 ECDSA | `github.com/decred/dcrd/dcrec/secp256k1/v4` (under `btcec/v2`), for verification only | Signing is variable time on secrets: decred's `ecdsa.Sign` multiplies the secret nonce with `ScalarBaseMultNonConst`. `ParsePubKey` takes compressed, uncompressed and hybrid keys. `PrivKeyFromBytes` asks callers for `[1, N−1]`, without refusing outside it. `Signature.Verify` documents no low-S check. | check the length (33, else `InvalidEncoding`) and the tag (`02`/`03`, else `InvalidKey`) before parsing; check the secret scalar's range; build `r` and `s` from the 64-byte compact form with `ModNScalar.SetByteSlice` (it reports overflow), then refuse zero and `s > n/2` before verifying. Signing, BIP32 derivation (a secret key's public key) and anything else that multiplies a secret need a constant-time backend, and none is vetted in pure Go; until one is, the Go port verifies secp256k1 but does not sign or derive. That is a decision for the user. | `verification` secp256k1 rows (high-S, other tags); `keyDecoding` |
| BIP-340, tagged Schnorr | `github.com/btcsuite/btcd/btcec/v2/schnorr`, for verification only | `Sign` documents "a few remaining variable time aspects" that "make use of the private key and the generated nonce". It also uses an RFC 6979 nonce unless the `CustomNonce` option is given ("If this option isn't set, then rfc6979 will be used"), which is not BIP-340's aux-randomness nonce. `ParseSignature` refuses `r ≥ p` as an error but reduces `s` modulo `n` without refusing it, so on its own it accepts an `s ≥ n` that BIP-340 refuses. | write two adapters. BIP-340 takes a 32-byte message type, so another length cannot be expressed. Tagged Schnorr takes a message of any length and verifies BIP-340 over `taggedHash_SHA256("dilf4s/sig/bip340/v1", message)` (§1.4); the two never accept each other's signatures. In both, decoding a signature checks its length only (64), as Rust's does (`rs/signing/src/schnorr.rs:91`); verification answers `false` for `r ≥ p` or `s ≥ n`, checked before calling btcec, then uses `schnorr.ParsePubKey`, `ParseSignature` and `Verify`. Signing waits for the same constant-time backend as ECDSA; when it comes, it must use BIP-340's aux-randomness nonce (the vectors use `a = 42…42`). | `signing_v1.json` `signing` (byte for byte) and `verification` |
| ecrecover | decred `ecdsa.RecoverCompact` | its compact form puts the header byte first (27 + recovery id, plus 4 when compressed), and it documents no EIP-2 low-S rule | reorder `r ‖ s ‖ v`; accept `v` in {27, 28} or {0, 1}; refuse `r` or `s` of zero or not below `n`, `s > n/2`, and an `r` that is not an x-coordinate, all before recovery (`jlvm.md` §6). Don't use go-ethereum's `crypto`, which uses cgo libsecp256k1 by default. | the JLVM's 29 `ecrecover` cases (no native vector: GAP-4) |
| P-256 ECDSA | stdlib `crypto/ecdsa`; `filippo.io/nistec` for compressed points | `PrivateKey.Sign` is RFC 6979 only when its random source is nil, and it returns an ASN.1 DER signature, not the spec's 64-byte `r ‖ s` (`signing.md` §1). Nothing normalises to low-S, and `Verify` documents no low-S check. `crypto/ecdsa` has no compressed-key parser (`ParseUncompressedPublicKey` refuses one). | sign `SHA-256(message)` with a nil random source and `crypto.SHA256` as the hash option; decode the DER result, normalise `s` to `n − s` above `n/2`, and write `r` and `s` each as 32 big-endian bytes. To verify, decode the 64 bytes into `r` and `s`, refuse zero, values at or above `n`, and high-S, then call `ecdsa.Verify` on the digest; parse keys with `nistec`'s `SetBytes` after checking the length and the `02`/`03` tag yourself; `ParseRawPrivateKey` (Go 1.25) refuses zero and values at or above `n` | `signing_v1.json` P-256 rows |
| BLS12-381 (PoP ciphersuite) | `github.com/consensys/gnark-crypto/ecc/bls12-381`, for verification and aggregation only | `G1Affine.SetBytes` accepts both the compressed and the uncompressed form; it checks the curve and the subgroup. The identity decodes. There is no BLS signature scheme. `PairingCheck` does not check subgroups. gnark-crypto is not constant time, so it must never touch a secret key (`signing.md` §1.6 requires constant-time secret operations). | check the length (48 for keys, 96 for signatures) before `SetBytes`, and confirm that its flag bits (compression, infinity, sign) are the ZCash layout the eth2 suite uses; refuse an identity key (KeyValidate); let an identity signature decode and never verify; use `HashToG2` (RFC 9380 random oracle), not encode-to-curve, with the spec's DSTs; build Verify, Aggregate, FastAggregateVerify, AggregateVerify and PopVerify on it. Sign, PopProve, a secret key's public key and EIP-2333 need a constant-time backend, and no vetted pure-Go one is known: `supranational/blst` needs cgo, which is forbidden; `cloudflare/circl`'s `sign/bls` implements only the basic scheme, not this PoP ciphersuite, and its documentation claims no constant time; `kilic/bls12-381` is not maintained. Until one is vetted, the Go port verifies BLS but does not sign. That is a decision for the user. | `signing_v1.json` BLS rows and `blsPop`; the eth2 corpus, including its deserialization cases (`rs/signing/tests/reference_vectors.rs:551`) |
| BN254: EIP-196/197, Groth16, SP1 | `github.com/consensys/gnark-crypto/ecc/bn254` | `SetBytes` reads the top two bits of the first byte as flags (compressed, infinity), so a coordinate at or above `p` with those bits set is read as another encoding, not refused as malformed. EIP-197 puts G2's imaginary part first, while gnark's `E2` is `A0 + A1·u`. `PairingCheck` does not check subgroups. | parse each 32-byte coordinate yourself, refusing values at or above `p` (`zk.md` §1); build the point, with EIP-197's imaginary half as `A1`; check the curve, and G2's subgroup with `IsInSubGroup`; treat `(0, 0)` and all zeros as infinity; reduce `mul`'s scalar mod `r`; drop identity pairs before pairing, as Rust does. Verify Groth16 and SP1 directly on the curve (`rs/proofs/src/zk/groth16.rs:101`); gnark's own Groth16 types use another serialization. | `zk_v1.json` `add`, `mul`, `pairing`, `sp1Groth16`; `zk_jlvm_v1.json`'s proof |
| Poseidon | none fits our API: generate the tables from the reference's `PoseidonConstantsGen` | `github.com/iden3/go-iden3-crypto/poseidon` is circomlib-compatible, but it works on `*big.Int` and its maintained line is a later major | use go-iden3-crypto as a cross-check only | `poseidon_v1.json` |
| Sigma verification over secp256k1 | decred secp256k1 (`JacobianPoint`, `ModNScalar`), on public data only; `x/crypto/blake2b` | `ParsePubKey` refuses the 33-zero identity and accepts uncompressed forms | decode canonically yourself (`sigma.md` §1): 33 bytes, `02`/`03` with `x < p`, or 33 zeros; port GF(2^192) and its tests from `rs/signing/src/sigma/gf2_192.rs` | the three sigma files |
| BIP-39 normalisation | `golang.org/x/text/unicode/norm` (NFKD) | `strings.Fields` splits on every Unicode space (`U+0085`, `U+2028` and others). The three SDKs split, after NFKD, on exactly `[ \t\n\v\f\r\u3000]`. | split on that set: `crypto/m/keys/Bip39.scala:118`, `rs/signing/src/keys/bip39.rs:216`, `ts/signing/src/bip39.ts:21`. The SDKs already disagree at the ends: Scala calls `String.trim` before splitting, which drops every code point up to U+0020 (controls included), and Rust and TS do not, so a phrase led by U+0000 parses in Scala alone. The spec says only "whitespace (including U+3000)", and no vector pins either edge (`parity.md` GAP-16). Follow the spec once it decides. | none yet |

### Java (a layer over the Scala library)

- **Types.** Java 21 records and sealed interfaces. Results are a sealed `Result<T, E>` (`Ok` and `Err` records),
  matched exhaustively with `switch`; absence is `Optional`. Collections are `List.copyOf` and `Map.copyOf`.
- **Bytes.** `byte[]` is copied on the way in and on the way out. A record holding a `byte[]` compares arrays by
  reference, and its contents stay mutable, so wrap bytes in an immutable `Bytes` type with value equality.
- **Scala types.** `scala.math.BigInt` maps to `java.math.BigInteger` (its `.bigInteger`); scodec `ByteVector` to a
  copied `byte[]`; `Either` to `Result`; `Option` to `Optional`.
- **Scala 3 interop to plan for:**
  - union-typed lefts erase to `Object` (for example `Either[CanonicalError | Cbor.TooLarge, Digest[H]]`,
    `core/m/hash/TaggedHash.scala:52`), so the facade maps them onto a sealed Java type;
  - default arguments (`Config()`) have no Java overloads, so write them;
  - implicit and given instances (`Hasher[H]`, `MerkleHashing[H]`, `TrieHashing[H]`, `VersionKey[V]`) are passed
    explicitly;
  - Scala 3 `enum`s are not `java.lang.Enum`;
  - cats-mtl `Raise` parameters appear on a few constructors (`crypto/m/signing/Capabilities.scala:61`).
- **Purity.** The claim that "Scala's core, crypto and JLVM APIs are pure (`Either`), so there's no effect runtime,
  and `unsafeRunSync` is never needed" is not true as it stands. `parity.md` section 7 lists the exceptions:
  - the native loaders: `Jlvm.load`, `Secp256k1.load`, `Bls12381.load` and `Bn254Engine.gnark`, without which there
    is no JLVM, secp256k1 or BLS;
  - every `SecureRandom[F]` API, `SigmaProver.prove` among them, whose fixed-seed form is test-only (`sigma.md` §7);
  - the stores.

  Everything else (codecs, hashing, verifiers, evaluation once loaded) is pure. Three ways to bridge the edge:
  1. The facade's one `load()` runs the Scala loaders in `SyncIO` with `.attempt`, then `unsafeRunSync()`. It returns
     a `Result<Dilf4s, LoadError>`, and everything after it is pure. That is one `unsafeRunSync` at the program edge,
     as `IOApp` is in Scala, but the rule says never in production code.
  2. Hand Java callers a cats-effect `IO` to run themselves. That avoids the call, but makes every Java user adopt
     cats-effect.
  3. Expose only what is pure. That drops the JLVM, secp256k1, BLS and proving from Java.

  This is a decision for the user (`parity.md` GAP-2). Option 1 is the smallest. Whichever is chosen, signing in Java
  takes its randomness as explicit bytes, through the existing pure forms: `bip340SignWithAux`,
  `taggedSchnorrSignWithAux` and `signWithRandomness`. Proving needs a public, seeded `prove` in Scala, and the spec
  currently calls that form test-only.
- **Tests.** Write them in Java, through the Java API, over every vector file. Mapping bugs live in the facade, and
  Scala's own gates cannot see them. The facade is an sbt module, so its tests run in the `scala` job.

### Python (PyO3 bindings over the Rust SDK)

- **Types.** Frozen types: `#[pyclass(frozen)]`, and frozen dataclasses for the generated families, with a `.pyi`.
- **Results.** `Ok` and `Err` classes with `__match_args__`, so callers use `match`. No exception escapes the API.
  Take arguments as `Bound<PyAny>`, and turn a wrong type into `Err(TypeMismatch)`. Never let PyO3's extraction raise
  `TypeError`.
- **Text.** A `str` can hold a lone surrogate, which has no UTF-8 form, so it cannot cross into Rust's `&str`. Refuse
  it as a value, as the canonical JSON parser does for an unpaired surrogate, never as `UnicodeEncodeError`.
- **Typing and tests.** `mypy --strict`, and Hypothesis with a fixed seed, for reproducible CI.
- **The GIL.** Release it around heavy calls, such as pairings, BLS, sigma, a JLVM evaluation or a long hash, with
  PyO3's GIL release (`allow_threads`; check the name in the PyO3 version you pin). Copy every input before releasing
  it: a `bytearray` or `memoryview` can change under another thread while the GIL is released. That is the
  shared-memory bug of section 3 again.
- **Wheels.** abi3 wheels, built with maturin, for one minimum CPython version. abi3 does not cover free-threaded
  builds; decide separately whether to support those.
- **Vectors.** Every vector file runs through the **Python** API, loaded with the binding's own canonical JSON parser
  and the generated Python vector types. That tests the binding layer itself, not only Rust.
- **Inheritance.** Python inherits Rust's gaps (`parity.md`): no sigma prover, no Merkle or trie proof producers, no
  Poseidon builder, no JLVM render, no encoding ceiling. Close them in Rust, not in Python.
- **CI.** A `python` job filtered on `sdk/python/**`, `sdk/rust/**`, `shared/**` and `ci.yml`. It runs:
  - the pinned Rust toolchain;
  - `cargo fmt` and `clippy` on the binding crate;
  - `maturin build --release`, then installing the wheel;
  - `mypy --strict`;
  - `pytest`.

## 5. Pitfalls you can foresee

### Orders: there are three, and they differ

- **CBOR map keys** sort by their encoded bytes (`canonical-encoding.md` §2), so a shorter key sorts before a longer
  one before any byte is compared.
- **JSON rendering and the JLVM's `keys`, `values` and `entries`** use Unicode code-point order.
- **Trie keys** use bitwise order, a prefix before its extensions (`trie.md` §4).

Go's `sort.Strings` and Python's `sorted` give code-point order on well-formed text. Java's `String.compareTo` gives
UTF-16 order, which differs for supplementary characters. None of them gives CBOR order.

### Go

- `map` iteration order is random by design. Never range over a map to produce output, a hash or an error.
- `encoding/json` decodes numbers into `float64`, lets the last of two duplicate keys win, replaces invalid UTF-8
  with U+FFFD silently, and matches field names case-insensitively. Use your canonical parser.
- Strings are bytes and may hold invalid UTF-8. `len` counts bytes, and `[]rune(s)` turns bad bytes into U+FFFD
  silently. Validate with `utf8.ValidString` where text enters; count code points with
  `utf8.RuneCountInString`.
- `strings.ToLower` and `ToUpper` are Unicode. The JLVM's `lower` and `upper` map ASCII only (`jlvm.md` §2).
- `big.Int.SetString` accepts a leading `+`, and with base 0 it accepts prefixes and underscores. Parse canonical
  JSON numbers with your own grammar, then convert.
- `int` is 32 bits on `386` and `arm`. Keep weights, gas and sizes in `uint64` or `int64`.
- `golang.org/x/text` implements Unicode 17.0, Java 21 15.0, Rust's `unicode-normalization` 18.0, and TS whatever
  its engine ships. NFKD agrees on characters every version knows, but a passphrase holding a compatibility
  character assigned later gives a different seed in each (`parity.md` GAP-19). Follow the version the spec pins
  once it does; until then, match the reference.

### Java

- `byte` is signed: use `b & 0xff`. Use `new BigInteger(1, bytes)` for unsigned magnitudes. `toByteArray()` adds a
  sign byte and varies in length.
- `String.hashCode` is trivially collidable. Never key a `HashMap` by caller text; use a `TreeMap` with a code-point
  comparator.
- `String.length()` counts UTF-16 units. `toLowerCase()` uses the default locale, which breaks in Turkish.
  `Character.isWhitespace` is not Unicode `White_Space`. `new String(bytes, UTF_8)` replaces malformed input
  silently; use a `CharsetDecoder` set to `REPORT`.
- Native backends are JNI, and their loaders are effects (above). Never load one in a static initialiser.

### Python

- A `dict` keeps insertion order, not canonical order. Never build canonical output from `dict` order.
- `json.loads` reads numbers as `float`, accepts `NaN` and `Infinity`, and lets the last duplicate key win.
- CPython refuses `int` and `str` conversions past 4,300 digits by default (`sys.set_int_max_str_digits`). That is
  below the canonical 10,000. Move integers across the binding as bytes (`int.from_bytes`, or PyO3's big-integer
  conversion), never as decimal strings.
- `hash()` of an `int` is its value modulo 2^61 − 1, and is not randomised, so a caller can choose positions or keys
  that collide (section 3). `str` hashing is randomised per process, so set order differs between runs. Never let
  either reach an output.
- A `float` is an IEEE double: `float("9007199254740993")` is `9007199254740992.0`. No number of the data model may
  pass through one (`canonical-encoding.md` §3).

## 6. Before the first PR of a port

- [ ] The new column of `parity.md` is filled in for the phase, with `file:line` for each entry point and gate.
- [ ] Every vector file and section of the phase is loaded through the SDK's own parser and generated types, and
      checked row by row.
- [ ] Kind coverage runs over the generated kind lists, and a planted missing row fails it.
- [ ] DoS tests count work, and each fails on a planted regression.
- [ ] No hash map is keyed by caller or peer data; no recursion on caller data; no panic, throw or `unsafe` path in
      the library.
- [ ] CI runs every gate above, behind its path filter, and the `scala` job's staleness check covers the generated
      sources.
- [ ] Any gap found is added to `parity.md` section 8 with its rank, and becomes a lane rather than a silent
      divergence.
