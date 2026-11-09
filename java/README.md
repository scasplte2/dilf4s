# dilf4s-java

A Java API over the dilf4s Scala library: one dependency, `xyz.kd5ujc:dilf4s-java`, for Java 21 and later. Every call
delegates to the Scala reference (`core`, `crypto`, `accumulators`, `jlvm`), so its bytes and its refusals are the
reference's, and the shared vectors in `shared/vectors` hold for it as they do for the Scala, Rust and TypeScript SDKs.

## Shapes

- **No exceptions.** Every fallible call returns a sealed `Result<T, E>`, `Ok` or `Err`, so a `switch` over one is
  exhaustive. The refusals are the reference's error families (`xyz.kd5ujc.dilf4s.error`), one sealed interface per
  family and one record per kind, with the kinds and fields of `shared/schemas/*-errors.cddl`. A null argument is the
  shared `TypeMismatch`, which is a member of every family, as in the TypeScript SDK.
- **Records** for data, **sealed interfaces** for choices, `Optional` for absence, unmodifiable `List`s and `Map`s.
- **Arrays are copied** at the boundary in both directions: changing an array you passed or got changes nothing.
- **No Scala type** in any public signature (a test scans the API by reflection). The conversions from the reference's
  types live in `xyz.kd5ujc.dilf4s.internal`, which is not API.

`Value` (the canonical data model) is a sealed family of final classes, not records: each holds the reference's value,
so building a value, reading a part or encoding it never copies or walks it, and a value with shared parts costs what
it occupies.

## What is here

| Area | Classes |
|---|---|
| Values | `Value` (builders and accessors), `Cbor` (canonical CBOR), `Json` (the JSON mapping), `Hashing` (the seven hashes, digests, domain tags, tagged and content hashes, `expand_message_xmd`), `ValueCommitment` |
| Signing | `Ed25519` (ZIP-215, RFC 8032 strict, libsodium), `P256`, `Ristretto255`, `Icarus` (Ed25519-BIP32), `Ecrecover`, `Signing` (key pairs, SLIP-10, derivation paths), `Bip39`, `Vrf` (TAI, ELL2, draft-03), `Kes` (dilf4s sum and product, Cardano sum), `Sigma`; on `Backends`: `Secp256k1` (ECDSA, BIP-340, tagged Schnorr, BIP-32) and `Bls` (BLS12-381, aggregation, proofs of possession, EIP-2333) |
| Proofs | `Merkle` (dilf4s and RFC 6962), `Trie`, `VersionedTrie`, `Poseidon`, `Zk` (BN254 and SP1's Groth16) |
| JLVM | `Jlvm`: parse and render the surface, the canonical form and its CBOR, the program id, compile, the JSON surface of values; on `Backends` (`Backends.load()`): `evaluate`, with gas and bindings, to an `Outcome` (a value or a refusal with its kind, op and cause, the gas used and the warnings) |

## The native backends

JLVM evaluation, secp256k1 and BLS12-381 run on native libraries (libsecp256k1, blst and gnark, through JNI). Those
calls take a loaded `Backends`, and are pure given it. One call loads it:

```java
Result<Backends, LoadError> backends = Backends.load();
switch (backends) {
  case Result.Ok<Backends, LoadError> ok -> Jlvm.evaluate(ok.value(), program, data, Map.of(), Jlvm.DEFAULT_GAS_LIMIT);
  case Result.Err<Backends, LoadError> err -> System.err.println("no native backends here: " + err.error());
}
```

`Backends.load()` is the one effect this API runs, and Java source is where it runs: the Scala sources hold no
`unsafe*` call. It is safe to call from anywhere, any number of times:

- **The reference's own loads.** The Scala bridge (`internal/Natives.scala`) describes `Secp256k1.load[SyncIO]`,
  `Bls12381.load[SyncIO]` (with its self-check) and `Jlvm.load[SyncIO]`, each `attempt`ed. `Backends.load` runs them,
  in that order, with `unsafeRunSync()`. `SyncIO` needs no runtime and no threads.
- **Nothing crashes the caller.** `attempt` holds a non-fatal failure (an unsupported platform, the self-check) as a
  value: `LoadError.Failed`. A native library that does not link raises a `LinkageError` (`UnsatisfiedLinkError`,
  `ExceptionInInitializerError`), which the effect runtime treats as fatal and rethrows. The one `catch` in the library,
  around that one call, turns it into `LoadError.Unlinked`. Nothing broader is caught: a `VirtualMachineError` is the
  JVM's own. Either refusal names the backend, and the throwable's class and message, bounded to 256 code points.
- **Once per process.** The outcome, success or refusal, is kept in a lazy holder (the JVM initialises it once,
  thread-safely, on the first call), and every call returns it. A failed native class initialiser cannot be run
  again: a second attempt would throw `NoClassDefFoundError`, so a refusal is never retried.

`EffectScanTest` pins it: `unsafeRunSync` appears once in the Java sources, in `Backends.run`, and never in the Scala
ones, and the only `try`/`catch` is that one, catching `LinkageError`. Every suite that needs the backends runs on
the ones `Backends.load()` returns.

## What is not here, and why

- **Signing with fresh randomness, key generation and the sigma prover**: they draw from `SecureRandom[F]`, and the
  sigma prover has no public pure form. Sign with your own randomness where a scheme takes it (`Ristretto255.sign`,
  `Secp256k1.bip340Sign`, `Secp256k1.taggedSign`).
- **BN254 on gnark**: `Zk` runs on the reference's pure engine (`Bn254Engine.reference`); the native one also loads
  only in an effect (`Bn254Engine.gnark[F]`). The JLVM's BN254 operations run on gnark, inside the loaded evaluator.
- **BIP-39 word lists**: the reference reads its bundled lists in `WordList.load[F]`. Pass the 2048 words to
  `Bip39.wordList`.
- **PBKDF2-based derivations** (BIP-39's seed, Icarus's root from entropy): the reference's PBKDF2 recurses as deep as
  its rounds ("Thread stacks" below). Icarus's 4096 overflow even a 1 MB stack, and BIP-39's 2048 overflow 448 KB.
  `Icarus.root` takes the 96-byte PBKDF2 output instead, and `Bip39`'s documentation gives the seed's recipe.

## Thread stacks

The no-exception guarantee holds on a thread with at least the JVM's default stack, 1 MB on 64-bit platforms, with one
exception below (domain tags). Parts of the reference recurse once per nesting level or segment of their input, so a
smaller stack, or a longer tag, can make them throw `StackOverflowError` (or, the first time a class is initialised,
`ExceptionInInitializerError`):

- **The JLVM's program reader.** `Jlvm.parse` walks program text nested up to the JSON bound (256 levels) before the
  program's own depth bound refuses it (`jlvm/src/main/scala/xyz/kd5ujc/jlvm/Parser.scala:87-113`). Cold, that needs
  between 768 KB and 1 MB of stack. `Jlvm.decode` and `Jlvm.fromCanonical` recurse over the canonical form of the
  deepest program, 126 levels (`Parser.fromWire`); that needs between 512 and 768 KB.
- **The domain-tag grammar** (`core/src/main/scala/xyz/kd5ujc/core/hash/TaggedHash.scala:14-17`). It is a regex whose
  repeated group `(/[a-z0-9][a-z0-9._-]*)*` recurses once per segment. Measured cold at 1 MB, a valid tag overflows at
  about 1,245 segments (about 2,500 characters) and an invalid one at about 1,350. No stack is enough for every
  length: the 1 MB guarantee holds for `Hashing.domainTag` only on tags shorter than that, so a caller passing
  untrusted tags should bound their length first. The library's own tags have a few segments.
- **PBKDF2** (`crypto/src/main/scala/xyz/kd5ujc/crypto/keys/Hmac.scala:21-44`). Each round's `xor` is lazy, so the
  output is a chain as deep as the rounds, and reading it recurses that deep. That is 2048 rounds for BIP-39's seed and
  4096 for Icarus. This layer offers neither derivation; see above.

`DeepInputTest` pins the 1 MB claim: it runs the deepest inputs first, in a JVM of their own at `-Xss1m`. Making those
readers stack-safe in the reference would lift the requirement.

## Building and installing locally

Nothing is published to a remote repository. To use the jar from a Maven or Gradle project on this machine, publish
the Scala modules it depends on and the Java layer to a local Maven repository:

```
sbt "numerics/publishM2; core/publishM2; storage/publishM2; crypto/publishM2; accumulators/publishM2; jlvm/publishM2; javaApi/publishM2"
```

(`publishLocal` writes the Ivy layout to `~/.ivy2/local` instead, for sbt consumers.) The coordinates are
`xyz.kd5ujc:dilf4s-java:0.1.0-SNAPSHOT` (no Scala suffix); its dependencies carry `_3`.

Two of its transitive dependencies, Hyperledger Besu's `gnark` and `besu-native-common`, are not on Maven Central: the
pinned copies are in this repository's `vendor/maven`. A consumer lists that directory as a repository:

```xml
<repositories>
  <repository>
    <id>dilf4s-vendor</id>
    <url>file:///path/to/dilf4s/vendor/maven</url>
  </repository>
</repositories>
<dependencies>
  <dependency>
    <groupId>xyz.kd5ujc</groupId>
    <artifactId>dilf4s-java</artifactId>
    <version>0.1.0-SNAPSHOT</version>
  </dependency>
</dependencies>
```

## Example

```java
import xyz.kd5ujc.dilf4s.*;

Result<Value, ?> parsed = Json.parse("{\"a\": [1, 2.5, \"x\"]}");
switch (parsed) {
  case Result.Ok<Value, ?> ok -> System.out.println(Cbor.encode(ok.value()));
  case Result.Err<Value, ?> err -> System.out.println("refused: " + err.error());
}
```

## Tests

`sbt javaApi/test` runs the JUnit Jupiter suites (`sbt-jupiter-interface`), each in a fresh JVM of its own at a 1 MB
thread stack, so no suite is warmed by another; `DeepInputTest` runs the deepest inputs first, cold. They read the shared vectors through this API: all of `canonical_encoding_v1.json`; all of `jlvm_v1.json`
(every case evaluated, with its exact outcome, gas and warnings; the aliases; the decodings); and the signing, VRF,
KES, sigma, Merkle, trie, versioned trie, Poseidon and BN254/SP1 files. Three more suites scan the public API by
reflection (no foreign type, every family sealed), call every method and record constructor with nulls (no throw),
and check every returned array is a fresh copy.
