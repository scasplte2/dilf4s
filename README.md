# DiLF4S
**Di**stributed **L**edger **F**unctionalities for **S**cala

## Description
Canonical data, cryptography, authenticated storage and a deterministic, gas-metered JSON expression language (JLVM) for distributed ledger protocols. Scala is the reference implementation; the other languages implement the same specified behavior or expose it through bindings.

## Repository layout

| Directory | Responsibility |
|---|---|
| `lang/scala/` | Reference implementation, sbt build and plugins, Scala modules, code generators and JVM benchmarks. |
| `lang/rust/` | Independent protocol implementation and SP1 integration. `native/` owns the native Cargo workspace; the Cargo workspace at the language root owns the patched SP1 guest. |
| `lang/ts/` | Independent TypeScript protocol implementation and its compiler, lint and test configuration. |
| `lang/go/` | Independent Go implementation. Core encoding and hashing are implemented; remaining protocol coverage is tracked in [parity](docs/guides/parity.md). |
| `lang/java/` | Java bindings to the Scala implementation, built by the Scala sbt build. |
| `lang/python/` | Python bindings to Rust, with its own Python and extension build configuration. |
| `formal/` | Lean specifications, proofs and Lake build. |
| `shared/` | Language-neutral schemas, conformance vectors and Unicode data. |
| `tools/` | Cross-language tooling: MCP server, differential fuzzing and gas calibration. |
| `docs/`, `vendor/` | Specifications and guides; pinned third-party dependencies. |

Language-specific source, tests and build configuration stay with their language. Repository-wide CI, licences and
shared assets stay at the root. The root npm workspace and lockfile connect the TypeScript libraries to `tools/mcp`;
they do not own the TypeScript compiler configuration. Java's sbt project delegates its source and output paths to
`lang/java`; it does not duplicate the Scala dependency graph.

Independent implementations must match the protocol, including refusals, gas and serialization. Bindings inherit
behavior from their implementation. The directory name does not imply a reduced protocol contract. See the
[porting guide](docs/guides/porting.md) and [coverage ledger](docs/guides/parity.md) for current gaps.

## Development

Run from the repository root unless a command changes directory:

```sh
tools/sbt checkPR test
npm ci
npm run check:ts
npm run build:ts
npm run test:ts
(cd lang/rust/native && cargo test --locked --workspace)
(cd lang/go && go test ./...)
```

Python build/test instructions are in [lang/python](lang/python/README.md); the separate SP1 build is documented in
[lang/rust/zk](lang/rust/zk/README.md). Open `lang/scala` as the sbt build in an IDE. `tools/sbt` is a thin wrapper for
that build; ordinary `sbt` commands also work from `lang/scala`. The `checkPR` and `preparePR` aliases pass the
repository source root to Scalafix; direct Scalafix invocations need `--sourceroot=../..` from that directory.

Conformance tests pin exact coverage and check every row's expected result or refusal. Adding vectors requires
updating those expectations after rebasing; a merge conflict is not a reason to replace them with lower bounds.
The canonical round-trip suites also check `shared/vectors/jlvm_parse_refusals_v1.tsv`: one case name and error kind
per tab-separated row, naming every program that cannot be parsed. `JlvmVectorGen` regenerates it with the main
JLVM corpus, and the Scala staleness suite checks it against the reference parser.

## Scala modules

| sbt project | What it holds |
|---|---|
| `numerics` | Exact rational arithmetic. |
| `core` | The canonical data model, its CBOR and JSON mappings (`docs/specs/canonical-encoding.md`), tagged hashing and the generated wire models. |
| `circeInterop` (`lang/scala/circe-interop/`) | circe `Encoder`/`Decoder` interop with the canonical model: `CanonicalCirce.fromEncoder`, `decode`, `codec`, `fromCirce`, `toCirce`. |
| `crypto` | Signature schemes, VRF, KES, BLS, sigma protocols and zk primitives. |
| `storage`, `storageRocksDb` | Stores and versioned stores, in memory and on RocksDB. |
| `accumulators` | Merkle trees, the binary trie and the versioned trie. |
| `jlvm` | The JLVM (`docs/specs/jlvm.md`). |
| `javaApi` (`lang/java/`) | `dilf4s-java`: a Java API over `core`, `crypto`, `accumulators` and `jlvm`, with no Scala type in its signatures (`lang/java/README.md`). |
| `modelsGen`, `benchmarks`, `sharedTest` | The CDDL model generator, JMH benchmarks and test helpers. |

**JSON and circe.** Only `circeInterop` depends on circe in its main sources. `core` has no circe dependency, main or
test, so nothing in it can reach circe's parser or its hashed `JsonObject`, and `storage`, `accumulators` and `jlvm`
compile without circe. JSON text, a program's above all, is read by `CanonicalJson.parse` alone: one parser, with the
same grammar, number bounds and refusal of repeated member names as the Rust and TypeScript SDKs (pinned in
`shared/vectors`). `circeInterop` is for typed data a program built (`a.asJson`). It is not a way in for untrusted
text: by the time circe has parsed JSON, its `JsonObject` has merged repeated member names and hashed the keys. (It
still reads a circe-parsed number from its source text, so that the number reads as the parser reads it.) Some test
suites outside `core` still use circe to write and read the vector files.

## Installation
*Work In Progress*

## Usage
*Work In Progress*

## Contributing
Contributions are welcome! If you would like to contribute to this project, please follow these steps:
1. Fork the repository.
2. Create a new branch (`git checkout -b feature-branch`).
3. Make your changes.
4. Commit your changes (`git commit -am 'Add some feature'`).
5. Push to the branch (`git push origin feature-branch`).
6. Create a new Pull Request.

## License
dilf4s is licensed under the Apache License, Version 2.0 (see LICENSE). Two source files and five test-vector files adapted from MPL-2.0 projects stay under the Mozilla Public License 2.0, and third-party word lists and test data keep their own licences; NOTICE lists them, and LICENSES holds each licence's text.
