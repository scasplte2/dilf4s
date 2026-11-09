# DiLF4S
**Di**stributed **L**edger **F**unctionalities for **S**cala

## Description
This repository contains cryptographic functionalities designed to support the development of distributed ledger protocols using Scala.

## Modules
| sbt project | What it holds |
|---|---|
| `numerics` | Exact rational arithmetic. |
| `core` | The canonical data model, its CBOR and JSON mappings (`docs/specs/canonical-encoding.md`), tagged hashing and the generated wire models. |
| `circeInterop` (`circe-interop/`) | circe `Encoder`/`Decoder` interop with the canonical model: `CanonicalCirce.fromEncoder`, `decode`, `codec`, `fromCirce`, `toCirce`. |
| `crypto` | Signature schemes, VRF, KES, BLS, sigma protocols and zk primitives. |
| `storage`, `storageRocksDb` | Stores and versioned stores, in memory and on RocksDB. |
| `accumulators` | Merkle trees, the binary trie and the versioned trie. |
| `jlvm` | The JLVM (`docs/specs/jlvm.md`). |
| `javaApi` (`java/`) | `dilf4s-java`: a Java API over `core`, `crypto`, `accumulators` and `jlvm`, with no Scala type in its signatures (`java/README.md`). |
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
This project is licensed under the Mozilla Public License 2.0 (MPL 2.0). For more details, see the LICENSE file in the repository.
