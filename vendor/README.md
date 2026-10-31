# Vendored artifacts

Libraries that are not on Maven Central and sit on a consensus path. A copy is kept here, in Maven layout, so the
build never depends on the publisher's server. `project/Pinned.scala` pins the SHA-256 of each one, so `update` fails
if the bytes or the version ever change.

| Artifact | Why | Source | SHA-256 |
|---|---|---|---|
| `org.hyperledger.besu:gnark:2.0.0` | BN254 (EIP-196/197) for `bn254_*` and `groth16_verify`: gnark-crypto, as Besu's precompiles use it | `https://hyperledger.jfrog.io/artifactory/besu-maven/` | `9632cf12…c5095e` |
| `org.hyperledger.besu:besu-native-common:2.0.0` | its native library loader | same | `8c29eece…d17829` |

These jars bundle prebuilt native libraries for Linux and macOS, on x86-64 and aarch64. Other platforms, Windows
included, fail at `CryptoOps.load`. The libraries are built by Besu's release process from
`github.com/hyperledger/besu-native`; they are not rebuilt here.

To update:
1. Download the new jar and pom.
2. Check them against the publisher's `.sha1` files.
3. Replace the directory, and update the version in `project/Dependencies.scala` and the hashes in
   `project/Pinned.scala`.
4. Run the gate. The pure-Scala engine cross-checks gnark in `Bn254Suite`, `Sp1Groth16GnarkSuite` and
   `ZkOpsGnarkSuite`.
