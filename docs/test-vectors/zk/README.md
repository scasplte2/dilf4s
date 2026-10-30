# zk reference vectors

| File | Source |
|---|---|
| `bn256Add.json`, `bn256ScalarMul.json`, `bn256Pairing.json` | go-ethereum `core/vm/testdata/precompiles`, fetched from `master` at `a0cba163b1f17f6044df84502ff13036dffb5de1` (LGPL-3.0 test data). These are the EIP-196/197 precompile vectors. |
| `sp1-groth16-premium.json`, `sp1-groth16-shielded.json` | metakit `src/test/resources/zk`: real SP1 v6.1.0 Groth16-BN254 proofs (a JsonLogic program and a shielded transfer), described inside each file. |

All data is public: proofs, keys and inputs only.

- `xyz.kd5ujc.crypto.zk.Bn254Suite` replays the go-ethereum vectors.
- `Sp1Groth16Suite` replays the SP1 proofs.
