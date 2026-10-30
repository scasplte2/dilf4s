# Cardano Sum6 KES fixtures

These files were copied unchanged from `scarab/fixtures/sum6` and `scarab/fixtures/kes-gen` at commit
`714511e8ef8c534f290ca9cc5bff6d2c9150e3f4`; scarab documents how they were produced. `SHA256SUMS` matches scarab's pins.

| File | Rows | Contents |
|---|---|---|
| `originals.tsv` | 4 | `slot  root vk  relative period  message  signature`: the Sum6 KES signatures of four public preprod block headers (Amaru test data) |
| `observations.tsv` | 17,336 | `slot  case  verdict  hashes  leaves`: verdicts for each original and its mutations |
| `keygen.tsv` | 3 | `name  r1  vk1  vk0  vk` from `cardano-cli node key-gen-KES` (cardano-node 11.1.3): regenerating the depth-5 tree from `r1` must give `vk1`, and `vk = Blake2b-256(vk0 ‖ vk1)` |

Each mutation case in `observations.tsv` is one of:

- `period/<p>`;
- `<root|signature|message>/xor/<offset>/<bit>`;
- `signature/swap-pair/<depth>`;
- `message/empty`, `message/truncate-last` or `message/append-zero`.

The verdicts come from a source-derived model of cardano-base `KES/Sum.hs` with a native libsodium Ed25519 leaf. That
makes them differential evidence, not node executions.

All data is public: no signing keys or private material. The data is derived data, licensed Apache-2.0.

`xyz.kd5ujc.crypto.kes.CardanoSumKesSuite` replays every row.
