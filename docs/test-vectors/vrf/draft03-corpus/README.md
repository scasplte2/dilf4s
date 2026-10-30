# Cardano draft-03 VRF oracle corpus

These are verdicts from the Cardano fork of libsodium (`IntersectMBO/libsodium` at
`dbb48cce5429cb6585c9034f002568964f1ce567`, function `crypto_vrf_ietfdraft03_verify`). They were copied unchanged from
`scarab/fixtures/vrf` at commit `58172e52dc4054e627a18647477c6bd6953361d7`, which documents the pinned native build and
how the rows were produced. `SHA256SUMS` matches scarab's manifest.

## Files

| File | Rows | Contents |
|---|---|---|
| `vectors.tsv` / `expected.tsv` | 2,048 | `id  pk  proof  alpha` → `VALID:<beta>`, `INVALID` or `MALFORMED` |
| `additional.tsv` / `additional-results.tsv` | 2,338 | re-runs, mixed-order/torsion/negative-zero mutations, boundaries |
| `helpers.tsv` / `helper-results.tsv` | 3,426 | intermediate steps; see below |

The rows in `helpers.tsv` are of four kinds:

- `d`: point decoding and the small-order test.
- `u`: the Elligator map `ge25519_from_uniform`.
- `e`: the equation `(-c mod L) P + s Q`.
- `t`: the full pipeline `H‖U‖V‖c‖beta`.

## Use and licence

Every key, proof and message is public. No secret was used to produce the rows. The data is derived data, licensed
Apache-2.0.

`xyz.kd5ujc.crypto.vrf.Draft03CorpusSuite` replays every row against `Vrf.draft03`.
