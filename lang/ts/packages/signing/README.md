# @kd5ujc/dilf4s-signing

dilf4s signatures, key derivation, VRFs, key-evolving signatures and sigma protocols. The normative specifications
are [`signing.md`](../../../../docs/specs/signing.md), [`vrf.md`](../../../../docs/specs/vrf.md),
[`kes.md`](../../../../docs/specs/kes.md) and [`sigma.md`](../../../../docs/specs/sigma.md); the conformance vectors in
`shared/vectors` are part of them. `ecrecover` is Ethereum's signer recovery as the JLVM's `ecrecover` defines it
([`jlvm.md`](../../../../docs/specs/jlvm.md) section 6): a high-`s` signature recovers nothing.

## Sigma protocols

`sigma` verifies Ergo-compatible sigma proofs over secp256k1 (`sigma.md`): a tree of `and`, `or` and `threshold` over
`dlog` and `dhtuple` statements, proven without revealing which branches the prover knew. It is the verifier only, as
in Rust; proofs are made by the Scala prover (or Ergo's).

| Export | What it does |
|---|---|
| `sigma.Proposition.fromValue(value)` | Decodes a proposition from its canonical form, a core `Value` (§3), in the reference's order; the JLVM's `sigma_verify` does this before it checks the proof's and message's kinds. |
| `proposition.depth`, `.leaves`, `.proofLength` | The nesting depth, the leaf count and the exact length of every proof (§5). |
| `sigma.verify(proposition, proof, message)` | Checks a proof of a decoded proposition (§6): the JLVM's `sigma_verify`. |
| `sigma.verifyValue(value, proof, message)` | `fromValue`, then `verify`. |
| `sigma.verifyDlogBytes(key, message, proof)` | A one-leaf `dlog` proof over a 33-byte key (Ergo's P2PK signature): the JLVM's `schnorr_verify`. |
| `sigma.CHALLENGE_LENGTH`, `ELEMENT_LENGTH`, `SCALAR_LENGTH`, `MAX_DEPTH`, `MAX_LEAVES`, `MAX_CHILDREN`, `MAX_WEIGHT` | The protocol's sizes and the bounds of `limits.cddl`. |

Each returns a `Result`: `true` or `false` for a well-formed proof; a `SigmaError` for malformed input, checked in
the reference's order (the proposition, whose `InvalidProposition` and `InvalidPoint` follow §3's decoding order, then
the proof's length, `ProofLength`, then each response, `ResponseOutOfRange`); or core's `TypeMismatch` for an argument
of the wrong type, checked first. A proof is measured before it is read, and a
proposition's weight before it is decoded, so the work is bounded by the proof and the proposition's limits; nothing
recurses on the input's shape.

## Side channels

This package is **not side-channel hardened**. JavaScript gives no constant-time guarantee, and part of the
secret-key arithmetic (BIP32 and Icarus child keys, VRF and ristretto255 responses) is plain `bigint`. Verification
handles only public data and is unaffected.

Where an attacker can time your signing (a server, a shared host), sign with the Rust SDK (`dilf4s-signing`), whose
secret scalars are constant time.
