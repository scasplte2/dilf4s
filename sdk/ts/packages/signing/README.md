# @kd5ujc/dilf4s-signing

dilf4s signatures, key derivation, VRFs and key-evolving signatures. The normative specifications are
[`signing.md`](../../../../docs/specs/signing.md), [`vrf.md`](../../../../docs/specs/vrf.md) and
[`kes.md`](../../../../docs/specs/kes.md); the conformance vectors in `shared/vectors` are part of them.

## Side channels

This package is **not side-channel hardened**. JavaScript gives no constant-time guarantee, and part of the
secret-key arithmetic (BIP32 and Icarus child keys, VRF and ristretto255 responses) is plain `bigint`. Verification
handles only public data and is unaffected.

Where an attacker can time your signing (a server, a shared host), sign with the Rust SDK (`dilf4s-signing`), whose
secret scalars are constant time.
