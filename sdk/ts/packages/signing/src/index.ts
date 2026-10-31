/**
 * dilf4s signatures, key derivation, VRFs and key-evolving signatures. The normative specifications are
 * `docs/specs/signing.md`, `vrf.md` and `kes.md`; the conformance vectors in `shared/vectors` are part of them.
 *
 * Malformed input (a wrong length, a key that does not decode) is a typed error; a well-formed input that fails
 * verification is `false`. No function throws.
 *
 * Not side-channel hardened: JavaScript gives no constant-time guarantee, and part of the secret-key arithmetic
 * (BIP32 and Icarus child keys, VRF and ristretto255 responses) is plain `bigint`. Verification is unaffected; sign
 * where an attacker can time you (a server, a shared host) with the Rust SDK, whose secret scalars are constant time.
 */
export type { KesError, KeyError, SigningError, VrfError } from "./errors.js";
export { Secret, Signature, SigningKey, VerifyingKey, type KeyPair, type SchemeName } from "./keys.js";
export type { SchemeCodec, SeedDerivation, Verifier } from "./scheme.js";

export { ed25519, ed25519Bip32, type Ed25519Bip32Scheme, type Ed25519Scheme } from "./ed25519.js";
export { p256Ecdsa, secp256k1Ecdsa, type EcdsaScheme, type EcdsaSchemeName } from "./ecdsa.js";
export { bip340, Message32, taggedSchnorr, type Bip340Scheme, type TaggedSchnorrScheme } from "./schnorr.js";
export { ristretto255Schnorr, type Ristretto255Scheme } from "./ristretto255.js";
export { blsPop, POP_DST, SIGNATURE_DST, type BlsScheme } from "./bls.js";

export { generateMnemonic, Mnemonic, mnemonicToSeed, toEntropy, toMnemonic, WordList } from "./bip39.js";
export { bip32Secp256k1, ChildIndex, ExtendedKey, icarus, parsePath, slip10Ed25519, type PublicChild } from "./derivation.js";

export { ecvrfDraft03, ecvrfEll2, ecvrfTai, VrfOutput, VrfProof, type Vrf, type VrfSuiteName } from "./vrf.js";
export { CardanoSumKes, CardanoSumKesKey, KesProduct, KesProductKey, KesSum, KesSumKey } from "./kes.js";
