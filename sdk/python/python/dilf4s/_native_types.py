"""The closed sets of names the native module takes, as `Literal` types, and the argument types it reads.

tests/test_api.py checks each set against the names the native module holds.
"""

from fractions import Fraction
from typing import Literal

from dilf4s._native import Value

type Buffer = bytes | bytearray | memoryview
"""Bytes in: `bytes` are read in place; a `bytearray` or `memoryview` is copied once, as its `tobytes()`."""

type Data = (
    bool
    | int
    | Fraction
    | str
    | bytes
    | bytearray
    | memoryview
    | list[Data]
    | tuple[Data, ...]
    | dict[str, Data]
    | Value
    | None
)
"""Python data `Value.of` reads: arrays as lists or tuples, maps as dicts with `str` keys (`None` values dropped)."""

type PythonData = bool | int | Fraction | str | bytes | list[PythonData] | dict[str, PythonData] | None
"""Python data `Value.to_python` gives: arrays as lists, maps as dicts in canonical key order."""

type HashAlgorithmName = Literal[
    "SHA-256", "SHA-512", "SHA3-256", "SHA3-512", "Keccak-256", "BLAKE2b-256", "BLAKE2b-512"
]
"""A hash algorithm, by the name the specs use. Any other text is `UnknownAlgorithm`."""

type Scheme = Literal[
    "Ed25519",
    "Ed25519-libsodium",
    "P-256-ECDSA",
    "secp256k1-ECDSA",
    "BIP-340",
    "secp256k1-tagged-Schnorr",
    "ristretto255-Schnorr",
    "BLS12-381-PoP",
]
"""A verification scheme (signing.md)."""

type Signer = Literal[
    "Ed25519",
    "Ed25519-BIP32",
    "P-256-ECDSA",
    "secp256k1-ECDSA",
    "BIP-340",
    "secp256k1-tagged-Schnorr",
    "ristretto255-Schnorr",
    "BLS12-381-PoP",
]
"""A scheme a `SigningKey` signs for."""

type VrfSuite = Literal[
    "ECVRF-EDWARDS25519-SHA512-TAI", "ECVRF-EDWARDS25519-SHA512-ELL2", "ECVRF-ED25519-SHA512-Elligator2 (draft-03)"
]
"""A VRF suite (vrf.md)."""

type KesConstruction = Literal["dilf4s-sum", "dilf4s-product", "cardano-sum"]
"""A KES construction (kes.md)."""

type MerkleProfileName = Literal["tagged", "rfc6962"]
"""A Merkle tree profile (merkle.md): dilf4s's tagged hashes, or RFC 6962's."""
