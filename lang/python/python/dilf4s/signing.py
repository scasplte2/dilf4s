"""Signatures, key derivation, VRFs, key-evolving signatures and sigma proofs.

The specifications are docs/specs/signing.md, vrf.md, kes.md and sigma.md. Verification of every scheme is by name
(`SCHEMES`): malformed input (a wrong length, a key that does not decode) is a typed error, and a well-formed signature
that does not verify is `False`. Secret keys never show their bytes in a `repr` and compare in constant time; their
Rust values are zeroized when the last reference drops, but `to_bytes` gives Python a copy it cannot zeroize.
Randomised signers take their randomness explicitly.
"""

from typing import Final

from dilf4s import _native
from dilf4s._native import (
    ExtendedKey,
    Kes,
    KesKey,
    SigmaProposition,
    SigningKey,
    VrfSigningKey,
    WordList,
    bip32_public_child,
    bls_aggregate,
    bls_aggregate_verify,
    bls_batch_verify,
    bls_fast_aggregate_verify,
    bls_pop_verify,
    ecrecover_address,
    eth_address_of_key,
    icarus_public_child,
    mnemonic_from_entropy,
    mnemonic_to_entropy,
    mnemonic_to_seed,
    parse_path,
    sigma_verify,
    sigma_verify_dlog,
    vrf_verify,
)
from dilf4s._native import signing_decode_verifying_key as decode_verifying_key
from dilf4s._native import signing_verify as verify
from dilf4s._native_types import KesConstruction, Scheme, Signer, VrfSuite
from dilf4s.errors.crypto import CryptoError, CryptoErrorKind
from dilf4s.errors.kes import KesError, KesErrorKind
from dilf4s.errors.key import KeyError, KeyErrorKind, PathRefusal  # noqa: A004 - the schema's name, as in Rust
from dilf4s.errors.sigma import SigmaError, SigmaErrorKind
from dilf4s.errors.vrf import VrfError, VrfErrorKind

SCHEMES: Final[tuple[Scheme, ...]] = _native.SIGNING_SCHEMES
"""Every verification scheme, by the name signing.md gives it."""
SIGNERS: Final[tuple[Signer, ...]] = _native.SIGNING_SIGNERS
"""Every scheme a `SigningKey` signs for."""
VRF_SUITES: Final[tuple[VrfSuite, ...]] = _native.SIGNING_VRF_SUITES
"""Every VRF suite."""
HARDENED: Final[int] = 2**31
"""The first hardened child index."""

__all__ = [
    "HARDENED",
    "SCHEMES",
    "SIGNERS",
    "VRF_SUITES",
    "CryptoError",
    "CryptoErrorKind",
    "ExtendedKey",
    "Kes",
    "KesConstruction",
    "KesError",
    "KesErrorKind",
    "KesKey",
    "KeyError",
    "KeyErrorKind",
    "PathRefusal",
    "Scheme",
    "SigmaError",
    "SigmaErrorKind",
    "SigmaProposition",
    "Signer",
    "SigningKey",
    "VrfError",
    "VrfErrorKind",
    "VrfSigningKey",
    "VrfSuite",
    "WordList",
    "bip32_public_child",
    "bls_aggregate",
    "bls_aggregate_verify",
    "bls_batch_verify",
    "bls_fast_aggregate_verify",
    "bls_pop_verify",
    "decode_verifying_key",
    "ecrecover_address",
    "eth_address_of_key",
    "icarus_public_child",
    "mnemonic_from_entropy",
    "mnemonic_to_entropy",
    "mnemonic_to_seed",
    "parse_path",
    "sigma_verify",
    "sigma_verify_dlog",
    "verify",
    "vrf_verify",
]
