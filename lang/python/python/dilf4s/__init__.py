"""dilf4s for Python: the canonical data model, signatures, proof verifiers and the JLVM, bound to the Rust SDK.

- `dilf4s.core`: canonical values, CBOR, the JSON mapping, tagged hashing and value commitments.
- `dilf4s.signing`: signature verification for every scheme, signing and key derivation, VRFs, KES and sigma proofs.
- `dilf4s.proofs`: Merkle trees, tries, versioned tries, Poseidon, BN254 and Groth16 (SP1's included).
- `dilf4s.jlvm`: the JLVM, a deterministic, gas-metered expression language for contracts.
- `dilf4s.errors`: every refusal, one module per error family of `shared/schemas`.

Every call that can refuse gives `Ok(value)` or `Err(error)` (frozen, and matched by class), never an exception for a
refusal: an argument of the wrong type is `Err(TypeMismatch(...))`. Every type is immutable. Calling with the wrong
number of arguments, or an unknown keyword, raises `TypeError` before any SDK code runs, as for any Python function.
"""

from dilf4s import core, errors, jlvm, proofs, signing
from dilf4s._native import Value
from dilf4s._preview import Preview
from dilf4s._refusals import DivisionByZero, TooLarge, TooManyCopies
from dilf4s._result import Err, Ok, Result

__all__ = [
    "DivisionByZero",
    "Err",
    "Ok",
    "Preview",
    "Result",
    "TooLarge",
    "TooManyCopies",
    "Value",
    "core",
    "errors",
    "jlvm",
    "proofs",
    "signing",
]
