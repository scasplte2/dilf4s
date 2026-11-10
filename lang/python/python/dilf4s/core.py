"""The canonical data model: values, canonical CBOR, the JSON mapping, tagged hashing and value commitments.

The normative specification is docs/specs/canonical-encoding.md, with the vectors of shared/vectors. A `Value` is the
Rust SDK's own, immutable and sharing its parts: building, comparing and returning one never copies what it shares.
Encoding, hashing and committing cost a value's weight, which for shared parts may be far more than its memory: bound
a value not known to be small with the `_within` forms.
"""

from typing import Final

from dilf4s import _native
from dilf4s._native import (
    Value,
    build_value,
    cbor_decode,
    cbor_decode_with_limit,
    cbor_encode,
    cbor_encode_within,
    cbor_encoded_length_within,
    content_hash,
    digest,
    digest_from_hex,
    domain_tag,
    expand_message_xmd,
    hash_algorithm,
    hash_parts,
    hash_sizes,
    json_parse,
    json_parse_with_max_scale,
    json_render,
    preview,
    tagged_hash,
    value_commitment,
)
from dilf4s._native_types import Buffer, Data, HashAlgorithmName, PythonData
from dilf4s._preview import Preview
from dilf4s._refusals import DivisionByZero, TooLarge, TooManyCopies
from dilf4s.errors.canonical import CanonicalError, CanonicalErrorKind, TypeMismatch
from dilf4s.errors.hash import HashError, HashErrorKind

MAX_DEPTH: Final[int] = _native.MAX_DEPTH
"""The nesting limit of arrays and maps."""
MAX_INTEGER_BITS: Final[int] = _native.MAX_INTEGER_BITS
"""The widest integer, in Java `bitLength` bits."""
MAX_RATIONAL_BITS: Final[int] = _native.MAX_RATIONAL_BITS
"""The widest numerator or denominator of a rational, in Java `bitLength` bits."""
MAX_WEIGHT: Final[int] = _native.MAX_WEIGHT
"""Weights saturate here."""
HASH_ALGORITHMS: Final[tuple[str, ...]] = _native.HASH_ALGORITHMS
"""Every hash algorithm, by the name the specs use."""

__all__ = [
    "HASH_ALGORITHMS",
    "MAX_DEPTH",
    "MAX_INTEGER_BITS",
    "MAX_RATIONAL_BITS",
    "MAX_WEIGHT",
    "Buffer",
    "CanonicalError",
    "CanonicalErrorKind",
    "Data",
    "DivisionByZero",
    "HashAlgorithmName",
    "HashError",
    "HashErrorKind",
    "Preview",
    "PythonData",
    "TooLarge",
    "TooManyCopies",
    "TypeMismatch",
    "Value",
    "build_value",
    "cbor_decode",
    "cbor_decode_with_limit",
    "cbor_encode",
    "cbor_encode_within",
    "cbor_encoded_length_within",
    "content_hash",
    "digest",
    "digest_from_hex",
    "domain_tag",
    "expand_message_xmd",
    "hash_algorithm",
    "hash_parts",
    "hash_sizes",
    "json_parse",
    "json_parse_with_max_scale",
    "json_render",
    "preview",
    "tagged_hash",
    "value_commitment",
]
