"""`Ok` and `Err`: what every call that can refuse gives, never an exception for a refusal.

Both are frozen, so a result can be shared and hashed (when what it holds can be), and both match by class::

    match dilf4s.core.cbor_decode(data):
        case Ok(value):
            ...
        case Err(error):
            ...
"""

from dataclasses import dataclass
from typing import final


@final
@dataclass(frozen=True, slots=True)
class Ok[T]:
    """A call's value."""

    value: T


@final
@dataclass(frozen=True, slots=True)
class Err[E]:
    """A call's refusal: one of the error families of `dilf4s.errors`, or one of the SDK's own (`dilf4s.TooLarge`)."""

    error: E


type Result[T, E] = Ok[T] | Err[E]
"""A call's outcome: `Ok` of its value, or `Err` of its refusal."""
