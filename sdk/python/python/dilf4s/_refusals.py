"""The SDK's own refusals, which no schema family holds (as in the Rust and TypeScript SDKs)."""

from dataclasses import dataclass, field
from typing import Literal, final


@final
@dataclass(frozen=True, slots=True)
class TooLarge:
    """An encoding, a rendering or a weight over a limit. It carries the limit only: how far past it is never measured.

    Attributes:
        limit: The limit: bytes for an encoding or a rendering, weight for `Value.to_python`.
    """

    limit: int
    kind: Literal["TooLarge"] = field(default="TooLarge", init=False)


@final
@dataclass(frozen=True, slots=True)
class DivisionByZero:
    """`Value.number` of a zero denominator."""

    kind: Literal["DivisionByZero"] = field(default="DivisionByZero", init=False)


@final
@dataclass(frozen=True, slots=True)
class TooManyCopies:
    """A `built-value` recipe asking for more copies than this platform can index.

    Attributes:
        copies: The copies asked for.
    """

    copies: int
    kind: Literal["TooManyCopies"] = field(default="TooManyCopies", init=False)
