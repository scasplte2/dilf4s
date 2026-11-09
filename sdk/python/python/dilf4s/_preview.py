"""`Preview`: caller text as an error holds it (docs/specs/conformance.md section 8)."""

from dataclasses import dataclass
from typing import final


@final
@dataclass(frozen=True, slots=True)
class Preview:
    """Caller text as an error holds it, however long the text was.

    Attributes:
        head: The first 32 code points, each unpaired surrogate replaced by U+FFFD.
        length: The whole text's length in UTF-8 bytes, an unpaired surrogate counting 3.
        truncated: Whether the text was longer than the head.
    """

    head: str
    length: int
    truncated: bool
