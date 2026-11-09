"""The JLVM's outcome and warnings, as the native module builds them."""

from dataclasses import dataclass, field
from typing import Literal, final

from dilf4s._native import Value
from dilf4s._preview import Preview
from dilf4s._result import Result
from dilf4s.errors.jlvm import JlvmError


@final
@dataclass(frozen=True, slots=True)
class AmbiguousPath:
    """Inside a callback, a plain path resolved in an element while the data or an outer element has its first key.

    `$current` (or an explicit outer path) says which one is meant. Two long paths that share their preview count as one
    warning. A warning never changes a result or its gas.

    Attributes:
        path: The path, as a preview.
    """

    path: Preview
    kind: Literal["AmbiguousPath"] = field(default="AmbiguousPath", init=False)


type JlvmWarning = AmbiguousPath
"""A lint finding of an evaluation."""


@final
@dataclass(frozen=True, slots=True)
class Outcome:
    """An evaluation's outcome: the gas it used whether it succeeded or failed, its value or error, and its warnings.

    Attributes:
        gas_used: The gas used; out of gas, the whole limit.
        result: `Ok` of the value, or `Err` of the error (its kind, and for the operation-level kinds its `op`, and for
            `Crypto` its `cause`, are normative).
        warnings: The distinct warnings, at most `MAX_WARNINGS`.
    """

    gas_used: int
    result: Result[Value, JlvmError]
    warnings: tuple[JlvmWarning, ...]
