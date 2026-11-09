"""Reading shared/vectors, and unwrapping results with a message that says what was expected."""

import json
from collections.abc import Callable, Mapping, Sequence
from decimal import Decimal
from pathlib import Path
from typing import Any, Final, Literal, TypeGuard, assert_never

from dilf4s import Err, Ok, Result
from dilf4s.core import Buffer, Value, build_value
from dilf4s.signing import SCHEMES, SIGNERS, VRF_SUITES, Scheme, Signer, VrfSuite

VECTORS: Final = Path(__file__).resolve().parents[3] / "shared" / "vectors"

type Json = Any
"""A vector file's JSON, as `json` reads it: the vectors are checked against their schemas by every SDK's own tests."""


def load(file: str) -> Json:
    """A vector file; a number with a fraction or exponent is a `Decimal`, never a float."""
    return json.loads((VECTORS / file).read_text(encoding="utf-8"), parse_float=Decimal)


def ok[T, E](result: Result[T, E], what: str) -> T:
    """The value of an `Ok`, or a failure naming `what` and the refusal."""
    match result:
        case Ok(value):
            return value
        case Err(error):
            msg = f"{what}: expected Ok, got Err({error!r})"
            raise AssertionError(msg)
        case _:
            assert_never(result)


def err[T, E](result: Result[T, E], what: str) -> E:
    """The refusal of an `Err`, or a failure naming `what` and the value."""
    match result:
        case Err(error):
            return error
        case Ok(value):
            msg = f"{what}: expected Err, got Ok({value!r})"
            raise AssertionError(msg)
        case _:
            assert_never(result)


def kind_of(error: object) -> str:
    """An error's kind: every refusal the SDK gives has one."""
    kind = getattr(error, "kind", None)
    assert isinstance(kind, str), f"{error!r} has no kind"
    return kind


def built(recipe: Mapping[str, Json]) -> Value:
    """A `built-value` (refusal-vectors.cddl): the leaf, then each step, built with sharing by the SDK's builder."""
    steps: list[tuple[Literal["wrap"], Buffer, int] | tuple[Literal["copies"], int]] = []
    for step in recipe.get("steps", []):
        if "copies" in step:
            steps.append(("copies", step["copies"]))
        else:
            steps.append(("wrap", bytes.fromhex(step["template"]), step["times"]))
    return ok(build_value(bytes.fromhex(recipe["leaf"]), steps), f"built {recipe}")


def untyped(x: object) -> Any:
    """`x`, typed as anything: an argument of the wrong type, passed on purpose."""
    return x


def is_scheme(name: str) -> TypeGuard[Scheme]:
    """Whether a vector's scheme name is one of the SDK's."""
    return name in SCHEMES


def is_signer(name: str) -> TypeGuard[Signer]:
    """Whether a vector's scheme name is one a signing key signs for."""
    return name in SIGNERS


def is_suite(name: str) -> TypeGuard[VrfSuite]:
    """Whether a vector's suite name is one of the SDK's."""
    return name in VRF_SUITES


def failures[T](cases: Sequence[T], check: Callable[[T], str | None]) -> int:
    """Checks every case, collecting failures so one run reports all of them; the number of cases."""
    found = [f for f in map(check, cases) if f is not None]
    assert not found, f"{len(found)} of {len(cases)} cases failed:\n" + "\n".join(found[:60])
    return len(cases)
