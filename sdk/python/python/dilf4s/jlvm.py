"""The JLVM: a strict, deterministic, gas-metered expression language for ledgers, in JsonLogic syntax.

Language version 1 on gas schedule version 1 (docs/specs/jlvm.md): the Rust SDK's evaluator, over its native
cryptographic backend. A result is a function of the program, the data, the bindings, the language version and the gas
schedule; an evaluation reports the gas it used whether it succeeded or failed::

    match jlvm.parse('{"+": [1, {"var": "x"}]}'), jlvm.parse_value('{"x": 2}'):
        case Ok(program), Ok(data):
            outcome = jlvm.evaluate(program, data)

Evaluation releases the GIL, so contracts run on other threads in parallel.
"""

from typing import Final

from dilf4s import _native
from dilf4s._jlvm_types import AmbiguousPath, JlvmWarning, Outcome
from dilf4s._native import Compiled, Program
from dilf4s._native import jlvm_compile as compile  # noqa: A004 - the Rust SDK's name
from dilf4s._native import jlvm_decode as decode
from dilf4s._native import jlvm_evaluate as evaluate
from dilf4s._native import jlvm_from_canonical as from_canonical
from dilf4s._native import jlvm_parse as parse
from dilf4s._native import jlvm_parse_value as parse_value
from dilf4s.errors.jlvm import JlvmError, JlvmErrorKind, JlvmSite
from dilf4s.errors.jlvm_crypto_cause import JlvmCryptoCause

VERSION: Final[int] = _native.JLVM_VERSION
"""The language version."""
GAS_SCHEDULE: Final[int] = _native.JLVM_GAS_SCHEDULE
"""The gas schedule version every evaluation uses."""
DEFAULT_GAS_LIMIT: Final[int] = _native.JLVM_DEFAULT_GAS_LIMIT
"""The gas limit when none is given: 2**24."""
MAX_GAS_LIMIT: Final[int] = _native.JLVM_MAX_GAS_LIMIT
"""The largest gas limit, 2**53 - 1: a limit outside 0 to it is the `Limit` outcome, with no gas used."""
MAX_WARNINGS: Final[int] = _native.JLVM_MAX_WARNINGS
"""At most this many distinct warnings are kept per evaluation."""
PROGRAM_TAG: Final[str] = _native.JLVM_PROGRAM_TAG
"""The domain tag of a program's identity."""

__all__ = [
    "DEFAULT_GAS_LIMIT",
    "GAS_SCHEDULE",
    "MAX_GAS_LIMIT",
    "MAX_WARNINGS",
    "PROGRAM_TAG",
    "VERSION",
    "AmbiguousPath",
    "Compiled",
    "JlvmCryptoCause",
    "JlvmError",
    "JlvmErrorKind",
    "JlvmSite",
    "JlvmWarning",
    "Outcome",
    "Program",
    "compile",
    "decode",
    "evaluate",
    "from_canonical",
    "parse",
    "parse_value",
]
