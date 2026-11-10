"""`shared/vectors/jlvm_v1.json` through the Python API, as the reference's `JlvmVectors.run` runs it.

Each case: parse the program, the data and the host bindings (the first failure is the outcome, with no gas), add the
bindings built with shared parts, then evaluate with the case's gas limit (the file's when absent). Every case must give
the same value (canonical CBOR) or error (its kind, `op` and `cause`), the same gas and the same number of warnings.
"""

from typing import Final, get_args

import pytest

from dilf4s import Err, Ok, Value, jlvm
from dilf4s.core import TypeMismatch, cbor_encode, hash_algorithm
from dilf4s.errors import jlvm as jlvm_errors
from dilf4s.errors import jlvm_crypto_cause
from dilf4s.jlvm import (
    JlvmError,
    Outcome,
    Program,
    decode,
    evaluate,
    from_canonical,
    parse,
    parse_value,
)
from tests.support import VECTORS as VECTOR_DIR
from tests.support import Json, built, err, failures, load, ok

VECTORS: Final = load("jlvm_v1.json")


def failed(error: JlvmError | TypeMismatch) -> Outcome:
    """A parse failure as an outcome: no gas, no warnings."""
    assert not isinstance(error, TypeMismatch), f"a vector's text is a str: {error!r}"
    return Outcome(gas_used=0, result=Err(error), warnings=())


def outcome_of(case: Json) -> Outcome:
    """`JlvmVectors.run`: the first parse failure (program, then data, then bindings) is the outcome, with no gas."""
    program = parse(case["program"])
    if isinstance(program, Err):
        return failed(program.error)
    data = parse_value(case["data"])
    if isinstance(data, Err):
        return failed(data.error)
    bindings: dict[str, Value] = {}
    for name, text in case.get("env", {}).items():
        parsed = parse_value(text)
        if isinstance(parsed, Err):
            return failed(parsed.error)
        bindings[name] = parsed.value
    for name, recipe in case.get("built", {}).items():
        bindings[name] = built(recipe)
    limit = case.get("gasLimit", VECTORS["gasLimit"])
    return ok(evaluate(program.value, data.value, bindings, limit), case["name"])


def cause_of(error: JlvmError) -> dict[str, str] | None:
    """A Crypto error's cause as the vectors record it: its family (camelCase) and that family's kind."""
    if not isinstance(error, jlvm_errors.Crypto) or error.cause is None:
        return None
    family = error.cause.kind
    return {family[0].lower() + family[1:]: error.cause.error.kind}


def described(o: Outcome) -> str:
    """An outcome as the vectors record it: a value's CBOR, or an error's kind, `op` and cause."""
    match o.result:
        case Ok(v):
            return f"value {ok(cbor_encode(v), 'encode').hex()}"
        case Err(e):
            op = getattr(e, "op", None)
            return f"error {e.kind} op {op} cause {cause_of(e)}"


def expected(case: Json) -> str:
    """A case's outcome as `described` writes it."""
    out = case["outcome"]
    if "error" in out:
        return f"error {out['error']} op {out.get('op')} cause {out.get('cause')}"
    return f"value {out['cbor']}"


def mismatch(case: Json) -> str | None:
    """Where a case's outcome differs from the vector's, if it does."""
    o = outcome_of(case)
    want, got = expected(case), described(o)
    if want != got or o.gas_used != case["gasUsed"] or len(o.warnings) != case["warnings"]:
        return (
            f"{case['name']}: want {want} gas {case['gasUsed']} warnings {case['warnings']}, "
            f"got {got} gas {o.gas_used} warnings {len(o.warnings)} ({o.result})"
        )
    return None


def test_every_case_gives_its_outcome_gas_and_warnings() -> None:
    n = failures(VECTORS["cases"], mismatch)
    assert n == len(VECTORS["cases"]) == 3363


def test_the_file_is_the_version_and_schedule_this_sdk_runs() -> None:
    assert VECTORS["version"] == jlvm.VERSION, "language version"
    assert VECTORS["schedule"] == jlvm.GAS_SCHEDULE, "gas schedule"


def test_every_jlvm_error_kind_has_a_row() -> None:
    """The coverage gate (conformance.md section 6): every kind of the family has a case, through this API."""
    kinds = {c["outcome"]["error"] for c in VECTORS["cases"] if "error" in c["outcome"]}
    assert kinds == set(get_args(jlvm_errors.JlvmErrorKind)), "every kind, and only the family's kinds"
    families = {next(iter(c["outcome"]["cause"])) for c in VECTORS["cases"] if "cause" in c["outcome"]}
    named = {k[0].lower() + k[1:] for k in get_args(jlvm_crypto_cause.JlvmCryptoCauseKind)}
    assert families <= named, f"causes {families - named} are no family of the schema"


def test_each_alias_parses_to_its_operation_with_the_rows_form_and_identity() -> None:
    assert len(VECTORS["aliases"]) == 4, "one row per alias"
    for row in VECTORS["aliases"]:
        alias = ok(parse(row["program"]), row["alias"])
        own = ok(parse(row["own"]), row["op"])
        assert alias == own, f"{row['alias']} parses as {row['op']}"
        assert ok(alias.encode(), "encode").hex() == row["cbor"], f"{row['alias']}: canonical form"
        assert ok(alias.id(), "id").hex() == row["id"], f"{row['alias']}: identity"
        assert ok(alias.id("SHA-256"), "id").hex() == row["id"], f"{row['alias']}: identity, named"
        decoded = ok(decode(bytes.fromhex(row["cbor"])), f"{row['alias']}: decode")
        assert decoded == own, f"{row['alias']}: the CBOR is the program its own name writes"


def read_as(r: Ok[Program] | Err[JlvmError | TypeMismatch]) -> str:
    match r:
        case Ok(p):
            return f"cbor {ok(p.encode(), 'encode').hex()}"
        case Err(e):
            return f"error {e.kind}"


def test_every_canonical_form_is_read_as_its_row_says() -> None:
    """`decodings` (jlvm.md section 1): a form built with shared parts, read by `from_canonical`, and by `decode`."""
    rows = VECTORS["decodings"]
    assert len(rows) == 6, f"{len(rows)} decodings rows"
    for row in rows:
        form = built(row["form"])
        out = row["outcome"]
        want = f"error {out['error']}" if "error" in out else f"cbor {out['cbor']}"
        assert read_as(from_canonical(form)) == want, row["name"]
        if form.weight <= 1 << 20:
            assert read_as(decode(ok(cbor_encode(form), "encode"))) == want, f"{row['name']}, from its CBOR"


def test_every_program_round_trips_through_its_canonical_form() -> None:
    expected = dict(line.split("\t") for line in (VECTOR_DIR / "jlvm_parse_refusals_v1.tsv").read_text().splitlines())
    assert len(expected) == 67, "expected program parse refusals"
    assert len(VECTORS["cases"]) == 3363
    assert len(VECTORS["aliases"]) == 4
    parsed_cases = [(c["name"], parse(c["program"])) for c in VECTORS["cases"]]
    refused = {name: r.error.kind for name, r in parsed_cases if isinstance(r, Err)}
    assert refused == expected, "exact program parse refusals, by case name and error kind"
    accepted = [r.value for _, r in parsed_cases if isinstance(r, Ok)]
    accepted += [ok(parse(t), a["alias"]) for a in VECTORS["aliases"] for t in (a["program"], a["own"])]
    programs = 0
    for p in accepted:
        text = repr(p)
        encoded = ok(p.encode(), text)
        back = ok(decode(encoded), text)
        assert back == p, f"{text}: decoded"
        assert ok(back.encode(), text) == encoded, f"{text}: re-encoded"
        assert ok(back.id(), text) == ok(p.id(), text), f"{text}: identity"
        assert ok(from_canonical(ok(p.canonical(), text)), text) == p, f"{text}: from its form"
        programs += 1
    assert programs == 3304, f"{programs} programs round-trip"


def test_compiled_programs_evaluate_alike() -> None:
    p = ok(parse('{"+": [1, {"var": "x"}]}'), "parse")
    c = ok(jlvm.compile(p), "compile")
    data = ok(parse_value('{"x": 2}'), "data")
    a = ok(evaluate(p, data), "evaluate")
    b = ok(evaluate(c, data), "evaluate compiled")
    assert a == b, "a program and its compiled form evaluate alike"
    assert a.result == Ok(ok(Value.int(3), "3")), "1 + 2"
    assert c.program == p, "the compiled program is the program"


@pytest.mark.parametrize("name", ["SHA-512", "BLAKE2b-256"])
def test_identity_under_another_algorithm(name: str) -> None:
    p = ok(parse('{"var": "x"}'), "parse")
    assert ok(p.id(name), name) != ok(p.id(), "SHA-256"), "a different hash, a different identity"
    assert len(ok(p.id(name), name)) in {32, 64}, "a digest"
    assert ok(hash_algorithm(name), name) == name, "the name is an algorithm's"
    assert err(p.id("MD5"), "MD5").kind == "UnknownAlgorithm", "an unknown algorithm is refused"


def test_a_gas_limit_outside_its_range_is_limit_through_both_entry_points() -> None:
    """jlvm.md section 7: 0 to 2**53 - 1, metered alike at the maximum; outside it (negative, 2**53, or past what a
    u64 holds) the `Limit` outcome with no gas used, never a refusal of the call. Not an int is a `TypeMismatch`."""
    p = ok(parse('{"+": [1, 2]}'), "parse")
    c = ok(jlvm.compile(p), "compile")
    data = ok(parse_value("null"), "data")
    assert jlvm.MAX_GAS_LIMIT == 2**53 - 1, "the largest limit"
    for target in (p, c):
        at_max = ok(evaluate(target, data, None, jlvm.MAX_GAS_LIMIT), "at the maximum")
        assert at_max.result == Ok(ok(Value.int(3), "3")), "metered at the maximum"
        assert at_max.gas_used == 148, f"the gas at the maximum: {at_max.gas_used}"
        for limit in (-1, -(2**64), 2**53, 2**63, 2**64, 2**200):
            o = ok(evaluate(target, data, None, limit), f"limit {limit}")
            assert isinstance(o.result, Err), f"limit {limit}: refused, got {o.result!r}"
            assert o.result.error.kind == "Limit", f"limit {limit}: Limit, got {o.result.error.kind}"
            assert o.gas_used == 0, f"limit {limit}: no gas used, got {o.gas_used}"
        for wrong in (1.5, "10", True):
            assert isinstance(evaluate(target, data, None, wrong), Err), f"limit {wrong!r}: a TypeMismatch"  # type: ignore[arg-type]
