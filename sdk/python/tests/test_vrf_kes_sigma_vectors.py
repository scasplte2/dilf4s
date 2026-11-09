"""`vrf_v1.json`, `kes_v1.json`, `sigma_v1.json`, `sigma_dilf4s_v1.json` and `sigma_refusals_v1.json` through the
Python API."""

from typing import Final, get_args

from dilf4s import Err, Ok, Value
from dilf4s.core import Data, cbor_decode
from dilf4s.errors import kes as kes_errors
from dilf4s.errors import sigma as sigma_errors
from dilf4s.errors import vrf as vrf_errors
from dilf4s.signing import (
    VRF_SUITES,
    Kes,
    SigmaProposition,
    VrfSigningKey,
    sigma_verify,
    sigma_verify_dlog,
    vrf_verify,
)
from tests.support import Json, err, failures, kind_of, load, ok

VRF: Final = load("vrf_v1.json")
KES: Final = load("kes_v1.json")
SIGMA_REFUSALS: Final = load("sigma_refusals_v1.json")


# ------------------------------------------------------------------------------------------------ VRF


def test_vrf_keys_proofs_and_outputs() -> None:
    count = 0
    for suite in VRF["suites"]:
        name = suite["suite"]
        for k in suite["keys"]:
            sk = ok(VrfSigningKey.from_seed(name, bytes.fromhex(k["seed"])), f"{name} key")
            assert sk.to_bytes().hex() == k["signingKey"], f"{name} signing key"
            assert sk.verifying_key().hex() == k["verifyingKey"], f"{name} verifying key"
            for p in k["proofs"]:
                alpha = bytes.fromhex(p["alpha"])
                beta = vrf_verify(name, bytes.fromhex(k["verifyingKey"]), alpha, bytes.fromhex(p["pi"]))
                assert beta == Ok(bytes.fromhex(p["beta"])), f"{name} verify"
                pi, out = ok(sk.prove(alpha), f"{name} prove")
                assert (pi.hex(), out.hex()) == (p["pi"], p["beta"]), f"{name} prove {p['alpha']}"
                count += 1
    assert count > 0
    assert {s["suite"] for s in VRF["suites"]} == set(VRF_SUITES), "every suite has keys"


def test_vrf_rejections() -> None:
    def check(r: Json) -> str | None:
        got = vrf_verify(r["suite"], *(bytes.fromhex(r[f]) for f in ("verifyingKey", "alpha", "pi")))
        outcome = "valid" if isinstance(got, Ok) else kind_of(got.error)
        return None if outcome == r["result"] else f"{r['suite']} {r['name']}: expected {r['result']}, got {outcome}"

    failures(VRF["rejections"], check)


def test_vrf_signing_key_decoding() -> None:
    def check(c: Json) -> str | None:
        got = VrfSigningKey.from_bytes(c["suite"], bytes.fromhex(c["bytes"]))
        outcome = "accepted" if isinstance(got, Ok) else kind_of(got.error)
        return None if outcome == c["result"] else f"{c['suite']} {c['name']}: expected {c['result']}, got {outcome}"

    failures(VRF["keyDecoding"], check)


def test_every_vrf_error_kind_has_a_row() -> None:
    rejected = {r["result"] for r in VRF["rejections"]}
    decoded = {c["result"] for c in VRF["keyDecoding"]}
    for k in get_args(vrf_errors.VrfErrorKind):
        exempt = k == "HashToCurveFailed"
        assert (k in rejected | decoded) != exempt, f"a row pins {k} exactly when it is not exempt"
    verification = {"Malformed", "InvalidPublicKey", "SmallOrderPublicKey", "InvalidGamma", "NonCanonicalScalar"}
    assert verification | {"ChallengeMismatch"} <= rejected, "verification refuses malformed input and rejects the rest"
    assert {"Malformed", "KeyMismatch"} <= decoded, "signing-key decoding refuses a wrong length and another key"


# ------------------------------------------------------------------------------------------------ KES


def scheme_of(construction: str, p: Json) -> Ok[Kes] | Err[object]:
    match construction:
        case "dilf4s-sum":
            return Kes.sum(p["hash"], p["height"])
        case "dilf4s-product":
            return Kes.product(p["hash"], p["superHeight"], p["subHeight"])
        case "cardano-sum":
            return Kes.cardano(p["depth"])
    msg = f"unknown construction {construction}"
    raise AssertionError(msg)


def outcome(r: Ok[object] | Err[object]) -> str:
    """A key operation's outcome as a row records it."""
    return "accepted" if isinstance(r, Ok) else kind_of(r.error)


def refusal_outcome(c: Json) -> str:
    scheme = scheme_of(c["construction"], c["parameters"])
    if isinstance(scheme, Err):
        return kind_of(scheme.error)
    kes = scheme.value

    def field(f: str) -> bytes:
        return bytes.fromhex(c.get(f, ""))

    period = c.get("period", 0)
    match c["check"]:
        case "parameters":
            return "accepted"
        case "verify":
            verdict = kes.verify(field("verifyingKey"), period, field("message"), field("signature"))
            if isinstance(verdict, Ok):
                return "accepted" if verdict.value else "rejected"
            return kind_of(verdict.error)
        case "generate":
            return outcome(kes.generate(field("seed")))
        case "load":
            return outcome(kes.load(field("key"), period))
        case "evolve":
            loaded = kes.load(field("key"), period)
            return outcome(loaded.value.evolve() if isinstance(loaded, Ok) else loaded)
    msg = f"unknown check {c['check']}"
    raise AssertionError(msg)


def test_kes_refusals_by_kind() -> None:
    rows = KES["refusals"]
    failures(rows, lambda c: None if refusal_outcome(c) == c["outcome"] else f"{c['name']}: {refusal_outcome(c)}")
    assert len(rows) >= 60
    outcomes = {c["outcome"] for c in rows}
    assert {"accepted", "rejected", *get_args(kes_errors.KesErrorKind)} <= outcomes, "every outcome has a row"


def test_every_kes_period_verifies_and_nowhere_else() -> None:
    count = 0
    for v in KES["vectors"]:
        kes = ok(scheme_of(v["construction"], v["parameters"]), v["construction"])
        vk, message = bytes.fromhex(v["verifyingKey"]), bytes.fromhex(v["message"])
        assert len(v["periods"]) == kes.total_periods
        for p in v["periods"]:
            sig = bytes.fromhex(p["signature"])
            assert kes.verify(vk, p["period"], message, sig) == Ok(True), f"{v['construction']} period {p['period']}"
            for other in (t for t in range(kes.total_periods) if t != p["period"]):
                assert kes.verify(vk, other, message, sig) == Ok(False), f"other period {other}"
            assert kes.verify(vk, p["period"], message + b"\0", sig) == Ok(False), "another message"
            assert isinstance(err(kes.verify(vk, kes.total_periods, message, sig), "past"), kes_errors.PeriodOutOfRange)
            assert isinstance(err(kes.verify(vk[1:], p["period"], message, sig), "short key"), kes_errors.Malformed)
            assert isinstance(err(kes.verify(vk, p["period"], message, sig[1:]), "short sig"), kes_errors.Malformed)
            count += 1
    assert count == 4 + 2 + 8 + 4


def test_kes_keys_signatures_loading_and_evolution_byte_for_byte() -> None:
    for v in KES["vectors"]:
        kes = ok(scheme_of(v["construction"], v["parameters"]), v["construction"])
        message = bytes.fromhex(v["message"])
        last = v["periods"][-1]["period"]
        key = ok(kes.generate(bytes.fromhex(v["seed"])), "generate")
        assert key.verifying_key().hex() == v["verifyingKey"]
        for p in v["periods"]:
            assert key.period == p["period"]
            assert key.to_bytes().hex() == p["key"], f"{v['construction']} key at {p['period']}"
            assert ok(key.sign(message), "sign").hex() == p["signature"], f"signature at {p['period']}"
            loaded = ok(kes.load(bytes.fromhex(p["key"]), p["period"]), "load")
            assert ok(loaded.sign(message), "sign loaded").hex() == p["signature"]
            assert loaded.verifying_key() == key.verifying_key()
            if p["period"] != last:
                assert isinstance(kes.load(bytes.fromhex(p["key"]), p["period"] + 1), Err), "loads only at its period"
                key = ok(key.evolve(), "evolve")
        assert err(key.evolve(), "exhausted") == kes_errors.Exhausted(period=last)
        first = ok(kes.generate(bytes.fromhex(v["seed"])), "generate")
        assert ok(first.evolve_to(last), "evolve_to").to_bytes().hex() == v["periods"][-1]["key"], "evolve_to the last"


# ------------------------------------------------------------------------------------------------ sigma


def hex0x(text: str) -> bytes:
    assert text.startswith("0x"), f"not 0x-hex: {text}"
    return bytes.fromhex(text[2:])


def to_value(p: Json) -> Value:
    """A vector proposition's canonical form: its hex fields as bytes, as the JLVM holds them."""
    entries: dict[str, Data] = {"type": p["type"]}
    for k in ("pk", "g", "h", "u", "v"):
        if k in p:
            entries[k] = hex0x(p[k])
    if "k" in p:
        entries["k"] = p["k"]
    if "children" in p:
        entries["children"] = [to_value(c) for c in p["children"]]
    return ok(Value.of(entries), "proposition form")


def build(p: Json) -> Ok[SigmaProposition] | Err[object]:
    """The same proposition through the constructors."""
    children = [ok(build(c), "child") for c in p.get("children", [])]
    match p["type"]:
        case "dlog":
            return SigmaProposition.dlog(hex0x(p["pk"]))
        case "dhtuple":
            return SigmaProposition.dh_tuple(*(hex0x(p[f]) for f in ("g", "h", "u", "v")))
        case "and":
            return SigmaProposition.and_(children)
        case "or":
            return SigmaProposition.or_(children)
        case "threshold":
            return SigmaProposition.threshold(p["k"], children)
    msg = f"unknown type {p['type']}"
    raise AssertionError(msg)


def sigma_outcome(r: Ok[bool] | Err[object]) -> str:
    match r:
        case Ok(v):
            return "accepted" if v else "rejected"
        case Err(e):
            return kind_of(e)


def run_sigma(file: str, outcomes: list[str]) -> None:
    cases = load(file)["cases"]
    assert len(cases) == len(outcomes) == 120, f"{file}: one outcome per case"
    tally = {"valid": 0, "wrong_length": 0}
    for c, expected in zip(cases, outcomes, strict=True):
        what = f"{file} {c['name']} ({c['kind']})"
        value = to_value(c["proposition"])
        prop = ok(SigmaProposition.from_value(value), f"{what}: proposition")
        assert build(c["proposition"]) == Ok(prop), f"{what}: the constructors build the same proposition"
        proof, message = hex0x(c["proof"]), hex0x(c["message"])
        ours = sigma_verify(value, proof, message)
        assert ours == sigma_verify(prop, proof, message), f"{what}: a proposition and its form verify alike"
        if c["proposition"]["type"] == "dlog":
            assert sigma_verify_dlog(hex0x(c["proposition"]["pk"]), message, proof) == ours, f"{what}: dlog"
        assert sigma_outcome(ours) == expected, f"{what}: expected {expected}, got {ours}"
        if c["kind"] == "valid" and c["ergo"]:
            assert ours == Ok(True), f"{what}: valid"
            assert len(proof) == prop.proof_length, f"{what}: proof length"
            tally["valid"] += 1
        if c["kind"] in {"trailingByte", "truncated"} and isinstance(ours, Err):
            assert isinstance(ours.error, sigma_errors.ProofLength), f"{what}: a length refusal"
            assert ours.error.expected == prop.proof_length, f"{what}: the expected length"
            tally["wrong_length"] += 1
    assert tally["valid"] == 20, f"{file}: valid proofs"


def test_ergo_proofs() -> None:
    run_sigma("sigma_v1.json", SIGMA_REFUSALS["ergoCases"])


def test_dilf4s_proofs() -> None:
    run_sigma("sigma_dilf4s_v1.json", SIGMA_REFUSALS["dilf4sCases"])


def test_sigma_refusals_by_kind() -> None:
    def check(c: Json) -> str | None:
        prop = ok(cbor_decode(bytes.fromhex(c["proposition"])), c["name"])
        got = sigma_outcome(sigma_verify(prop, bytes.fromhex(c["proof"]), bytes.fromhex(c["message"])))
        return None if got == c["outcome"] else f"{c['name']}: expected {c['outcome']}, got {got}"

    assert failures(SIGMA_REFUSALS["cases"], check) >= 20
    outcomes = {c["outcome"] for c in SIGMA_REFUSALS["cases"]}
    assert {"accepted", "rejected", *get_args(sigma_errors.SigmaErrorKind)} <= outcomes, "every outcome has a row"
