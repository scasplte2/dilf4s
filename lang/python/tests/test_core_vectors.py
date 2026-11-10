"""`canonical_encoding_v1.json` and `diagnostics_v1.json` through the Python API."""

from typing import Final, get_args

from dilf4s import Err, Ok, TooLarge
from dilf4s.core import (
    HASH_ALGORITHMS,
    cbor_decode,
    cbor_encode,
    cbor_encode_within,
    cbor_encoded_length_within,
    content_hash,
    digest,
    digest_from_hex,
    domain_tag,
    expand_message_xmd,
    hash_algorithm,
    json_parse,
    json_render,
    preview,
    tagged_hash,
)
from dilf4s.errors import canonical
from dilf4s.errors import hash as hash_errors
from tests.support import Json, err, failures, kind_of, load, ok

V: Final = load("canonical_encoding_v1.json")
TAG: Final = V["contentHashTag"]
ALGORITHM: Final = V["contentHashAlgorithm"]


def refused(case: str, kind: str, limit: int | None, actual: object) -> str | None:
    """A refusal as a row records it: its kind and its normative limit (`RationalTooWide`, `IntegerTooWide`)."""
    got_limit = (
        getattr(actual, "limit", None)
        if isinstance(actual, canonical.RationalTooWide | canonical.IntegerTooWide)
        else None
    )
    got = (kind_of(actual), got_limit) if actual is not None else None
    return None if got == (kind, limit) else f"{case}: expected {kind} (limit {limit}), got {actual!r}"


def test_json_cases() -> None:
    def check(c: Json) -> str | None:
        r = json_parse(c["json"])
        if "error" in c:
            return refused(c["json"], c["error"], c.get("limit"), r.error if isinstance(r, Err) else None)
        v = ok(r, c["json"])
        problems = [
            ("cbor", c["cbor"], ok(cbor_encode(v), "encode").hex()),
            ("content hash", c["contentHash"], ok(content_hash(ALGORITHM, TAG, v), "hash").hex()),
            ("render", c["render"], ok(json_render(v, 2**64 - 1), "render")),
            (
                "decode then encode",
                c["cbor"],
                ok(cbor_encode(ok(cbor_decode(bytes.fromhex(c["cbor"])), "decode")), "e").hex(),
            ),
        ]
        bad = [f"{c['json']}: {what} expected {want}, got {got}" for what, want, got in problems if want != got]
        return "; ".join(bad) or None

    assert failures(V["json"], check) == 138


def test_model_cases() -> None:
    def check(c: Json) -> str | None:
        v = ok(cbor_decode(bytes.fromhex(c["cbor"])), c["value"])
        same = ok(cbor_encode(v), "encode").hex() == c["cbor"]
        hashed = ok(content_hash(ALGORITHM, TAG, v), "hash").hex() == c["contentHash"]
        return None if same and hashed else f"{c['value']}: cbor {same}, content hash {hashed}"

    failures(V["model"], check)


def test_render_rejections() -> None:
    def check(c: Json) -> str | None:
        r = json_render(ok(cbor_decode(bytes.fromhex(c["cbor"])), c["cbor"][:40]), 2**64 - 1)
        return refused(c["cbor"][:40], c["error"], None, r.error if isinstance(r, Err) else None)

    failures(V["renderRejections"], check)


def test_limits() -> None:
    """Encoding/rendering and CBOR length measurement match each row's exact outcome and output."""
    assert sum(c["encoding"] == "cbor" for c in V["limits"]) == 9

    def check(c: Json) -> str | None:
        v = ok(cbor_decode(bytes.fromhex(c["value"])), c["name"])
        if c["encoding"] == "cbor":
            r: Ok[object] | Err[object] = cbor_encode_within(v, c["maxBytes"])
            output = r.value.hex() if isinstance(r, Ok) and isinstance(r.value, bytes) else None
            measured = cbor_encoded_length_within(v, c["maxBytes"])
            expected = Ok(len(r.value)) if isinstance(r, Ok) and isinstance(r.value, bytes) else r
            if measured != expected:
                return f"{c['name']}: measured length expected {expected}, got {measured}"
        else:
            r = json_render(v, c["maxBytes"])
            output = r.value if isinstance(r, Ok) and isinstance(r.value, str) else None
        outcome: Json = "accepted"
        if isinstance(r, Err):
            e = r.error
            outcome = {"kind": "TooLarge", "limit": e.limit} if isinstance(e, TooLarge) else {"kind": kind_of(e)}
        if outcome == c["outcome"] and output == c.get("output"):
            return None
        return f"{c['name']}: expected {c['outcome']} {c.get('output')}, got {outcome} {output}"

    assert failures(V["limits"], check) == 19


def test_cbor_rejections() -> None:
    def check(c: Json) -> str | None:
        r = cbor_decode(bytes.fromhex(c["cbor"]))
        return refused(c["cbor"], c["error"], c.get("limit"), r.error if isinstance(r, Err) else None)

    failures(V["cborRejections"], check)


def test_every_canonical_error_kind_has_a_row() -> None:
    vt = load("versioned_trie_v1.json")
    pinned = {c["error"] for c in V["json"] if "error" in c}
    pinned |= {c["error"] for c in V["renderRejections"]} | {c["error"] for c in V["cborRejections"]}
    pinned |= {"TypeMismatch" for c in vt["stateKeys"] if c["schema"] == "TypeMismatch"}
    assert set(get_args(canonical.CanonicalErrorKind)) <= pinned


def test_tagged_hashes_and_digests() -> None:
    for c in V["taggedHash"]:
        assert ok(tagged_hash(c["algorithm"], c["tag"], bytes.fromhex(c["message"])), "tagged").hex() == c["digest"], c
    for c in V["digests"]:
        assert ok(digest(c["algorithm"], bytes.fromhex(c["message"])), "digest").hex() == c["digest"], c


def hash_outcome(r: Ok[object] | Err[object]) -> tuple[str, int | None, str | None]:
    """A hash call's outcome as a row records it: "accepted", or the kind with its `offset` or `refusal`."""
    match r:
        case Ok(_):
            return ("accepted", None, None)
        case Err(hash_errors.InvalidHex(offset=o)):
            return ("InvalidHex", o, None)
        case Err(hash_errors.InvalidExpansion(refusal=x)):
            return ("InvalidExpansion", None, x)
        case Err(e):
            return (kind_of(e), None, None)


def test_domain_tags_algorithms_digest_hex_and_expansions() -> None:
    for c in V["domainTags"]:
        assert hash_outcome(domain_tag(c["tag"]))[0] == c["outcome"], repr(c["tag"])
    for c in V["algorithms"]:
        named = hash_algorithm(c["name"])
        assert hash_outcome(named)[0] == c["outcome"], repr(c["name"])
        if isinstance(named, Ok):
            assert named.value == c["name"]
    for c in V["digestHex"]:
        d = digest_from_hex(c["algorithm"], c["hex"])
        assert hash_outcome(d)[:2] == (c["outcome"], c.get("offset")), f"{c['algorithm']} {c['hex']!r}"
        if isinstance(d, Ok):
            assert d.value.hex() == c["hex"].removeprefix("0x").removeprefix("0X").lower()
    for c in V["expansions"]:
        x = expand_message_xmd(c["algorithm"], [bytes.fromhex(c["message"])], bytes.fromhex(c["dst"]), c["length"])
        assert hash_outcome(x) == (c["outcome"], None, c.get("refusal")), f"{c['algorithm']} {c['length']}"
        assert (x.value.hex() if isinstance(x, Ok) else None) == c.get("output")
    assert {c["name"] for c in V["algorithms"] if c["outcome"] == "accepted"} == set(HASH_ALGORITHMS)


def test_every_hash_error_kind_has_a_row() -> None:
    pinned = {c["outcome"] for section in ("domainTags", "algorithms", "digestHex", "expansions") for c in V[section]}
    assert set(get_args(hash_errors.HashErrorKind)) <= pinned
    assert set(get_args(hash_errors.ExpansionRefusal)) <= {c.get("refusal") for c in V["expansions"]}


def test_a_preview_keeps_the_first_32_code_points_and_the_utf8_length() -> None:
    rows = load("diagnostics_v1.json")["previews"]
    assert len(rows) == 11
    for c in rows:
        e = err(json_parse(c["json"]), c["json"])
        assert isinstance(e, canonical.DuplicateKey), c["json"]
        assert (e.key.head, e.key.length, e.key.truncated) == (c["head"], c["length"], c["truncated"]), c["json"]
        assert ok(preview(e.key.head), "preview").head == c["head"]


def test_a_repeated_1_mib_member_name_is_a_preview() -> None:
    key = "k" * (1 << 20)
    e = err(json_parse(f'{{"{key}":1,"{key}":2}}'), "duplicate")
    assert isinstance(e, canonical.DuplicateKey)
    assert (len(e.key.head), e.key.length, e.key.truncated) == (32, 1 << 20, True)
    assert len(repr(e)) < 512, "a refusal renders boundedly"
