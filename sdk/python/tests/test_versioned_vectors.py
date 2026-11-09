"""`versioned_trie_v1.json` and `refusals_v1.json` through the Python API."""

from typing import Final, get_args

from dilf4s import Err, Ok, Value
from dilf4s.core import Data, cbor_decode, cbor_encode
from dilf4s.errors import merkle as merkle_errors
from dilf4s.errors import poseidon_merkle as pmt_errors
from dilf4s.errors import state_key as state_key_errors
from dilf4s.errors import trie as trie_errors
from dilf4s.errors import versioned_trie as vt_errors
from dilf4s.proofs import (
    MAX_PROOF_WEIGHT,
    VERSIONED_MAX_VALUE_BYTES,
    VERSIONED_MAX_VALUE_DEPTH,
    ConsistencyProof,
    HeadChunk,
    HistoryChunk,
    InclusionProof,
    KeyMode,
    MerkleHashing,
    NotCovered,
    PoseidonMerkleProof,
    Replica,
    StateChunk,
    TrieHashing,
    VersionedHead,
    VersionedTrie,
    VersionKey,
    merkle_check_consistency,
    merkle_check_inclusion,
    merkle_verify,
    state_key_from_bytes,
    state_key_parse,
    state_namespace_parse,
    trie_check,
    trie_prefix_verify,
    trie_verify,
)
from tests.support import Json, built, err, kind_of, load, ok

VECTORS: Final = load("versioned_trie_v1.json")
VT: Final = ok(VersionedTrie.of(ok(VersionKey.unsigned_long(VECTORS["width"]), "key"), "SHA-256"), "profile")
ROOT: Final = bytes.fromhex(VECTORS["root"])


def value_of(hex_cbor: str) -> Value:
    return ok(cbor_decode(bytes.fromhex(hex_cbor)), hex_cbor[:40])


def head_of(h: Json) -> VersionedHead:
    return ok(VT.decode_head(value_of(h["cbor"])), "head")


FINAL: Final = head_of(VECTORS["head"])


def key_outcome(r: Ok[str] | Err[object]) -> Json:
    if isinstance(r, Ok):
        return "accepted"
    e = r.error
    if isinstance(e, state_key_errors.InvalidSegment | state_key_errors.SegmentTooLong):
        return {"kind": e.kind, "segment": e.segment}
    return {"kind": kind_of(e)}


def test_the_empty_state_root_and_the_head() -> None:
    assert VT.empty_state_root.hex() == VECTORS["emptyStateRoot"]
    h = VECTORS["head"]
    assert FINAL.version == h["version"]
    assert ok(VT.vkey(h["version"]), "vkey").hex() == h["vkey"]
    assert (FINAL.state_root.hex(), FINAL.history_root.hex()) == (h["stateRoot"], h["historyRoot"])
    assert ok(cbor_encode(ok(VT.encode_head(FINAL), "encode")), "cbor").hex() == h["cbor"]
    assert ok(VT.root(FINAL), "root").hex() == h["root"] == VECTORS["root"]


def test_every_commits_delta_replays_through_a_replica_to_its_roots() -> None:
    replica = ok(Replica.empty(VT), "empty")
    for c in VECTORS["commits"]:
        delta = ok(VT.decode_delta(value_of(c["delta"])), f"delta {c['version']}")
        assert [(k, ok(cbor_encode(v), "v").hex()) for k, v in delta.upserts] == [
            (u["key"], u["value"]) for u in c["upserts"]
        ]
        assert list(delta.removes) == c["removes"]
        before = replica
        replica = ok(replica.apply_delta(value_of(c["delta"])), f"applies {c['version']}")
        assert replica.head is not None
        assert replica.head.version == c["version"]
        assert (replica.head.state_root.hex(), replica.head.history_root.hex()) == (c["stateRoot"], c["historyRoot"])
        assert ok(replica.root(), "root") == bytes.fromhex(c["root"])
        assert before.head is None or before.head.version < c["version"], "the replica it applied to is unchanged"
    assert ok(replica.root(), "root") == ROOT


def test_the_snapshot_restores_chunk_by_chunk_to_the_root() -> None:
    chunks = [bytes.fromhex(c) for c in VECTORS["snapshot"]["chunks"]]
    for c in chunks:
        decoded = ok(VT.decode_chunk(c), "chunk")
        match decoded:
            case StateChunk(entries=es):
                assert len(es) <= VECTORS["snapshot"]["chunkEntries"]
            case HistoryChunk(versions=vs):
                assert len(vs) <= VECTORS["snapshot"]["chunkEntries"]
            case HeadChunk(head=h):
                assert h == FINAL
    replica = ok(Replica.restore(VT, FINAL, chunks), "restores")
    assert ok(replica.root(), "root") == ROOT
    assert ok(replica.get("oracle/a/b"), "get") == ok(Value.text("a"), "a")
    assert len(replica) == len(replica.entries())


def test_restore_refuses_at_the_first_chunk_that_fails_reading_none_after_it() -> None:
    """Each chunk is decoded and fed in turn: state chunks before the head are refused by their order, before a later
    chunk that does not decode is read (and a list of many references to one chunk costs it once)."""
    chunks = [bytes.fromhex(c) for c in VECTORS["snapshot"]["chunks"]]
    state = next(c for c in chunks if isinstance(ok(VT.decode_chunk(c), "chunk"), StateChunk))
    refusal = err(Replica.restore(VT, FINAL, [state] * 3 + [b"\xff"]), "out of order")
    assert kind_of(refusal) == "Malformed"
    assert getattr(refusal, "reason", None) == "snapshot: it starts with its head"


def test_state_keys_namespaces_and_their_bytes() -> None:
    for c in VECTORS["stateKeys"]:
        assert key_outcome(state_key_parse(c["key"])) == c["outcome"], repr(c["key"])[:40]
    for c in VECTORS["stateKeyBytes"]:
        assert key_outcome(state_key_from_bytes(bytes.fromhex(c["bytes"]))) == c["outcome"], c["bytes"][:40]
    for c in VECTORS["namespaces"]:
        assert key_outcome(state_namespace_parse(c["namespace"])) == c["outcome"], c["namespace"][:40]
    outcomes = [c["outcome"] for c in VECTORS["stateKeys"] + VECTORS["stateKeyBytes"] + VECTORS["namespaces"]]
    pinned = {o["kind"] for o in outcomes if o != "accepted"}
    assert set(get_args(state_key_errors.StateKeyErrorKind)) <= pinned, "every state key error kind has a row"


def test_a_surrogate_counts_three_utf8_bytes_as_in_typescript() -> None:
    assert err(state_key_parse("\ud800" * 86), "258 bytes") == state_key_errors.TooLong(limit=256)
    assert kind_of(err(state_key_parse("\ud800" * 85), "255 bytes")) == "InvalidSegment"


def test_version_keys() -> None:
    for c in VECTORS["versionKeys"]:
        got = VT.vkey(c["version"])
        if c["outcome"] == "accepted":
            assert got == Ok(bytes.fromhex(c["key"])), c["version"]
        else:
            assert kind_of(err(got, str(c["version"]))) == c["outcome"]["kind"], c["version"]


def version_error(c: Json) -> Json:
    vt = ok(VersionedTrie.of(ok(VersionKey.bytes(c["width"]), "key")), "profile")
    if c["check"] == "replica":
        replica = ok(Replica.empty(vt), "empty")
        for d in c["deltas"]:
            match replica.apply_delta(value_of(d)):
                case Ok(next_replica):
                    replica = next_replica
                case Err(e):
                    return e
        return None
    state = vt.verify_historical_key(bytes.fromhex(c["root"]), value_of(c["proof"]), bytes.fromhex(c["at"]))
    return state.error if isinstance(state, Err) else None


def test_version_errors_name_versions_by_their_full_keys() -> None:
    for c in VECTORS["versionErrors"]:
        e = version_error(c)
        o = c["outcome"]
        match e:
            case vt_errors.VersionNotIncreasing(head=h, next=n):
                assert {"kind": e.kind, "head": h, "next": n} == o, c["name"]
            case vt_errors.UnknownVersion(version=v):
                assert {"kind": e.kind, "version": v} == o, c["name"]
            case _:
                msg = f"{c['name']}: {e!r}"
                raise AssertionError(msg)


def test_every_proof_answers_its_claims() -> None:
    assert {p["kind"] for p in VECTORS["proofs"]} == {"state", "historical", "version", "asOf", "head", "versions"}
    for p in VECTORS["proofs"]:
        proof = value_of(p["cbor"])
        assert ok(VT.decode_proof(proof), p["name"]) == FINAL, f"{p['name']}: the head binds to the root"
        at = p.get("at")
        match p["kind"]:
            case "state" | "historical":
                state = ok(
                    VT.verify_state(ROOT, proof) if p["kind"] == "state" else VT.verify_historical_key(ROOT, proof, at),
                    p["name"],
                )
                for c in p.get("claims", []):
                    claimed = None if "value" not in c else value_of(c["value"])
                    assert state.holds(c["key"], claimed) == Ok(True), f"{p['name']}: {c['key']}"
                    looked = ok(state.lookup(c["key"]), "lookup")
                    assert looked == (None if claimed is None else ok(VT.value_hash(claimed), "hash"))
                    if claimed is not None:
                        assert state.holds(c["key"], ok(Value.int(424242), "other")) == Ok(False), "another value"
                for listing in p.get("listings", []):
                    got = [(k, h.hex()) for k, h in ok(state.namespace(listing["namespace"]), "namespace")]
                    want = [
                        (e["key"], ok(VT.value_hash(value_of(e["value"])), "hash").hex()) for e in listing["entries"]
                    ]
                    assert got == want, f"{p['name']}: {listing['namespace']}"
            case "version":
                state_root = ok(VT.verify_version(ROOT, proof, at), p["name"])
                assert (None if state_root is None else state_root.hex()) == p.get("answer", {}).get("stateRoot")
                assert kind_of(err(VT.verify_version(ROOT, proof, at + 1), "another version")) == "Malformed"
            case "asOf":
                found = ok(VT.verify_as_of(ROOT, proof, at), p["name"])
                assert ({} if found is None else {"version": found[0], "stateRoot": found[1].hex()}) == p["answer"]
            case "head":
                assert ok(VT.verify_head(ROOT, proof), p["name"]) == FINAL
            case "versions":
                versions = [(v["version"], bytes.fromhex(v["stateRoot"])) for v in p.get("versions", [])]
                assert VT.verify_versions(ROOT, proof, at, p.get("until"), versions) == Ok(True), p["name"]
                assert VT.verify_versions(ROOT, proof, at, p.get("until"), versions[1:]) == Ok(False), "one left out"
        e = err(VT.verify_head(VT.empty_state_root, proof), "another root")
        assert isinstance(e, vt_errors.RootMismatch), f"{p['name']}: against another root, the binding fails"
        assert e.component == "root"


# ------------------------------------------------------------------------------------------------ refusals_v1.json

REFUSALS: Final = load("refusals_v1.json")
TAGGED: Final = ok(MerkleHashing.tagged(), "tagged")
T: Final = ok(TrieHashing.of(), "trie")


def mode_of(m: Json | None) -> KeyMode:
    m = m or {"fixed": 0}
    return ok(KeyMode.fixed(m["fixed"]) if "fixed" in m else KeyMode.variable(m["variable"]), "mode")


def outcome_of(e: object) -> Json:
    """A refusal as the vectors record it: its family and kind, and its normative fields."""
    match e:
        case vt_errors.TooLarge(limit=limit):
            return {"versionedTrie": "TooLarge", "limit": limit}
        case vt_errors.TooDeep(depth=depth, limit=limit):
            return {"versionedTrie": "TooDeep", "limit": limit, "depth": depth}
        case vt_errors.Trie(error=t):
            return {"versionedTrie": "Trie", "cause": {"trie": t.kind}}
        case vt_errors.Key(error=k):
            segment = getattr(k, "segment", None)
            return {
                "versionedTrie": "Key",
                "cause": {"stateKey": k.kind} | ({} if segment is None else {"segment": segment}),
            }
        case vt_errors.RootMismatch(component=c):
            return {"versionedTrie": "RootMismatch", "component": c}
        case (
            vt_errors.Empty()
            | vt_errors.InvalidVersion()
            | vt_errors.VersionNotIncreasing()
            | vt_errors.VersionConflict()
        ):
            return {"versionedTrie": kind_of(e)}
        case vt_errors.CommitRewrite() | vt_errors.NotRetained() | vt_errors.UnknownVersion() | vt_errors.NotCovered():
            return {"versionedTrie": kind_of(e)}
        case vt_errors.Malformed() | vt_errors.Store():
            return {"versionedTrie": kind_of(e)}
        case merkle_errors.TooLarge(limit=limit):
            return {"merkle": "TooLarge", "limit": limit}
        case merkle_errors.IndexOutOfRange() | merkle_errors.InvalidSizes() | merkle_errors.InvalidProof():
            return {"merkle": kind_of(e)}
        case merkle_errors.RootMismatch() | merkle_errors.Encoding():
            return {"merkle": kind_of(e)}
        case trie_errors.TooLarge(limit=limit):
            return {"trie": "TooLarge", "limit": limit}
        case (
            trie_errors.InvalidKey()
            | trie_errors.MissingNode()
            | trie_errors.InvalidNode()
            | trie_errors.InvalidProof()
        ):
            return {"trie": kind_of(e)}
        case trie_errors.RootMismatch() | trie_errors.Encoding() | trie_errors.NotCovered():
            return {"trie": kind_of(e)}
        case pmt_errors.Position(depth=depth):
            return {"poseidonMerkle": "Position", "depth": depth}
        case pmt_errors.Depth() | pmt_errors.OccupiedPosition() | pmt_errors.Encoding():
            return {"poseidonMerkle": kind_of(e)}
    return {"unexpected": repr(e)}


def refused(r: Ok[object] | Err[object]) -> Json:
    return "accepted" if isinstance(r, Ok) else outcome_of(r.error)


def commit(c: Json) -> Json:
    """`vt-commit`: each value's depth and weight alone, then the delta's weight, then each value's measured length."""
    upserts = [(u["stateKey"], built(u["value"])) for u in c.get("batch", [])]
    for k, v in upserts:
        if v.depth > VERSIONED_MAX_VALUE_DEPTH or v.weight > VERSIONED_MAX_VALUE_BYTES:
            r = VT.check_value(k, v)
            if isinstance(r, Err):
                return outcome_of(r.error)
    head: dict[str, Data] = {"v": bytes.fromhex("0000000000000001"), "s": bytes(32), "h": bytes(32)}
    writes: list[Data] = [{"k": k, "v": v} for k, v in upserts]
    delta = ok(Value.of({"head": head, "upserts": writes, "removes": []}), "delta")
    if delta.weight > MAX_PROOF_WEIGHT:
        return {"versionedTrie": "TooLarge", "limit": MAX_PROOF_WEIGHT}
    for k, v in upserts:
        r = VT.check_value(k, v)
        if isinstance(r, Err):
            return outcome_of(r.error)
    return "accepted"


def run(c: Json) -> Json:
    """A case's outcome, by its check (conformance.md section 6)."""
    match c["check"]:
        case "vt-value":
            return refused(VT.check_value(c["stateKey"], built(c["input"])))
        case "vt-chunk":
            return refused(VT.decode_chunk(bytes.fromhex(c["chunk"]) if "chunk" in c else bytes(c["zeros"])))
        case "vt-delta":
            return refused(VT.decode_delta(built(c["input"])))
        case "vt-proof":
            return refused(VT.decode_proof(built(c["input"])))
        case "vt-state" | "vt-historical":
            proof, root = built(c["input"]), bytes.fromhex(c["root"])
            decoded = VT.decode_proof(proof)
            if isinstance(decoded, Err):
                return outcome_of(decoded.error)
            state = (
                VT.verify_state(root, proof)
                if c["check"] == "vt-state"
                else VT.verify_historical_key(root, proof, c["at"])
            )
            return refused(state.value.lookup(c["stateKey"]) if isinstance(state, Ok) else state)
        case "vt-replica":
            replica = ok(Replica.empty(VT), "empty")
            for d in c.get("inputs", []):
                match replica.apply_delta(built(d)):
                    case Err(e):
                        return outcome_of(e)
                    case Ok(next_replica):
                        replica = next_replica
            return "accepted"
        case "merkle-inclusion":
            p = InclusionProof.decode(TAGGED, built(c["input"]))
            if isinstance(p, Err):
                return outcome_of(p.error)
            return refused(
                merkle_check_inclusion(TAGGED, bytes.fromhex(c["root"]), bytes.fromhex(c["leafHash"]), p.value)
            )
        case "merkle-consistency":
            q = ConsistencyProof.decode(TAGGED, built(c["input"]))
            if isinstance(q, Err):
                return outcome_of(q.error)
            return refused(
                merkle_check_consistency(TAGGED, bytes.fromhex(c["oldRoot"]), bytes.fromhex(c["root"]), q.value)
            )
        case "trie-lookup":
            checked = trie_check(T, mode_of(c.get("mode")), bytes.fromhex(c["root"]), built(c["input"]))
            if isinstance(checked, Err):
                return outcome_of(checked.error)
            found = checked.value.lookup(bytes.fromhex(c["key"]))
            if isinstance(found, Err):
                return outcome_of(found.error)
            return {"trie": "NotCovered"} if isinstance(found.value, NotCovered) else "accepted"
        case "poseidon-proof":
            return refused(PoseidonMerkleProof.decode(built(c["input"])))
        case "vt-commit":
            return commit(c)
        case "merkle-verify":
            claim = built(c.get("claim", {"leaf": ""}))
            return refused(merkle_verify(TAGGED, bytes.fromhex(c["root"]), c.get("size", 0), claim, built(c["input"])))
        case "trie-verify":
            claimed = built(c["claim"]) if "claim" in c else None
            return refused(
                trie_verify(
                    T,
                    mode_of(c.get("mode")),
                    bytes.fromhex(c["root"]),
                    bytes.fromhex(c["key"]),
                    claimed,
                    built(c["input"]),
                )
            )
        case "trie-prefix-verify":
            entries = [(bytes.fromhex(e["key"]), built(e["value"])) for e in c.get("entries", [])]
            return refused(
                trie_prefix_verify(
                    T,
                    mode_of(c.get("mode")),
                    bytes.fromhex(c["root"]),
                    bytes.fromhex(c["prefix"]),
                    entries,
                    built(c["input"]),
                )
            )
    msg = f"unknown check {c['check']}"
    raise AssertionError(msg)


def normalised(o: Json) -> Json:
    """A vector outcome without its absent fields."""
    return {k: normalised(v) for k, v in o.items() if v is not None} if isinstance(o, dict) else o


def test_every_refusal_case_gives_its_kind_and_normative_fields() -> None:
    assert len({c["check"] for c in REFUSALS["cases"]}) == 15, "a case for every check"
    failed = [
        f"{c['name']}: expected {c['outcome']}, got {run(c)}"
        for c in REFUSALS["cases"]
        if run(c) != normalised(c["outcome"])
    ]
    assert not failed, "\n".join(failed)


def test_every_kind_of_the_proof_families_has_a_row_or_is_exempt() -> None:
    outcomes = [c["outcome"] for c in REFUSALS["cases"] if c["outcome"] != "accepted"]
    vts = {o["versionedTrie"] for o in outcomes if "versionedTrie" in o}
    vts |= {r["outcome"]["kind"] for r in VECTORS["versionErrors"]}
    merkles = {o["merkle"] for o in outcomes if "merkle" in o} | {
        r["error"] for r in load("merkle_v1.json")["malformed"]
    }
    tries = {o["trie"] for o in outcomes if "trie" in o} | {r["error"] for r in load("trie_v1.json")["malformed"]}
    tries |= {o["cause"]["trie"] for o in outcomes if "trie" in o.get("cause", {})}
    poseidons = {o["poseidonMerkle"] for o in outcomes if "poseidonMerkle" in o}
    exempt_vt = {"Empty", "VersionConflict", "CommitRewrite", "NotRetained", "Store"}
    for k in get_args(vt_errors.VersionedTrieErrorKind):
        assert (k in vts) != (k in exempt_vt), f"versioned trie {k}: a row exactly when not exempt"
    assert set(get_args(merkle_errors.MerkleErrorKind)) <= merkles, "every Merkle kind has a row"
    for k in get_args(trie_errors.TrieErrorKind):
        assert (k in tries) != (k in {"MissingNode", "InvalidNode"}), f"trie {k}: a row exactly when not exempt"
    for k in get_args(pmt_errors.PoseidonMerkleErrorKind):
        assert (k in poseidons) != (k in {"Depth", "OccupiedPosition"}), f"Poseidon {k}: a row exactly when not exempt"
