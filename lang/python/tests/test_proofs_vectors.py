"""`merkle_v1.json`, `trie_v1.json`, `poseidon_v1.json`, `zk_v1.json` and `zk_jlvm_v1.json` through the Python API."""

from typing import Final, get_args

from dilf4s import Err, Ok, Result, Value
from dilf4s.core import Data, TypeMismatch, cbor_decode, cbor_encode, tagged_hash, value_commitment
from dilf4s.errors import groth16 as groth16_errors
from dilf4s.errors import merkle as merkle_errors
from dilf4s.errors import poseidon as poseidon_errors
from dilf4s.errors import zk as zk_errors
from dilf4s.jlvm import Outcome, evaluate, parse, parse_value
from dilf4s.proofs import (
    FR_MODULUS,
    Absent,
    ConsistencyProof,
    Groth16VerifyingKey,
    InclusionProof,
    KeyMode,
    MerkleHashing,
    NotCovered,
    PoseidonMerkleProof,
    Present,
    TrieHashing,
    VerifiedTrieProof,
    bn254_add,
    bn254_mul,
    bn254_pairing,
    merkle_check_inclusion,
    merkle_consistency_verify,
    merkle_head,
    merkle_root,
    merkle_verify,
    merkle_verify_consistency,
    merkle_verify_inclusion,
    poseidon_compress,
    poseidon_hash,
    poseidon_merkle_root,
    poseidon_verify_absence,
    poseidon_verify_inclusion,
    poseidon_zero_hashes,
    sp1_hash_public_values,
    sp1_verify,
    trie_check,
    trie_range_verify,
    trie_root,
    trie_verify_proof,
)
from tests.support import Json, err, failures, kind_of, load, ok

MERKLE: Final = load("merkle_v1.json")
TAGGED: Final = ok(MerkleHashing.tagged("SHA-256"), "tagged")
RFC: Final = ok(MerkleHashing.rfc6962(), "rfc6962")


def value_of(hex_cbor: str) -> Value:
    return ok(cbor_decode(bytes.fromhex(hex_cbor)), hex_cbor[:40])


# ------------------------------------------------------------------------------------------------ Merkle


def test_merkle_trees_under_both_profiles() -> None:
    leaves = [bytes.fromhex(x) for x in MERKLE["leaves"]]
    leaf_hashes = [bytes.fromhex(x) for x in MERKLE["leafHashes"]]
    rfc_hashes = [ok(RFC.leaf_hash(x), "rfc leaf") for x in leaves]
    for leaf, h in zip(leaves, leaf_hashes, strict=True):
        assert ok(tagged_hash("SHA-256", "dilf4s/merkle/leaf/v1", leaf), "leaf") == h, "a raw leaf's tagged hash"
    assert ok(merkle_root(TAGGED, []), "empty").hex() == MERKLE["trees"][0]["root"], "the empty tagged root"
    assert RFC.empty.hex() == MERKLE["trees"][0]["rfc6962Root"], "the empty RFC 6962 root"
    for t in MERKLE["trees"]:
        n = t["size"]
        assert ok(merkle_root(TAGGED, leaf_hashes[:n]), "root").hex() == t["root"], f"tagged root of {n}"
        assert ok(merkle_root(RFC, rfc_hashes[:n]), "root").hex() == t["rfc6962Root"], f"rfc6962 root of {n}"
        for profile, hashes, inclusion, consistency, root_hex in (
            (TAGGED, leaf_hashes, t["inclusion"], t["consistency"], t["root"]),
            (RFC, rfc_hashes, t["rfc6962Inclusion"], t["rfc6962Consistency"], t["rfc6962Root"]),
        ):
            root = bytes.fromhex(root_hex)
            assert len(inclusion) == n
            for i, proof_hex in enumerate(inclusion):
                p = ok(InclusionProof.decode(profile, value_of(proof_hex)), f"inclusion {i} of {n}")
                assert (p.index, p.size) == (i, n)
                assert ok(cbor_encode(ok(p.encode(), "encode")), "cbor").hex() == proof_hex, "re-encodes"
                assert merkle_verify_inclusion(profile, root, hashes[i], p) == Ok(True), f"{profile} {i} of {n}"
                other = hashes[(i + 1) % 8]
                assert merkle_verify_inclusion(profile, root, other, p) == Ok(False), "another leaf"
                assert merkle_check_inclusion(profile, root, hashes[i], p) == Ok(None), "checks"
            assert len(consistency) == n + 1
            for m, proof_hex in enumerate(consistency):
                q = ok(ConsistencyProof.decode(profile, value_of(proof_hex)), f"consistency {m} -> {n}")
                assert (q.old_size, q.new_size) == (m, n)
                assert ok(cbor_encode(ok(q.encode(), "encode")), "cbor").hex() == proof_hex, "re-encodes"
                old = ok(merkle_root(profile, hashes[:m]), "old root")
                assert merkle_verify_consistency(profile, old, root, q) == Ok(True), f"{profile} {m} -> {n}"
                jlvm = merkle_consistency_verify(profile, old, m, root, n, value_of(proof_hex))
                assert jlvm == Ok(True), "the JLVM's form"
                if 0 < m < n:
                    wrong = ok(merkle_root(profile, hashes[: m - 1]), "wrong root")
                    assert merkle_verify_consistency(profile, wrong, root, q) == Ok(False), "a wrong old root"


def test_value_commitments_hash_as_the_leaf_of_a_value() -> None:
    for c in MERKLE["valueCommitments"]:
        v = value_of(c["cbor"])
        assert ok(value_commitment(v), "commit").hex() == c["commitment"], "commit(v)"
        assert ok(TAGGED.leaf_hash(v), "leaf").hex() == c["leafHash"], "leaf hash"


def test_merkle_verify_through_a_value_leaf() -> None:
    tree = next(t for t in MERKLE["trees"] if t["size"] == 5)
    text = ok(Value.text("a"), "text")
    hashes = [ok(TAGGED.leaf_hash(text), "leaf"), *(bytes.fromhex(h) for h in MERKLE["leafHashes"][1:5])]
    root = ok(merkle_root(TAGGED, hashes), "root")
    proof = value_of(tree["inclusion"][0])
    assert merkle_verify(TAGGED, root, 5, text, proof) == Ok(True), "a value leaf verifies through its commitment"
    as_bytes = ok(Value.bytes(b"\x61\x61"), "bytes")
    assert merkle_verify(TAGGED, root, 5, as_bytes, proof) == Ok(False), "bytes with the text's CBOR do not"
    assert merkle_verify(TAGGED, root, 6, text, proof) == Ok(False), "a head of another size"
    head = ok(Value.of({"root": root, "size": 5}), "head")
    assert merkle_head(TAGGED, head) == Ok((root, 5)), "a head from its JLVM form"
    assert isinstance(err(merkle_verify(TAGGED, root[:31], 5, text, proof), "short root"), merkle_errors.Encoding)


def test_merkle_malformed_rows() -> None:
    assert len(MERKLE["malformed"]) == 12
    for c in MERKLE["malformed"]:
        form = value_of(c["cbor"])
        got = InclusionProof.decode(TAGGED, form) if c["kind"] == "inclusion" else ConsistencyProof.decode(TAGGED, form)
        assert kind_of(err(got, c["name"])) == c["error"], c["name"]


def test_merkle_constructors_take_sizes_up_to_2_63_minus_1() -> None:
    top = 2**63 - 1
    assert isinstance(InclusionProof.of(TAGGED, top - 1, top, []), Ok)
    assert kind_of(err(InclusionProof.of(TAGGED, top, 2**63, []), "past")) == "IndexOutOfRange"
    assert isinstance(ConsistencyProof.of(TAGGED, top, top, []), Ok)
    assert kind_of(err(ConsistencyProof.of(TAGGED, 0, 2**63, []), "past")) == "InvalidSizes"
    assert kind_of(err(InclusionProof.of(TAGGED, 2**64, 1, []), "past u64")) == "TypeMismatch"


# ------------------------------------------------------------------------------------------------ tries

TRIE: Final = load("trie_v1.json")
T: Final = ok(TrieHashing.of("SHA-256"), "trie hashing")


def mode_of(m: Json) -> KeyMode:
    return ok(KeyMode.fixed(m["fixed"]) if "fixed" in m else KeyMode.variable(m["variable"]), "mode")


def lookup_hex(found: object) -> Json:
    match found:
        case Present(value_hash=h):
            return {"present": h.hex()}
        case Absent():
            return "absent"
        case NotCovered():
            return "notCovered"
    msg = f"not a lookup: {found!r}"
    raise AssertionError(msg)


def listing_hex(listed: object) -> Json:
    if isinstance(listed, NotCovered):
        return "notCovered"
    assert isinstance(listed, tuple)
    return [{"key": k.hex(), "valueHash": v.hex()} for k, v in listed]


def test_trie_empty_root_and_node_hashes() -> None:
    assert T.empty.hex() == TRIE["emptyRoot"]
    leaf, branch = TRIE["nodeHashes"]["leaf"], TRIE["nodeHashes"]["branch"]
    s = leaf["segment"]
    assert (
        ok(T.leaf_hash(s["bits"], bytes.fromhex(s["bytes"]), bytes.fromhex(leaf["valueHash"])), "leaf").hex()
        == leaf["hash"]
    )
    s = branch["segment"]
    got = T.branch_hash(
        s["bits"], bytes.fromhex(s["bytes"]), bytes.fromhex(branch["valueHash"]), bytes.fromhex(branch["left"])
    )
    assert ok(got, "branch").hex() == branch["hash"]


def test_trie_fixtures() -> None:
    for f in TRIE["fixtures"]:
        mode, root = mode_of(f["mode"]), bytes.fromhex(f["root"])
        entries = [(bytes.fromhex(e["key"]), bytes.fromhex(e["valueHash"])) for e in f["entries"]]
        assert ok(trie_root(T, mode, entries), "root").hex() == f["root"], f"{f['name']}: builds the root"
        assert ok(trie_root(T, mode, entries[::-1]), "root").hex() == f["root"], f"{f['name']}: in any order"
        proof = value_of(f["proof"]["cbor"])
        checked = ok(trie_check(T, mode, root, proof), f["name"])
        assert isinstance(checked, VerifiedTrieProof)
        assert ok(checked.encode(), "encode") == proof, f"{f['name']}: re-encodes to the same records"
        for row in f["proof"]["lookups"]:
            assert lookup_hex(ok(checked.lookup(bytes.fromhex(row["key"])), "lookup")) == row["result"], row["key"]
        for row in f["proof"]["prefixListings"]:
            assert listing_hex(ok(checked.prefix(bytes.fromhex(row["prefix"])), "prefix")) == row["entries"], row[
                "prefix"
            ]
        for row in f["proof"]["rangeListings"]:
            until = None if row.get("until") is None else bytes.fromhex(row["until"])
            got = listing_hex(ok(checked.range(bytes.fromhex(row["from"]), until), "range"))
            expected = row["result"] if row["result"] == "notCovered" else row["result"]["entries"]
            assert got == expected, f"[{row['from']}, {row.get('until')})"
            first = entries[0][0] if entries else b""
            verdict = trie_range_verify(
                T, mode, root, bytes.fromhex(row["from"]), until, [(first, ok(Value.int(0), "0"))], proof
            )
            if row["result"] == "notCovered":
                assert kind_of(err(verdict, "not covered")) == "NotCovered"
            else:
                assert verdict == Ok(False), "a covered range with a wrong claim"
        for row in f["proof"]["ranges"]:
            until = None if row.get("until") is None else bytes.fromhex(row["until"])
            assert not isinstance(ok(checked.range(bytes.fromhex(row["from"]), until), "range"), NotCovered)
        other = bytes([root[0] ^ 1]) + root[1:]
        assert trie_verify_proof(T, mode, other, proof) == Ok(None), "another root is no proof of it"
        assert kind_of(err(trie_check(T, mode, other, proof), "mismatch")) == "RootMismatch"


def test_trie_malformed_rows() -> None:
    def check(c: Json) -> str | None:
        got = trie_verify_proof(T, mode_of(c["mode"]), bytes.fromhex(c["root"]), value_of(c["cbor"]))
        return None if isinstance(got, Err) and kind_of(got.error) == c["error"] else f"{c['name']}: {got}"

    assert failures(TRIE["malformed"], check) == 15


# ------------------------------------------------------------------------------------------------ Poseidon

POSEIDON: Final = load("poseidon_v1.json")


def test_poseidon_hash_vectors() -> None:
    for c in POSEIDON["poseidon"]:
        assert poseidon_hash([int(x) for x in c["inputs"]]) == Ok(int(c["output"])), c["inputs"]
    assert (
        ok(poseidon_hash([FR_MODULUS - 1]), "r - 1")
        == 3366645945435192953002076803303112651887535928162668198103357554665518664470
    )
    assert err(poseidon_hash([]), "none") == poseidon_errors.InputCount(count=0, max=4)
    assert err(poseidon_hash([1] * 5), "five") == poseidon_errors.InputCount(count=5, max=4)
    assert poseidon_compress(1, 2) == poseidon_hash([1, 2])
    assert kind_of(err(poseidon_hash([FR_MODULUS]), "r")) == "NonCanonical"
    assert kind_of(err(poseidon_hash([-1]), "negative")) == "NonCanonical"
    assert kind_of(err(poseidon_hash([2**100_000]), "huge")) == "NonCanonical"


def sparse_root(depth: int, leaves: dict[int, int]) -> int:
    """zk.md section 4's root of a sparse tree, by its definition: zero subtrees by the zero hashes."""
    zeros = ok(poseidon_zero_hashes(depth), "zeros")
    level = dict(leaves)
    for d in range(depth):
        parents: dict[int, int] = {}
        for p in {k >> 1 for k in level}:
            left, right = level.get(2 * p, zeros[d]), level.get(2 * p + 1, zeros[d])
            parents[p] = ok(poseidon_compress(left, right), "compress")
        level = parents
    return level.get(0, zeros[depth])


def test_poseidon_build_vectors() -> None:
    assert len(POSEIDON["builds"]) == 5
    for b in POSEIDON["builds"]:
        leaves: dict[int, int] = {}
        for e in b["entries"]:
            if int(e["leaf"]) == 0:
                leaves.pop(int(e["position"]), None)
            else:
                leaves[int(e["position"])] = int(e["leaf"])
        assert sparse_root(b["depth"], leaves) == int(b["root"]), b["entries"]


def test_poseidon_merkle_vectors() -> None:
    m = POSEIDON["merkle"]
    depth, root = m["depth"], int(m["root"])
    leaves = {int(p): int(x) for p, x in m["leaves"].items()}
    assert sparse_root(depth, leaves) == root
    for p in m["proofs"]:
        position = int(p["position"])
        proof = ok(PoseidonMerkleProof.of(position, [int(s) for s in p["siblings"]]), "proof")
        assert ok(PoseidonMerkleProof.decode(ok(proof.encode(), "encode")), "decode") == proof, "round trip"
        if position in leaves:
            leaf = leaves[position]
            assert poseidon_verify_inclusion(leaf, proof, root) == Ok(True), f"position {position}"
            assert poseidon_verify_absence(proof, root) == Ok(False)
            assert poseidon_verify_inclusion(leaf + 1, proof, root) == Ok(False)
        else:
            assert poseidon_verify_absence(proof, root) == Ok(True), f"position {position} empty"
            assert poseidon_verify_inclusion(1, proof, root) == Ok(False)
        assert poseidon_merkle_root(0, proof) != poseidon_merkle_root(5, proof)
    zeros = ok(poseidon_zero_hashes(depth), "zeros")
    empty = ok(PoseidonMerkleProof.of(3, list(reversed(zeros[:depth]))), "empty proof")
    assert poseidon_verify_absence(empty, zeros[depth]) == Ok(True), "an empty tree's root is the top zero hash"


def test_poseidon_malformed_proofs() -> None:
    assert kind_of(err(PoseidonMerkleProof.of(4, [7, 7]), "position")) == "Position"
    assert isinstance(PoseidonMerkleProof.of(3, [7, 7]), Ok)
    assert kind_of(err(PoseidonMerkleProof.of(1, []), "position")) == "Position"

    def wire(pos: int, sib: list[int]) -> Value:
        return ok(Value.of({"position": pos, "siblings": list[Data](sib)}), "wire")

    assert kind_of(err(PoseidonMerkleProof.decode(wire(2, [1])), "position")) == "Position"
    assert kind_of(err(PoseidonMerkleProof.decode(wire(-1, [])), "negative")) == "Encoding"
    assert kind_of(err(PoseidonMerkleProof.decode(wire(0, [FR_MODULUS])), "r")) == "Encoding"
    assert kind_of(err(PoseidonMerkleProof.decode(wire(0, [0] * 257)), "deep")) == "Encoding"
    assert kind_of(err(PoseidonMerkleProof.of(0, [7] * 257), "deep")) == "Position"


# ------------------------------------------------------------------------------------------------ BN254, Groth16, SP1

ZK: Final = load("zk_v1.json")


def point_result(r: Ok[bytes] | Err[object]) -> Json:
    return {"point": r.value.hex()} if isinstance(r, Ok) else {"error": kind_of(r.error)}


def test_bn254_add_mul_and_pairing() -> None:
    assert len(ZK["add"]) == 20
    for c in ZK["add"]:
        assert point_result(bn254_add(bytes.fromhex(c["a"]), bytes.fromhex(c["b"]))) == c["result"], c["name"]
    assert len(ZK["mul"]) == 23
    for c in ZK["mul"]:
        assert point_result(bn254_mul(bytes.fromhex(c["point"]), bytes.fromhex(c["scalar"]))) == c["result"], c["name"]
    assert len(ZK["pairing"]) == 21
    for c in ZK["pairing"]:
        got = bn254_pairing([(bytes.fromhex(p["g1"]), bytes.fromhex(p["g2"])) for p in c["pairs"]])
        expected = c["result"]
        assert (
            (got == Ok(expected["value"])) if "value" in expected else kind_of(err(got, "pairing")) == expected["error"]
        )


def test_sp1_groth16() -> None:
    assert len(ZK["sp1Groth16"]) == 16
    vk = ok(Groth16VerifyingKey.sp1(), "the SP1 verifying key")
    assert vk.public_inputs == 5
    for c in ZK["sp1Groth16"]:
        got = sp1_verify(*(bytes.fromhex(c[f]) for f in ("programVKey", "publicValues", "proofBytes")))
        outcome = "verified" if got == Ok(None) else kind_of(err(got, c["name"]))
        assert outcome == c["result"], c["name"]
    assert ok(sp1_hash_public_values(b"any public values"), "hash")[0] & 0xE0 == 0, "below 2**253"


def test_every_zk_error_kind_has_a_row() -> None:
    results = [c["result"] for c in ZK["add"] + ZK["mul"] + ZK["pairing"]]
    pinned = {r["error"] for r in results if "error" in r}
    assert set(get_args(zk_errors.ZkErrorKind)) <= pinned
    sp1 = {c["result"] for c in ZK["sp1Groth16"]}
    assert set(get_args(groth16_errors.Groth16ErrorKind)) | {"verified"} <= sp1


def test_a_groth16_key_with_an_identity_point_is_refused() -> None:
    g1 = bytes.fromhex("00" * 31 + "01" + "00" * 31 + "02")
    g2 = bytes.fromhex(
        "198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa"
    )
    o1, o2 = bytes(64), bytes(128)
    assert isinstance(Groth16VerifyingKey.from_bytes(g1, g2, g2, g2, [g1, g1]), Ok), "finite points make a key"
    for args in ((o1, g2, g2, g2, [g1, g1]), (g1, o2, g2, g2, [g1, g1]), (g1, g2, g2, g2, [o1, g1])):
        assert kind_of(err(Groth16VerifyingKey.from_bytes(*args), "identity")) == "Malformed"


# ------------------------------------------------------------------------------------------------ zk_jlvm_v1.json

ZK_JLVM: Final = load("zk_jlvm_v1.json")


def commitment_digest(tag: str, v: Value) -> str:
    return ok(tagged_hash("SHA-256", tag, ok(value_commitment(v), "commit")), tag).hex()


def statement_of(call: Json) -> Json:
    """A call's statement from zk-jlvm.md's definitions alone, through the Python API."""
    program = ok(parse(call["program"]), "program")
    data = ok(parse_value(call["data"]), "data")
    env = {k: ok(parse_value(v), k) for k, v in call.get("env", {}).items()}
    outcome = ok(evaluate(program, data, env, call["gasLimit"]), "evaluate")
    parts = b""
    for name in sorted(env):
        named = ok(cbor_encode(ok(Value.text(name), name)), name) + ok(value_commitment(env[name]), name)
        parts += ok(tagged_hash("SHA-256", "dilf4s/zk/jlvm/binding/v1", named), name)
    result: Json
    match outcome.result:
        case Ok(v):
            result = {"result": commitment_digest("dilf4s/zk/jlvm/result/v1", v)}
        case Err(e):
            result = {"error": e.kind, "op": getattr(e, "op", None)}
    return {
        "language": 1,
        "schedule": 1,
        "program": ok(program.id(), "id").hex(),
        "input": commitment_digest("dilf4s/zk/jlvm/input/v1", data),
        "bindings": ok(tagged_hash("SHA-256", "dilf4s/zk/jlvm/bindings/v1", parts), "bindings").hex(),
        "gasLimit": call["gasLimit"],
        "gasUsed": outcome.gas_used,
        "outcome": result,
    }


def test_each_statement_is_its_call_and_the_public_values_its_encoding() -> None:
    assert len(ZK_JLVM["calls"]) == 8, "zk_jlvm_v1.json call coverage"
    for c in ZK_JLVM["calls"]:
        expected = dict(c["statement"])
        if "error" in expected["outcome"]:
            expected["outcome"] = {"error": expected["outcome"]["error"], "op": expected["outcome"].get("op")}
        assert statement_of(c) == expected, f"{c['name']}: the statement"
        public = bytes.fromhex(c["publicValues"])
        decoded = ok(cbor_decode(public), "public values")
        assert ok(cbor_encode(decoded), "encode") == public, f"{c['name']}: canonical, once"
        fields = ok(decoded.to_python(1 << 16), "fields")
        assert isinstance(fields, dict)
        program = fields["program"]
        assert isinstance(program, bytes)
        assert program.hex() == c["statement"]["program"], f"{c['name']}: the public values hold the statement"
        assert fields["gasUsed"] == c["statement"]["gasUsed"], f"{c['name']}: and its gas"


def test_each_pinned_proof_verifies_under_the_guests_key_and_only_for_its_public_values() -> None:
    proved = [c for c in ZK_JLVM["calls"] if "proofBytes" in c]
    assert [c["name"] for c in proved] == ["contract multisig_wallet: 2 of 3 owners sign"]
    vkey = bytes.fromhex(ZK_JLVM["programVKey"])
    for c in proved:
        public, proof = bytes.fromhex(c["publicValues"]), bytes.fromhex(c["proofBytes"])
        assert sp1_verify(vkey, public, proof) == Ok(None), f"{c['name']}: the SDK's verifier"

        def run(k: bytes, p: bytes, proof: bytes = proof) -> Result[Outcome, TypeMismatch]:
            text = f'{{"groth16_verify": ["0x{k.hex()}", "0x{p.hex()}", "0x{proof.hex()}"]}}'
            return evaluate(ok(parse(text), "program"), Value.null())

        true, false = ok(Value.bool(True), "true"), ok(Value.bool(False), "false")
        assert ok(run(vkey, public), "evaluate").result == Ok(true), c["name"]
        other = public[:-1] + bytes([public[-1] ^ 1])
        assert ok(run(vkey, other), "evaluate").result == Ok(false), "other public values"
        key = vkey[:31] + bytes([vkey[31] ^ 1])
        assert ok(run(key, public), "evaluate").result == Ok(false), "another key"
