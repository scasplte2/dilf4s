"""`shared/vectors/signing_v1.json` through the Python API: every scheme signs and verifies byte for byte, every verdict
and refusal, key decoding, BIP-39, SLIP-10, BIP-32, Icarus and path strictness."""

from typing import Final, get_args

from dilf4s import Err, Ok
from dilf4s.errors import crypto, key
from dilf4s.signing import (
    SCHEMES,
    SIGNERS,
    ExtendedKey,
    SigningKey,
    WordList,
    bip32_public_child,
    bls_pop_verify,
    decode_verifying_key,
    icarus_public_child,
    mnemonic_from_entropy,
    mnemonic_to_entropy,
    mnemonic_to_seed,
    parse_path,
    verify,
)
from tests.support import Json, err, failures, is_scheme, is_signer, kind_of, load, ok, untyped

VECTORS: Final = load("signing_v1.json")
RANDOMISED: Final = {"BIP-340", "secp256k1-tagged-Schnorr", "ristretto255-Schnorr"}


def check_key(scheme: str, k: Json, aux: bytes) -> str | None:
    """One key of one scheme: the derived key pair, then every signature re-signed and verified."""
    signer, verifier = scheme, scheme
    assert is_signer(signer), scheme
    assert is_scheme(verifier), scheme
    sk = ok(SigningKey.from_seed(signer, bytes.fromhex(k["seed"])), f"{scheme} from seed")
    if sk.to_bytes().hex() != k["signingKey"]:
        return f"{scheme} signing key: expected {k['signingKey']}, got {sk.to_bytes().hex()}"
    vk = ok(sk.verifying_key(), f"{scheme} verifying key")
    if vk.hex() != k["verifyingKey"]:
        return f"{scheme} verifying key: expected {k['verifyingKey']}, got {vk.hex()}"
    for s in k["signatures"]:
        m = bytes.fromhex(s["message"])
        sig = ok(sk.sign(m, aux if scheme in RANDOMISED else None), f"{scheme} sign {s['message']}")
        if sig.hex() != s["signature"]:
            return f"{scheme} signature on {s['message']}: expected {s['signature']}, got {sig.hex()}"
        if verify(verifier, vk, m, sig) != Ok(True):
            return f"{scheme} does not verify {s['message']}"
    return None


def test_every_scheme_signs_and_verifies_byte_for_byte() -> None:
    aux = bytes.fromhex(VECTORS["randomness"])
    count = 0
    for s in VECTORS["signing"]:
        scheme: str = s["scheme"]

        def check(k: Json, scheme: str = scheme) -> str | None:
            return check_key(scheme, k, aux)

        count += failures(s["keys"], check)
    assert count == 21, "7 schemes x 3 keys"


def test_every_committed_signature_verifies() -> None:
    count = 0
    for scheme in VECTORS["signing"]:
        for k in scheme["keys"]:
            for s in k["signatures"]:
                vk, m, sig = (bytes.fromhex(x) for x in (k["verifyingKey"], s["message"], s["signature"]))
                assert verify(scheme["scheme"], vk, m, sig) == Ok(True), f"{scheme['scheme']} {s['message']}"
                count += 1
    icarus = VECTORS["ed25519Bip32"]
    for s in icarus["signatures"]:
        vk, m, sig = (bytes.fromhex(x) for x in (icarus["addressVerifyingKey"], s["message"], s["signature"]))
        assert verify("Ed25519", vk, m, sig) == Ok(True), f"Icarus {s['message']}"
        count += 1
    assert count == 7 * 3 * 3 + 3


def verdict(c: Json) -> Json:
    """A verification row's result: the key is decoded first, then the signature."""
    k, m, s = (bytes.fromhex(c[f]) for f in ("verifyingKey", "message", "signature"))
    key_alone = decode_verifying_key(c["scheme"], k)
    match verify(c["scheme"], k, m, s):
        case Ok(True):
            return "valid"
        case Ok(False):
            return "invalid"
        case Err(e):
            part = "verifyingKey" if isinstance(key_alone, Err) else "signature"
            return {"part": part, "kind": kind_of(e)}


def test_verification_verdicts() -> None:
    rows = VECTORS["verification"]
    failures(rows, lambda c: None if verdict(c) == c["result"] else f"{c['scheme']} {c['name']}: {verdict(c)}")
    assert len(rows) >= 400, f"{len(rows)} verification cases"


def test_verifying_key_decoding() -> None:
    rows = [c for c in VECTORS["keyDecoding"] if c["part"] == "verifyingKey"]

    def check(c: Json) -> str | None:
        r = decode_verifying_key(c["scheme"], bytes.fromhex(c["bytes"]))
        got = "accepted" if isinstance(r, Ok) else kind_of(r.error)
        return None if got == c["result"] else f"{c['scheme']} {c['name']}: expected {c['result']}, got {got}"

    assert failures(rows, check) >= 40


def test_signing_key_decoding() -> None:
    rows = [c for c in VECTORS["keyDecoding"] if c["part"] == "signingKey"]

    def check(c: Json) -> str | None:
        r = SigningKey.from_bytes(c["scheme"], bytes.fromhex(c["bytes"]))
        got = "accepted" if isinstance(r, Ok) else kind_of(r.error)
        return None if got == c["result"] else f"{c['scheme']} {c['name']}: expected {c['result']}, got {got}"

    assert failures(rows, check) >= 40


def test_every_signing_error_kind_has_a_row_but_backend_failure() -> None:
    decoded = {c["result"] for c in VECTORS["keyDecoding"]}
    verified = {c["result"]["kind"] for c in VECTORS["verification"] if isinstance(c["result"], dict)}
    for k in get_args(crypto.CryptoErrorKind):
        exempt = k == "BackendFailure"
        assert (k in decoded) != exempt, f"keyDecoding pins {k} exactly when it is not exempt"
        assert (k in verified) != exempt, f"verification pins {k} exactly when it is not exempt"


def test_bls_proofs_of_possession() -> None:
    for p in VECTORS["blsPop"]:
        sk = ok(SigningKey.from_bytes("BLS12-381-PoP", bytes.fromhex(p["signingKey"])), "BLS key")
        vk = ok(sk.verifying_key(), "verifying key")
        assert vk.hex() == p["verifyingKey"], "PoP verifying key"
        proof = ok(sk.pop_prove(), "PoP")
        assert proof.hex() == p["proof"], "PoP"
        assert bls_pop_verify(vk, proof) == Ok(True), "PopVerify"
        assert verify("BLS12-381-PoP", vk, vk, proof) == Ok(False), "a PoP is not a message signature"
    assert len(VECTORS["blsPop"]) == 3


def test_bip39_mnemonics_and_seeds() -> None:
    english = WordList.english()
    for c in VECTORS["bip39"]:
        entropy = bytes.fromhex(c["entropy"])
        assert ok(mnemonic_from_entropy(entropy), "mnemonic") == c["mnemonic"]
        assert ok(mnemonic_from_entropy(entropy, english), "mnemonic, the list named") == c["mnemonic"]
        assert ok(mnemonic_to_entropy(c["mnemonic"]), "entropy").hex() == c["entropy"]
        assert ok(mnemonic_to_seed(c["mnemonic"], c["passphrase"]), "seed").hex() == c["seed"]
    assert len(VECTORS["bip39"]) == 6


def test_slip10_bip32_and_icarus_derivations() -> None:
    s = VECTORS["slip10Ed25519"]
    master = ok(ExtendedKey.slip10_master(bytes.fromhex(s["seed"])), "slip10 master")
    assert (master.key.to_bytes().hex(), master.chain_code.hex()) == (s["master"]["key"], s["master"]["chainCode"])
    child = ok(master.derive(s["path"]), "slip10 derive")
    assert (child.key.to_bytes().hex(), child.chain_code.hex()) == (s["child"]["key"], s["child"]["chainCode"])

    b = VECTORS["bip32Secp256k1"]
    master = ok(ExtendedKey.bip32_master(bytes.fromhex(b["seed"])), "bip32 master")
    assert (master.key.to_bytes().hex(), master.chain_code.hex()) == (b["master"]["key"], b["master"]["chainCode"])
    child = ok(master.derive(b["path"]), "bip32 derive")
    assert (child.key.to_bytes().hex(), child.chain_code.hex()) == (b["child"]["key"], b["child"]["chainCode"])
    assert ok(child.key.verifying_key(), "vk").hex() == b["verifyingKey"]

    p = b["publicDerivation"]
    parent = ok(master.derive(p["parentPath"]), "bip32 parent")
    assert ok(parent.key.verifying_key(), "vk").hex() == p["parentVerifyingKey"]
    assert parent.chain_code.hex() == p["parentChainCode"]
    vk, cc = bytes.fromhex(p["parentVerifyingKey"]), bytes.fromhex(p["parentChainCode"])
    for index in ok(parse_path(p["path"]), "path"):
        vk, cc = ok(bip32_public_child(vk, cc, index), f"CKDpub {index}")
    assert (vk.hex(), cc.hex()) == (p["verifyingKey"], p["chainCode"])
    assert p["verifyingKey"] == b["verifyingKey"], "CKDpub reaches the private path's key"

    i = VECTORS["ed25519Bip32"]
    root = ok(ExtendedKey.icarus_from_entropy(bytes.fromhex(i["entropy"]), i["passphrase"]), "icarus root")
    assert (root.key.to_bytes().hex(), root.chain_code.hex()) == (i["root"]["key"], i["root"]["chainCode"])
    account = ok(root.derive(i["accountPath"]), "icarus account")
    assert (account.key.to_bytes().hex(), account.chain_code.hex()) == (i["account"]["key"], i["account"]["chainCode"])
    assert ok(account.key.verifying_key(), "vk").hex() == i["accountVerifyingKey"]
    address = ok(account.derive(i["addressPath"]), "icarus address")
    assert (address.key.to_bytes().hex(), address.chain_code.hex()) == (i["address"]["key"], i["address"]["chainCode"])
    address_vk = ok(address.key.verifying_key(), "vk")
    assert address_vk.hex() == i["addressVerifyingKey"]
    pk, code = ok(account.key.verifying_key(), "vk"), account.chain_code
    for index in ok(parse_path(i["addressPath"]), "path"):
        pk, code = ok(icarus_public_child(pk, code, index), f"public child {index}")
    assert (pk, code) == (address_vk, address.chain_code), "public derivation reaches the address key"
    for sig in i["signatures"]:
        m = bytes.fromhex(sig["message"])
        signed = ok(address.key.sign(m), "Icarus sign")
        assert signed.hex() == sig["signature"], "Icarus signature"
        assert verify("Ed25519", address_vk, m, signed) == Ok(True), "Icarus signature verifies"


def test_path_strictness() -> None:
    rows = VECTORS["paths"]

    def check(c: Json) -> str | None:
        parsed = parse_path(c["path"])
        got: Json
        if isinstance(parsed, Ok):
            got = {"indices": list(parsed.value)}
        else:
            got = parsed.error.refusal if isinstance(parsed.error, key.InvalidPath) else kind_of(parsed.error)
        return None if got == c["result"] else f"path {c['path'][:40]!r}: expected {c['result']}, got {got}"

    assert failures(rows, check) >= 54
    refusals = {c["result"] for c in rows if isinstance(c["result"], str)}
    assert refusals == set(get_args(key.PathRefusal)), "every path refusal has a row"


def key_call(c: Json) -> object:
    """What a `keyRefusals` row's call gives: the refusal, or the value when accepted."""
    match c["check"]:
        case "bip39-entropy":
            return mnemonic_from_entropy(bytes.fromhex(c["entropy"]))
        case "bip39-phrase":
            return mnemonic_to_entropy(c["phrase"])
        case "word-list":
            return WordList.of(c["words"], " ")
        case "slip10":
            master = ExtendedKey.slip10_master(bytes.fromhex(c["seed"]))
            return master.value.derive(c["path"]) if isinstance(master, Ok) else master
        case "bip32":
            master = ExtendedKey.bip32_master(bytes.fromhex(c["seed"]))
            return master.value.derive(c["path"]) if isinstance(master, Ok) else master
        case "bip32-public" | "icarus-public":
            step = bip32_public_child if c["check"] == "bip32-public" else icarus_public_child
            vk, cc = bytes.fromhex(c["verifyingKey"]), bytes.fromhex(c["chainCode"])
            parsed = parse_path(c["path"])
            if isinstance(parsed, Err):
                return parsed
            for index in parsed.value:
                child = step(vk, cc, index)
                if isinstance(child, Err):
                    return child
                vk, cc = child.value
            return Ok((vk, cc))
    msg = f"unknown check {c['check']}"
    raise AssertionError(msg)


def test_key_derivation_refusals() -> None:
    rows = VECTORS["keyRefusals"]

    def check(c: Json) -> str | None:
        r = key_call(c)
        if not isinstance(r, Err):
            return f"{c['check']} {c['name']}: accepted, expected {c['kind']}"
        return None if kind_of(r.error) == c["kind"] else f"{c['check']} {c['name']}: got {kind_of(r.error)}"

    assert failures(rows, check) >= 16
    pinned = {c["kind"] for c in rows}
    assert set(get_args(key.KeyErrorKind)) - pinned == {"InvalidPath"}, (
        "every key error kind, the paths rows pinning InvalidPath"
    )


def test_the_scheme_names_are_the_vectors() -> None:
    assert {c["scheme"] for c in VECTORS["verification"]} == set(SCHEMES), "every scheme has verification rows"
    assert {c["scheme"] for c in VECTORS["keyDecoding"] if c["part"] == "signingKey"} == set(SIGNERS), "every signer"
    assert err(verify(untyped("Ed448"), b"", b"", b""), "an unknown scheme").kind == "TypeMismatch"
    assert err(SigningKey.from_seed(untyped("Ed448"), bytes(32)), "an unknown signer").kind == "TypeMismatch"


def test_a_mnemonic_holding_a_surrogate_is_refused_never_repaired() -> None:
    """A word list may hold U+FFFD; a phrase with a surrogate where it stands is another text, refused as such."""
    words = ok(WordList.of([f"�{i}" for i in range(2048)]), "a word list holding U+FFFD")
    phrase = ok(mnemonic_from_entropy(bytes(16), words), "mnemonic")
    assert ok(mnemonic_to_entropy(phrase, words), "entropy") == bytes(16)
    spoiled = phrase.replace("�", "\ud800")
    assert kind_of(err(mnemonic_to_entropy(spoiled, words), "entropy of a spoiled phrase")) == "InvalidMnemonic"
    assert kind_of(err(mnemonic_to_seed(spoiled, "", words), "seed of a spoiled phrase")) == "InvalidMnemonic"
    assert kind_of(err(mnemonic_to_seed(phrase, "\ud800", words), "a spoiled passphrase")) == "TypeMismatch"
