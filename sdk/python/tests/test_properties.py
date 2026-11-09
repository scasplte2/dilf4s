"""Properties of the boundary, with Hypothesis: round trips, no exception from any public call whatever its arguments,
caller buffers copied once, and refusals decided as the Rust SDK decides them."""

import inspect
import subprocess
import sys
import threading
from collections import UserList
from collections.abc import Callable, Iterator
from fractions import Fraction
from typing import Any, Final, cast

import pytest
from hypothesis import given, settings
from hypothesis import strategies as st

from dilf4s import Err, Ok, Value, _native, core, jlvm, proofs, signing
from dilf4s.core import (
    MAX_DEPTH,
    MAX_INTEGER_BITS,
    MAX_RATIONAL_BITS,
    Data,
    DivisionByZero,
    TooLarge,
    TooManyCopies,
    cbor_decode,
    cbor_encode,
    json_parse,
    json_render,
)
from dilf4s.errors import canonical
from dilf4s.errors import groth16 as groth16_errors
from dilf4s.errors import key as key_errors
from dilf4s.errors import poseidon as poseidon_errors
from dilf4s.errors import poseidon_merkle as pmt_errors
from dilf4s.errors import sigma as sigma_errors
from tests.support import err, kind_of, load, ok, untyped

# ------------------------------------------------------------------------------------------------ strategies

wide_ints: Final = st.one_of(
    st.integers(),
    st.integers(min_value=-(2**40), max_value=2**40),
    st.builds(lambda e, s, d: s * (2**e) + d, st.integers(0, 33300), st.sampled_from([-1, 1]), st.integers(-2, 2)),
)
texts: Final = st.text(alphabet=st.characters(codec="utf-8"), max_size=20)
any_text: Final = st.text(max_size=20)  # surrogates included

scalars: Final = st.one_of(
    st.none(),
    st.booleans(),
    st.integers(min_value=-(2**200), max_value=2**200),
    st.builds(Fraction, st.integers(-(10**30), 10**30), st.integers(1, 10**30)),
    texts,
    st.binary(max_size=40),
)
data: Final = st.recursive(
    scalars,
    lambda inner: st.one_of(
        st.lists(inner, max_size=5),
        st.lists(inner, max_size=5).map(tuple),
        st.dictionaries(texts, inner, max_size=5),
    ),
    max_leaves=30,
)


def java_bits(n: int) -> int:
    """Java's `BigInteger.bitLength`, which the data model bounds."""
    return (~n).bit_length() if n < 0 else n.bit_length()


def as_view(b: bytes) -> memoryview:
    return memoryview(b)


def value_of(x: Data) -> Value:
    return ok(Value.of(x), "of")


# ------------------------------------------------------------------------------------------------ round trips


@given(data)
def test_cbor_round_trips_every_value(x: Data) -> None:
    v = value_of(x)
    encoded = ok(cbor_encode(v), "encode")
    assert cbor_decode(encoded) == Ok(v), "decode(encode(v)) == v"
    assert ok(cbor_encode(ok(cbor_decode(encoded), "decode")), "re-encode") == encoded, "one encoding"


@given(data)
def test_python_data_round_trips(x: Data) -> None:
    v = value_of(x)
    back = ok(v.to_python(v.weight), "to_python")
    # what to_python gives is data of() reads; the list and dict types differ only in their element type
    assert Value.of(cast("Data", back)) == Ok(v), "of(to_python(v)) == v"
    if v.weight > 1:
        assert v.to_python(v.weight - 1) == Err(TooLarge(limit=v.weight - 1)), "a lighter limit refuses it in O(1)"


@given(data)
def test_json_round_trips_what_it_renders(x: Data) -> None:
    v = value_of(x)
    match json_render(v, 1 << 20):
        case Ok(text):
            assert json_parse(text) == Ok(v), f"parse(render(v)) == v for {text[:80]}"
        case Err(e):
            assert isinstance(e, canonical.NotJsonRepresentable | TooLarge), f"only what JSON cannot hold: {e!r}"


@given(wide_ints)
def test_integers_are_refused_exactly_past_the_data_models_width(n: int) -> None:
    r = Value.int(n)
    if java_bits(n) <= MAX_INTEGER_BITS:
        assert ok(r, "int").as_int() == n, "the integer, unchanged"
    else:
        assert r == Err(canonical.IntegerTooWide(limit=MAX_INTEGER_BITS)), "refused by its Java bit length"


def test_the_boundary_integers() -> None:
    top = 2**MAX_INTEGER_BITS
    assert isinstance(Value.int(top - 1), Ok)
    assert isinstance(Value.int(-top), Ok), "-2**33219 is 33219 bits to Java, though 33220 to Python"
    assert Value.int(top) == Err(canonical.IntegerTooWide(limit=MAX_INTEGER_BITS))
    assert Value.int(-top - 1) == Err(canonical.IntegerTooWide(limit=MAX_INTEGER_BITS))


@given(wide_ints, st.one_of(wide_ints, st.just(0)))
def test_numbers_are_reduced_then_bounded(n: int, d: int) -> None:
    r = Value.number(n, d)
    if d == 0:
        assert r == Err(DivisionByZero())
        return
    f = Fraction(n, d)
    if f.denominator == 1:
        expected = ok(Value.int(f.numerator), "int") if java_bits(f.numerator) <= MAX_INTEGER_BITS else None
        assert r == (Ok(expected) if expected is not None else Err(canonical.IntegerTooWide(limit=MAX_INTEGER_BITS)))
    elif max(java_bits(f.numerator), java_bits(f.denominator)) > MAX_RATIONAL_BITS:
        assert r == Err(canonical.RationalTooWide(limit=MAX_RATIONAL_BITS))
    else:
        assert ok(r, "number").as_rational() == f


def test_a_number_is_checked_once_reduced() -> None:
    assert Value.number(2**33220, 2**33219) == Value.int(2), "2**33220 / 2**33219 is 2"
    assert Value.number(2**33220, 2) == Err(canonical.IntegerTooWide(limit=MAX_INTEGER_BITS))
    assert Value.number(2**33220, 6) == Err(canonical.RationalTooWide(limit=MAX_RATIONAL_BITS))


# ------------------------------------------------------------------------------------------------ sharing and cycles


def test_shared_python_data_is_converted_once_and_shared() -> None:
    x: Data = []
    for _ in range(200):
        x = [x, x]
    v = value_of(x)
    assert v.depth == 201
    assert v.weight == core.MAX_WEIGHT, "2**201 - 1 nodes, saturated: built from 200 lists, never expanded"
    deeper: Data = []
    for _ in range(MAX_DEPTH):
        deeper = [deeper, deeper]
    assert Value.of(deeper) == Err(canonical.DepthExceeded(limit=MAX_DEPTH))


def test_a_cycle_is_refused_as_too_deep() -> None:
    loop: list[Data] = []
    loop.append(loop)
    assert Value.of(loop) == Err(canonical.DepthExceeded(limit=MAX_DEPTH))
    looped: dict[str, Data] = {}
    looped["self"] = looped
    assert Value.of(looped) == Err(canonical.DepthExceeded(limit=MAX_DEPTH))


# ------------------------------------------------------------------------------------------------ caller buffers


@given(st.binary(min_size=1, max_size=64))
def test_a_bytearray_is_copied_at_the_boundary(b: bytes) -> None:
    buffer = bytearray(b)
    v = ok(Value.bytes(buffer), "bytes")
    view = ok(Value.bytes(memoryview(buffer)), "memoryview")
    buffer[0] ^= 0xFF
    buffer.extend(b"more")
    assert v.as_bytes() == b, "the value holds the bytes as they were"
    assert view.as_bytes() == b, "a memoryview is copied too"


@given(st.binary(min_size=32, max_size=32))
def test_a_key_read_from_a_bytearray_does_not_change_with_it(seed: bytes) -> None:
    buffer = bytearray(seed)
    sk = ok(signing.SigningKey.from_bytes("Ed25519", buffer), "key")
    buffer[:] = bytes(32)
    assert sk.to_bytes() == seed


def test_a_released_memoryview_is_a_type_mismatch() -> None:
    view = memoryview(b"abc")
    view.release()
    assert isinstance(Value.bytes(view), Err)
    assert ok(Value.bytes(memoryview(b"abcd").cast("I")), "a cast view's bytes").as_bytes() == b"abcd"


# ------------------------------------------------------------------------------------------------ no exception escapes


class HostileInt(int):
    """An `int` whose every method a caller could override raises: the boundary reads it with `int`'s own."""

    def __lt__(self, other: object) -> bool:
        raise RuntimeError

    def __index__(self) -> int:
        raise RuntimeError

    def bit_length(self) -> int:
        raise RuntimeError

    def to_bytes(self, *_: object, **__: object) -> bytes:
        raise RuntimeError


class HostileStr(str):
    """A `str` whose methods raise."""

    __slots__ = ()

    def encode(self, *_: object, **__: object) -> bytes:
        raise RuntimeError


class HostileBytes(bytes):
    """A `bytes` whose methods raise."""

    def __len__(self) -> int:
        raise RuntimeError

    def __buffer__(self, flags: int, /) -> memoryview:
        raise RuntimeError


hostile: Final = st.one_of(
    wide_ints.map(HostileInt),
    any_text.map(HostileStr),
    st.binary(max_size=40).map(HostileBytes),
    st.lists(st.integers(), max_size=3).map(lambda xs: range(len(xs))),
)

junk: Final = st.one_of(
    hostile,
    st.none(),
    st.booleans(),
    wide_ints,
    st.floats(allow_nan=True),
    any_text,
    st.binary(max_size=80),
    st.binary(max_size=80).map(bytearray),
    st.binary(max_size=80).map(as_view),
    st.lists(st.one_of(st.integers(), st.binary(max_size=40), any_text), max_size=4),
    st.lists(st.binary(max_size=40), max_size=3).map(tuple),
    st.dictionaries(any_text, st.integers(), max_size=3),
    data.map(value_of),
    st.just(object()),
    st.just(int),
    st.sampled_from(["SHA-256", "BLAKE2b-256", "Ed25519", "BLS12-381-PoP", "m/0'/1", "a/b", "fiber/1"]),
)


def samples() -> dict[str, object]:
    """One instance of every native class, whose methods are fuzzed too."""
    h = ok(proofs.MerkleHashing.tagged(), "hashing")
    vt = ok(proofs.VersionedTrie.of(ok(proofs.VersionKey.unsigned_long(), "key")), "vt")
    kes = ok(signing.Kes.sum("BLAKE2b-256", 2), "kes")
    program = ok(jlvm.parse('{"+": [1, {"var": "x"}]}'), "program")
    versioned = load("versioned_trie_v1.json")
    current = next(p for p in versioned["proofs"] if p["kind"] == "state")
    state_proof = ok(cbor_decode(bytes.fromhex(current["cbor"])), "proof")
    return {
        "Value": value_of({"a": [1, b"x", Fraction(1, 3)]}),
        "Program": program,
        "Compiled": ok(jlvm.compile(program), "compiled"),
        "SigningKey": ok(signing.SigningKey.from_seed("BIP-340", bytes(range(32))), "key"),
        "WordList": signing.WordList.english(),
        "ExtendedKey": ok(signing.ExtendedKey.bip32_master(bytes(32)), "extended"),
        "VrfSigningKey": ok(signing.VrfSigningKey.from_seed("ECVRF-EDWARDS25519-SHA512-TAI", bytes(32)), "vrf"),
        "Kes": kes,
        "KesKey": ok(kes.generate(bytes(32)), "kes key"),
        "SigmaProposition": ok(
            signing.SigmaProposition.dlog(
                bytes.fromhex("02" + "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798")
            ),
            "dlog",
        ),
        "MerkleHashing": h,
        "InclusionProof": ok(proofs.InclusionProof.of(h, 0, 1, []), "inclusion"),
        "ConsistencyProof": ok(proofs.ConsistencyProof.of(h, 1, 1, []), "consistency"),
        "TrieHashing": ok(proofs.TrieHashing.of(), "trie"),
        "KeyMode": ok(proofs.KeyMode.variable(8), "mode"),
        "VerifiedTrieProof": ok(
            proofs.trie_check(
                ok(proofs.TrieHashing.of(), "t"),
                ok(proofs.KeyMode.variable(8), "m"),
                ok(proofs.TrieHashing.of(), "t").empty,
                ok(Value.array([]), "[]"),
            ),
            "verified",
        ),
        "PoseidonMerkleProof": ok(proofs.PoseidonMerkleProof.of(0, []), "pmt"),
        "Groth16VerifyingKey": ok(proofs.Groth16VerifyingKey.sp1(), "sp1"),
        "VersionKey": ok(proofs.VersionKey.unsigned_long(), "key"),
        "VersionedTrie": vt,
        "Replica": ok(proofs.Replica.empty(vt), "replica"),
        "VerifiedState": ok(vt.verify_state(bytes.fromhex(versioned["root"]), state_proof), "state"),
    }


SAMPLES: Final = samples()
# what a call returns without room for a refusal: an infallible constructor, a getter, a secret's bytes, an equality
INFALLIBLE: Final = {
    "Value.null",
    "WordList.english",
    "Value.as_bool",
    "Value.as_int",
    "Value.as_rational",
    "Value.as_text",
    "Value.as_bytes",
    "Value.as_array",
    "Value.as_map",
    "Value.get",
    "SigningKey.to_bytes",
    "VrfSigningKey.to_bytes",
    "VrfSigningKey.verifying_key",
    "KesKey.to_bytes",
    "KesKey.verifying_key",
    "Replica.entries",
}


def callables() -> dict[str, Callable[..., Any]]:
    """Every public function and method of the native module, by name."""
    found: dict[str, Callable[..., Any]] = {}
    for name in dir(_native):
        obj = getattr(_native, name)
        if name.startswith("_"):
            continue
        if inspect.isclass(obj):
            instance = SAMPLES.get(name)
            for attr, member in vars(obj).items():
                if attr.startswith("_") or not callable(member):
                    continue
                # a static method is called on the class; any other through the class's sample
                static = isinstance(inspect.getattr_static(obj, attr), staticmethod) or instance is None
                found[f"{name}.{attr}"] = getattr(obj, attr) if static else getattr(instance, attr)
        elif callable(obj):
            found[name] = obj
    return found


CALLS: Final = callables()


def test_every_class_has_a_sample_and_every_export_is_fuzzed() -> None:
    classes = {n for n in dir(_native) if inspect.isclass(getattr(_native, n))}
    assert classes == set(SAMPLES), "a sample of every class"
    exported = {
        getattr(m, n).__name__
        for m in (core, signing, proofs, jlvm)
        for n in m.__all__
        if inspect.isbuiltin(getattr(m, n))
    }
    assert exported <= set(CALLS), f"unfuzzed: {exported - set(CALLS)}"


def arity(fn: Callable[..., Any]) -> tuple[int, int]:
    """How many positional arguments a call takes: required, and at most."""
    params = [p for p in inspect.signature(fn).parameters.values() if p.name not in {"self", "$self"}]
    return sum(p.default is inspect.Parameter.empty for p in params), len(params)


@settings(max_examples=40)
@given(st.data())
def test_no_public_call_raises_on_any_arguments(draw: st.DataObject) -> None:
    for name, fn in sorted(CALLS.items()):
        low, high = arity(fn)
        if high == 0:
            continue
        n = draw.draw(st.integers(low, high), label=f"{name} arity")
        args = [draw.draw(junk, label=f"{name} argument {i}") for i in range(n)]
        result = fn(*args)
        if name not in INFALLIBLE:
            assert isinstance(result, Ok | Err), f"{name} gave {result!r}"


def test_wrong_types_are_type_mismatches_never_type_errors() -> None:
    assert core.cbor_decode(untyped("text")) == Err(
        canonical.TypeMismatch(reason="expected bytes, a bytearray or a memoryview")
    )
    assert isinstance(core.cbor_encode(untyped(b"bytes")), Err)
    assert isinstance(Value.int(untyped(True)), Err), "a bool is not an int"
    assert isinstance(Value.int(untyped(1.0)), Err), "a float is not an int"
    assert isinstance(jlvm.evaluate(untyped("program"), Value.null()), Err)
    assert isinstance(Value.text("\ud800"), Err)
    assert Value.text("\ud800") == Err(canonical.UnpairedSurrogate())
    two = proofs.poseidon_hash([1, 2])
    assert isinstance(two, Ok)
    assert proofs.poseidon_hash((1, 2)) == two, "a tuple"
    not_items = canonical.TypeMismatch(reason="expected the inputs: a list or a tuple")
    for other in (range(1, 3), UserList([1, 2]), iter([1, 2]), b"\x01\x02", "ab", bytearray(b"\x01\x02")):
        assert proofs.poseidon_hash(untyped(other)) == Err(not_items), f"{type(other).__name__} is no list or tuple"
    assert core.hash_parts("SHA-256", untyped(b"")) == Err(
        canonical.TypeMismatch(reason="expected the parts: a list or a tuple")
    ), "empty bytes are never read as no parts"


class RaisingList(list[int]):
    """A list whose own methods raise: read with the concrete API, none of them runs."""

    def __len__(self) -> int:
        raise LookupError

    def __iter__(self) -> Iterator[int]:
        raise LookupError

    def __getitem__(self, i: Any) -> Any:
        raise LookupError


class RaisingSequence(UserList[int]):
    """A sequence that is no list or tuple, whose own methods raise: refused by its type, none of them run."""

    def __len__(self) -> int:
        raise LookupError

    def __iter__(self) -> Iterator[int]:
        raise LookupError


def test_no_caller_code_runs_reading_a_sequence() -> None:
    assert proofs.poseidon_hash(RaisingList([1, 2])) == proofs.poseidon_hash([1, 2]), "a list subclass, as a list"
    refused = Err(canonical.TypeMismatch(reason="expected the inputs: a list or a tuple"))
    assert proofs.poseidon_hash(untyped(RaisingSequence([1]))) == refused


R: Final = proofs.FR_MODULUS


def test_a_count_refused_alone_is_refused_before_any_item_is_read() -> None:
    """Each argument's items would be refused too (`R` is no field element; the pairs hold no bytes), so the count's
    refusal shows they were never read."""
    assert proofs.poseidon_hash([R] * 5) == Err(poseidon_errors.InputCount(count=5, max=4))
    assert proofs.poseidon_hash([0] * 10_000_000) == Err(poseidon_errors.InputCount(count=10_000_000, max=4))
    assert proofs.poseidon_hash([R]) == Err(poseidon_errors.NonCanonical(bits=254)), "within the count, read"
    of = proofs.PoseidonMerkleProof.of
    assert of(0, [R] * 257) == Err(pmt_errors.Position(position_bits=0, depth=257))
    assert of(2**300, [R] * 10_000_000) == Err(pmt_errors.Position(position_bits=301, depth=10_000_000))
    assert of(0, [R] * 256) == Err(poseidon_errors.NonCanonical(bits=254)), "within the depth, read"
    wrong_pairs = [
        (proofs.bn254_pairing(untyped([(1, 2, 3)])), "expected (G1, G2) pairs"),
        (signing.bls_aggregate_verify(untyped([(1, 2, 3)]), b""), "expected (key, message) pairs"),
        (signing.bls_batch_verify(untyped([(1, 2)])), "expected (key, message, signature) triples"),
        (
            proofs.trie_root(
                ok(proofs.TrieHashing.of(), "hashing"), ok(proofs.KeyMode.fixed(32), "mode"), untyped([(1,)])
            ),
            "expected (key, value hash) entries",
        ),
    ]
    for got, reason in wrong_pairs:
        assert got == Err(canonical.TypeMismatch(reason=reason)), reason


def test_a_word_list_of_another_count_is_refused_by_rust_from_its_count_alone() -> None:
    def count_refusal(n: int) -> key_errors.InvalidWordList:
        return key_errors.InvalidWordList(
            reason=f"expected 2048 distinct words without separators, got {n}", separator=None
        )

    assert signing.WordList.of(untyped([1] * 5)) == Err(count_refusal(5)), "the words are not read"
    assert signing.WordList.of(["x" * 10**6] * 10**6) == Err(count_refusal(10**6)), "nor copied"
    assert signing.WordList.of([]) == Err(count_refusal(0))
    assert isinstance(signing.WordList.of(untyped([1] * 2048)), Err), "2048 are read"
    assert signing.WordList.of(untyped([1] * 2048)) == Err(canonical.TypeMismatch(reason="expected a str"))
    assert signing.WordList.of(["x"] * 5, untyped("ab")) == Err(
        canonical.TypeMismatch(reason="expected one separator character")
    ), "the separator first, as Rust checks it first"


def test_a_connective_is_refused_as_rust_refuses_it() -> None:
    """Its width, its leaves and its threshold, each refused as the owned Rust constructors refuse them."""
    sigma = signing.SigmaProposition
    leaf = ok(sigma.dlog(bytes.fromhex("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798")), "leaf")
    wide = ok(sigma.and_([leaf] * 255), "255 leaves")
    full = ok(sigma.and_([wide] * 16), "4080 leaves")
    assert full.leaves == 4080
    too_wide = Err(sigma_errors.InvalidProposition(reason="a connective has at most 255 children"))
    assert sigma.and_([leaf] * 256) == too_wide
    assert sigma.or_([leaf] * 256) == too_wide
    assert sigma.and_([full, full]) == Err(
        sigma_errors.InvalidProposition(reason="a proposition has at most 4096 leaves")
    )
    assert sigma.threshold(300, [leaf] * 256) == Err(
        sigma_errors.InvalidProposition(reason="a threshold needs 1 <= k <= 256 children, got k = 300")
    )
    assert ok(sigma.threshold(2, [leaf] * 3), "2 of 3") == ok(sigma.threshold(2, (leaf, leaf, leaf)), "a tuple")
    nest = leaf
    for _ in range(63):
        nest = ok(sigma.and_([nest]), "nested")
    assert nest.depth == 64
    assert sigma.and_([nest]) == Err(sigma_errors.InvalidProposition(reason="a proposition nests at most 64 deep"))
    assert sigma.or_([]) == Err(
        sigma_errors.InvalidProposition(reason="a connective needs at least one child: an empty one holds for anyone")
    )
    assert sigma.threshold(0, [full] * 300) == Err(
        sigma_errors.InvalidProposition(reason="a threshold needs 1 <= k <= 300 children, got k = 0")
    ), "the threshold before the width, as Rust checks them"


G1: Final = (1).to_bytes(32, "big") + (2).to_bytes(32, "big")
"""BN254's G1 generator, which decodes: a proof starting with it passes Rust's checks before the inputs' count."""


def test_more_public_inputs_than_the_key_takes_are_refused_by_their_count_unread() -> None:
    key = ok(proofs.Groth16VerifyingKey.sp1(), "SP1's key")
    assert key.public_inputs == 5
    proof = G1 + b"\x01" * 192
    word = b"\x00" * 32

    def over(got: int) -> Err[groth16_errors.Malformed]:
        return Err(groth16_errors.Malformed(reason=f"expected 5 public inputs, got {got}"))

    assert proofs.groth16_verify(key, proof, [word] * 4) == over(4), "fewer: read, and refused by Rust"
    assert proofs.groth16_verify(key, proof, [word] * 6) == over(6), "more: refused by the count, in Rust's words"
    assert proofs.groth16_verify(key, proof, untyped([1] * 10_000_000)) == over(10_000_000), "unread"
    assert proofs.groth16_verify(key, b"", untyped([1] * 6)) == Err(
        groth16_errors.Malformed(reason="a proof is 256 bytes, got 0")
    ), "the proof's own checks first, as Rust makes them"


CONSTRAINED: Final = """
import resource
from fractions import Fraction
from dilf4s import Value, core, proofs, signing

def mapped():
    with open("/proc/self/status", encoding="ascii") as status:
        return next(int(line.split()[1]) * 1024 for line in status if line.startswith("VmSize:"))

sigma = signing.SigmaProposition
leaf = sigma.dlog(bytes.fromhex("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798")).value
full = sigma.and_([sigma.and_([leaf] * 255).value] * 16).value
key = proofs.Groth16VerifyingKey.sp1().value
proof = (1).to_bytes(32, "big") + (2).to_bytes(32, "big") + bytes([1]) * 192
wide, pair, words, inputs = [full] * 256, [full] * 2, [""] * 2_000_000, [bytes(32)] * 2_000_000
many, long = [leaf] * 5_000_000, ["x" * 1_000_000] * 2048
merkle, digests = proofs.MerkleHashing.tagged().value, [bytes(1_000_000)] * 2048
big, shared = bytes(1_000_000), bytearray(1_000_000)
trie, mode = proofs.TrieHashing.of().value, proofs.KeyMode.variable(8).value
g1 = bytes.fromhex("97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb")
pairs, entries, parts, signed = [(big, big)] * 64, [(b"k", big)] * 2048, [shared] * 2048, [(g1, big)] * 2048
wraps, name = [("wrap", shared, 1)] * 64, "k" * 1_000_000
dicts = [{name: 1} for _ in range(64)] + [object()]
huge, deep = 1 << 33000, 0
for _ in range(256):
    deep = [deep]
numbers, fractions = [huge] * 20000 + [deep], [Fraction(huge, 3)] * 20000 + [deep]
vt, version = proofs.VersionedTrie.of(proofs.VersionKey.bytes(1024).value).value, bytes(1024)
claims, keyed, keys = [(version, bytes(32))] * 100_000, [(g1, b"")] * 300_000, [g1] * 300_000
points, one = [bytes(64)] * 1_000_000, bytearray(12 * 2**20)
hashes = bytes(16 * 2**20)
resource.setrlimit(resource.RLIMIT_AS, (mapped() + 32 * 2**20, resource.getrlimit(resource.RLIMIT_AS)[1]))
for result in (
    sigma.and_(wide),
    sigma.and_(pair),
    signing.WordList.of(words),
    proofs.groth16_verify(key, proof, inputs),
    sigma.and_(many),
    signing.WordList.of(long, "x"),
    signing.WordList.of(long),
    proofs.merkle_root(merkle, digests),
    proofs.bn254_pairing(pairs),
    proofs.trie_root(trie, mode, entries),
    core.expand_message_xmd("SHA-256", parts, b"dst", 2**40),
    signing.bls_aggregate_verify(signed, bytes(5)),
    core.build_value(b"", wraps),
    Value.of(dicts),
    Value.of(numbers),
    Value.of(fractions),
    vt.verify_versions(bytes(32), Value.null(), version, None, claims),
    signing.bls_aggregate_verify(keyed, b""),
    signing.bls_fast_aggregate_verify(keys, b"m", b""),
    proofs.Groth16VerifyingKey.from_bytes(bytes(64), bytes(128), bytes(128), bytes(128), points),
    signing.verify("Ed25519", one, one, one),
    trie.branch_hash(0, b"", hashes, hashes, hashes),
):
    print(getattr(result.error, "reason", result.error.kind))
"""


@pytest.mark.skipif(sys.platform != "linux", reason="reads /proc/self/status")
def test_a_refusal_of_a_large_argument_allocates_nothing_in_proportion_to_it() -> None:
    """Each argument is allocated, then the process may map only 32 MiB more: refusing it must not copy it."""
    run = subprocess.run([sys.executable, "-c", CONSTRAINED], capture_output=True, text=True, check=False)  # noqa: S603
    assert run.returncode == 0, f"aborted ({run.returncode}): {run.stderr[-400:]}"
    assert run.stdout.splitlines() == [
        "a connective has at most 255 children",
        "a proposition has at most 4096 leaves",
        "expected 2048 distinct words without separators, got 2000000",
        "expected 5 public inputs, got 2000000",
        "a connective has at most 255 children",
        "the separator is not one phrases are split on",
        "expected 2048 distinct words without separators, got 2048 (1 distinct)",
        "a digest is 32 bytes, got 1000000",
        "a G1 point must be 64 bytes, got 1000000",
        "a digest is 32 bytes, got 1000000",
        "InvalidExpansion",
        "expected 96 bytes, got 5",
        "UnexpectedEnd",
        "expected None, a bool, an int, a Fraction, a str, bytes, a list, a tuple, a dict or a Value",
        "DepthExceeded",
        "DepthExceeded",
        "<root>: expected map, got null",
        "expected 96 bytes, got 0",
        "expected 96 bytes, got 0",
        "the key's alpha is the identity",
        "expected 32 bytes, got 12582912",
        "a digest is 32 bytes, got 16777216",
    ]


def test_json_text_with_a_surrogate_is_refused_where_it_stands() -> None:
    """As TypeScript's parser refuses a lone surrogate: in a string, `UnpairedSurrogate`; after a backslash, an
    invalid escape; outside a string, a syntax error at its offset. Never other, valid text."""
    invalid_escape = canonical.JsonSyntax(offset=1, reason="invalidEscape")  # at the backslash
    for parse in (json_parse, lambda t: core.json_parse_with_max_scale(t, 100)):
        assert parse('"\ud800"') == Err(canonical.UnpairedSurrogate()), "in a string"
        assert parse('"\\\ud800"') == Err(invalid_escape), "just after a backslash: an escape it does not complete"
        assert parse('{"\\\ud800": 1}') == Err(canonical.JsonSyntax(offset=2, reason="invalidEscape")), "in a key"
        assert parse('"\\\\\ud800"') == Err(canonical.UnpairedSurrogate()), "after an escaped backslash"
        assert parse("[1, \ud800]") == Err(canonical.JsonSyntax(offset=4, reason="expectedValue")), "outside"
        assert parse('"\ud800\\udc00"') == Ok(ok(Value.text("\U00010000"), "pair")), "a pair, as UTF-16 joins it"
    assert kind_of(err(jlvm.parse_value('"\\\ud800"'), "JLVM data")) == "Malformed"


def test_a_wide_int_subclass_is_read_with_ints_own_methods() -> None:
    wide = HostileInt(1 << 600)
    assert proofs.poseidon_hash([wide]) == Err(poseidon_errors.NonCanonical(bits=601))
    assert proofs.fr_to_bytes(wide) == Err(poseidon_errors.NonCanonical(bits=601))
    assert proofs.fr_to_bytes(HostileInt(-(1 << 600))) == Err(poseidon_errors.NonCanonical(bits=600))
    assert Value.int(HostileInt(7)) == Value.int(7)
    assert Value.int(HostileInt(1 << 40_000)) == Err(canonical.IntegerTooWide(limit=MAX_INTEGER_BITS))


def test_a_built_value_asks_for_no_more_than_the_platform_holds() -> None:
    null = bytes.fromhex("f6")
    assert core.build_value(null, [("copies", 2**64 - 1)]) == Err(TooManyCopies(copies=2**64 - 1))
    assert core.build_value(null, [("copies", 2**60)]) == Err(TooManyCopies(copies=2**60))
    assert ok(core.build_value(null, [("copies", 3)]), "three").weight == 4
    # a copies step Rust never reaches is never refused: an earlier template that does not decode comes first
    first = core.build_value(null, [("wrap", b"", 1), ("copies", 2**64 - 1)])
    assert kind_of(err(first, "the template first")) == "UnexpectedEnd"
    dollar, pair = bytes.fromhex("6124"), bytes.fromhex("8261246124")
    assert core.build_value(null, [("wrap", dollar, 2**64 - 1)]) == Ok(Value.null()), "a bare $ is the value itself"
    assert core.build_value(null, [("wrap", pair, 2**64 - 1)]) == Err(canonical.DepthExceeded(limit=MAX_DEPTH))
    assert ok(core.build_value(null, [("wrap", pair, 40)]), "2**40 nulls").weight == 2**41 - 1


# ------------------------------------------------------------------------------------------------ threads


def test_evaluations_on_threads_agree_with_one_thread() -> None:
    """Evaluation releases the GIL; outcomes are a function of their inputs, on any thread."""
    program = ok(jlvm.parse('{"reduce": [{"var": "xs"}, {"+": [{"var": "current"}, {"var": "accumulator"}]}, 0]}'), "p")
    inputs = [value_of({"xs": list(range(n))}) for n in range(1, 41)]
    alone = [ok(jlvm.evaluate(program, x), "evaluate") for x in inputs]
    results: dict[int, object] = {}

    def run(i: int) -> None:
        results[i] = ok(jlvm.evaluate(program, inputs[i]), "evaluate")

    threads = [threading.Thread(target=run, args=(i,)) for i in range(len(inputs))]
    for t in threads:
        t.start()
    for t in threads:
        t.join()
    assert [results[i] for i in range(len(inputs))] == alone
