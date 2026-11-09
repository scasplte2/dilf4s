"""The package against its sources: the error families against the Rust SDK's, the name sets against the native module,
the generated modules against the generator, and the bindings' lockfile against the Rust SDK's."""

import dataclasses
import importlib.util
import re
import sys
import tomllib
from pathlib import Path
from types import ModuleType
from typing import Final, get_args

import pytest

import dilf4s
from dilf4s import _native_types, core, errors, jlvm, proofs, signing
from dilf4s.errors import FAMILIES

SDK: Final = Path(__file__).resolve().parents[2]
GENERATED: Final = SDK / "rust" / "core" / "src" / "generated"


def camel(rule: str) -> str:
    return "".join(part[:1].upper() + part[1:] for part in rule.split("-"))


def rust_families() -> dict[str, dict[str, list[str]]]:
    """Every `pub enum` of the Rust SDK's generated error files: its variants and each variant's fields, in order."""
    found: dict[str, dict[str, list[str]]] = {}
    for path in GENERATED.glob("*_errors.rs"):
        enum: str | None = None
        variant: str | None = None
        for line in path.read_text(encoding="utf-8").splitlines():
            if m := re.match(r"^pub enum (\w+) \{$", line):
                enum, found[m.group(1)] = m.group(1), {}
            elif line == "}":
                enum = None
            elif enum and (m := re.match(r"^    (\w+)(?: \{|,)$", line)):
                variant = m.group(1)
                found[enum][variant] = []
            elif enum and variant and (m := re.match(r"^        (\w+): ", line)):
                found[enum][variant].append(m.group(1))
    return found


RUST: Final = rust_families()


@pytest.mark.parametrize(("schema", "module"), sorted(FAMILIES.items()))
def test_each_family_is_the_rust_sdks_kind_for_kind_and_field_for_field(schema: str, module: ModuleType) -> None:
    family = camel(schema)
    rust = RUST.get(family)
    assert rust is not None, f"no Rust family {family}"
    kinds = get_args(getattr(module, f"{family}Kind"))
    assert list(kinds) == list(rust), f"{family}: the kinds, in the family's order"
    union = set(get_args(getattr(module, family)))
    for kind, fields in rust.items():
        cls = getattr(module, kind)
        assert cls in union, f"{family}.{kind} is in the family"
        names = [f.name for f in dataclasses.fields(cls) if f.name != "kind"]
        assert names == fields, f"{family}.{kind}: the Rust fields, by name and order"
        params = getattr(cls, "__dataclass_params__", None)
        assert params is not None
        assert params.frozen, f"{family}.{kind} is frozen"


def test_the_name_sets_are_the_native_modules() -> None:
    assert set(get_args(_native_types.Scheme.__value__)) == set(signing.SCHEMES)
    assert set(get_args(_native_types.Signer.__value__)) == set(signing.SIGNERS)
    assert set(get_args(_native_types.VrfSuite.__value__)) == set(signing.VRF_SUITES)
    assert set(get_args(_native_types.HashAlgorithmName.__value__)) == set(core.HASH_ALGORITHMS)
    made = [signing.Kes.sum("SHA-256", 1), signing.Kes.product("SHA-256", 1, 1), signing.Kes.cardano(1)]
    constructions = {k.value.construction for k in made if isinstance(k, dilf4s.Ok)}
    assert constructions == set(get_args(_native_types.KesConstruction.__value__))


def test_the_generated_modules_are_current() -> None:
    spec = importlib.util.spec_from_file_location("gen_errors", SDK / "python" / "tools" / "gen_errors.py")
    assert spec is not None
    assert spec.loader is not None
    gen = importlib.util.module_from_spec(spec)
    sys.modules["gen_errors"] = gen
    spec.loader.exec_module(gen)
    for path, text in gen.outputs().items():
        assert path.read_text(encoding="utf-8") == text, f"{path.name} is stale: run tools/gen_errors.py"
    written = {p.name for p in (SDK / "python" / "python" / "dilf4s" / "errors").glob("*.py")}
    assert written == {p.name for p in gen.outputs()}, "no module the generator does not write"


def test_the_bindings_build_the_crates_the_rust_sdk_tests() -> None:
    """Every crate both lockfiles hold resolves to the same version and source in each."""

    def packages(lock: Path) -> dict[str, set[tuple[str, str]]]:
        out: dict[str, set[tuple[str, str]]] = {}
        for p in tomllib.loads(lock.read_text(encoding="utf-8"))["package"]:
            out.setdefault(p["name"], set()).add((p["version"], p.get("source", "path")))
        return out

    ours, theirs = packages(SDK / "python" / "Cargo.lock"), packages(SDK / "rust" / "Cargo.lock")
    shared = set(ours) & set(theirs)
    assert {"dilf4s-core", "dilf4s-jlvm", "dilf4s-proofs", "dilf4s-signing", "k256", "bls12_381"} <= shared
    differ = {name: (ours[name], theirs[name]) for name in shared if not ours[name] <= theirs[name]}
    assert not differ, f"crates the bindings resolve otherwise than sdk/rust: {differ}"


def test_every_exported_name_resolves() -> None:
    for module in (dilf4s, core, signing, proofs, jlvm, errors):
        for name in module.__all__:
            assert hasattr(module, name), f"{module.__name__}.{name}"


def test_secret_keys_never_show_their_bytes() -> None:
    seed = bytes(range(32))
    sk = dilf4s.signing.SigningKey.from_seed("Ed25519", seed)
    assert isinstance(sk, dilf4s.Ok)
    assert seed.hex() not in repr(sk.value)
    assert "redacted" in repr(sk.value)
    vrf = dilf4s.signing.VrfSigningKey.from_seed("ECVRF-EDWARDS25519-SHA512-TAI", seed)
    assert isinstance(vrf, dilf4s.Ok)
    assert seed.hex() not in repr(vrf.value)
