"""Generates the error families of `shared/schemas/*-errors.cddl` as frozen dataclasses: `python/dilf4s/errors`.

Each family (`x-error = { "kind": "A", ... // "kind": "B", ... }`) becomes a module: one frozen, final dataclass per
kind, holding that kind's fields (camelCase names in snake_case, as the Rust emitter names them) and its `kind`
literal; a `Kind` literal of every kind in the family's order (checked against `x-error-kind`); and the family itself,
the union of its kinds. A choice of text literals becomes a `Literal` alias in the module of the first family that
names it. `preview` is the SDK's own `dilf4s.Preview`, as the schema says.

    python tools/gen_errors.py           # writes python/dilf4s/errors
    python tools/gen_errors.py --check   # fails if a written file differs (tests/test_generated.py runs it)
"""

from __future__ import annotations

import argparse
import re
import sys
import textwrap
from dataclasses import dataclass
from pathlib import Path
from typing import Final

ROOT: Final = Path(__file__).resolve().parents[3]
SCHEMAS: Final = ROOT / "shared" / "schemas"
OUT: Final = Path(__file__).resolve().parents[1] / "python" / "dilf4s" / "errors"
FILES: Final = ("codec-errors", "hash-errors", "crypto-errors", "proofs-errors", "jlvm-errors", "store-errors")
WIDTH: Final = 120

RULE = re.compile(r"^([a-z0-9_-]+) =(.*)$")
FIELD = re.compile(r'^(\?\s*)?"([A-Za-z0-9]+)"\s*:\s*(.+)$')
IMPORT = re.compile(r"^;# import ([a-z0-9-]+) as ([a-z0-9]+)$")
INT_RANGE = re.compile(r"^-?[0-9]+\.\.-?[0-9]+$")


@dataclass(frozen=True)
class Rule:
    """A CDDL rule: its file, name, body (comments removed from code lines, kept as `;` lines) and doc comment."""

    file: str
    name: str
    body: str
    doc: str


@dataclass(frozen=True)
class Field:
    """One field of a kind."""

    name: str
    type: str
    optional: bool
    doc: str


@dataclass(frozen=True)
class Variant:
    """One kind of a family."""

    kind: str
    doc: str
    fields: tuple[Field, ...]


def read_file(file: str) -> tuple[dict[str, Rule], dict[str, str]]:
    """The rules of one schema file, and its imports (namespace to file).

    A rule starts at a line `name =` in the first column and runs to the next blank line or first-column comment,
    except inside braces it has not closed; the first-column comments just before it are its doc.
    """
    rules: dict[str, Rule] = {}
    imports: dict[str, str] = {}
    pending: list[str] = []
    current: tuple[str, list[str], str] | None = None

    def close() -> None:
        nonlocal current
        if current:
            rules[current[0]] = Rule(file, current[0], "\n".join(current[1]), current[2])
            current = None

    for line in (SCHEMAS / f"{file}.cddl").read_text(encoding="utf-8").splitlines():
        imported = IMPORT.match(line)
        started = RULE.match(line)
        open_braces = current is not None and not is_closed(current[1])
        if imported:
            imports[imported.group(2)] = imported.group(1)
        elif started:
            close()
            current = (started.group(1), [started.group(2)], comment_text(pending))
            pending = []
        elif open_braces and current:
            current[1].append(line)
        elif line.startswith(";"):
            close()
            pending.append(line)
        elif not line.strip():
            close()
            pending = []
        elif current:
            current[1].append(line)
    close()
    return rules, imports


def is_closed(lines: list[str]) -> bool:
    """Whether a rule's lines so far close every brace they open."""
    code = "\n".join(line for line in lines if not line.strip().startswith(";"))
    return code.count("{") == code.count("}")


def comment_text(lines: list[str]) -> str:
    """Comment lines as one paragraph, without their `;` marks."""
    return " ".join(line.lstrip(";").strip() for line in lines if not line.startswith(";;")).strip()


def variants(rule: Rule) -> tuple[Variant, ...] | None:
    """A family's kinds, or None when the rule is not a choice of maps led by `"kind"`."""
    body = rule.body.strip()
    if not (body.startswith("{") and body.endswith("}")):
        return None
    found: list[Variant] = []
    pending: list[str] = []
    kind: str | None = None
    kind_doc = ""
    fields: list[Field] = []
    for raw in body[1:-1].splitlines():
        line = raw.strip()
        if line.startswith(";"):
            pending.append(line)
            continue
        for piece in re.split(r"(//|,)", line):
            part = piece.strip()
            if part in {"", ","}:
                continue
            if part == "//":
                if kind is None:
                    return None
                found.append(Variant(kind, kind_doc, tuple(fields)))
                kind, fields = None, []
                continue
            matched = FIELD.match(part)
            if not matched:
                return None
            optional, name, type_ = bool(matched.group(1)), matched.group(2), matched.group(3).strip()
            if name == "kind":
                kind, kind_doc, pending = type_.strip('"'), comment_text(pending), []
            else:
                fields.append(Field(name, type_, optional, comment_text(pending)))
                pending = []
    if kind is None:
        return None
    found.append(Variant(kind, kind_doc, tuple(fields)))
    return tuple(found)


def choices(rule: Rule) -> tuple[str, ...]:
    """The alternatives of a type choice (`a / b / "c"`)."""
    code = " ".join(line for line in rule.body.splitlines() if not line.strip().startswith(";"))
    # a `/` inside a text literal (the operation "/") does not separate alternatives
    parts = [""]
    for token in re.findall(r'"(?:[^"\\]|\\.)*"|/|[^/"]+', code):
        if token == "/":
            parts.append("")
        else:
            parts[-1] += token
    return tuple(part.strip() for part in parts if part.strip())


def snake(name: str) -> str:
    """`keyBytes` as `key_bytes`: the Rust emitter's field names."""
    return re.sub(r"(?<!^)([A-Z])", r"_\1", name).lower()


def camel(rule: str) -> str:
    """`canonical-error` as `CanonicalError`."""
    return "".join(part[:1].upper() + part[1:] for part in re.split(r"[-_]", rule))


def module_of(family: str) -> str:
    """The module of a family: its name without `-error`, in snake_case."""
    return family.removesuffix("-error").replace("-", "_")


class Schemas:
    """Every rule the error files reach, by file."""

    def __init__(self) -> None:
        """Reads the error files and every file they import."""
        self.rules: dict[str, dict[str, Rule]] = {}
        self.imports: dict[str, dict[str, str]] = {}
        todo = list(FILES)
        while todo:
            file = todo.pop()
            if file in self.rules:
                continue
            self.rules[file], self.imports[file] = read_file(file)
            todo.extend(self.imports[file].values())
        self.families: dict[tuple[str, str], tuple[Variant, ...]] = {
            (file, name): found
            for file in FILES
            for name, rule in self.rules[file].items()
            if (found := variants(rule)) is not None
        }
        # each text alias's home: the module of the first family that names it
        self.aliases: dict[tuple[str, str], str] = {}
        for (file, name), found in self.families.items():
            for variant in found:
                for f in variant.fields:
                    target = self.resolve(file, f.type)
                    if target and target not in self.families and self.literals(*target) and target[1] != "preview":
                        self.aliases.setdefault(target, module_of(name))

    def resolve(self, file: str, ref: str) -> tuple[str, str] | None:
        """The (file, rule) a reference names, or None when it is not a rule."""
        namespace, _, name = ref.rpartition(".")
        target = self.imports[file].get(namespace) if namespace else file
        if target is None or name not in self.rules.get(target, {}):
            return None
        return (target, name)

    def literals(self, file: str, name: str) -> tuple[str, ...] | None:
        """The texts of a rule that is a choice of text literals (through other such rules), or None."""
        out: list[str] = []
        for part in choices(self.rules[file][name]):
            if part.startswith('"') and part.endswith('"'):
                out.append(part.strip('"'))
                continue
            target = self.resolve(file, part)
            nested = self.literals(*target) if target else None
            if nested is None:
                return None
            out.extend(nested)
        return tuple(out)

    def python_type(self, file: str, expr: str, home: str) -> tuple[str, set[str]]:
        """A field type's Python annotation, and the modules it needs."""
        head = expr.split(maxsplit=1)[0]
        if head in {"text", "tstr"}:
            return "str", set()
        if head in {"uint", "int", "nint"} or INT_RANGE.match(head):
            return "int", set()
        if head == "bool":
            return "bool", set()
        if head in {"bytes", "bstr"}:
            return "bytes", set()
        target = self.resolve(file, head)
        if target is None:
            msg = f"{file}: no rule {expr!r}"
            raise SystemExit(msg)
        if target[1] == "preview":
            return "Preview", {"preview"}
        if target in self.families:
            module = module_of(target[1])
            name = camel(target[1])
            return (name, set()) if module == home else (f"{module}.{name}", {module})
        if target in self.aliases:
            module = self.aliases[target]
            name = camel(target[1])
            return (name, set()) if module == home else (f"{module}.{name}", {module})
        alternatives = choices(self.rules[target[0]][target[1]])
        if len(alternatives) == 1:
            return self.python_type(target[0], alternatives[0], home)
        msg = f"{file}: cannot type {expr!r}"
        raise SystemExit(msg)


def docstring(text: str, indent: str, attributes: list[tuple[str, str]] | None = None) -> list[str]:
    """A docstring, wrapped, with a Google-style attribute list."""
    width = WIDTH - len(indent)
    summary = text or "No description."
    lines = textwrap.wrap(summary.replace("\\", "\\\\"), width=width - 3)
    lines[-1] += "." if not lines[-1].endswith((".", ":", ")")) else ""
    if attributes:
        lines += ["", "Attributes:"]
        for name, doc in attributes:
            body = (doc or "See the schema.").replace("\\", "\\\\")
            wrapped = textwrap.wrap(f"{name}: {body}", width=width - 4, subsequent_indent="    ")
            lines += [f"    {w}" for w in wrapped]
    if len(lines) == 1 and len(lines[0]) + 6 <= width:
        return [f'{indent}"""{lines[0]}"""']
    out = [f'{indent}"""{lines[0]}']
    out += [f"{indent}{line}" if line else "" for line in lines[1:]]
    out.append(f'{indent}"""')
    return out


def literal(name: str, texts: tuple[str, ...]) -> list[str]:
    """A `Literal` alias, one text per line when it does not fit on one."""
    one = f"{name}: TypeAlias = Literal[{', '.join(repr_text(t) for t in texts)}]"
    if len(one) <= WIDTH:
        return [one]
    return [f"{name}: TypeAlias = Literal[", *[f"    {repr_text(t)}," for t in texts], "]"]


def repr_text(text: str) -> str:
    """A text as a double-quoted Python literal."""
    return '"' + text.replace("\\", "\\\\").replace('"', '\\"') + '"'


def union(name: str, members: list[str]) -> list[str]:
    """A union alias, one member per line when it does not fit on one."""
    one = f"{name}: TypeAlias = {' | '.join(members)}"
    if len(one) <= WIDTH:
        return [one]
    inner = f"    {' | '.join(members)}"
    if len(inner) <= WIDTH:
        return [f"{name}: TypeAlias = (", inner, ")"]
    return [f"{name}: TypeAlias = (", *[f"    {m}" if i == 0 else f"    | {m}" for i, m in enumerate(members)], ")"]


def render(schemas: Schemas, file: str, name: str, found: tuple[Variant, ...]) -> str:
    """One family's module."""
    home = module_of(name)
    rule = schemas.rules[file][name]
    kind_rule = schemas.rules[file].get(f"{name}-kind")
    kinds = tuple(v.kind for v in found)
    if kind_rule is None or schemas.literals(file, kind_rule.name) != kinds:
        msg = f"{file}: {name}-kind does not list {name}'s kinds in order"
        raise SystemExit(msg)
    needed: set[str] = set()
    classes: list[str] = []
    for variant in found:
        annotations = []
        for f in variant.fields:
            annotation, modules = schemas.python_type(file, f.type, home)
            needed |= modules
            annotations.append((snake(f.name), f"{annotation} | None" if f.optional else annotation, f.doc))
        lines = ["", "", "", "@final", "@dataclass(frozen=True, slots=True)", f"class {variant.kind}:"]
        lines += docstring(variant.doc, "    ", [(n, d) for n, _, d in annotations] or None)
        lines.append("")
        lines += [f"    {n}: {a}" for n, a, _ in annotations]
        lines.append(f'    kind: Literal["{variant.kind}"] = field(default="{variant.kind}", init=False)')
        classes.append("\n".join(lines))
    aliases = [
        literal(camel(target[1]), schemas.literals(*target) or ())
        for target, module in schemas.aliases.items()
        if module == home
    ]
    header = [
        f'"""GENERATED by lang/python/tools/gen_errors.py from shared/schemas/{file}.cddl (`{name}`). Do not edit.',
        "",
        *textwrap.wrap((rule.doc or "An error family.").replace("\\", "\\\\"), width=WIDTH),
        '"""',
        "",
        "from dataclasses import dataclass, field",
        "from typing import Literal, TypeAlias, final",
    ]
    imports = sorted(m for m in needed if m != "preview")
    if "preview" in needed:
        header += ["", "from dilf4s._preview import Preview"]
    if imports:
        header += ([] if "preview" in needed else [""]) + [f"from dilf4s.errors import {', '.join(imports)}"]
    body = [*header]
    for alias in aliases:
        body += ["", *alias]
    body += ["", *literal(f"{camel(name)}Kind", kinds)]
    out = "\n".join(body) + "".join(classes) + "\n\n\n"
    out += "\n".join(union(camel(name), list(kinds))) + "\n"
    return out


def render_init(schemas: Schemas) -> str:
    """The package: every family's module, by the family's schema name."""
    modules = sorted(module_of(name) for _, name in schemas.families)
    lines = [
        '"""GENERATED by lang/python/tools/gen_errors.py from shared/schemas/*-errors.cddl. Do not edit.',
        "",
        "The error families of the schemas, one module each: a frozen dataclass per kind, the family's `Kind`",
        "literal and the family as the union of its kinds. A refusal names its family by module, as",
        "`canonical.TypeMismatch`.",
        '"""',
        "",
        "from types import ModuleType",
        "from typing import Final",
        "",
        "from dilf4s.errors import (",
        *[f"    {m}," for m in modules],
        ")",
        "",
        "FAMILIES: Final[dict[str, ModuleType]] = {",
        *[f'    "{name}": {module_of(name)},' for _, name in sorted(schemas.families, key=lambda k: k[1])],
        "}",
        '"""Every family\'s module, by the family\'s schema name."""',
        "",
        "__all__ = [",
        *[f"    {repr_text(m)}," for m in ["FAMILIES", *modules]],
        "]",
    ]
    return "\n".join(lines) + "\n"


def outputs() -> dict[Path, str]:
    """Every file the generator writes, and its text."""
    schemas = Schemas()
    files = {OUT / f"{module_of(name)}.py": render(schemas, f, name, v) for (f, name), v in schemas.families.items()}
    files[OUT / "__init__.py"] = render_init(schemas)
    return files


def main() -> int:
    """Writes the modules, or with `--check` reports any that differ."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true", help="fail if a generated file is stale")
    args = parser.parse_args()
    files = outputs()
    stale = sorted(p.name for p, text in files.items() if not p.exists() or p.read_text(encoding="utf-8") != text)
    extra = sorted(p.name for p in OUT.glob("*.py") if p not in files)
    if args.check:
        for name in stale + extra:
            print(f"stale: python/dilf4s/errors/{name}", file=sys.stderr)
        return 1 if stale or extra else 0
    OUT.mkdir(parents=True, exist_ok=True)
    for path, text in files.items():
        path.write_text(text, encoding="utf-8")
    for name in extra:
        (OUT / name).unlink()
    return 0


if __name__ == "__main__":
    sys.exit(main())
