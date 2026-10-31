"""Converts Scala's checked-in Poseidon tables (core PoseidonConstants.scala) to a TypeScript module, and prints the
SHA-256 of the tables in the canonical text form the TS test recomputes.

Run from the repository root:
python3 -I sdk/ts/packages/proofs/tools/poseidon_tables.py core/src/main/scala/xyz/kd5ujc/core/hash/PoseidonConstants.scala \
  sdk/ts/packages/proofs/src/poseidon-constants.ts"""
import hashlib
import re
import sys

src_path, out_path = sys.argv[1], sys.argv[2]
src = open(src_path, encoding="utf-8").read()

widths = []
for m in re.finditer(r"private object PoseidonWidth(\d)\s*\{(.*?)\n\}", src, re.S):
    body = m.group(2)
    t = int(re.search(r"t = (\d+)", body).group(1))
    rp = int(re.search(r"partialRounds = (\d+)", body).group(1))

    def section(name, nxt):
        start = body.index(name + " = Vector(")
        end = body.index(nxt + " = Vector(") if nxt else len(body)
        return body[start:end]

    def flat(s):
        return [int(x) for x in re.findall(r'BigInt\("(\d+)"\)', s)]

    def matrix(s):
        rows = s.split("Vector(")[2:]
        return [flat(r) for r in rows]

    constants = flat(section("constants", "sparse"))
    sparse = flat(section("sparse", "mds"))
    mds = matrix(section("mds", "preSparse"))
    pre = matrix(section("preSparse", None))
    assert int(m.group(1)) == t
    assert len(constants) == 8 * t + rp, (t, len(constants))
    assert len(sparse) == (2 * t - 1) * rp, (t, len(sparse))
    assert len(mds) == t and all(len(r) == t for r in mds)
    assert len(pre) == t and all(len(r) == t for r in pre)
    widths.append((t, rp, constants, sparse, mds, pre))

assert [w[0] for w in widths] == [2, 3, 4, 5]


def canonical(w):
    t, rp, c, s, mds, pre = w
    parts = [str(t), str(rp), ",".join(map(str, c)), ",".join(map(str, s)),
             ",".join(map(str, [x for r in mds for x in r])), ",".join(map(str, [x for r in pre for x in r]))]
    return ";".join(parts)


text = "|".join(canonical(w) for w in widths)
digest = hashlib.sha256(text.encode()).hexdigest()

lines = [
    "// GENERATED from core/src/main/scala/xyz/kd5ujc/core/hash/PoseidonConstants.scala (the source of truth). Do not edit.",
    "// Poseidon over BN254 in the optimised form (docs/specs/zk.md section 3); test/poseidon.test.ts checks these tables",
    "// against the Scala file and pins their digest.",
    "",
    "/** One width's constants in the optimised form, laid out as Scala's `PoseidonParams`. */",
    "export interface PoseidonParams {",
    "  readonly t: number;",
    "  readonly partialRounds: number;",
    "  readonly constants: readonly bigint[];",
    "  readonly sparse: readonly bigint[];",
    "  readonly mds: readonly (readonly bigint[])[];",
    "  readonly preSparse: readonly (readonly bigint[])[];",
    "}",
    "",
    "export const FULL_ROUNDS = 8;",
    "",
]


def arr(xs, indent):
    pad = " " * indent
    return "[\n" + "".join(f"{pad}  {x}n,\n" for x in xs) + f"{pad}]"


for (t, rp, c, s, mds, pre) in widths:
    lines.append(f"export const WIDTH{t}: PoseidonParams = {{")
    lines.append(f"  t: {t},")
    lines.append(f"  partialRounds: {rp},")
    lines.append(f"  constants: {arr(c, 2)},")
    lines.append(f"  sparse: {arr(s, 2)},")
    lines.append("  mds: [")
    for r in mds:
        lines.append(f"    {arr(r, 4)},")
    lines.append("  ],")
    lines.append("  preSparse: [")
    for r in pre:
        lines.append(f"    {arr(r, 4)},")
    lines.append("  ],")
    lines.append("};")
    lines.append("")

open(out_path, "w", encoding="utf-8").write("\n".join(lines))
print(digest)
