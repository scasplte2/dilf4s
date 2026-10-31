"""Generate sdk/rust/proofs/src/poseidon/constants.rs from Scala's checked-in PoseidonConstants.scala.

The Scala tables (the optimised form) are the source of truth. The base round constants are re-derived with the Grain
LFSR exactly as PoseidonDerivation.scala does; the script then checks PoseidonDerivation.digest over all six tables
against the circomlibjs digests pinned in PoseidonSuite.scala, and that the derived MDS matrix equals the table's.
Run from anywhere: python3 -I sdk/rust/proofs/tools/poseidon_gen.py
"""

import hashlib
import os
import re

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..", ".."))
SCALA = ROOT + "/core/src/main/scala/xyz/kd5ujc/core/hash/PoseidonConstants.scala"
OUT = ROOT + "/sdk/rust/proofs/src/poseidon/constants.rs"

R = 21888242871839275222246405745257275088548364400416034343698204186575808495617
FIELD_BITS = 254
FULL = 8
PINNED = {
    2: "d5e58f509cc3494297ac43fd51badea46b75bcc505b18cd31dffa274fe154149",
    3: "38e80297a0e0ecd5a20acc1cf519eb5eb898e924e2ae68808b49c7027e7e0b94",
    4: "6bc25738e42a0b5a18ca2b5d262ce7da4f3aa029bebf1090a08f4a466ce313b6",
    5: "fb21c20f55dc308123237c06f8838aa22772201c518746bb2ca699f0a4b7530e",
}


def numbers(text):
    return [int(x) for x in re.findall(r'BigInt\("(\d+)"\)', text)]


def parse():
    src = open(SCALA).read()
    blocks = src.split("val params: PoseidonParams = PoseidonParams(")[1:]
    widths = {}
    for b in blocks:
        t = int(re.search(r"t = (\d+)", b).group(1))
        rp = int(re.search(r"partialRounds = (\d+)", b).group(1))
        c = b.index("constants = Vector(")
        s = b.index("sparse = Vector(")
        m = b.index("mds = Vector(")
        p = b.index("preSparse = Vector(")
        constants = numbers(b[c:s])
        sparse = numbers(b[s:m])
        mds = numbers(b[m:p])
        pre = numbers(b[p:])
        assert len(constants) == 8 * t + rp, t
        assert len(sparse) == (2 * t - 1) * rp, t
        assert len(mds) == t * t and len(pre) == t * t, t
        widths[t] = dict(t=t, rp=rp, constants=constants, sparse=sparse, mds=mds, pre=pre)
    assert sorted(widths) == [2, 3, 4, 5]
    return widths


class Grain:
    def __init__(self, t, rp):
        def field(value, width):
            return [(value >> i) & 1 for i in range(width - 1, -1, -1)]

        self.reg = field(1, 2) + field(0, 4) + field(FIELD_BITS, 12) + field(t, 12) + field(FULL, 10) + field(rp, 10) + [1] * 30
        assert len(self.reg) == 80
        for _ in range(160):
            self.step()

    def step(self):
        b = self.reg
        new = b[62] ^ b[51] ^ b[38] ^ b[23] ^ b[13] ^ b[0]
        self.reg = b[1:] + [new]
        return new

    def bit(self):
        while True:
            a, b = self.step(), self.step()
            if a == 1:
                return b

    def bits(self):
        v = 0
        for _ in range(FIELD_BITS):
            v = (v << 1) | self.bit()
        return v

    def round_constant(self):
        while True:
            v = self.bits()
            if v < R:
                return v


def base(t, rp):
    g = Grain(t, rp)
    rc = [g.round_constant() for _ in range((FULL + rp) * t)]
    points = [g.bits() % R for _ in range(2 * t)]
    xs, ys = points[:t], points[t:]
    assert len(set(points)) == 2 * t
    mds = [pow((xs[i] + ys[j]) % R, R - 2, R) for i in range(t) for j in range(t)]
    return rc, mds


def digest(rc, w):
    tables = [rc, w["mds"], w["constants"], w["sparse"], w["mds"], w["pre"]]
    text = ";".join(",".join(str(x) for x in tab) for tab in tables)
    return hashlib.sha256(text.encode()).hexdigest()


def fr(x):
    assert 0 <= x < R
    return 'MontFp!("%d")' % x


def arr(xs, indent):
    pad = " " * indent
    return "[\n" + "".join("%s    %s,\n" % (pad, fr(x)) for x in xs) + pad + "]"


def mat(rows, indent):
    pad = " " * indent
    return "[\n" + "".join("%s    %s,\n" % (pad, arr(r, indent + 4)) for r in rows) + pad + "]"


def chunks(xs, n):
    return [xs[i : i + n] for i in range(0, len(xs), n)]


def emit(widths):
    out = [
        "//! GENERATED from core/src/main/scala/xyz/kd5ujc/core/hash/PoseidonConstants.scala (the optimised tables) and",
        "//! the Grain LFSR derivation of the base round constants (docs/specs/zk.md §3). Do not edit. The generator",
        "//! checks the circomlibjs digests pinned in PoseidonSuite.scala, and so do this crate's tests.",
        "",
        "use ark_ff::MontFp;",
        "",
        "use super::Params;",
        "",
    ]
    for t in [2, 3, 4, 5]:
        w = widths[t]
        rp = w["rp"]
        rc, mds = base(t, rp)
        assert mds == w["mds"], "derived MDS differs at width %d" % t
        got = digest(rc, w)
        assert got == PINNED[t], "width %d digest %s" % (t, got)
        c = w["constants"]
        sp = chunks(w["sparse"], 2 * t - 1)
        sparse_w = [row[:t] for row in sp]
        sparse_v = [[0] + row[t:] for row in sp]
        fields = [
            ("round_constants", mat(chunks(rc, t), 4)),
            ("mds", mat(chunks(w["mds"], t), 4)),
            ("first", arr(c[0:t], 4)),
            ("full_first", mat(chunks(c[t : 4 * t], t), 4)),
            ("opened", arr(c[4 * t : 5 * t], 4)),
            ("partial", arr(c[5 * t : 5 * t + rp], 4)),
            ("full_last", mat(chunks(c[5 * t + rp :], t), 4)),
            ("sparse_w", mat(sparse_w, 4)),
            ("sparse_v", mat(sparse_v, 4)),
            ("pre_sparse", mat(chunks(w["pre"], t), 4)),
        ]
        assert len(c[5 * t + rp :]) == 3 * t
        out.append("/// Width %d: %d input%s, %d partial rounds." % (t, t - 1, "" if t == 2 else "s", rp))
        out.append("pub static WIDTH%d: Params<%d, %d, %d> = Params {" % (t, t, rp, FULL + rp))
        for name, body in fields:
            out.append("    %s: %s," % (name, body))
        out.append("};")
        out.append("")
        print("width %d: digest %s ok" % (t, got))
    open(OUT, "w").write("\n".join(out))


emit(parse())
