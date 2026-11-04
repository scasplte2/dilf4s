#!/usr/bin/env python3
"""Statistics for the JLVM gas calibration (docs/specs/jlvm.md section 7, "Calibration").

Reads the raw rounds that GasRatios (JVM) and gas_ratios (Rust) print, one file per process, named
`<backend>-<n>.tsv` (backend `jvm` or `rust`), and prints one row per case:

- per round, the case's ratio to the anchor is mean(case) / mean(anchor) over the palindrome, and the same against
  the reference (sha256 of 1024 bytes);
- rounds a cluster's MAD rejects are kept aside: when at least 3 of them agree (within 5%) above the accepted ratio,
  they are a minority mode (a slower core too rare to cluster), and the case is priced from the highest such group
  and left unconverged;
- a round is rejected when its two probes differ by more than 10% (the thread migrated or the clock moved inside it);
- rounds are clustered by probe time into two clusters (performance and efficiency cores), when the clusters' centres
  differ by more than 10%; within each cluster, rounds whose probe is more than 3 MADs from its median are rejected;
- per process and cluster, the median ratio and a bootstrap 95% interval of it; the case's ratio in a process is the
  HIGHER of its clusters', so no op is priced from the faster core type alone. A cluster of fewer than 15 rounds
  still counts towards that, but has no interval, which leaves the case unconverged;
- across processes, the HIGHEST (a process's heap and code layout moves allocation-heavy operations by up to tens of
  percent, and a node runs in whichever layout it gets), and the spread (max - min) / median;
- the cross-check: median(case/reference) x median(reference/anchor) against median(case/anchor).

A case is converged when its interval half-width is at most 5% (the widest over processes) and the cross-check agrees
within 5%; processes that disagree by more than 5% are flagged `spread`, and priced from the highest. Gas is ratio x
the anchor's own charge; the schedule's charge is beside it.

usage: ratios.py <dir with jvm-*.tsv and rust-*.tsv> [regex] | ratios.py --selftest
"""
import glob
import random
import re
import statistics
import sys
from collections import defaultdict

MIN_ROUNDS = 15
MIGRATION = 0.10


def median(xs):
    return statistics.median(xs)


def mad(xs):
    m = median(xs)
    return median([abs(x - m) for x in xs])


def bootstrap_ci(xs, n=400, seed=1):
    rnd = random.Random(seed)
    k = len(xs)
    meds = sorted(median([xs[rnd.randrange(k)] for _ in range(k)]) for _ in range(n))
    return meds[int(0.025 * n)], meds[int(0.975 * n) - 1]


def highest_mode(xs, band=1.05, support=3):
    """The median of the highest group of at least `support` values within a factor `band` of one another, or None."""
    s = sorted(xs, reverse=True)
    for i, top in enumerate(s):
        group = [x for x in s[i:] if x * band >= top]
        if len(group) >= support:
            return median(group)
    return None


def two_means(xs):
    """1-D k-means with k = 2 on log probe times: the split point, or None for one cluster."""
    s = sorted(xs)
    lo, hi = s[len(s) // 10], s[(9 * len(s)) // 10]
    if hi <= lo * 1.10:
        return None
    for _ in range(50):
        cut = (lo + hi) / 2
        a = [x for x in s if x <= cut]
        b = [x for x in s if x > cut]
        if not a or not b:
            return None
        nlo, nhi = statistics.fmean(a), statistics.fmean(b)
        if (nlo, nhi) == (lo, hi):
            break
        lo, hi = nlo, nhi
    return (lo + hi) / 2 if hi > lo * 1.10 else None


def load(path):
    meta, rounds, anchor = {}, defaultdict(list), None
    for line in open(path):
        f = line.rstrip('\n').split('\t')
        if line.startswith('# anchor'):
            anchor = int(re.findall(r'anchor (\d+)', line)[0])
        elif f[0] == 'C':
            meta[f[1]] = dict(name=f[2], group=f[3], charged=int(f[4]))
        elif f[0] == 'S':
            p1, p2, a1, a2, x1, x2, r = map(float, f[4:11])
            rounds[f[1]].append((p1, p2, a1, a2, x1, x2, r))
    return anchor, meta, rounds


def process(path):
    """Per case: (ratio, ci half-width, cluster ratios, cross-check disagreement, rejected, accepted, op ns, anchor ns)."""
    anchor, meta, rounds = load(path)
    probes = [(p1 + p2) / 2 for rs in rounds.values() for (p1, p2, *_) in rs if abs(p1 - p2) <= MIGRATION * (p1 + p2) / 2]
    cut = two_means(probes) if len(probes) > 2 * MIN_ROUNDS else None
    clusters = {}
    for name, sel in (('fast', lambda p: cut is None or p <= cut), ('slow', lambda p: cut is not None and p > cut)):
        ps = [p for p in probes if sel(p)]
        if ps:
            clusters[name] = (sel, median(ps), mad(ps))
    out = {}
    for idx, rs in rounds.items():
        rejected = 0
        by = defaultdict(list)
        outliers = []  # rounds a cluster's MAD rejected: a stable minority mode must not vanish with them
        for (p1, p2, a1, a2, x1, x2, r) in rs:
            p = (p1 + p2) / 2
            if abs(p1 - p2) > MIGRATION * p:
                rejected += 1
                continue
            placed = False
            for name, (sel, m, d) in clusters.items():
                if sel(p):
                    placed = True
                    if abs(p - m) > 3 * 1.4826 * max(d, 1e-9):
                        rejected += 1
                        outliers.append((x1 + x2) / (a1 + a2))
                    else:
                        a, x = (a1 + a2) / 2, (x1 + x2) / 2
                        by[name].append((x / a, x / r, r / a, x, a))
            if not placed:
                rejected += 1
        per = {}
        for name, v in by.items():
            # a cluster with too few rounds still counts towards the highest ratio, but leaves the case unconverged
            ratios = [t[0] for t in v]
            m = median(ratios)
            lo, hi = bootstrap_ci(ratios) if len(v) >= MIN_ROUNDS else (float('-inf'), float('inf'))
            cross = median([t[1] for t in v]) * median([t[2] for t in v])
            per[name] = dict(ratio=m, half=(hi - lo) / 2 / abs(m) if m else float('inf'),
                             cross=abs(cross - m) / abs(m) if m else float('inf'), n=len(v),
                             x_ns=median([t[3] for t in v]), a_ns=median([t[4] for t in v]))
        if not per:
            out[idx] = None
            continue
        # rejected rounds that agree with one another and sit above the accepted ratio are a mode of their own (a slower
        # core too rare to cluster): the case is priced from them and left unconverged
        # the highest coherent group among them, so cheaper rejected rounds cannot mask an expensive mode
        mode = highest_mode(outliers)
        if mode is not None and mode > 1.05 * max(c['ratio'] for c in per.values()):
            per['rejected'] = dict(ratio=mode, half=float('inf'), cross=0.0, n=len(outliers),
                                   x_ns=float('nan'), a_ns=float('nan'))
        top = max(per.values(), key=lambda c: c['ratio'])
        # the widest interval and cross-check of any cluster: an undersampled cluster makes the whole case unconverged
        out[idx] = dict(meta=meta[idx], ratio=top['ratio'], half=max(c['half'] for c in per.values()),
                        cross=max(c['cross'] for c in per.values()),
                        clusters={k: v['ratio'] for k, v in per.items()}, rounds={k: v['n'] for k, v in per.items()}, rejected=rejected,
                        accepted=sum(c['n'] for c in per.values()), x_ns=top['x_ns'], a_ns=top['a_ns'])
    return anchor, out


def main(d, filt=''):
    results = defaultdict(dict)
    anchors = {}
    for be in ('jvm', 'rust'):
        runs = [process(p) for p in sorted(glob.glob(f'{d}/{be}-*.tsv'))]
        if not runs:
            continue
        anchors[be] = runs[0][0]
        for idx in set().union(*[r[1].keys() for r in runs]):
            per = [r[1][idx] for r in runs if r[1].get(idx)]
            if not per:
                continue
            ratios = [p['ratio'] for p in per]
            m = median(ratios)
            results[idx][be] = dict(
                meta=per[0]['meta'], ratio=max(ratios), spread=(max(ratios) - min(ratios)) / abs(m) if m else float('inf'),
                half=max(p['half'] for p in per), cross=median([p['cross'] for p in per]),
                clusters=per[0]['clusters'], rounds=per[0]['rounds'], rejected=sum(p['rejected'] for p in per),
                accepted=sum(p['accepted'] for p in per), procs=len(per),
                x_us=median([p['x_ns'] for p in per]) / 1000, a_us=median([p['a_ns'] for p in per]) / 1000)
    cols = ['name', 'group', 'charged']
    for be in ('jvm', 'rust'):
        cols += [f'{be}_gas', f'{be}_ci%', f'{be}_agree%', f'{be}_cross%', f'{be}_clusters', f'{be}_rej/acc', f'{be}_us']
    cols += ['worst', 'worst/charged', 'flags']
    print('# anchors ' + ', '.join(f'{k} {v}' for k, v in anchors.items()))
    print('\t'.join(cols))
    for idx in sorted(results, key=lambda i: int(i)):
        e = results[idx]
        meta = next(iter(e.values()))['meta']
        if filt and not re.search(filt, meta['name']):
            continue
        row = [meta['name'], meta['group'], str(meta['charged'])]
        worst, flags = 0.0, []
        for be in ('jvm', 'rust'):
            x = e.get(be)
            if not x:
                row += ['-'] * 7
                continue
            g = x['ratio'] * anchors[be]
            worst = max(worst, g)
            cl = '/'.join(f"{k}:{v * anchors[be]:.0f}({x['rounds'][k]})" for k, v in sorted(x['clusters'].items()))
            row += [f'{g:.0f}', f"{100 * x['half']:.1f}", f"{100 * x['spread']:.1f}", f"{100 * x['cross']:.1f}", cl,
                    f"{x['rejected']}/{x['accepted']}", f"{x['x_us']:.3f}"]
            if x['procs'] < 3:
                flags.append(f'{be}:procs={x["procs"]}')
            if x['half'] > 0.05 or x['cross'] > 0.05:
                flags.append(f'{be}:unconverged')
            if x['spread'] > 0.05:
                flags.append(f'{be}:spread')
        c = meta['charged']
        r = worst / c if c > 0 else float('inf')
        row += [f'{worst:.0f}', f'{r:.2f}']
        if r > 1.0:
            flags.append('UNDER')
        if r < 0.5:
            flags.append('over2x')
        row.append(','.join(flags))
        print('\t'.join(row))


def selftest():
    """Rare slow modes that MAD rejects must price the case and leave it unconverged:
    - 100 fast rounds at ratio 1 and 5 slow ones at ratio 3;
    - the same, with 8 more rejected rounds at ratio 1, which must not mask the 5 at ratio 3."""
    import tempfile
    rnd = random.Random(7)

    def case(shape):
        with tempfile.TemporaryDirectory() as d:
            path = f'{d}/jvm-1.tsv'
            with open(path, 'w') as f:
                f.write('# anchor 6000; reference 1000\nC\t2\tcase\tg\t6000\t1\t1\n')
                for k, (probe, ratio) in enumerate(shape):
                    p = probe * (1 + rnd.uniform(-0.01, 0.01))
                    a = 30000 * (1 + rnd.uniform(-0.01, 0.01))
                    x = a * ratio
                    f.write(f'S\t2\t{k}\tAXA\t{p}\t{p}\t{a}\t{a}\t{x}\t{x}\t{a / 6}\n')
            return process(path)[1]['2']

    fast = [(180000, 1)] * 100
    for name, shape in (('5 slow', fast + [(360000, 3)] * 5), ('5 slow, 8 cheap outliers', fast + [(240000, 1)] * 8 + [(360000, 3)] * 5)):
        c = case(shape)
        assert c['ratio'] > 2.5, f'{name}: priced from the slow mode, got {c["ratio"]}'
        assert c['half'] == float('inf'), f'{name}: unconverged'
    print('selftest ok')


if __name__ == '__main__':
    if sys.argv[1:] == ['--selftest']:
        selftest()
    else:
        main(sys.argv[1], sys.argv[2] if len(sys.argv) > 2 else '')
