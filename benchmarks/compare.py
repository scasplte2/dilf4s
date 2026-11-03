#!/usr/bin/env python3
"""Measured gas against a schedule's charges: one row per case, worst backend first.

Joins `ratios.py`'s table (the measured gas per case and backend) with one or two `GasRatios check` outputs (what a
schedule charges each case), and flags every case charged less than it costs (UNDER) or more than twice (over2x).

usage: compare.py <ratios table> <check output (new schedule)> [check output (old schedule)]
"""
import sys


def charges(path):
    out = {}
    for line in open(path):
        f = line.rstrip('\n').split('\t')
        if len(f) == 3 and f[1].lstrip('-').isdigit():
            out[f[0]] = (int(f[1]), f[2])
    return out


def main(table, new, old=None):
    rows = [l.rstrip('\n').split('\t') for l in open(table) if not l.startswith('#')]
    ix = {h: i for i, h in enumerate(rows[0])}
    now = charges(new)
    before = charges(old) if old else {}
    print('\t'.join(['name', 'jvm_gas', 'rust_gas', 'worst', 'old', 'new', 'worst/new', 'flags']))
    for r in rows[1:]:
        name = r[0]
        if name not in now:
            continue
        def gas(k):
            v = r[ix[k]]
            return float(v) if v not in ('-', '') else 0.0
        worst = max(gas('jvm_gas'), gas('rust_gas'))
        c, outcome = now[name]
        flags = [f for f in r[ix['flags']].split(',') if 'unconverged' in f or 'spread' in f]
        ratio = worst / c if c > 0 else float('inf')
        if outcome != 'ok':
            flags.append(outcome)
        if ratio > 1.0:
            flags.append('UNDER')
        if 0 < ratio < 0.5:
            flags.append('over2x')
        o = str(before[name][0]) if name in before else '-'
        print('\t'.join([name, r[ix['jvm_gas']], r[ix['rust_gas']], f'{worst:.0f}', o, str(c), f'{ratio:.2f}', ','.join(flags)]))


if __name__ == '__main__':
    main(*sys.argv[1:])
