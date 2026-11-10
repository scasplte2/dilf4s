#!/usr/bin/env bash
# Isolated maximum-shape unique/intersect measurements; see README.md.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
cd "$root"
out=${1:?usage: tools/gas/dedup.sh OUT KIND COUNT [ORDER] [ROUNDS] [PROCESSES]}
kind=${2:?int, short or prefix}
count=${3:?number of input values}
order=${4:-permuted}
rounds=${5:-101}
processes=${6:-3}
cpu=${CPU:-0}
mkdir -p "$out"
tools/sbt -batch 'export benchmarks/Test/fullClasspath' > "$out/build-scala.log" 2>&1
if grep -q '^\[error\]' "$out/build-scala.log"; then cat "$out/build-scala.log" >&2; exit 1; fi
tail -1 "$out/build-scala.log" > "$out/classpath"
(cd lang/rust/native && cargo build --locked --release -p dilf4s-jlvm --example gas_ratios)
java -Xmx4g -cp "$(cat "$out/classpath")" xyz.kd5ujc.benchmarks.GasRatios export "$out/all-cases.tsv"
python3 - "$out" "$kind" "$count" "$order" <<'INPUTS'
import json
import sys
from pathlib import Path
out, kind, count, order = sys.argv[1:]
root = Path(out)
n = int(count)
assert kind in ('int', 'short', 'prefix')
assert order in ('ascending', 'descending', 'permuted', 'repeated')
assert 0 < n < 2**24 and n % 104729 != 0
anchors = '\n'.join((root / 'all-cases.tsv').read_text().splitlines()[:2]) + '\n'
for op in ('unique', 'intersect'):
    path = root / op
    path.mkdir(exist_ok=True)
    args = [{'var': 'a0'}] if op == 'unique' else [{'var': 'a0'}, {'var': 'a1'}]
    row = [f'dedup/{kind}-{n}-{order}-{op}', op, '0', json.dumps({op: args}, separators=(',', ':')),
           json.dumps({'var': 'a2' if order == 'repeated' and op == 'unique' else 'a0'}, separators=(',', ':')), f'dedup:{kind}:{n}:{order}']
    (path / 'cases.tsv').write_text(anchors + '\t'.join(row) + '\n')
INPUTS
reference=(-Xms4g -Xmx4g -XX:+UseSerialGC -XX:+AlwaysPreTouch)
runner=lang/rust/native/target/release/examples/gas_ratios
for op in unique intersect; do
  dir="$out/$op"
  taskset -c "$cpu" java "${reference[@]}" -cp "$(cat "$out/classpath")" xyz.kd5ujc.benchmarks.GasRatios check "$dir/cases.tsv" > "$dir/jvm-check.log"
  taskset -c "$cpu" "$runner" "$dir/cases.tsv" check > "$dir/rust-check.log"
  for log in "$dir/jvm-check.log" "$dir/rust-check.log"; do
    awk -F '\t' 'NF != 3 || $3 != "ok" { bad = 1 } END { exit bad || NR != 3 }' "$log"
  done
  # Both checks assert the exact dedup outcome: first occurrences for unique, true for intersect.
  for ((i = 1; i <= processes; i++)); do
    taskset -c "$cpu" java "${reference[@]}" "-Xlog:gc:file=$dir/jvm-$i.gc" -cp "$(cat "$out/classpath")" xyz.kd5ujc.benchmarks.GasRatios measure "$rounds" '' "$dir/cases.tsv" > "$dir/jvm-$i.tsv"
    taskset -c "$cpu" "$runner" "$dir/cases.tsv" "$rounds" > "$dir/rust-$i.tsv"
  done
  python3 tools/gas/ratios.py "$dir" > "$dir/summary.tsv"
done
