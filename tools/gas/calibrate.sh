#!/usr/bin/env bash
# The gas calibration as the schedule's prices are derived (docs/specs/jlvm.md section 7): every case of GasCases,
# timed by both node harnesses in separate processes, one at a time, each pinned to one CPU. The JVM runs in the
# reference configuration (GasRatios.Reference), which GasRatios checks before it measures.
#
# usage: tools/gas/calibrate.sh <out dir> [jvm processes (10)] [rust processes (3)] [rounds (101)] [regex/substrings]
#   CPU=<n> picks the CPU (default 0); run it from the repository root, on a quiet machine. Then:
#   python3 tools/gas/ratios.py <out dir> > table.tsv
set -euo pipefail

out=${1:?usage: tools/gas/calibrate.sh <out dir> [jvm processes] [rust processes] [rounds] [filter]}
jvms=${2:-10}
rusts=${3:-3}
rounds=${4:-101}
filter=${5:-}
cpu=${CPU:-0}
reference=(-Xms4g -Xmx4g -XX:+UseSerialGC -XX:+AlwaysPreTouch)

mkdir -p "$out"
tools/sbt -batch "export benchmarks/Test/fullClasspath" | tail -1 > "$out/classpath"
java -Xmx4g -cp "$(cat "$out/classpath")" xyz.kd5ujc.benchmarks.GasRatios export "$out/cases.tsv"
(cd lang/rust/native && cargo build --release -p dilf4s-jlvm --example gas_ratios)

for ((i = 1; i <= (jvms > rusts ? jvms : rusts); i++)); do
  if ((i <= jvms)); then
    taskset -c "$cpu" java "${reference[@]}" "-Xlog:gc:file=$out/jvm-$i.gc" -cp "$(cat "$out/classpath")" \
      xyz.kd5ujc.benchmarks.GasRatios measure "$rounds" "$filter" > "$out/jvm-$i.tsv"
    sleep 20
  fi
  if ((i <= rusts)); then
    taskset -c "$cpu" lang/rust/native/target/release/examples/gas_ratios "$out/cases.tsv" "$rounds" "$filter" > "$out/rust-$i.tsv"
    sleep 20
  fi
done
echo "calibration done: $out"
