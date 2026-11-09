#!/usr/bin/env bash
# Runs every Fuzz target of the module for a short fixed time (FUZZTIME, default 20s), one invocation each, as
# `go test -fuzz` takes one target at a time. Each is seeded from the shared vectors, so a plain `go test` replays
# the seeds too; a failure leaves its input under the package's testdata/fuzz. Run from sdk/go.
set -euo pipefail

fuzztime="${FUZZTIME:-20s}"
# the seeds are whole vector files, so minimizing each new input (60s by default) would take most of a short run
minimizetime="${FUZZMINIMIZETIME:-2s}"
mapfile -t packages < <(go list ./...)
ran=0
for pkg in "${packages[@]}"; do
  mapfile -t targets < <(go test -list '^Fuzz' "$pkg" | grep '^Fuzz' || true)
  for target in "${targets[@]}"; do
    echo "fuzzing $pkg $target for $fuzztime"
    go test -run '^$' -fuzz "^${target}\$" -fuzztime "$fuzztime" -fuzzminimizetime "$minimizetime" "$pkg"
    ran=$((ran + 1))
  done
done
if [ "$ran" -eq 0 ]; then
  echo "no Fuzz targets found" >&2
  exit 1
fi
echo "fuzzed $ran targets"
