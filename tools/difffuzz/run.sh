#!/usr/bin/env bash
# The JLVM differential fuzzing harness, in one command (tools/difffuzz/README.md): builds the Rust runner (release)
# and the TypeScript packages, compiles the Scala driver, and runs it with the given options from the repository root.
#
#   tools/difffuzz/run.sh --seed 42 --cases 20000
#
# DIFFFUZZ_PREBUILT=1 skips the builds and runs on what CI's build jobs left: the Rust binary in place, the TypeScript
# packages' dist, and the Scala classpath bundled by bundle.sh in target/difffuzz-bundle.
set -euo pipefail

root=$(cd "$(dirname "$0")/../.." && pwd)
cd "$root"

if [[ "${DIFFFUZZ_PREBUILT:-0}" == 1 ]]; then
  classpath=$(tr ':' '\n' < target/difffuzz-bundle/classpath | sed "s#^#$root/target/difffuzz-bundle/#" | paste -sd ':' -)
else
  cargo build --release --quiet --manifest-path sdk/rust/Cargo.toml -p dilf4s-difffuzz
  [[ -d sdk/ts/node_modules ]] || npm ci --prefix sdk/ts --silent
  npm run build --prefix sdk/ts --silent
  classpath=$(sbt -batch -error "export jlvm/Test/fullClasspath" | tail -n 1)
fi

exec java -Xss8m -Xmx4g -cp "$classpath" xyz.kd5ujc.jlvm.difffuzz.DiffFuzz "$@"
