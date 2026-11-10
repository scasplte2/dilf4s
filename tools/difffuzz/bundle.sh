#!/usr/bin/env bash
# Copies a JVM classpath (the line `tools/sbt "export jlvm/Test/fullClasspath"` prints, read from standard input) into one
# directory, entry by entry, with a `classpath` file of the entries' paths relative to it: what CI's Scala job hands
# the difffuzz job, so the driver runs there without tools/sbt or a second compilation (tools/difffuzz/README.md).
#
#   tools/sbt -batch -error "export jlvm/Test/fullClasspath" | tail -n 1 | tools/difffuzz/bundle.sh target/difffuzz-bundle
set -euo pipefail

out=$1
rm -rf "$out"
mkdir -p "$out"
IFS=':' read -r -a entries
relative=()
for i in "${!entries[@]}"; do
  entry=${entries[$i]}
  name=$(printf '%03d-%s' "$i" "$(basename "$entry")")
  if [[ -d "$entry" ]]; then
    cp -r "$entry" "$out/$name"
  elif [[ -f "$entry" ]]; then
    cp "$entry" "$out/$name"
  else
    continue
  fi
  relative+=("$name")
done
(IFS=':'; printf '%s\n' "${relative[*]}") > "$out/classpath"
echo "bundled ${#relative[@]} classpath entries into $out"
