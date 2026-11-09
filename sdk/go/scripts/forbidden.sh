#!/usr/bin/env bash
# Fails when library code (every .go file but tests) uses what the Go SDK forbids: unsafe, reflect, cgo, panic,
# recover, goroutines, or a package-level variable (mutable state). Run from sdk/go. A test file may panic; a library
# file never does (docs/sdk/porting.md section 4).
set -euo pipefail

mapfile -t files < <(find . -name '*.go' ! -name '*_test.go' | sort)
if [ "${#files[@]}" -eq 0 ]; then
  echo "no library files found: run from sdk/go" >&2
  exit 1
fi

status=0
check() {
  local what="$1" pattern="$2"
  local hits
  # comment lines are prose, which may say "go" or "panic"
  hits="$(grep -nE "$pattern" "${files[@]}" | grep -vE '^[^:]+:[0-9]+:[[:space:]]*//' || true)"
  if [ -n "$hits" ]; then
    echo "forbidden in library code: $what" >&2
    echo "$hits" >&2
    status=1
  fi
}

check 'the unsafe package' '"unsafe"'
check 'the reflect package' '"reflect"'
check 'cgo' '^import "C"|^[[:space:]]*"C"$'
check 'panic' '(^|[^A-Za-z0-9_."])panic\('
check 'recover' '(^|[^A-Za-z0-9_."])recover\('
check 'a goroutine' '(^|[[:space:]])go [A-Za-z_(]'
check 'a package-level variable' '^var[[:space:](]'

exit "$status"
