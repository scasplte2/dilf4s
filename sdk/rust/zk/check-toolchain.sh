#!/usr/bin/env bash
# Fails unless the SP1 toolchain is the pinned one: cargo-prove v6.10.0 (commit d83ed91), whose `install-toolchain`
# installs succinct-1.96.0-64bit-v3 (rustc 1.96.0-dev), and a rustc that knows riscv64im-succinct-zkvm-elf.
# Install it with the v6.10.0 sp1up: sp1up --version v6.10.0
set -euo pipefail

prove=$(cargo prove --version)
case "$prove" in
  *"(d83ed91 "*) ;;
  *) echo "cargo prove is not v6.10.0 (d83ed91): $prove" >&2; exit 1 ;;
esac

rustc_version=$(rustc +succinct --version)
case "$rustc_version" in
  "rustc 1.96.0-dev"*) ;;
  *) echo "the succinct toolchain is not succinct-1.96.0-64bit-v3 (rustc 1.96.0-dev): $rustc_version" >&2; exit 1 ;;
esac

rustc +succinct --print target-list | grep -qx riscv64im-succinct-zkvm-elf \
  || { echo "the succinct toolchain has no riscv64im-succinct-zkvm-elf target" >&2; exit 1; }

echo "$prove; $rustc_version"
