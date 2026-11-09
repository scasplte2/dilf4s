# Rust

The independent Rust protocol implementation lives in `core`, `signing`, `proofs` and `jlvm`.

Run native builds from `native/` so rustup selects this language's pinned toolchain:

```sh
cd native
cargo test --locked --workspace
cargo clippy --locked --all-targets -- -D warnings
```

The Cargo manifest at `lang/rust/Cargo.toml` belongs to the SP1 guest. It contains every guest path dependency to
make its binary reproducible across checkout locations. Its patched dependencies must remain separate from the
native workspace. SP1 host and case harnesses have their own manifests; see [zk/README.md](zk/README.md).
