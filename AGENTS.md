# Repository conventions

- Keep language implementations and bindings under `lang/<language>`, with their language-specific build files and tests. Scala's sbt build owns Java compilation, with Java sources and build outputs under `lang/java`.
- Keep Lean and its build under `formal`, shared protocol assets under `shared`, and cross-language executables under `tools`. Root configuration must serve the repository as a whole.
- Scala, Rust, TypeScript and Go implement protocol behavior independently. Java binds Scala; Python binds Rust. Coverage gaps belong in `docs/guides/parity.md`, not in an implicit weaker definition of an implementation.
- Use exact corpus counts or exact case-name sets, and assert every expected value and refusal. Never weaken coverage assertions to minimum counts to avoid rebase conflicts. Explicitly account for cases a specialized harness cannot execute, with their expected refusal and an exact excluded set/count.
- Preserve checks that generated sources and shared vectors equal their generators exactly. Moving a generator requires updating its output paths, fixtures and regeneration commands together.
- Use `tools/sbt` from the repository root or `sbt` inside `lang/scala`. Native Rust commands run in `lang/rust/native`; the patched guest has a separate workspace at `lang/rust`. Keep native and SP1 dependency graphs separate.
- A guest-path change can change its ELF and verifying key. Run the SP1 key and proof checks; never loosen or remove the pinned-key assertion to make a layout change pass.
- Keep investigation notes and handoffs in ignored `.workspace/`, never in tracked source or references from tracked documentation.
