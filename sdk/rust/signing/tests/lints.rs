//! `Cargo.toml` copies the workspace's lint table so it can lower `unsafe_code` from "forbid" to "deny" (for the
//! SP1 guest's `sys_bigint` module). Everything else must stay the workspace's.

#![allow(
    clippy::unwrap_used,
    clippy::std_instead_of_alloc,
    clippy::std_instead_of_core,
    reason = "tests fail by panicking, and run with std"
)]

/// The lint lines of `manifest` under section headers starting `[{prefix}`, with that prefix written `[lints.`,
/// comments and blank lines dropped.
fn lint_lines(manifest: &str, prefix: &str) -> Vec<String> {
    let mut section = false;
    manifest
        .lines()
        .map(str::trim)
        .filter_map(|line| {
            if line.starts_with('[') {
                section = line.starts_with(&format!("[{prefix}"));
                return section.then(|| line.replacen(prefix, "lints.", 1));
            }
            (section && !line.is_empty() && !line.starts_with('#')).then(|| line.to_owned())
        })
        .collect()
}

#[test]
fn the_lint_table_is_the_workspaces_but_for_unsafe_code() {
    let dir = env!("CARGO_MANIFEST_DIR");
    let workspace = std::fs::read_to_string(format!("{dir}/../Cargo.toml")).unwrap();
    let crate_ = std::fs::read_to_string(format!("{dir}/Cargo.toml")).unwrap();
    let ws = lint_lines(&workspace, "workspace.lints.");
    let ours = lint_lines(&crate_, "lints.");
    assert!(ws.contains(&r#"unsafe_code = "forbid""#.to_owned()), "the workspace forbids unsafe code: {ws:?}");
    assert!(ours.contains(&r#"unsafe_code = "deny""#.to_owned()), "this crate denies it: {ours:?}");
    let other =
        |lines: &[String]| -> Vec<String> { lines.iter().filter(|l| !l.starts_with("unsafe_code")).cloned().collect() };
    assert_eq!(other(&ours), other(&ws), "every other lint is the workspace's");
    assert!(ws.len() > 20, "the workspace's lints were found: {ws:?}");
}
