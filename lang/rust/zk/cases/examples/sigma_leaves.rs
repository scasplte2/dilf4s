//! Issue #33, natively: each sigma leaf commitment of `sigma_v1.json` as two scalar multiplications and as one k256
//! linear combination, timed. Run with `cargo run --release --features vectors --example sigma_leaves`.

#![allow(
    clippy::print_stdout,
    clippy::std_instead_of_core,
    clippy::as_conversions,
    clippy::cast_precision_loss,
    reason = "a benchmark that reports by printing"
)]

use std::hint::black_box;
use std::time::Instant;

use dilf4s_zk_cases::leaves::{Prepared, encode, lincomb, prepare, two_muls};
use dilf4s_zk_cases::vectors::leaves;
use k256::ProjectivePoint;

const ROUNDS: u32 = 2000;

/// The mean time per leaf, in microseconds, over `ROUNDS` passes.
fn per_leaf(leaves: &[Prepared], f: fn(&[Prepared]) -> Vec<ProjectivePoint>) -> f64 {
    let start = Instant::now();
    for _ in 0..ROUNDS {
        black_box(f(black_box(leaves)));
    }
    start.elapsed().as_secs_f64() * 1e6 / (f64::from(ROUNDS) * leaves.len() as f64)
}

fn main() -> Result<(), String> {
    let (generator, other) = leaves()?;
    for (label, raw) in [("generator (dlog)", generator), ("other (DH tuple)", other)] {
        let prepared = prepare(&raw)?;
        if encode(&two_muls(&prepared)) != encode(&lincomb(&prepared)) {
            return Err(format!("{label}: the two forms disagree"));
        }
        // warm up, then alternate so neither form is favoured by frequency scaling
        per_leaf(&prepared, two_muls);
        let (a1, b1, a2, b2) = (
            per_leaf(&prepared, two_muls),
            per_leaf(&prepared, lincomb),
            per_leaf(&prepared, two_muls),
            per_leaf(&prepared, lincomb),
        );
        let (a, b) = (f64::midpoint(a1, a2), f64::midpoint(b1, b2));
        println!(
            "{label}: {} leaves, two muls {a:.1} us/leaf, lincomb {b:.1} us/leaf, lincomb/two muls {:.3}",
            prepared.len(),
            b / a
        );
    }
    Ok(())
}
