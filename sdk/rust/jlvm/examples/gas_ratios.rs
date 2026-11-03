//! The Rust half of the gas calibration (`benchmarks/.../GasRatios.scala`, `docs/specs/jlvm.md` section 7): the same
//! cases (from `GasRatios export <file>`) and the same rounds, printed in the same raw form for `benchmarks/ratios.py`.
//!
//! A round is `probe, s1, s2, reference, s2, s1, probe`, with `(s1, s2)` either (anchor, case) or (case, anchor) at
//! random: a palindrome, so drift linear across the round cancels in every ratio. Anchor, case and reference are each
//! timed net of their baselines; the probe is a fixed xorshift64 chain, the core's speed during the round.
//!
//! `cargo run --release -p dilf4s-jlvm --example gas_ratios -- <cases file> [rounds | check] [substrings, |-separated]`

#![allow(
    clippy::arithmetic_side_effects,
    clippy::as_conversions,
    clippy::cast_precision_loss,
    clippy::indexing_slicing,
    clippy::panic,
    clippy::print_stdout,
    clippy::float_arithmetic,
    clippy::many_single_char_names,
    reason = "a benchmark harness: timing arithmetic on measured floats, and it fails by panicking"
)]

extern crate alloc;

use alloc::collections::BTreeMap;
use std::time::Instant;

use dilf4s_jlvm::{Compiled, Config, Gas, GasLimit, GasSchedule, Jlvm, Native, Value, parse, parse_value};

/// Each batch of a unit takes at least this many nanoseconds (`GasRatios.Target`).
const TARGET: f64 = 2.5e5;
/// Iterations of the probe (`GasRatios.ProbeSteps`).
const PROBE_STEPS: u32 = 1 << 17;

struct Case {
    name: String,
    group: String,
    adjust: u64,
    program: Compiled,
    baseline: Compiled,
    data: Value,
}

fn case(line: &str) -> Case {
    let f: Vec<&str> = line.split('\t').collect();
    assert!(f.len() == 6, "a case has six fields: {}", f.first().unwrap_or(&""));
    let compile = |text: &str| {
        Compiled::new(parse(text).unwrap_or_else(|e| panic!("{}: {e}", f[0]))).unwrap_or_else(|e| panic!("{e}"))
    };
    Case {
        name: f[0].to_owned(),
        group: f[1].to_owned(),
        adjust: f[2].parse().unwrap_or_else(|e| panic!("{e}")),
        program: compile(f[3]),
        baseline: compile(f[4]),
        data: parse_value(f[5]).unwrap_or_else(|e| panic!("{}: {e}", f[0])),
    }
}

fn xorshift(steps: u32, seed: u64) -> u64 {
    (0..steps).fold(seed, |x, _| {
        let a = x ^ (x << 13);
        let b = a ^ (a >> 7);
        b ^ (b << 17)
    })
}

struct Bench {
    jlvm: Jlvm<Native>,
    env: BTreeMap<String, Value>,
    config: Config,
}

/// A case with its batch sizes, timed as one net unit: the program less its baseline.
struct Timed<'c> {
    c: &'c Case,
    n: u64,
    n_base: u64,
}

impl Bench {
    fn gas(&self, c: &Case, p: &Compiled) -> u64 {
        self.jlvm.evaluate_compiled(p, &c.data, &self.env, &self.config).gas_used.value()
    }

    fn charged(&self, c: &Case) -> u64 {
        self.gas(c, &c.program) - self.gas(c, &c.baseline) + c.adjust
    }

    /// Nanoseconds per evaluation over a batch of `n`.
    fn batch(&self, c: &Case, p: &Compiled, n: u64) -> f64 {
        let start = Instant::now();
        let sink = (0..n).fold(0u64, |s, _| s.wrapping_add(self.gas(c, p)));
        let ns = start.elapsed().as_nanos() as f64;
        core::hint::black_box(sink);
        ns / n as f64
    }

    fn probe() -> f64 {
        let start = Instant::now();
        core::hint::black_box(xorshift(core::hint::black_box(PROBE_STEPS), 0x9e37_79b9_7f4a_7c15));
        start.elapsed().as_nanos() as f64
    }

    /// The batch size that takes at least `target` nanoseconds, doubling from 1.
    fn size(&self, c: &Case, p: &Compiled, target: f64) -> u64 {
        let mut n = 1u64;
        while self.batch(c, p, n) * (n as f64) < target && n < (1 << 24) {
            n *= 2;
        }
        n
    }

    fn timed<'c>(&self, c: &'c Case) -> Timed<'c> {
        Timed { c, n: self.size(c, &c.program, TARGET), n_base: self.size(c, &c.baseline, TARGET) }
    }

    fn net(&self, t: &Timed<'_>) -> f64 {
        self.batch(t.c, &t.c.program, t.n) - self.batch(t.c, &t.c.baseline, t.n_base)
    }

    /// One round: probe1, probe2, anchor1, anchor2, case1, case2, reference (ns).
    fn round(&self, a: &Timed<'_>, x: &Timed<'_>, r: &Timed<'_>, anchor_first: bool) -> [f64; 7] {
        let (s1, s2) = if anchor_first { (a, x) } else { (x, a) };
        let p1 = Self::probe();
        let u1 = self.net(s1);
        let u2 = self.net(s2);
        let rf = self.net(r);
        let v2 = self.net(s2);
        let v1 = self.net(s1);
        let p2 = Self::probe();
        let ((a1, a2), (x1, x2)) = if anchor_first { ((u1, v1), (u2, v2)) } else { ((u2, v2), (u1, v1)) };
        [p1, p2, a1, a2, x1, x2, rf]
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let path = args.get(1).unwrap_or_else(|| panic!("usage: gas_ratios <cases file> [rounds | check] [substring]"));
    let check = args.get(2).is_some_and(|a| a == "check");
    let rounds: usize =
        if check { 0 } else { args.get(2).map_or(101, |r| r.parse().unwrap_or_else(|e| panic!("{e}"))) };
    let filter = args.get(3).cloned().unwrap_or_default();
    let text = std::fs::read_to_string(path).unwrap_or_else(|e| panic!("{path}: {e}"));
    let cases: Vec<Case> = text.lines().filter(|l| !l.is_empty()).map(case).collect();
    let b = Bench {
        jlvm: Jlvm::new(Native::new().unwrap_or_else(|e| panic!("{e:?}"))),
        env: BTreeMap::new(),
        config: Config { limit: GasLimit(Gas::new(u64::MAX / 4)), schedule: GasSchedule::V1 },
    };
    // what each case is charged, and what its program evaluates to: a case that fails early times no operation
    if check {
        for c in &cases {
            let o = b.jlvm.evaluate_compiled(&c.program, &c.data, &b.env, &b.config);
            let result = o.result.as_ref().map_or_else(|e| format!("error {}", e.kind()), |_| String::from("ok"));
            println!("{}\t{}\t{result}", c.name, b.charged(c));
        }
        return;
    }
    let [anchor, reference, rest @ ..] = cases.as_slice() else { panic!("the anchor and the reference come first") };
    let chosen: Vec<(usize, &Case)> = rest
        .iter()
        .enumerate()
        .map(|(i, c)| (i + 2, c))
        .filter(|(_, c)| filter.split('|').any(|f| c.name.contains(f)))
        .collect();
    println!("# backend Rust; {} cases; {rounds} rounds; probe {PROBE_STEPS} steps", chosen.len());
    println!("# anchor {}; reference {}", b.charged(anchor), b.charged(reference));
    for c in [anchor, reference].into_iter().chain(chosen.iter().map(|(_, c)| *c)) {
        b.size(c, &c.program, 5e7);
        b.size(c, &c.baseline, 5e7);
    }
    let a = b.timed(anchor);
    let r = b.timed(reference);
    let mut state: u64 = 0x2545_f491_4f6c_dd1d;
    for (i, c) in chosen {
        let x = b.timed(c);
        println!("C\t{i}\t{}\t{}\t{}\t{}\t{}", c.name, c.group, b.charged(c), x.n, x.n_base);
        for k in 0..rounds {
            state = xorshift(1, state);
            let anchor_first = (state >> 33) & 1 == 1;
            let v = b.round(&a, &x, &r, anchor_first);
            let cols: Vec<String> = v.iter().map(|d| format!("{d:.1}")).collect();
            println!("S\t{i}\t{k}\t{}\t{}", if anchor_first { "AXA" } else { "XAX" }, cols.join("\t"));
        }
    }
}
