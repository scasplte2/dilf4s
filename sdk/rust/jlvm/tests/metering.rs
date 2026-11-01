//! Denial-of-service regressions, ported from the reference's `MeteringSuite`: every unit of work is charged before it
//! is done, bounded by the gas limit, and nothing hashes or recurses on attacker-chosen input. The reference's timed
//! comparisons are replaced by what they guard that is deterministic: results, error kinds, gas gaps, and equal gas
//! for colliding and ordinary inputs.

#![allow(clippy::panic, reason = "tests fail by panicking")]

extern crate alloc;

use alloc::collections::BTreeMap;
use std::sync::OnceLock;

use dilf4s_core::BigInt;
use dilf4s_jlvm::{
    Compiled, Config, Gas, GasLimit, GasSchedule, Jlvm, JlvmError, Native, Op, Outcome, Value, parse, parse_value,
};

const S: GasSchedule = GasSchedule::V1;

fn t(s: &str) -> Value {
    Value::text(s)
}

fn int(n: i64) -> Value {
    Value::int(n)
}

fn arr(vs: Vec<Value>) -> Value {
    Value::array(vs).unwrap_or_else(|e| panic!("{e}"))
}

fn obj(kvs: Vec<(&str, Value)>) -> Value {
    Value::map(kvs.into_iter().map(|(k, v)| (String::from(k), v))).unwrap_or_else(|e| panic!("{e}"))
}

/// One evaluator for every test: building the backend decodes SP1's Groth16 key.
fn jlvm() -> &'static Jlvm<Native> {
    static JLVM: OnceLock<Jlvm<Native>> = OnceLock::new();
    JLVM.get_or_init(|| Jlvm::new(Native::new().unwrap_or_else(|e| panic!("{e:?}"))))
}

fn eval_limit(program: &str, data: &Value, limit: u64) -> Outcome {
    let p = parse(program).unwrap_or_else(|e| panic!("{program}: {e}"));
    let config = Config { limit: GasLimit(Gas::new(limit)), schedule: S };
    jlvm().evaluate(&p, data, &BTreeMap::new(), &config)
}

fn eval(program: &str, data: &Value) -> Outcome {
    eval_limit(program, data, GasLimit::DEFAULT.0.value())
}

fn gas(o: &Outcome) -> u64 {
    o.gas_used.value()
}

fn is_limit(o: &Outcome) -> bool {
    matches!(o.result, Err(JlvmError::Limit(_)))
}

/// `2^k` distinct strings of length `2k` with one Java hash code: every combination of "Aa" and "BB".
fn colliding(k: u32) -> Vec<String> {
    combinations(k, "Aa", "BB")
}

/// As many strings, as long, from "Aa" and "Ac", whose hash codes differ.
fn control(k: u32) -> Vec<String> {
    combinations(k, "Aa", "Ac")
}

fn combinations(k: u32, a: &str, b: &str) -> Vec<String> {
    (0..k).fold(vec![String::new()], |acc, _| acc.into_iter().flat_map(|s| [s.clone() + a, s + b]).collect())
}

// ---- 1. join

#[test]
fn join_charges_its_output_before_building_it() {
    let p = r#"{"join": [{"var": "xs"}, {"var": "sep"}]}"#;
    let fits = eval(p, &obj(vec![("xs", arr(vec![Value::Null; 1000])), ("sep", t(&"x".repeat(1000)))]));
    let out = fits.result.as_ref().map(Value::weight).unwrap();
    assert!(gas(&fits) >= S.cost(Op::Join).per_unit * out, "gas {} pays for the output's weight {out}", gas(&fits));
    let oversized = eval(p, &obj(vec![("xs", arr(vec![Value::Null; 5000])), ("sep", t(&"x".repeat(5000)))]));
    assert!(is_limit(&oversized), "25 MB of separators is Limit: {:?}", oversized.result);
    assert!(gas(&oversized) < S.cost(Op::Join).per_unit * 4999 * 5000, "rejected before its output is charged");
}

#[test]
fn cat_and_join_charge_for_rendering_numbers() {
    let fifth = Value::number(&BigInt::from(1), &num_traits::pow(BigInt::from(5), 1764)).unwrap();
    let half = Value::number(&BigInt::from(1), &(BigInt::from(1) << 4095)).unwrap();
    for (p, v) in [
        (r#"{"cat": {"var": "xs"}}"#, &fifth),
        (r#"{"cat": {"var": "xs"}}"#, &half),
        (r#"{"join": [{"var": "xs"}, ""]}"#, &fifth),
    ] {
        let o = eval(p, &obj(vec![("xs", arr(vec![v.clone(); 20]))]));
        let out = o.result.as_ref().map_or_else(|e| panic!("renders: {e}"), Value::weight);
        assert!(gas(&o) >= out, "at least 1 gas per output character: {} for {out}", gas(&o));
    }
    let ints = arr((0..1000).map(|i| int(i * 1_000_003)).collect());
    let words = arr(vec![t("w"); 1000]);
    let cat = r#"{"cat": {"var": "xs"}}"#;
    let gas_ints = gas(&eval(cat, &obj(vec![("xs", ints)])));
    let gas_words = gas(&eval(cat, &obj(vec![("xs", words)])));
    assert!(gas_ints > gas_words, "1000 rendered integers ({gas_ints}) cost more than 1000 letters ({gas_words})");
}

// ---- 2. variadic arithmetic

#[test]
fn add_and_mul_bound_every_intermediate() {
    let mut twos = vec![int(2); 10000];
    twos.push(int(0));
    assert!(is_limit(&eval(r#"{"*": {"var": "xs"}}"#, &obj(vec![("xs", arr(twos))]))), "spread");
    let strict = format!(r#"{{"*": [{}, 0]}}"#, vec!["2"; 5000].join(", "));
    assert!(is_limit(&eval(&strict, &Value::Null)), "strict");
    let sum = format!(r#"{{"+": [{}, 0]}}"#, vec!["1"; 5000].join(", "));
    assert_eq!(eval(&sum, &Value::Null).result, Ok(int(5000)));
}

#[test]
fn each_step_pays_for_its_accumulator() {
    let o = eval(r#"{"*": {"var": "xs"}}"#, &obj(vec![("xs", arr(vec![int(2); 4000]))]));
    assert_eq!(o.result, Ok(Value::int(BigInt::from(1) << 4000)));
    let accumulators: u64 = (1..4000u64).map(|i| 1 + i / 64).sum();
    assert!(gas(&o) >= S.cost(Op::Mul).per_unit * accumulators, "gas {}", gas(&o));
}

// ---- 3. unique and intersect

#[test]
fn unique_and_intersect_cost_the_same_for_colliding_strings() {
    let same = arr(colliding(13).iter().map(|s| t(s)).collect());
    let other = arr(control(13).iter().map(|s| t(s)).collect());
    let unique = r#"{"unique": {"var": "xs"}}"#;
    let intersect = r#"{"intersect": [{"var": "xs"}, {"var": "xs"}]}"#;
    let u = eval(unique, &obj(vec![("xs", same.clone())]));
    assert_eq!(u.result.as_ref(), Ok(&same));
    assert_eq!(gas(&u), gas(&eval(unique, &obj(vec![("xs", other.clone())]))));
    let i = eval(intersect, &obj(vec![("xs", same)]));
    assert_eq!(i.result, Ok(Value::Bool(true)));
    assert_eq!(gas(&i), gas(&eval(intersect, &obj(vec![("xs", other)]))));
}

#[test]
fn unique_keeps_first_occurrences_and_intersect_is_a_subset_test() {
    let run = |p: &str| eval(p, &Value::Null).result;
    let ok = |j: &str| Ok(parse_value(j).unwrap());
    assert_eq!(
        run(
            r#"{"unique": [[3, "a", 1, 3, [1], {"literal": {"k": 1}}, "a", 2, [1], {"literal": {"k": 1}}, null, 2, null]]}"#
        ),
        ok(r#"[3, "a", 1, [1], {"k": 1}, 2, null]"#)
    );
    assert_eq!(run(r#"{"unique": [[1, 2.5, 1, 2.5]]}"#), ok("[1, 2.5]"));
    assert_eq!(run(r#"{"intersect": [[2, [1], "x"], [[1], 1, 2, "x"]]}"#), ok("true"));
    assert_eq!(run(r#"{"intersect": [[2, [1, 2]], [[1], 1, 2]]}"#), ok("false"));
}

// ---- 4. missing and missing_some

#[test]
fn lookups_pay_for_the_path_text_before_resolving_it() {
    let long = t(&vec!["a"; 50001].join("."));
    for p in [r#"{"missing_some": [0, [{"var": "p"}]]}"#, r#"{"missing": [{"var": "p"}]}"#] {
        let gap = gas(&eval(p, &obj(vec![("p", long.clone())]))) - gas(&eval(p, &obj(vec![("p", t("a"))])));
        assert!(gap >= long.weight(), "{p}: {gap} more gas for weight {}", long.weight());
    }
}

#[test]
fn inside_callbacks_missing_pays_for_every_frame() {
    let data = obj(vec![("ks", arr((0..50).map(|i| t(&format!("k{i}"))).collect()))]);
    let one = gas(&eval(r#"{"map": [[1], {"missing": {"var": [["ks"]]}}]}"#, &data));
    let two = gas(&eval(r#"{"map": [[1], {"map": [[1], {"missing": {"var": [["ks"]]}}]}]}"#, &data));
    let one_var = gas(&eval(r#"{"map": [[1], {"var": "k0"}]}"#, &data));
    let two_var = gas(&eval(r#"{"map": [[1], {"map": [[1], {"var": "k0"}]}]}"#, &data));
    let (frames, var_frame) = (two - one, two_var - one_var);
    let seg = S.per_path_segment.value();
    assert!(frames - var_frame >= 50 * seg - seg, "{frames} vs {var_frame} for one more frame");
}

// ---- 5. computed paths

#[test]
fn a_computed_path_is_charged_by_its_size_before_it_is_parsed() {
    // a trailing '.' makes the path malformed: parsing it is the only way to find out
    let malformed = t(&(vec!["a"; 50001].join(".") + "."));
    let read = S.var_access.value() + S.per_path_segment.value();
    let starved = eval_limit(r#"{"var": {"var": "p"}}"#, &obj(vec![("p", malformed)]), read + 15);
    assert!(matches!(starved.result, Err(JlvmError::OutOfGas { .. })), "{:?}", starved.result);
}

#[test]
fn integer_path_segments_are_bounded_before_they_are_rendered() {
    let p = r#"{"var": [["m", {"var": "i"}]]}"#;
    let data = obj(vec![("m", obj(vec![("-1", int(7)), ("12", int(8))])), ("i", int(-1))]);
    assert_eq!(eval(p, &data).result, Ok(int(7)));
    assert_eq!(eval(p, &obj(vec![("m", obj(vec![("12", int(8))])), ("i", int(12))])).result, Ok(int(8)));
    let huge = obj(vec![("m", obj(vec![("-1", int(7))])), ("i", Value::int((BigInt::from(1) << 4095) + 1))]);
    assert!(matches!(eval(p, &huge).result, Err(JlvmError::Domain { op: "var", .. })));
}

#[test]
fn a_computed_array_path_pays_for_the_length_of_its_segments() {
    let p = r#"{"var": [[{"var": "k"}]]}"#;
    let long = "k".repeat(50000);
    let gap = gas(&eval(p, &obj(vec![("k", t(&long))]))) - gas(&eval(p, &obj(vec![("k", t("k"))])));
    assert!(gap >= 50000 / 32, "{gap} more gas for a 50,000-character segment");
}

// ---- 6. object keys and let names

#[test]
fn object_literals_evaluate_with_long_keys() {
    let key = "k".repeat(50000);
    let p = format!(r#"{{"count": [{{"var": "xs"}}, {{"object": {{"{key}": 1}}}}]}}"#);
    assert_eq!(eval(&p, &obj(vec![("xs", arr(vec![int(1); 1000]))])).result, Ok(int(1000)));
}

fn lets(n: usize, names: &[String]) -> String {
    let pairs: Vec<String> = names.iter().take(n).map(|x| format!(r#"["{x}", 1]"#)).collect();
    format!(
        r#"{{"let": [[{}], {{"var": "${}"}}]}}"#,
        pairs.join(", "),
        names.get(n.saturating_sub(1)).map_or("", String::as_str)
    )
}

#[test]
fn let_compiles_thousands_of_bindings() {
    let names: Vec<String> = (0..8000).map(|i| format!("n{i}")).collect();
    let o = eval(&lets(8000, &names), &Value::Null);
    assert_eq!(o.result, Ok(int(1)));
}

#[test]
fn let_and_reads_cost_the_same_for_colliding_names() {
    let bound = eval(&lets(1024, &colliding(10)), &Value::Null);
    let unbound = eval(&lets(1024, &control(10)), &Value::Null);
    assert_eq!(bound.result, Ok(int(1)));
    assert_eq!(gas(&bound), gas(&unbound));
}

#[test]
fn a_let_pays_for_the_length_of_its_names() {
    let prefix = "n".repeat(100_000);
    let two = |p: &str| format!(r#"{{"let": [[["{p}a", 1], ["{p}b", 2]], 3]}}"#);
    let gap = gas(&eval(&two(&prefix), &Value::Null)) - gas(&eval(&two(""), &Value::Null));
    let blocks = 2 * ((u64::try_from(prefix.len()).unwrap() + 1) / 32);
    assert!(gap >= S.element.value() * blocks, "{gap} more gas for two names of {} characters", prefix.len() + 1);
}

#[test]
fn set_unset_and_merge_update_a_large_map_in_place() {
    // a host map of 100,000 entries updated 2,000 times: each update is O(log n) and shares the rest, so the run is
    // quick; rebuilding the map per update would copy 200 million entries
    let m = obj((0..100_000)
        .map(|i| (format!("k{i}"), int(i)))
        .collect::<Vec<_>>()
        .iter()
        .map(|(k, v)| (k.as_str(), v.clone()))
        .collect());
    let data = obj(vec![("xs", arr(vec![int(1); 2000])), ("m", m)]);
    let start = std::time::Instant::now();
    let set = eval(
        r#"{"reduce": [{"var": "xs"}, {"set": [{"var": "$accumulator"}, {"cat": ["n", {"var": "$index"}]}, 1]}, {"var": "m"}]}"#,
        &data,
    );
    assert_eq!(eval(r#"{"length": {"var": "m"}}"#, &data).result, Ok(int(100_000)));
    assert!(set.result.is_ok(), "{:?}", set.result.err());
    let unset = eval(
        r#"{"length": {"reduce": [{"var": "xs"}, {"unset": [{"var": "$accumulator"}, {"cat": ["k", {"var": "$index"}]}]}, {"var": "m"}]}}"#,
        &data,
    );
    assert_eq!(unset.result, Ok(int(98_000)));
    let merged = eval(r#"{"length": {"merge": [{"var": "m"}, {"object": {"k1": 2, "new": 3}}]}}"#, &data);
    assert_eq!(merged.result, Ok(int(100_001)));
    assert!(start.elapsed().as_secs() < 20, "{:?}", start.elapsed());
}

#[test]
fn get_has_set_and_unset_pay_for_long_keys() {
    let long = "k".repeat(1 << 20);
    let build = r#"{"reduce": [{"var": "xs"}, {"set": [{"var": "$accumulator"}, {"var": "k"}, {"var": "$index"}]}, {"object": {}}]}"#;
    let lookup = r#"{"count": [{"var": "xs"}, {"get": [{"var": "m"}, {"var": "k"}]}]}"#;
    let data = |k: &str| obj(vec![("xs", arr(vec![int(1); 200])), ("k", t(k)), ("m", obj(vec![(k, int(1))]))]);
    assert!(eval(build, &data(&long)).result.is_ok());
    assert_eq!(eval(lookup, &data(&long)).result, Ok(int(200)));
    let gap = gas(&eval(lookup, &data(&long))) - gas(&eval(lookup, &data("k")));
    assert!(gap >= 200 * ((1 << 20) / 32), "get pays per block of its key: {gap}");
}

// ---- 7. programs are bounded before compilation

#[test]
fn programs_are_bounded_in_size_before_compilation() {
    let limit = |text: &str| matches!(parse(text), Err(JlvmError::Limit(_)));
    let nulls = |n: usize| format!("[{}]", vec!["null"; n].join(","));
    // each element counts once, and the array itself once
    assert!(limit(&nulls(1 << 18)), "one past the bound");
    assert!(parse(&nulls((1 << 18) / 4)).is_ok(), "within the bound");
    let x = "x".repeat((1 << 18) + 1);
    assert!(limit(&format!(r#"{{"var": "{x}"}}"#)), "a path segment counts its length");
    assert!(limit(&format!(r#"{{"var": "${x}"}}"#)), "a binding name counts its length");
    let shared = vec!["y".repeat(1000); 300].join(".");
    assert!(limit(&format!(r#"{{"var": "{shared}"}}"#)), "a long path is bounded before it is rendered");
}

// ---- structure: nothing recurses on program or data

#[test]
fn deep_programs_and_data_run_on_the_explicit_stack() {
    // 127 nested operations: each takes two canonical levels
    let mut p = String::from("1");
    for _ in 0..127 {
        p = format!(r#"{{"+": [{p}, 1]}}"#);
    }
    assert_eq!(eval(&p, &Value::Null).result, Ok(int(128)));
    let too_deep = format!(r#"{{"+": [{p}, 1]}}"#);
    assert!(matches!(parse(&too_deep), Err(JlvmError::Limit(_))), "128 nested operations exceed the canonical depth");
    // data 255 arrays deep, read whole and compared
    let mut d = String::from("1");
    for _ in 0..255 {
        d = format!("[{d}]");
    }
    let data = parse_value(&format!(r#"{{"d": {d}}}"#)).unwrap();
    let o = eval(r#"{"==": [{"var": "d"}, {"var": "d"}]}"#, &data);
    assert_eq!(o.result, Ok(Value::Bool(true)));
}

#[test]
fn ambiguous_paths_warn_once_and_never_change_gas() {
    let data = parse_value(r#"{"a": 1, "xs": [{"a": 2}]}"#).unwrap();
    let plain = eval(r#"{"map": [{"var": "xs"}, {"var": "a"}]}"#, &data);
    assert_eq!(plain.result, Ok(parse_value("[2]").unwrap()));
    assert_eq!(plain.warnings.len(), 1);
    let explicit = eval(r#"{"map": [{"var": "xs"}, {"var": "$current.a"}]}"#, &data);
    assert!(explicit.warnings.is_empty());
}

#[test]
fn host_bindings_are_shadowed_by_let_and_callbacks() {
    let p = parse(
        r#"{"let": [[["x", {"var": "$ordinal"}]], {"map": [[10], {"+": [{"var": "$x"}, {"var": "$current"}]}]}]}"#,
    )
    .unwrap();
    let env: BTreeMap<String, Value> = [(String::from("ordinal"), int(5))].into_iter().collect();
    let o = jlvm().evaluate(&p, &Value::Null, &env, &Config::default());
    assert_eq!(o.result, Ok(parse_value("[15]").unwrap()));
    let unbound = eval(r#"{"var": "$nope"}"#, &Value::Null);
    assert!(matches!(unbound.result, Err(JlvmError::Malformed(_))));
}

#[test]
fn lower_and_upper_change_only_ascii_letters() {
    let data = obj(vec![("s", t("AzΣσİßé\u{301}e\u{301}\u{10D50}\u{1D400}[`@{"))]);
    let lower = eval(r#"{"lower": {"var": "s"}}"#, &data);
    assert_eq!(lower.result, Ok(t("azΣσİßé\u{301}e\u{301}\u{10D50}\u{1D400}[`@{")));
    let upper = eval(r#"{"upper": {"var": "s"}}"#, &data);
    assert_eq!(upper.result, Ok(t("AZΣσİßé\u{301}E\u{301}\u{10D50}\u{1D400}[`@{")));
}

/// Not a regression test: measures the gas rate of reading large data values and of building results from them, which
/// share structure. Run with `--release -- --ignored --nocapture`.
#[test]
#[ignore = "a measurement, not an assertion"]
#[allow(clippy::print_stdout, reason = "reports the measurement")]
fn measure_the_copy_cost_of_reading_large_values() {
    let big = t(&"b".repeat(1 << 20));
    for (name, n, p) in [
        ("typeof of a 1 MiB text", 1000, r#"{"count": [{"var": "xs"}, {"typeof": {"var": "big"}}]}"#),
        ("typeof of $current", 1000, r#"{"count": [{"var": "xs"}, {"typeof": {"var": "$current"}}]}"#),
        ("map to a 1 MiB text, 10k times", 10_000, r#"{"map": [{"var": "xs"}, {"var": "big"}]}"#),
        ("filter 10k copies of a 1 MiB text", 10_000, r#"{"filter": [{"var": "xs"}, true]}"#),
    ] {
        let xs = if name.starts_with("filter") { vec![big.clone(); n] } else { vec![int(1); n] };
        let data = obj(vec![("xs", arr(xs)), ("big", big.clone())]);
        // the backend (which decodes SP1's Groth16 key) and the program are built outside the timing
        let compiled = parse(p).and_then(Compiled::new).unwrap_or_else(|e| panic!("{e}"));
        let start = std::time::Instant::now();
        let o = jlvm().evaluate_compiled(&compiled, &data, &BTreeMap::new(), &Config::default());
        let micros = start.elapsed().as_secs_f64() * 1e6;
        let rate = f64::from(u32::try_from(gas(&o)).unwrap()) / micros;
        println!(
            "{name}: {} gas in {micros:.0} us = {rate:.2} gas/us (calibrated: 150), {:?}",
            gas(&o),
            o.result.err()
        );
    }
}

// ---- shared values: logical size is not memory

#[test]
fn charges_are_computed_in_constant_time_per_argument_and_only_as_far_as_payable() {
    // 1,000 references to one 1 MiB text as a computed path, under a limit of 20: the path's read is 19, and its
    // segment charge is counted from each text's cached UTF-16 length, stopping once it exceeds the gas left
    let big = t(&"k".repeat(1 << 20));
    let data = obj(vec![("p", arr(vec![big.clone(); 1000])), ("m", obj(vec![("k", int(1))])), ("k", big)]);
    let start = std::time::Instant::now();
    let path = eval_limit(r#"{"var": {"var": "p"}}"#, &data, 20);
    assert!(matches!(path.result, Err(JlvmError::OutOfGas { .. })), "{:?}", path.result);
    // 100 of them at the default limit: each segment is a 1 MiB key, priced by its blocks
    let few = obj(vec![("p", arr(vec![t(&"k".repeat(1 << 20)); 100]))]);
    let paid = eval(r#"{"var": {"var": "p"}}"#, &few);
    assert_eq!(paid.result, Ok(Value::Null));
    // get, has, set and unset price a long key from its cached length too
    for p in [
        r#"{"get": [{"var": "m"}, {"var": "k"}]}"#,
        r#"{"has": [{"var": "m"}, {"var": "k"}]}"#,
        r#"{"set": [{"var": "m"}, {"var": "k"}, 1]}"#,
        r#"{"unset": [{"var": "m"}, {"var": "k"}]}"#,
    ] {
        let o = eval_limit(p, &data, 100);
        assert!(matches!(o.result, Err(JlvmError::OutOfGas { .. })), "{p}: {:?}", o.result);
    }
    assert!(start.elapsed().as_secs() < 10, "{:?}", start.elapsed());
}

#[test]
fn object_literals_with_long_common_prefix_keys_build_without_comparing_them() {
    // two 130,000-byte keys differing in their last byte, built into a map 60 times: the keys' order was found once,
    // at compilation, and each map is built from it in O(n), sharing the keys
    let prefix = "k".repeat(130_000);
    let p = format!(r#"{{"map": [{{"var": "xs"}}, {{"object": {{"{prefix}a": 1, "{prefix}b": 2}}}}]}}"#);
    let start = std::time::Instant::now();
    let o = eval(&p, &obj(vec![("xs", arr(vec![int(1); 60]))]));
    let Ok(Value::Array(maps)) = o.result else { panic!("{:?}", o.result.err()) };
    let firsts: Vec<*const u8> = maps
        .items()
        .iter()
        .filter_map(|m| match m {
            Value::Map(m) => m.keys().next().map(|k| k.as_str().as_ptr()),
            _ => None,
        })
        .collect();
    assert_eq!(firsts.len(), 60);
    assert!(firsts.windows(2).all(|w| w.first() == w.get(1)), "every map shares the keys");
    assert!(start.elapsed().as_secs() < 10, "{:?}", start.elapsed());
}

#[test]
fn object_literals_share_their_compiled_keys_across_evaluations() {
    // a 200,000-byte key, built into a map 50 times (within the weight bound): every map holds the program's one copy
    // of the key
    let key = "k".repeat(200_000);
    let p = format!(r#"{{"map": [{{"var": "xs"}}, {{"object": {{"{key}": {{"var": "$index"}}}}}}]}}"#);
    let o = eval(&p, &obj(vec![("xs", arr(vec![int(1); 50]))]));
    let Ok(Value::Array(maps)) = o.result else { panic!("{:?}", o.result.err()) };
    let keys: Vec<*const u8> = maps
        .items()
        .iter()
        .filter_map(|m| match m {
            Value::Map(m) => m.keys().next().map(|k| k.as_str().as_ptr()),
            _ => None,
        })
        .collect();
    assert_eq!(keys.len(), 50);
    assert!(keys.windows(2).all(|w| w.first() == w.get(1)), "every map shares the key");
}

#[test]
fn a_starved_charge_copies_nothing_it_has_not_paid_for() {
    // a 64 MiB computed path, a 1,000,000-element spread, a 1,000,000-bit number for abs (charged by its base only):
    // each charge fails, or the bound is found, before the input is copied
    let path = t(&"a".repeat(64 << 20));
    let start = std::time::Instant::now();
    let read = eval_limit(r#"{"var": {"var": "p"}}"#, &obj(vec![("p", path)]), 20);
    assert!(matches!(read.result, Err(JlvmError::OutOfGas { .. })), "{:?}", read.result);
    let spread = eval_limit(r#"{"+": {"var": "xs"}}"#, &obj(vec![("xs", arr(vec![int(1); 1_000_000]))]), 200);
    assert!(matches!(spread.result, Err(JlvmError::OutOfGas { .. })), "{:?}", spread.result);
    let huge = Value::int(BigInt::from(1) << 1_000_000);
    let abs = eval(r#"{"abs": {"var": "n"}}"#, &obj(vec![("n", huge.clone())]));
    assert!(is_limit(&abs), "{:?}", abs.result);
    // -2^1,000,000: its bit length is cached, so neither abs nor pow (both charged by their base) scans or copies it
    let negative = Value::int(-(BigInt::from(1) << 1_000_000usize));
    for p in [r#"{"abs": {"var": "n"}}"#, r#"{"pow": [{"var": "n"}, 2]}"#, r#"{"round": {"var": "n"}}"#] {
        let o = eval(p, &obj(vec![("n", negative.clone())]));
        assert!(is_limit(&o), "{p}: {:?}", o.result);
    }
    // a pmt_verify index of a million bits, charged as one element, is refused before it is copied or rendered
    let zero = format!("0x{}", "00".repeat(32));
    let pmt = eval(&format!(r#"{{"pmt_verify": ["{zero}", "{zero}", {{"var": "n"}}, []]}}"#), &obj(vec![("n", huge)]));
    assert!(matches!(pmt.result, Err(JlvmError::Crypto { .. })), "{:?}", pmt.result);
    assert!(start.elapsed().as_secs() < 10, "{:?}", start.elapsed());
}

#[test]
fn a_map_to_a_large_value_shares_it_and_stops_at_the_weight_bound() {
    // 10,000 references to one 1 MiB text: about 10 GiB of logical weight in 10,000 pointers. The result is past the
    // weight bound, which is checked from the cached weights as it is built, so the run ends with Limit, within its
    // gas, without expanding the text
    let data = obj(vec![("xs", arr(vec![int(1); 10_000])), ("big", t(&"b".repeat(1 << 20)))]);
    let start = std::time::Instant::now();
    let o = eval(r#"{"map": [{"var": "xs"}, {"var": "big"}]}"#, &data);
    assert!(is_limit(&o), "{:?}", o.result);
    assert!(gas(&o) < 1_000_000, "{} gas", gas(&o));
    assert!(start.elapsed().as_secs() < 10, "{:?}", start.elapsed());
    // the same text kept by a filter is shared too
    let kept = eval(
        r#"{"length": {"filter": [{"var": "xs"}, true]}}"#,
        &obj(vec![("xs", arr(vec![t(&"b".repeat(1 << 20)); 15]))]),
    );
    assert_eq!(kept.result, Ok(int(15)));
}

/// `[x, x]` nested `k` deep: `2^k` leaves in `k` allocations.
fn doubling(k: u32) -> Value {
    (0..k).fold(t("leaf"), |x, _| arr(vec![x.clone(), x]))
}

#[test]
fn doubling_data_is_charged_by_its_logical_weight_before_any_walk() {
    for k in [40, 200] {
        let d = doubling(k);
        assert!(d.weight() > 1 << 40, "k = {k}: weight {}", d.weight());
        let data = obj(vec![("d", d.clone()), ("e", doubling(k))]);
        let starved = |p: &str| {
            let o = eval(p, &data);
            assert!(matches!(o.result, Err(JlvmError::OutOfGas { .. })), "k = {k}: {p}: {:?}", o.result);
            assert_eq!(gas(&o), GasLimit::DEFAULT.0.value());
        };
        // comparing, deduplicating, encoding, hashing and committing all pay by weight before they walk
        starved(r#"{"==": [{"var": "d"}, {"var": "e"}]}"#);
        starved(r#"{"in": [1, {"var": "d"}]}"#);
        starved(r#"{"unique": {"var": "d"}}"#);
        starved(r#"{"intersect": [{"var": "d"}, {"var": "e"}]}"#);
        starved(r#"{"flatten": {"var": "d"}}"#);
        starved(r#"{"sha256": {"var": "d"}}"#);
        starved(r#"{"cat": [{"var": "d"}, {"var": "e"}]}"#);
        starved(r#"{"merge": [{"var": "d"}]}"#);
        starved(r#"{"sigma_verify": [{"var": "d"}, "0x00", "0x00"]}"#);
        starved(r#"{"merkle_verify": [{"object": {"root": {"var": "x"}, "size": 1}}, {"var": "d"}, {"var": "d"}]}"#);
        // building a value from it is bounded by the cached weight at once
        let built = eval(r#"[{"var": "d"}]"#, &data);
        assert!(is_limit(&built), "k = {k}: {:?}", built.result);
        // reading it, or asking its kind, walks nothing
        assert_eq!(eval(r#"{"var": "d"}"#, &data).result, Ok(d.clone()));
        assert_eq!(eval(r#"{"typeof": {"var": "d"}}"#, &data).result, Ok(t("array")));
    }
}
