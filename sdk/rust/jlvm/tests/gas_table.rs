//! The gas table of `docs/specs/jlvm.md` section 7 is this crate's schedule, row for row: the reference generates the
//! table from its own schedule (`GasTable.scala`), so the two implementations and the spec agree on every operation,
//! not only on those the vectors reach.

#![allow(clippy::panic, reason = "tests fail by panicking")]

use dilf4s_jlvm::{GasSchedule, Op, OpCost, Rate, SizeUnit};

const BEGIN: &str = "<!-- gas-table: generated from GasSchedule.V1 by GasTableGen; do not edit -->";
const END: &str = "<!-- gas-table: end -->";

fn unit(u: SizeUnit) -> String {
    match u {
        SizeUnit::None => String::from("none"),
        other => format!("`{other:?}`"),
    }
}

fn rate(r: Rate) -> String {
    format!("{} per {}", r.per_unit, unit(r.unit))
}

fn row(op: Op, c: &OpCost) -> String {
    let mut others: Vec<String> = Vec::new();
    if let Some((i, r)) = c.by_argument {
        others.push(format!("argument {i}: {}", rate(r)));
    }
    for r in c.extra.iter().flatten() {
        others.push(format!("plus {}", rate(*r)));
    }
    if let Some(s) = c.search {
        others.push(format!("search: {} per level, {} per level and key block", s.per_level, s.per_block));
    }
    if let Some(m) = c.merging {
        others
            .push(format!("merging: {} per level and entry, {} per level and weight block", m.per_level, m.per_block));
    }
    let per = if c.unit == SizeUnit::None && c.per_unit == 0 {
        String::new()
    } else {
        rate(Rate { per_unit: c.per_unit, unit: c.unit })
    };
    format!("| `{}` | {} | {per} | {} |", op.wire(), c.base, others.join("; "))
}

fn render(s: &GasSchedule) -> String {
    let scalars = [
        ("literal", s.literal),
        ("element", s.element),
        ("varAccess", s.var_access),
        ("perPathSegment", s.per_path_segment),
        ("callbackRun", s.callback_run),
        ("rendering", s.rendering),
    ];
    let mut lines = vec![
        String::from(BEGIN),
        String::new(),
        format!(
            "Schedule v{}. Scalar charges: {}.",
            s.version,
            scalars.iter().map(|(k, g)| format!("`{k}` {}", g.value())).collect::<Vec<_>>().join(", ")
        ),
        String::new(),
        String::from("| Operation | Base | Per unit | Other rates |"),
        String::from("|---|---|---|---|"),
    ];
    lines.extend(Op::ALL.iter().map(|op| row(*op, &s.cost(*op))));
    lines.push(String::new());
    lines.push(String::from(END));
    lines.join("\n")
}

#[test]
fn the_specs_gas_table_is_this_schedule() {
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../../docs/specs/jlvm.md");
    let spec = std::fs::read_to_string(path).unwrap_or_else(|e| panic!("{path}: {e}"));
    let (Some(b), Some(e)) = (spec.find(BEGIN), spec.find(END)) else { panic!("{path} has no gas-table markers") };
    let table = spec.get(b..e.saturating_add(END.len())).unwrap_or_default();
    assert_eq!(table, render(&GasSchedule::V1), "the spec's gas table and the Rust schedule disagree");
}
