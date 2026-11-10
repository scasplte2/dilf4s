use alloc::{collections::BTreeMap, string::String, vec::Vec};
use dilf4s_core::generated::jlvm_vectors::JlvmVectors;

pub fn read<P>(vectors: &JlvmVectors, parse: impl Fn(&str) -> Result<P, String>) -> Vec<P> {
    assert_eq!(vectors.cases.len(), 3371, "conformance corpus");
    assert_eq!(vectors.aliases.len(), 4, "alias rows");
    let expected: BTreeMap<_, _> = include_str!("../../../../../shared/vectors/jlvm_parse_refusals_v1.tsv")
        .lines()
        .map(|line| line.split_once('\t').unwrap_or_else(|| panic!("invalid refusal row: {line}")))
        .collect();
    assert_eq!(expected.len(), 67, "expected program parse refusals");
    let mut refused = BTreeMap::new();
    let mut programs = Vec::new();
    for case in &vectors.cases {
        match parse(&case.program) {
            Ok(p) => programs.push(p),
            Err(kind) => {
                refused.insert(case.name.as_str(), kind);
            }
        }
    }
    let actual: BTreeMap<_, _> = refused.iter().map(|(name, kind)| (*name, kind.as_str())).collect();
    assert_eq!(actual, expected, "exact program parse refusals, by case name and error kind");
    for alias in &vectors.aliases {
        for text in [&alias.program, &alias.own] {
            programs.push(parse(text).unwrap_or_else(|e| panic!("{}: {e}", alias.alias)));
        }
    }
    assert_eq!(programs.len(), 3312, "programs including aliases");
    programs
}
