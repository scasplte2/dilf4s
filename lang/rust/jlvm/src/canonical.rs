//! Reading a program back from its canonical form (`Parser.fromCanonical`, `Parser.decode`; docs/specs/jlvm.md
//! section 1, "Reading a canonical form"), its refusals in the normative order:
//!
//! 1. bytes only: the CBOR ([`decode`]), a depth error `Limit` and anything else `Malformed`;
//! 2. a form heavier than [`MAX_CANONICAL_WEIGHT`] is `Limit`, from its cached weight, before anything reads it;
//! 3. a value that is not a `jlvm-program` of `shared/schemas/jlvm.cddl`, anywhere in it, is `Malformed`;
//! 4. a `var` path that does not parse is `Malformed`;
//! 5. then [`Program::from`]: the size and depth bounds (`Limit`), the shapes no surface text writes (`Malformed`), the
//!    rendered surface's depth and the canonical form.
//!
//! The form is read as its generated decoder (`dilf4s_core::generated::jlvm`) reads it, alternative for alternative,
//! but on an explicit stack: no walk recurses on the form, and each keeps one entry per level it is inside (at most the
//! value's depth, 256). Steps 3 and 4 read the whole form in place, building nothing but each `var` path in turn. Only
//! then is the tree built, and only while it is within the size bound: the tree a form makes the reader build is never
//! larger than the largest program ([`crate::MAX_SIZE`]), however far past that a form built with shared parts carries
//! its weight (the reference builds the whole form's tree first, up to the weight guard).

use alloc::boxed::Box;
use alloc::string::String;
use alloc::vec::Vec;
use core::cell::Cell;

use dilf4s_core::codec::{self as c, Alternative, Model as _};
use dilf4s_core::value::{Entries, Map};
use dilf4s_core::{DecodeFailure, Value, cbor};

use crate::ast::{Expr, MAX_CANONICAL_WEIGHT, MAX_SIZE, Program, VarRef, canonical_error, size};
use crate::error::{JlvmError, Raise};
use crate::op::Op;

/// The program a canonical form denotes (`Parser.fromCanonical`), its refusals in the normative order (see the
/// [module documentation](self)).
pub fn from_canonical(form: &Value) -> Result<Program, JlvmError> {
    if form.weight() > MAX_CANONICAL_WEIGHT {
        return Err(JlvmError::limit(alloc::format!(
            "the form weighs more than any program's, {MAX_CANONICAL_WEIGHT}"
        )));
    }
    let expr = root(form)?;
    read(expr)?;
    Program::from(build(expr, &Cell::new(0))?)
}

/// The program a canonical CBOR encoding denotes (`Parser.decode`): the strict CBOR decoder, then [`from_canonical`].
pub fn decode(bytes: &[u8]) -> Result<Program, JlvmError> {
    cbor::decode(bytes).map_err(canonical_error).and_then(|form| from_canonical(&form))
}

fn not_a_program(failure: &DecodeFailure) -> JlvmError {
    JlvmError::malformed(alloc::format!("not a canonical program: {failure}"))
}

fn missing(key: &str) -> JlvmError {
    not_a_program(&DecodeFailure::new(alloc::format!("missing required key {key:?}")))
}

fn expected(what: &str, value: &Value) -> JlvmError {
    not_a_program(&DecodeFailure::new(alloc::format!("expected {what}, got {}", value.kind())))
}

/// The expression of a `jlvm-program`: a map of exactly `jlvm`, which is 1, and `expr` (as `JlvmProgram::from_value`).
fn root(form: &Value) -> Result<&Value, JlvmError> {
    let fields = c::Fields::of(form, &["jlvm", "expr"]).map_err(|e| not_a_program(&e))?;
    fields.required("jlvm", &c::LiteralInt(1)).map_err(|e| not_a_program(&e.at("jlvm")))?;
    match form {
        Value::Map(m) => m.get("expr").ok_or_else(|| missing("expr")),
        other => Err(expected("map", other)),
    }
}

/// The record alternatives of `jlvm-expr`, as its generated decoder selects them (`c::select_alternative`): the record
/// whose required keys (first) are all present and which allows every key present (second). In the schema's order;
/// at most one fits any map.
const ALTERNATIVES: [(&[&str], &[&str]); 8] = [
    (&[], &[]),
    (&["lit"], &["lit"]),
    (&["var"], &["default", "var"]),
    (&["path"], &["default", "path"]),
    (&["args", "op"], &["args", "op"]),
    (&["arg", "spread"], &["arg", "spread"]),
    (&["arr"], &["arr"]),
    (&["map"], &["map"]),
];

/// One node of a form, as `jlvm-expr` reads it, borrowing its parts.
#[derive(Clone, Copy)]
enum Node<'v> {
    Null,
    Lit(&'v Value),
    Var(&'v str, Option<&'v Value>),
    VarAt(&'v Value, Option<&'v Value>),
    Apply(Op, &'v [Value]),
    Spread(Op, &'v Value),
    Arr(&'v [Value]),
    Map(&'v Map),
}

/// An operation by its own wire name, never an alias (`JlvmOp::from_value`).
fn op(v: &Value) -> Result<Op, JlvmError> {
    Op::from_value(v).map_err(|e| not_a_program(&e))
}

fn items(v: &Value) -> Result<&[Value], JlvmError> {
    match v {
        Value::Array(a) => Ok(a.items()),
        other => Err(expected("array", other)),
    }
}

/// The node `v` is, with its own fields checked; its sub-expressions are read by the caller.
fn node(v: &Value) -> Result<Node<'_>, JlvmError> {
    let alternative = c::select_alternative(v, &ALTERNATIVES, &[]).map_err(|e| not_a_program(&e))?;
    let Value::Map(m) = v else { return Err(expected("map", v)) };
    let field = |key: &str| m.get(key).ok_or_else(|| missing(key));
    match alternative {
        Alternative::Record(0) => Ok(Node::Null),
        Alternative::Record(1) => field("lit").map(Node::Lit),
        Alternative::Record(2) => match field("var")? {
            Value::Text(t) => Ok(Node::Var(t.as_str(), m.get("default"))),
            other => Err(expected("text", other)),
        },
        Alternative::Record(3) => field("path").map(|p| Node::VarAt(p, m.get("default"))),
        Alternative::Record(4) => Ok(Node::Apply(op(field("op")?)?, items(field("args")?)?)),
        Alternative::Record(5) => Ok(Node::Spread(op(field("spread")?)?, field("arg")?)),
        Alternative::Record(6) => items(field("arr")?).map(Node::Arr),
        Alternative::Record(7) => match field("map")? {
            Value::Map(fields) => Ok(Node::Map(fields)),
            other => Err(expected("map", other)),
        },
        other => Err(not_a_program(&c::unknown_alternative(other))),
    }
}

/// A node's sub-expressions still to read, in the order the decoder reads them (a map's in canonical key order).
enum Parts<'v> {
    Items(core::slice::Iter<'v, Value>),
    Fields(Entries<'v>),
    One(Option<&'v Value>),
}

impl<'v> Iterator for Parts<'v> {
    type Item = &'v Value;

    fn next(&mut self) -> Option<&'v Value> {
        match self {
            Self::Items(it) => it.next(),
            Self::Fields(it) => it.next().map(|(_, x)| x),
            Self::One(x) => x.take(),
        }
    }
}

impl<'v> Node<'v> {
    fn parts(self) -> Parts<'v> {
        match self {
            Self::Null | Self::Lit(_) | Self::Var(_, _) => Parts::One(None),
            Self::VarAt(x, _) | Self::Spread(_, x) => Parts::One(Some(x)),
            Self::Apply(_, xs) | Self::Arr(xs) => Parts::Items(xs.iter()),
            Self::Map(m) => Parts::Fields(m.entries()),
        }
    }
}

/// Steps 3 and 4, in place: the shape of every node, then the first `var` path that does not parse. A path is
/// refused only once the whole form has its shape, as the reference decodes the whole form before it reads a path.
/// O(the form's weight), which the guard has bounded; an explicit stack of one entry per level.
fn read(expr: &Value) -> Result<(), JlvmError> {
    let mut path: Option<JlvmError> = None;
    let mut todo: Vec<Parts<'_>> = alloc::vec![Parts::One(Some(expr))];
    while let Some(parts) = todo.last_mut() {
        let Some(v) = parts.next() else {
            todo.pop();
            continue;
        };
        let n = node(v)?;
        if let (Node::Var(p, _), None) = (n, &path) {
            path = VarRef::parse(p).err();
        }
        todo.push(n.parts());
    }
    path.map_or(Ok(()), Err)
}

/// A node being built: its own fields, its sub-expressions still to build, and those built.
struct Building<'v> {
    node: Node<'v>,
    parts: Parts<'v>,
    built: Vec<Expr>,
}

impl<'v> Building<'v> {
    fn of(v: &'v Value) -> Result<Self, JlvmError> {
        node(v).map(|node| Self { node, parts: node.parts(), built: Vec::new() })
    }
}

/// The one sub-expression of a `var` with a computed path or of a spread.
fn only(built: Vec<Expr>) -> Result<Box<Expr>, JlvmError> {
    <[Expr; 1]>::try_from(built)
        .map(|[x]| Box::new(x))
        .map_err(|b| JlvmError::malformed(alloc::format!("{} sub-expressions where the form has one", b.len())))
}

/// The expression of a node whose sub-expressions are built. A map's fields are kept in code-point order of their
/// keys, as the parser keeps them; the form holds them in canonical (length-first) order.
fn finish(node: Node<'_>, built: Vec<Expr>) -> Result<Expr, JlvmError> {
    Ok(match node {
        Node::Null => Expr::Lit(Value::Null),
        Node::Lit(v) => Expr::Lit(v.clone()),
        Node::Var(p, d) => Expr::Var(VarRef::parse(p)?, d.cloned()),
        Node::VarAt(_, d) => Expr::VarAt(only(built)?, d.cloned()),
        Node::Apply(op, _) => Expr::Apply(op, built),
        Node::Spread(op, _) => Expr::Spread(op, only(built)?),
        Node::Arr(_) => Expr::Arr(built),
        Node::Map(m) => {
            let mut fields: Vec<(String, Expr)> = m.keys().map(|k| String::from(k.as_str())).zip(built).collect();
            fields.sort_by(|a, b| a.0.cmp(&b.0));
            Expr::Obj(fields)
        }
    })
}

/// The tree of a form whose shape and paths [`read`] has checked, built bottom-up on an explicit stack, each node's
/// size (`Program.size`) counted as it is finished: past [`MAX_SIZE`] it is `Limit` at once, as [`Program::from`]
/// would refuse it, so the tree built never holds more than the largest program. `opened` counts the nodes begun.
fn build(expr: &Value, opened: &Cell<u64>) -> Result<Expr, JlvmError> {
    let begin = |v| {
        opened.set(opened.get().saturating_add(1));
        Building::of(v)
    };
    let mut seen = 0u64;
    let mut todo: Vec<Building<'_>> = alloc::vec![begin(expr)?];
    while let Some(top) = todo.last_mut() {
        if let Some(v) = top.parts.next() {
            let next = begin(v)?;
            todo.push(next);
            continue;
        }
        let Some(Building { node, built, .. }) = todo.pop() else { break };
        let e = finish(node, built)?;
        seen = seen.saturating_add(size(&e));
        if seen > MAX_SIZE {
            return Err(JlvmError::limit(alloc::format!("the program is larger than {MAX_SIZE}")));
        }
        match todo.last_mut() {
            Some(parent) => parent.built.push(e),
            None => return Ok(e),
        }
    }
    Err(JlvmError::malformed("the form has no expression"))
}

#[cfg(test)]
mod tests {
    extern crate std;

    use dilf4s_core::generated::jlvm as w;
    use dilf4s_core::generated::jlvm_errors::JlvmErrorKind;
    use dilf4s_core::generated::jlvm_vectors::JlvmVectors;
    use dilf4s_core::json;

    use super::*;

    fn program(text: &str) -> Program {
        crate::parse(text).unwrap_or_else(|e| panic!("{text}: {e}"))
    }

    #[test]
    fn a_map_expression_keeps_its_fields_in_code_point_order() {
        // canonical order puts "b" before "aa" (shorter first); the tree, as the parser builds it, "aa" before "b"
        let p = program(r#"{"b": 1, "aa": 2}"#);
        let back = decode(&p.encode().unwrap()).unwrap();
        assert_eq!(back, p);
        assert!(matches!(&back.expr, Expr::Obj(fs) if fs.first().is_some_and(|(k, _)| k == "aa")));
    }

    /// The reference's reading (`Parser.fromWire` after the generated decoder), recursively: the oracle the reader's
    /// explicit stacks are checked against.
    fn from_wire(e: &w::JlvmExpr) -> Result<Expr, JlvmError> {
        let all = |xs: &[w::JlvmExpr]| xs.iter().map(from_wire).collect::<Result<Vec<_>, _>>();
        Ok(match e {
            w::JlvmExpr::JlvmNull(_) => Expr::Lit(Value::Null),
            w::JlvmExpr::JlvmLit(l) => Expr::Lit(l.lit.clone()),
            w::JlvmExpr::JlvmVar(v) => Expr::Var(VarRef::parse(&v.var)?, v.default.clone()),
            w::JlvmExpr::JlvmVarAt(v) => Expr::VarAt(Box::new(from_wire(&v.path)?), v.default.clone()),
            w::JlvmExpr::JlvmApply(a) => Expr::Apply(a.op, all(&a.args)?),
            w::JlvmExpr::JlvmSpread(s) => Expr::Spread(s.spread, Box::new(from_wire(&s.arg)?)),
            w::JlvmExpr::JlvmArray(a) => Expr::Arr(all(&a.arr)?),
            w::JlvmExpr::JlvmMap(m) => Expr::Obj(
                m.map.iter().map(|(k, x)| from_wire(x).map(|e| (k.clone(), e))).collect::<Result<Vec<_>, _>>()?,
            ),
        })
    }

    fn oracle(f: &Value) -> Result<Expr, JlvmError> {
        w::JlvmProgram::from_value(f).map_err(|e| not_a_program(&e)).and_then(|p| from_wire(&p.expr))
    }

    /// Each copy of `v` with its `n`th map (in pre-order) replaced by one of `f` of it; `None` if it has no `n`th map.
    fn at_map(v: &Value, n: &mut usize, f: &dyn Fn(&Map) -> Vec<Value>) -> Option<Vec<Value>> {
        match v {
            Value::Map(m) if *n == 0 => Some(f(m)),
            Value::Map(m) => {
                *n = n.saturating_sub(1);
                let entries = m.clone().into_entries();
                entries.iter().enumerate().find_map(|(i, (_, x))| {
                    let ys = at_map(x, n, f)?;
                    let rebuilt = |y| {
                        let mut e = entries.clone();
                        e[i].1 = y;
                        Value::map(e).unwrap()
                    };
                    Some(ys.into_iter().map(rebuilt).collect())
                })
            }
            Value::Array(a) => a.items().iter().enumerate().find_map(|(i, x)| {
                let ys = at_map(x, n, f)?;
                let rebuilt = |y| {
                    let mut e = a.items().to_vec();
                    e[i] = y;
                    Value::array(e).unwrap()
                };
                Some(ys.into_iter().map(rebuilt).collect())
            }),
            _ => None,
        }
    }

    /// Every way the mutations change one map: a key added, each key removed, and each key's value replaced by a
    /// value of another kind, another operation (an alias and an unknown name among them) or another path.
    fn mutations(m: &Map) -> Vec<Value> {
        let entries = m.clone().into_entries();
        let with = |k: &str, x: Value| {
            let mut e: Vec<(String, Value)> = entries.iter().filter(|(j, _)| j != k).cloned().collect();
            e.push((String::from(k), x));
            Value::map(e).unwrap()
        };
        let one = |k: &str, x: Value| Value::map([(String::from(k), x)]).unwrap();
        let replacements = [
            Value::int(1),
            Value::int(2),
            Value::text("x"),
            Value::text("+"),
            Value::text("-"),
            Value::text("getKey"),
            Value::text("nope"),
            Value::text("a..b"),
            Value::text("$"),
            Value::text("$x.y"),
            Value::Bool(true),
            Value::bytes(alloc::vec![0u8]),
            Value::array(Vec::new()).unwrap(),
            Value::array(alloc::vec![Value::map([]).unwrap(), Value::int(1)]).unwrap(),
            Value::map([]).unwrap(),
            one("lit", Value::int(1)),
            one("var", Value::text("a")),
            one("arr", Value::array(Vec::new()).unwrap()),
        ];
        let mut out = alloc::vec![with("x", Value::int(1)), with("default", Value::int(1))];
        for (k, _) in &entries {
            out.push(Value::map(entries.iter().filter(|(j, _)| j != k).cloned()).unwrap());
            out.extend(replacements.iter().map(|x| with(k, x.clone())));
        }
        out
    }

    fn vector_programs() -> Vec<Program> {
        let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../../shared/vectors/jlvm_v1.json");
        let text = std::fs::read_to_string(path).unwrap();
        let v = JlvmVectors::from_value(&json::parse(&text).unwrap()).unwrap();
        let cases = v.cases.iter().map(|c| c.program.as_str());
        let aliases = v.aliases.iter().flat_map(|a| [a.program.as_str(), a.own.as_str()]);
        cases.chain(aliases).filter_map(|t| crate::parse(t).ok()).collect()
    }

    /// The reader against the reference's recursive reading through the generated decoder: on every vector program's
    /// form, and on every mutation of each of the first twelve maps of every twentieth, the two accept the same forms,
    /// build the same tree, and refuse with the same kind, and so does the whole of [`from_canonical`].
    #[test]
    fn the_reader_reads_every_form_as_the_generated_decoder_does() {
        let programs = vector_programs();
        assert_eq!(programs.len(), 3304, "vector programs including aliases");
        let mut forms: Vec<Value> = programs.iter().map(|p| p.canonical().unwrap()).collect();
        for p in programs.iter().step_by(20) {
            let f = p.canonical().unwrap();
            for n in 0..12 {
                forms.extend(at_map(&f, &mut { n }, &mutations).unwrap_or_default());
            }
        }
        let (mut accepted, mut refused) = (0usize, 0usize);
        for f in &forms {
            let mine = root(f).and_then(|e| read(e).map(|()| e));
            let theirs = oracle(f);
            match (&mine, &theirs) {
                (Ok(e), Ok(tree)) => {
                    assert_eq!(build(e, &Cell::new(0)).as_ref(), Ok(tree), "the tree of {f:?}");
                    accepted = accepted.saturating_add(1);
                }
                (Err(a), Err(b)) => {
                    assert_eq!((a.kind(), b.kind()), (JlvmErrorKind::Malformed, JlvmErrorKind::Malformed), "{f:?}");
                    refused = refused.saturating_add(1);
                }
                _ => panic!("the reader gives {mine:?}, the generated decoder {theirs:?}, for {f:?}"),
            }
            let reference = theirs.and_then(Program::from).map_err(|e| e.kind());
            assert_eq!(from_canonical(f).map_err(|e| e.kind()), reference, "{f:?}");
        }
        std::println!("{accepted} forms accepted and {refused} refused alike");
        assert_eq!(accepted, 9020, "accepted canonical forms");
        assert_eq!(refused, 17191, "refused canonical forms");
    }

    /// The tree is built only while it is within the size bound: a form of 2^20 shared nulls (about 2^20 in weight,
    /// within the guard) opens at most half the largest program's size of nodes (each null counts 2), not 2^20.
    #[test]
    fn building_stops_at_the_size_bound_whatever_the_weight() {
        let nulls = Value::array(alloc::vec![Value::map([]).unwrap(); 1 << 20]).unwrap();
        let f = Value::map([
            (String::from("jlvm"), Value::int(1)),
            (String::from("expr"), Value::map([(String::from("arr"), nulls)]).unwrap()),
        ])
        .unwrap();
        assert!(f.weight() > 1 << 20 && f.weight() <= MAX_CANONICAL_WEIGHT, "weight {}", f.weight());
        let expr = root(&f).unwrap();
        assert_eq!(read(expr), Ok(()));
        let opened = Cell::new(0);
        assert_eq!(build(expr, &opened).map_err(|e| e.kind()), Err(JlvmErrorKind::Limit));
        assert!(opened.get() <= MAX_SIZE / 2 + 2, "{} nodes opened", opened.get());
        assert_eq!(from_canonical(&f).map_err(|e| e.kind()), Err(JlvmErrorKind::Limit));
    }
}
