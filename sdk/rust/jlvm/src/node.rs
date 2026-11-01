//! The checked form of a program (`Node.scala`). Every special form's shape (`if`, `let`, the callbacks) is validated
//! once, by [`compile`], so evaluation never meets a malformed form; what remains are value errors.

use alloc::boxed::Box;
use alloc::collections::BTreeMap;
use alloc::string::String;
use alloc::vec::Vec;
use core::cmp::Ordering;

use dilf4s_core::Value;
use dilf4s_core::value::{MapShape, Text};

use crate::ast::{Expr, Program, VarRef};
use crate::error::JlvmError;
use crate::op::Op;
use crate::values::{kind, segment_count};

/// `and`, `or`, `default`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Logical {
    And,
    Or,
    Default,
}

impl Logical {
    pub(crate) const fn op(self) -> Op {
        match self {
            Self::And => Op::And,
            Self::Or => Op::Or,
            Self::Default => Op::Default,
        }
    }
}

/// `map`, `filter`, `all`, `none`, `find`, and `count` with a callback.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Iteration {
    Map,
    Filter,
    All,
    None,
    Find,
    Count,
}

impl Iteration {
    pub(crate) const fn op(self) -> Op {
        match self {
            Self::Map => Op::Map,
            Self::Filter => Op::Filter,
            Self::All => Op::All,
            Self::None => Op::None,
            Self::Find => Op::Find,
            Self::Count => Op::Count,
        }
    }
}

/// A `let`'s bindings, in evaluation order, with an index of their names for lookup. Names are chosen by the program,
/// so they are found by binary search over a sorted index, never hashed.
#[derive(Debug)]
#[allow(clippy::struct_field_names, reason = "a let's bindings are its `bindings`")]
pub(crate) struct Bindings {
    pub(crate) bindings: Vec<(String, Node)>,
    /// The names' segment counts, summed once: what each evaluation charges per insertion.
    pub(crate) name_segments: u64,
    /// Positions into `bindings`, sorted by name.
    sorted: Vec<usize>,
}

impl Bindings {
    fn new(bindings: Vec<(String, Node)>) -> Self {
        let mut sorted: Vec<usize> = (0..bindings.len()).collect();
        sorted.sort_by(|&a, &b| name_at(&bindings, a).cmp(name_at(&bindings, b)));
        let name_segments = bindings.iter().fold(0u64, |n, (name, _)| n.saturating_add(segment_count(name)));
        Self { bindings, name_segments, sorted }
    }

    /// The position of the binding named `name`, if this `let` binds it.
    pub(crate) fn position(&self, name: &str) -> Option<usize> {
        self.sorted
            .binary_search_by(|&i| name_at(&self.bindings, i).cmp(name))
            .ok()
            .and_then(|at| self.sorted.get(at).copied())
    }
}

fn name_at(bindings: &[(String, Node)], i: usize) -> &str {
    bindings.get(i).map_or("", |(n, _)| n.as_str())
}

/// A node of the checked program.
#[derive(Debug)]
pub(crate) enum Node {
    Const(Value),
    /// A static path, with its segment count (each segment once, and once more per 32 UTF-16 units; a `$name` counts
    /// as a segment), computed here once: a read never rescans its path to price it.
    Read(VarRef, Option<Value>, u64),
    /// `var` with a computed path.
    ReadAt(Box<Node>, Option<Value>),
    ArrayOf(Vec<Node>),
    /// Field values in code-point order of their keys (the evaluation order), and the keys' shape: checked once,
    /// here, so every evaluation builds its map from the same shared keys in O(n), without copying, comparing or
    /// rescanning a key (`Values.textKeyed`).
    ObjectOf(MapShape, Vec<Node>),
    /// An operation on evaluated arguments.
    Strict(Op, Vec<Node>),
    /// A variadic strict operation on the elements of one array value.
    Spread(Op, Box<Node>),
    /// The first branch whose condition is truthy, else `otherwise`.
    Cond(Vec<(Node, Node)>, Box<Node>),
    Logic(Logical, Vec<Node>),
    Bind(Bindings, Box<Node>),
    Each(Iteration, Box<Node>, Box<Node>),
    /// `some`, with an optional quorum.
    Quorum(Box<Node>, Box<Node>, Option<Box<Node>>),
    /// `reduce`, with an optional initial accumulator.
    Fold(Box<Node>, Box<Node>, Option<Box<Node>>),
    /// `missing`, `missing_some`.
    Lookup(Op, Vec<Node>),
    Crypto(Op, Vec<Node>),
}

/// Names a `let` may bind: non-empty, without `.` or a leading `$`.
fn bindable(name: &str) -> bool {
    !name.is_empty() && !name.contains('.') && !name.starts_with('$')
}

/// The `let` names in scope, counted, so an inner `let` that rebinds a name leaves the outer one in scope when it ends.
/// Sorted, never hashed.
#[derive(Default)]
struct Lets<'e> {
    names: BTreeMap<&'e str, usize>,
}

impl<'e> Lets<'e> {
    fn contains(&self, name: &str) -> bool {
        self.names.contains_key(name)
    }

    fn add(&mut self, name: &'e str) {
        let n = self.names.entry(name).or_insert(0);
        *n = n.saturating_add(1);
    }

    fn remove(&mut self, name: &'e str) {
        if let Some(n) = self.names.get_mut(name) {
            *n = n.saturating_sub(1);
            if *n == 0 {
                self.names.remove(name);
            }
        }
    }
}

/// Compiles a program. Recursion is bounded: the program's depth is bounded by [`crate::ast::MAX_DEPTH`] and its size
/// by [`crate::ast::MAX_SIZE`], and the work is linear in the size (the `let` scope is one counted set, updated in
/// place).
pub(crate) fn compile(program: &Program) -> Result<Node, JlvmError> {
    node(&program.expr, &mut Lets::default())
}

fn malformed<A>(m: impl Into<String>) -> Result<A, JlvmError> {
    Err(JlvmError::malformed(m))
}

fn node<'e>(e: &'e Expr, lets: &mut Lets<'e>) -> Result<Node, JlvmError> {
    match e {
        Expr::Lit(v) => Ok(Node::Const(v.clone())),
        Expr::Var(r @ VarRef::Data(path), _) if path.first().is_some_and(|h| lets.contains(h)) => {
            let n = path.first().map_or("", String::as_str);
            malformed(alloc::format!(
                "\"{}\" starts with \"{n}\", which let binds: read the binding as \"${n}\", the data as \
                 {{\"var\": [[\"{n}\", ...]]}}, or a callback element's field as \"$current.{n}\"",
                r.render()
            ))
        }
        Expr::Var(r, d) => {
            let count = |p: &[String]| p.iter().fold(0u64, |n, s| n.saturating_add(segment_count(s)));
            let segments = match r {
                VarRef::Data(p) => count(p),
                VarRef::Bound(n, p) => segment_count(n).saturating_add(count(p)),
            };
            Ok(Node::Read(r.clone(), d.clone(), segments))
        }
        Expr::VarAt(p, d) => node(p, lets).map(|p| Node::ReadAt(Box::new(p), d.clone())),
        Expr::Spread(op, a) => {
            let not_array = |k: &str| {
                malformed(alloc::format!(
                    "a spread {} takes an array, and its argument is always of kind {k}",
                    op.wire()
                ))
            };
            match node(a, lets)? {
                n @ Node::Const(Value::Array(_)) => Ok(Node::Spread(*op, Box::new(n))),
                Node::Const(v) => not_array(kind(&v)),
                Node::ObjectOf(..) => not_array("object"),
                n => Ok(Node::Spread(*op, Box::new(n))),
            }
        }
        Expr::Arr(items) => nodes(items, lets).map(Node::ArrayOf),
        Expr::Obj(fields) => {
            fields.iter().map(|(_, x)| node(x, lets)).collect::<Result<Vec<_>, _>>().and_then(|nodes| {
                MapShape::new(fields.iter().map(|(k, _)| Text::from(k.as_str())).collect())
                    .map(|shape| Node::ObjectOf(shape, nodes))
                    .map_err(|e| JlvmError::malformed(alloc::format!("{e}")))
            })
        }
        Expr::Apply(op, args) => apply(*op, args, lets),
    }
}

fn nodes<'e>(es: &'e [Expr], lets: &mut Lets<'e>) -> Result<Vec<Node>, JlvmError> {
    es.iter().map(|e| node(e, lets)).collect()
}

fn boxed<'e>(e: &'e Expr, lets: &mut Lets<'e>) -> Result<Box<Node>, JlvmError> {
    node(e, lets).map(Box::new)
}

fn apply<'e>(op: Op, args: &'e [Expr], lets: &mut Lets<'e>) -> Result<Node, JlvmError> {
    let each = |kind: Iteration, lets: &mut Lets<'e>| match args {
        [over, body] => Ok(Node::Each(kind, boxed(over, lets)?, boxed(body, lets)?)),
        _ => malformed(alloc::format!("{} takes [array, callback]", op.wire())),
    };
    match op {
        Op::If => {
            let ns = nodes(args, lets)?;
            let n = ns.len();
            let mut it = ns.into_iter();
            let pairs = |count: usize, it: &mut alloc::vec::IntoIter<Node>| {
                let mut out = Vec::with_capacity(count);
                while let (Some(c), Some(t)) = (it.next(), it.next()) {
                    out.push((c, t));
                }
                out
            };
            if n >= 2 && n % 2 == 0 {
                Ok(Node::Cond(pairs(n / 2, &mut it), Box::new(Node::Const(Value::Null))))
            } else if n >= 3 {
                let otherwise = it.next_back().unwrap_or(Node::Const(Value::Null));
                Ok(Node::Cond(pairs(n / 2, &mut it), Box::new(otherwise)))
            } else {
                malformed("if takes [condition, then, (condition, then)..., (else)]")
            }
        }
        Op::And => nodes(args, lets).map(|ns| Node::Logic(Logical::And, ns)),
        Op::Or => nodes(args, lets).map(|ns| Node::Logic(Logical::Or, ns)),
        Op::Default => nodes(args, lets).map(|ns| Node::Logic(Logical::Default, ns)),
        Op::Let => let_form(args, lets),
        Op::Map => each(Iteration::Map, lets),
        Op::Filter => each(Iteration::Filter, lets),
        Op::All => each(Iteration::All, lets),
        Op::None => each(Iteration::None, lets),
        Op::Find => each(Iteration::Find, lets),
        Op::Count if args.len() == 2 => each(Iteration::Count, lets),
        Op::Some => match args {
            [over, body] => Ok(Node::Quorum(boxed(over, lets)?, boxed(body, lets)?, None)),
            [over, body, min] => Ok(Node::Quorum(boxed(over, lets)?, boxed(body, lets)?, Some(boxed(min, lets)?))),
            _ => malformed("some takes [array, callback] or [array, callback, minimum]"),
        },
        Op::Reduce => match args {
            [over, body] => Ok(Node::Fold(boxed(over, lets)?, boxed(body, lets)?, None)),
            [over, body, init] => Ok(Node::Fold(boxed(over, lets)?, boxed(body, lets)?, Some(boxed(init, lets)?))),
            _ => malformed("reduce takes [array, callback] or [array, callback, initial]"),
        },
        Op::Missing | Op::MissingSome => nodes(args, lets).map(|ns| Node::Lookup(op, ns)),
        _ if op.is_strict() => nodes(args, lets).map(|ns| Node::Strict(op, ns)),
        _ => nodes(args, lets).map(|ns| Node::Crypto(op, ns)),
    }
}

/// `[[[name, expr], ...], body]`, or `[{name: expr, ...}, body]` with bindings in code-point order of the names.
fn let_form<'e>(args: &'e [Expr], lets: &mut Lets<'e>) -> Result<Node, JlvmError> {
    let name = |n: &'e str| {
        if bindable(n) {
            Ok(n)
        } else {
            malformed(alloc::format!("let cannot bind \"{n}\": names are non-empty, without '.' or a leading '$'"))
        }
    };
    let bindings: Result<Vec<(&'e str, &'e Expr)>, JlvmError> = match args.first() {
        Some(Expr::Arr(pairs)) => pairs
            .iter()
            .map(|pair| match pair {
                Expr::Arr(nx) => match nx.as_slice() {
                    [Expr::Lit(Value::Text(n)), x] => name(n).map(|n| (n, x)),
                    _ => malformed("a let binding is [name, expression]"),
                },
                _ => malformed("a let binding is [name, expression]"),
            })
            .collect(),
        // an object's fields are already in code-point order of their keys
        Some(Expr::Obj(fields)) => fields.iter().map(|(n, x)| name(n).map(|n| (n, x))).collect(),
        _ => malformed("let takes [bindings, body]"),
    };
    let [_, body] = args else {
        return malformed("let takes [bindings, body]");
    };
    let bs = bindings?;
    let mut names: Vec<&str> = bs.iter().map(|(n, _)| *n).collect();
    names.sort_unstable();
    if names.windows(2).any(|w| matches!(w, [a, b] if a.cmp(b) == Ordering::Equal)) {
        return malformed("let binds a name twice");
    }
    // each binding sees the ones before it, and the body sees all of them: one pass, carrying the scope forward
    let mut compiled: Vec<(String, Node)> = Vec::with_capacity(bs.len());
    let mut added: Vec<&'e str> = Vec::with_capacity(bs.len());
    let mut outcome = Ok(());
    for (n, x) in bs {
        match node(x, lets) {
            Ok(c) => {
                compiled.push((String::from(n), c));
                lets.add(n);
                added.push(n);
            }
            Err(e) => {
                outcome = Err(e);
                break;
            }
        }
    }
    let result = outcome.and_then(|()| node(body, lets));
    for n in added {
        lets.remove(n);
    }
    result.map(|b| Node::Bind(Bindings::new(compiled), Box::new(b)))
}
