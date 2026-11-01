//! The syntax tree and programs (`Ast.scala`): `var` references, expressions, and [`Program::from`], which bounds a
//! tree's size and depth before anything else reads it, and checks that it has exactly one surface and canonical form.

use alloc::boxed::Box;
use alloc::collections::BTreeMap;
use alloc::string::String;
use alloc::vec::Vec;

use dilf4s_core::codec::Model;
use dilf4s_core::generated::jlvm as w;
use dilf4s_core::hash::{DomainTag, HashAlgorithm};
use dilf4s_core::{CanonicalError, Value, cbor};

use crate::error::JlvmError;
use crate::op::{Op, call_name};
use crate::values::utf16_len;

/// A `var` path: `$name.path` reads a binding (host, `let` or callback); any other path reads the data.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum VarRef {
    Data(Vec<String>),
    Bound(String, Vec<String>),
}

impl VarRef {
    pub(crate) fn render(&self) -> String {
        match self {
            Self::Data(p) => p.join("."),
            Self::Bound(n, p) => {
                let mut s = String::from("$");
                s.push_str(n);
                for seg in p {
                    s.push('.');
                    s.push_str(seg);
                }
                s
            }
        }
    }

    /// The `.`-separated segments of `s`, empty ones included.
    pub(crate) fn segments(s: &str) -> Vec<String> {
        s.split('.').map(String::from).collect()
    }

    pub(crate) fn parse(s: &str) -> Result<Self, JlvmError> {
        if s.is_empty() {
            return Ok(Self::Data(Vec::new()));
        }
        let segments = Self::segments(s);
        if segments.iter().any(String::is_empty) {
            return Err(JlvmError::malformed(alloc::format!("empty segment in var path \"{s}\"")));
        }
        let mut it = segments.into_iter();
        match it.next() {
            Some(h) if h.starts_with('$') => {
                let name = String::from(h.get(1..).unwrap_or(""));
                if name.is_empty() {
                    Err(JlvmError::malformed(alloc::format!("empty binding name in \"{s}\"")))
                } else {
                    Ok(Self::Bound(name, it.collect()))
                }
            }
            Some(h) => {
                let mut all = alloc::vec![h];
                all.extend(it);
                Ok(Self::Data(all))
            }
            None => Ok(Self::Data(Vec::new())),
        }
    }
}

/// The syntax tree of a program (language version 1). Built only by the parser, which bounds its depth by the strict
/// JSON parser's (256), so its derived `Drop` and the walks that follow [`Program::from`]'s bound may recurse.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Expr {
    Lit(Value),
    Var(VarRef, Option<Value>),
    /// A path computed at run time.
    VarAt(Box<Expr>, Option<Value>),
    Apply(Op, Vec<Expr>),
    /// A variadic operation whose arguments are the elements of one array value.
    Spread(Op, Box<Expr>),
    Arr(Vec<Expr>),
    /// Fields in code-point order of the keys, which are distinct.
    Obj(Vec<(String, Expr)>),
}

impl Expr {
    fn children(&self) -> &[Self] {
        match self {
            Self::Apply(_, xs) | Self::Arr(xs) => xs,
            Self::Spread(_, x) | Self::VarAt(x, _) => core::slice::from_ref(&**x),
            Self::Obj(_) | Self::Lit(_) | Self::Var(_, _) => &[],
        }
    }

    /// The sub-expressions, fields included.
    fn each_child(&self) -> impl Iterator<Item = &Self> {
        let fields: &[(String, Self)] = if let Self::Obj(fs) = self { fs } else { &[] };
        self.children().iter().chain(fields.iter().map(|(_, x)| x))
    }
}

/// A program: language version 1 and its expression, bounded in size and depth, with exactly one surface form and one
/// canonical form. Built only by [`crate::parse`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub(crate) expr: Expr,
}

/// The language version.
pub const VERSION: u32 = 1;

/// Expression nesting: an operation's arguments, an array's items and a map's fields are one level deeper.
pub const MAX_DEPTH: u64 = 256;

/// JSON nesting of the rendered program, as the canonical JSON parser bounds it.
pub const MAX_SURFACE_DEPTH: u64 = 256;

/// The largest program, in size: one per expression, plus the weight of each literal and default, plus the UTF-16
/// length of each object key, binding name and path segment, plus one per segment.
pub const MAX_SIZE: u64 = 1 << 18;

/// The tag of a program's identity.
pub const PROGRAM_TAG: &str = "dilf4s/jlvm/program/v1";

fn path_size(p: &[String]) -> u64 {
    p.iter().fold(0u64, |n, s| n.saturating_add(utf16_len(s)).saturating_add(1))
}

fn default_weight(d: Option<&Value>) -> u64 {
    d.map_or(0, Value::weight)
}

/// One node's contribution to the size (`Program.size`); saturating.
fn size(e: &Expr) -> u64 {
    match e {
        Expr::Lit(v) => v.weight().saturating_add(1),
        Expr::Var(VarRef::Data(p), d) => path_size(p).saturating_add(1).saturating_add(default_weight(d.as_ref())),
        Expr::Var(VarRef::Bound(n, p), d) => {
            utf16_len(n).saturating_add(2).saturating_add(path_size(p)).saturating_add(default_weight(d.as_ref()))
        }
        Expr::VarAt(_, d) => default_weight(d.as_ref()).saturating_add(1),
        Expr::Obj(fs) => fs.iter().fold(1u64, |n, (k, _)| n.saturating_add(1).saturating_add(utf16_len(k))),
        Expr::Arr(_) | Expr::Apply(_, _) | Expr::Spread(_, _) => 1,
    }
}

impl Program {
    /// The size and depth bounds, then the shapes no surface text can produce, then the rendered surface's depth and
    /// the canonical form (`Program.from`).
    pub(crate) fn from(expr: Expr) -> Result<Self, JlvmError> {
        measure(&expr)?;
        check(&expr)?;
        let surface = surface_depth(&expr);
        if surface > MAX_SURFACE_DEPTH {
            return Err(JlvmError::limit(alloc::format!(
                "the program's JSON surface nests {surface} deep, deeper than {MAX_SURFACE_DEPTH}"
            )));
        }
        let program = Self { expr };
        program.canonical()?;
        Ok(program)
    }

    /// The canonical form (`shared/schemas/jlvm.cddl`).
    pub fn canonical(&self) -> Result<Value, JlvmError> {
        w::JlvmProgram { expr: to_wire(&self.expr) }.to_value().map_err(canonical_error)
    }

    /// The canonical CBOR encoding.
    pub fn encode(&self) -> Result<alloc::vec::Vec<u8>, JlvmError> {
        self.canonical().map(|v| cbor::encode(&v))
    }

    /// The program's identity: the tagged hash of its canonical form.
    pub fn id(&self, hash: HashAlgorithm) -> Result<alloc::vec::Vec<u8>, JlvmError> {
        let tag = DomainTag::new(PROGRAM_TAG).map_err(|e| JlvmError::malformed(alloc::format!("{e}")))?;
        self.encode().map(|bytes| hash.tagged_hash(&tag, &bytes))
    }
}

/// A depth bound is `Limit`, however it is reached; anything else malformed is `Malformed`.
pub(crate) fn canonical_error(e: CanonicalError) -> JlvmError {
    match e {
        CanonicalError::DepthExceeded { limit } => {
            JlvmError::limit(alloc::format!("the program nests deeper than its canonical form allows ({limit} levels)"))
        }
        other => JlvmError::malformed(alloc::format!("{other}")),
    }
}

/// Siblings still to visit from an index, at a depth; an object's fields count after its plain children.
type Pending<'e> = (&'e [Expr], usize, u64, &'e [(String, Expr)]);

/// The size and depth bounds, before anything else reads the tree: each node counts as it is reached, and the walk
/// stops as soon as a bound is exceeded. An explicit stack of (siblings, next index, depth).
fn measure(root: &Expr) -> Result<(), JlvmError> {
    let too_large = || JlvmError::limit(alloc::format!("the program is larger than {MAX_SIZE}"));
    let mut todo: Vec<Pending<'_>> = alloc::vec![(core::slice::from_ref(root), 0usize, 0u64, &[][..])];
    let mut seen = 0u64;
    loop {
        if seen > MAX_SIZE {
            return Err(too_large());
        }
        let Some((items, i, d, fields)) = todo.pop() else {
            return Ok(());
        };
        // the plain children first, then the fields of an object
        let item = items.get(i).or_else(|| i.checked_sub(items.len()).and_then(|j| fields.get(j)).map(|(_, x)| x));
        let Some(e) = item else {
            continue;
        };
        if d > MAX_DEPTH {
            return Err(JlvmError::limit(alloc::format!("the program nests deeper than {MAX_DEPTH}")));
        }
        todo.push((items, i.saturating_add(1), d, fields));
        let own: &[(String, Expr)] = if let Expr::Obj(fs) = e { fs } else { &[] };
        todo.push((e.children(), 0, d.saturating_add(1), own));
        seen = seen.saturating_add(size(e));
    }
}

fn unspellable(r: &VarRef) -> JlvmError {
    JlvmError::malformed(alloc::format!(
        "the var path {r:?} has no text form: \"{}\" reads back as another path; read a key holding '.', an empty key \
         or a key starting with '$' with the exact-segment form {{\"var\": [[...]]}}",
        r.render()
    ))
}

/// The shapes no surface text can produce. Runs once [`measure`] has bounded the tree; an explicit stack.
fn check(root: &Expr) -> Result<(), JlvmError> {
    let mut todo: Vec<&Expr> = alloc::vec![root];
    while let Some(e) = todo.pop() {
        match e {
            Expr::Spread(op, _) if !op.is_variadic() => {
                return Err(JlvmError::malformed(alloc::format!("{} does not spread its argument", op.wire())));
            }
            Expr::Spread(op, a) if matches!(**a, Expr::Arr(_)) => {
                return Err(JlvmError::malformed(alloc::format!(
                    "a spread {} takes one expression, not an argument list",
                    op.wire()
                )));
            }
            Expr::Var(_, Some(Value::Null)) | Expr::VarAt(_, Some(Value::Null)) => {
                return Err(JlvmError::malformed(
                    "a var default is never null: the canonical form cannot tell it from no default",
                ));
            }
            Expr::Var(r, _) if VarRef::parse(&r.render()).ok().as_ref() != Some(r) => return Err(unspellable(r)),
            _ => {}
        }
        let mut children: Vec<&Expr> = e.each_child().collect();
        children.reverse();
        todo.extend(children);
    }
    Ok(())
}

// ---- the depth of the rendered surface (Parser.surfaceDepth)

/// A literal value's nesting, counting only non-empty arrays and maps (as the canonical JSON parser counts a rendered
/// value). An explicit stack.
fn value_depth(v: &Value) -> u64 {
    let mut deepest = 0u64;
    let mut todo: Vec<(&Value, u64)> = alloc::vec![(v, 0)];
    while let Some((x, above)) = todo.pop() {
        match x {
            Value::Array(a) if !a.items().is_empty() => {
                let here = above.saturating_add(1);
                deepest = deepest.max(here);
                todo.extend(a.items().iter().map(|c| (c, here)));
            }
            Value::Map(m) if !m.is_empty() => {
                let here = above.saturating_add(1);
                deepest = deepest.max(here);
                todo.extend(m.values().map(|c| (c, here)));
            }
            _ => {}
        }
    }
    deepest
}

/// A literal or default as the renderer writes it: quoted, `{"literal": v}`, when it is an array or a map.
fn literal_depth(v: &Value) -> u64 {
    match v {
        Value::Array(_) | Value::Map(_) => value_depth(v).saturating_add(1),
        _ => 0,
    }
}

fn defaults(d: Option<&Value>) -> u64 {
    d.map_or(0, literal_depth)
}

fn plus(l: u64, n: u64) -> u64 {
    l.saturating_add(n)
}

/// How deep the rendered surface nests a program's JSON, case by case as `Parser.surfaceStep`: a worklist of
/// (expression, nesting of the JSON container holding it).
fn surface_depth(root: &Expr) -> u64 {
    let mut deepest = 0u64;
    let mut todo: Vec<(&Expr, u64)> = alloc::vec![(root, 0)];
    while let Some((e, l)) = todo.pop() {
        let own = match e {
            Expr::Lit(v) => plus(l, literal_depth(v)),
            Expr::Var(_, None) => plus(l, 1),
            Expr::Var(_, Some(d)) => plus(plus(l, 2), literal_depth(d)),
            Expr::VarAt(p, d) => match (&**p, d) {
                (Expr::Lit(v), None) => plus(plus(l, 2), value_depth(v)),
                (Expr::Lit(v), Some(d)) => plus(plus(l, 3), value_depth(v)).max(plus(plus(l, 2), literal_depth(d))),
                (p @ Expr::Arr(_), None) => {
                    todo.push((p, plus(l, 2)));
                    plus(l, 2)
                }
                (p, None) => {
                    todo.push((p, plus(l, 1)));
                    plus(l, 1)
                }
                (p, d) => {
                    todo.push((p, plus(l, 2)));
                    plus(plus(l, 2), defaults(d.as_ref()))
                }
            },
            Expr::Apply(Op::Let, args) if matches!(args.as_slice(), [Expr::Arr(_), _]) => {
                let (pairs, body) = match args.as_slice() {
                    [Expr::Arr(pairs), body] => (pairs.as_slice(), body),
                    _ => (&[][..], e),
                };
                todo.push((body, plus(l, 2)));
                let mut own = plus(l, 2);
                for pair in pairs {
                    match pair {
                        Expr::Arr(nx) if matches!(nx.as_slice(), [Expr::Lit(Value::Text(_)), _]) => {
                            if let Some(x) = nx.get(1) {
                                todo.push((x, plus(l, 4)));
                            }
                            own = own.max(plus(l, 4));
                        }
                        other => {
                            todo.push((other, plus(l, 3)));
                            own = own.max(plus(l, 3));
                        }
                    }
                }
                own
            }
            Expr::Apply(_, args) => {
                todo.extend(args.iter().map(|a| (a, plus(l, 2))));
                if args.is_empty() { plus(l, 1) } else { plus(l, 2) }
            }
            Expr::Spread(_, a) => {
                todo.push((a, plus(l, 1)));
                plus(l, 1)
            }
            Expr::Arr(items) => match items.split_first() {
                Some((Expr::Lit(Value::Text(s)), rest)) if call_name(s) => {
                    todo.extend(rest.iter().map(|x| (x, plus(l, 1))));
                    plus(l, 2)
                }
                None => l,
                Some(_) => {
                    todo.extend(items.iter().map(|x| (x, plus(l, 1))));
                    plus(l, 1)
                }
            },
            Expr::Obj(fields) => match fields.as_slice() {
                [(k, x)] if !k.starts_with('_') => {
                    todo.push((x, plus(l, 2)));
                    plus(l, 2)
                }
                [] => l,
                fs => {
                    todo.extend(fs.iter().map(|(_, x)| (x, plus(l, 1))));
                    plus(l, 1)
                }
            },
        };
        deepest = deepest.max(own);
    }
    deepest
}

// ---- the canonical form

/// The canonical syntax tree. Recursion is bounded: [`measure`] has bounded the depth to [`MAX_DEPTH`].
fn to_wire(e: &Expr) -> w::JlvmExpr {
    match e {
        Expr::Lit(Value::Null) => w::JlvmExpr::JlvmNull(w::JlvmNull),
        Expr::Lit(v) => w::JlvmExpr::JlvmLit(w::JlvmLit { lit: v.clone() }),
        Expr::Var(r, d) => w::JlvmExpr::JlvmVar(w::JlvmVar { var: r.render(), default: d.clone() }),
        Expr::VarAt(p, d) => w::JlvmExpr::JlvmVarAt(w::JlvmVarAt { path: Box::new(to_wire(p)), default: d.clone() }),
        Expr::Apply(op, args) => w::JlvmExpr::JlvmApply(w::JlvmApply {
            op: String::from(op.wire()),
            args: args.iter().map(to_wire).collect(),
        }),
        Expr::Spread(op, a) => {
            w::JlvmExpr::JlvmSpread(w::JlvmSpread { spread: String::from(op.wire()), arg: Box::new(to_wire(a)) })
        }
        Expr::Arr(items) => w::JlvmExpr::JlvmArray(w::JlvmArray { arr: items.iter().map(to_wire).collect() }),
        Expr::Obj(fields) => w::JlvmExpr::JlvmMap(w::JlvmMap {
            map: fields.iter().map(|(k, x)| (k.clone(), to_wire(x))).collect::<BTreeMap<_, _>>(),
        }),
    }
}
