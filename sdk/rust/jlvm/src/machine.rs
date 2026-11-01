//! The evaluation rules as small steps, run by an explicit continuation stack (`Machine.scala`: `Semantics` and the
//! `direct` driver).
//!
//! The reference writes each node's behaviour as `Step`s whose continuations are closures. Here every closure is a
//! named variant: [`Then`] for the work a `Charge` pays for, [`Kont`] for what `Eval` does with a value. Each variant
//! cites the reference line it ports, so the order of charges (which decides the gas of an out-of-gas run) can be read
//! off side by side. The driver keeps the continuations on a `Vec`; nothing recurses on the program or the data.

use alloc::collections::BTreeMap;
use alloc::rc::Rc;
use alloc::string::String;
use alloc::vec::Vec;

use dilf4s_core::Value;

use dilf4s_core::value::{Array, MapShape, Text};

use crate::ast::VarRef;
use crate::crypto::{self, CryptoBackend};
use crate::error::JlvmError;
use crate::gas::{Gas, GasSchedule};
use crate::node::{self, Iteration, Logical, Node};
use crate::op::Op;
use crate::ops::{self, Joining};
use crate::ratio::Ratio;
use crate::values::{self, at, kind, segment_count, text_segment_count, truthy};

/// Lint findings recorded during evaluation. They never change a result or its gas.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Warning {
    /// Inside a callback, a plain path resolved in an element, and an outer element or the data holds the same first
    /// key: `$current` (or an explicit outer path) says which one is meant.
    AmbiguousPath(String),
}

/// At most this many distinct warnings are kept per evaluation.
pub const MAX_WARNINGS: usize = 64;

/// Gas used so far, against the limit; and the distinct warnings so far.
struct Meter {
    used: Gas,
    limit: Gas,
    warnings: Vec<Warning>,
}

impl Meter {
    fn warn(&mut self, w: Warning) {
        if self.warnings.len() < MAX_WARNINGS && !self.warnings.contains(&w) {
            self.warnings.push(w);
        }
    }
}

// ------------------------------------------------------------------------------------------------ scope

/// The `$` bindings: the host's, then one level per enclosing `let` and callback (innermost first). A walk is bounded
/// by lexical nesting, not by the number of bindings: a `let` is one level whose names are found by binary search, and
/// its values live in the machine's let stack.
enum Bindings<'a> {
    Host(&'a BTreeMap<String, Value>),
    /// The first `count` bindings of a `let`, whose values are `slots[slot]`.
    Let {
        parent: Rc<Bindings<'a>>,
        node: &'a node::Bindings,
        slot: usize,
        count: usize,
    },
    Element {
        parent: Rc<Bindings<'a>>,
        current: Value,
        index: Value,
    },
    Accumulating {
        parent: Rc<Bindings<'a>>,
        current: Value,
        index: Value,
        accumulator: Value,
    },
}

/// An enclosing callback's contribution to plain paths (`Machine.scala` `Frame`).
enum Frame {
    Element(Value),
    Accumulating(Value, Value),
}

impl Frame {
    /// What `""` reads.
    fn element(&self) -> &Value {
        match self {
            Self::Element(v) | Self::Accumulating(v, _) => v,
        }
    }

    /// The value this frame binds to `key`, if it binds it.
    fn lookup(&self, key: &str) -> Option<&Value> {
        match self {
            Self::Element(v) => match v {
                Value::Map(m) => m.get(key),
                _ => None,
            },
            Self::Accumulating(c, _) if key == "current" => Some(c),
            Self::Accumulating(_, a) if key == "accumulator" => Some(a),
            Self::Accumulating(_, _) => None,
        }
    }
}

struct Frames {
    frame: Frame,
    parent: Option<Rc<Frames>>,
}

/// What a node sees (`Machine.scala` `Scope`): the bindings and, inside callbacks, the enclosing frames.
#[derive(Clone)]
struct Scope<'a> {
    bindings: Rc<Bindings<'a>>,
    frames: Option<Rc<Frames>>,
    frame_count: u64,
}

impl<'a> Scope<'a> {
    fn frames(&self) -> impl Iterator<Item = &Frame> {
        core::iter::successors(self.frames.as_deref(), |f| f.parent.as_deref()).map(|f| &f.frame)
    }

    fn enter(&self, bindings: Bindings<'a>, frame: Frame) -> Self {
        Self {
            bindings: Rc::new(bindings),
            frames: Some(Rc::new(Frames { frame, parent: self.frames.clone() })),
            frame_count: self.frame_count.saturating_add(1),
        }
    }
}

/// A plain data path (`Scope.resolve`): `""` is the innermost element, or the data outside callbacks. Otherwise the
/// path is read from the innermost frame binding its first key, else from the data; it is ambiguous when an outer frame
/// or the data also binds that key.
fn resolve<'s, S: AsRef<str>>(data: &'s Value, scope: &'s Scope<'_>, path: &[S]) -> (&'s Value, bool) {
    let Some((key, rest)) = path.split_first() else {
        return (scope.frames().next().map_or(data, Frame::element), false);
    };
    let mut bound = scope.frames().filter_map(|f| f.lookup(key.as_ref()));
    match bound.next() {
        Some(inner) => {
            let ambiguous = bound.next().is_some() || *at(data, core::slice::from_ref(key)) != Value::Null;
            (at(inner, rest), ambiguous)
        }
        None => (at(data, path), false),
    }
}

/// The binding `$name`, innermost first.
fn binding<'s>(slots: &'s [Vec<Value>], scope: &'s Scope<'_>, name: &str) -> Option<&'s Value> {
    let mut here: &Bindings<'_> = &scope.bindings;
    loop {
        match here {
            Bindings::Host(m) => return m.get(name),
            Bindings::Let { parent, node, slot, count } => {
                let found = node.position(name).filter(|p| p < count).and_then(|p| slots.get(*slot)?.get(p));
                if found.is_some() {
                    return found;
                }
                here = parent;
            }
            Bindings::Element { parent, current, index } => match name {
                "current" => return Some(current),
                "index" => return Some(index),
                _ => here = parent,
            },
            Bindings::Accumulating { parent, current, index, accumulator } => match name {
                "current" => return Some(current),
                "index" => return Some(index),
                "accumulator" => return Some(accumulator),
                _ => here = parent,
            },
        }
    }
}

// ------------------------------------------------------------------------------------------------ steps

/// A path being read: the program's own, or one computed at run time.
enum PathRef<'a> {
    Static(&'a VarRef),
    Owned(VarRef),
}

impl PathRef<'_> {
    fn get(&self) -> &VarRef {
        match self {
            Self::Static(r) => r,
            Self::Owned(r) => r,
        }
    }
}

/// The nodes `all` evaluates left to right: arguments or items, or an object's field values.
#[derive(Clone, Copy)]
enum Args<'a> {
    Nodes(&'a [Node]),
}

impl<'a> Args<'a> {
    fn get(self, i: usize) -> Option<&'a Node> {
        match self {
            Self::Nodes(ns) => ns.get(i),
        }
    }
}

/// What `all` continues with once every argument is evaluated.
#[derive(Clone, Copy)]
enum AllThen<'a> {
    /// `Machine.scala:145`
    Array,
    /// `Machine.scala:150`
    Object(&'a MapShape),
    /// `Machine.scala:156`
    Strict(Op),
    /// `Machine.scala:222-225`
    Lookup(Op),
    /// `Machine.scala:233-234`
    Crypto(Op),
}

/// What a callback form does with its evaluated collection (`Machine.scala:186`, `:192`, `:210`).
enum CollThen<'a> {
    Each { kind: Iteration, body: &'a Node, scope: Scope<'a> },
    Quorum { body: &'a Node, min: Option<&'a Node>, scope: Scope<'a> },
    Fold { body: &'a Node, init: Option<&'a Node>, scope: Scope<'a> },
}

impl<'a> CollThen<'a> {
    fn scope(&self) -> &Scope<'a> {
        match self {
            Self::Each { scope, .. } | Self::Quorum { scope, .. } | Self::Fold { scope, .. } => scope,
        }
    }
}

/// A callback's collection: the array itself, shared (cloning it is O(1)).
type Elements = dilf4s_core::value::Array;

/// The work a `Charge` pays for: each is the reference's `() => Step` thunk at the cited line.
enum Then<'a> {
    /// `Machine.scala:113`
    Done(Value),
    /// `Machine.scala:256`: reads a path already paid for.
    Lookup { path: PathRef<'a>, default: Option<&'a Value>, scope: Scope<'a> },
    /// `Machine.scala:126`: parses a computed text path, already paid for.
    ParsePath { text: Text, default: Option<&'a Value>, scope: Scope<'a> },
    /// `Machine.scala:138`: converts a computed array path's segments, already paid for.
    SegmentPath { segments: Array, default: Option<&'a Value>, scope: Scope<'a> },
    /// `Machine.scala:145`, `:150`, `:156`, `:222`, `:233`: evaluates the arguments.
    All { args: Args<'a>, scope: Scope<'a>, then: AllThen<'a> },
    /// `Machine.scala:156`, `:167`: the operation, after its size charge.
    Operate { op: Op, vs: Vec<Value> },
    /// `Machine.scala:167`: a spread's operation, after its size charge, on its array's elements.
    OperateOn { op: Op, array: Array },
    /// `Machine.scala:163`
    SpreadArg { op: Op, arg: &'a Node, scope: Scope<'a> },
    /// `Machine.scala:173`
    Cond { branches: &'a [(Node, Node)], otherwise: &'a Node, scope: Scope<'a> },
    /// `Machine.scala:335`: tests condition `j`, after its element charge.
    CondTest { branches: &'a [(Node, Node)], j: usize, otherwise: &'a Node, scope: Scope<'a> },
    /// `Machine.scala:175`
    Logic { kind: Logical, args: &'a [Node], scope: Scope<'a> },
    /// `Machine.scala:182`
    Bind { node: &'a node::Bindings, body: &'a Node, scope: Scope<'a> },
    /// `Machine.scala:186`, `:192`, `:210`: evaluates the collection.
    Collection { op: Op, over: &'a Node, then: CollThen<'a> },
    /// `Machine.scala:375`: the collection's elements, after their charge.
    Proceed { then: CollThen<'a>, vs: Elements },
    /// `Machine.scala:399`: one run of a `map`/`filter`/`all`/`none`/`find`/`count` body.
    IterRun {
        kind: Iteration,
        vs: Elements,
        body: &'a Node,
        scope: Scope<'a>,
        i: usize,
        acc: Vec<Value>,
        picked: Vec<usize>,
        n: u64,
    },
    /// `Machine.scala:427`
    QuorumRun { vs: Elements, body: &'a Node, scope: Scope<'a>, min: u64, i: usize, n: u64 },
    /// `Machine.scala:438`
    FoldRun { vs: Elements, body: &'a Node, scope: Scope<'a>, i: usize, acc: Value },
    /// `Machine.scala:325-326`: one step of `+` or `*`, after its accumulator's charge.
    Accumulate { op: Op, rest: Vec<Ratio>, i: usize, acc: Ratio },
    /// `Machine.scala:308`: renders `join`'s pieces, after the rendering charge.
    JoinPieces { vs: Vec<Value> },
    /// `Machine.scala:308`: builds a `join`, after its separators' charge.
    JoinBuild { j: Joining },
    /// `Machine.scala:310`
    Cat { vs: Vec<Value> },
    /// `Machine.scala:225`
    LookupRun { op: Op, vs: Vec<Value>, scope: Scope<'a> },
    /// `Machine.scala:234`
    CryptoRun { op: Op, vs: Vec<Value> },
}

/// What `Eval` does with the value: each is the reference's `CanonicalValue => Step` at the cited line.
enum Kont<'a> {
    /// `Eval(n, s, Done(_))`: the value is the result.
    Return,
    /// `Machine.scala:122-141`
    ReadAt { default: Option<&'a Value>, scope: Scope<'a> },
    /// `Machine.scala:107`
    All { args: Args<'a>, i: usize, acc: Vec<Value>, scope: Scope<'a>, then: AllThen<'a> },
    /// `Machine.scala:166-169`
    Spread { op: Op },
    /// `Machine.scala:335`
    CondTest { branches: &'a [(Node, Node)], j: usize, otherwise: &'a Node, scope: Scope<'a> },
    /// `Machine.scala:351-358`
    Logic { kind: Logical, args: &'a [Node], i: usize, scope: Scope<'a> },
    /// `Machine.scala:363`: binding `i`'s value.
    BindValue { node: &'a node::Bindings, body: &'a Node, slot: usize, i: usize, outer: Scope<'a> },
    /// The `let` body's value: its bindings go out of scope.
    BindDone,
    /// `Machine.scala:374-378`
    Collection { op: Op, then: CollThen<'a> },
    /// `Machine.scala:197-200`
    QuorumMin { vs: Elements, body: &'a Node, scope: Scope<'a> },
    /// `Machine.scala:212`
    FoldInit { vs: Elements, body: &'a Node, scope: Scope<'a> },
    /// `Machine.scala:403-414`
    IterBody {
        kind: Iteration,
        vs: Elements,
        body: &'a Node,
        scope: Scope<'a>,
        i: usize,
        acc: Vec<Value>,
        picked: Vec<usize>,
        n: u64,
    },
    /// `Machine.scala:427`
    QuorumBody { vs: Elements, body: &'a Node, scope: Scope<'a>, min: u64, i: usize, n: u64 },
    /// `Machine.scala:438`
    FoldBody { vs: Elements, body: &'a Node, scope: Scope<'a>, i: usize },
}

/// One step of evaluation (`Machine.scala` `Step`).
enum Step<'a> {
    Done(Value),
    Fail(JlvmError),
    /// Pay, then do the work: nothing is done before it is paid for.
    Charge(Gas, Then<'a>),
    /// Evaluate the node, then continue with its value.
    Eval(&'a Node, Scope<'a>, Kont<'a>),
    /// Record a warning (free), then finish with the value.
    Note(Warning, Value),
}

fn result<'a>(r: Result<Value, JlvmError>) -> Step<'a> {
    r.map_or_else(Step::Fail, Step::Done)
}

fn len_u64(n: usize) -> u64 {
    u64::try_from(n).unwrap_or(u64::MAX)
}

// ------------------------------------------------------------------------------------------------ the machine

/// One evaluation's state: the inputs, the meter, and the values of the `let`s being evaluated (a stack: a `let`'s
/// bindings go out of scope when its body returns, and evaluation is depth-first).
pub(crate) struct Machine<'a> {
    schedule: GasSchedule,
    backend: &'a dyn CryptoBackend,
    data: &'a Value,
    slots: Vec<Vec<Value>>,
    meter: Meter,
}

/// What an evaluation ends with: the gas used, the value or error, and the warnings.
pub(crate) struct Ran {
    pub(crate) used: Gas,
    pub(crate) result: Result<Value, JlvmError>,
    pub(crate) warnings: Vec<Warning>,
}

impl<'a> Machine<'a> {
    pub(crate) fn run(
        schedule: GasSchedule,
        backend: &'a dyn CryptoBackend,
        root: &'a Node,
        data: &'a Value,
        env: &'a BTreeMap<String, Value>,
        limit: Gas,
    ) -> Ran {
        let mut m = Machine {
            schedule,
            backend,
            data,
            slots: Vec::new(),
            meter: Meter { used: Gas::ZERO, limit, warnings: Vec::new() },
        };
        let scope = Scope { bindings: Rc::new(Bindings::Host(env)), frames: None, frame_count: 0 };
        let result = m.drive(root, scope);
        Ran { used: m.meter.used, result, warnings: m.meter.warnings }
    }

    /// The direct driver (`Machine.direct`): the step, an explicit stack of continuations, and the meter.
    fn drive(&mut self, root: &'a Node, scope: Scope<'a>) -> Result<Value, JlvmError> {
        let mut ks: Vec<Kont<'a>> = Vec::new();
        let mut step = self.start(root, scope);
        loop {
            step = match step {
                Step::Done(v) => match ks.pop() {
                    Some(k) => self.resume(k, v),
                    None => return Ok(v),
                },
                Step::Fail(e) => return Err(e),
                Step::Charge(g, then) => {
                    let total = self.meter.used.plus(g);
                    if total <= self.meter.limit {
                        self.meter.used = total;
                        self.after(then)
                    } else {
                        self.meter.used = self.meter.limit;
                        return Err(JlvmError::OutOfGas {
                            required: total.value(),
                            available: self.meter.limit.value(),
                        });
                    }
                }
                Step::Eval(n, s, k) => {
                    if !matches!(k, Kont::Return) {
                        ks.push(k);
                    }
                    self.start(n, s)
                }
                Step::Note(w, v) => {
                    self.meter.warn(w);
                    Step::Done(v)
                }
            };
        }
    }

    /// The gas left: what a charge can still be paid from.
    fn left(&self) -> Gas {
        Gas::new(self.meter.limit.value().saturating_sub(self.meter.used.value()))
    }

    fn base(&self, op: Op) -> Gas {
        self.schedule.base(op)
    }

    fn sized(&self, op: Op, vs: &[Value]) -> Gas {
        self.schedule.sized(op, vs)
    }

    /// `Semantics.start` (`Machine.scala:111-237`).
    fn start(&self, node: &'a Node, scope: Scope<'a>) -> Step<'a> {
        let s = &self.schedule;
        match node {
            Node::Const(v) => Step::Charge(s.literal, Then::Done(v.clone())),
            Node::Read(r, d, segments) => self.read_static(r, d.as_ref(), *segments, scope),
            Node::ReadAt(path, d) => Step::Eval(path, scope.clone(), Kont::ReadAt { default: d.as_ref(), scope }),
            Node::ArrayOf(items) => Step::Charge(
                s.literal.plus(s.element.times(len_u64(items.len()))),
                Then::All { args: Args::Nodes(items), scope, then: AllThen::Array },
            ),
            Node::ObjectOf(shape, fields) => Step::Charge(
                s.literal.plus(s.element.times(len_u64(fields.len()))),
                Then::All { args: Args::Nodes(fields), scope, then: AllThen::Object(shape) },
            ),
            Node::Strict(op, args) => {
                Step::Charge(self.base(*op), Then::All { args: Args::Nodes(args), scope, then: AllThen::Strict(*op) })
            }
            Node::Spread(op, arg) => Step::Charge(self.base(*op), Then::SpreadArg { op: *op, arg, scope }),
            Node::Cond(branches, otherwise) => {
                Step::Charge(self.base(Op::If), Then::Cond { branches, otherwise, scope })
            }
            Node::Logic(kind, args) => Step::Charge(self.base(kind.op()), Then::Logic { kind: *kind, args, scope }),
            // each binding is an insertion into the scope, paid like a field of an object literal, and like a path
            // segment once more per KeyBlock of its name
            Node::Bind(b, body) => Step::Charge(
                self.base(Op::Let).plus(s.element.times(b.name_segments)),
                Then::Bind { node: b, body, scope },
            ),
            Node::Each(kind, over, body) => Step::Charge(
                self.base(kind.op()),
                Then::Collection { op: kind.op(), over, then: CollThen::Each { kind: *kind, body, scope } },
            ),
            Node::Quorum(over, body, min) => Step::Charge(
                self.base(Op::Some),
                Then::Collection { op: Op::Some, over, then: CollThen::Quorum { body, min: min.as_deref(), scope } },
            ),
            Node::Fold(over, body, init) => Step::Charge(
                self.base(Op::Reduce),
                Then::Collection { op: Op::Reduce, over, then: CollThen::Fold { body, init: init.as_deref(), scope } },
            ),
            Node::Lookup(op, args) => {
                Step::Charge(self.base(*op), Then::All { args: Args::Nodes(args), scope, then: AllThen::Lookup(*op) })
            }
            Node::Crypto(op, args) => {
                Step::Charge(self.base(*op), Then::All { args: Args::Nodes(args), scope, then: AllThen::Crypto(*op) })
            }
        }
    }

    /// The work after a charge.
    #[allow(clippy::too_many_lines, reason = "one arm per continuation, each citing the reference")]
    fn after(&mut self, then: Then<'a>) -> Step<'a> {
        match then {
            Then::Done(v) => Step::Done(v),
            Then::Lookup { path, default, scope } => self.lookup(path.get(), default, &scope),
            Then::ParsePath { text, default, scope } => match VarRef::parse(&text) {
                Ok(r) => self.read(PathRef::Owned(r), default, scope),
                Err(e) => Step::Fail(JlvmError::domain("var", alloc::format!("{e}"))),
            },
            Then::SegmentPath { segments, default, scope } => {
                match segments.items().iter().map(segment).collect::<Result<Vec<_>, _>>() {
                    Ok(p) => self.lookup(&VarRef::Data(p), default, &scope),
                    Err(e) => Step::Fail(e),
                }
            }
            Then::All { args, scope, then } => self.all(args, 0, Vec::new(), scope, then),
            Then::Operate { op, vs } => self.operate(op, vs),
            Then::OperateOn { op, array } => self.operate(op, array.into_items()),
            Then::SpreadArg { op, arg, scope } => Step::Eval(arg, scope, Kont::Spread { op }),
            Then::Cond { branches, otherwise, scope } => self.cond(branches, 0, otherwise, scope),
            Then::CondTest { branches, j, otherwise, scope } => match branches.get(j) {
                Some((c, _)) => Step::Eval(c, scope.clone(), Kont::CondTest { branches, j, otherwise, scope }),
                None => Step::Eval(otherwise, scope, Kont::Return),
            },
            Then::Logic { kind, args, scope } => Self::logic(kind, args, 0, scope),
            Then::Bind { node, body, scope } => {
                let slot = self.slots.len();
                self.slots.push(Vec::with_capacity(node.bindings.len()));
                Self::bind(node, body, slot, 0, scope)
            }
            Then::Collection { op, over, then } => {
                let scope = then.scope().clone();
                Step::Eval(over, scope, Kont::Collection { op, then })
            }
            Then::Proceed { then, vs } => self.proceed(then, vs),
            Then::IterRun { kind, vs, body, scope, i, acc, picked, n } => {
                let inner = element(&scope, &vs, i);
                Step::Eval(body, inner, Kont::IterBody { kind, vs, body, scope, i, acc, picked, n })
            }
            Then::QuorumRun { vs, body, scope, min, i, n } => {
                let inner = element(&scope, &vs, i);
                Step::Eval(body, inner, Kont::QuorumBody { vs, body, scope, min, i, n })
            }
            Then::FoldRun { vs, body, scope, i, acc } => {
                let inner = accumulating(&scope, &vs, i, acc);
                Step::Eval(body, inner, Kont::FoldBody { vs, body, scope, i })
            }
            Then::Accumulate { op, rest, i, acc } => self.combine(op, rest, i, &acc),
            Then::JoinPieces { vs } => match ops::joining(vs) {
                Ok(j) => {
                    let g = self.schedule.per_unit(Op::Join).times(j.separator_bytes());
                    Step::Charge(g, Then::JoinBuild { j })
                }
                Err(e) => Step::Fail(e),
            },
            Then::JoinBuild { j } => result(ops::joined(&j)),
            Then::Cat { vs } => result(ops::apply(Op::Cat, vs)),
            Then::LookupRun { op, vs, scope } => {
                let data = self.data;
                result(lookups(op, &vs, |p| *resolve(data, &scope, p).0 == Value::Null))
            }
            Then::CryptoRun { op, vs } => result(crypto::apply(self.backend, op, &vs)),
        }
    }

    /// What a continuation does with a value.
    #[allow(clippy::too_many_lines, reason = "one arm per continuation, each citing the reference")]
    fn resume(&mut self, k: Kont<'a>, v: Value) -> Step<'a> {
        match k {
            Kont::Return => Step::Done(v),
            Kont::ReadAt { default, scope } => match v {
                // the path stays shared until its charge is paid
                Value::Text(ref t) => {
                    let g = self.schedule.path_text(&v);
                    Step::Charge(g, Then::ParsePath { text: t.clone(), default, scope })
                }
                // each segment's count is O(1) (text caches its UTF-16 length), and the segments are counted only as far
                // as the gas left can pay for
                Value::Array(ss) => {
                    let g = if ss.is_empty() {
                        self.read_cost(0)
                    } else {
                        let left = self.left();
                        ss.items()
                            .iter()
                            .try_fold(self.read_cost(scope.frame_count), |g, s| {
                                let next = g.plus(self.schedule.per_path_segment.times(match s {
                                    Value::Text(t) => text_segment_count(t),
                                    _ => 1,
                                }));
                                if next > left { Err(next) } else { Ok(next) }
                            })
                            .unwrap_or_else(|over| over)
                    };
                    Step::Charge(g, Then::SegmentPath { segments: ss, default, scope })
                }
                other => Step::Fail(JlvmError::type_error(
                    "var",
                    alloc::format!("a computed path is text or an array of segments, got {}", kind(&other)),
                )),
            },
            Kont::All { args, i, mut acc, scope, then } => {
                acc.push(v);
                self.all(args, i.saturating_add(1), acc, scope, then)
            }
            // the elements are sized where they are, and taken out of the array only once that is paid
            Kont::Spread { op } => match v {
                Value::Array(a) => {
                    let g = self.schedule.sized_within(op, a.items(), self.left());
                    Step::Charge(g, Then::OperateOn { op, array: a })
                }
                other => Step::Fail(JlvmError::type_error(
                    op.wire(),
                    alloc::format!("a spread argument is an array, got {}", kind(&other)),
                )),
            },
            Kont::CondTest { branches, j, otherwise, scope } => {
                if truthy(&v) {
                    match branches.get(j) {
                        Some((_, t)) => Step::Eval(t, scope, Kont::Return),
                        None => Step::Eval(otherwise, scope, Kont::Return),
                    }
                } else {
                    self.cond(branches, j.saturating_add(1), otherwise, scope)
                }
            }
            Kont::Logic { kind, args, i, scope } => {
                let decisive = if kind == Logical::And { !truthy(&v) } else { truthy(&v) };
                let last = i.saturating_add(1) == args.len();
                if decisive || (last && kind != Logical::Default) {
                    Step::Done(v)
                } else {
                    Self::logic(kind, args, i.saturating_add(1), scope)
                }
            }
            Kont::BindValue { node, body, slot, i, outer } => {
                if let Some(values) = self.slots.get_mut(slot) {
                    values.push(v);
                }
                Self::bind(node, body, slot, i.saturating_add(1), outer)
            }
            Kont::BindDone => {
                self.slots.pop();
                Step::Done(v)
            }
            Kont::Collection { op, then } => match v {
                Value::Array(a) => {
                    let vs: Elements = a;
                    let g = self.schedule.per_unit(op).times(len_u64(vs.len()));
                    Step::Charge(g, Then::Proceed { then, vs })
                }
                Value::Null => match dilf4s_core::value::Array::new(Vec::new()) {
                    Ok(empty) => self.proceed(then, empty),
                    Err(e) => Step::Fail(JlvmError::limit(alloc::format!("{e}"))),
                },
                other => Step::Fail(JlvmError::type_error(
                    op.wire(),
                    alloc::format!("expected an array, got {}", kind(&other)),
                )),
            },
            Kont::QuorumMin { vs, body, scope } => match values::integer(Op::Some, &v) {
                Ok(n) => {
                    let min = if num_traits::Signed::is_negative(n) {
                        0
                    } else {
                        num_traits::ToPrimitive::to_i64(n).map_or(i64::MAX.unsigned_abs(), i64::unsigned_abs)
                    };
                    self.quorum(vs, body, scope, min, 0, 0)
                }
                Err(e) => Step::Fail(e),
            },
            Kont::FoldInit { vs, body, scope } => self.fold(vs, body, scope, 0, v),
            Kont::IterBody { kind, vs, body, scope, i, mut acc, mut picked, n } => {
                let t = truthy(&v);
                let next = i.saturating_add(1);
                match kind {
                    Iteration::Map => {
                        acc.push(v);
                        self.iterate(kind, vs, body, scope, next, acc, picked, n)
                    }
                    Iteration::Filter => {
                        if t {
                            picked.push(i);
                        }
                        self.iterate(kind, vs, body, scope, next, acc, picked, n)
                    }
                    Iteration::All if !t => Step::Done(Value::Bool(false)),
                    Iteration::None if t => Step::Done(Value::Bool(false)),
                    Iteration::Find if t => Step::Done(vs.items().get(i).cloned().unwrap_or(Value::Null)),
                    Iteration::Count => {
                        let n = if t { n.saturating_add(1) } else { n };
                        self.iterate(kind, vs, body, scope, next, acc, picked, n)
                    }
                    Iteration::All | Iteration::None | Iteration::Find => {
                        self.iterate(kind, vs, body, scope, next, acc, picked, n)
                    }
                }
            }
            Kont::QuorumBody { vs, body, scope, min, i, n } => {
                let n = if truthy(&v) { n.saturating_add(1) } else { n };
                self.quorum(vs, body, scope, min, i.saturating_add(1), n)
            }
            Kont::FoldBody { vs, body, scope, i } => self.fold(vs, body, scope, i.saturating_add(1), v),
        }
    }

    // ---- variables

    /// The access, and each segment.
    fn read_cost(&self, segments: u64) -> Gas {
        self.schedule.var_access.plus(self.schedule.per_path_segment.times(segments))
    }

    /// A static read: its segments were counted at compilation; a non-empty plain path inside callbacks also counts
    /// each enclosing frame (`Machine.scala:249-257`).
    fn read_static(&self, r: &'a VarRef, default: Option<&'a Value>, segments: u64, scope: Scope<'a>) -> Step<'a> {
        let segments = match r {
            VarRef::Data(p) if !p.is_empty() => segments.saturating_add(scope.frame_count),
            _ => segments,
        };
        Step::Charge(self.read_cost(segments), Then::Lookup { path: PathRef::Static(r), default, scope })
    }

    /// Charges for the access and each segment, then reads (`Machine.scala:249-257`).
    fn read(&self, path: PathRef<'a>, default: Option<&'a Value>, scope: Scope<'a>) -> Step<'a> {
        let count = |p: &[String]| p.iter().fold(0u64, |n, s| n.saturating_add(segment_count(s)));
        let segments = match path.get() {
            VarRef::Data(p) if !p.is_empty() => count(p).saturating_add(scope.frame_count),
            VarRef::Data(_) => 0,
            VarRef::Bound(n, p) => segment_count(n).saturating_add(count(p)),
        };
        Step::Charge(self.read_cost(segments), Then::Lookup { path, default, scope })
    }

    /// Reads a path already paid for (`Machine.scala:260-272`).
    fn lookup(&self, r: &VarRef, default: Option<&'a Value>, scope: &Scope<'a>) -> Step<'a> {
        let or_default = |v: &Value| {
            if *v == Value::Null { default.cloned().unwrap_or(Value::Null) } else { v.clone() }
        };
        match r {
            VarRef::Data(path) => {
                let (v, ambiguous) = resolve(self.data, scope, path);
                let value = or_default(v);
                if ambiguous { Step::Note(Warning::AmbiguousPath(r.render()), value) } else { Step::Done(value) }
            }
            VarRef::Bound(name, path) => match binding(&self.slots, scope, name) {
                Some(v) => Step::Done(or_default(at(v, path))),
                None => match default {
                    Some(d) => Step::Done(d.clone()),
                    None => Step::Fail(JlvmError::malformed(alloc::format!("${name} is not bound"))),
                },
            },
        }
    }

    // ---- strict operations

    /// `Machine.scala:105-109`: evaluates the arguments from `i` on, then continues with all their values.
    fn all(&self, args: Args<'a>, i: usize, acc: Vec<Value>, scope: Scope<'a>, then: AllThen<'a>) -> Step<'a> {
        match args.get(i) {
            Some(n) => Step::Eval(n, scope.clone(), Kont::All { args, i, acc, scope, then }),
            None => self.all_done(then, acc, scope),
        }
    }

    fn all_done(&self, then: AllThen<'a>, vs: Vec<Value>, scope: Scope<'a>) -> Step<'a> {
        match then {
            AllThen::Array => result(values::array("array", vs)),
            AllThen::Object(shape) => result(
                shape
                    .map(&vs)
                    .map_err(|e| JlvmError::limit(alloc::format!("object: {e}")))
                    .and_then(|m| values::bounded("object", Value::Map(m))),
            ),
            AllThen::Strict(op) => Step::Charge(self.sized(op, &vs), Then::Operate { op, vs }),
            // each key is a path parsed at run time, and searches every enclosing frame: both paid before resolving
            AllThen::Lookup(op) => {
                // each key's charge is O(1), and the keys are priced only as far as the gas left can pay for
                let keys = lookup_paths(op, &vs);
                let left = self.left();
                let frames =
                    self.schedule.per_path_segment.times(scope.frame_count.saturating_mul(len_u64(keys.len())));
                let start = self.sized(op, &vs).plus(frames);
                let g = keys
                    .iter()
                    .try_fold(start, |g, k| {
                        let next = g.plus(self.schedule.path_text(k));
                        if next > left { Err(next) } else { Ok(next) }
                    })
                    .unwrap_or_else(|over| over);
                Step::Charge(g, Then::LookupRun { op, vs, scope })
            }
            AllThen::Crypto(op) => Step::Charge(self.sized(op, &vs), Then::CryptoRun { op, vs }),
        }
    }

    /// An operation after its size charge (`Machine.scala:292-312`): `+` and `*` fold in charged steps; `cat` and
    /// `join` pay for rendering numbers before they render them, and `join` for its separators before building.
    fn operate(&self, op: Op, vs: Vec<Value>) -> Step<'a> {
        match op {
            Op::Add | Op::Mul => match ops::operands(op, &vs) {
                Ok(rs) => {
                    let mut it = rs.into_iter();
                    match it.next() {
                        Some(first) => self.accumulate(op, it.collect(), 0, first),
                        None => Step::Done(ops::identity(op)),
                    }
                }
                Err(e) => Step::Fail(e),
            },
            Op::Join => {
                let g = self.schedule.rendered(ops::rendered(op, &vs));
                Step::Charge(g, Then::JoinPieces { vs })
            }
            Op::Cat => {
                let g = self.schedule.rendered(ops::rendered(op, &vs));
                Step::Charge(g, Then::Cat { vs })
            }
            _ => result(ops::apply(op, vs)),
        }
    }

    /// `+` or `*`, left to right (`Machine.scala:322-327`): each step after the first pays the size charge of its
    /// accumulator before it runs.
    fn accumulate(&self, op: Op, rest: Vec<Ratio>, i: usize, acc: Ratio) -> Step<'a> {
        if i >= rest.len() {
            result(values::num(op, &acc))
        } else if i == 0 {
            self.combine(op, rest, i, &acc)
        } else {
            let g = self.sized(op, core::slice::from_ref(&acc.to_value()));
            Step::Charge(g, Then::Accumulate { op, rest, i, acc })
        }
    }

    fn combine(&self, op: Op, rest: Vec<Ratio>, i: usize, acc: &Ratio) -> Step<'a> {
        let Some(x) = rest.get(i) else {
            return result(values::num(op, acc));
        };
        match ops::combine(op, acc, x) {
            Ok(r) => self.accumulate(op, rest, i.saturating_add(1), r),
            Err(e) => Step::Fail(e),
        }
    }

    // ---- lazy forms

    /// Each condition tested is charged one element (`Machine.scala:332-337`).
    fn cond(&self, branches: &'a [(Node, Node)], j: usize, otherwise: &'a Node, scope: Scope<'a>) -> Step<'a> {
        if j < branches.len() {
            Step::Charge(self.schedule.element, Then::CondTest { branches, j, otherwise, scope })
        } else {
            Step::Eval(otherwise, scope, Kont::Return)
        }
    }

    /// `and` stops at the first falsy value, `or` and `default` at the first truthy one (`Machine.scala:340-359`).
    fn logic(kind: Logical, args: &'a [Node], i: usize, scope: Scope<'a>) -> Step<'a> {
        match args.get(i) {
            None => Step::Done(match kind {
                Logical::And => Value::Bool(true),
                Logical::Or => Value::Bool(false),
                Logical::Default => Value::Null,
            }),
            Some(n) => Step::Eval(n, scope.clone(), Kont::Logic { kind, args, i, scope }),
        }
    }

    /// Binding `i`, seeing the ones before it; then the body, seeing all (`Machine.scala:361-365`).
    fn bind(node: &'a node::Bindings, body: &'a Node, slot: usize, i: usize, outer: Scope<'a>) -> Step<'a> {
        let scope = Scope {
            bindings: Rc::new(Bindings::Let { parent: outer.bindings.clone(), node, slot, count: i }),
            frames: outer.frames.clone(),
            frame_count: outer.frame_count,
        };
        match node.bindings.get(i) {
            Some((_, x)) => Step::Eval(x, scope, Kont::BindValue { node, body, slot, i, outer }),
            None => Step::Eval(body, scope, Kont::BindDone),
        }
    }

    // ---- callbacks

    /// The callback form, once its collection is evaluated and paid for.
    fn proceed(&self, then: CollThen<'a>, vs: Elements) -> Step<'a> {
        match then {
            CollThen::Each { kind, body, scope } => self.iterate(kind, vs, body, scope, 0, Vec::new(), Vec::new(), 0),
            CollThen::Quorum { body, min, scope } => match min {
                None => self.quorum(vs, body, scope, 1, 0, 0),
                Some(m) => Step::Eval(m, scope.clone(), Kont::QuorumMin { vs, body, scope }),
            },
            CollThen::Fold { body, init, scope } => match init {
                None => match vs.items().first().cloned() {
                    None => Step::Done(Value::Null),
                    Some(first) => self.fold(vs, body, scope, 1, first),
                },
                Some(x) => Step::Eval(x, scope.clone(), Kont::FoldInit { vs, body, scope }),
            },
        }
    }

    #[allow(clippy::too_many_arguments, reason = "the reference's `go(i, acc, n)` over the callback's context")]
    fn iterate(
        &self,
        kind: Iteration,
        vs: Elements,
        body: &'a Node,
        scope: Scope<'a>,
        i: usize,
        acc: Vec<Value>,
        picked: Vec<usize>,
        n: u64,
    ) -> Step<'a> {
        if i < vs.len() {
            return Step::Charge(
                self.schedule.callback_run,
                Then::IterRun { kind, vs, body, scope, i, acc, picked, n },
            );
        }
        match kind {
            Iteration::Map => result(values::array(kind.op().wire(), acc)),
            Iteration::Filter => {
                let mut keep = picked.into_iter().peekable();
                let kept = vs
                    .items()
                    .iter()
                    .enumerate()
                    .filter(|(j, _)| keep.next_if_eq(j).is_some())
                    .map(|(_, v)| v.clone())
                    .collect();
                result(values::array(kind.op().wire(), kept))
            }
            Iteration::All => Step::Done(Value::Bool(!vs.is_empty())),
            Iteration::None => Step::Done(Value::Bool(true)),
            Iteration::Find => Step::Done(Value::Null),
            Iteration::Count => Step::Done(values::count(n)),
        }
    }

    /// Stops as soon as `min` elements satisfy the body (`Machine.scala:421-428`).
    fn quorum(&self, vs: Elements, body: &'a Node, scope: Scope<'a>, min: u64, i: usize, n: u64) -> Step<'a> {
        if n >= min {
            Step::Done(Value::Bool(true))
        } else if i >= vs.len() {
            Step::Done(Value::Bool(false))
        } else {
            Step::Charge(self.schedule.callback_run, Then::QuorumRun { vs, body, scope, min, i, n })
        }
    }

    /// `Machine.scala:433-439`
    fn fold(&self, vs: Elements, body: &'a Node, scope: Scope<'a>, i: usize, acc: Value) -> Step<'a> {
        if i < vs.len() {
            Step::Charge(self.schedule.callback_run, Then::FoldRun { vs, body, scope, i, acc })
        } else {
            Step::Done(acc)
        }
    }
}

/// The element is bound as `$current` and is the innermost frame for plain paths (`Machine.scala:382-383`).
fn element<'a>(scope: &Scope<'a>, vs: &Elements, i: usize) -> Scope<'a> {
    let current = vs.items().get(i).cloned().unwrap_or(Value::Null);
    scope.enter(
        Bindings::Element {
            parent: scope.bindings.clone(),
            current: current.clone(),
            index: values::count(len_u64(i)),
        },
        Frame::Element(current),
    )
}

/// `Machine.scala:430-431`
fn accumulating<'a>(scope: &Scope<'a>, vs: &Elements, i: usize, acc: Value) -> Scope<'a> {
    let current = vs.items().get(i).cloned().unwrap_or(Value::Null);
    scope.enter(
        Bindings::Accumulating {
            parent: scope.bindings.clone(),
            current: current.clone(),
            index: values::count(len_u64(i)),
            accumulator: acc.clone(),
        },
        Frame::Accumulating(current, acc),
    )
}

/// A segment of a computed data path (`Machine.scala:278-284`): text is an exact key; an integer is an index, or the
/// key that is its decimal form, bounded to 64 bits before it is rendered.
fn segment(v: &Value) -> Result<String, JlvmError> {
    match v {
        Value::Text(t) => Ok(String::from(t.as_str())),
        Value::Int(i) => values::small_integer_text(i)
            .ok_or_else(|| JlvmError::domain("var", "an integer path segment must fit in 64 bits")),
        other => Err(JlvmError::type_error(
            "var",
            alloc::format!("a path segment is text or an integer, got {}", kind(other)),
        )),
    }
}

// ---- lookups (Lookups.scala)

/// The values a lookup reads as paths, unchecked: what its charge is computed from, before any is parsed.
fn lookup_paths(op: Op, args: &[Value]) -> &[Value] {
    match (op, args) {
        (Op::Missing, [Value::Array(ks)]) | (Op::MissingSome, [_, Value::Array(ks)]) => ks.items(),
        (Op::Missing, ks) => ks,
        _ => &[],
    }
}

/// `missing` and `missing_some`: which data paths are absent, as `absent` says.
fn lookups(op: Op, args: &[Value], absent: impl Fn(&[String]) -> bool) -> Result<Value, JlvmError> {
    let keys = |ks: &[Value]| -> Result<Vec<String>, JlvmError> {
        let list = match ks {
            [Value::Array(inner)] => inner.items(),
            other => other,
        };
        list.iter()
            .map(|k| match k {
                Value::Text(t) => Ok(String::from(t.as_str())),
                other => {
                    Err(JlvmError::type_error(op.wire(), alloc::format!("a key is a string path, got {}", kind(other))))
                }
            })
            .collect()
    };
    let missing = |ks: Vec<String>| -> Vec<Value> {
        ks.into_iter()
            .filter(|k| if k.is_empty() { absent(&[]) } else { absent(&VarRef::segments(k)) })
            .map(Value::text)
            .collect()
    };
    match op {
        Op::Missing => values::array(op.wire(), missing(keys(args)?)),
        Op::MissingSome => match args {
            [min, ks @ Value::Array(_)] => {
                let n = values::integer(op, min)?.clone();
                let all = keys(core::slice::from_ref(ks))?;
                let total = len_u64(all.len());
                let gone = missing(all);
                let present = total.saturating_sub(len_u64(gone.len()));
                if dilf4s_core::BigInt::from(present) >= n {
                    values::array(op.wire(), Vec::new())
                } else {
                    values::array(op.wire(), gone)
                }
            }
            _ => Err(JlvmError::arity(op.wire(), "takes [minimum, [keys]]")),
        },
        other => Err(JlvmError::malformed(alloc::format!("{} is not a lookup", other.wire()))),
    }
}
