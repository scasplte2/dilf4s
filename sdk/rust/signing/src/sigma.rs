//! Sigma protocols over secp256k1, byte-compatible with Ergo (`docs/specs/sigma.md`): propositions and the verifier.
//!
//! A [`Proposition`] is a tree of `and`, `or` and `threshold` over `dlog` and `dhtuple` statements. It exists only
//! through its checked constructors ([`Proposition::dlog`], [`Proposition::and`], ..., [`Proposition::from_value`]),
//! so every proposition meets section 3: statement points on the curve and not the identity, 1 to 255 children per
//! connective, `1 <= k <= n`, at most 64 levels and 4096 leaves. It is held flat, in pre-order, so decoding, the proof
//! walk and the transcript are loops: nothing recurses on the shape of the input.
//!
//! - [`verify`] checks a proof of a proposition for a message.
//! - [`verify_value`] decodes the proposition from its canonical form first, as the JLVM's `sigma_verify` does.
//! - [`verify_dlog_bytes`] checks a single-key proof (Ergo's P2PK signature), as the JLVM's `schnorr_verify` does.
//!
//! Malformed input is a [`SigmaError`]; a well-formed proof that fails is `Ok(false)`.

mod gf2_192;

use alloc::boxed::Box;
use alloc::format;
use alloc::string::String;
use alloc::vec::Vec;

use dilf4s_core::hash::{Blake2b256, Hasher as _};
use dilf4s_core::{Map, Value};
use k256::elliptic_curve::Group as _;
use k256::elliptic_curve::PrimeField as _;
use k256::elliptic_curve::ops::Reduce;
use k256::elliptic_curve::point::DecompressPoint as _;
use k256::elliptic_curve::sec1::ToEncodedPoint as _;
use k256::elliptic_curve::subtle::Choice;
use k256::{AffinePoint, FieldBytes, ProjectivePoint, Scalar, U256};

use self::gf2_192::{Gf2_192, evaluate};
use crate::error::SigmaError;
use crate::util::{SECP256K1_P, be_less_than};

/// Bytes of a challenge: 192-bit soundness.
pub const CHALLENGE_LENGTH: usize = 24;

/// Bytes of an encoded group element: SEC1 compressed, or 33 zero bytes for the identity.
pub const ELEMENT_LENGTH: usize = 33;

/// Bytes of a response `z`: a big-endian scalar below the group order.
pub const SCALAR_LENGTH: usize = 32;

/// The deepest a proposition nests (a leaf alone is depth 1).
pub const MAX_DEPTH: usize = 64;

/// The most leaves a proposition has.
pub const MAX_LEAVES: usize = 4096;

/// The most children of one connective: a threshold's evaluation points are the bytes `1..=n`.
pub const MAX_CHILDREN: usize = 255;

/// The weight of the heaviest valid proposition in its canonical form (section 3): 4096 `dhtuple` leaves (158 each)
/// at depth 64, under `1 + 255 + 61·4096` one-`k` thresholds (29 each, plus their children). A heavier value is
/// refused before any of it is decoded.
pub const MAX_WEIGHT: u64 = 7_900_416;

type Challenge = [u8; CHALLENGE_LENGTH];

type Encoded = [u8; ELEMENT_LENGTH];

/// The encoding of the identity.
const IDENTITY: Encoded = [0; ELEMENT_LENGTH];

/// A leaf statement is Ergo's `ErgoTree` for `SigmaPropConstant(leaf)`, constants segregated: this header (version 0,
/// segregated, one constant of type `SigmaProp`), the leaf's opcode and points, then [`TREE_BODY`].
const TREE_HEADER: [u8; 3] = [0x10, 0x01, 0x08];

/// The body of a leaf statement: `ConstantPlaceholder(0)`.
const TREE_BODY: [u8; 2] = [0x73, 0x00];

/// `ProveDlog`.
const DLOG_OPCODE: u8 = 0xcd;

/// `ProveDHTuple`.
const DH_TUPLE_OPCODE: u8 = 0xce;

/// A statement point: on the curve, never the identity, with its canonical encoding.
#[derive(Clone, Copy, Debug)]
struct Element {
    bytes: Encoded,
    point: AffinePoint,
}

impl PartialEq for Element {
    fn eq(&self, other: &Self) -> bool {
        self.bytes == other.bytes
    }
}

impl Eq for Element {}

/// A group element from its canonical encoding (section 1); `None` is the identity. Only canonical encodings decode:
/// 33 bytes, prefix `02` or `03` with `x` below the field modulus and on the curve, or 33 zero bytes.
fn decode_element(what: &'static str, bytes: &[u8]) -> Result<Option<Element>, SigmaError> {
    let invalid = |reason: String| SigmaError::InvalidPoint { what, reason };
    let encoded: Encoded = bytes
        .try_into()
        .map_err(|_| invalid(format!("a group element is {ELEMENT_LENGTH} bytes, got {}", bytes.len())))?;
    if encoded == IDENTITY {
        return Ok(None);
    }
    let [prefix, x @ ..] = encoded;
    let y_is_odd = match prefix {
        0x02 => 0,
        0x03 => 1,
        _ => return Err(invalid("a group element is compressed: prefix 02 or 03".into())),
    };
    if !be_less_than(&x, &SECP256K1_P) {
        return Err(invalid("not a point of secp256k1: x is not below the field modulus".into()));
    }
    let point: Option<AffinePoint> = AffinePoint::decompress(&FieldBytes::from(x), Choice::from(y_is_odd)).into();
    let point = point.ok_or_else(|| invalid("not a point of secp256k1".into()))?;
    if encode(&ProjectivePoint::from(point)) != encoded {
        return Err(invalid("not the canonical encoding of a point".into()));
    }
    Ok(Some(Element { bytes: encoded, point }))
}

/// A statement point is never the identity: `dlog(O)` holds for `w = 0`, so anyone could prove it, and likewise a
/// tuple with `u` or `v` the identity; an identity base proves nothing.
fn statement_point(what: &str, decoded: Option<Element>) -> Result<Element, SigmaError> {
    decoded.ok_or_else(|| invalid(format!("{what} is the identity, for which anyone can prove knowledge (w = 0)")))
}

/// 33 bytes: SEC1 compressed, or 33 zero bytes for the identity (Ergo's encoding, not SEC1's single zero byte).
fn encode(point: &ProjectivePoint) -> Encoded {
    let mut out = IDENTITY;
    if !bool::from(point.is_identity()) {
        for (o, b) in out.iter_mut().zip(point.to_affine().to_encoded_point(true).as_bytes()) {
            *o = *b;
        }
    }
    out
}

fn invalid(reason: impl Into<String>) -> SigmaError {
    SigmaError::InvalidProposition { reason: reason.into() }
}

fn too_wide() -> SigmaError {
    invalid(format!("a connective has at most {MAX_CHILDREN} children"))
}

/// One node of a proposition, in pre-order: a connective is followed by its `n` children's subtrees.
#[derive(Clone, Debug, PartialEq, Eq)]
enum Node {
    Dlog(Box<Element>),
    DhTuple(Box<[Element; 4]>),
    And(u8),
    Or(u8),
    Threshold { k: u8, n: u8 },
}

/// A connective before its children are checked; a threshold's `k` as given.
enum Connective {
    And,
    Or,
    Threshold(i64),
}

/// Section 3's checks on a connective with `n` children, in the reference's order (`SigmaProp.threshold` then
/// `SigmaProp.conjecture`): its header and its depth.
fn close(kind: &Connective, n: usize, child_depth: usize, leaves: usize) -> Result<(Node, usize), SigmaError> {
    if let Connective::Threshold(k) = kind {
        if !usize::try_from(*k).is_ok_and(|k| (1..=n).contains(&k)) {
            return Err(invalid(format!("a threshold needs 1 <= k <= {n} children, got k = {k}")));
        }
    }
    if n == 0 {
        return Err(invalid("a connective needs at least one child: an empty one holds for anyone"));
    }
    let n = u8::try_from(n).map_err(|_| too_wide())?;
    let depth = child_depth.saturating_add(1);
    if depth > MAX_DEPTH {
        return Err(invalid(format!("a proposition nests at most {MAX_DEPTH} deep")));
    }
    if leaves > MAX_LEAVES {
        return Err(invalid(format!("a proposition has at most {MAX_LEAVES} leaves")));
    }
    let header = match kind {
        Connective::And => Node::And(n),
        Connective::Or => Node::Or(n),
        Connective::Threshold(k) => Node::Threshold { k: u8::try_from(*k).map_err(|_| too_wide())?, n },
    };
    Ok((header, depth))
}

/// A sigma proposition (`docs/specs/sigma.md` section 3), valid by construction.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Proposition {
    /// Pre-order.
    nodes: Vec<Node>,
    depth: usize,
    leaves: usize,
}

impl Proposition {
    fn leaf(node: Node) -> Self {
        Self { nodes: alloc::vec![node], depth: 1, leaves: 1 }
    }

    /// Knowledge of `w` with `pk = w·G`. `pk` is a canonical, non-identity group element.
    pub fn dlog(pk: &[u8]) -> Result<Self, SigmaError> {
        let pk = statement_point("pk", decode_element("pk", pk)?)?;
        Ok(Self::leaf(Node::Dlog(Box::new(pk))))
    }

    /// Knowledge of `w` with `u = w·g` and `v = w·h`. Each point is a canonical, non-identity group element; all four
    /// are decoded before any is checked against the identity, as the reference does.
    pub fn dh_tuple(g: &[u8], h: &[u8], u: &[u8], v: &[u8]) -> Result<Self, SigmaError> {
        let (g, h, u, v) =
            (decode_element("g", g)?, decode_element("h", h)?, decode_element("u", u)?, decode_element("v", v)?);
        Ok(Self::leaf(Node::DhTuple(Box::new([
            statement_point("g", g)?,
            statement_point("h", h)?,
            statement_point("u", u)?,
            statement_point("v", v)?,
        ]))))
    }

    /// Every child holds.
    pub fn and(children: Vec<Self>) -> Result<Self, SigmaError> {
        Self::connective(&Connective::And, children)
    }

    /// At least one child holds; which one is hidden.
    pub fn or(children: Vec<Self>) -> Result<Self, SigmaError> {
        Self::connective(&Connective::Or, children)
    }

    /// At least `k` children hold; which ones are hidden.
    pub fn threshold(k: usize, children: Vec<Self>) -> Result<Self, SigmaError> {
        Self::connective(&Connective::Threshold(i64::try_from(k).unwrap_or(i64::MAX)), children)
    }

    fn connective(kind: &Connective, children: Vec<Self>) -> Result<Self, SigmaError> {
        let child_depth = children.iter().map(|c| c.depth).max().unwrap_or(0);
        let leaves = children.iter().fold(0usize, |n, c| n.saturating_add(c.leaves));
        let (header, depth) = close(kind, children.len(), child_depth, leaves)?;
        let mut nodes = Vec::with_capacity(children.iter().fold(1usize, |n, c| n.saturating_add(c.nodes.len())));
        nodes.push(header);
        for child in children {
            nodes.extend(child.nodes);
        }
        Ok(Self { nodes, depth, leaves })
    }

    /// The canonical form (section 3), as the JLVM passes it: `{"type": "dlog", "pk": bytes}`, `{"type": "dhtuple",
    /// "g", "h", "u", "v": bytes}`, `{"type": "and" | "or", "children": [...]}` or `{"type": "threshold", "k": int,
    /// "children": [...]}`, with no other field. Decoded with an explicit stack, in the reference's order
    /// (`SigmaProp.fromCanonical`, section 3): a value heavier than [`MAX_WEIGHT`] first, before anything is decoded;
    /// then, in pre-order, each node's own fields, the leaf bound before a leaf's points and the width before a
    /// connective's children; and each connective's remaining checks once its children are decoded.
    ///
    /// The work is bounded however much `value` shares: the weight is cached, so the first check is O(1), and what
    /// passes it is at most [`MAX_WEIGHT`] of logical size, of which at most [`MAX_LEAVES`] leaves are decoded.
    pub fn from_value(value: &Value) -> Result<Self, SigmaError> {
        if value.weight() > MAX_WEIGHT {
            return Err(invalid(format!("a proposition weighs at most {MAX_WEIGHT}")));
        }
        let mut nodes = Vec::new();
        let mut stack: Vec<Open<'_>> = Vec::new();
        let mut step = Step::Decode(value);
        // every leaf decoded so far, in the whole proposition
        let mut leaves = 0usize;
        loop {
            step = match step {
                Step::Decode(v) => match decode_node(v, leaves)? {
                    Decoded::Leaf(node) => {
                        nodes.push(node);
                        leaves = leaves.saturating_add(1);
                        Step::Completed { depth: 1, leaves: 1 }
                    }
                    Decoded::Connective(kind, items) => {
                        let header = nodes.len();
                        // a placeholder until the children are decoded and the connective checked
                        nodes.push(Node::And(0));
                        advance(Open { kind, items, next: 0, header, depth: 0, leaves: 0 }, &mut stack, &mut nodes)?
                    }
                },
                Step::Completed { depth, leaves } => match stack.pop() {
                    None => return Ok(Self { nodes, depth, leaves }),
                    Some(mut parent) => {
                        parent.depth = parent.depth.max(depth);
                        parent.leaves = parent.leaves.saturating_add(leaves);
                        advance(parent, &mut stack, &mut nodes)?
                    }
                },
            };
        }
    }

    /// The exact length of every proof of this proposition (section 5):
    /// `24 + Σ leaves 32 + Σ or-nodes 24·(n−1) + Σ threshold-nodes 24·(n−k)`.
    pub fn proof_length(&self) -> usize {
        self.nodes.iter().fold(CHALLENGE_LENGTH, |length, node| {
            length.saturating_add(match node {
                Node::Dlog(_) | Node::DhTuple(_) => SCALAR_LENGTH,
                Node::And(_) => 0,
                Node::Or(n) => CHALLENGE_LENGTH.saturating_mul(usize::from(n.saturating_sub(1))),
                Node::Threshold { k, n } => CHALLENGE_LENGTH.saturating_mul(usize::from(n.saturating_sub(*k))),
            })
        })
    }

    /// The nesting depth: 1 for a leaf.
    pub const fn depth(&self) -> usize {
        self.depth
    }

    /// The number of leaves.
    pub const fn leaves(&self) -> usize {
        self.leaves
    }
}

/// A connective of [`Proposition::from_value`] whose children are being decoded.
struct Open<'a> {
    kind: Connective,
    items: &'a [Value],
    /// The next child to decode.
    next: usize,
    /// The index of its header in the nodes.
    header: usize,
    /// The deepest child so far.
    depth: usize,
    leaves: usize,
}

enum Step<'a> {
    Decode(&'a Value),
    Completed { depth: usize, leaves: usize },
}

/// The next child of `open` to decode, or, when there is none, `open` checked and closed.
fn advance<'a>(mut open: Open<'a>, stack: &mut Vec<Open<'a>>, nodes: &mut [Node]) -> Result<Step<'a>, SigmaError> {
    if let Some(child) = open.items.get(open.next) {
        open.next = open.next.saturating_add(1);
        stack.push(open);
        return Ok(Step::Decode(child));
    }
    let (header, depth) = close(&open.kind, open.items.len(), open.depth, open.leaves)?;
    if let Some(slot) = nodes.get_mut(open.header) {
        *slot = header;
    }
    Ok(Step::Completed { depth, leaves: open.leaves })
}

enum Decoded<'a> {
    Leaf(Node),
    Connective(Connective, &'a [Value]),
}

/// One node of the canonical form, `leaves` leaves into the proposition: a checked leaf, or a connective, within
/// [`MAX_CHILDREN`], with its children still to decode.
fn decode_node(value: &Value, leaves: usize) -> Result<Decoded<'_>, SigmaError> {
    let Value::Map(m) = value else {
        return Err(invalid("a proposition is an object"));
    };
    let point = |k: &'static str| match m.get(k) {
        Some(Value::Bytes(b)) => decode_element(k, b),
        _ => Err(invalid(format!("{k} must be bytes"))),
    };
    let children = || match m.get("children") {
        Some(Value::Array(a)) if a.items().len() > MAX_CHILDREN => Err(too_wide()),
        Some(Value::Array(a)) => Ok(a.items()),
        _ => Err(invalid("children must be an array")),
    };
    let one_more_leaf = || {
        if leaves < MAX_LEAVES {
            Ok(())
        } else {
            Err(invalid(format!("a proposition has at most {MAX_LEAVES} leaves")))
        }
    };
    let ty = match m.get("type") {
        Some(Value::Text(t)) => t.as_str(),
        _ => "",
    };
    match ty {
        "dlog" => {
            fields(m, ty, &["pk"])?;
            one_more_leaf()?;
            let pk = point("pk")?;
            Ok(Decoded::Leaf(Node::Dlog(Box::new(statement_point("pk", pk)?))))
        }
        "dhtuple" => {
            fields(m, ty, &["g", "h", "u", "v"])?;
            one_more_leaf()?;
            let (g, h, u, v) = (point("g")?, point("h")?, point("u")?, point("v")?);
            Ok(Decoded::Leaf(Node::DhTuple(Box::new([
                statement_point("g", g)?,
                statement_point("h", h)?,
                statement_point("u", u)?,
                statement_point("v", v)?,
            ]))))
        }
        "and" => {
            fields(m, ty, &["children"])?;
            Ok(Decoded::Connective(Connective::And, children()?))
        }
        "or" => {
            fields(m, ty, &["children"])?;
            Ok(Decoded::Connective(Connective::Or, children()?))
        }
        "threshold" => {
            fields(m, ty, &["k", "children"])?;
            let k = match m.get("k") {
                Some(Value::Int(k)) => i32::try_from(k).ok(),
                _ => None,
            }
            .ok_or_else(|| invalid("k must be an integer"))?;
            Ok(Decoded::Connective(Connective::Threshold(i64::from(k)), children()?))
        }
        _ => Err(invalid("a proposition's type is dlog, dhtuple, and, or or threshold")),
    }
}

/// Exactly `type` and `keys`: a field outside its type's row is an error, so the encoding is canonical.
fn fields(m: &Map, ty: &str, keys: &[&str]) -> Result<(), SigmaError> {
    let exact = m.len() == keys.len().saturating_add(1) && keys.iter().all(|k| m.get(k).is_some());
    if exact { Ok(()) } else { Err(invalid(format!("{ty} takes exactly the fields {}", keys.join(", ")))) }
}

/// The proof, read front to back.
struct Reader<'a> {
    rest: &'a [u8],
    length: usize,
}

impl Reader<'_> {
    /// The next `N` bytes. The proof's length is checked before reading, so this never runs out.
    fn take<const N: usize>(&mut self, expected: usize) -> Result<[u8; N], SigmaError> {
        let (head, rest) =
            self.rest.split_first_chunk::<N>().ok_or(SigmaError::ProofLength { expected, actual: self.length })?;
        self.rest = rest;
        Ok(*head)
    }
}

/// An open connective of the proof walk: how it hands challenges to its remaining children.
struct Split {
    rule: Rule,
    /// Children not yet given a challenge.
    remaining: u8,
    /// The number of children given a challenge (a threshold child's evaluation point).
    given: u8,
}

enum Rule {
    /// Each child's challenge is the node's.
    And(Challenge),
    /// Each child but the last reads its own; the last is the XOR of the node's and its siblings'.
    Or { node: Challenge, siblings: Challenge },
    /// Child `i` gets `Q(i)`; `Q`'s coefficient 0 is the node's challenge.
    Threshold(Vec<Gf2_192>),
}

impl Split {
    fn next_challenge(&mut self, reader: &mut Reader<'_>, expected: usize) -> Result<Challenge, SigmaError> {
        self.remaining = self.remaining.saturating_sub(1);
        self.given = self.given.saturating_add(1);
        match &mut self.rule {
            Rule::And(e) => Ok(*e),
            Rule::Or { node, siblings } => {
                if self.remaining > 0 {
                    let e: Challenge = reader.take(expected)?;
                    *siblings = xor(siblings, &e);
                    Ok(e)
                } else {
                    Ok(xor(node, siblings))
                }
            }
            Rule::Threshold(q) => Ok(evaluate(q, self.given).to_bytes()),
        }
    }
}

fn xor(a: &Challenge, b: &Challenge) -> Challenge {
    let mut out = *a;
    for (o, x) in out.iter_mut().zip(b) {
        *o ^= x;
    }
    out
}

/// A leaf's challenge read as a scalar: big-endian, below `2^192`, so below `q`.
fn challenge_scalar(e: &Challenge) -> Scalar {
    let mut wide = [0u8; 32];
    for (w, b) in wide.iter_mut().skip(32usize.saturating_sub(CHALLENGE_LENGTH)).zip(e) {
        *w = *b;
    }
    <Scalar as Reduce<U256>>::reduce_bytes(&FieldBytes::from(wide))
}

/// `len(x)` as two big-endian bytes; every length here is below 256.
fn push_length(out: &mut Vec<u8>, length: usize) {
    out.extend_from_slice(&u16::try_from(length).unwrap_or(u16::MAX).to_be_bytes());
}

/// A leaf's transcript bytes (section 4): `01 ‖ len(s) ‖ s ‖ len(c) ‖ c`, with `s` the leaf's statement and `c` its
/// commitment, recomputed from its challenge `e` and response `z` (section 2).
fn push_leaf(out: &mut Vec<u8>, node: &Node, e: &Challenge, z: &Scalar) {
    let c = challenge_scalar(e);
    let (opcode, points, commitment): (u8, &[Element], Vec<u8>) = match node {
        Node::Dlog(pk) => {
            let a = ProjectivePoint::GENERATOR * z - ProjectivePoint::from(pk.point) * c;
            (DLOG_OPCODE, core::slice::from_ref(pk), encode(&a).to_vec())
        }
        Node::DhTuple(tuple) => {
            let [g, h, u, v] = &**tuple;
            let a = ProjectivePoint::from(g.point) * z - ProjectivePoint::from(u.point) * c;
            let b = ProjectivePoint::from(h.point) * z - ProjectivePoint::from(v.point) * c;
            (DH_TUPLE_OPCODE, tuple.as_slice(), [encode(&a), encode(&b)].concat())
        }
        Node::And(_) | Node::Or(_) | Node::Threshold { .. } => return,
    };
    let statement_length = points
        .len()
        .saturating_mul(ELEMENT_LENGTH)
        .saturating_add(TREE_HEADER.len().saturating_add(1).saturating_add(TREE_BODY.len()));
    out.push(0x01);
    push_length(out, statement_length);
    out.extend_from_slice(&TREE_HEADER);
    out.push(opcode);
    for p in points {
        out.extend_from_slice(&p.bytes);
    }
    out.extend_from_slice(&TREE_BODY);
    push_length(out, commitment.len());
    out.extend_from_slice(&commitment);
}

/// Verifier steps 2 to 4 in one pre-order pass: each node's challenge read or computed, each leaf's response read and
/// its commitment recomputed, and the Fiat-Shamir transcript (section 4) written. The root challenge and the
/// transcript.
fn walk(proposition: &Proposition, proof: &[u8]) -> Result<(Challenge, Vec<u8>), SigmaError> {
    let expected = proposition.proof_length();
    let mut reader = Reader { rest: proof, length: proof.len() };
    let root: Challenge = reader.take(expected)?;
    let mut transcript = Vec::new();
    // the open connectives, at most MAX_DEPTH
    let mut stack: Vec<Split> = Vec::new();
    for node in &proposition.nodes {
        let e = match stack.last_mut() {
            None => root,
            Some(parent) => parent.next_challenge(&mut reader, expected)?,
        };
        match node {
            Node::Dlog(_) | Node::DhTuple(_) => {
                let z: [u8; SCALAR_LENGTH] = reader.take(expected)?;
                let z = Option::<Scalar>::from(Scalar::from_repr(FieldBytes::from(z)))
                    .ok_or(SigmaError::ResponseOutOfRange)?;
                push_leaf(&mut transcript, node, &e, &z);
                while stack.last().is_some_and(|s| s.remaining == 0) {
                    stack.pop();
                }
            }
            Node::And(n) => {
                transcript.extend_from_slice(&[0x00, 0x00]);
                push_length(&mut transcript, usize::from(*n));
                stack.push(Split { rule: Rule::And(e), remaining: *n, given: 0 });
            }
            Node::Or(n) => {
                transcript.extend_from_slice(&[0x00, 0x01]);
                push_length(&mut transcript, usize::from(*n));
                stack.push(Split {
                    rule: Rule::Or { node: e, siblings: [0; CHALLENGE_LENGTH] },
                    remaining: *n,
                    given: 0,
                });
            }
            Node::Threshold { k, n } => {
                transcript.extend_from_slice(&[0x00, 0x02, *k]);
                push_length(&mut transcript, usize::from(*n));
                let mut q = Vec::with_capacity(usize::from(n.saturating_sub(*k)).saturating_add(1));
                q.push(Gf2_192::from_bytes(&e));
                for _ in *k..*n {
                    q.push(Gf2_192::from_bytes(&reader.take(expected)?));
                }
                stack.push(Split { rule: Rule::Threshold(q), remaining: *n, given: 0 });
            }
        }
    }
    Ok((root, transcript))
}

/// The challenge of a transcript and message: `Blake2b-256(transcript ‖ message)`, its first 24 bytes.
fn challenge(transcript: &[u8], message: &[u8]) -> Challenge {
    let mut out = [0u8; CHALLENGE_LENGTH];
    for (o, b) in out.iter_mut().zip(Blake2b256::hash_parts(&[transcript, message])) {
        *o = b;
    }
    out
}

/// Whether `proof` proves `proposition` for `message` (section 6). The message must bind what the proof authorises,
/// or the proof can be replayed.
///
/// A proof of the wrong length or with a response `z >= q` is malformed (Ergo ignores trailing bytes and reduces
/// `z`); a well-formed proof whose recomputed challenge differs from its root challenge is `Ok(false)`.
pub fn verify(proposition: &Proposition, proof: &[u8], message: &[u8]) -> Result<bool, SigmaError> {
    let expected = proposition.proof_length();
    if proof.len() != expected {
        return Err(SigmaError::ProofLength { expected, actual: proof.len() });
    }
    let (root, transcript) = walk(proposition, proof)?;
    Ok(challenge(&transcript, message) == root)
}

/// [`verify`] of a proposition in its canonical form ([`Proposition::from_value`]): the JLVM's
/// `sigma_verify(proposition, proof, message)`.
pub fn verify_value(proposition: &Value, proof: &[u8], message: &[u8]) -> Result<bool, SigmaError> {
    verify(&Proposition::from_value(proposition)?, proof, message)
}

/// [`verify`] of `dlog(key)`, Ergo's P2PK signature: the JLVM's `schnorr_verify(key, message, proof)`. The key is a
/// 33-byte canonical, non-identity group element and the proof 56 bytes.
pub fn verify_dlog_bytes(key: &[u8], message: &[u8], proof: &[u8]) -> Result<bool, SigmaError> {
    verify(&Proposition::dlog(key)?, proof, message)
}

#[cfg(test)]
mod tests {
    #![allow(clippy::unwrap_used, clippy::indexing_slicing, reason = "tests fail by panicking")]

    use super::{IDENTITY, decode_element, encode};
    use k256::ProjectivePoint;

    #[test]
    fn the_identity_encodes_as_33_zero_bytes_and_decodes_from_them() {
        assert_eq!(encode(&ProjectivePoint::IDENTITY), IDENTITY);
        assert_eq!(decode_element("p", &IDENTITY).unwrap(), None);
    }

    #[test]
    fn the_generator_round_trips() {
        let g = encode(&ProjectivePoint::GENERATOR);
        assert_eq!(g[0], 0x02);
        assert_eq!(decode_element("p", &g).unwrap().unwrap().bytes, g);
    }
}
