/**
 * Sigma protocols over secp256k1, byte-compatible with Ergo (`docs/specs/sigma.md`): propositions and the verifier, as
 * Rust's `dilf4s_signing::sigma` (the Scala reference, `xyz.kd5ujc.crypto.sigma`, settles any difference).
 *
 * A {@link Proposition} is a tree of `and`, `or` and `threshold` over `dlog` and `dhtuple` statements. It is decoded
 * only from its canonical form ({@link Proposition.fromValue}), so every proposition meets section 3: statement points
 * on the curve and not the identity, 1 to 255 children per connective, `1 <= k <= n`, at most 64 levels and 4096
 * leaves. It is held flat, in pre-order, so decoding, the proof walk and the transcript are loops: nothing recurses on
 * the shape of the input.
 *
 * - {@link verify} checks a proof of a decoded proposition for a message: the JLVM's `sigma_verify`, once its
 *   proposition is decoded (Rust's `sigma_proposition`, then `sigma_verify`).
 * - {@link verifyValue} decodes the proposition from its canonical form first.
 * - {@link verifyDlogBytes} checks a single-key proof (Ergo's P2PK signature): the JLVM's `schnorr_verify`.
 *
 * Malformed input is a `SigmaError`; a well-formed proof that fails is `false`. An argument of the wrong type (from
 * JavaScript, or a cast) is core's `TypeMismatch`, checked for every argument, in order, before anything is decoded.
 */
import { err, isValue, models, ok, type MapValue, type Result, type TypeMismatch, type Value } from "@kd5ujc/dilf4s-core";
import { bytesLength, hashPartsOf, mint, ownBytes } from "@kd5ujc/dilf4s-core/internal";
import { secp256k1 } from "@noble/curves/secp256k1.js";

import type { SigmaError } from "./errors.js";
import { attempt } from "./internal/attempt.js";
import { beToBig, equal, isZero } from "./internal/bytes.js";
import { expected, NOT_BYTES, verdict, viewOf } from "./internal/check.js";
import { evaluate, fromBytes as element192, toBytes, type Gf2_192 } from "./internal/gf2_192.js";
import { INTERNAL, type Internal } from "./internal/token.js";

/** A sigma refusal, or an argument of the wrong type. */
export type SigmaRefusal = SigmaError | TypeMismatch;

/** Bytes of a challenge: 192-bit soundness. */
export const CHALLENGE_LENGTH = 24;

/** Bytes of an encoded group element: SEC1 compressed, or 33 zero bytes for the identity. */
export const ELEMENT_LENGTH = 33;

/** Bytes of a response `z`: a big-endian scalar below the group order. */
export const SCALAR_LENGTH = 32;

/** The deepest a proposition nests (a leaf alone is depth 1). */
export const MAX_DEPTH: number = models.limits.SIGMA_MAX_DEPTH;

/** The most leaves a proposition has. */
export const MAX_LEAVES: number = models.limits.SIGMA_MAX_LEAVES;

/** The most children of one connective: a threshold's evaluation points are the bytes `1..=n`. */
export const MAX_CHILDREN: number = models.limits.SIGMA_MAX_CHILDREN;

/**
 * The weight of the heaviest valid proposition in its canonical form (section 3): 4096 `dhtuple` leaves (158 each) at
 * depth 64, under `1 + 255 + 61·4096` one-`k` thresholds (29 each, plus their children). A heavier value is refused
 * before any of it is decoded. A `bigint`, as a value's `weight` is.
 */
export const MAX_WEIGHT = BigInt(models.limits.SIGMA_MAX_WEIGHT);

const Point = secp256k1.Point;
type CurvePoint = typeof Point.BASE;

/** The field modulus `p` and the group order `q`. */
const P: bigint = Point.Fp.ORDER;
const Q: bigint = Point.Fn.ORDER;

/**
 * A leaf statement is Ergo's `ErgoTree` for `SigmaPropConstant(leaf)`, constants segregated: this header (version 0,
 * segregated, one constant of type `SigmaProp`), the leaf's opcode and points, then {@link TREE_BODY}.
 */
const TREE_HEADER: readonly number[] = Object.freeze([0x10, 0x01, 0x08]);

/** The body of a leaf statement: `ConstantPlaceholder(0)`. */
const TREE_BODY: readonly number[] = Object.freeze([0x73, 0x00]);

/** `ProveDlog`. */
const DLOG_OPCODE = 0xcd;

/** `ProveDHTuple`. */
const DH_TUPLE_OPCODE = 0xce;

/** The bytes of a leaf statement of `points` points: 39 for a `dlog`, 138 for a `dhtuple`. */
const statementLength = (points: number): number => TREE_HEADER.length + 1 + points * ELEMENT_LENGTH + TREE_BODY.length;

/** A leaf's transcript bytes (section 4), `01 ‖ len(s) ‖ s ‖ len(c) ‖ c`, its commitment `c` of `commitments` elements. */
const leafBytes = (points: number, commitments: number): number => 1 + 2 + statementLength(points) + 2 + commitments * ELEMENT_LENGTH;
const DLOG_LEAF_BYTES = leafBytes(1, 1);
const DH_TUPLE_LEAF_BYTES = leafBytes(4, 2);

/** Every proof of `dlog(pk)`: its root challenge and its response. */
const DLOG_PROOF_LENGTH = CHALLENGE_LENGTH + SCALAR_LENGTH;

// ------------------------------------------------------------------ refusals

const NOT_A_VALUE = expected("a canonical value");
const NOT_A_PROPOSITION = expected("a sigma Proposition");

const invalid = (reason: string): SigmaError => ({ kind: "InvalidProposition", reason });
const invalidPoint = (what: string, reason: string): SigmaError => ({ kind: "InvalidPoint", what, reason });
const tooWide = (): SigmaError => invalid(`a connective has at most ${String(MAX_CHILDREN)} children`);
const tooManyLeaves = (): SigmaError => invalid(`a proposition has at most ${String(MAX_LEAVES)} leaves`);
const wrongLength = (what: string, got: number): SigmaError => invalidPoint(what, `a group element is ${String(ELEMENT_LENGTH)} bytes, got ${String(got)}`);
/** A proof of `actual` bytes where the proposition fixes `expected`. */
const proofLength = (expected: number, actual: number): SigmaError => ({ kind: "ProofLength", expected, actual });
const RESPONSE_OUT_OF_RANGE: SigmaError = Object.freeze({ kind: "ResponseOutOfRange" });

// ------------------------------------------------------------------ group elements

/** A statement point: on the curve, never the identity, with its canonical encoding. */
interface Element {
  readonly bytes: Uint8Array;
  readonly point: CurvePoint;
}

/** 33 bytes: SEC1 compressed, or 33 zero bytes for the identity (Ergo's encoding, not SEC1's single zero byte). */
const encode = (p: CurvePoint): Uint8Array => (p.is0() ? new Uint8Array(ELEMENT_LENGTH) : p.toBytes(true));

/**
 * A group element from its canonical encoding (section 1); `undefined` is the identity. Only canonical encodings
 * decode: 33 bytes, prefix `02` or `03` with `x` below the field modulus and on the curve, or 33 zero bytes. Each of
 * these is checked before noble sees the bytes; its decompression is the one call that may throw.
 */
function decodeElement(what: string, bytes: Uint8Array): Result<Element | undefined, SigmaError> {
  if (bytes.length !== ELEMENT_LENGTH) return err(wrongLength(what, bytes.length));
  if (isZero(bytes)) return ok(undefined);
  const prefix = bytes[0];
  if (prefix !== 0x02 && prefix !== 0x03) return err(invalidPoint(what, "a group element is compressed: prefix 02 or 03"));
  if (beToBig(bytes.subarray(1)) >= P) return err(invalidPoint(what, "not a point of secp256k1: x is not below the field modulus"));
  const point = attempt(() => Point.fromBytes(bytes));
  if (point === undefined) return err(invalidPoint(what, "not a point of secp256k1"));
  return equal(encode(point), bytes) ? ok({ bytes, point }) : err(invalidPoint(what, "not the canonical encoding of a point"));
}

/**
 * A statement point is never the identity: `dlog(O)` holds for `w = 0`, so anyone could prove it, and likewise a
 * tuple with `u` or `v` the identity; an identity base proves nothing.
 */
function statementPoint(what: string, decoded: Element | undefined): Result<Element, SigmaError> {
  return decoded === undefined ? err(invalid(`${what} is the identity, for which anyone can prove knowledge (w = 0)`)) : ok(decoded);
}

// ------------------------------------------------------------------ propositions

/** One node of a proposition, in pre-order: a connective is followed by its `n` children's subtrees. */
type Node =
  | { readonly kind: "dlog"; readonly pk: Element }
  | { readonly kind: "dhtuple"; readonly points: readonly [Element, Element, Element, Element] }
  | { readonly kind: "and"; readonly n: number }
  | { readonly kind: "or"; readonly n: number }
  | { readonly kind: "threshold"; readonly k: number; readonly n: number };

type Leaf = Extract<Node, { readonly kind: "dlog" | "dhtuple" }>;

/** A connective before its children are checked; a threshold's `k` as given. */
type Connective = { readonly kind: "and" } | { readonly kind: "or" } | { readonly kind: "threshold"; readonly k: number };

/**
 * Section 3's checks on a connective with `n` children, in the reference's order (`SigmaProp.threshold` then
 * `SigmaProp.conjecture`): its header and its depth.
 */
function close(kind: Connective, n: number, childDepth: number, leaves: number): Result<{ readonly header: Node; readonly depth: number }, SigmaError> {
  if (kind.kind === "threshold" && !(kind.k >= 1 && kind.k <= n)) return err(invalid(`a threshold needs 1 <= k <= ${String(n)} children, got k = ${String(kind.k)}`));
  if (n === 0) return err(invalid("a connective needs at least one child: an empty one holds for anyone"));
  if (n > MAX_CHILDREN) return err(tooWide());
  const depth = childDepth + 1;
  if (depth > MAX_DEPTH) return err(invalid(`a proposition nests at most ${String(MAX_DEPTH)} deep`));
  if (leaves > MAX_LEAVES) return err(tooManyLeaves());
  return ok({ header: kind.kind === "threshold" ? { kind: "threshold", k: kind.k, n } : { kind: kind.kind, n }, depth });
}

/**
 * The exact length of every proof of `nodes` (section 5), `24 + Σ leaves 32 + Σ or-nodes 24·(n−1) + Σ threshold-nodes
 * 24·(n−k)`, and of their transcript (section 4).
 */
function lengths(nodes: readonly Node[]): readonly [number, number] {
  let [proof, transcript] = [CHALLENGE_LENGTH, 0];
  for (const node of nodes) {
    switch (node.kind) {
      case "dlog":
        [proof, transcript] = [proof + SCALAR_LENGTH, transcript + DLOG_LEAF_BYTES];
        break;
      case "dhtuple":
        [proof, transcript] = [proof + SCALAR_LENGTH, transcript + DH_TUPLE_LEAF_BYTES];
        break;
      case "and":
        transcript += 4;
        break;
      case "or":
        [proof, transcript] = [proof + CHALLENGE_LENGTH * (node.n - 1), transcript + 4];
        break;
      case "threshold":
        [proof, transcript] = [proof + CHALLENGE_LENGTH * (node.n - node.k), transcript + 5];
        break;
    }
  }
  return [proof, transcript];
}

const PROPOSITION = mint();
let build!: (nodes: Node[], depth: number, leaves: number) => Proposition;
let nodesOf!: (p: Proposition) => readonly Node[];
let transcriptLengthOf!: (p: Proposition) => number;

/** A sigma proposition (`docs/specs/sigma.md` section 3), valid by construction: only {@link fromValue} makes one. */
export class Proposition {
  /** Pre-order, frozen. */
  readonly #nodes: readonly Node[];
  readonly #depth: number;
  readonly #leaves: number;
  readonly #proofLength: number;
  readonly #transcriptLength: number;

  private constructor(token: Internal, nodes: Node[], depth: number, leaves: number) {
    // without the token, nothing of the caller's is read or kept
    const minted = token === INTERNAL;
    this.#nodes = Object.freeze(minted ? nodes : []);
    this.#depth = minted ? depth : 0;
    this.#leaves = minted ? leaves : 0;
    [this.#proofLength, this.#transcriptLength] = lengths(this.#nodes);
    if (minted) PROPOSITION.stamp(this);
    Object.freeze(this);
  }

  static {
    build = (nodes, depth, leaves) => new Proposition(INTERNAL, nodes, depth, leaves);
    nodesOf = (p) => p.#nodes;
    transcriptLengthOf = (p) => p.#transcriptLength;
  }

  /**
   * The canonical form (section 3), as the JLVM passes it: `{"type": "dlog", "pk": bytes}`, `{"type": "dhtuple", "g",
   * "h", "u", "v": bytes}`, `{"type": "and" | "or", "children": [...]}` or `{"type": "threshold", "k": int,
   * "children": [...]}`, with no other field. Decoded with an explicit stack, in the reference's order
   * (`SigmaProp.decode`, section 3): a value heavier than {@link MAX_WEIGHT} first, before anything is decoded; then,
   * in pre-order, each node's own fields, the leaf bound before a leaf's points and the width before a connective's
   * children; and each connective's remaining checks once its children are decoded.
   *
   * The work is bounded however much `value` shares: its weight is cached, so the first check is O(1), and what passes
   * it is at most {@link MAX_WEIGHT} of logical size, of which at most {@link MAX_LEAVES} leaves are decoded. A point's
   * bytes are measured before they are copied. What is not a canonical value is a `TypeMismatch`.
   */
  static fromValue(value: Value): Result<Proposition, SigmaRefusal> {
    return isValue(value) ? decode(value) : err(NOT_A_VALUE);
  }

  /** The nesting depth: 1 for a leaf. */
  get depth(): number {
    return this.#depth;
  }

  /** The number of leaves. */
  get leaves(): number {
    return this.#leaves;
  }

  /**
   * The exact length of every proof of this proposition (section 5):
   * `24 + Σ leaves 32 + Σ or-nodes 24·(n−1) + Σ threshold-nodes 24·(n−k)`.
   */
  get proofLength(): number {
    return this.#proofLength;
  }
}
Object.freeze(Proposition);
Object.freeze(Proposition.prototype);

/** A connective of {@link decode} whose children are being decoded. */
interface Open {
  readonly kind: Connective;
  readonly items: readonly Value[];
  /** The next child to decode. */
  readonly next: number;
  /** The index of its header in the nodes. */
  readonly header: number;
  /** The deepest child so far. */
  readonly depth: number;
  readonly leaves: number;
}

type Step = { readonly kind: "decode"; readonly value: Value } | { readonly kind: "completed"; readonly depth: number; readonly leaves: number };

type Decoded = { readonly kind: "leaf"; readonly node: Leaf } | { readonly kind: "connective"; readonly connective: Connective; readonly items: readonly Value[] };

/** A placeholder header, replaced once its connective's children are decoded and it is checked. */
const PENDING: Node = Object.freeze({ kind: "and", n: 0 });

/** {@link Proposition.fromValue} of a value. */
function decode(value: Value): Result<Proposition, SigmaError> {
  if (value.weight > MAX_WEIGHT) return err(invalid(`a proposition weighs at most ${String(MAX_WEIGHT)}`));
  const nodes: Node[] = [];
  const stack: Open[] = [];
  // every leaf decoded so far, in the whole proposition
  let leaves = 0;
  let step: Step = { kind: "decode", value };
  for (;;) {
    let next: Result<Step, SigmaError>;
    if (step.kind === "decode") {
      const d = decodeNode(step.value, leaves);
      if (!d.ok) return d;
      if (d.value.kind === "leaf") {
        nodes.push(d.value.node);
        leaves += 1;
        next = ok({ kind: "completed", depth: 1, leaves: 1 });
      } else {
        const header = nodes.length;
        nodes.push(PENDING);
        next = advance({ kind: d.value.connective, items: d.value.items, next: 0, header, depth: 0, leaves: 0 }, stack, nodes);
      }
    } else {
      const parent = stack.pop();
      if (parent === undefined) return ok(build(nodes, step.depth, step.leaves));
      next = advance({ ...parent, depth: Math.max(parent.depth, step.depth), leaves: parent.leaves + step.leaves }, stack, nodes);
    }
    if (!next.ok) return next;
    step = next.value;
  }
}

/** The next child of `open` to decode, or, when there is none, `open` checked and closed. */
function advance(open: Open, stack: Open[], nodes: Node[]): Result<Step, SigmaError> {
  const child = open.items[open.next];
  if (child !== undefined) {
    stack.push({ ...open, next: open.next + 1 });
    return ok({ kind: "decode", value: child });
  }
  const closed = close(open.kind, open.items.length, open.depth, open.leaves);
  if (!closed.ok) return closed;
  nodes[open.header] = closed.value.header;
  return ok({ kind: "completed", depth: closed.value.depth, leaves: open.leaves });
}

const DLOG_FIELDS: readonly string[] = Object.freeze(["pk"]);
const DH_TUPLE_FIELDS: readonly string[] = Object.freeze(["g", "h", "u", "v"]);
const CONNECTIVE_FIELDS: readonly string[] = Object.freeze(["children"]);
const THRESHOLD_FIELDS: readonly string[] = Object.freeze(["k", "children"]);

/** `k` fits 32 bits, as the reference's `Int`. */
const INT_MIN = -(2n ** 31n);
const INT_MAX = 2n ** 31n - 1n;

/**
 * One node of the canonical form, `leaves` leaves into the proposition: a checked leaf, or a connective, within
 * {@link MAX_CHILDREN}, with its children still to decode.
 */
function decodeNode(value: Value, leaves: number): Result<Decoded, SigmaError> {
  if (value.kind !== "map") return err(invalid("a proposition is an object"));
  const m = value;
  const t = m.get("type");
  const ty = t?.kind === "text" ? t.value : "";
  switch (ty) {
    case "dlog": {
      const f = fields(m, ty, DLOG_FIELDS);
      if (!f.ok) return f;
      if (leaves >= MAX_LEAVES) return err(tooManyLeaves());
      const pk = point(m, "pk");
      if (!pk.ok) return pk;
      const s = statementPoint("pk", pk.value);
      return s.ok ? ok({ kind: "leaf", node: { kind: "dlog", pk: s.value } }) : s;
    }
    case "dhtuple": {
      const f = fields(m, ty, DH_TUPLE_FIELDS);
      if (!f.ok) return f;
      if (leaves >= MAX_LEAVES) return err(tooManyLeaves());
      // all four points decoded, in this order, before any is checked against the identity, as the reference does
      const g = point(m, "g");
      if (!g.ok) return g;
      const h = point(m, "h");
      if (!h.ok) return h;
      const u = point(m, "u");
      if (!u.ok) return u;
      const v = point(m, "v");
      return v.ok ? dhTuple(g.value, h.value, u.value, v.value) : v;
    }
    case "and":
    case "or": {
      const f = fields(m, ty, CONNECTIVE_FIELDS);
      if (!f.ok) return f;
      const items = children(m);
      return items.ok ? ok({ kind: "connective", connective: { kind: ty }, items: items.value }) : items;
    }
    case "threshold": {
      const f = fields(m, ty, THRESHOLD_FIELDS);
      if (!f.ok) return f;
      const k = m.get("k");
      if (k?.kind !== "int" || k.value < INT_MIN || k.value > INT_MAX) return err(invalid("k must be an integer"));
      const items = children(m);
      return items.ok ? ok({ kind: "connective", connective: { kind: "threshold", k: Number(k.value) }, items: items.value }) : items;
    }
    default:
      return err(invalid("a proposition's type is dlog, dhtuple, and, or or threshold"));
  }
}

/** A `dhtuple` leaf of four decoded points, each checked against the identity in turn. */
function dhTuple(g: Element | undefined, h: Element | undefined, u: Element | undefined, v: Element | undefined): Result<Decoded, SigmaError> {
  const sg = statementPoint("g", g);
  if (!sg.ok) return sg;
  const sh = statementPoint("h", h);
  if (!sh.ok) return sh;
  const su = statementPoint("u", u);
  if (!su.ok) return su;
  const sv = statementPoint("v", v);
  return sv.ok ? ok({ kind: "leaf", node: { kind: "dhtuple", points: [sg.value, sh.value, su.value, sv.value] } }) : sv;
}

/** The point at `key`: bytes, measured before they are copied (a 7.9 MB key is refused by its length, uncopied). */
function point(m: MapValue, key: string): Result<Element | undefined, SigmaError> {
  const b = m.get(key);
  if (b?.kind !== "bytes") return err(invalid(`${key} must be bytes`));
  return b.length === ELEMENT_LENGTH ? decodeElement(key, b.bytes) : err(wrongLength(key, b.length));
}

/** `children`: an array of at most {@link MAX_CHILDREN} values, refused by its length before any is decoded. */
function children(m: MapValue): Result<readonly Value[], SigmaError> {
  const c = m.get("children");
  if (c?.kind !== "array") return err(invalid("children must be an array"));
  return c.length > MAX_CHILDREN ? err(tooWide()) : ok(c.items);
}

/** Exactly `type` and `keys`: a field outside its type's row is an error, so the encoding is canonical. */
function fields(m: MapValue, ty: string, keys: readonly string[]): Result<undefined, SigmaError> {
  const exact = m.size === keys.length + 1 && keys.every((k) => m.get(k) !== undefined);
  return exact ? ok(undefined) : err(invalid(`${ty} takes exactly the fields ${keys.join(", ")}`));
}

// ------------------------------------------------------------------ the verifier

/** An open connective of the proof walk: how it hands challenges to its remaining children. */
interface Split {
  readonly rule: Rule;
  /** Children not yet given a challenge. */
  readonly remaining: number;
  /** The number of children given a challenge (a threshold child's evaluation point). */
  readonly given: number;
}

type Rule =
  /** Each child's challenge is the node's. */
  | { readonly kind: "and"; readonly e: Uint8Array }
  /** Each child but the last reads its own; the last is the XOR of the node's and its siblings'. */
  | { readonly kind: "or"; readonly node: Uint8Array; readonly siblings: Uint8Array }
  /** Child `i` gets `Q(i)`; `Q`'s coefficient 0 is the node's challenge. */
  | { readonly kind: "threshold"; readonly q: readonly Gf2_192[] };

/** The proof, read front to back. Its length is checked before it is read, so this never runs out. */
interface Reader {
  readonly take: (n: number) => Uint8Array | undefined;
}

function reader(proof: Uint8Array): Reader {
  let at = 0;
  return {
    take: (n) => {
      if (at + n > proof.length) return undefined;
      const out = proof.subarray(at, at + n);
      at += n;
      return out;
    },
  };
}

/** The bytewise XOR of two challenges. */
function xor(a: Uint8Array, b: Uint8Array): Uint8Array {
  const out = new Uint8Array(CHALLENGE_LENGTH);
  for (let i = 0; i < CHALLENGE_LENGTH; i++) out[i] = (a[i] ?? 0) ^ (b[i] ?? 0);
  return out;
}

/** The next child's challenge from `split`, and `split` with that child given it. */
function nextChallenge(split: Split, read: Reader, short: () => SigmaError): Result<readonly [Uint8Array, Split], SigmaError> {
  const remaining = split.remaining - 1;
  const given = split.given + 1;
  const rule = split.rule;
  switch (rule.kind) {
    case "and":
      return ok([rule.e, { rule, remaining, given }]);
    case "or": {
      if (remaining === 0) return ok([xor(rule.node, rule.siblings), { rule, remaining, given }]);
      const e = read.take(CHALLENGE_LENGTH);
      return e === undefined ? err(short()) : ok([e, { rule: { ...rule, siblings: xor(rule.siblings, e) }, remaining, given }]);
    }
    case "threshold":
      return ok([toBytes(evaluate(rule.q, given)), { rule, remaining, given }]);
  }
}

/** The transcript (section 4), written front to back into its exact length. */
interface Transcript {
  readonly put: (bytes: Uint8Array | readonly number[]) => void;
  /** `len(x)` as two big-endian bytes; every length here is below 256. */
  readonly length: (n: number) => void;
  readonly bytes: Uint8Array;
}

function transcript(size: number): Transcript {
  const bytes = new Uint8Array(size);
  let at = 0;
  return {
    put: (b) => {
      bytes.set(b, at);
      at += b.length;
    },
    length: (n) => {
      bytes[at] = (n >>> 8) & 0xff;
      bytes[at + 1] = n & 0xff;
      at += 2;
    },
    bytes,
  };
}

/**
 * A leaf's transcript bytes (section 4): `01 ‖ len(s) ‖ s ‖ len(c) ‖ c`, with `s` the leaf's statement and `c` its
 * commitment, recomputed from its challenge `e` and response `z` (section 2): `a = z·G − e·pk`, or `a = z·g − e·u` and
 * `b = z·h − e·v`.
 *
 * Each commitment is one double-scalar multiplication, `z·base + e·(−statement)`, by noble's `mulAddUnsafe`
 * (Strauss–Shamir, one doubling chain for both): the same point as Rust's two multiplications and a subtraction, and
 * here about 1.5 times as fast for a tuple's (a `dlog`'s takes as long either way). Rust keeps two because k256's
 * `lincomb` costs more cycles in the SP1 guest (issue #33), which does not apply here. Negating the point, not the
 * scalar, keeps both scalars in noble's range: `z < q` is checked and `e < 2^192`, zero included. Both are public, so
 * the variable-time form is the right one.
 */
function pushLeaf(out: Transcript, node: Leaf, e: Uint8Array, z: bigint): void {
  const c = beToBig(e);
  const commit = (base: CurvePoint, statement: CurvePoint): Uint8Array => encode(base.mulAddUnsafe(z, statement.negate(), c));
  const [opcode, points, commitments] =
    node.kind === "dlog"
      ? ([DLOG_OPCODE, [node.pk], [commit(Point.BASE, node.pk.point)]] as const)
      : ([DH_TUPLE_OPCODE, node.points, [commit(node.points[0].point, node.points[2].point), commit(node.points[1].point, node.points[3].point)]] as const);
  out.put([0x01]);
  out.length(statementLength(points.length));
  out.put(TREE_HEADER);
  out.put([opcode]);
  for (const p of points) out.put(p.bytes);
  out.put(TREE_BODY);
  out.length(commitments.length * ELEMENT_LENGTH);
  for (const a of commitments) out.put(a);
}

/**
 * Verifier steps 2 to 4 in one pre-order pass: each node's challenge read or computed, each leaf's response read and
 * its commitment recomputed, and the Fiat-Shamir transcript (section 4) written. The root challenge and the
 * transcript.
 */
function walk(p: Proposition, proof: Uint8Array): Result<{ readonly root: Uint8Array; readonly transcript: Uint8Array }, SigmaError> {
  const short = (): SigmaError => proofLength(p.proofLength, proof.length);
  const read = reader(proof);
  const root = read.take(CHALLENGE_LENGTH);
  if (root === undefined) return err(short());
  const out = transcript(transcriptLengthOf(p));
  // the open connectives, at most MAX_DEPTH
  const stack: Split[] = [];
  for (const node of nodesOf(p)) {
    const parent = stack.pop();
    let e = root;
    if (parent !== undefined) {
      const next = nextChallenge(parent, read, short);
      if (!next.ok) return next;
      [e] = next.value;
      stack.push(next.value[1]);
    }
    switch (node.kind) {
      case "dlog":
      case "dhtuple": {
        const zb = read.take(SCALAR_LENGTH);
        if (zb === undefined) return err(short());
        const z = beToBig(zb);
        if (z >= Q) return err(RESPONSE_OUT_OF_RANGE);
        pushLeaf(out, node, e, z);
        // the connectives this leaf completes
        for (let top = stack.at(-1); top?.remaining === 0; top = stack.at(-1)) stack.pop();
        break;
      }
      case "and":
        out.put([0x00, 0x00]);
        out.length(node.n);
        stack.push({ rule: { kind: "and", e }, remaining: node.n, given: 0 });
        break;
      case "or":
        out.put([0x00, 0x01]);
        out.length(node.n);
        stack.push({ rule: { kind: "or", node: e, siblings: new Uint8Array(CHALLENGE_LENGTH) }, remaining: node.n, given: 0 });
        break;
      case "threshold": {
        out.put([0x00, 0x02, node.k]);
        out.length(node.n);
        const q: Gf2_192[] = [element192(e)];
        for (let i = node.k; i < node.n; i++) {
          const c = read.take(CHALLENGE_LENGTH);
          if (c === undefined) return err(short());
          q.push(element192(c));
        }
        stack.push({ rule: { kind: "threshold", q }, remaining: node.n, given: 0 });
        break;
      }
    }
  }
  return ok({ root, transcript: out.bytes });
}

/** The challenge of a transcript and message: `Blake2b-256(transcript ‖ message)`, its first 24 bytes. */
const challenge = (transcript: Uint8Array, message: Uint8Array): Uint8Array => hashPartsOf("BLAKE2b-256", [transcript, message]).subarray(0, CHALLENGE_LENGTH);

/**
 * Section 6 over owned bytes: the proof's length (`proof` is that length, a number, when longer than `p` allows), then
 * each response in pre-order, then the challenge.
 */
function check(p: Proposition, proof: Uint8Array | number, message: Uint8Array): Result<boolean, SigmaError> {
  const actual = typeof proof === "number" ? proof : proof.length;
  if (typeof proof === "number" || actual !== p.proofLength) return err(proofLength(p.proofLength, actual));
  const w = walk(p, proof);
  return w.ok ? verdict(equal(challenge(w.value.transcript, message), w.value.root)) : w;
}

/**
 * Whether `proof` proves `proposition` for `message` (section 6). The message must bind what the proof authorises, or
 * the proof can be replayed.
 *
 * A proof of the wrong length or with a response `z >= q` is malformed (Ergo ignores trailing bytes and reduces `z`);
 * a well-formed proof whose recomputed challenge differs from its root challenge is `false`. The proof is measured
 * before it is read, so a longer one is refused uncopied; the message is read as a view (core's byte boundary).
 * A `proposition` that is not a {@link Proposition}, or bytes that are not a `Uint8Array`, are a `TypeMismatch`.
 */
export function verify(proposition: Proposition, proof: Uint8Array, message: Uint8Array): Result<boolean, SigmaRefusal> {
  if (!PROPOSITION.has(proposition)) return err(NOT_A_PROPOSITION);
  const pf = ownBytes(proof, proposition.proofLength);
  if (pf === undefined) return err(NOT_BYTES);
  const m = viewOf(message);
  return m === undefined ? err(NOT_BYTES) : check(proposition, pf, m);
}

/**
 * {@link verify} of a proposition in its canonical form ({@link Proposition.fromValue}): the JLVM's
 * `sigma_verify(proposition, proof, message)`. Each argument's type is checked first, then the proposition is decoded,
 * then the proof checked.
 */
export function verifyValue(proposition: Value, proof: Uint8Array, message: Uint8Array): Result<boolean, SigmaRefusal> {
  if (!isValue(proposition)) return err(NOT_A_VALUE);
  if (bytesLength(proof) === undefined) return err(NOT_BYTES);
  const m = viewOf(message);
  if (m === undefined) return err(NOT_BYTES);
  const p = decode(proposition);
  if (!p.ok) return p;
  // measured by its own length when the proposition fixed it; no caller code ran since the check above
  const pf = ownBytes(proof, p.value.proofLength);
  return pf === undefined ? err(NOT_BYTES) : check(p.value, pf, m);
}

/**
 * {@link verify} of `dlog(key)`, Ergo's P2PK signature: the JLVM's `schnorr_verify(key, message, proof)` (note the
 * order). The key is a 33-byte canonical, non-identity group element and the proof 56 bytes; each is measured before
 * it is read.
 */
export function verifyDlogBytes(key: Uint8Array, message: Uint8Array, proof: Uint8Array): Result<boolean, SigmaRefusal> {
  const k = ownBytes(key, ELEMENT_LENGTH);
  const m = viewOf(message);
  const pf = ownBytes(proof, DLOG_PROOF_LENGTH);
  if (k === undefined || m === undefined || pf === undefined) return err(NOT_BYTES);
  if (typeof k === "number") return err(wrongLength("pk", k));
  const pk = decodeElement("pk", k);
  if (!pk.ok) return pk;
  const s = statementPoint("pk", pk.value);
  return s.ok ? check(build([{ kind: "dlog", pk: s.value }], 1, 1), pf, m) : s;
}
