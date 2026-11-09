/**
 * The values the machine works on: core's canonical values, plus arrays and maps the machine builds itself.
 *
 * Core's {@link ArrayValue} and {@link MapValue} are flat: building one copies and checks its parts, and a map sorts
 * its keys. The machine builds containers at every step (`map`, `filter`, an array or map expression) and updates maps
 * in place of others (`set`, `unset`, `merge`), which gas pays per level of a search, not per entry (jlvm.md section 7).
 * So the containers it builds are its own: a {@link JArray} holds its items as they are, and a {@link JMap} is a
 * persistent AVL tree, so `set` shares every subtree it does not touch and costs O(log n). Both cache their weight and
 * depth, from their parts', as core's values do. Data, host bindings and literals stay core values; the containers the
 * machine builds hold either kind.
 *
 * What leaves the machine is a core value: {@link Heap.value} converts a container once (a {@link Heap} remembers each
 * one it has converted, so a shared part is converted once), with an explicit stack and no recursion. It is also how
 * equality and the canonical encoding read a container: both are charged by weight, which bounds the conversion.
 */
import { ArrayValue, MapValue, MAX_DEPTH, MAX_WEIGHT, Value, type BoolValue, type BytesValue, type IntValue, type NullValue, type RationalValue, type TextValue } from "@kd5ujc/dilf4s-core";
import { compareCodePoints, utf8LengthOf as utf8Length } from "@kd5ujc/dilf4s-core/internal";

/** A canonical scalar. */
export type Scalar = NullValue | BoolValue | IntValue | RationalValue | TextValue | BytesValue;

/** An array: core's, or one the machine built. */
export type AnyArray = ArrayValue | JArray;

/** A map: core's, or one the machine built. */
export type AnyMap = MapValue | JMap;

/** A value the machine works on. */
export type JValue = Scalar | AnyArray | AnyMap;

/** `w`, saturated at core's `MAX_WEIGHT` (2^63 - 1): every term is non-negative, so clamping once saturates each step. */
const saturated = (w: bigint): bigint => (w > MAX_WEIGHT ? MAX_WEIGHT : w);

/** An array the machine built: its items as they are, its weight and depth computed once, from theirs, exactly. */
export class JArray {
  readonly kind = "array";
  private constructor(
    readonly items: readonly JValue[],
    readonly weight: bigint,
    readonly depth: number,
  ) {
    Object.freeze(this);
  }

  get length(): number {
    return this.items.length;
  }

  /** The array of `items`, which it now holds; `undefined` if it nests deeper than `MAX_DEPTH`. O(n). */
  static of(items: readonly JValue[]): JArray | undefined {
    let [deepest, weight] = [0, 1n];
    for (const v of items) {
      if (v.depth > deepest) deepest = v.depth;
      weight += v.weight;
    }
    return deepest >= MAX_DEPTH ? undefined : new JArray(items, saturated(weight), deepest + 1);
  }

  /** The empty array. */
  static readonly empty: JArray = new JArray([], 1n, 1);
}

/**
 * A core map under a {@link JMap}: read through its own binary search, never copied. What enumerating it needs (its
 * entries in code-point order, the depths of its values) is found the first time it is needed and kept: each is a
 * pure function of the map, so keeping it changes no result.
 */
class Base {
  #sorted: readonly (readonly [string, Value])[] | undefined;
  #depths: readonly number[] | undefined;
  constructor(readonly map: MapValue) {}

  /** The entries in code-point order of their keys: O(n log n), once. */
  sorted(): readonly (readonly [string, Value])[] {
    this.#sorted ??= [...this.map.entries()].sort((a, b) => compareCodePoints(a[0], b[0]));
    return this.#sorted;
  }

  /**
   * How many values have each depth, by depth: one pass over the entries, once, and none for a map of scalars, whose
   * values all have depth 0.
   */
  depths(): readonly number[] {
    if (this.#depths !== undefined) return this.#depths;
    const counts: number[] = Array.from({ length: Math.max(this.map.depth, 1) }, () => 0);
    if (this.map.depth <= 1) counts[0] = this.map.size;
    else for (const [, v] of this.map.entries()) counts[v.depth] = (counts[v.depth] ?? 0) + 1;
    this.#depths = counts;
    return counts;
  }
}

/**
 * A node of a {@link JMap}'s AVL tree: an entry the map sets, or a tombstone (no `value`) hiding an entry of its base.
 * Each node records the base entry it shadows, if any, and caches its subtree's height and aggregates.
 */
interface Tree {
  readonly key: string;
  /** The value, or `undefined` for a tombstone. */
  readonly value: JValue | undefined;
  /** `1 +` the key's UTF-8 length `+` the value's weight: the entry's share of the map's weight (0 for a tombstone). */
  readonly own: bigint;
  /** The base's entry at this key, which this node shadows, as its share of the base's weight and its value's depth. */
  readonly shadows: { readonly own: bigint; readonly depth: number } | undefined;
  readonly left: Tree | undefined;
  readonly right: Tree | undefined;
  readonly height: number;
  /** The entries this subtree sets. */
  readonly present: number;
  /** The base entries this subtree shadows. */
  readonly shadowed: number;
  /** The weight this subtree's entries add. */
  readonly weight: bigint;
  /** The weight of the base entries this subtree shadows. */
  readonly shadowedWeight: bigint;
  /** The depth of the deepest value this subtree sets, -1 if it sets none: exact, as every value's depth is. */
  readonly deepest: number;
}

const heightOf = (t: Tree | undefined): number => t?.height ?? 0;

type Content = Pick<Tree, "key" | "value" | "own" | "shadows">;

function node(c: Content, left: Tree | undefined, right: Tree | undefined): Tree {
  return {
    key: c.key,
    value: c.value,
    own: c.own,
    shadows: c.shadows,
    left,
    right,
    height: Math.max(heightOf(left), heightOf(right)) + 1,
    present: (left?.present ?? 0) + (right?.present ?? 0) + (c.value === undefined ? 0 : 1),
    shadowed: (left?.shadowed ?? 0) + (right?.shadowed ?? 0) + (c.shadows === undefined ? 0 : 1),
    weight: saturated((left?.weight ?? 0n) + (right?.weight ?? 0n) + c.own),
    shadowedWeight: saturated((left?.shadowedWeight ?? 0n) + (right?.shadowedWeight ?? 0n) + (c.shadows?.own ?? 0n)),
    deepest: Math.max(left?.deepest ?? -1, right?.deepest ?? -1, c.value?.depth ?? -1),
  };
}

const share = (key: string, value: JValue): bigint => saturated(1n + BigInt(utf8Length(key)) + value.weight);

/** `t` with new children. */
const withChildren = (t: Tree, left: Tree | undefined, right: Tree | undefined): Tree => node(t, left, right);

/** `t`'s subtrees rebalanced, as an AVL insertion or deletion leaves them: at most one double rotation. */
function balance(t: Tree): Tree {
  const lh = heightOf(t.left);
  const rh = heightOf(t.right);
  if (lh > rh + 1 && t.left !== undefined) {
    const l = t.left;
    // left-right: rotate the left child left first
    const pivot = heightOf(l.left) >= heightOf(l.right) || l.right === undefined ? l : rotateLeft(l, l.right);
    return withChildren(pivot, pivot.left, withChildren(t, pivot.right, t.right));
  }
  if (rh > lh + 1 && t.right !== undefined) {
    const r = t.right;
    const pivot = heightOf(r.right) >= heightOf(r.left) || r.left === undefined ? r : rotateRight(r, r.left);
    return withChildren(pivot, withChildren(t, t.left, pivot.left), pivot.right);
  }
  return t;
}

function rotateLeft(t: Tree, r: Tree): Tree {
  return withChildren(r, withChildren(t, t.left, r.left), r.right);
}

function rotateRight(t: Tree, l: Tree): Tree {
  return withChildren(l, l.left, withChildren(t, l.right, t.right));
}

/** A step down a search path: the node, and whether the search went left from it. */
type Turn = readonly [Tree, boolean];

/** Rebuilds the path above `child`, bottom up, rebalancing each node: O(log n) nodes, the rest shared. */
function rebuild(path: readonly Turn[], child: Tree | undefined): Tree | undefined {
  let t = child;
  for (let i = path.length - 1; i >= 0; i--) {
    const turn = path[i];
    if (turn === undefined) continue;
    const [p, left] = turn;
    t = balance(left ? withChildren(p, t, p.right) : withChildren(p, p.left, t));
  }
  return t;
}

/** `t` without its least entry, and that entry. */
function withoutLeast(t: Tree): readonly [Tree | undefined, Tree] {
  const path: Turn[] = [];
  let here = t;
  for (let l = t.left; l !== undefined; l = l.left) {
    path.push([here, true]);
    here = l;
  }
  return [rebuild(path, here.right), here];
}

/** `t` without its root node. */
function withoutRoot(t: Tree): Tree | undefined {
  if (t.left === undefined) return t.right;
  if (t.right === undefined) return t.left;
  const [rest, least] = withoutLeast(t.right);
  return balance(node(least, t.left, rest));
}

/** A count of shadowed base values by depth, as sorted `[depth, count]` pairs: copied on change, at most 257 long. */
type DepthCounts = readonly (readonly [number, number])[];

function counted(counts: DepthCounts, depth: number): DepthCounts {
  const i = counts.findIndex(([d]) => d === depth);
  if (i < 0) return [...counts, [depth, 1] as const].sort((a, b) => a[0] - b[0]);
  return counts.map(([d, n], j) => (j === i ? ([d, n + 1] as const) : ([d, n] as const)));
}

const countAt = (counts: DepthCounts, depth: number): number => counts.find(([d]) => d === depth)?.[1] ?? 0;

/** How a map over a base came from the one before it: the key it set (to `value`) or removed (no `value`). */
interface Derived {
  readonly from: JMap;
  readonly key: string;
  readonly value: JValue | undefined;
}

/**
 * A map the machine built, or updated: a persistent AVL tree in code-point order of its keys, over an optional core
 * map, its base. A node sets an entry, or hides one of the base's (a tombstone); every other base entry shows through,
 * read by the base's own search. So an update costs O(log n) whatever the base's size, and touches no entry it does
 * not set or remove: the size, weight and depth are kept exactly from the nodes' cached aggregates and the base's own
 * (the depth, once a map hides one of the base's deepest values, from the base's count of values by depth).
 * Keys are compared, never hashed, so a program cannot make them collide. The map never holds null.
 */
export class JMap {
  readonly kind = "map";
  readonly weight: bigint;
  readonly depth: number;
  readonly size: number;
  /** The same map with no base, once {@link JMap.compact} has made it. A cache: written once, from the map alone. */
  #compacted: JMap | undefined;
  private constructor(
    private readonly root: Tree | undefined,
    private readonly base: Base | undefined,
    private readonly shadowedDepths: DepthCounts,
    /** For a map over a base, the step that made it from another: what its compacted form is replayed from. */
    private readonly derived?: Derived,
  ) {
    const b = base?.map;
    this.size = (b?.size ?? 0) - (root?.shadowed ?? 0) + (root?.present ?? 0);
    const own = root?.weight ?? 0n;
    this.weight = b === undefined ? saturated(1n + own) : b.weight >= MAX_WEIGHT ? MAX_WEIGHT : saturated(b.weight - (root?.shadowedWeight ?? 0n) + own);
    this.depth = Math.max(root?.deepest ?? -1, this.baseDeepest(), 0) + 1;
    Object.freeze(this);
  }

  /**
   * The depth of the deepest base value still showing, -1 if none, exactly: the base's own depth, unless the map hides
   * a value at that depth; then the base's count of values by depth decides, less the hidden ones, read from the
   * deepest down. O(1) in the base's size: the count takes one pass over the host map, once per evaluation, the work
   * the README's "Work outside gas" bounds by the host data; then each update reads at most one count per depth and
   * per hidden depth (each at most 257).
   */
  private baseDeepest(): number {
    const b = this.base;
    if (b === undefined || b.map.size - (this.root?.shadowed ?? 0) === 0) return -1;
    const top = b.map.depth - 1;
    if (countAt(this.shadowedDepths, top) === 0) return top;
    const all = b.depths();
    // the hidden counts are sorted by depth: walked down alongside, once
    let h = this.shadowedDepths.length - 1;
    for (let d = top; d >= 0; d--) {
      for (; h >= 0 && (this.shadowedDepths[h]?.[0] ?? -1) > d; h--);
      const hidden = h >= 0 && this.shadowedDepths[h]?.[0] === d ? (this.shadowedDepths[h]?.[1] ?? 0) : 0;
      if ((all[d] ?? 0) - hidden > 0) return d;
    }
    return -1;
  }

  /** The empty map. */
  static readonly empty: JMap = new JMap(undefined, undefined, []);

  /** A map over the core map `m`, sharing it: O(1). */
  static over(m: MapValue): JMap {
    return new JMap(undefined, new Base(m), []);
  }

  /** A map of the root `t` over this map's base, unless it nests deeper than `MAX_DEPTH`. */
  private rooted(t: Tree | undefined, shadowedDepths: DepthCounts, derived?: Derived): JMap | undefined {
    const m = new JMap(t, this.base, shadowedDepths, derived);
    return m.depth > MAX_DEPTH ? undefined : m;
  }

  /**
   * The map of `entries`, which are in code-point order of their distinct keys; null values are dropped. `undefined`
   * if it nests deeper than `MAX_DEPTH`. O(n): a balanced tree built bottom up, without comparing a key. `keyBytes`,
   * when given, holds each key's UTF-8 length, found once (a map expression's keys, at compilation), so building the
   * map reads no key; otherwise each key is measured.
   */
  static sorted(entries: readonly (readonly [string, JValue])[], keyBytes?: readonly number[]): JMap | undefined {
    const kept = entries.flatMap(([k, v], i) => (v.kind === "null" ? [] : [[k, v, keyBytes?.[i] ?? utf8Length(k)] as const]));
    // each range [lo, hi) is built once its halves are: a post-order walk with an explicit stack
    const built: (Tree | undefined)[] = [];
    const todo: [number, number, boolean][] = [[0, kept.length, false]];
    for (let top = todo.pop(); top !== undefined; top = todo.pop()) {
      const [lo, hi, halves] = top;
      if (lo >= hi) {
        built.push(undefined);
        continue;
      }
      const mid = lo + Math.floor((hi - lo) / 2);
      if (!halves) {
        todo.push([lo, hi, true], [mid + 1, hi, false], [lo, mid, false]);
        continue;
      }
      const right = built.pop();
      const left = built.pop();
      const e = kept[mid];
      if (e === undefined) continue;
      built.push(node({ key: e[0], value: e[1], own: saturated(1n + BigInt(e[2]) + e[1].weight), shadows: undefined }, left, right));
    }
    return JMap.empty.rooted(built.pop(), []);
  }

  /** The node at `key`, and the path to it (or to where it would go). */
  private find(key: string): readonly [Tree | undefined, Turn[]] {
    const path: Turn[] = [];
    for (let t = this.root; t !== undefined; ) {
      const c = compareCodePoints(key, t.key);
      if (c === 0) return [t, path];
      path.push([t, c < 0]);
      t = c < 0 ? t.left : t.right;
    }
    return [undefined, path];
  }

  /** The value at `key`, if present: O(log n) comparisons, in the tree and then in the base. */
  get(key: string): JValue | undefined {
    const [t] = this.find(key);
    return t !== undefined ? t.value : this.base?.map.get(key);
  }

  /** A new node for `key`, shadowing the base's entry there if it has one. */
  private fresh(key: string, value: JValue | undefined): readonly [Content, DepthCounts] {
    const under = this.base?.map.get(key);
    const shadows = under === undefined ? undefined : { own: share(key, under), depth: under.depth };
    const own = value === undefined ? 0n : share(key, value);
    return [{ key, value, own, shadows }, shadows === undefined ? this.shadowedDepths : counted(this.shadowedDepths, shadows.depth)];
  }

  /**
   * This map with `key` set to `value`, or removed when `value` is null: O(log n), sharing every subtree it does not
   * touch. `undefined` if the result nests deeper than `MAX_DEPTH`.
   */
  set(key: string, value: JValue): JMap | undefined {
    if (value.kind === "null") return this.removed(key);
    const [t, path] = this.find(key);
    const derived = this.base === undefined ? undefined : { from: this, key, value };
    if (t !== undefined) {
      const replaced = this.rooted(rebuild(path, node({ key: t.key, value, own: share(t.key, value), shadows: t.shadows }, t.left, t.right)), this.shadowedDepths, derived);
      return replaced === undefined ? undefined : JMap.settled(replaced);
    }
    const [content, depths] = this.fresh(key, value);
    const added = this.rooted(rebuild(path, node(content, undefined, undefined)), depths, derived);
    return added === undefined ? undefined : JMap.settled(added);
  }

  /** This map without `key`: O(log n). Removing never deepens a map. */
  removed(key: string): JMap {
    const [t, path] = this.find(key);
    const derived = this.base === undefined ? undefined : { from: this, key, value: undefined };
    if (t === undefined) {
      if (this.base?.map.get(key) === undefined) return this;
      // a tombstone hides the base's entry
      const [content, depths] = this.fresh(key, undefined);
      return JMap.settled(new JMap(rebuild(path, node(content, undefined, undefined)), this.base, depths, derived));
    }
    if (t.value === undefined) return this;
    const rest = t.shadows === undefined ? withoutRoot(t) : node({ key: t.key, value: undefined, own: 0n, shadows: t.shadows }, t.left, t.right);
    return JMap.settled(new JMap(rebuild(path, rest), this.base, this.shadowedDepths, derived));
  }

  /**
   * `m`, or its compacted form once it hides more of its base than shows through, or hides an entry of a base whose
   * weight saturates (only a recount finds what is left). So a map's tombstones never outnumber the base entries it
   * lists, and listing it is linear in its live entries.
   */
  private static settled(m: JMap): JMap {
    const b = m.base;
    const hidden = m.root?.shadowed ?? 0;
    if (b === undefined || hidden === 0) return m;
    return hidden > b.map.size - hidden || b.map.weight >= MAX_WEIGHT ? JMap.compact(m) : m;
  }

  /**
   * `m` with no base: its live entries in the machine's own tree. The map over a core map is converted once (its
   * entries sorted, O(n log n)), and every map derived from it replays its one step onto its predecessor's compacted
   * form (O(log n)), each kept once made: so compacting costs the conversion once per host map, and a search per
   * update. The conversion happens only once a lineage has hidden half the host map (each hiding a charged update),
   * or hidden an entry of a host map whose weight saturates (at least 2^63, so built with shared parts): then a single
   * update converts it. That is O(n log n) in the host map the caller supplied, once per such map per evaluation,
   * and not paid by gas: gas prices the validating nodes' work (Scala, Rust), whose maps carry their entries' weights;
   * this package is a client, whose gas has only to match theirs.
   */
  private static compact(m: JMap): JMap {
    const chain: JMap[] = [];
    let v = m;
    for (; v.base !== undefined && v.#compacted === undefined && v.derived !== undefined; v = v.derived.from) chain.push(v);
    let at = v.#compacted ?? (v.base === undefined ? v : (JMap.sorted(v.base.sorted()) ?? v));
    v.#compacted = at;
    for (let i = chain.length - 1; i >= 0; i--) {
      const w = chain[i];
      const step = w?.derived;
      if (w === undefined || step === undefined) continue;
      // a step that succeeded on the map it was taken from succeeds on the same entries
      at = (step.value === undefined ? at.removed(step.key) : at.set(step.key, step.value)) ?? at;
      w.#compacted = at;
    }
    return at;
  }

  /** The tree's nodes in code-point order of their keys: an in-order walk with an explicit stack of at most the height. */
  private nodes(): readonly Tree[] {
    const out: Tree[] = [];
    const stack: Tree[] = [];
    for (let t = this.root; t !== undefined || stack.length > 0; ) {
      if (t !== undefined) {
        stack.push(t);
        t = t.left;
        continue;
      }
      const top = stack.pop();
      if (top === undefined) break;
      out.push(top);
      t = top.right;
    }
    return out;
  }

  /**
   * The entries in code-point order of their keys: the tree's merged with the base's that show through. O(n), plus
   * sorting the base by code point once, O(n log n): what reads every entry pays for them by weight.
   */
  entries(): readonly (readonly [string, JValue])[] {
    const mine = this.nodes();
    const theirs = this.base?.sorted() ?? [];
    const out: (readonly [string, JValue])[] = [];
    let [i, j] = [0, 0];
    for (; i < mine.length || j < theirs.length; ) {
      const a = mine[i];
      const b = theirs[j];
      const c = a === undefined ? 1 : b === undefined ? -1 : compareCodePoints(a.key, b[0]);
      if (c <= 0 && a !== undefined) {
        if (a.value !== undefined) out.push([a.key, a.value]);
        i++;
        // the tree's node shadows the base's entry at the same key
        if (c === 0) j++;
      } else if (b !== undefined) {
        out.push(b);
        j++;
      }
    }
    return out;
  }
}


/** Whether `v` is a container the machine built, which has no core form yet. */
const isOwn = (v: JValue): v is JArray | JMap => v instanceof JArray || v instanceof JMap;

/** A container being converted to a core value: its parts, the next one, and the parts converted. */
interface Converting {
  readonly of: JArray | JMap;
  readonly keys: readonly string[];
  readonly parts: readonly JValue[];
  readonly out: Value[];
  at: number;
}

/**
 * One evaluation's conversions between the two kinds of container, remembered by identity so a container converted
 * twice (a shared part, or one compared again) costs once. Its entries are only ever added, and each one is a pure
 * function of its key, so remembering one changes no result.
 */
export class Heap {
  readonly #values = new WeakMap<JArray | JMap, Value>();
  readonly #maps = new WeakMap<MapValue, JMap>();

  /**
   * `v` as a core value: itself if it is one, otherwise each container converted once, bottom up, with an explicit
   * stack. `undefined` only if a container was refused, which cannot happen: each was checked when it was built.
   *
   * Converting an evaluation's result happens after its last charge. It is bounded by the result's weight, at most
   * 2^24 (every container the machine builds is checked against that bound), times the log of a map's size for
   * sorting its keys; not by the gas used. A host map the result updated is listed whole, within the same bound.
   */
  value(v: JValue): Value | undefined {
    if (!isOwn(v)) return v;
    const known = this.#values.get(v);
    if (known !== undefined) return known;
    const stack: Converting[] = [this.#open(v)];
    for (let top = stack.at(-1); top !== undefined; top = stack.at(-1)) {
      const part = top.parts[top.at];
      if (part !== undefined) {
        const done = isOwn(part) ? this.#values.get(part) : part;
        if (done !== undefined) {
          top.out.push(done);
          top.at++;
        } else if (isOwn(part)) stack.push(this.#open(part));
        continue;
      }
      stack.pop();
      const built = top.of instanceof JArray ? ArrayValue.of(top.out) : MapValue.of(top.keys.map((k, i) => [k, top.out[i] ?? Value.null] as const));
      if (!built.ok) return undefined;
      this.#values.set(top.of, built.value);
      const below = stack.at(-1);
      if (below === undefined) return built.value;
      below.out.push(built.value);
      below.at++;
    }
    return undefined;
  }

  #open(c: JArray | JMap): Converting {
    if (c instanceof JArray) return { of: c, keys: [], parts: c.items, out: [], at: 0 };
    const es = c.entries();
    return { of: c, keys: es.map(([k]) => k), parts: es.map(([, x]) => x), out: [], at: 0 };
  }

  /**
   * `m` as a map the machine can update: its own as it is, and core's as the base of an empty tree, in O(1), copying
   * nothing. The same core map is the same base each time, so what its base finds once (its entries sorted by code
   * point, its values' depths) is found once per evaluation.
   */
  own(m: AnyMap): JMap {
    if (m instanceof JMap) return m;
    const known = this.#maps.get(m);
    if (known !== undefined) return known;
    const over = JMap.over(m);
    this.#maps.set(m, over);
    return over;
  }
}
