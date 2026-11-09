/**
 * A value as JSON surface text (`JsonSurface.render`, the jlvm package's `renderValue`): canonical JSON with bytes in
 * `0x`-hex form, so that `parseValue` reads it back as the same value. It is what `jlvm_v1.json` records as a value's
 * `json`. A value holding text of the `0x`-hex form (which would read back as bytes, a map key aside) or a
 * non-terminating rational has none; the conformance test checks this rendering against every recorded `json`.
 */
import { ArrayValue, json, MapValue, TextValue, toHex, Value, type Result } from "@kd5ujc/dilf4s-core";

/** Why a value has no surface text here: it has none, or it is longer than the limit. */
export type NoSurface = "NoSurfaceForm" | "TooLarge";

const HEX_TEXT = /^0x(?:[0-9a-f]{2})*$/;

/** Whether a text value (not a map key) in `root` has the `0x`-hex form. An explicit stack. */
function hasHexText(root: Value): boolean {
  const todo: Value[] = [root];
  for (let v = todo.pop(); v !== undefined; v = todo.pop()) {
    if (v.kind === "text" && HEX_TEXT.test(v.value)) return true;
    // pushed one by one: spreading a long array into a call's arguments could overflow the engine's limit
    if (v.kind === "array") for (const x of v.items) todo.push(x);
    else if (v.kind === "map") for (const [, x] of v.entries()) todo.push(x);
  }
  return false;
}

type Rebuilding = { readonly map: false; readonly items: readonly Value[]; readonly out: Value[] } | { readonly map: true; readonly entries: readonly (readonly [string, Value])[]; readonly out: Value[] };

/** Bytes as text in `0x`-hex form, everywhere in `root`: an explicit stack, each container rebuilt once. */
function hexed(root: Value): Value | undefined {
  const leaf = (v: Value): Value | undefined => {
    if (v.kind !== "bytes") return v;
    const h = toHex(v.bytes);
    const t = h.ok ? TextValue.of(`0x${h.value}`) : undefined;
    return t?.ok === true ? t.value : undefined;
  };
  const open = (v: Value): Rebuilding | undefined => (v.kind === "array" ? { map: false, items: v.items, out: [] } : v.kind === "map" ? { map: true, entries: v.entries(), out: [] } : undefined);
  const first = open(root);
  if (first === undefined) return leaf(root);
  const stack: Rebuilding[] = [first];
  for (let top = stack.at(-1); top !== undefined; top = stack.at(-1)) {
    const i = top.out.length;
    const next = top.map ? top.entries[i]?.[1] : top.items[i];
    if (next !== undefined) {
      const o = open(next);
      if (o !== undefined) stack.push(o);
      else {
        const l = leaf(next);
        if (l === undefined) return undefined;
        top.out.push(l);
      }
      continue;
    }
    stack.pop();
    const t = top;
    const built = t.map ? MapValue.of(t.entries.map(([k], j) => [k, t.out[j] ?? Value.null] as const)) : ArrayValue.of(t.out);
    if (!built.ok) return undefined;
    const below = stack.at(-1);
    if (below === undefined) return built.value;
    below.out.push(built.value);
  }
  return root;
}

/**
 * The surface text of `v`, if it has one of at most `maxBytes` UTF-8 bytes. A value's text is never shorter than its
 * weight, so a heavier one is refused in O(1), before it is walked; otherwise the work is O(weight).
 */
export function renderSurface(v: Value, maxBytes: number): Result<string, NoSurface> {
  if (v.weight > BigInt(maxBytes)) return { ok: false, error: "TooLarge" };
  if (hasHexText(v)) return { ok: false, error: "NoSurfaceForm" };
  const c = hexed(v);
  if (c === undefined) return { ok: false, error: "NoSurfaceForm" };
  const r = json.renderWithin(c, maxBytes);
  if (r.ok) return r;
  return { ok: false, error: r.error.kind === "TooLarge" ? "TooLarge" : "NoSurfaceForm" };
}
