/** Prefix-free typed byte views. Payload access is constant time and returns an unsigned byte. */
export interface RadixKey {
  readonly length: number;
  readonly byteAt: (index: number) => number;
}

export function radixKey(kind: number, length: number, payload: (index: number) => number): RadixKey {
  return {
    length: 9 + length,
    byteAt: (i) => i < 8 ? Math.floor(length / 2 ** (8 * (7 - i))) % 256 : i === 8 ? kind : payload(i - 9),
  };
}

export interface RadixWork { readonly bytes: number; readonly branches: number }
export interface RadixResult { readonly found: boolean; readonly work: RadixWork }
interface Leaf { readonly kind: "leaf"; readonly representative: RadixKey }
interface Branch { readonly kind: "branch"; readonly representative: RadixKey; readonly bit: number; left: Node; right: Node }
type Node = Leaf | Branch;

// Division preserves positions past 2^32; bitwise shifts would truncate the index.
const bitAt = (key: RadixKey, bit: number): boolean => (key.byteAt(Math.floor(bit / 8)) & (128 >>> (bit % 8))) !== 0;

function differing(a: RadixKey, b: RadixKey): { readonly bit: number | undefined; readonly bytes: number } {
  const end = Math.min(a.length, b.length);
  for (let i = 0; i < end; i++) {
    const xor = a.byteAt(i) ^ b.byteAt(i);
    if (xor !== 0) return { bit: 8 * i + Math.clz32(xor) - 24, bytes: 2 * (i + 1) };
  }
  return { bit: undefined, bytes: 2 * end };
}

/** A compressed binary radix set: no hashing, recursion or comparison sorting. */
export class RadixIndex {
  private root: Node | undefined;

  private candidate(key: RadixKey, root: Node): { readonly key: RadixKey; readonly work: RadixWork } {
    let node = root;
    let bytes = 0;
    let branches = 0;
    while (node.kind === "branch") {
      branches++;
      if (node.bit >= 8 * key.length) break;
      bytes++;
      node = bitAt(key, node.bit) ? node.right : node.left;
    }
    return { key: node.representative, work: { bytes, branches } };
  }

  contains(key: RadixKey): RadixResult {
    if (this.root === undefined) return { found: false, work: { bytes: 0, branches: 0 } };
    const candidate = this.candidate(key, this.root);
    const diff = differing(key, candidate.key);
    return { found: diff.bit === undefined, work: { bytes: candidate.work.bytes + diff.bytes, branches: candidate.work.branches } };
  }

  add(key: RadixKey): { readonly added: boolean; readonly work: RadixWork } {
    if (this.root === undefined) {
      this.root = { kind: "leaf", representative: key };
      return { added: true, work: { bytes: 0, branches: 0 } };
    }
    const candidate = this.candidate(key, this.root);
    const diff = differing(key, candidate.key);
    let bytes = candidate.work.bytes + diff.bytes;
    let branches = candidate.work.branches;
    if (diff.bit === undefined) return { added: false, work: { bytes, branches } };
    let node = this.root;
    let parent: Branch | undefined;
    let right = false;
    while (node.kind === "branch" && node.bit < diff.bit) {
      parent = node;
      right = bitAt(key, node.bit);
      bytes++;
      branches++;
      node = right ? node.right : node.left;
    }
    const added: Leaf = { kind: "leaf", representative: key };
    const split: Branch = bitAt(key, diff.bit)
      ? { kind: "branch", bit: diff.bit, left: node, right: added, representative: node.representative }
      : { kind: "branch", bit: diff.bit, left: added, right: node, representative: key };
    bytes++;
    if (parent === undefined) this.root = split;
    else if (right) parent.right = split;
    else parent.left = split;
    return { added: true, work: { bytes, branches } };
  }
}
