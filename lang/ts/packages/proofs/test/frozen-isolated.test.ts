// Nothing here imports the package statically: each test loads the trie's operations table on its own first, as a
// caller importing the shipped trie-internal.js by its URL can, and changes it before trie.ts (which fills it) loads.
import { describe, expect, it, vi } from "vitest";

describe("the trie's operations table, changed before trie.ts loads", () => {
  it("an operation a caller put there first is replaced, and the table is frozen", async () => {
    vi.resetModules();
    const { TRIE } = await import("../src/trie-internal.js");
    const lie = (): unknown => ({ ok: true, value: true });
    Object.defineProperty(TRIE, "lookup", { get: () => lie, set: () => undefined, configurable: true });
    Object.defineProperty(TRIE, "check", { value: lie, writable: true, configurable: true });
    const { trie } = await import("../src/index.js");
    expect(Object.isFrozen(TRIE), "frozen once trie.ts has loaded").toBe(true);
    expect(Reflect.get(Object.getOwnPropertyDescriptor(TRIE, "lookup") ?? {}, "get"), "the accessor is gone").toBeUndefined();
    expect(Reflect.get(TRIE, "lookup"), "trie.ts's own lookup").not.toBe(lie);
    expect(Reflect.get(TRIE, "check"), "trie.ts's own check").not.toBe(lie);
    const hashing = trie.TrieHashing.of("SHA-256");
    const mode = trie.KeyMode.fixed(1);
    expect(hashing.ok && mode.ok).toBe(true);
    if (!hashing.ok || !mode.ok) return;
    const root = trie.trieRoot(hashing.value, mode.value, []);
    expect(root.ok).toBe(true);
  });

  it("one a caller fixed there first stops the package loading: nothing runs with it", async () => {
    vi.resetModules();
    const { TRIE } = await import("../src/trie-internal.js");
    Object.defineProperty(TRIE, "check", { value: () => ({ ok: true }), writable: false, configurable: false });
    await expect(import("../src/trie.js")).rejects.toThrow(TypeError);
  });
});
