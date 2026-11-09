/**
 * The server through an MCP client in memory: the tools it lists, structured refusals of wrong-typed, missing and
 * unknown arguments, the size and gas caps (by counts, never by time), and the same result for the same call.
 */
import { DEFAULT_GAS_LIMIT, MAX_GAS_LIMIT, parse } from "@kd5ujc/dilf4s-jlvm";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { createServer, DEFAULT_GAS_CAP, EXPONENT_CEILING, GAS_CAP_CEILING, MAX_BINDINGS, MAX_BINDINGS_BYTES, MAX_TEXT_BYTES, measure, TOOL_NAMES, TOOLS } from "../src/index.js";
import { connected, expectOk, refusalKind, type Called } from "./support.js";

let session: Awaited<ReturnType<typeof connected>>;
beforeAll(async () => {
  session = await connected();
});
afterAll(async () => {
  await session.close();
});

const call = (name: string, args?: unknown): Promise<Called> => session.call(name, args);
const evaluate = (args: unknown): Promise<Called> => call("jlvm_evaluate", args);
const where = (c: Called): unknown => (c.structured["refusal"] as { readonly where: unknown }).where;

const hexId = (text: string): string => Buffer.from(expectOk(expectOk(parse(text), "parses").id(), "an id")).toString("hex");

describe("the tools", { timeout: 60_000 }, () => {
  it("are listed with their argument and result schemas", () => {
    expect(TOOLS.map((t) => t.name)).toEqual([...TOOL_NAMES]);
    for (const t of TOOLS) expect([t.inputSchema["type"], t.outputSchema["type"]], t.name).toEqual(["object", "object"]);
  });

  it("jlvm_check gives a program's identity, canonical form, weight and depth", async () => {
    const r = await call("jlvm_check", { program: '{"+": [1, {"var": "x"}]}' });
    expect(r.isError).toBe(false);
    expect(r.structured).toMatchObject({ ok: true, id: hexId('{"+": [1, {"var": "x"}]}') });
    expect(typeof r.structured["canonical"]).toBe("string");
    expect(r.structured["depth"]).toBeGreaterThan(0);
    // an alias names the same program
    expect((await call("jlvm_check", { program: '{"size": ["abc"]}' })).structured["id"]).toBe(hexId('{"length": ["abc"]}'));
  });

  it("jlvm_check refuses text that is not JSON, with the offset of the error, and a program that does not compile", async () => {
    const bad = await call("jlvm_check", { program: '{"+": [1, }' });
    expect(bad.isError, "a refusal is an ordinary result").toBe(false);
    expect(refusalKind(bad)).toBe("Malformed");
    expect(where(bad)).toEqual({ argument: "program", offset: 10 });
    const unknown = await call("jlvm_check", { program: '{"nope": [1]}' });
    expect(refusalKind(unknown)).toBe("Malformed");
    const uncompiled = await call("jlvm_check", { program: '{"let": [[["x", 1], ["x", 2]], {"var": "$x"}]}' });
    expect(refusalKind(uncompiled), "a duplicate binding is refused by compile").toBe("Malformed");
    expect(where(uncompiled)).toEqual({ argument: "program" });
  });

  it("jlvm_evaluate runs a program on data and bindings: the value, gas, warnings and statement", async () => {
    const r = await evaluate({ program: '{"*": [{"var": "$rate"}, {"var": "amount"}]}', data: '{"amount": 40}', bindings: { rate: "0.5" }, gasLimit: 100_000 });
    expect(r.structured).toMatchObject({ ok: true, outcome: { value: { json: "20", cbor: "14", weight: 1 } }, gasLimit: 100_000, warnings: [] });
    expect(r.structured["gasUsed"]).toBeGreaterThan(0);
    expect((r.structured["statement"] as Record<string, unknown>)["program"]).toBe(hexId('{"*": [{"var": "$rate"}, {"var": "amount"}]}'));
    const bytes = await evaluate({ program: '{"sha256": ["abc"]}' });
    expect(bytes.structured).toMatchObject({ outcome: { value: { json: '"0xba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"' } } });
    const hexText = await evaluate({ program: '{"hex": ["0x01"]}' });
    expect(hexText.structured).toMatchObject({ outcome: { value: { cbor: "6430783031", omitted: { json: "NoSurfaceForm" } } } });
  });

  it("jlvm_evaluate reports an error as the vectors record it, and running out of gas uses the whole limit", async () => {
    const e = await evaluate({ program: '{"+": [1, {"abs": "x"}]}' });
    expect(e.structured).toMatchObject({ ok: true, outcome: { error: { error: "Type", op: "abs" } } });
    const o = await evaluate({ program: '{"map": [[1, 2, 3, 4, 5, 6, 7, 8], {"+": [1, 2]}]}', gasLimit: 50 });
    expect(o.structured).toMatchObject({ ok: true, outcome: { error: { error: "OutOfGas", available: 50 } }, gasUsed: 50 });
    const unparsed = await evaluate({ program: "1", data: "{" });
    expect(refusalKind(unparsed)).toBe("Malformed");
    expect(where(unparsed)).toMatchObject({ argument: "data" });
    const binding = await evaluate({ program: "1", bindings: { a: "1", b: "[" } });
    expect(where(binding)).toMatchObject({ argument: "bindings", binding: "b" });
  });

  it("jlvm_ops lists every operation, or the one an alias names", async () => {
    const all = await call("jlvm_ops", {});
    expect((all.structured["ops"] as readonly unknown[]).length).toBe(93);
    expect(all.structured["gas"]).toMatchObject({ defaultLimit: DEFAULT_GAS_LIMIT, cap: DEFAULT_GAS_CAP, literal: 10 });
    const one = await call("jlvm_ops", { op: "size" });
    expect((one.structured["ops"] as readonly { readonly op: string }[]).map((e) => e.op)).toEqual(["length"]);
    expect(refusalKind(await call("jlvm_ops", { op: "noop" }))).toBe("InvalidArguments");
  });

  it("the same call gives the same result", async () => {
    const args = { program: '{"merge": [{"var": "a"}, {"var": "$b"}]}', data: '{"a": {"x": [1, 2.5, "0x00"]}}', bindings: { b: '{"y": true}' } };
    const [a, b] = [await evaluate(args), await evaluate(args)];
    expect(a.text).toBe(b.text);
  });
});

describe("arguments of the wrong type, missing or unknown are structured refusals", { timeout: 60_000 }, () => {
  const misuse = async (name: string, args: unknown, argument: string): Promise<void> => {
    const r = await call(name, args);
    expect([refusalKind(r), r.isError], JSON.stringify(args)).toEqual(["InvalidArguments", true]);
    expect(where(r)).toMatchObject({ argument });
  };

  it("in each tool", async () => {
    await misuse("jlvm_check", {}, "program");
    await misuse("jlvm_check", { program: 1 }, "program");
    await misuse("jlvm_check", { program: "1", extra: 1 }, "arguments");
    await misuse("jlvm_evaluate", { data: "1" }, "program");
    await misuse("jlvm_evaluate", { program: "1", data: null }, "data");
    await misuse("jlvm_evaluate", { program: "1", data: 1 }, "data");
    await misuse("jlvm_evaluate", { program: "1", bindings: ["1"] }, "bindings");
    await misuse("jlvm_evaluate", { program: "1", bindings: { a: 1 } }, "bindings");
    await misuse("jlvm_evaluate", { program: "1", gas: 1 }, "arguments");
    await misuse("jlvm_ops", { op: 1 }, "op");
  });

  it("an unknown field is named by its preview only", async () => {
    const r = await call("jlvm_check", { program: "1", ["k".repeat(100_000)]: 1 });
    expect((r.structured["refusal"] as { readonly message: string }).message.length).toBeLessThan(200);
  });

  it("an unknown tool", async () => {
    const r = await call("jlvm_run", { program: "1" });
    expect([refusalKind(r), r.isError]).toEqual(["UnknownTool", true]);
  });
});

describe("the gas cap", { timeout: 60_000 }, () => {
  it("is the default limit, 2^24, and a call above it is refused, never clamped", async () => {
    expect(DEFAULT_GAS_CAP).toBe(2 ** 24);
    expect((await evaluate({ program: "1", gasLimit: DEFAULT_GAS_CAP })).structured).toMatchObject({ ok: true, gasLimit: DEFAULT_GAS_CAP });
    for (const g of [DEFAULT_GAS_CAP + 1, GAS_CAP_CEILING, MAX_GAS_LIMIT]) expect(refusalKind(await evaluate({ program: "1", gasLimit: g })), String(g)).toBe("GasLimitOutOfRange");
  });

  it("refuses a limit that is not a whole number from 0: fractional, negative, past 2^53 (as JSON's 9223372036854775807 arrives, 2^63), or text", async () => {
    for (const g of [1.5, -1, 2 ** 53, 2 ** 63, "10", null]) expect(refusalKind(await evaluate({ program: "1", gasLimit: g })), String(g)).toBe("InvalidArguments");
  });

  it("can be raised to 2^32 and no further, and is never Gas.Max", async () => {
    expect(createServer({ gasCap: GAS_CAP_CEILING }).ok).toBe(true);
    for (const g of [GAS_CAP_CEILING + 1, MAX_GAS_LIMIT, 2 ** 63 - 1, -1, 1.5, Number.NaN]) expect(createServer({ gasCap: g }).ok, String(g)).toBe(false);
    const raised = await connected({ gasCap: GAS_CAP_CEILING });
    expect((await raised.call("jlvm_evaluate", { program: "1", gasLimit: GAS_CAP_CEILING })).structured["ok"]).toBe(true);
    expect(refusalKind(await raised.call("jlvm_evaluate", { program: "1", gasLimit: GAS_CAP_CEILING + 1 }))).toBe("GasLimitOutOfRange");
    await raised.close();
  });
});

describe("the size caps", { timeout: 120_000 }, () => {
  const textOf = (bytes: number): string => `"${"a".repeat(bytes - 2)}"`;

  it("a text measuring the cap is read, and one byte more is refused before it is parsed", async () => {
    expect(measure(textOf(MAX_TEXT_BYTES), MAX_TEXT_BYTES)).toBe(MAX_TEXT_BYTES);
    expect((await evaluate({ program: '{"length": [{"var": ""}]}', data: textOf(MAX_TEXT_BYTES) })).structured).toMatchObject({ ok: true, outcome: { value: { json: String(MAX_TEXT_BYTES - 2) } } });
    const over = await evaluate({ program: "1", data: textOf(MAX_TEXT_BYTES + 1) });
    expect([refusalKind(over), where(over)]).toEqual(["InputTooLarge", { argument: "data" }]);
    expect(refusalKind(await call("jlvm_check", { program: textOf(MAX_TEXT_BYTES + 1) }))).toBe("InputTooLarge");
    // not JSON at all, yet refused by its length: nothing reads it
    expect(refusalKind(await evaluate({ program: "{".repeat(MAX_TEXT_BYTES + 1) }))).toBe("InputTooLarge");
  });

  it("each number's exponent counts as that many digits, so a few short numbers cannot cost a megabyte's work", async () => {
    const numbers = (n: number): string => `[${Array.from({ length: n }, () => "8e9999").join(",")}]`;
    // n numbers measure 2 + 7n - 1 characters, plus 9999 for each exponent
    const fits = Math.floor((MAX_TEXT_BYTES - 1) / (7 + 9999));
    expect(measure(numbers(fits), MAX_TEXT_BYTES)).toBe(1 + 7 * fits + 9999 * fits);
    expect(measure(numbers(fits + 1), MAX_TEXT_BYTES)).toBeGreaterThan(MAX_TEXT_BYTES);
    expect(numbers(fits + 1).length, "a short text").toBeLessThan(MAX_TEXT_BYTES / 1000);
    expect((await evaluate({ program: '{"length": [{"var": ""}]}', data: numbers(fits) })).structured).toMatchObject({ ok: true, outcome: { value: { json: String(fits) } } });
    expect(refusalKind(await evaluate({ program: "1", data: numbers(fits + 1) }))).toBe("InputTooLarge");
    expect(refusalKind(await call("jlvm_check", { program: `{"literal": ${numbers(fits + 1)}}` }))).toBe("InputTooLarge");
    // an exponent past the largest scale is refused by the parser unconverted, so it counts no more than the ceiling
    expect(measure("1e999999999999", MAX_TEXT_BYTES)).toBe(14 + EXPONENT_CEILING);
    expect(refusalKind(await evaluate({ program: "1", data: "1e999999999999" }))).toBe("Malformed");
  });

  it("the bindings are capped in number and in measure together, names included", async () => {
    const many = Object.fromEntries(Array.from({ length: MAX_BINDINGS + 1 }, (_, i) => [`b${i.toString()}`, "1"]));
    expect(refusalKind(await evaluate({ program: "1", bindings: many }))).toBe("InputTooLarge");
    const half = textOf(MAX_BINDINGS_BYTES / 2);
    expect(refusalKind(await evaluate({ program: "1", bindings: { a: half, b: half } })), "two halves and their names").toBe("InputTooLarge");
    expect((await evaluate({ program: "1", bindings: { a: half } })).structured["ok"]).toBe(true);
  });
});
