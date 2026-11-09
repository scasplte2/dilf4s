/**
 * The built bin over stdio, as an MCP client starts it: spawned from `dist/bin.js`, initialised, its tools listed, and a
 * program evaluated. Run after the build (the package's postbuild), since it needs `dist`.
 */
import { existsSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import { describe, expect, it } from "vitest";

const bin = fileURLToPath(new URL("../dist/bin.js", import.meta.url));

describe("the built bin over stdio", { timeout: 60_000 }, () => {
  it("initialises, lists its tools and evaluates a program", async () => {
    expect(existsSync(bin), `${bin} is built`).toBe(true);
    const client = new Client({ name: "dilf4s-mcp-smoke", version: "0.0.0" });
    await client.connect(new StdioClientTransport({ command: process.execPath, args: [bin], stderr: "inherit" }));
    expect(client.getServerVersion()).toMatchObject({ name: "dilf4s-jlvm" });
    const { tools } = await client.listTools();
    expect(tools.map((t) => t.name)).toEqual(["jlvm_check", "jlvm_evaluate", "jlvm_ops"]);
    const r = await client.callTool({ name: "jlvm_evaluate", arguments: { program: '{"+": [1, {"var": "x"}]}', data: '{"x": 2}' } });
    expect(r.structuredContent).toMatchObject({ ok: true, outcome: { value: { json: "3" } }, gasLimit: 2 ** 24 });
    const refused = await client.callTool({ name: "jlvm_evaluate", arguments: { program: "1", gasLimit: 2 ** 24 + 1 } });
    expect(refused.structuredContent).toMatchObject({ ok: false, refusal: { kind: "GasLimitOutOfRange" } });
    await client.close();
  });
});
