/**
 * The stdio transport owns the framing so that no request goes unanswered: an envelope the SDK's schema refuses is
 * answered `Invalid Request` in its turn, input that cannot be answered is skipped without stopping the reading, and an
 * over-long line is skipped to its end.
 */
import { PassThrough, Writable } from "node:stream";

import { describe, expect, it } from "vitest";

import { admitted, createServer } from "../src/index.js";
import { stdioTransport } from "../src/stdio.js";

interface Reply {
  readonly id: number | string;
  readonly result?: unknown;
  readonly error?: { readonly code: number; readonly message: string };
}

async function serve(maxLineBytes = 1 << 20): Promise<{ readonly input: PassThrough; readonly replies: () => readonly Reply[]; readonly close: () => Promise<void> }> {
  const input = new PassThrough();
  const written: string[] = [];
  const out = new Writable({
    write(chunk: Buffer, _encoding, done) {
      written.push(chunk.toString("utf8"));
      done();
    },
  });
  const made = createServer();
  if (!made.ok) throw new Error(made.error);
  await made.value.connect(admitted(stdioTransport(input, out, maxLineBytes), input));
  const replies = (): readonly Reply[] =>
    written
      .join("")
      .split("\n")
      .filter((l) => l.length > 0)
      .map((l) => JSON.parse(l) as Reply);
  return { input, replies, close: () => made.value.close() };
}

const settle = async (until: () => boolean): Promise<void> => {
  for (let i = 0; i < 400 && !until(); i++) await new Promise((r) => setImmediate(r));
};

const ping = (id: number): string => `${JSON.stringify({ jsonrpc: "2.0", id, method: "ping" })}\n`;

describe("stdio transport", { timeout: 60_000 }, () => {
  it("answers a request whose envelope the SDK's schema refuses, in its turn, and goes on", async () => {
    const { input, replies, close } = await serve();
    // Codex's request: valid arguments, but params._meta null; the SDK's transport would drop it unanswered
    const call = { jsonrpc: "2.0", id: 1, method: "tools/call", params: { name: "jlvm_ops", arguments: {}, _meta: null } };
    input.write(`${JSON.stringify(call)}\n${ping(2)}`);
    await settle(() => replies().length >= 2);
    expect(replies().map((r) => r.id)).toEqual([1, 2]);
    expect(replies()[0]?.error?.code, "Invalid Request").toBe(-32600);
    expect(replies()[1]?.result, "the next request is answered").toEqual({});
    await close();
  });

  it("skips what has no id to answer, or is not JSON, and keeps reading", async () => {
    const { input, replies, close } = await serve();
    input.write(`${JSON.stringify({ jsonrpc: "2.0", method: 7 })}\nnot json at all\n{"jsonrpc":\n${ping(1)}`);
    await settle(() => replies().length >= 1);
    expect(replies().map((r) => r.id)).toEqual([1]);
    await close();
  });

  it("closes the transport on a line longer than its cap, whole or in pieces, so the client stops waiting", async () => {
    for (const pieces of [1, 64]) {
      const { input, replies, close } = await serve(256);
      const line = `${JSON.stringify({ jsonrpc: "2.0", id: 1, method: "ping", params: { pad: "x".repeat(4096) } })}\n${ping(2)}`;
      const size = Math.ceil(line.length / pieces);
      for (let i = 0; i < line.length; i += size) input.write(line.slice(i, i + size));
      await settle(() => input.destroyed);
      expect(input.destroyed, `the input is released (${pieces.toString()} pieces)`).toBe(true);
      expect(replies(), "nothing is answered after the oversized line").toEqual([]);
      await close();
    }
  });

  it("joins a line that arrives in pieces", async () => {
    const { input, replies, close } = await serve();
    const line = ping(1);
    input.write(line.slice(0, 7));
    input.write(line.slice(7, 20));
    input.write(line.slice(20) + ping(2));
    await settle(() => replies().length >= 2);
    expect(replies().map((r) => r.id)).toEqual([1, 2]);
    await close();
  });

  it("answers an envelope with an id and no method as Invalid Request, never a response, and goes on", async () => {
    const { input, replies, close } = await serve();
    // Codex's requests: an id but no method; a method with a result or an error field (still a request). A malformed
    // response (no method, an error that is not an error object) is not a request, so it is not answered
    const lines = [
      { jsonrpc: "2.0", id: 1, params: {} },
      { jsonrpc: "2.0", id: 3, method: "ping", result: {} },
      { jsonrpc: "2.0", id: 4, method: "tools/call", params: { name: "jlvm_ops", arguments: {} }, error: null },
      { jsonrpc: "2.0", id: 7, error: "x" },
    ];
    input.write(lines.map((l) => `${JSON.stringify(l)}\n`).join("") + ping(2));
    await settle(() => replies().length >= 4);
    expect(replies().map((r) => r.id)).toEqual([1, 3, 4, 2]);
    expect(replies().slice(0, 3).map((r) => r.error?.code), "Invalid Request").toEqual([-32600, -32600, -32600]);
    await close();
  });

  it("reads a line sent one byte at a time", async () => {
    const { input, replies, close } = await serve();
    const line = `${JSON.stringify({ jsonrpc: "2.0", id: 1, method: "ping", params: { _meta: { pad: "x".repeat(20_000) } } })}\n`;
    for (const byte of Buffer.from(line)) input.write(Buffer.of(byte));
    input.write(ping(2));
    await settle(() => replies().length >= 2);
    expect(replies().map((r) => r.id)).toEqual([1, 2]);
    await close();
  });
});
