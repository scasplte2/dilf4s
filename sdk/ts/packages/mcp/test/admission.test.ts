/**
 * Backpressure over the SDK's stdio transport, by counts: a client that stops reading holds the server at one reply in
 * the output and its input paused, however many requests it sends; once it reads again, every request is answered, in
 * order, a cancellation included.
 */
import { PassThrough, Writable } from "node:stream";

import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { describe, expect, it } from "vitest";

import { admitted, createServer } from "../src/index.js";
import { MAX_MESSAGE_VALUES, withinBound } from "../src/admission.js";

/** An output whose reader can stall: it holds each write until released. */
function stallable(): { readonly out: Writable; readonly lines: () => readonly string[]; readonly release: () => void } {
  let stalled = true;
  const held: (() => void)[] = [];
  const written: string[] = [];
  const out = new Writable({
    highWaterMark: 1024,
    write(chunk: Buffer, _encoding, done) {
      written.push(chunk.toString("utf8"));
      if (stalled)
        held.push(() => {
          done();
        });
      else done();
    },
  });
  const release = (): void => {
    stalled = false;
    for (let f = held.shift(); f !== undefined; f = held.shift()) f();
  };
  return { out, lines: () => written.join("").split("\n").filter((l) => l.length > 0), release };
}

const ticks = async (n: number): Promise<void> => {
  for (let i = 0; i < n; i++) await new Promise((r) => setImmediate(r));
};

const request = (id: number): string => `${JSON.stringify({ jsonrpc: "2.0", id, method: "tools/call", params: { name: "jlvm_ops", arguments: {} } })}\n`;

describe("admission", { timeout: 60_000 }, () => {
  it("holds a stalled client at one reply, its input paused, and answers every request once it reads", async () => {
    const input = new PassThrough();
    const { out, lines, release } = stallable();
    const made = createServer();
    if (!made.ok) throw new Error(made.error);
    await made.value.connect(admitted(new StdioServerTransport(input, out), input));
    const n = 300;
    input.write(Array.from({ length: n }, (_, i) => request(i + 1)).join(""));
    await ticks(50);
    expect(lines().length, "one reply written while the client is not reading").toBe(1);
    // the output holds that one reply, not a queue of them (the SDK alone buffers every reply it makes)
    expect(out.writableLength, "the bytes waiting in the output").toBeLessThanOrEqual(Buffer.byteLength(lines()[0] ?? "") + 1);
    expect(input.isPaused(), "the input is paused").toBe(true);
    release();
    for (let i = 0; i < 2000 && lines().length < n; i++) await ticks(1);
    expect(lines().map((l) => (JSON.parse(l) as { readonly id: number }).id)).toEqual(Array.from({ length: n }, (_, i) => i + 1));
    expect(input.isPaused(), "the input flows again").toBe(false);
    await made.value.close();
  });

  it("drops a cancellation, before or after the request it names, so every request is answered", async () => {
    const input = new PassThrough();
    const { out, lines, release } = stallable();
    release();
    const made = createServer();
    if (!made.ok) throw new Error(made.error);
    await made.value.connect(admitted(new StdioServerTransport(input, out), input));
    const cancel = `${JSON.stringify({ jsonrpc: "2.0", method: "notifications/cancelled", params: { requestId: 1 } })}\n`;
    // before: the SDK would handle it after admitting request 1, abort that and never reply (Codex's sequence)
    input.write(cancel + request(1) + request(2));
    for (let i = 0; i < 200 && lines().length < 2; i++) await ticks(1);
    input.write(request(3) + cancel.replace('"requestId":1', '"requestId":3') + request(4));
    for (let i = 0; i < 200 && lines().length < 4; i++) await ticks(1);
    expect(lines().map((l) => (JSON.parse(l) as { readonly id: number }).id)).toEqual([1, 2, 3, 4]);
    await made.value.close();
  });

  it("answers a cancellation-shaped request (it has an id) as any request, and goes on", async () => {
    const input = new PassThrough();
    const { out, lines, release } = stallable();
    release();
    const made = createServer();
    if (!made.ok) throw new Error(made.error);
    await made.value.connect(admitted(new StdioServerTransport(input, out), input));
    // Codex's request: a JSON-RPC request named like the notification must get MethodNotFound, not silence
    input.write(`${JSON.stringify({ jsonrpc: "2.0", id: 1, method: "notifications/cancelled", params: { requestId: 99 } })}\n` + request(2));
    for (let i = 0; i < 200 && lines().length < 2; i++) await ticks(1);
    const replies = lines().map((l) => JSON.parse(l) as { readonly id: number; readonly error?: { readonly code: number } });
    expect(replies.map((r) => r.id)).toEqual([1, 2]);
    expect(replies[0]?.error?.code, "MethodNotFound").toBe(-32601);
    await made.value.close();
  });

  it("answers a message of too many values with a short error, never the SDK's validation of each, and goes on", async () => {
    const input = new PassThrough();
    const { out, lines, release } = stallable();
    release();
    const made = createServer();
    if (!made.ok) throw new Error(made.error);
    await made.value.connect(admitted(new StdioServerTransport(input, out), input));
    // Codex's request: 25,000 nulls in clientInfo.icons made a 6 MB error reply from 125 KB
    const icons = Array.from({ length: 25_000 }, () => null);
    const initialize = { jsonrpc: "2.0", id: 1, method: "initialize", params: { protocolVersion: "2025-06-18", capabilities: {}, clientInfo: { name: "x", version: "1", icons } } };
    const noisy = { jsonrpc: "2.0", method: "notifications/initialized", params: { icons } };
    input.write(`${JSON.stringify(initialize)}\n${JSON.stringify(noisy)}\n` + request(2));
    for (let i = 0; i < 200 && lines().length < 2; i++) await ticks(1);
    expect(lines().length, "the notification is dropped, the two requests answered").toBe(2);
    const first = lines()[0] ?? "";
    expect(first.length, "a short reply").toBeLessThan(256);
    expect((JSON.parse(first) as { readonly error?: { readonly code: number } }).error?.code, "InvalidRequest").toBe(-32600);
    expect((JSON.parse(lines()[1] ?? "{}") as { readonly id: number }).id).toBe(2);
    await made.value.close();
  });

  it("counts values without recursion, stopping at the bound", () => {
    // the message's values: itself, then each member and element, however deep
    expect(withinBound(Array.from({ length: MAX_MESSAGE_VALUES - 1 }, () => 0)), "the array and its elements: the bound").toBe(true);
    expect(withinBound(Array.from({ length: MAX_MESSAGE_VALUES }, () => 0)), "one more").toBe(false);
    let deep: unknown = 0;
    for (let i = 0; i < MAX_MESSAGE_VALUES - 1; i++) deep = [deep];
    expect(withinBound(deep), "nested to the bound").toBe(true);
    expect(withinBound([deep]), "one level more").toBe(false);
    expect(withinBound({ a: { b: null }, c: [1, "x"] }), "a small message").toBe(true);
    const wide: Record<string, null> = {};
    for (let i = 0; i < MAX_MESSAGE_VALUES; i++) wide[`k${i.toString()}`] = null;
    expect(withinBound(wide), "an object of too many members").toBe(false);
    // nested arrays: 4000 arrays of 4000, each short of the bound alone, refused before more than the bound waits
    const row = Array.from({ length: 4000 }, () => 0);
    expect(withinBound(Array.from({ length: 4000 }, () => row)), "arrays that only together pass the bound").toBe(false);
  });
});