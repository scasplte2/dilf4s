/**
 * The stdio transport: one JSON-RPC message per line, as the SDK's, but owning the framing, so that no request goes
 * unanswered. The SDK's transport validates each message's envelope before anything else sees it and, when that fails,
 * only reports the error: a request with, say, `params._meta: null` would never be answered. Here a line that parses as
 * JSON but is not a valid message is passed on, when it carries a request id, as an {@link INVALID_REQUEST} request,
 * which admission answers with a short error in its turn (keeping the output's backpressure); without an id there is no
 * one to answer, and it is reported as the SDK's are. A line longer than its cap cannot be answered (its id is past
 * the cap), so it closes the transport, releasing the input: the client then rejects every call it waits on, never
 * left waiting for a reply that will not come.
 */
import type { Transport } from "@modelcontextprotocol/sdk/shared/transport.js";
import { JSONRPCMessageSchema, type JSONRPCMessage } from "@modelcontextprotocol/sdk/types.js";

import { INVALID_REQUEST } from "./admission.js";

/** A line as JSON, or `undefined` when it is not JSON: `JSON.parse` has no form that does not throw. */
function parsed(line: string): { readonly value: unknown } | undefined {
  try {
    return { value: JSON.parse(line) as unknown };
  } catch {
    return undefined;
  }
}

/** What the transport reads: a readable stream's events (Node's `Readable` is one). */
export interface Input {
  on(event: "data", listener: (chunk: Uint8Array) => void): unknown;
  on(event: "error", listener: (e: Error) => void): unknown;
  on(event: "end", listener: () => void): unknown;
  off(event: "data", listener: (chunk: Uint8Array) => void): unknown;
  off(event: "error", listener: (e: Error) => void): unknown;
  off(event: "end", listener: () => void): unknown;
  destroy(): unknown;
}

/** What the transport writes: a writable stream (Node's `Writable` is one). */
export interface Output {
  write(text: string): boolean;
  once(event: "drain", listener: () => void): unknown;
}

/**
 * The request id of a value that is not a valid message, if it has one a reply can name: any object with a string or
 * safe-integer id that names a method (a request, whatever else it holds) or, naming none, is not a response (it has
 * neither `result` nor `error`).
 */
function requestId(value: unknown): string | number | undefined {
  if (typeof value !== "object" || value === null || Array.isArray(value)) return undefined;
  const response = Object.hasOwn(value, "result") || Object.hasOwn(value, "error");
  if (response && !Object.hasOwn(value, "method")) return undefined;
  const id: unknown = (value as Record<string, unknown>)["id"];
  return typeof id === "string" || (typeof id === "number" && Number.isSafeInteger(id)) ? id : undefined;
}

/** A transport over `input` and `output`, reading lines of at most `maxLineBytes`. */
export function stdioTransport(input: Input, output: Output, maxLineBytes: number): Transport {
  // the line read so far, in one buffer grown by doubling: what it holds depends on the bytes, never on how many
  // pieces they came in (a client sending one byte at a time holds no more than one sending the line at once)
  let line = new Uint8Array(0);
  let held = 0;
  let closed = false;
  const transport: Transport = {
    start: () => {
      input.on("data", onData);
      input.on("error", onError);
      input.on("end", onEnd);
      return Promise.resolve();
    },
    send: (message: JSONRPCMessage) =>
      new Promise<void>((resolve) => {
        if (output.write(`${JSON.stringify(message)}\n`)) resolve();
        else output.once("drain", resolve);
      }),
    close: () => {
      if (closed) return Promise.resolve();
      closed = true;
      input.off("data", onData);
      input.off("error", onError);
      input.off("end", onEnd);
      input.destroy();
      line = new Uint8Array(0);
      held = 0;
      transport.onclose?.();
      return Promise.resolve();
    },
  };
  function onError(e: Error): void {
    transport.onerror?.(e);
  }
  function onEnd(): void {
    void transport.close();
  }
  /** Appends `part` to the line, growing its buffer by doubling, up to just past the cap. */
  function append(part: Uint8Array): void {
    if (held + part.length > line.length) {
      const grown = new Uint8Array(Math.min(Math.max(2 * line.length, held + part.length, 1024), maxLineBytes + 1));
      grown.set(line.subarray(0, held));
      line = grown;
    }
    line.set(part, held);
    held += part.length;
  }
  function onData(chunk: Uint8Array): void {
    for (let rest = chunk; rest.length > 0 && !closed; ) {
      const end = rest.indexOf(0x0a);
      const part = end < 0 ? rest : rest.subarray(0, end);
      rest = end < 0 ? rest.subarray(rest.length) : rest.subarray(end + 1);
      if (held + part.length > maxLineBytes) {
        transport.onerror?.(new Error(`a line longer than ${maxLineBytes.toString()} bytes: the transport closes`));
        void transport.close();
        return;
      }
      append(part);
      if (end >= 0) {
        message(decoder.decode(line.subarray(0, held)).replace(/\r$/, ""));
        line = new Uint8Array(0);
        held = 0;
      }
    }
  }
  function message(text: string): void {
    if (text.length === 0) return;
    const json = parsed(text);
    if (json === undefined) {
      transport.onerror?.(new Error("a line that is not JSON"));
      return;
    }
    const message = JSONRPCMessageSchema.safeParse(json.value);
    if (message.success) {
      transport.onmessage?.(message.data);
      return;
    }
    const id = requestId(json.value);
    if (id === undefined) transport.onerror?.(new Error("a message that is not valid JSON-RPC"));
    else transport.onmessage?.({ jsonrpc: "2.0", id, method: INVALID_REQUEST });
  }
  return transport;
}

const decoder = new TextDecoder();
