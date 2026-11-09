/**
 * Backpressure for a stream transport: requests are admitted one at a time, in the order they arrive, the next only
 * once the last one's reply has been written out (the SDK's stdio transport resolves a send when the output has
 * drained). While requests wait, the input is paused, so no more are read. A client that stops reading the output
 * therefore holds the server at one reply in the output's buffer, and the requests of the one input chunk already
 * read; without this, the SDK reads and answers every request it is sent, queueing the replies in memory.
 *
 * Notifications keep their place in that order, and are passed on without waiting for a reply; but a cancellation is
 * dropped. Every tool answers at once, so there is nothing to cancel, and the SDK, which handles notifications
 * asynchronously, would otherwise abort a request admitted after the cancellation arrived and suppress its reply,
 * leaving the next request waiting forever. A reply to a cancelled request is allowed (its sender ignores it). Only a
 * notification is a cancellation: a request (it has an id) of that method is answered as any other.
 *
 * A message holding more than {@link MAX_MESSAGE_VALUES} JSON values never reaches the SDK: its validation reports every
 * issue it finds, so a short message of many wrong values (an `initialize` with 25,000 nulls) would make a reply of
 * megabytes, or a string past the engine's limit. Such a request is answered with a short error, in its turn; such a
 * notification is dropped. No tool's arguments come near the bound (each text argument is one value).
 */
import type { Transport, TransportSendOptions } from "@modelcontextprotocol/sdk/shared/transport.js";
import type { JSONRPCMessage, MessageExtraInfo, RequestId } from "@modelcontextprotocol/sdk/types.js";

/** The input's flow control: a readable stream's. */
export interface Flow {
  pause(): unknown;
  resume(): unknown;
}

const isRequest = (m: JSONRPCMessage): boolean => "method" in m && "id" in m;
const isReply = (m: JSONRPCMessage): boolean => "id" in m && ("result" in m || "error" in m);
const isCancellation = (m: JSONRPCMessage): boolean => "method" in m && !("id" in m) && m.method === "notifications/cancelled";

/** The most JSON values (each object, array, member and element) a message may hold for the SDK to validate it. */
export const MAX_MESSAGE_VALUES = 4096;

/** Whether `m` holds at most {@link MAX_MESSAGE_VALUES} values: counted without recursion, stopping at the bound. */
export function withinBound(m: unknown): boolean {
  const pending: unknown[] = [m];
  let seen = 0;
  for (let v = pending.pop(); seen <= MAX_MESSAGE_VALUES; v = pending.pop()) {
    seen += 1;
    if (Array.isArray(v)) {
      // what waits is counted with what was seen: everything pushed is counted in the end, so the verdict is the same,
      // and what waits never exceeds the bound (nested arrays cannot fill it past the bound before the refusal)
      if (v.length > MAX_MESSAGE_VALUES - seen - pending.length) return false;
      pending.push(...(v as unknown[]));
    } else if (typeof v === "object" && v !== null) {
      for (const key in v) {
        if (!Object.hasOwn(v, key)) continue;
        if (pending.length >= MAX_MESSAGE_VALUES - seen) return false;
        pending.push((v as Record<string, unknown>)[key]);
      }
    }
    if (pending.length === 0) return true;
  }
  return false;
}

/** The method the stdio transport gives a request that is not a valid message, so that it is answered in its turn. */
export const INVALID_REQUEST = "dilf4s/invalid-request";

/** The short `Invalid Request` reply to a request too large to validate, or not a valid message. */
const invalid = (id: RequestId, message: string): JSONRPCMessage => ({ jsonrpc: "2.0", id, error: { code: -32600, message } });
const TOO_LARGE = `the request holds more than ${MAX_MESSAGE_VALUES.toString()} JSON values`;
const NOT_VALID = "the request is not a valid JSON-RPC message";

/** `inner`, admitting one request at a time and pausing `input` while any waits. */
export function admitted(inner: Transport, input: Flow): Transport {
  const waiting: (readonly [JSONRPCMessage, MessageExtraInfo | undefined])[] = [];
  let answering = false;
  const outer: Transport = {
    start: () => {
      inner.onmessage = (m, extra) => {
        if (isCancellation(m)) return;
        if (!withinBound(m) && !isRequest(m)) return;
        waiting.push([m, extra]);
        pump();
      };
      inner.onerror = (e) => outer.onerror?.(e);
      inner.onclose = () => outer.onclose?.();
      return inner.start();
    },
    send: (message: JSONRPCMessage, options?: TransportSendOptions) => {
      const reply = isReply(message);
      const settled = (): void => {
        if (!reply) return;
        answering = false;
        pump();
      };
      return inner.send(message, options).then(settled, (e: unknown) => {
        settled();
        return Promise.reject(e instanceof Error ? e : new Error(String(e)));
      });
    },
    close: () => inner.close(),
  };
  /** Passes on what may go: notifications at once, a request only when no other is being answered. */
  function pump(): void {
    for (let next = waiting[0]; next !== undefined && !answering; next = waiting[0]) {
      waiting.shift();
      const [message, extra] = next;
      answering = isRequest(message);
      const id = "id" in message ? message.id : undefined;
      const refusal = "method" in message && message.method === INVALID_REQUEST ? NOT_VALID : withinBound(message) ? undefined : TOO_LARGE;
      if (answering && id !== undefined && refusal !== undefined) void outer.send(invalid(id, refusal)).catch(() => undefined);
      else outer.onmessage?.(message, extra);
    }
    if (waiting.length > 0) input.pause();
    else input.resume();
  }
  return outer;
}
