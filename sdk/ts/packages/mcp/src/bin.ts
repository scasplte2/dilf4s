#!/usr/bin/env node
/**
 * `dilf4s-jlvm-mcp`: the JLVM MCP server over stdio, at the default gas cap. The edge, and the only module that touches
 * the process (Node's global `process`, so the package imports no module outside its dependencies): it reads requests
 * from stdin and writes responses to stdout, one JSON-RPC message per line.
 */
import { admitted } from "./admission.js";
import { MAX_BINDINGS_BYTES, MAX_TEXT_BYTES } from "./limits.js";
import { createServer } from "./server.js";
import { stdioTransport } from "./stdio.js";

/**
 * The longest request line read: room for a call's largest texts (a program, data and the bindings, each at their
 * cap) with every character escaped six times over (`\u0000`), and the envelope. A longer line closes the transport.
 */
export const MAX_LINE_BYTES = 6 * (2 * MAX_TEXT_BYTES + MAX_BINDINGS_BYTES) + 2 ** 20;

const made = createServer();
if (made.ok) {
  // one request at a time, the input paused while its reply waits for a client that is not reading (admission.ts)
  made.value.connect(admitted(stdioTransport(process.stdin, process.stdout, MAX_LINE_BYTES), process.stdin)).then(
    () => undefined,
    (e: unknown) => {
      process.stderr.write(`dilf4s-jlvm-mcp: ${String(e)}\n`);
      process.exitCode = 1;
    },
  );
}
