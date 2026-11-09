/**
 * The MCP server: the tools of `tools.ts` behind the SDK's protocol layer. It holds no state but its gas cap, and its
 * handlers are the pure tool functions, so a call's result depends on its arguments alone.
 *
 * It is built on the SDK's low-level `Server`, which the SDK keeps for advanced use, rather than `McpServer`: the
 * tools read and refuse their own arguments as structured results, where `McpServer` validates them first and refuses
 * with unstructured text. A handler never throws; if the SDK's own code did, its protocol layer answers that request
 * with a JSON-RPC error, and the server goes on.
 */
import { Server } from "@modelcontextprotocol/sdk/server/index.js";
import type { Transport } from "@modelcontextprotocol/sdk/shared/transport.js";
import { CallToolRequestSchema, ListToolsRequestSchema, type CallToolResult } from "@modelcontextprotocol/sdk/types.js";

import { DEFAULT_GAS_CAP, GAS_CAP_CEILING } from "./limits.js";
import { TOOLS } from "./schemas.js";
import { callTool, type ToolResult } from "./tools.js";

/** The server's name and version, as `initialize` reports them. */
export const SERVER_INFO = { name: "dilf4s-jlvm", version: "0.1.0" } as const;

const INSTRUCTIONS =
  "Tools for the dilf4s JLVM, a strict, deterministic, gas-metered JSON expression language for ledger contracts (language 1, gas schedule 1). Call jlvm_ops for the operations and their argument kinds, jlvm_check to parse and compile a program, and jlvm_evaluate to run it on data and host bindings within a gas limit. Programs and values are JSON surface text: \"0x\" + lowercase hex strings are bytes. Results are deterministic and bounded.";

/** How a server is made. */
export interface ServerOptions {
  /** The highest gas limit a call may ask for: a whole number from 0 to {@link GAS_CAP_CEILING}; {@link DEFAULT_GAS_CAP} unless given. */
  readonly gasCap?: number;
}

/** A JLVM MCP server, ready to connect to one transport. */
export interface JlvmServer {
  /** Its gas cap. */
  readonly gasCap: number;
  /** Starts serving over `transport`. */
  connect(transport: Transport): Promise<void>;
  /** Stops serving. */
  close(): Promise<void>;
}

/** A tool result as MCP's `CallToolResult`: the structured content, and the same as JSON text, as the spec asks. */
export const toCallToolResult = (r: ToolResult): CallToolResult => ({
  content: [{ type: "text", text: JSON.stringify(r.structured) }],
  structuredContent: r.structured,
  ...(r.isError ? { isError: true } : {}),
});

/** A server with these options; a gas cap that is not a whole number from 0 to {@link GAS_CAP_CEILING} is refused. */
export function createServer(options: ServerOptions = {}): { readonly ok: true; readonly value: JlvmServer } | { readonly ok: false; readonly error: string } {
  const cap: unknown = options.gasCap ?? DEFAULT_GAS_CAP;
  if (typeof cap !== "number" || !Number.isSafeInteger(cap) || cap < 0 || cap > GAS_CAP_CEILING) return { ok: false, error: `the gas cap must be a whole number from 0 to ${GAS_CAP_CEILING.toString()}` };
  // eslint-disable-next-line @typescript-eslint/no-deprecated -- the low-level Server: see the module's header
  const server = new Server(SERVER_INFO, { capabilities: { tools: {} }, instructions: INSTRUCTIONS });
  server.setRequestHandler(ListToolsRequestSchema, () => ({ tools: TOOLS.map((t) => ({ ...t })) }));
  server.setRequestHandler(CallToolRequestSchema, (request) => toCallToolResult(callTool(request.params.name, request.params.arguments, cap)));
  return {
    ok: true,
    value: Object.freeze({
      gasCap: cap,
      connect: (transport: Transport) => server.connect(transport),
      close: () => server.close(),
    }),
  };
}
