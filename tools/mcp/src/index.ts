/**
 * The dilf4s JLVM as a Model Context Protocol server (`@kd5ujc/dilf4s-mcp`): any MCP-capable agent can check,
 * evaluate and look up JLVM programs (docs/specs/jlvm.md) through its tools, run by the TypeScript JLVM, which gives
 * the conformance vectors' outcome, gas and warnings on every case.
 *
 * - {@link createServer} makes a server to connect to a transport; the package's `bin` serves it over stdio.
 * - {@link callTool} and the tools themselves are pure functions of their arguments: bounded, deterministic, and with
 *   no access to files, the network, a clock or the environment. Every refusal is a structured result.
 */
export { admitted, type Flow } from "./admission.js";
export { CATALOGUE, type Cost, type Entry, type Example, type Kind, type OpClass, type Param, type Signature } from "./catalogue.js";
export { DEFAULT_GAS_CAP, GAS_CAP_CEILING, MAX_BINDINGS, MAX_BINDINGS_BYTES, MAX_OUTPUT_BYTES, MAX_TEXT_BYTES } from "./limits.js";
export { EXPONENT_CEILING, measure } from "./measure.js";
export { TOOLS, type ToolDescriptor } from "./schemas.js";
export { createServer, SERVER_INFO, toCallToolResult, type JlvmServer, type ServerOptions } from "./server.js";
export { callTool, check, evaluateCall, ops, TOOL_NAMES, type Refusal, type RefusalKind, type Refused, type ToolName, type ToolResult, type Where } from "./tools.js";
