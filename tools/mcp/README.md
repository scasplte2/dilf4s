# @kd5ujc/dilf4s-mcp

The dilf4s JLVM ([`docs/specs/jlvm.md`](../../docs/specs/jlvm.md)) as a [Model Context Protocol](https://modelcontextprotocol.io)
server, so that any MCP-capable agent can write, check and run JLVM contracts, whatever language it runs in. It serves
three tools over stdio, run by the TypeScript JLVM (`@kd5ujc/dilf4s-jlvm`) on the official SDK
(`@modelcontextprotocol/sdk` 1.32.2, MIT).

Programs, data and binding values are JSON surface text (`jlvm.md` section 1): canonical JSON, in which a string `"0x"`
followed by lowercase hex is bytes.

## Tools

### `jlvm_check`

Parses and compiles a program. Returns its identity (`taggedHash_SHA-256("dilf4s/jlvm/program/v1", cbor(program))`),
its canonical form as hex, and the canonical form's weight and depth, or the refusal.

```json
{"program": "{\"+\": [1, {\"var\": \"x\"}]}"}
```

gives `{"ok": true, "id": "…", "canonical": "a2…", "weight": 33, "depth": 4}`. Text that is not JSON gives
`{"ok": false, "refusal": {"kind": "Malformed", "message": "…", "where": {"argument": "program", "offset": 10}}}`.

### `jlvm_evaluate`

Runs a program on data, with host bindings (read as `{"var": "$name"}`) and a gas limit (2^24 unless given).

```json
{"program": "{\"*\": [{\"var\": \"$rate\"}, {\"var\": \"amount\"}]}", "data": "{\"amount\": 40}", "bindings": {"rate": "0.5"}, "gasLimit": 100000}
```

gives:

- the outcome, as the conformance vectors record it: `{"value": {"cbor": "14", "json": "20", "weight": 1}}`, or
  `{"error": {"error": "Type", "op": "abs", "message": "…"}}`, with a `Crypto` error's `cause` as `{family: kind}`.
  A value with no surface JSON (it holds `0x`-hex text, or a non-terminating rational) says so in `omitted`, and so
  does one past the output bound;
- `gasUsed`, `gasLimit` and the `warnings`;
- the call's `statement` (`docs/specs/zk-jlvm.md` section 2) and its canonical CBOR, `statementCbor`. The statement
  holds the program's identity (`program`), the input commitment (`input`), the bindings commitment (`bindings`),
  and the outcome or its digest, so an agent can cite exactly what it ran. A proved call's public values are this
  CBOR.

A program, data or binding that does not parse is refused before anything runs, naming the argument, the binding and
the offset of a JSON error.

### `jlvm_ops`

The closed list of operations, or the one named (`{"op": "merkle_verify"}`; an alias names its operation). Each entry
gives its summary, aliases, class (`form`, `strict` or `crypto`), arity, argument kinds (`typeof`'s names, `integer`
and `any`), an example program and data that run, and its gas cost from schedule v1. The result also gives the
schedule's scalar charges, the default limit and the server's gas cap.

## Adding it to a client

Build the workspace once (`npm ci && npm run build:ts` at the repository root), then register the bin.

Claude Code:

```sh
claude mcp add jlvm -- node /path/to/dilf4s/tools/mcp/dist/bin.js
```

Any client configured with JSON (`mcpServers`):

```json
{
  "mcpServers": {
    "jlvm": { "command": "node", "args": ["/path/to/dilf4s/tools/mcp/dist/bin.js"] }
  }
}
```

A host embedding the server calls `createServer({ gasCap })` and connects it to any MCP transport.

## What it guarantees

- **The nodes' outcomes.** The TS JLVM gives the value, error, gas and warnings of `shared/vectors/jlvm_v1.json` on
  every case, as the Scala and Rust nodes do. This package's tests run a deterministic sample of the vectors (every
  seventh case a text can carry) through `jlvm_evaluate` from an MCP client, and every call of
  `shared/vectors/zk_jlvm_v1.json`, whose statements and public values it reproduces. The 20 cases whose bindings are
  built with shared parts are heavier than any text the server accepts.
- **Bounded.** Each text (a program, data, a binding) measures at most 1 MiB, the bindings 1 MiB together (names
  included) and at most 256 of them, all refused by their measure before anything is parsed. A text's measure is its
  UTF-8 length plus each number's exponent: reading a number converts it exactly, so `8e9999` costs what its 10,000
  digits would, not its six characters. The gas limit is at most the server's cap: 2^24 (the spec's default limit)
  unless a host raises it, to at most 2^32, never the unmetered maximum; a limit above it is refused, never clamped.
  Each rendering in a result (a value's CBOR, its JSON, a program's canonical form) is at most 1 MiB, and a longer one
  is omitted, its digest still given. Over stdio, requests are answered one at a time, in order, and input is not
  read while a reply waits for a client that has stopped reading, so the output holds at most one reply. A request line
  over about 19 MiB closes the transport.
- **Pure.** No tool reads a file, the network, a clock or the environment (a source scan checks it); the same
  arguments give the same result.
- **Refusals are results.** Every refusal is a structured result, `{"ok": false, "refusal": {"kind", "message",
  "where"}}`, matching the tool's output schema. Only the caller's misuse of the protocol (`InvalidArguments`: a field
  missing, of the wrong type, or unknown; `UnknownTool`) sets MCP's `isError`. Nothing throws.

## Tests

`test/`: the tools through the SDK's in-memory transport (`server.test.ts`), the vectors through the tool
(`conformance.test.ts`), the catalogue against the engine (`ops.test.ts`: the op list, the costs, every example, every
arity), the measure (`measure.test.ts`), backpressure over the stdio transport (`admission.test.ts`), and the source
scan (`ambient.test.ts`). `stdio.smoke.ts` spawns the built
bin; it runs after `npm run build`.
