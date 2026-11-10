/**
 * Rendering is exact (jlvm.md section 1): parsing a program's rendered text gives the same program, for every program
 * of the conformance vectors; and what has no surface form is refused.
 */
import { models, Value } from "@kd5ujc/dilf4s-core";
import { describe, expect, it } from "vitest";

import { fromCanonical, parse, render } from "../src/index.js";
import { expectErr, expectOk, hex, loadVectors, readRepoFile } from "./vectors.js";

const vectors = loadVectors("jlvm_v1.json", models.jlvmVectors.JlvmVectors);

describe("rendering", { timeout: 120_000 }, () => {
  it("reads back as the same program, for every program of the vectors that parses", () => {
    expect(vectors.cases).toHaveLength(3363);
    expect(vectors.aliases).toHaveLength(4);
    const expected = readRepoFile("shared/vectors/jlvm_parse_refusals_v1.tsv").trimEnd().split("\n").map(line => line.split("\t"));
    expect(expected).toHaveLength(67);
    const parsed = vectors.cases.map(c => ({ name: c.name, result: parse(c.program) }));
    expect(parsed.flatMap(c => c.result.ok ? [] : [[c.name, c.result.error.kind]])).toEqual(expected);
    const accepted = parsed.flatMap(c => c.result.ok ? [{ name: c.name, program: c.result.value }] : []);
    accepted.push(...vectors.aliases.flatMap(a => [a.program, a.own].map(text => ({ name: a.alias, program: expectOk(parse(text), a.alias) }))));
    let rendered = 0;
    for (const { name, program } of accepted) {
      const surface = expectOk(render(program), `renders ${name}`);
      const back = expectOk(parse(surface), `re-parses ${surface.slice(0, 60)}`);
      expect(hex(expectOk(back.encode(), "encodes")), name).toBe(hex(expectOk(program.encode(), "encodes")));
      // and rendering is a fixed point
      expect(expectOk(render(back), "renders again")).toBe(surface);
      rendered++;
    }
    expect(rendered).toBe(3304);
  });

  it("writes single-key maps as object, quotes data headed by a call's name, and bytes as 0x text", () => {
    const cases: readonly (readonly [string, string])[] = [
      ['{"object": {"if": 1}}', '{"object":{"if":1}}'],
      ['{"_result": 1}', '{"_result":1}'],
      ['[{"literal": "+"}, 1]', '[{"literal":"+"},1]'],
      ['{"literal": [1, {"a": 2}]}', '{"literal":[1,{"a":2}]}'],
      ['"0x00ff"', '"0x00ff"'],
      ['{"var": ["a", {"literal": {"k": 1}}]}', '{"var":["a",{"literal":{"k":1}}]}'],
      ['{"var": {"cat": ["a", "b"]}}', '{"var":{"cat":["a","b"]}}'],
      ['{"var": [["a.b", 0]]}', '{"var":[["a.b",0]]}'],
      ['{"+": {"var": "xs"}}', '{"+":{"var":"xs"}}'],
      ['{"let": [{"b": 1, "a": 2}, {"var": "$a"}]}', '{"let":[{"a":2,"b":1},{"var":"$a"}]}'],
      ["{\"size\": [\"x\"]}", '{"length":["x"]}'],
      ['{"var": {"literal": null}}', '{"var":{"literal":null}}'],
      ['{"var": [{"literal": null}, 1]}', '{"var":[{"literal":null},1]}'],
      ['{"var": [{"literal": "a.b"}, 1]}', '{"var":[{"literal":"a.b"},1]}'],
    ];
    for (const [text, surface] of cases) expect(expectOk(render(expectOk(parse(text), text)), text), text).toBe(surface);
  });

  it("refuses a literal of 0x-hex text, or a non-terminating rational, which have no surface form", () => {
    const form = (lit: Value): Value => expectOk(models.jlvm.JlvmProgram.encode({ jlvm: 1n, expr: { lit } }), "a form");
    const hexText = expectOk(fromCanonical(form(expectOk(Value.text("0xab"), "text"))), "a program of 0x text");
    expect(expectErr(render(hexText), "0x text")).toMatchObject({ kind: "Malformed" });
    const third = expectOk(fromCanonical(form(expectOk(Value.number(1n, 3n), "1/3"))), "a program of 1/3");
    expect(expectErr(render(third), "1/3")).toMatchObject({ kind: "Malformed" });
    expect(expectErr(render({} as never), "not a program")).toMatchObject({ kind: "TypeMismatch" });
  });
});
