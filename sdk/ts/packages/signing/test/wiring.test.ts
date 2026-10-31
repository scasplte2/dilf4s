import { expect, test } from "vitest";

import { toHex } from "../src/index.js";

test("wiring", () => {
  expect(toHex(new Uint8Array([1]))).toBe("01");
});
