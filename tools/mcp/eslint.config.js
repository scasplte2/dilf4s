import shared from "../../lang/ts/eslint.config.js";

export default [
  ...shared,
  {
    files: ["src/stdio.ts"],
    rules: { "no-restricted-syntax": ["error", { selector: "ThrowStatement", message: "Library code returns a Result; it never throws." }] },
  },
];
