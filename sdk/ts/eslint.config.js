// @ts-check
import js from "@eslint/js";
import tseslint from "typescript-eslint";

export default tseslint.config(
  { ignores: ["**/dist/**", "**/node_modules/**"] },
  js.configs.recommended,
  ...tseslint.configs.strictTypeChecked,
  ...tseslint.configs.stylisticTypeChecked,
  {
    languageOptions: {
      parserOptions: {
        projectService: { allowDefaultProject: ["eslint.config.js", "vitest.config.ts"] },
        tsconfigRootDir: import.meta.dirname,
      },
    },
    rules: {
      "@typescript-eslint/no-explicit-any": "error",
      "@typescript-eslint/explicit-module-boundary-types": "error",
      "@typescript-eslint/consistent-type-imports": "error",
      "@typescript-eslint/switch-exhaustiveness-check": "error",
      "@typescript-eslint/prefer-readonly": "error",
      "no-restricted-syntax": [
        "error",
        { selector: "ThrowStatement", message: "Library code returns a Result; it never throws." },
        // try/finally (no catch) is allowed, only to close iterators or release resources when caller code (a getter,
        // an iterator, a callback, a hand-made codec) throws mid-walk, as for...of closes its iterator. A catch clause
        // stays banned: errors are never swallowed or turned into values, outside the two sanctioned files below.
        { selector: "TryStatement[handler]", message: "Library code returns a Result; it has nothing to catch (try/finally is allowed only to release resources)." },
      ],
    },
  },
  {
    // the three sanctioned catches, and only these files: signing's boundary around noble (which throws on adversarial
    // bytes), core's guard around Array.isArray (which throws on a revoked Proxy), and the MCP server's stdio framing
    // around JSON.parse (which has no form that does not throw). A catch anywhere else fails lint.
    files: ["packages/signing/src/internal/attempt.ts", "packages/core/src/guard.ts", "packages/mcp/src/stdio.ts"],
    rules: {
      "no-restricted-syntax": ["error", { selector: "ThrowStatement", message: "Library code returns a Result; it never throws." }],
    },
  },
  {
    files: ["**/test/**"],
    rules: { "no-restricted-syntax": "off" },
  },
  {
    files: ["eslint.config.js"],
    extends: [tseslint.configs.disableTypeChecked],
  },
);
