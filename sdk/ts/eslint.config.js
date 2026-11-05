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
        // stays banned: errors are never swallowed or turned into values, outside signing's one attempt.ts boundary.
        { selector: "TryStatement[handler]", message: "Library code returns a Result; it has nothing to catch (try/finally is allowed only to release resources)." },
      ],
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
