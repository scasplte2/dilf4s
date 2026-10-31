import { defineConfig } from "vitest/config";

export default defineConfig({
  // the workspace packages import each other's sources (the "source" export condition), so tests need no build
  resolve: { conditions: ["source"] },
  ssr: { resolve: { conditions: ["source"] } },
  test: {
    include: ["packages/*/test/**/*.test.ts"],
  },
});
