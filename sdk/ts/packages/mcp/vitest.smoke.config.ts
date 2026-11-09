import { defineConfig } from "vitest/config";

// the smoke test spawns the built bin, so it runs after the build (the package's postbuild), never in the workspace's
// `npm test`, which runs from the sources
export default defineConfig({
  test: {
    include: ["test/**/*.smoke.ts"],
  },
});
