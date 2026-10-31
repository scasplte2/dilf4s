import { describe, expect, it } from "vitest";

import { DomainTag } from "@kd5ujc/dilf4s-core";

import { merkle, trie } from "../src/index.js";

describe("the library's domain tags", () => {
  it.each([merkle.LEAF_TAG, merkle.NODE_TAG, merkle.EMPTY_TAG, trie.VALUE_TAG, trie.LEAF_TAG, trie.BRANCH_TAG, trie.EMPTY_TAG])("%s matches the DomainTag grammar", (tag) => {
    expect(DomainTag.of(tag).ok).toBe(true);
  });
});
