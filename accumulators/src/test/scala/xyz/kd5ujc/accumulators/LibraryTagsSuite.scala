package xyz.kd5ujc.accumulators

import xyz.kd5ujc.accumulators.merkle.MerkleHashing
import xyz.kd5ujc.accumulators.trie.TrieHashing
import xyz.kd5ujc.core.hash.DomainTag

import weaver.SimpleIOSuite

/** Library tags skip validation at construction (DomainTag.literal); this proves each one is valid and distinct. */
object LibraryTagsSuite extends SimpleIOSuite {

  private val all = MerkleHashing.Tags ++ TrieHashing.Tags

  pureTest("every library domain tag satisfies the tag grammar") {
    all.map(t => expect(DomainTag(t).isRight, s"$t must be a valid domain tag")).reduce(_ and _)
  }

  pureTest("library domain tags are pairwise distinct") {
    expect.same(all.size, all.distinct.size)
  }
}
