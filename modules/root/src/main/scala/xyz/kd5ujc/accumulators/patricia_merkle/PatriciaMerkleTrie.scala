package xyz.kd5ujc.accumulators.patricia_merkle

import xyz.kd5ujc.accumulators.Node
import xyz.kd5ujc.hash.Digest

final case class PatriciaMerkleTrie[L](
  rootNode:        Node[L],
  leafDigestIndex: Map[Digest[L], Int]
)

object PatriciaMerkleTrie {
  val leafPrefix: Array[Byte] = Array(0: Byte)
  val branchPrefix: Array[Byte] = Array(1: Byte)
  val extensionPrefix: Array[Byte] = Array(2: Byte)
}
