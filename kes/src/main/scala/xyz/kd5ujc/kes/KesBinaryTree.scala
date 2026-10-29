package xyz.kd5ujc.kes

/**
 * ADT for KES binary tree structure.
 *
 * The tree represents a binary Merkle structure where:
 * - MerkleNode: Internal node with children and witness hashes
 * - SigningLeaf: Leaf node containing Ed25519 keypair
 * - Empty: Placeholder for pruned/unused subtrees
 *
 * Arrays are intentionally mutable to support forward-secure key erasure.
 */
sealed trait KesBinaryTree

object KesBinaryTree {
  val nodeTypePrefix: Byte = 0
  val leafTypePrefix: Byte = 1
  val emptyTypePrefix: Byte = 2

  /**
   * Internal Merkle node.
   *
   * @param seed         Seed for regenerating right subtree (erased after use)
   * @param witnessLeft  Hash of left child
   * @param witnessRight Hash of right child
   * @param left         Left subtree
   * @param right        Right subtree
   */
  final case class MerkleNode(
    seed:         Array[Byte],
    witnessLeft:  Array[Byte],
    witnessRight: Array[Byte],
    left:         KesBinaryTree,
    right:        KesBinaryTree
  ) extends KesBinaryTree

  /**
   * Signing leaf containing Ed25519 keypair.
   *
   * @param sk Secret key (32-byte seed, erased after use)
   * @param vk Verification key (32-byte public key)
   */
  final case class SigningLeaf(
    sk: Array[Byte],
    vk: Array[Byte]
  ) extends KesBinaryTree

  /**
   * Empty placeholder for pruned subtrees.
   */
  final case class Empty() extends KesBinaryTree
}
