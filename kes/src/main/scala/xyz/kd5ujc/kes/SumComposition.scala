package xyz.kd5ujc.kes

import java.security.SecureRandom

import scala.annotation.tailrec

import xyz.kd5ujc.kes.KesBinaryTree._

/**
 * KES Sum Composition using binary Merkle trees.
 *
 * Implementation of the MMM construction:
 * Malkin, T., Micciancio, D. and Miner, S. (2002) 'Efficient generic
 * forward-secure signatures with an unbounded number of time periods',
 * Advances in Cryptology Eurocrypt '02, LNCS 2332, Springer, pp.400–417.
 *
 * Provides forward-secure signatures: old keys are erased after time-step
 * evolution so past signatures can't be forged even if the current key leaks.
 *
 * Tree height h yields 2^h time steps.
 */
class SumComposition extends KesEd25519Blake2b256 {

  private val random = new SecureRandom()

  /**
   * Get the current time step of a sum composition key.
   */
  private[kes] def getKeyTime(keyTree: KesBinaryTree): Int =
    keyTree match {
      case MerkleNode(_, _, _, Empty(), _: SigningLeaf)    => 1
      case MerkleNode(_, _, _, Empty(), right: MerkleNode) => getKeyTime(right) + exp(getTreeHeight(right))
      case MerkleNode(_, _, _, left, Empty())              => getKeyTime(left)
      case _                                               => 0
    }

  /**
   * Generate the verification key from a secret key tree.
   */
  private[kes] def generateVerificationKey(keyTree: KesBinaryTree): (Array[Byte], Int) =
    keyTree match {
      case node: MerkleNode  => (witness(node), getKeyTime(keyTree))
      case leaf: SigningLeaf => (witness(leaf), 0)
      case Empty()           => (Array.fill(hashBytes)(0: Byte), 0)
    }

  /**
   * Generate a secret key tree from a seed.
   *
   * @param seed   32-byte input entropy
   * @param height tree height (yields 2^height time steps)
   * @return secret key tree at time step 0
   */
  private[kes] def generateSecretKey(seed: Array[Byte], height: Int): KesBinaryTree = {
    // Generate the full binary tree
    def seedTree(seed: Array[Byte], height: Int): KesBinaryTree =
      if (height == 0) {
        SigningLeaf.tupled(sGenKeypair(seed))
      } else {
        val r = prng(seed)
        val left = seedTree(r._1, height - 1)
        val right = seedTree(r._2, height - 1)
        MerkleNode(r._2, witness(left), witness(right), left, right)
      }

    // Reduce to leftmost branch, erasing unused subtrees
    def reduceTree(fullTree: KesBinaryTree): KesBinaryTree =
      fullTree match {
        case MerkleNode(seed, witL, witR, nodeL, nodeR) =>
          eraseOldNode(nodeR)
          MerkleNode(seed, witL, witR, reduceTree(nodeL), Empty())
        case leaf: SigningLeaf => leaf
        case _                 => Empty()
      }

    val out = reduceTree(seedTree(seed, height))
    random.nextBytes(seed)
    out
  }

  /**
   * Update the key to a new time step.
   *
   * @param keyTree current secret key tree
   * @param step    target time step
   * @return updated key tree
   */
  private[kes] def updateKey(keyTree: KesBinaryTree, step: Int): KesBinaryTree = {
    val totalSteps = exp(getTreeHeight(keyTree))
    val keyTime = getKeyTime(keyTree)
    if (step == 0) keyTree
    else if (step < totalSteps && keyTime < step) {
      evolveKey(keyTree, step)
    } else {
      throw new Error(
        s"Update error - Max steps: $totalSteps, current step: $keyTime, requested increase: $step"
      )
    }
  }

  /**
   * Securely erase a subtree by overwriting all key material.
   */
  private[kes] def eraseOldNode(node: KesBinaryTree): Unit =
    node match {
      case merkleNode: MerkleNode =>
        random.nextBytes(merkleNode.seed)
        random.nextBytes(merkleNode.witnessLeft)
        random.nextBytes(merkleNode.witnessRight)
        merkleNode.left match {
          case l: MerkleNode =>
            eraseOldNode(l)
          case l: SigningLeaf =>
            random.nextBytes(l.sk)
            random.nextBytes(l.vk)
          case _ =>
        }
        merkleNode.right match {
          case r: MerkleNode =>
            eraseOldNode(r)
          case r: SigningLeaf =>
            random.nextBytes(r.sk)
            random.nextBytes(r.vk)
          case _ =>
        }
      case leaf: SigningLeaf =>
        random.nextBytes(leaf.sk)
        random.nextBytes(leaf.vk)
      case _ =>
    }

  /**
   * Evolve the key to a target time step.
   */
  private[kes] def evolveKey(input: KesBinaryTree, step: Int): KesBinaryTree = {
    val halfTotalSteps = exp(getTreeHeight(input) - 1)
    val shiftStep: Int => Int = (step: Int) => step % halfTotalSteps

    if (step >= halfTotalSteps) {
      input match {
        case MerkleNode(seed, witL, witR, oldLeaf: SigningLeaf, Empty()) =>
          val newNode =
            MerkleNode(Array.fill(seed.length)(0: Byte), witL, witR, Empty(), SigningLeaf.tupled(sGenKeypair(seed)))
          eraseOldNode(oldLeaf)
          random.nextBytes(seed)
          newNode

        case MerkleNode(seed, witL, witR, oldNode: MerkleNode, Empty()) =>
          val newNode = MerkleNode(
            Array.fill(seed.length)(0: Byte),
            witL,
            witR,
            Empty(),
            evolveKey(generateSecretKey(seed, getTreeHeight(input) - 1), shiftStep(step))
          )
          eraseOldNode(oldNode)
          random.nextBytes(seed)
          newNode

        case MerkleNode(seed, witL, witR, Empty(), right) =>
          MerkleNode(seed, witL, witR, Empty(), evolveKey(right, shiftStep(step)))

        case leaf: SigningLeaf => leaf
        case _                 => Empty()
      }
    } else {
      input match {
        case MerkleNode(seed, witL, witR, left, Empty()) =>
          MerkleNode(seed, witL, witR, evolveKey(left, shiftStep(step)), Empty())

        case MerkleNode(seed, witL, witR, Empty(), right) =>
          MerkleNode(seed, witL, witR, Empty(), evolveKey(right, shiftStep(step)))

        case leaf: SigningLeaf => leaf
        case _                 => Empty()
      }
    }
  }

  /**
   * Sign a message with the sum composition key.
   *
   * @param keyTree secret key tree
   * @param m       message to sign
   * @return (verification key, signature, witness path)
   */
  private[kes] def sign(keyTree: KesBinaryTree, m: Array[Byte]): (Array[Byte], Array[Byte], Vector[Array[Byte]]) = {
    @tailrec
    def loop(
      keyTree: KesBinaryTree,
      W:       Vector[Array[Byte]] = Vector()
    ): (Array[Byte], Array[Byte], Vector[Array[Byte]]) = keyTree match {
      case MerkleNode(_, witL, _, Empty(), right) => loop(right, witL.clone() +: W)
      case MerkleNode(_, _, witR, left, _)        => loop(left, witR.clone() +: W)
      case leaf: SigningLeaf                      => (leaf.vk.clone(), sSign(m, leaf.sk).clone(), W)
      case _                                      => (Array.fill(pkBytes)(0: Byte), Array.fill(sigBytes)(0: Byte), Vector(Array()))
    }
    loop(keyTree)
  }

  /**
   * Verify a sum composition signature.
   *
   * @param kesSig signature (vk, sig, witness)
   * @param m      message
   * @param kesVk  verification key (root hash, step)
   * @return true if valid
   */
  private[kes] def verify(
    kesSig: (Array[Byte], Array[Byte], Vector[Array[Byte]]),
    m:      Array[Byte],
    kesVk:  (Array[Byte], Int)
  ): Boolean = {
    val (vkSign, sigSign, merkleProof) = kesSig
    val (root: Array[Byte], step: Int) = kesVk

    // Determine if step corresponds to left (0) or right (1) at each height
    val leftGoing: Int => Boolean = (level: Int) => ((step / exp(level)) % 2) == 0

    def verifyMerkle(W: Vector[Array[Byte]]): Boolean =
      if (W.isEmpty) emptyWitness
      else if (W.length == 1) singleWitness(W.head)
      else if (leftGoing(0)) multiWitness(W.tail, hash(vkSign), W.head, 1)
      else multiWitness(W.tail, W.head, hash(vkSign), 1)

    def emptyWitness: Boolean = root.sameElements(hash(vkSign))

    def singleWitness(witness: Array[Byte]): Boolean =
      if (leftGoing(0)) root.sameElements(hash(hash(vkSign) ++ witness))
      else root.sameElements(hash(witness ++ hash(vkSign)))

    @tailrec
    def multiWitness(
      witnessList:  Vector[Array[Byte]],
      witnessLeft:  Array[Byte],
      witnessRight: Array[Byte],
      index:        Int
    ): Boolean =
      if (witnessList.isEmpty) root.sameElements(hash(witnessLeft ++ witnessRight))
      else if (leftGoing(index))
        multiWitness(witnessList.tail, hash(witnessLeft ++ witnessRight), witnessList.head, index + 1)
      else multiWitness(witnessList.tail, witnessList.head, hash(witnessLeft ++ witnessRight), index + 1)

    val verifySign = sVerify(m, sigSign, vkSign)

    verifyMerkle(merkleProof) && verifySign
  }
}
