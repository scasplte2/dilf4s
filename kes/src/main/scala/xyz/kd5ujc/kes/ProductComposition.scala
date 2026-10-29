package xyz.kd5ujc.kes

import java.security.SecureRandom

import xyz.kd5ujc.kes.KesBinaryTree._
import xyz.kd5ujc.signing.Ed25519Signing

/**
 * KES Product Composition.
 *
 * Nests a "super" sum tree over "sub" sum trees to efficiently scale
 * the number of time steps. Heights (h_sup, h_sub) yields 2^(h_sup+h_sub) time steps.
 */
class ProductComposition extends KesEd25519Blake2b256 {

  protected val sumComposition = new SumComposition

  protected val random: SecureRandom = new SecureRandom

  // Internal type aliases matching the reference implementation
  type SIG = (
    (Array[Byte], Array[Byte], Vector[Array[Byte]]),
    (Array[Byte], Array[Byte], Vector[Array[Byte]]),
    Array[Byte]
  )
  type VK = (Array[Byte], Int)
  type SK = (KesBinaryTree, KesBinaryTree, Array[Byte], (Array[Byte], Array[Byte], Vector[Array[Byte]]))

  /**
   * Get the current time step of a product composition key.
   */
  private[kes] def getKeyTime(key: SK): Int = {
    val numSubSteps = exp(sumComposition.getTreeHeight(key._2))
    val tSup = sumComposition.getKeyTime(key._1)
    val tSub = sumComposition.getKeyTime(key._2)
    (tSup * numSubSteps) + tSub
  }

  /**
   * Generate the verification key from a secret key.
   */
  private[kes] def generateVerificationKey(key: SK): VK = key._1 match {
    case node: MerkleNode  => (witness(node), getKeyTime(key))
    case leaf: SigningLeaf => (witness(leaf), 0)
    case Empty()           => (Array.fill(hashBytes)(0: Byte), 0)
  }

  /**
   * Generate a product composition secret key.
   *
   * @param seed      32-byte input entropy
   * @param heightSup height of super tree
   * @param heightSub height of sub trees
   * @return product secret key
   */
  private[kes] def generateSecretKey(seed: Array[Byte], heightSup: Int, heightSub: Int): SK = {
    val rSuper = prng(seed)
    val rSub = prng(rSuper._2)
    val superScheme = sumComposition.generateSecretKey(rSuper._1, heightSup)
    val subScheme = sumComposition.generateSecretKey(rSub._1, heightSub)
    val kesVkSub = sumComposition.generateVerificationKey(subScheme)
    val kesSigSup = sumComposition.sign(superScheme, kesVkSub._1)
    random.nextBytes(rSuper._2)
    random.nextBytes(seed)
    (superScheme, subScheme, rSub._2, kesSigSup)
  }

  /**
   * Erase the secret key at the leaf level of a private key in the sum composition.
   * Used to commit to a child verification key and then convert the parent private key to a
   * state that can't be used to re-commit to another child key until the next time step.
   */
  private[kes] def eraseLeafSecretKey(input: KesBinaryTree): KesBinaryTree =
    input match {
      case n: MerkleNode =>
        (n.left, n.right) match {
          case (Empty(), _) =>
            MerkleNode(n.seed, n.witnessLeft, n.witnessRight, Empty(), eraseLeafSecretKey(n.right))
          case (_, Empty()) =>
            MerkleNode(n.seed, n.witnessLeft, n.witnessRight, eraseLeafSecretKey(n.left), Empty())
          case (_, _) => throw new Exception("Evolving Key Configuration Error")
        }
      case l: SigningLeaf =>
        random.nextBytes(l.sk)
        SigningLeaf(Array.fill[Byte](Ed25519Signing.ScalarBytes)(0), l.vk)
      case _ => throw new Exception("Evolving Key Configuration Error")
    }

  /**
   * Erase the secret key at the leaf level of a private key in the product composition.
   */
  private[kes] def eraseProductLeafSk(key: SK): SK =
    (key._1, eraseLeafSecretKey(key._2), key._3, key._4)

  /**
   * Update product keys to the specified time step.
   */
  private[kes] def updateKey(key: SK, step: Int): SK = {
    val keyTime = getKeyTime(key)
    val keyTimeSup = sumComposition.getKeyTime(key._1)
    val heightSup = sumComposition.getTreeHeight(key._1)
    val heightSub = sumComposition.getTreeHeight(key._2)
    val totalSteps = exp(heightSup + heightSub)
    val totalStepsSub = exp(heightSub)
    val newKeyTimeSup = step / totalStepsSub
    val newKeyTimeSub = step % totalStepsSub

    def getSeed(seeds: (Array[Byte], Array[Byte]), iter: Int): (Array[Byte], Array[Byte]) =
      if (iter < newKeyTimeSup) {
        val out = getSeed(prng(seeds._2), iter + 1)
        random.nextBytes(seeds._1)
        random.nextBytes(seeds._2)
        out
      } else seeds

    if (step == 0) key
    else if (step > keyTime && step < totalSteps) {
      if (keyTimeSup < newKeyTimeSup) {
        sumComposition.eraseOldNode(key._2)
        val (s1, s2) = getSeed((Array(), key._3), keyTimeSup)
        val superScheme = sumComposition.evolveKey(key._1, newKeyTimeSup)
        val newSubScheme = sumComposition.generateSecretKey(s1, heightSub)
        random.nextBytes(s1)
        val kesVkSub = sumComposition.generateVerificationKey(newSubScheme)
        val kesSigSuper = sumComposition.sign(superScheme, kesVkSub._1)
        val forwardSecureSuperScheme = eraseLeafSecretKey(superScheme)
        val updatedSubScheme = sumComposition.evolveKey(newSubScheme, newKeyTimeSub)
        (forwardSecureSuperScheme, updatedSubScheme, s2, kesSigSuper)
      } else {
        val subScheme = sumComposition.updateKey(key._2, newKeyTimeSub)
        (key._1, subScheme, key._3, key._4)
      }
    } else {
      throw new Error(
        s"Update error - Max steps: $totalSteps, current step: $keyTime, requested increase: $step"
      )
    }
  }

  /**
   * Sign a message with the product composition key.
   */
  private[kes] def sign(key: SK, m: Array[Byte]): SIG =
    (key._4, sumComposition.sign(key._2, m), sumComposition.generateVerificationKey(key._2)._1)

  /**
   * Verify a product composition signature.
   */
  private[kes] def verify(kesSig: SIG, m: Array[Byte], kesVk: VK): Boolean = {
    val totalStepsSub = exp(kesSig._2._3.length)
    val keyTimeSup = kesVk._2 / totalStepsSub
    val keyTimeSub = kesVk._2 % totalStepsSub

    val verifySup = sumComposition.verify(kesSig._1, kesSig._3, (kesVk._1, keyTimeSup))
    val verifySub = sumComposition.verify(kesSig._2, m, (kesSig._3, keyTimeSub))

    verifySup && verifySub
  }
}
