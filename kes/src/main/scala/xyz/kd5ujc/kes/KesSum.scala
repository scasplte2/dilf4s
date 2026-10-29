package xyz.kd5ujc.kes

/**
 * Public API for KES Sum Composition.
 *
 * Sum composition uses a binary Merkle tree of Ed25519 keys.
 * Tree height h yields 2^h time steps.
 */
class KesSum extends SumComposition {

  /**
   * Create a new KES sum keypair.
   *
   * @param seed   32-byte input entropy
   * @param height tree height (yields 2^height time steps)
   * @return (secret key, verification key)
   */
  def createKeyPair(seed: Array[Byte], height: Int): (SecretKeyKesSum, VerificationKeyKesSum) = {
    val sk = generateSecretKey(seed.clone(), height)
    val pk = generateVerificationKey(sk)
    (SecretKeyKesSum(sk), VerificationKeyKesSum(pk._1, pk._2))
  }

  /**
   * Sign a message with the KES sum secret key.
   *
   * @param privateKey secret key
   * @param message    message to sign
   * @return KES sum signature
   */
  def sign(privateKey: SecretKeyKesSum, message: Array[Byte]): SignatureKesSum = {
    val sumSig = sign(privateKey.tree, message)
    SignatureKesSum(sumSig._1, sumSig._2, sumSig._3)
  }

  /**
   * Verify a KES sum signature.
   *
   * @param signature signature to verify
   * @param message   message that was signed
   * @param verifyKey verification key
   * @return true if valid
   */
  def verify(
    signature: SignatureKesSum,
    message:   Array[Byte],
    verifyKey: VerificationKeyKesSum
  ): Boolean = {
    val sumSig = (signature.verificationKey, signature.signature, signature.witness.toVector)
    val sumVk = (verifyKey.value, verifyKey.step)
    verify(sumSig, message, sumVk)
  }

  /**
   * Update the secret key to a new time step.
   *
   * @param privateKey current secret key
   * @param steps      target time step
   * @return updated secret key
   */
  def update(privateKey: SecretKeyKesSum, steps: Int): SecretKeyKesSum =
    SecretKeyKesSum(updateKey(privateKey.tree, steps))

  /**
   * Get the current time step of a secret key.
   */
  def getCurrentStep(privateKey: SecretKeyKesSum): Int =
    getKeyTime(privateKey.tree)

  /**
   * Get the maximum time step for a secret key.
   */
  def getMaxStep(privateKey: SecretKeyKesSum): Int =
    exp(getTreeHeight(privateKey.tree))

  /**
   * Derive the verification key from a secret key.
   */
  def getVerificationKey(privateKey: SecretKeyKesSum): VerificationKeyKesSum = {
    val vk = generateVerificationKey(privateKey.tree)
    VerificationKeyKesSum(vk._1, vk._2)
  }
}

object KesSum {
  val instance: KesSum = new KesSum
}
