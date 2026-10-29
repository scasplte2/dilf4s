package xyz.kd5ujc.kes

/**
 * KES Sum Composition key and signature types.
 */
final case class SecretKeyKesSum(tree: KesBinaryTree)

final case class VerificationKeyKesSum(
  value: Array[Byte],
  step:  Int
) {

  override def equals(obj: Any): Boolean = obj match {
    case that: VerificationKeyKesSum =>
      this.step == that.step && this.value.sameElements(that.value)
    case _ => false
  }

  override def hashCode(): Int = {
    val prime = 31
    var result = 1
    result = prime * result + step
    result = prime * result + java.util.Arrays.hashCode(value)
    result
  }
}

final case class SignatureKesSum(
  verificationKey: Array[Byte],
  signature:       Array[Byte],
  witness:         Seq[Array[Byte]]
) {

  override def equals(obj: Any): Boolean = obj match {
    case that: SignatureKesSum =>
      this.verificationKey.sameElements(that.verificationKey) &&
      this.signature.sameElements(that.signature) &&
      this.witness.length == that.witness.length &&
      this.witness.zip(that.witness).forall { case (a, b) => a.sameElements(b) }
    case _ => false
  }

  override def hashCode(): Int = {
    val prime = 31
    var result = 1
    result = prime * result + java.util.Arrays.hashCode(verificationKey)
    result = prime * result + java.util.Arrays.hashCode(signature)
    result = prime * result + witness.map(java.util.Arrays.hashCode).hashCode()
    result
  }
}

/**
 * KES Product Composition key and signature types.
 */
final case class SecretKeyKesProduct(
  superTree:    KesBinaryTree,
  subTree:      KesBinaryTree,
  nextSubSeed:  Array[Byte],
  subSignature: SignatureKesSum
)

final case class VerificationKeyKesProduct(
  value: Array[Byte],
  step:  Int
) {

  override def equals(obj: Any): Boolean = obj match {
    case that: VerificationKeyKesProduct =>
      this.step == that.step && this.value.sameElements(that.value)
    case _ => false
  }

  override def hashCode(): Int = {
    val prime = 31
    var result = 1
    result = prime * result + step
    result = prime * result + java.util.Arrays.hashCode(value)
    result
  }
}

final case class SignatureKesProduct(
  superSignature: SignatureKesSum,
  subSignature:   SignatureKesSum,
  subRoot:        Array[Byte]
) {

  override def equals(obj: Any): Boolean = obj match {
    case that: SignatureKesProduct =>
      this.superSignature == that.superSignature &&
      this.subSignature == that.subSignature &&
      this.subRoot.sameElements(that.subRoot)
    case _ => false
  }

  override def hashCode(): Int = {
    val prime = 31
    var result = 1
    result = prime * result + superSignature.hashCode()
    result = prime * result + subSignature.hashCode()
    result = prime * result + java.util.Arrays.hashCode(subRoot)
    result
  }
}
