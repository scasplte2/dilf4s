/** Main sources that predate the purity gate. Each entry is removed when its file is rewritten; the list only shrinks. */
object LegacyPurity {

  val files: Seq[String] = Seq(
    "kes/src/main/scala/xyz/kd5ujc/kes/KesEd25519Blake2b256.scala",
    "kes/src/main/scala/xyz/kd5ujc/kes/models.scala",
    "kes/src/main/scala/xyz/kd5ujc/kes/ProductComposition.scala",
    "kes/src/main/scala/xyz/kd5ujc/kes/SumComposition.scala",
    "signing/src/main/scala/xyz/kd5ujc/signature/ecdsa/Ed25519.scala",
    "signing/src/main/scala/xyz/kd5ujc/signature/extended/Bip32Index.scala",
    "signing/src/main/scala/xyz/kd5ujc/signature/keygen/Entropy.scala",
    "signing/src/main/scala/xyz/kd5ujc/signature/keygen/Mnemonic.scala",
    "signing/src/main/scala/xyz/kd5ujc/signature/keygen/SeedDerivation.scala",
    "signing/src/main/scala/xyz/kd5ujc/signing/Ed25519Signing.scala",
    "signing/src/main/scala/xyz/kd5ujc/signing/Ristretto255.scala",
    "vrf/src/main/scala/xyz/kd5ujc/vrf/EcVrf25519.scala"
  )
}
