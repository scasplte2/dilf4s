/** Main sources that predate the purity gate. Each entry is removed when its file is rewritten; the list only shrinks. */
object LegacyPurity {

  val files: Seq[String] = Seq(
    "accumulators/src/main/scala/xyz/kd5ujc/accumulators/merkle/api/MerkleProver.scala",
    "accumulators/src/main/scala/xyz/kd5ujc/accumulators/merkle/impl/OptimizedMerkleProducer.scala",
    "accumulators/src/main/scala/xyz/kd5ujc/accumulators/mpt/api/MerklePatriciaProver.scala",
    "accumulators/src/main/scala/xyz/kd5ujc/accumulators/mpt/api/MerklePatriciaVerifier.scala",
    "accumulators/src/main/scala/xyz/kd5ujc/accumulators/mpt/impl/OptimizedMerklePatriciaProducer.scala",
    "accumulators/src/main/scala/xyz/kd5ujc/accumulators/mpt/impl/SimpleMerklePatriciaProducer.scala",
    "accumulators/src/main/scala/xyz/kd5ujc/accumulators/mpt/Nibble.scala",
    "core/src/main/scala/xyz/kd5ujc/hash/HashProof.scala",
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
    "storage/src/main/scala/xyz/kd5ujc/storage/store/LevelDbStore.scala",
    "storage/src/main/scala/xyz/kd5ujc/storage/versioned_store/package.scala",
    "vrf/src/main/scala/xyz/kd5ujc/vrf/EcVrf25519.scala"
  )
}
