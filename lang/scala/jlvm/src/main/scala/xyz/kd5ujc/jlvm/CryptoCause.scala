package xyz.kd5ujc.jlvm

enum CryptoCause {
  case Signing(error: xyz.kd5ujc.crypto.signing.CryptoError)
  case Sigma(error: xyz.kd5ujc.crypto.sigma.SigmaError)
  case Zk(error: xyz.kd5ujc.crypto.zk.ZkError)
  case Groth16(error: xyz.kd5ujc.crypto.zk.Groth16Error)
  case Vrf(error: xyz.kd5ujc.crypto.vrf.VrfError)
  case Hash(error: xyz.kd5ujc.core.hash.HashError)
  case Merkle(error: xyz.kd5ujc.accumulators.merkle.MerkleError)
  case Trie(error: xyz.kd5ujc.accumulators.trie.TrieError)
  case Poseidon(error: xyz.kd5ujc.core.hash.PoseidonError)
  case PoseidonMerkle(error: xyz.kd5ujc.accumulators.poseidon.PoseidonMerkleError)

  def kind: CryptoCauseKind = this match {
    case Signing(_)        => CryptoCauseKind.Signing
    case Sigma(_)          => CryptoCauseKind.Sigma
    case Zk(_)             => CryptoCauseKind.Zk
    case Groth16(_)        => CryptoCauseKind.Groth16
    case Vrf(_)            => CryptoCauseKind.Vrf
    case Hash(_)           => CryptoCauseKind.Hash
    case Merkle(_)         => CryptoCauseKind.Merkle
    case Trie(_)           => CryptoCauseKind.Trie
    case Poseidon(_)       => CryptoCauseKind.Poseidon
    case PoseidonMerkle(_) => CryptoCauseKind.PoseidonMerkle
  }

  def message: String = this match {
    case Signing(e)        => e.toString
    case Sigma(e)          => e.message
    case Zk(e)             => e.toString
    case Groth16(e)        => e.toString
    case Vrf(e)            => e.toString
    case Hash(e)           => e.toString
    case Merkle(e)         => e.toString
    case Trie(e)           => e.toString
    case Poseidon(e)       => e.toString
    case PoseidonMerkle(e) => e.toString
  }
}
