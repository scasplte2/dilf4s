package xyz.kd5ujc.dilf4s.error;

/** A JLVM {@code Crypto} error's cause: the refusal of the operation's library, by family and kind ({@code docs/specs/jlvm.md} section 3). */
public sealed interface CryptoCause
  permits
    CryptoCause.Signing,
    CryptoCause.Sigma,
    CryptoCause.Zk,
    CryptoCause.Groth16,
    CryptoCause.Vrf,
    CryptoCause.Hash,
    CryptoCause.Merkle,
    CryptoCause.Trie,
    CryptoCause.Poseidon,
    CryptoCause.PoseidonMerkle {

  /** The kind, as the schema and the vectors name it. */
  String kind();

  /** {@code Signing}. */
  record Signing(CryptoError error) implements CryptoCause {
    @Override
    public String kind() {
      return "Signing";
    }
  }

  /** {@code Sigma}. */
  record Sigma(SigmaError error) implements CryptoCause {
    @Override
    public String kind() {
      return "Sigma";
    }
  }

  /** {@code Zk}. */
  record Zk(ZkError error) implements CryptoCause {
    @Override
    public String kind() {
      return "Zk";
    }
  }

  /** {@code Groth16}. */
  record Groth16(Groth16Error error) implements CryptoCause {
    @Override
    public String kind() {
      return "Groth16";
    }
  }

  /** {@code Vrf}. */
  record Vrf(VrfError error) implements CryptoCause {
    @Override
    public String kind() {
      return "Vrf";
    }
  }

  /** {@code Hash}. */
  record Hash(HashError error) implements CryptoCause {
    @Override
    public String kind() {
      return "Hash";
    }
  }

  /** {@code Merkle}. */
  record Merkle(MerkleError error) implements CryptoCause {
    @Override
    public String kind() {
      return "Merkle";
    }
  }

  /** {@code Trie}. */
  record Trie(TrieError error) implements CryptoCause {
    @Override
    public String kind() {
      return "Trie";
    }
  }

  /** {@code Poseidon}. */
  record Poseidon(PoseidonError error) implements CryptoCause {
    @Override
    public String kind() {
      return "Poseidon";
    }
  }

  /** {@code PoseidonMerkle}. */
  record PoseidonMerkle(PoseidonMerkleError error) implements CryptoCause {
    @Override
    public String kind() {
      return "PoseidonMerkle";
    }
  }
}
