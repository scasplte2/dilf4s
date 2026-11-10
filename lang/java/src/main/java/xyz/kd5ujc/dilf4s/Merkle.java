package xyz.kd5ujc.dilf4s;

import java.util.List;

import xyz.kd5ujc.dilf4s.error.EncodeError;
import xyz.kd5ujc.dilf4s.error.MerkleError;
import xyz.kd5ujc.dilf4s.internal.Proofs;

/**
 * Merkle trees and their RFC 9162 proofs ({@code docs/specs/merkle.md}), under dilf4s's tagged hashing (leaves commit to
 * canonical values) or RFC 6962's ({@code H(0x00 || leaf)}, {@code H(0x01 || l || r)}). Digests are bytes of the
 * algorithm's length; a proof of another tree is {@code false}, a malformed one a {@link MerkleError}.
 */
public final class Merkle {
  private Merkle() {}

  /** How leaves and nodes are hashed. */
  public enum Profile {
    /** dilf4s's tagged hashing: {@code dilf4s/merkle/leaf/v1}, {@code node/v1}, {@code empty/v1}. */
    DILF4S,
    /** RFC 6962 (Certificate Transparency). */
    RFC6962
  }

  /**
   * An inclusion proof: the leaf's index, the tree's size and the audit path. The path's arrays are copied in and out.
   *
   * @param index the leaf's index
   * @param size the tree's size
   * @param path the audit path, leaf to root
   */
  public record InclusionProof(long index, long size, List<byte[]> path) {
    public InclusionProof {
      path = Bytes.copies(path);
    }

    @Override
    public List<byte[]> path() {
      return Bytes.copies(path);
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof InclusionProof p && p.index == index && p.size == size && Bytes.same(p.path, path);
    }

    @Override
    public int hashCode() {
      return Long.hashCode(index) * 31 + Long.hashCode(size);
    }

    @Override
    public String toString() {
      return "InclusionProof(" + index + ", " + size + ", " + Bytes.hexes(path) + ")";
    }
  }

  /**
   * A consistency proof between two sizes of one tree. The path's arrays are copied in and out.
   *
   * @param oldSize the older size
   * @param newSize the newer size
   * @param path the consistency path
   */
  public record ConsistencyProof(long oldSize, long newSize, List<byte[]> path) {
    public ConsistencyProof {
      path = Bytes.copies(path);
    }

    @Override
    public List<byte[]> path() {
      return Bytes.copies(path);
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof ConsistencyProof p && p.oldSize == oldSize && p.newSize == newSize && Bytes.same(p.path, path);
    }

    @Override
    public int hashCode() {
      return Long.hashCode(oldSize) * 31 + Long.hashCode(newSize);
    }

    @Override
    public String toString() {
      return "ConsistencyProof(" + oldSize + ", " + newSize + ", " + Bytes.hexes(path) + ")";
    }
  }

  private static boolean tagged(Profile p) {
    return switch (p) {
      case DILF4S -> true;
      case RFC6962 -> false;
    };
  }

  /**
   * A leaf's hash over its bytes as they are: dilf4s's tagged leaf hash, or RFC 6962's {@code H(0x00 || leaf)}. For
   * opaque leaves (an RFC 9162 log's entries); a canonical value commits through {@link #valueLeafHash}.
   *
   * @param profile the hashing
   * @param algorithm the hash
   * @param leaf the leaf's bytes
   * @return the hash, or {@code TypeMismatch} for a null argument
   */
  public static Result<byte[], MerkleError> leafHash(Profile profile, Hashing.Algorithm algorithm, byte[] leaf) {
    return profile == null || algorithm == null || leaf == null
      ? Result.err(Wrap.nullArgument("a profile, an algorithm and a leaf"))
      : Result.ok(Proofs.bytesLeaf(tagged(profile), algorithm, leaf));
  }

  /**
   * A dilf4s leaf's hash for a canonical value: the tagged hash of its commitment ({@link ValueCommitment}), as
   * {@link #verifyLeaf} commits a leaf.
   *
   * @param algorithm the hash
   * @param leaf the leaf
   * @return the hash, or {@code TooLarge}
   */
  public static Result<byte[], EncodeError> valueLeafHash(Hashing.Algorithm algorithm, Value leaf) {
    return algorithm == null || leaf == null
      ? Result.err(Wrap.nullArgument("an algorithm and a leaf"))
      : Proofs.merkleLeaf(algorithm, Wrap.scala(leaf));
  }

  /**
   * The root of the tree of {@code leafHashes} (the empty tree's root for none).
   *
   * @param profile the hashing
   * @param algorithm the hash
   * @param leafHashes the leaves' hashes, in order
   * @return the root, or {@code Encoding} for a hash of the wrong length
   */
  public static Result<byte[], MerkleError> root(Profile profile, Hashing.Algorithm algorithm, List<byte[]> leafHashes) {
    return profile == null || algorithm == null || Bytes.anyNull(leafHashes)
      ? Result.err(Wrap.nullArgument("a profile, an algorithm and leaf hashes"))
      : Proofs.merkleRoot(tagged(profile), algorithm, List.copyOf(leafHashes));
  }

  /**
   * The inclusion proof of the leaf at {@code index}.
   *
   * @param profile the hashing
   * @param algorithm the hash
   * @param leafHashes the leaves' hashes, in order
   * @param index the leaf
   * @return the proof, or the refusal
   */
  public static Result<InclusionProof, MerkleError> inclusionProof(Profile profile, Hashing.Algorithm algorithm, List<byte[]> leafHashes, long index) {
    return profile == null || algorithm == null || Bytes.anyNull(leafHashes)
      ? Result.err(Wrap.nullArgument("a profile, an algorithm and leaf hashes"))
      : Proofs.inclusionProof(tagged(profile), algorithm, List.copyOf(leafHashes), index);
  }

  /**
   * The consistency proof from the tree's first {@code oldSize} leaves to all of them.
   *
   * @param profile the hashing
   * @param algorithm the hash
   * @param leafHashes the leaves' hashes, in order
   * @param oldSize the older size
   * @return the proof, or the refusal
   */
  public static Result<ConsistencyProof, MerkleError> consistencyProof(Profile profile, Hashing.Algorithm algorithm, List<byte[]> leafHashes, long oldSize) {
    return profile == null || algorithm == null || Bytes.anyNull(leafHashes)
      ? Result.err(Wrap.nullArgument("a profile, an algorithm and leaf hashes"))
      : Proofs.consistencyProof(tagged(profile), algorithm, List.copyOf(leafHashes), oldSize);
  }

  /**
   * Whether {@code proof} places {@code leafHash} in the tree of {@code root}: {@code false} for a path that does not fit
   * or another root; an index outside the size is {@code IndexOutOfRange}.
   *
   * @param profile the hashing
   * @param algorithm the hash
   * @param root the root
   * @param leafHash the leaf's hash
   * @param proof the proof
   * @return the verdict, or the refusal
   */
  public static Result<Boolean, MerkleError> verifyInclusion(Profile profile, Hashing.Algorithm algorithm, byte[] root, byte[] leafHash, InclusionProof proof) {
    return profile == null || algorithm == null || root == null || leafHash == null || proof == null || Bytes.anyNull(proof.path)
      ? Result.err(Wrap.nullArgument("a profile, an algorithm, a root, a leaf hash and a proof"))
      : Proofs.verifyInclusion(tagged(profile), algorithm, root, leafHash, proof);
  }

  /**
   * Whether {@code proof} shows the tree of {@code newRoot} extends the tree of {@code oldRoot}.
   *
   * @param profile the hashing
   * @param algorithm the hash
   * @param oldRoot the older root
   * @param newRoot the newer root
   * @param proof the proof
   * @return the verdict, or the refusal ({@code InvalidSizes} when the old size passes the new)
   */
  public static Result<Boolean, MerkleError> verifyConsistency(Profile profile, Hashing.Algorithm algorithm, byte[] oldRoot, byte[] newRoot, ConsistencyProof proof) {
    return profile == null || algorithm == null || oldRoot == null || newRoot == null || proof == null || Bytes.anyNull(proof.path)
      ? Result.err(Wrap.nullArgument("a profile, an algorithm, two roots and a proof"))
      : Proofs.verifyConsistency(tagged(profile), algorithm, oldRoot, newRoot, proof);
  }

  /**
   * {@code merkle_verify}: whether {@code leaf} is at the proof's index of the dilf4s tree {@code (root, size)}, the proof
   * in its canonical form; a proof of another size or root is {@code false}, and a leaf heavier than the proof bound is
   * {@code TooLarge}, before the proof is read.
   *
   * @param algorithm the hash
   * @param root the root
   * @param size the tree's size
   * @param leaf the leaf
   * @param proof the proof's canonical value
   * @return the verdict, or the refusal
   */
  public static Result<Boolean, MerkleError> verifyLeaf(Hashing.Algorithm algorithm, byte[] root, long size, Value leaf, Value proof) {
    return algorithm == null || root == null || leaf == null || proof == null
      ? Result.err(Wrap.nullArgument("an algorithm, a root, a leaf and a proof"))
      : Proofs.verifyLeaf(algorithm, root, size, Wrap.scala(leaf), Wrap.scala(proof));
  }

  /**
   * An inclusion proof's canonical form ({@code shared/schemas/merkle.cddl}).
   *
   * @param algorithm the hash its path is in
   * @param proof the proof
   * @return the value, or the refusal
   */
  public static Result<Value, MerkleError> encode(Hashing.Algorithm algorithm, InclusionProof proof) {
    return algorithm == null || proof == null || Bytes.anyNull(proof.path)
      ? Result.err(Wrap.nullArgument("an algorithm and a proof"))
      : Proofs.encodeInclusion(algorithm, proof).map(Wrap::of);
  }

  /**
   * A consistency proof's canonical form.
   *
   * @param algorithm the hash its path is in
   * @param proof the proof
   * @return the value, or the refusal
   */
  public static Result<Value, MerkleError> encode(Hashing.Algorithm algorithm, ConsistencyProof proof) {
    return algorithm == null || proof == null || Bytes.anyNull(proof.path)
      ? Result.err(Wrap.nullArgument("an algorithm and a proof"))
      : Proofs.encodeConsistency(algorithm, proof).map(Wrap::of);
  }

  /**
   * An inclusion proof from its canonical form: a value heavier than the proof bound is {@code Encoding}, before it is
   * read.
   *
   * @param algorithm the hash its path is in
   * @param value the value
   * @return the proof, or the refusal
   */
  public static Result<InclusionProof, MerkleError> decodeInclusion(Hashing.Algorithm algorithm, Value value) {
    return algorithm == null || value == null
      ? Result.err(Wrap.nullArgument("an algorithm and a value"))
      : Proofs.decodeInclusion(algorithm, Wrap.scala(value));
  }

  /**
   * A consistency proof from its canonical form.
   *
   * @param algorithm the hash its path is in
   * @param value the value
   * @return the proof, or the refusal
   */
  public static Result<ConsistencyProof, MerkleError> decodeConsistency(Hashing.Algorithm algorithm, Value value) {
    return algorithm == null || value == null
      ? Result.err(Wrap.nullArgument("an algorithm and a value"))
      : Proofs.decodeConsistency(algorithm, Wrap.scala(value));
  }
}
