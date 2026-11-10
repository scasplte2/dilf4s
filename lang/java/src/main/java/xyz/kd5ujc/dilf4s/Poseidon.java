package xyz.kd5ujc.dilf4s;

import java.math.BigInteger;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import xyz.kd5ujc.dilf4s.error.PoseidonError;
import xyz.kd5ujc.dilf4s.error.PoseidonMerkleError;
import xyz.kd5ujc.dilf4s.internal.Proofs;

/**
 * Poseidon over BN254's scalar field, circomlib's parameters ({@code docs/specs/zk.md} section 3), and its sparse Merkle
 * tree (section 4). Field elements are {@code BigInteger}s in {@code [0, r)}: anything else is {@code NonCanonical}.
 */
public final class Poseidon {
  private Poseidon() {}

  /** The field modulus {@code r}. */
  public static final BigInteger MODULUS = Proofs.FrModulus();

  /** The most inputs {@link #hash} takes. */
  public static final int MAX_INPUTS = Proofs.PoseidonMaxInputs();

  /** The deepest tree. */
  public static final int MAX_DEPTH = Proofs.PoseidonMaxDepth();

  /**
   * A Merkle proof: the leaf's position and the siblings, leaf to root. Checked where it is used.
   *
   * @param position the position
   * @param siblings the siblings, each in the field
   */
  public record MerkleProof(BigInteger position, List<BigInteger> siblings) {
    public MerkleProof {
      siblings = Bytes.listCopy(siblings);
    }
  }

  private static boolean malformed(MerkleProof p) {
    return p == null || p.position() == null || p.siblings() == null || p.siblings().stream().anyMatch(Objects::isNull);
  }

  /**
   * {@code value}, if it is a field element.
   *
   * @param value the integer
   * @return the element, or {@code NonCanonical}
   */
  public static Result<BigInteger, PoseidonError> element(BigInteger value) {
    return value == null ? Result.err(Wrap.nullArgument("an integer")) : Proofs.frCheck(value);
  }

  /**
   * {@code value mod r}.
   *
   * @param value the integer
   * @return the element, or {@code TypeMismatch} for null
   */
  public static Result<BigInteger, PoseidonError> reduce(BigInteger value) {
    return value == null ? Result.err(Wrap.nullArgument("an integer")) : Result.ok(Proofs.frReduce(value));
  }

  /**
   * The hash of 1 to {@link #MAX_INPUTS} elements.
   *
   * @param inputs the elements
   * @return the hash, or the refusal
   */
  public static Result<BigInteger, PoseidonError> hash(List<BigInteger> inputs) {
    return inputs == null || inputs.stream().anyMatch(Objects::isNull)
      ? Result.err(Wrap.nullArgument("inputs"))
      : Proofs.poseidonHash(List.copyOf(inputs));
  }

  /**
   * The two-to-one compression: the hash of {@code [left, right]}.
   *
   * @param left an element
   * @param right an element
   * @return the hash, or the refusal
   */
  public static Result<BigInteger, PoseidonError> compress(BigInteger left, BigInteger right) {
    return left == null || right == null ? Result.err(Wrap.nullArgument("two elements")) : Proofs.poseidonCompress(left, right);
  }

  /**
   * {@code proof}, if its position fits its depth and its siblings are elements.
   *
   * @param proof the proof
   * @return the proof, or the refusal
   */
  public static Result<MerkleProof, PoseidonMerkleError> check(MerkleProof proof) {
    return malformed(proof) ? Result.err(Wrap.nullArgument("a proof")) : Proofs.poseidonProofCheck(proof);
  }

  /**
   * The root {@code proof} gives {@code leaf}.
   *
   * @param leaf the leaf
   * @param proof the proof
   * @return the root, or the refusal
   */
  public static Result<BigInteger, PoseidonMerkleError> computeRoot(BigInteger leaf, MerkleProof proof) {
    return leaf == null || malformed(proof) ? Result.err(Wrap.nullArgument("a leaf and a proof")) : Proofs.poseidonComputeRoot(leaf, proof);
  }

  /**
   * Whether {@code proof} places {@code leaf} under {@code root}. The leaf, the siblings and the root are field
   * elements: any outside the field is {@code Encoding}, never a mismatch.
   *
   * @param leaf the leaf
   * @param proof the proof
   * @param root the root
   * @return the verdict, or the refusal
   */
  public static Result<Boolean, PoseidonMerkleError> verifyInclusion(BigInteger leaf, MerkleProof proof, BigInteger root) {
    return leaf == null || root == null || malformed(proof)
      ? Result.err(Wrap.nullArgument("a leaf, a proof and a root"))
      : Proofs.poseidonVerifyInclusion(leaf, proof, root);
  }

  /**
   * Whether {@code proof} shows its position empty (a zero leaf) under {@code root}.
   *
   * @param proof the proof
   * @param root the root
   * @return the verdict, or the refusal
   */
  public static Result<Boolean, PoseidonMerkleError> verifyAbsence(MerkleProof proof, BigInteger root) {
    return root == null || malformed(proof) ? Result.err(Wrap.nullArgument("a proof and a root")) : Proofs.poseidonVerifyAbsence(proof, root);
  }

  /**
   * The roots of the empty subtrees, level 0 (a zero leaf) to {@code depth}.
   *
   * @param depth the depth
   * @return the roots, or {@code Depth} outside {@code 1..MAX_DEPTH}
   */
  public static Result<List<BigInteger>, PoseidonMerkleError> zeroHashes(int depth) {
    return Proofs.poseidonZeroHashes(depth).map(List::copyOf);
  }

  private static boolean anyNullEntry(List<Map.Entry<BigInteger, BigInteger>> entries) {
    return entries == null || entries.stream().anyMatch(e -> e == null || e.getKey() == null || e.getValue() == null);
  }

  /**
   * The root of the tree of {@code depth} with {@code entries} (position and leaf) set; a repeated position is refused.
   *
   * @param depth the depth
   * @param entries positions and leaves
   * @return the root, or the refusal
   */
  public static Result<BigInteger, PoseidonMerkleError> treeRoot(int depth, List<Map.Entry<BigInteger, BigInteger>> entries) {
    return anyNullEntry(entries) ? Result.err(Wrap.nullArgument("entries")) : Proofs.poseidonTreeRoot(depth, List.copyOf(entries));
  }

  /**
   * The proof of {@code position} in the tree of {@code depth} with {@code entries} set.
   *
   * @param depth the depth
   * @param entries positions and leaves
   * @param position the position
   * @return the proof, or the refusal
   */
  public static Result<MerkleProof, PoseidonMerkleError> treeProof(int depth, List<Map.Entry<BigInteger, BigInteger>> entries, BigInteger position) {
    return anyNullEntry(entries) || position == null
      ? Result.err(Wrap.nullArgument("entries and a position"))
      : Proofs.poseidonTreeProof(depth, List.copyOf(entries), position);
  }

  /**
   * A proof's canonical form ({@code shared/schemas/poseidon.cddl}).
   *
   * @param proof the proof
   * @return the value, or the refusal
   */
  public static Result<Value, PoseidonMerkleError> encode(MerkleProof proof) {
    return malformed(proof) ? Result.err(Wrap.nullArgument("a proof")) : Proofs.poseidonToValue(proof).map(Wrap::of);
  }

  /**
   * A proof from its canonical form.
   *
   * @param value the value
   * @return the proof, or the refusal
   */
  public static Result<MerkleProof, PoseidonMerkleError> decode(Value value) {
    return value == null ? Result.err(Wrap.nullArgument("a value")) : Proofs.poseidonFromValue(Wrap.scala(value));
  }
}
