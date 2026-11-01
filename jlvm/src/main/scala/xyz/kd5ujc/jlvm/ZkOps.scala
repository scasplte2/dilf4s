package xyz.kd5ujc.jlvm

import cats.syntax.all._

import xyz.kd5ujc.accumulators.poseidon.{PoseidonMerkleProof, PoseidonMerkleTree}
import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.hash.{Fr, Poseidon => PoseidonHash}
import xyz.kd5ujc.crypto.vrf.{EcVrfTai, Vrf, VrfError}
import xyz.kd5ujc.crypto.zk.{Bn254, Bn254Engine, Groth16Error, Sp1Groth16}

import scodec.bits.ByteVector

/**
 * The zero-knowledge and curve operations (docs/specs/jlvm.md §6), over the dilf4s backends. Arguments and results follow
 * metakit's `CryptoOps`, with its `0x`-hex strings read as byte strings: field elements and scalars are 32 bytes
 * big-endian, a G1 point is 64 bytes `x || y`, a G2 point is 128 bytes in EIP-197 order (imaginary part first), and the
 * all-zero encoding is the point at infinity.
 *
 * A value of the wrong kind is a [[JlvmError.Type]]; a wrong argument count an [[JlvmError.Arity]]; bytes that do not
 * encode the expected object (wrong width, a non-canonical field element, a point off the curve or outside the subgroup)
 * a [[JlvmError.Crypto]]. A well-formed proof that does not verify is `false`, never an error.
 */
final class ZkOps(engine: Bn254Engine) {

  import ZkOps._

  type Result = Either[JlvmError, CanonicalValue]

  /** None for operations this object does not implement. */
  def apply(op: Op, args: Vector[CanonicalValue]): Option[Result] =
    op match {
      case Op.Poseidon      => Option(poseidon(op, args))
      case Op.PmtVerify     => Option(pmtVerify(op, args))
      case Op.Groth16Verify => Option(groth16Verify(op, args))
      case Op.EcvrfVerify   => Option(ecvrfVerify(op, args))
      case Op.Bn254Add      => Option(bn254Add(op, args))
      case Op.Bn254Mul      => Option(bn254Mul(op, args))
      case Op.Bn254Pairing  => Option(bn254Pairing(op, args))
      case _                => Option.empty
    }

  // ---- poseidon [x1, .., xn] or [[x1, .., xn]], 1 <= n <= 4 -> 32 bytes

  private def poseidon(op: Op, args: Vector[CanonicalValue]): Result = {
    val inputs = args match {
      case Vector(CArray(vs)) if vs.nonEmpty => vs
      case _                                 => args
    }
    for {
      _ <- Either.cond(
        inputs.nonEmpty && inputs.size <= PoseidonHash.MaxInputs,
        (),
        JlvmError.Arity(op.wire, s"takes 1 to ${PoseidonHash.MaxInputs} field elements, got ${inputs.size}")
      )
      xs     <- inputs.zipWithIndex.traverse { case (v, i) => fr(op, s"input[$i]", v) }
      digest <- PoseidonHash.hash(xs).leftMap(e => JlvmError.Crypto(op.wire, e.toString))
      out    <- Values.bytesValue(op, Bn254.be32(digest.value))
    } yield out
  }

  // ---- pmt_verify [root, leaf, index, [siblings, root-first]] -> bool

  private def pmtVerify(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case Vector(rootV, leafV, indexV, siblingsV) =>
        for {
          root  <- fr(op, "root", rootV)
          leaf  <- fr(op, "leaf", leafV)
          index <- integer(op, "index", indexV)
          raw   <- array(op, "siblings", siblingsV)
          _     <- Either.cond(raw.size <= MaxSiblings, (), crypto(op, s"at most $MaxSiblings siblings, got ${raw.size}"))
          sibs  <- raw.zipWithIndex.traverse { case (v, i) => fr(op, s"sibling[$i]", v) }
          // the index is charged as one element, whatever its size: the error never renders it
          proof <- PoseidonMerkleProof.from(index, sibs).leftMap(_ => crypto(op, s"index is out of range for depth ${sibs.size}"))
        } yield Values.bool(PoseidonMerkleTree.verifyInclusion(leaf, proof, root))
      case _ => JlvmError.Arity(op.wire, s"takes [root, leaf, index, siblings], got ${args.size} arguments").asLeft
    }

  // ---- groth16_verify [programVKey (32), publicValues, proof (SP1 Groth16 v6.1.0)] -> bool

  private def groth16Verify(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case Vector(vkeyV, publicV, proofV) =>
        for {
          vkey    <- sized(op, "vkey", vkeyV, FieldBytes)
          pubVals <- bytes(op, "publicValues", publicV)
          proof   <- bytes(op, "proof", proofV)
          valid <- Sp1Groth16.verify(engine)(vkey, pubVals, proof) match {
            case Right(())                            => true.asRight
            case Left(Groth16Error.Malformed(reason)) => crypto(op, reason).asLeft
            case Left(Groth16Error.InvalidProof(_) | Groth16Error.PairingCheckFailed | Groth16Error.Rejected(_)) => false.asRight
          }
        } yield Values.bool(valid)
      case _ => JlvmError.Arity(op.wire, s"takes [vkey, publicValues, proof], got ${args.size} arguments").asLeft
    }

  // ---- ecvrf_verify [pk (32), alpha, proof (80)] -> {valid: true, beta: 64 bytes} | {valid: false}

  private def ecvrfVerify(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case Vector(pkV, alphaV, proofV) =>
        for {
          pk    <- sized(op, "pk", pkV, VrfKeyBytes)
          alpha <- bytes(op, "alpha", alphaV)
          proof <- sized(op, "proof", proofV, Vrf.ProofLength)
          out <- Vrf[EcVrfTai].verifyBytes(pk, alpha, proof) match {
            case Right(beta)                            => Values.map(op, List("valid" -> True, "beta" -> CBytes(beta.bytes)))
            case Left(VrfError.Malformed(what, reason)) => crypto(op, s"$what: $reason").asLeft
            case Left(
                  VrfError.InvalidPublicKey | VrfError.SmallOrderPublicKey | VrfError.InvalidGamma | VrfError.NonCanonicalScalar |
                  VrfError.ChallengeMismatch | VrfError.KeyMismatch | VrfError.HashToCurveFailed(_)
                ) =>
              Values.map(op, List("valid" -> False))
          }
        } yield out
      case _ => JlvmError.Arity(op.wire, s"takes [pk, alpha, proof], got ${args.size} arguments").asLeft
    }

  // ---- bn254_add [a (64), b (64)] -> 64; bn254_mul [p (64), scalar (32)] -> 64 (EIP-196)

  private def bn254Add(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case Vector(aV, bV) =>
        for {
          a   <- sized(op, "a", aV, G1Bytes)
          b   <- sized(op, "b", bV, G1Bytes)
          sum <- engine.add(a, b).leftMap(zkError(op, "a + b"))
          out <- Values.bytesValue(op, sum)
        } yield out
      case _ => JlvmError.Arity(op.wire, s"takes [a, b], got ${args.size} arguments").asLeft
    }

  /** Any 256-bit scalar; G1 has prime order r, so reducing it mod r first gives the same point. */
  private def bn254Mul(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case Vector(pV, sV) =>
        for {
          p   <- sized(op, "point", pV, G1Bytes)
          s   <- sized(op, "scalar", sV, FieldBytes)
          q   <- engine.mul(p, s).leftMap(zkError(op, "point"))
          out <- Values.bytesValue(op, q)
        } yield out
      case _ => JlvmError.Arity(op.wire, s"takes [point, scalar], got ${args.size} arguments").asLeft
    }

  // ---- bn254_pairing [[g1, g2], ..] or [[[g1, g2], ..]] -> bool (EIP-197; no pairs is true)

  /** A single argument whose elements are all arrays is the list of pairs (a pair holds bytes, never arrays). */
  private def bn254Pairing(op: Op, args: Vector[CanonicalValue]): Result = {
    val pairs = args match {
      case Vector(CArray(vs)) if vs.forall(isArray) => vs
      case _                                        => args
    }
    for {
      _  <- Either.cond(pairs.size <= MaxPairs, (), crypto(op, s"at most $MaxPairs pairs, got ${pairs.size}"))
      ps <- pairs.zipWithIndex.traverse { case (v, i) => pair(op, i, v) }
      ok <- engine.pairing(ps).leftMap(zkError(op, "a pair"))
    } yield Values.bool(ok)
  }

  private def pair(op: Op, i: Int, v: CanonicalValue): Either[JlvmError, (ByteVector, ByteVector)] =
    v match {
      case CArray(Vector(a, b)) => (sized(op, s"pair[$i].g1", a, G1Bytes), sized(op, s"pair[$i].g2", b, G2Bytes)).tupled
      case CArray(vs)           => crypto(op, s"pair[$i]: expected [g1, g2], got ${vs.size} elements").asLeft
      case other                => JlvmError.Type(op.wire, s"pair[$i]: expected an array, got ${Values.kind(other)}").asLeft
    }

  // ---- argument decoding

  private def crypto(op: Op, message: String): JlvmError = JlvmError.Crypto(op.wire, message)

  private def isArray(v: CanonicalValue): Boolean = v match {
    case CArray(_) => true
    case _         => false
  }

  private def bytes(op: Op, role: String, v: CanonicalValue): Either[JlvmError, ByteVector] =
    v match {
      case CBytes(b) => b.asRight
      case other     => JlvmError.Type(op.wire, s"$role: expected bytes, got ${Values.kind(other)}").asLeft
    }

  private def sized(op: Op, role: String, v: CanonicalValue, width: Long): Either[JlvmError, ByteVector] =
    bytes(op, role, v).flatMap(b => Either.cond(b.size == width, b, crypto(op, s"$role: expected $width bytes, got ${b.size}")))

  private def integer(op: Op, role: String, v: CanonicalValue): Either[JlvmError, BigInt] =
    v match {
      case CInt(i) => i.asRight
      case other   => JlvmError.Type(op.wire, s"$role: expected an integer, got ${Values.kind(other)}").asLeft
    }

  private def array(op: Op, role: String, v: CanonicalValue): Either[JlvmError, Vector[CanonicalValue]] =
    v match {
      case CArray(vs) => vs.asRight
      case other      => JlvmError.Type(op.wire, s"$role: expected an array, got ${Values.kind(other)}").asLeft
    }

  /** 32 bytes, big-endian, below the BN254 scalar field modulus r. */
  private def fr(op: Op, role: String, v: CanonicalValue): Either[JlvmError, Fr] =
    sized(op, role, v, FieldBytes).flatMap(b =>
      Fr.from(BigInt(1, b.toArray)).leftMap(_ => crypto(op, s"$role: not a canonical BN254 scalar field element (must be < r)"))
    )

  /** For the curve operations a point off the curve or outside the subgroup is malformed input, as in metakit. */
  private def zkError(op: Op, role: String)(e: Bn254.ZkError): JlvmError = e match {
    case Bn254.ZkError.Malformed(reason)    => crypto(op, s"$role: $reason")
    case Bn254.ZkError.InvalidPoint(reason) => crypto(op, s"$role: $reason")
  }
}

object ZkOps {

  /** The most pairs `bn254_pairing` accepts (metakit has no bound; gas already grows with the count). */
  val MaxPairs: Int = 64

  /** The most siblings `pmt_verify` accepts: the deepest Poseidon Merkle tree. */
  val MaxSiblings: Int = PoseidonMerkleTree.MaxDepth

  private val FieldBytes: Long = 32L
  private val VrfKeyBytes: Long = 32L
  private val G1Bytes: Long = 64L
  private val G2Bytes: Long = 128L

  /** On the pure-Scala engine: the reference, for tests. */
  val reference: ZkOps = new ZkOps(Bn254Engine.reference)
}
