package xyz.kd5ujc.crypto.sigma

import cats.data.StateT
import cats.syntax.all._

import xyz.kd5ujc.core.hash.{Blake2b256, Hasher}

import scodec.bits.ByteVector

/**
 * Non-interactive sigma proofs over [[SigmaProp]], byte-compatible with Ergo (docs/specs/sigma.md).
 *
 *   - A challenge is 24 bytes: Blake2b-256 of the Fiat-Shamir tree bytes and the message, truncated.
 *   - A proof holds the root challenge, then for each node in pre-order what the verifier cannot compute: an OR's
 *     children's challenges but the last (the last is the XOR), a threshold's polynomial coefficients from degree 1
 *     (the children's challenges are its values at 1..n), and each leaf's 32-byte response `z`.
 *   - The verifier recomputes every commitment from its challenge and response, serializes the tree with them, and
 *     accepts iff the hash equals the root challenge.
 *
 * Stricter than Ergo: a proof of the wrong length (Ergo ignores trailing bytes and warns on short ones) and a response
 * `z >= q` (Ergo reduces it) are malformed. Malformed input is `Left`; a well-formed proof that fails is `Right(false)`.
 */
object Sigma {

  import SigmaProp._

  /** Challenge bytes: 192-bit soundness. */
  val ChallengeSize: Int = 24

  private val hasher = Hasher[Blake2b256]

  def challenge(fiatShamirBytes: ByteVector, message: ByteVector): ByteVector =
    hasher.digest(fiatShamirBytes ++ message).bytes.take(ChallengeSize.toLong)

  /** The exact length of every proof of `prop`. */
  def proofLength(prop: SigmaProp): Long = ChallengeSize.toLong + body(prop)

  /** Bytes after a node's own challenge (which its parent may compute instead). */
  private def body(prop: SigmaProp): Long =
    prop match {
      case Dlog(_) | DhTuple(_, _, _, _) => Secp256k1Group.ScalarSize.toLong
      case And(cs)                       => cs.foldLeft(0L)(_ + body(_))
      case Or(cs)                        => cs.foldLeft(0L)(_ + body(_)) + ChallengeSize.toLong * (cs.size - 1)
      case Threshold(k, cs)              => cs.foldLeft(0L)(_ + body(_)) + ChallengeSize.toLong * (cs.size - k)
    }

  // ---- the checked tree: every node with its challenge, every leaf with its response

  private[sigma] enum Checked {
    case Leaf(prop: Dlog | DhTuple, challenge: ByteVector, z: BigInt)
    case Node(prop: SigmaProp, challenge: ByteVector, children: Vector[Checked])
  }

  private type Read[A] = StateT[Either[String, *], ByteVector, A]

  private def take(n: Long, what: String): Read[ByteVector] =
    StateT { bytes =>
      Either.cond(bytes.size >= n, (bytes.drop(n), bytes.take(n)), s"the proof ends inside $what")
    }

  /** Verifier steps 2 and 3: challenges top-down, read or computed; responses at the leaves. */
  private def parse(prop: SigmaProp, known: Option[ByteVector]): Read[Checked] =
    known.fold(take(ChallengeSize.toLong, "a challenge"))(_.pure[Read]).flatMap { e =>
      prop match {
        case leaf: (Dlog | DhTuple) =>
          take(Secp256k1Group.ScalarSize.toLong, "a response").flatMapF { zb =>
            val z = BigInt(1, zb.toArray)
            Either.cond(z < Secp256k1Group.Order, Checked.Leaf(leaf, e, z), "a response must be below the group order")
          }
        case And(cs) => cs.traverse(parse(_, Option(e))).map(Checked.Node(prop, e, _))
        case Or(cs) =>
          cs.dropRight(1).traverse(parse(_, scala.None)).flatMap { simulated =>
            val last = simulated.foldLeft(e)((x, c) => x ^ challengeOf(c))
            cs.lastOption.traverse(parse(_, Option(last))).map(l => Checked.Node(prop, e, simulated ++ l.toVector))
          }
        case Threshold(k, cs) =>
          take(ChallengeSize.toLong * (cs.size - k), "a threshold polynomial").flatMapF { coefficients =>
            (Gf2_192.fromBytes(e), Option(coefficients))
              .flatMapN(Gf2Poly.fromBytes)
              .toRight("a malformed threshold polynomial")
          }.flatMap { q =>
            cs.zipWithIndex.traverse { case (c, i) => parse(c, Option(q(i + 1).bytes)) }.map(Checked.Node(prop, e, _))
          }
      }
    }

  private def challengeOf(c: Checked): ByteVector =
    c match {
      case Checked.Leaf(_, e, _) => e
      case Checked.Node(_, e, _) => e
    }

  /** Verifier step 4: a leaf's commitment from its challenge `e` and response `z`. */
  private[sigma] def commitment(leaf: Dlog | DhTuple, e: ByteVector, z: BigInt): ByteVector = {
    val c = BigInt(1, e.toArray)
    leaf match {
      case Dlog(pk)            => (Secp256k1Group.base(z) + -(pk * c)).bytes
      case DhTuple(g, h, u, v) => (g * z + -(u * c)).bytes ++ (h * z + -(v * c)).bytes
    }
  }

  /** Verifier step 5: the Fiat-Shamir bytes, from each leaf's statement and commitment. */
  private[sigma] def fiatShamir(tree: SigmaProp, commitments: Vector[ByteVector]): ByteVector = {
    def short(n: Long): ByteVector = ByteVector.fromShort(n.toShort)
    def go(p: SigmaProp, cs: Vector[ByteVector]): (ByteVector, Vector[ByteVector]) =
      p match {
        case leaf: (Dlog | DhTuple) =>
          val s = SigmaProp.statement(leaf)
          val a = cs.headOption.getOrElse(ByteVector.empty)
          (ByteVector(0x01) ++ short(s.size) ++ s ++ short(a.size) ++ a, cs.drop(1))
        case node: Conjecture =>
          val header = node match {
            case Threshold(k, children) => ByteVector(0x00, 0x02, k.toByte) ++ short(children.size.toLong)
            case Or(children)           => ByteVector(0x00, 0x01) ++ short(children.size.toLong)
            case other                  => ByteVector(0x00, 0x00) ++ short(other.children.size.toLong)
          }
          node.children.foldLeft((header, cs)) {
            case ((acc, rest), child) =>
              val (bytes, after) = go(child, rest)
              (acc ++ bytes, after)
          }
      }
    go(tree, commitments)._1
  }

  private def leaves(c: Checked): Vector[Checked.Leaf] =
    c match {
      case l: Checked.Leaf        => Vector(l)
      case Checked.Node(_, _, cs) => cs.flatMap(leaves)
    }

  /**
   * Whether `proof` proves `prop` for `message`. The message must bind what the proof authorises (the transaction or
   * update), or the proof can be replayed.
   */
  def verify(prop: SigmaProp, proof: ByteVector, message: ByteVector): Either[String, Boolean] =
    for {
      _ <- Either.cond(proof.size == proofLength(prop), (), s"a proof of this proposition is ${proofLength(prop)} bytes, got ${proof.size}")
      checked <- parse(prop, scala.None).runA(proof)
      commitments = leaves(checked).map(l => commitment(l.prop, l.challenge, l.z))
    } yield challenge(fiatShamir(prop, commitments), message) == challengeOf(checked)
}
