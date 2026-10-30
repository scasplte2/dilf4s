package xyz.kd5ujc.crypto.sigma

import java.nio.charset.StandardCharsets.US_ASCII

import cats.Functor
import cats.effect.std.SecureRandom
import cats.syntax.all._

import xyz.kd5ujc.core.hash.{Blake2b256, Hasher}

import scodec.bits.ByteVector

/**
 * Proves a [[SigmaProp]] from secrets for some of its leaves (Ergo's prover steps; docs/specs/sigma.md):
 *
 *   1. mark each node real if the secrets satisfy it; keep exactly one real child under a real OR and exactly `k`
 *      under a real threshold, so every other subtree is simulated;
 *   2. give simulated subtrees random challenges (split down by XOR or by a random polynomial) and random responses,
 *      deriving their commitments backwards; give each real leaf a random nonce `r` and commitment `r·G`;
 *   3. hash the tree with every commitment into the root challenge, then push it down the real nodes: through an OR by
 *      XOR with the simulated siblings, through a threshold by the polynomial interpolating them;
 *   4. answer at each real leaf `z = r + e·w mod q`.
 *
 * Simulated and real transcripts are identically distributed, so the proof does not reveal which branches were real.
 * All randomness is derived from a fresh seed together with the secrets, the proposition and the message: a nonce
 * never repeats across messages even if a seed did, and nothing is predictable from public data.
 */
object SigmaProver {

  import SigmaProp._

  private val q = Secp256k1Group.Order
  private val hasher = Hasher[Blake2b256]

  /** Proves `prop` for `message` with the given secret scalars; fails when they do not satisfy the proposition. */
  def prove[F[_]: Functor](prop: SigmaProp, secrets: Vector[BigInt], message: ByteVector)(
    using R: SecureRandom[F]
  ): F[Either[String, ByteVector]] =
    R.nextBytes(32).map(seed => proveWith(prop, secrets, message, ByteVector.view(seed)))

  // ---- randomness: a hash of the seed, the secrets, the statement, the message, and the node and purpose

  private final case class Coins(key: ByteVector) {
    private def bytes(path: Vector[Int], purpose: Int, block: Int): ByteVector =
      hasher.digest(key ++ ByteVector(purpose, block) ++ path.foldLeft(ByteVector.empty)((b, i) => b ++ ByteVector.fromInt(i))).bytes

    /** A uniform nonzero scalar: 64 bytes reduced mod q. */
    def scalar(path: Vector[Int], purpose: Int): BigInt =
      BigInt(1, (bytes(path, purpose, 0) ++ bytes(path, purpose, 1)).toArray).mod(q - 1) + 1

    def challenge(path: Vector[Int], purpose: Int): ByteVector = bytes(path, purpose, 0).take(Sigma.ChallengeSize.toLong)
  }

  private val Nonce = 0
  private val Response = 1
  private val ChildChallenge = 2
  private val Coefficient = 3

  // ---- step 1: which nodes are real

  private enum Plan {
    case Leaf(prop: Dlog | DhTuple, path: Vector[Int], witness: Option[BigInt])
    case Node(prop: SigmaProp, path: Vector[Int], satisfied: Boolean, children: Vector[Plan])

    def real: Boolean = this match {
      case Leaf(_, _, w)    => w.isDefined
      case Node(_, _, r, _) => r
    }
  }

  private def plan(prop: SigmaProp, path: Vector[Int], witness: (Dlog | DhTuple) => Option[BigInt]): Plan =
    prop match {
      case leaf: (Dlog | DhTuple) => Plan.Leaf(leaf, path, witness(leaf))
      case node: Conjecture =>
        val cs = node.children.zipWithIndex.map { case (c, i) => plan(c, path :+ i, witness) }
        val real = node match {
          case And(_)          => cs.forall(_.real)
          case Or(_)           => cs.exists(_.real)
          case Threshold(k, _) => cs.count(_.real) >= k
        }
        Plan.Node(prop, path, real, cs)
    }

  /** Step 1, top-down: under a real OR keep one real child, under a real threshold `k`; everything else simulates. */
  private def prune(p: Plan): Plan =
    p match {
      case leaf: Plan.Leaf                  => leaf
      case Plan.Node(prop, path, false, cs) => Plan.Node(prop, path, false, cs.map(simulated))
      case Plan.Node(prop, path, true, cs) =>
        val keep = prop match {
          case Or(_)           => 1
          case Threshold(k, _) => k
          case _               => cs.size
        }
        val kept = cs.zipWithIndex.filter(_._1.real).map(_._2).take(keep).toSet
        Plan.Node(prop, path, true, cs.zipWithIndex.map { case (c, i) => if (kept.contains(i)) prune(c) else simulated(c) })
    }

  private def simulated(p: Plan): Plan =
    p match {
      case Plan.Leaf(prop, path, _)     => Plan.Leaf(prop, path, scala.None)
      case Plan.Node(prop, path, _, cs) => Plan.Node(prop, path, false, cs.map(simulated))
    }

  // ---- step 2: simulated transcripts and real commitments

  private enum Draft {
    case RealLeaf(prop: Dlog | DhTuple, w: BigInt, r: BigInt, commitment: ByteVector)
    case SimLeaf(prop: Dlog | DhTuple, e: ByteVector, z: BigInt, commitment: ByteVector)
    case RealNode(prop: SigmaProp, children: Vector[Draft])
    case SimNode(prop: SigmaProp, e: ByteVector, poly: Option[Gf2Poly], children: Vector[Draft])

    def challenge: Option[ByteVector] = this match {
      case SimLeaf(_, e, _, _)                   => Option(e)
      case SimNode(_, e, _, _)                   => Option(e)
      case RealLeaf(_, _, _, _) | RealNode(_, _) => scala.None
    }
  }

  private def real(p: Plan, coins: Coins): Draft =
    p match {
      case Plan.Leaf(leaf, path, scala.Some(w)) =>
        val r = coins.scalar(path, Nonce)
        val a = leaf match {
          case Dlog(_)             => Secp256k1Group.base(r).bytes
          case DhTuple(g, h, _, _) => (g * r).bytes ++ (h * r).bytes
        }
        Draft.RealLeaf(leaf, w, r, a)
      // not reached: only real nodes come here, and a leaf is real exactly when it has a witness
      case unproven @ Plan.Leaf(_, path, scala.None) => simulate(unproven, coins.challenge(path, ChildChallenge), coins)
      case Plan.Node(prop, _, _, cs)                 =>
        // a simulated child of a real node gets a fresh random challenge here
        Draft.RealNode(prop, cs.map(c => if (c.real) real(c, coins) else simulate(c, coins.challenge(pathOf(c), ChildChallenge), coins)))
    }

  private def pathOf(p: Plan): Vector[Int] = p match {
    case Plan.Leaf(_, path, _)    => path
    case Plan.Node(_, path, _, _) => path
  }

  /** A simulated subtree whose challenge is `e`. */
  private def simulate(p: Plan, e: ByteVector, coins: Coins): Draft =
    p match {
      case Plan.Leaf(leaf, path, _) =>
        val z = coins.scalar(path, Response)
        Draft.SimLeaf(leaf, e, z, Sigma.commitment(leaf, e, z))
      case Plan.Node(prop, path, _, cs) =>
        prop match {
          case Or(_) =>
            val firsts = cs.dropRight(1).map(c => coins.challenge(pathOf(c), ChildChallenge))
            val last = firsts.foldLeft(e)(_ ^ _)
            val es = firsts :+ last
            Draft.SimNode(prop, e, scala.None, cs.zip(es).map { case (c, ce) => simulate(c, ce, coins) })
          case Threshold(k, _) =>
            val e0 = Gf2_192.fromBytes(e).getOrElse(Gf2_192.Zero)
            val higher =
              (1 to cs.size - k).toVector.map(d => Gf2_192.fromBytes(coins.challenge(path :+ d, Coefficient)).getOrElse(Gf2_192.Zero))
            val poly = Gf2Poly(e0 +: higher)
            Draft.SimNode(prop, e, Option(poly), cs.zipWithIndex.map { case (c, i) => simulate(c, poly(i + 1).bytes, coins) })
          case _ => Draft.SimNode(prop, e, scala.None, cs.map(simulate(_, e, coins)))
        }
    }

  // ---- step 3 and 4: the root challenge, real challenges top-down, responses

  private enum Proved {
    case Leaf(e: ByteVector, z: BigInt)
    case Node(prop: SigmaProp, e: ByteVector, poly: Option[Gf2Poly], children: Vector[Proved])

    def challenge: ByteVector = this match {
      case Leaf(e, _)       => e
      case Node(_, e, _, _) => e
    }
  }

  private def commitments(d: Draft): Vector[ByteVector] =
    d match {
      case Draft.RealLeaf(_, _, _, a) => Vector(a)
      case Draft.SimLeaf(_, _, _, a)  => Vector(a)
      case Draft.RealNode(_, cs)      => cs.flatMap(commitments)
      case Draft.SimNode(_, _, _, cs) => cs.flatMap(commitments)
    }

  private def settle(d: Draft, e: ByteVector): Proved =
    d match {
      case Draft.SimLeaf(_, se, z, _)        => Proved.Leaf(se, z)
      case Draft.SimNode(prop, se, poly, cs) => Proved.Node(prop, se, poly, cs.map(c => settle(c, c.challenge.getOrElse(se))))
      case Draft.RealLeaf(_, w, r, _)        => Proved.Leaf(e, (r + BigInt(1, e.toArray) * w).mod(q))
      case Draft.RealNode(prop, cs) =>
        prop match {
          case Or(_) =>
            val others = cs.flatMap(_.challenge.toList).foldLeft(e)(_ ^ _)
            Proved.Node(prop, e, scala.None, cs.map(c => settle(c, c.challenge.getOrElse(others))))
          case Threshold(_, _) =>
            val points = cs.zipWithIndex.flatMap { case (c, i) => c.challenge.flatMap(Gf2_192.fromBytes).map(i + 1 -> _).toList }
            val poly = Gf2Poly.interpolate(Gf2_192.fromBytes(e).getOrElse(Gf2_192.Zero), points)
            Proved.Node(prop, e, Option(poly), cs.zipWithIndex.map { case (c, i) => settle(c, poly(i + 1).bytes) })
          case _ => Proved.Node(prop, e, scala.None, cs.map(settle(_, e)))
        }
    }

  /** Ergo's proof layout ([[Sigma]]): what the verifier cannot compute, in pre-order. */
  private def body(p: Proved, withChallenge: Boolean): ByteVector = {
    val own = if (withChallenge) p.challenge else ByteVector.empty
    p match {
      case Proved.Leaf(_, z) => own ++ Secp256k1Group.scalarBytes(z)
      case Proved.Node(Or(_), _, _, cs) =>
        own ++ cs.dropRight(1).foldLeft(ByteVector.empty)(_ ++ body(_, withChallenge = true)) ++ cs.lastOption.fold(ByteVector.empty)(
          body(_, withChallenge = false)
        )
      case Proved.Node(_, _, poly, cs) =>
        own ++ poly.fold(ByteVector.empty)(_.higherBytes) ++ cs.foldLeft(ByteVector.empty)(_ ++ body(_, withChallenge = false))
    }
  }

  /** The fixed-seed prover: for tests and vectors only; production draws the seed ([[prove]]). */
  private[crypto] def proveWith(
    prop:    SigmaProp,
    secrets: Vector[BigInt],
    message: ByteVector,
    seed:    ByteVector
  ): Either[String, ByteVector] = {
    val usable = secrets.filter(w => w > 0 && w < q)
    def witness(leaf: Dlog | DhTuple): Option[BigInt] = leaf match {
      case Dlog(pk)            => usable.find(w => Secp256k1Group.base(w) == pk)
      case DhTuple(g, h, u, v) => usable.find(w => g * w == u && h * w == v)
    }
    val planned = prune(plan(prop, Vector.empty, witness))
    val secretBytes = usable.foldLeft(ByteVector.empty)(_ ++ Secp256k1Group.scalarBytes(_))
    val coins = Coins(
      hasher
        .digest(
          ByteVector.view("dilf4s/sigma/prover/v1".getBytes(US_ASCII)) ++ seed ++ secretBytes ++ Sigma.statementBytes(prop) ++ message
        )
        .bytes
    )
    Either.cond(planned.real, (), "the secrets do not satisfy the proposition").map { _ =>
      val draft = real(planned, coins)
      val e0 = Sigma.challenge(Sigma.fiatShamir(prop, commitments(draft)), message)
      body(settle(draft, e0), withChallenge = true)
    }
  }
}
