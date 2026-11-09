package xyz.kd5ujc.accumulators.trie

import cats.syntax.all._
import cats.{Eval, Monad}

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

import xyz.kd5ujc.accumulators.trie.Node.{Branch, Leaf}
import xyz.kd5ujc.accumulators.{ProofInput, ValueCommitment}
import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.hash.{Digest, Hasher}
import xyz.kd5ujc.models.{trie => W}

import scodec.bits.{BitVector, ByteVector}

/** A node of a partial trie (docs/specs/trie.md §4). */
sealed trait ProofNode[H] extends Product with Serializable

object ProofNode {
  final case class Stub[H](hash: Digest[H]) extends ProofNode[H]
  final case class PLeaf[H](segment: BitVector, value: Digest[H]) extends ProofNode[H]
  final case class PBranch[H](segment: BitVector, value: Option[Digest[H]], left: Option[ProofNode[H]], right: Option[ProofNode[H]])
      extends ProofNode[H]
}

/** A partial trie; `None` is the proof of the empty trie. */
final case class TrieProof[H](top: Option[ProofNode[H]])

sealed trait Lookup[+H] extends Product with Serializable

object Lookup {
  final case class Present[H](valueHash: Digest[H]) extends Lookup[H]
  case object Absent extends Lookup[Nothing]

  /** The proof does not reach this key: neither presence nor absence is established. */
  case object NotCovered extends Lookup[Nothing]
}

/** What a partial trie proves, once its root has been checked. */
final class VerifiedProof[H] private[trie] (top: Option[ProofNode[H]]) {

  import ProofNode._

  def lookup(path: BitVector): Lookup[H] = {
    @tailrec
    def go(node: ProofNode[H], remaining: BitVector): Lookup[H] =
      node match {
        case Stub(_)           => Lookup.NotCovered
        case PLeaf(segment, v) => if (segment == remaining) Lookup.Present(v) else Lookup.Absent
        case PBranch(segment, value, left, right) =>
          if (!remaining.startsWith(segment)) Lookup.Absent
          else {
            val rest = remaining.drop(segment.size)
            if (rest.isEmpty) value.fold[Lookup[H]](Lookup.Absent)(Lookup.Present(_))
            else
              ((if (rest.head) right else left): Option[ProofNode[H]]) match {
                case None        => Lookup.Absent
                case Some(child) => go(child, rest.tail)
              }
          }
      }
    top.fold[Lookup[H]](Lookup.Absent)(go(_, path))
  }

  private type Entry = (BitVector, Digest[H])

  /** Paths as pieces, newest first: O(1) to extend, and compared with a claimed key piece by piece. */
  private val piece: (List[BitVector], BitVector) => List[BitVector] = (ps, bits) => if (bits.isEmpty) ps else bits :: ps

  /** Paths as one vector, extended by concatenation, which shares the path above with every key below it. */
  private val joined: (BitVector, BitVector) => BitVector = _ ++ _

  /**
   * Folds `visit` over every entry of the subtree at `start` (whose path is `at`) that `bounds` holds, in key order (a
   * branch's own entry, then its left subtree, then its right); `None` if a stub meets the bounds. A cursor carries
   * where its path stands against each bound, so every node compares only the bits it adds: the walk's work, counted
   * with `visit`'s, is linear in the proof's size, however long the keys and bounds (docs/specs/trie.md §4).
   */
  private def walk[P, S](start: ProofNode[H], at: Cursor[P], bounds: Bounds, grow: (P, BitVector) => P, z: S)(
    visit: (S, Cursor[P], Digest[H]) => (S, Long)
  ): Counted[Option[S]] = {
    def within(s: S, c: Cursor[P], v: Digest[H]): (S, Long) = if (bounds.holds(c)) visit(s, c, v) else (s, 0L)
    @tailrec
    def go(todo: List[(ProofNode[H], Cursor[P])], s: S, steps: Long): Counted[Option[S]] =
      todo match {
        case Nil               => Counted(Some(s), steps)
        case (Stub(_), _) :: _ => Counted(None, steps + 1)
        case (PLeaf(segment, v), c) :: rest =>
          val (seg, f) = Bits.flat(segment)
          val (here, n) = bounds.extend(c, seg, grow)
          val (next, m) = within(s, here, v)
          go(rest, next, steps + f + n + m)
        case (PBranch(segment, value, left, right), c) :: rest =>
          val (seg, f) = Bits.flat(segment)
          val (here, n) = bounds.extend(c, seg, grow)
          val (later, k) = List(left.map(false -> _), right.map(true -> _)).flatten.foldRight((rest, 0L)) {
            case ((bit, child), (todo2, j)) =>
              val (below, i) = bounds.extend(here, BitVector.bit(bit), grow)
              (if (bounds.meets(below)) (child, below) :: todo2 else todo2, i + j)
          }
          val (next, m) = value.fold((s, 0L))(within(s, here, _))
          go(later, next, steps + f + n + k + m)
      }
    go(List(start -> at), z, 0L)
  }

  /** The walk of `[from, until)` from the top: `z` without a walk when no key can lie in it, even under a stub. */
  private def ranged[P, S](from: BitVector, until: Option[BitVector], empty: P, grow: (P, BitVector) => P, z: S)(
    visit: (S, Cursor[P], Digest[H]) => (S, Long)
  ): Counted[Option[S]] = {
    val (bounds, copied) = Bounds.readable(from, until)
    top
      .filter(_ => bounds.meets(Cursor.origin(empty)))
      .fold(Counted(Option(z), 1L))(walk(_, Cursor.origin(empty), bounds, grow, z)(visit))
      .add(copied)
  }

  /**
   * The walk of the subtree holding exactly the keys that start with `p`: `z` if there is none, `None` if a stub lies
   * on the way or under it. Each node on the way compares only its own bits with `p`'s.
   */
  private def prefixed[P, S](prefix: BitVector, empty: P, grow: (P, BitVector) => P, z: S)(
    visit: (S, Cursor[P], Digest[H]) => (S, Long)
  ): Counted[Option[S]] = {
    // read from `p`, flat and cut where no path reaches, against the whole prefix's length
    val (p, copied) = Bits.readable(prefix)
    // the subtree's keys all start with `p`'s first `pos` bits: the walk below needs no bounds
    def under(node: ProofNode[H], pos: Long, steps: Long): Counted[Option[S]] = {
      val (at, n) = Bounds.All.extend(Cursor.origin(empty), p.take(pos), grow)
      walk(node, at, Bounds.All, grow, z)(visit).add(steps + n)
    }
    @tailrec
    def locate(node: ProofNode[H], pos: Long, steps: Long): Counted[Option[S]] =
      node match {
        case Stub(_) => Counted(None, steps + 1)
        case PLeaf(segment, _) =>
          val remaining = prefix.size - pos
          val (seg, f) = Bits.flat(segment)
          val same = Bits.agreeing(seg, 0L, p, pos, seg.size.min(remaining))
          if (same == remaining) under(node, pos, steps + f + same + 1) else Counted(Option(z), steps + f + same + 1)
        case PBranch(segment, _, left, right) =>
          val remaining = prefix.size - pos
          val (seg, f) = Bits.flat(segment)
          val same = Bits.agreeing(seg, 0L, p, pos, seg.size.min(remaining))
          if (same == remaining) under(node, pos, steps + f + same + 1)
          else if (same < seg.size) Counted(Option(z), steps + f + same + 1)
          else {
            val at = pos + seg.size
            ((if (p(at)) right else left): Option[ProofNode[H]]) match {
              case None        => Counted(Option(z), steps + f + same + 2)
              case Some(child) => locate(child, at + 1, steps + f + same + 2)
            }
          }
      }
    top.fold(Counted(Option(z), 1L))(locate(_, 0L, copied))
  }

  private def listed(acc: List[Entry], c: Cursor[BitVector], v: Digest[H]): (List[Entry], Long) = ((c.path, v) :: acc, 1L)

  /** A claimed listing matched entry by entry as the walk reaches each key: the entries still due, while all agree. */
  final private case class Matching(due: List[Entry], equal: Boolean) {
    def verdict: Boolean = equal && due.isEmpty
  }

  /** Compares the claim's next entry with the one reached, only while every earlier one matched, never building it. */
  private def matched(m: Matching, c: Cursor[List[BitVector]], v: Digest[H]): (Matching, Long) =
    m.due match {
      case (k, h) :: rest if m.equal =>
        val (same, n) = Bits.spells(c.path, c.depth, k)
        (Matching(rest, same && h == v), n + 1)
      case _ => (Matching(Nil, equal = false), 1L)
    }

  /**
   * Every `(path, valueHash)` with `from <= path < until` (bitwise order, prefixes first), if no stub meets the range;
   * `None` otherwise. A covered listing is complete, including an empty one. Its keys share the paths above them.
   */
  def range(from: BitVector, until: Option[BitVector]): Option[List[(BitVector, Digest[H])]] =
    ranged(from, until, BitVector.empty, joined, List.empty[Entry])(listed).value.map(_.reverse)

  /**
   * Every `(path, valueHash)` under `prefix`, in key order, if the proof covers the whole subtree; `None` if any part
   * of it is a stub. A covered listing is complete: no key under the prefix can be missing.
   */
  def prefix(prefix: BitVector): Option[List[(BitVector, Digest[H])]] =
    prefixed(prefix, BitVector.empty, joined, List.empty[Entry])(listed).value.map(_.reverse)

  // ---- claims, as the JLVM's trie operations check them (docs/specs/jlvm.md §6): not covered is a typed error

  /** Whether `path` holds the value hash `claim` (`None`: is absent); `NotCovered` if a stub lies on the path. */
  def holds(path: BitVector, claim: Option[Digest[H]]): Either[TrieError, Boolean] =
    lookup(path) match {
      case Lookup.Present(h) => claim.contains(h).asRight
      case Lookup.Absent     => claim.isEmpty.asRight
      case Lookup.NotCovered => TrieError.NotCovered("the key").asLeft
    }

  /** Whether `claimed`, in key order, is exactly the listing under `p`; `NotCovered` if a stub lies under it. */
  def listsPrefix(p: BitVector, claimed: List[(BitVector, Digest[H])]): Either[TrieError, Boolean] = checkPrefix(p, claimed).value

  /** Whether `claimed`, in key order, is exactly the listing of `[from, until)`; `NotCovered` if a stub meets it. */
  def listsRange(from: BitVector, until: Option[BitVector], claimed: List[(BitVector, Digest[H])]): Either[TrieError, Boolean] =
    checkRange(from, until, claimed).value

  /**
   * [[listsPrefix]] with its work: the bits compared (with the prefix and with the claim's keys) and the nodes
   * visited, never more than a constant times the proof's bits plus the prefix's and the claim's (docs/specs/trie.md §4).
   */
  private[kd5ujc] def checkPrefix(p: BitVector, claimed: List[(BitVector, Digest[H])]): Counted[Either[TrieError, Boolean]] =
    prefixed(p, List.empty[BitVector], piece, Matching(claimed, equal = true))(matched)
      .map(_.toRight(TrieError.NotCovered("the prefix")).map(_.verdict))

  /** [[listsRange]] with its work, bounded as [[checkPrefix]]'s is, by the proof's bits plus the bounds' and the claim's. */
  private[kd5ujc] def checkRange(
    from:    BitVector,
    until:   Option[BitVector],
    claimed: List[(BitVector, Digest[H])]
  ): Counted[Either[TrieError, Boolean]] =
    ranged(from, until, List.empty[BitVector], piece, Matching(claimed, equal = true))(matched)
      .map(_.toRight(TrieError.NotCovered("the range")).map(_.verdict))
}

/** A result and the work it took, in steps (bits compared, nodes visited), for tests to bound (docs/specs/trie.md §4). */
final private[kd5ujc] case class Counted[A](value: A, steps: Long) {
  def map[B](f: A => B): Counted[B] = Counted(f(value), steps)
  def add(more: Long): Counted[A] = Counted(value, steps + more)
}

object TrieProofs {

  import ProofNode._

  /** A query: a key path, or a prefix whose whole subtree must be expanded. */
  sealed trait Query extends Product with Serializable
  final case class Key(path: BitVector) extends Query
  final case class Prefix(path: BitVector) extends Query

  /** Every key in `[from, until)`, bitwise order with prefixes first; `None` is unbounded above. */
  final case class Range(from: BitVector, until: Option[BitVector]) extends Query

  /**
   * Builds the partial trie answering `queries`: gather the nodes to expand (keyed in `order`, never hashed), then
   * assemble with `Eval`.
   */
  def prove[F[_]: Monad, H](lookup: Digest[H] => F[Node[H]], root: Option[Digest[H]], queries: List[Query])(
    implicit order: Ordering[Digest[H]]
  ): F[TrieProof[H]] =
    root.fold(TrieProof[H](None).pure[F]) { r =>
      expanded(lookup, r, queries).map(nodes => TrieProof(Some(assemble(r, nodes).value)))
    }

  private def assemble[H](hash: Digest[H], nodes: TreeMap[Digest[H], Node[H]]): Eval[ProofNode[H]] =
    nodes.get(hash) match {
      case None             => Eval.now(Stub(hash))
      case Some(l: Leaf[H]) => Eval.now(PLeaf(l.segment, l.value))
      case Some(b: Branch[H]) =>
        (b.left.traverse(h => Eval.defer(assemble(h, nodes))), b.right.traverse(h => Eval.defer(assemble(h, nodes))))
          .mapN(PBranch(b.segment, b.value, _, _))
    }

  /** The nodes on every query path, plus whole subtrees under prefix queries. */
  private def expanded[F[_]: Monad, H](lookup: Digest[H] => F[Node[H]], root: Digest[H], queries: List[Query])(
    implicit order: Ordering[Digest[H]]
  ): F[TreeMap[Digest[H], Node[H]]] =
    queries.foldLeftM(TreeMap.empty[Digest[H], Node[H]](using order)) {
      case (acc, Range(from, until)) =>
        val (bounds, _) = Bounds.readable(from, until)
        if (bounds.meets(Cursor.origin(()))) rangeNodes(lookup, root, bounds, acc) else acc.pure[F]
      case (acc, Key(p))    => pathNodes(lookup, root, p, isPrefix = false, acc)
      case (acc, Prefix(p)) => pathNodes(lookup, root, p, isPrefix = true, acc)
    }

  /**
   * The nodes whose key intervals meet the bounds, gathered by a work list from the root. Each entry carries where its
   * path stands against the bounds, so a node compares only its own bits with them.
   */
  private def rangeNodes[F[_]: Monad, H](
    lookup: Digest[H] => F[Node[H]],
    root:   Digest[H],
    bounds: Bounds,
    seen:   TreeMap[Digest[H], Node[H]]
  ): F[TreeMap[Digest[H], Node[H]]] = {
    val unit: (Unit, BitVector) => Unit = (_, _) => ()
    (List((root, Cursor.origin(()))), seen).tailRecM {
      case (Nil, acc) => acc.asRight[(List[(Digest[H], Cursor[Unit])], TreeMap[Digest[H], Node[H]])].pure[F]
      case ((h, at) :: rest, acc) =>
        lookup(h).map {
          case l: Leaf[H] => (rest, acc.updated(h, l)).asLeft
          case b: Branch[H] =>
            val (here, _) = bounds.extend(at, Bits.flat(b.segment)._1, unit)
            val next = List(b.left.map(false -> _), b.right.map(true -> _)).flatten.flatMap {
              case (bit, child) =>
                val (below, _) = bounds.extend(here, BitVector.bit(bit), unit)
                Option.when(bounds.meets(below))(child -> below)
            }
            (next ++ rest, acc.updated(h, b)).asLeft
        }
    }
  }

  /** The nodes on one key's path, plus the whole subtree under a prefix. */
  private def pathNodes[F[_]: Monad, H](
    lookup:   Digest[H] => F[Node[H]],
    root:     Digest[H],
    path:     BitVector,
    isPrefix: Boolean,
    acc:      TreeMap[Digest[H], Node[H]]
  ): F[TreeMap[Digest[H], Node[H]]] =
    (root, path, acc).tailRecM {
      case (hash, remaining, seen) =>
        lookup(hash).flatMap {
          case l: Leaf[H] =>
            seen.updated(hash, l).asRight[(Digest[H], BitVector, TreeMap[Digest[H], Node[H]])].pure[F]
          case b: Branch[H] =>
            val withThis = seen.updated(hash, b)
            if (isPrefix && b.segment.startsWith(remaining))
              TrieEngine.collect(lookup, List(hash))(using Monad[F], seen.ordering).map(sub => (withThis ++ sub).asRight)
            else {
              val rest = remaining.drop(b.segment.size)
              if (!remaining.startsWith(b.segment) || rest.isEmpty)
                withThis.asRight[(Digest[H], BitVector, TreeMap[Digest[H], Node[H]])].pure[F]
              else
                b.child(rest.head)
                  .fold(withThis.asRight[(Digest[H], BitVector, TreeMap[Digest[H], Node[H]])].pure[F])(c =>
                    (c, rest.tail, withThis).asLeft[TreeMap[Digest[H], Node[H]]].pure[F]
                  )
            }
        }
    }

  // ---- the JLVM's trie operations over caller values (docs/specs/jlvm.md §6), as the SDKs' trie_verify and
  // trie_prefix_verify: claims are capped by weight before any is hashed and before the proof is decoded

  private def claimHash[H](v: CanonicalValue)(implicit T: TrieHashing[H]): Either[TrieError, Digest[H]] =
    ValueCommitment.source(v).bimap(e => TrieError.TooLarge(e.limit), T.valueOf)

  /** A verified proof, or `None` for one of another trie (`false`); a root of the wrong length is `Encoding`. */
  private def verifiedOrNone[H](mode: KeyMode, root: ByteVector, proof: CanonicalValue)(
    implicit H: Hasher[H],
    T:          TrieHashing[H]
  ): Either[TrieError, Option[VerifiedProof[H]]] =
    for {
      r <- Digest.fromBytes[H](root).leftMap(e => TrieError.Encoding(s"root: $e"))
      p <- decode[H](proof)
      v <- verify(mode, r, p) match {
        case Right(v)                           => Option(v).asRight
        case Left(TrieError.RootMismatch(_, _)) => None.asRight
        case Left(other)                        => other.asLeft
      }
    } yield v

  /** `trie_verify`: whether `key` holds `claim` (`None` or null: is absent). */
  def verifyClaim[H](mode: KeyMode, root: ByteVector, key: ByteVector, claim: Option[CanonicalValue], proof: CanonicalValue)(
    implicit H: Hasher[H],
    T:          TrieHashing[H]
  ): Either[TrieError, Boolean] =
    for {
      path <- mode.path(key)
      present = claim.filterNot(_ == CanonicalValue.CNull)
      _        <- ProofInput.claimed(present.toList)(TrieError.TooLarge(_))
      verified <- verifiedOrNone(mode, root, proof)
      held     <- verified.fold(false.asRight[TrieError])(v => present.traverse(claimHash[H]).flatMap(v.holds(path, _)))
    } yield held

  /** `trie_prefix_verify`: whether `entries`, in key order, are exactly the trie's entries under `prefix`. */
  def verifyPrefixClaims[H](
    mode:    KeyMode,
    root:    ByteVector,
    prefix:  ByteVector,
    entries: List[(ByteVector, CanonicalValue)],
    proof:   CanonicalValue
  )(
    implicit H: Hasher[H],
    T:          TrieHashing[H]
  ): Either[TrieError, Boolean] =
    for {
      paths <- entries.traverse {
        case (k, v) =>
          if (v == CanonicalValue.CNull) TrieError.Encoding("an entry is [key, value], with a value").asLeft
          else mode.path(k).map(_ -> v)
      }
      _        <- ProofInput.claimed(entries.map(_._2))(TrieError.TooLarge(_))
      claimed  <- paths.traverse { case (p, v) => claimHash[H](v).map(p -> _) }
      verified <- verifiedOrNone(mode, root, proof)
      listed   <- verified.fold(false.asRight[TrieError])(_.listsPrefix(prefix.bits, claimed))
    } yield listed

  /** Recomputes the root, enforcing the branch invariant, the mode, and the key-length bound. */
  def verify[H](mode: KeyMode, root: Digest[H], proof: TrieProof[H])(implicit T: TrieHashing[H]): Either[TrieError, VerifiedProof[H]] =
    proof.top
      .fold(T.empty.asRight[TrieError])(top =>
        // one heavier than any decoder accepts (a shared subtree, a lazy segment), refused as decoding would refuse it
        if (withinProofWeight(top)) hash(mode, top, 0L).value
        else TrieError.Encoding(s"a proof heavier than ${ProofInput.MaxProofWeight}").asLeft
      )
      .flatMap(computed => Either.cond(computed == root, new VerifiedProof[H](proof.top), TrieError.RootMismatch(root.hex, computed.hex)))

  private def hash[H](mode: KeyMode, node: ProofNode[H], depth: Long)(implicit T: TrieHashing[H]): Eval[Either[TrieError, Digest[H]]] =
    node match {
      case Stub(h) => Eval.now(h.asRight)
      // a segment longer than the bits left is refused before any length is added: a host's lazy BitVector can be
      // Long.MaxValue long, and the sum would wrap past every bound; depth is at most mode.maxBits, so nothing overflows
      case PLeaf(segment, _) if segment.size > mode.maxBits - depth =>
        Eval.now(TrieError.InvalidProof(s"a leaf segment of ${segment.size} bits passes $mode").asLeft)
      case PBranch(segment, _, _, _) if segment.size > mode.maxBits - depth - 1 =>
        Eval.now(TrieError.InvalidProof(s"a branch segment of ${segment.size} bits passes $mode").asLeft)
      case PLeaf(segment, v) =>
        val bits = depth + segment.size
        val lengthOk = mode match {
          case f: KeyMode.Fixed => bits == f.maxBits
          case _                => bits <= mode.maxBits && bits % 8 == 0
        }
        Eval.now(Either.cond(lengthOk, T.leaf(segment, v), TrieError.InvalidProof(s"leaf ends at bit $bits, invalid for $mode")))
      case PBranch(segment, value, left, right) =>
        val here = depth + segment.size
        val children = left.fold(0)(_ => 1) + right.fold(0)(_ => 1)
        if (!Node.validBranch(value, children))
          Eval.now(TrieError.InvalidProof("branch needs two children, or one child and a value").asLeft)
        else if (value.isDefined && (!mode.allowsBranchValues || here % 8 != 0 || here > mode.maxBits))
          Eval.now(TrieError.InvalidProof(s"branch value at bit $here is invalid for $mode").asLeft)
        else if (here + 1 > mode.maxBits) Eval.now(TrieError.InvalidProof(s"branch at bit $here exceeds $mode").asLeft)
        else
          (
            left.traverse(c => Eval.defer(hash(mode, c, here + 1))).map(_.sequence),
            right.traverse(c => Eval.defer(hash(mode, c, here + 1))).map(_.sequence)
          ).mapN((l, r) => (l, r).mapN(T.branch(segment, value, _, _)))
    }

  // ------------------------------------------------------------------ flat encoding (docs/specs/trie.md §5)

  private def wireSegment(bits: BitVector): W.TrieSegment = W.TrieSegment(bits.size, bits.toByteVector)

  /**
   * Pre-order records through the generated codec of shared/schemas/trie.cddl. A proof every verifier would refuse is
   * never emitted: one heavier than `ProofInput.MaxProofWeight` is `TooLarge` (docs/specs/conformance.md §5).
   */
  def encode[H](proof: TrieProof[H]): Either[TrieError, CanonicalValue] =
    Either
      .cond(proof.top.forall(withinProofWeight), (), TrieError.TooLarge(ProofInput.MaxProofWeight))
      .flatMap(_ =>
        W.TrieProof.codec
          .encode(proof.top.fold(Vector.empty[W.TrieRecord])(t => records(List(t), Nil).toVector))
          .leftMap(e => TrieError.Encoding(e.toString))
      )
      .flatMap(v => Either.cond(v.weight <= ProofInput.MaxProofWeight, v, TrieError.TooLarge(ProofInput.MaxProofWeight)))

  /**
   * Whether the flat encoding of the proof under `top` weighs at most `MaxProofWeight`: its exact weight
   * ([[wireWeight]]), counted only as far as the limit, so O(min(nodes, MaxProofWeight)) and nothing is built. A proof
   * built in memory can share a subtree (`[x, x]` nested `k` deep is `2^k` records) or hold a host's lazy segment of any
   * length; either is refused before a record is expanded or a node hashed.
   */
  private def withinProofWeight[H](top: ProofNode[H]): Boolean = wireWeight(top).isDefined

  /**
   * The weight of the flat encoding of the proof under `top` (trie.cddl's `trie-proof`, as `CanonicalValue.weight`
   * counts it), if it is at most `MaxProofWeight`; counting stops past it. A map weighs 1 and each entry its key's
   * (1 and its UTF-8) and its value's; bytes weigh 1 and their length; a version below 2^63 and a children code weigh 1.
   * So a segment of `n` bits weighs `7 + ceil(n / 8)`, a stub `4 + |h|`, a leaf `6 + segment + |v|` and a branch
   * `6 + segment`, and `3 + |v|` more with a value; the array weighs 1 more.
   */
  private[trie] def wireWeight[H](top: ProofNode[H]): Option[Long] = {
    // a segment's bytes, rounded up without overflow, and every sum below the limit plus one record's: under 2^62
    def segment(bits: BitVector): Long = 7L + bits.size / 8 + (if (bits.size % 8 == 0) 0L else 1L)
    @tailrec
    def go(todo: List[ProofNode[H]], spent: Long): Option[Long] =
      if (spent > ProofInput.MaxProofWeight) None
      else
        todo match {
          case Nil                 => Some(spent)
          case Stub(h) :: rest     => go(rest, spent + 4L + h.bytes.size)
          case PLeaf(s, v) :: rest => go(rest, spent + 6L + segment(s) + v.bytes.size)
          case PBranch(s, v, l, r) :: rest =>
            go(l.toList ++ r.toList ++ rest, spent + 6L + segment(s) + v.fold(0L)(d => 3L + d.bytes.size))
        }
    go(List(top), 1L)
  }

  /** The records, pre-order: only of a proof [[withinProofWeight]], so their number and their bytes are bounded. */
  @tailrec
  private def records[H](todo: List[ProofNode[H]], acc: List[W.TrieRecord]): List[W.TrieRecord] =
    todo match {
      case Nil                 => acc.reverse
      case Stub(h) :: rest     => records(rest, W.TrieStub(h.bytes) :: acc)
      case PLeaf(s, v) :: rest => records(rest, W.TrieLeaf(wireSegment(s), v.bytes) :: acc)
      case PBranch(s, v, l, r) :: rest =>
        val children = (l, r) match {
          case (Some(_), Some(_)) => W.TrieChildren.Both
          case (Some(_), None)    => W.TrieChildren.Left
          case _                  => W.TrieChildren.Right
        }
        records(l.toList ++ r.toList ++ rest, W.TrieBranch(wireSegment(s), v.map(_.bytes), children) :: acc)
    }

  /** Pending decoder work: a branch waiting for its children, newest first. */
  final private case class Open[H](
    segment:   BitVector,
    value:     Option[Digest[H]],
    wantLeft:  Boolean,
    wantRight: Boolean,
    left:      Option[ProofNode[H]]
  )

  /**
   * The generated decoder checks each record's shape; this rebuilds the tree and checks the structure. A proof heavier
   * than `ProofInput.MaxProofWeight` is `Encoding`, refused before it is decoded.
   */
  def decode[H: Hasher](value: CanonicalValue): Either[TrieError, TrieProof[H]] =
    ProofInput
      .bounded(value)(TrieError.Encoding(_))
      .flatMap(W.TrieProof.codec.decoder.decode(_).leftMap(e => TrieError.Encoding(e.message)))
      .flatMap {
        case rs if rs.isEmpty => TrieProof[H](None).asRight
        case rs               => assembleFlat[H](rs.toList, Nil).map(t => TrieProof(Some(t)))
      }

  @tailrec
  private def assembleFlat[H: Hasher](records: List[W.TrieRecord], stack: List[Open[H]]): Either[TrieError, ProofNode[H]] =
    records match {
      case Nil => TrieError.Encoding("proof ended with unfinished branches").asLeft
      case r :: rest =>
        record[H](r) match {
          case Left(e)           => e.asLeft
          case Right(Left(open)) => assembleFlat(rest, open :: stack)
          case Right(Right(done)) =>
            attach(done, stack) match {
              case Left(e)             => e.asLeft
              case Right(Right(top))   => Either.cond(rest.isEmpty, top, TrieError.Encoding("trailing proof records"))
              case Right(Left(stack2)) => assembleFlat(rest, stack2)
            }
        }
    }

  /** Gives a finished node to the innermost open branch, closing branches that become complete. */
  @tailrec
  private def attach[H](node: ProofNode[H], stack: List[Open[H]]): Either[TrieError, Either[List[Open[H]], ProofNode[H]]] =
    stack match {
      case Nil => node.asRight.asRight
      case o :: rest =>
        if (o.wantLeft && o.left.isEmpty) {
          val filled = o.copy(left = Some(node))
          if (o.wantRight) (filled :: rest).asLeft.asRight else attach(PBranch(o.segment, o.value, Some(node), None), rest)
        } else attach(PBranch(o.segment, o.value, o.left, Some(node)), rest)
    }

  /** One typed record: a finished node, or an open branch awaiting children. */
  private def record[H](r: W.TrieRecord)(implicit H: Hasher[H]): Either[TrieError, Either[Open[H], ProofNode[H]]] = {
    def digest(b: scodec.bits.ByteVector): Either[TrieError, Digest[H]] =
      Digest.fromBytes[H](b).leftMap(e => TrieError.Encoding(e.toString))
    r match {
      case W.TrieStub(h)    => digest(h).map(d => Stub(d).asRight)
      case W.TrieLeaf(s, v) => (NodeCodec.fromWire(s), digest(v)).mapN((seg, d) => (PLeaf(seg, d): ProofNode[H]).asRight)
      case W.TrieBranch(s, v, c) =>
        val (left, right) = c match {
          case W.TrieChildren.Left  => (true, false)
          case W.TrieChildren.Right => (false, true)
          case W.TrieChildren.Both  => (true, true)
        }
        (NodeCodec.fromWire(s), v.traverse(digest)).tupled.flatMap {
          case (seg, value) =>
            Either.cond(
              Node.validBranch(value, (if (left) 1 else 0) + (if (right) 1 else 0)),
              Open[H](seg, value, left, right, None).asLeft,
              TrieError.Encoding("branch needs two children, or one child and a value")
            )
        }
    }
  }
}
