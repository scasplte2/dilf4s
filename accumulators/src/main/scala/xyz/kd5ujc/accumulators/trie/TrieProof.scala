package xyz.kd5ujc.accumulators.trie

import cats.syntax.all._
import cats.{Eval, Monad}

import scala.annotation.tailrec

import xyz.kd5ujc.accumulators.trie.Node.{Branch, Leaf}
import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.hash.{Digest, Hasher}
import xyz.kd5ujc.models.{trie => W}

import scodec.bits.BitVector

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

  /**
   * Every `(path, valueHash)` with `from <= path < until` (bitwise order, prefixes first), if no stub meets the range;
   * `None` otherwise. A covered listing is complete, including an empty one.
   */
  def range(from: BitVector, until: Option[BitVector]): Option[List[(BitVector, Digest[H])]] = {
    @tailrec
    def go(todo: List[(ProofNode[H], BitVector)], acc: List[(BitVector, Digest[H])]): Option[List[(BitVector, Digest[H])]] =
      todo match {
        case Nil => Some(acc.reverse)
        case (node, at) :: rest =>
          node match {
            case Stub(_) => None
            case PLeaf(segment, v) =>
              val key = at ++ segment
              go(rest, if (KeyOrder.within(key, from, until)) (key, v) :: acc else acc)
            case PBranch(segment, value, left, right) =>
              val here = at ++ segment
              val own = value.filter(_ => KeyOrder.within(here, from, until)).map(here -> _)
              val children = List(left.map(_ -> (here :+ false)), right.map(_ -> (here :+ true))).flatten.filter {
                case (_, p) =>
                  KeyOrder.intersects(p, from, until)
              }
              go(children ++ rest, own.fold(acc)(_ :: acc))
          }
      }
    top.fold[Option[List[(BitVector, Digest[H])]]](Some(Nil)) { t =>
      if (KeyOrder.intersects(BitVector.empty, from, until)) go(List(t -> BitVector.empty), Nil) else Some(Nil)
    }
  }

  /**
   * Every `(path, valueHash)` under `prefix`, in key order, if the proof covers the whole subtree; `None` if any part
   * of it is a stub. A covered listing is complete: no key under the prefix can be missing.
   */
  def prefix(prefix: BitVector): Option[List[(BitVector, Digest[H])]] = {
    // Walk to the node whose subtree holds exactly the keys starting with `prefix`.
    @tailrec
    def locate(node: ProofNode[H], consumed: BitVector, remaining: BitVector): Option[Option[(ProofNode[H], BitVector)]] =
      node match {
        case Stub(_) => None
        case PLeaf(segment, _) =>
          Some(Option.when(segment.startsWith(remaining))((node, consumed)))
        case PBranch(segment, _, left, right) =>
          if (segment.startsWith(remaining)) Some(Some((node, consumed)))
          else if (!remaining.startsWith(segment)) Some(None)
          else {
            val rest = remaining.drop(segment.size)
            ((if (rest.head) right else left): Option[ProofNode[H]]) match {
              case None        => Some(None)
              case Some(child) => locate(child, consumed ++ segment ++ BitVector.bit(rest.head), rest.tail)
            }
          }
      }

    // Collect every entry of a fully expanded subtree, depth first in key order.
    @tailrec
    def gather(todo: List[(ProofNode[H], BitVector)], acc: List[(BitVector, Digest[H])]): Option[List[(BitVector, Digest[H])]] =
      todo match {
        case Nil                             => Some(acc.reverse)
        case (Stub(_), _) :: _               => None
        case (PLeaf(segment, v), at) :: rest => gather(rest, (at ++ segment, v) :: acc)
        case (PBranch(segment, value, left, right), at) :: rest =>
          val here = at ++ segment
          val children = left.map(_ -> (here :+ false)).toList ++ right.map(_ -> (here :+ true)).toList
          gather(children ++ rest, value.fold(acc)(v => (here, v) :: acc))
      }

    top.fold[Option[List[(BitVector, Digest[H])]]](Some(Nil)) { t =>
      locate(t, BitVector.empty, prefix).flatMap(_.fold[Option[List[(BitVector, Digest[H])]]](Some(Nil)) {
        case (n, at) => gather(List(n -> at), Nil)
      })
    }
  }
}

/** Key-interval arithmetic in bitwise lexicographic order, where a proper prefix sorts before its extensions. */
private[trie] object KeyOrder {

  def lt(a: BitVector, b: BitVector): Boolean = {
    val c = Bits.commonPrefix(a, b)
    if (c == a.size) a.size < b.size else if (c == b.size) false else !a(c)
  }

  def lteq(a: BitVector, b: BitVector): Boolean = a == b || lt(a, b)

  /** Does the subtree of keys starting with `prefix` meet `[from, until)`? */
  def intersects(prefix: BitVector, from: BitVector, until: Option[BitVector]): Boolean = {
    val whollyBelow = !from.startsWith(prefix) && lt(prefix, from)
    val whollyAbove = until.exists(u => lteq(u, prefix))
    !whollyBelow && !whollyAbove
  }

  def within(key: BitVector, from: BitVector, until: Option[BitVector]): Boolean = lteq(from, key) && until.forall(lt(key, _))
}

object TrieProofs {

  import ProofNode._

  /** A query: a key path, or a prefix whose whole subtree must be expanded. */
  sealed trait Query extends Product with Serializable
  final case class Key(path: BitVector) extends Query
  final case class Prefix(path: BitVector) extends Query

  /** Every key in `[from, until)`, bitwise order with prefixes first; `None` is unbounded above. */
  final case class Range(from: BitVector, until: Option[BitVector]) extends Query

  /** Builds the partial trie answering `queries`: gather the nodes to expand, then assemble with `Eval`. */
  def prove[F[_]: Monad, H](lookup: Digest[H] => F[Node[H]], root: Option[Digest[H]], queries: List[Query]): F[TrieProof[H]] =
    root.fold(TrieProof[H](None).pure[F]) { r =>
      expanded(lookup, r, queries).map(nodes => TrieProof(Some(assemble(r, nodes).value)))
    }

  private def assemble[H](hash: Digest[H], nodes: Map[Digest[H], Node[H]]): Eval[ProofNode[H]] =
    nodes.get(hash) match {
      case None             => Eval.now(Stub(hash))
      case Some(l: Leaf[H]) => Eval.now(PLeaf(l.segment, l.value))
      case Some(b: Branch[H]) =>
        (b.left.traverse(h => Eval.defer(assemble(h, nodes))), b.right.traverse(h => Eval.defer(assemble(h, nodes))))
          .mapN(PBranch(b.segment, b.value, _, _))
    }

  /** The nodes on every query path, plus whole subtrees under prefix queries. */
  private def expanded[F[_]: Monad, H](lookup: Digest[H] => F[Node[H]], root: Digest[H], queries: List[Query]): F[Map[Digest[H], Node[H]]] =
    queries.foldLeftM(Map.empty[Digest[H], Node[H]]) {
      case (acc, Range(from, until)) =>
        if (KeyOrder.intersects(BitVector.empty, from, until)) rangeNodes(lookup, root, from, until, acc) else acc.pure[F]
      case (acc, Key(p))    => pathNodes(lookup, root, p, isPrefix = false, acc)
      case (acc, Prefix(p)) => pathNodes(lookup, root, p, isPrefix = true, acc)
    }

  /** The nodes whose key intervals meet `[from, until)`, gathered by a work list from the root. */
  private def rangeNodes[F[_]: Monad, H](
    lookup: Digest[H] => F[Node[H]],
    root:   Digest[H],
    from:   BitVector,
    until:  Option[BitVector],
    seen:   Map[Digest[H], Node[H]]
  ): F[Map[Digest[H], Node[H]]] =
    (List((root, BitVector.empty)), seen).tailRecM {
      case (Nil, acc) => acc.asRight[(List[(Digest[H], BitVector)], Map[Digest[H], Node[H]])].pure[F]
      case ((h, at) :: rest, acc) =>
        lookup(h).map {
          case l: Leaf[H] => (rest, acc.updated(h, l)).asLeft
          case b: Branch[H] =>
            val here = at ++ b.segment
            val next = List(b.left.map(_ -> (here :+ false)), b.right.map(_ -> (here :+ true))).flatten.filter {
              case (_, p) =>
                KeyOrder.intersects(p, from, until)
            }
            (next ++ rest, acc.updated(h, b)).asLeft
        }
    }

  /** The nodes on one key's path, plus the whole subtree under a prefix. */
  private def pathNodes[F[_]: Monad, H](
    lookup:   Digest[H] => F[Node[H]],
    root:     Digest[H],
    path:     BitVector,
    isPrefix: Boolean,
    acc:      Map[Digest[H], Node[H]]
  ): F[Map[Digest[H], Node[H]]] =
    (root, path, acc).tailRecM {
      case (hash, remaining, seen) =>
        lookup(hash).flatMap {
          case l: Leaf[H] =>
            seen.updated(hash, l).asRight[(Digest[H], BitVector, Map[Digest[H], Node[H]])].pure[F]
          case b: Branch[H] =>
            val withThis = seen.updated(hash, b)
            if (isPrefix && b.segment.startsWith(remaining)) TrieEngine.collect(lookup, List(hash)).map(sub => (withThis ++ sub).asRight)
            else {
              val rest = remaining.drop(b.segment.size)
              if (!remaining.startsWith(b.segment) || rest.isEmpty)
                withThis.asRight[(Digest[H], BitVector, Map[Digest[H], Node[H]])].pure[F]
              else
                b.child(rest.head)
                  .fold(withThis.asRight[(Digest[H], BitVector, Map[Digest[H], Node[H]])].pure[F])(c =>
                    (c, rest.tail, withThis).asLeft[Map[Digest[H], Node[H]]].pure[F]
                  )
            }
        }
    }

  /** Recomputes the root, enforcing the branch invariant, the mode, and the key-length bound. */
  def verify[H](mode: KeyMode, root: Digest[H], proof: TrieProof[H])(implicit T: TrieHashing[H]): Either[TrieError, VerifiedProof[H]] =
    proof.top
      .fold(T.empty.asRight[TrieError])(top => hash(mode, top, 0L).value)
      .flatMap(computed => Either.cond(computed == root, new VerifiedProof[H](proof.top), TrieError.RootMismatch(root.hex, computed.hex)))

  private def hash[H](mode: KeyMode, node: ProofNode[H], depth: Long)(implicit T: TrieHashing[H]): Eval[Either[TrieError, Digest[H]]] =
    node match {
      case Stub(h) => Eval.now(h.asRight)
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

  /** Pre-order records through the generated codec of shared/schemas/trie.cddl. */
  def encode[H](proof: TrieProof[H]): Either[TrieError, CanonicalValue] =
    W.TrieProof.codec
      .encode(proof.top.fold(Vector.empty[W.TrieRecord])(t => records(List(t), Nil).toVector))
      .leftMap(e => TrieError.Encoding(e.toString))

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

  /** The generated decoder checks each record's shape; this rebuilds the tree and checks the structure. */
  def decode[H: Hasher](value: CanonicalValue): Either[TrieError, TrieProof[H]] =
    W.TrieProof.codec.decoder.decode(value).leftMap(e => TrieError.Encoding(e.message)).flatMap {
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
