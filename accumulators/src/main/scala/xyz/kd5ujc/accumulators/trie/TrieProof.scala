package xyz.kd5ujc.accumulators.trie

import cats.syntax.all._
import cats.{Eval, Monad}

import scala.annotation.tailrec

import xyz.kd5ujc.accumulators.trie.Node.{Branch, Leaf}
import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.hash.{Digest, Hasher}

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
              (if (rest.head) right else left) match {
                case None        => Lookup.Absent
                case Some(child) => go(child, rest.tail)
              }
          }
      }
    top.fold[Lookup[H]](Lookup.Absent)(go(_, path))
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
            (if (rest.head) right else left) match {
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

object TrieProofs {

  import ProofNode._

  /** A query: a key path, or a prefix whose whole subtree must be expanded. */
  sealed trait Query extends Product with Serializable
  final case class Key(path: BitVector) extends Query
  final case class Prefix(path: BitVector) extends Query

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
    queries.foldLeftM(Map.empty[Digest[H], Node[H]]) { (acc, q) =>
      val (path, isPrefix) = q match {
        case Key(p)    => (p, false)
        case Prefix(p) => (p, true)
      }
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

  def encode[H](proof: TrieProof[H]): Either[TrieError, CanonicalValue] =
    CanonicalValue
      .array(proof.top.fold(Vector.empty[CanonicalValue])(t => records(List(t), Nil).toVector))
      .leftMap(e => TrieError.Encoding(e.toString))

  @tailrec
  private def records[H](todo: List[ProofNode[H]], acc: List[CanonicalValue]): List[CanonicalValue] =
    todo match {
      case Nil                 => acc.reverse
      case Stub(h) :: rest     => records(rest, CanonicalValue.trustedMap(List("h" -> CanonicalValue.bytes(h.bytes))) :: acc)
      case PLeaf(s, v) :: rest => records(rest, NodeCodec.leaf(s, v) :: acc)
      case PBranch(s, v, l, r) :: rest =>
        val mask = (if (l.isDefined) 1 else 0) + (if (r.isDefined) 2 else 0)
        val rec = CanonicalValue.trustedMap(
          List("s" -> NodeCodec.segment(s), "c" -> CInt(mask)) ++ v.map(x => "v" -> CanonicalValue.bytes(x.bytes)).toList
        )
        records(l.toList ++ r.toList ++ rest, rec :: acc)
    }

  /** Pending decoder work: a branch waiting for its children, newest first. */
  final private case class Open[H](
    segment:   BitVector,
    value:     Option[Digest[H]],
    wantLeft:  Boolean,
    wantRight: Boolean,
    left:      Option[ProofNode[H]]
  )

  def decode[H: Hasher](value: CanonicalValue): Either[TrieError, TrieProof[H]] =
    value match {
      case CArray(rs) if rs.isEmpty => TrieProof[H](None).asRight
      case CArray(rs)               => assembleFlat[H](rs.toList, Nil).map(t => TrieProof(Some(t)))
      case other                    => TrieError.Encoding(s"proof must be an array of records: $other").asLeft
    }

  @tailrec
  private def assembleFlat[H: Hasher](records: List[CanonicalValue], stack: List[Open[H]]): Either[TrieError, ProofNode[H]] =
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

  /** One record: a finished node, or an open branch awaiting children. */
  private def record[H](v: CanonicalValue)(implicit H: Hasher[H]): Either[TrieError, Either[Open[H], ProofNode[H]]] = {
    def digest(c: CanonicalValue): Either[TrieError, Digest[H]] = c match {
      case CBytes(b) => Digest.fromBytes[H](b).leftMap(e => TrieError.Encoding(e.toString))
      case other     => TrieError.Encoding(s"expected digest bytes: $other").asLeft
    }
    v match {
      case CMap(m) if m.keySet == Set("h") => digest(m("h")).map(h => Stub(h).asRight)
      case CMap(m) if m.keySet == Set("s", "v") =>
        (NodeCodec.decodeSegment(m("s")), digest(m("v"))).mapN((s, d) => (PLeaf(s, d): ProofNode[H]).asRight)
      case CMap(m) if m.keySet == Set("s", "c") || m.keySet == Set("s", "c", "v") =>
        (
          NodeCodec.decodeSegment(m("s")),
          m.get("v").traverse(digest),
          m("c") match {
            case CInt(c) if c >= 1 && c <= 3 => c.toInt.asRight
            case other                       => TrieError.Encoding(s"child mask must be 1, 2 or 3: $other").asLeft
          }
        ).tupled.flatMap {
          case (s, value, mask) =>
            val children = (mask & 1) + ((mask >> 1) & 1)
            Either.cond(
              Node.validBranch(value, children),
              Open[H](s, value, (mask & 1) == 1, (mask & 2) == 2, None).asLeft,
              TrieError.Encoding("branch needs two children, or one child and a value")
            )
        }
      case other => TrieError.Encoding(s"unrecognised proof record: $other").asLeft
    }
  }
}
