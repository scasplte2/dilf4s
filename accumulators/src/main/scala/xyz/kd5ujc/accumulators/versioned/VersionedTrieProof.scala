package xyz.kd5ujc.accumulators.versioned

import cats.syntax.all._

import xyz.kd5ujc.accumulators.trie.{TrieProof, TrieProofs}
import xyz.kd5ujc.accumulators.versioned.VersionedTrie.Head
import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.hash.{Digest, Hasher}
import xyz.kd5ujc.models.{trie => WT, versioned_trie => W}

import scodec.bits.ByteVector

/** A version `u` asked about, and its state root if it was committed (`None` proves it was not). */
final case class At[V, H](version: V, stateRoot: Option[Digest[H]])

/** Which state entries a proof answers: keys, whole namespaces, and key ranges `[from, until)`. */
final case class StateQuery(keys: List[StateKey], namespaces: List[StateNamespace], ranges: List[(StateKey, Option[StateKey])])

object StateQuery {
  def keys(ks:       StateKey*): StateQuery = StateQuery(ks.toList, Nil, Nil)
  def namespaces(ns: StateNamespace*): StateQuery = StateQuery(Nil, ns.toList, Nil)
  def range(from:    StateKey, until: Option[StateKey]): StateQuery = StateQuery(Nil, Nil, List(from -> until))
}

/**
 * A proof against one commitment root (docs/specs/versioned-trie.md §5). `top` binds `head` to the root; `history` is a
 * proof over the head's history root about `at` (or about the head itself); `state` is a proof over the state root of
 * `at` if present, else of the head. Trie proofs carry no keys: the verifier supplies every key, prefix and range.
 */
final case class VersionedTrieProof[V, H](
  head:    Head[V, H],
  top:     TrieProof[H],
  at:      Option[At[V, H]],
  history: Option[TrieProof[H]],
  state:   Option[TrieProof[H]]
)

/** One version's change: the head before it (`None` for the first commit), the head after it, and the writes. */
final case class Delta[V, H](
  parent:  Option[Head[V, H]],
  head:    Head[V, H],
  upserts: List[(StateKey, CanonicalValue)],
  removes: List[StateKey]
)

/** Wire forms through the codecs generated from shared/schemas/versioned-trie.cddl. */
object VersionedTrieWire {

  private def malformed(e: Any): VersionedTrieError = VersionedTrieError.Malformed(e.toString)

  private def digest[H: Hasher](b: ByteVector): Either[VersionedTrieError, Digest[H]] = Digest.fromBytes[H](b).leftMap(malformed)

  def head[V, H](h: Head[V, H])(implicit V: VersionKey[V]): Either[VersionedTrieError, W.VersionedTrieHead] =
    V.encode(h.version).map(W.VersionedTrieHead(_, h.stateRoot.bytes, h.historyRoot.bytes))

  def fromHead[V, H: Hasher](w: W.VersionedTrieHead)(implicit V: VersionKey[V]): Either[VersionedTrieError, Head[V, H]] =
    (V.decode(w.v), digest[H](w.s), digest[H](w.h)).mapN(Head(_, _, _))

  private def trieProof[H](p: TrieProof[H]): Either[VersionedTrieError, WT.TrieProof] =
    VersionedTrie.trie(TrieProofs.encode(p)).flatMap(WT.TrieProof.codec.decoder.decode(_).leftMap(malformed))

  private def fromTrieProof[H: Hasher](w: WT.TrieProof): Either[VersionedTrieError, TrieProof[H]] =
    WT.TrieProof.codec.encode(w).leftMap(malformed).flatMap(v => VersionedTrie.trie(TrieProofs.decode[H](v)))

  def encodeProof[V: VersionKey, H](p: VersionedTrieProof[V, H]): Either[VersionedTrieError, CanonicalValue] =
    (
      head(p.head),
      trieProof(p.top),
      p.at.traverse(a => VersionKey[V].encode(a.version).map(W.VersionedTrieAt(_, a.stateRoot.map(_.bytes)))),
      p.history.traverse(trieProof),
      p.state.traverse(trieProof)
    ).mapN(W.VersionedTrieProof(_, _, _, _, _)).flatMap(W.VersionedTrieProof.codec.encode(_).leftMap(malformed))

  def decodeProof[V: VersionKey, H: Hasher](v: CanonicalValue): Either[VersionedTrieError, VersionedTrieProof[V, H]] =
    W.VersionedTrieProof.codec.decoder.decode(v).leftMap(malformed).flatMap { w =>
      (
        fromHead[V, H](w.head),
        fromTrieProof[H](w.top),
        w.at.traverse(a => (VersionKey[V].decode(a.v), a.s.traverse(digest[H])).mapN(At[V, H](_, _))),
        w.history.traverse(fromTrieProof[H]),
        w.state.traverse(fromTrieProof[H])
      ).mapN(VersionedTrieProof(_, _, _, _, _))
    }

  def encodeDelta[V: VersionKey, H](d: Delta[V, H]): Either[VersionedTrieError, CanonicalValue] =
    (d.parent.traverse(head(_)), head(d.head)).tupled.flatMap {
      case (parent, h) =>
        W.VersionedTrieDelta.codec
          .encode(
            W.VersionedTrieDelta(
              parent,
              h,
              d.upserts.map { case (k, v) => W.VersionedTrieUpsert(k.value, v) }.toVector,
              d.removes.map(_.value).toVector
            )
          )
          .leftMap(malformed)
    }

  def decodeDelta[V: VersionKey, H: Hasher](v: CanonicalValue): Either[VersionedTrieError, Delta[V, H]] =
    W.VersionedTrieDelta.codec.decoder.decode(v).leftMap(malformed).flatMap { w =>
      def key(s: String) = StateKey.parse(s).leftMap(VersionedTrieError.Key(_))
      (
        w.parent.traverse(fromHead[V, H]),
        fromHead[V, H](w.head),
        w.upserts.toList.traverse(u => key(u.k).map(_ -> u.v)),
        w.removes.toList.traverse(key)
      ).mapN(Delta(_, _, _, _))
    }
}
