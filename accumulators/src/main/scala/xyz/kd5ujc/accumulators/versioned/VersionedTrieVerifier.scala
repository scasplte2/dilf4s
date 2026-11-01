package xyz.kd5ujc.accumulators.versioned

import cats.syntax.all._

import xyz.kd5ujc.accumulators.trie.{KeyMode, Lookup, TrieHashing, TrieProof, TrieProofs, VerifiedProof}
import xyz.kd5ujc.accumulators.versioned.VersionedTrie.Head
import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.hash.Digest

import scodec.bits.BitVector

/** A state proof checked against its state root: what it shows about keys, namespaces and ranges. */
final class VerifiedState[H] private[versioned] (proof: VerifiedProof[H])(implicit T: TrieHashing[H]) {

  private def covered[A](what: String)(a: Option[A]): Either[VersionedTrieError, A] = a.toRight(VersionedTrieError.NotCovered(what))

  private def keyed(listing: List[(BitVector, Digest[H])]): Either[VersionedTrieError, List[(StateKey, Digest[H])]] =
    listing.traverse { case (bits, h) => StateKey.fromBytes(bits.bytes).bimap(VersionedTrieError.Key(_), _ -> h) }

  /** The key's value hash, or `None` if it is proven absent. */
  def lookup(key: StateKey): Either[VersionedTrieError, Option[Digest[H]]] =
    proof.lookup(key.bytes.bits) match {
      // `Lookup` is covariant, so its digest is re-typed through the checked constructor
      case Lookup.Present(h) => Digest.fromBytes[H](h.bytes)(using T.hasher).bimap(e => VersionedTrieError.Malformed(e.toString), Some(_))
      case Lookup.Absent     => None.asRight
      case Lookup.NotCovered => VersionedTrieError.NotCovered(s"key ${key.value}").asLeft
    }

  /**
   * Whether the key holds `value` (`None`: is absent), as a JLVM `trie_verify` decides it: a null claims absence
   * there, so `Some(CNull)` is absence too (no key can hold null; a null value deletes it). The lookup comes first, and
   * a claim is checked and hashed only against a present key. A claim no commit accepts (`VersionedTrie.checkValue`)
   * is never stored, so it is not held: `false`, without hashing it. One too large for its weight alone is answered in
   * O(1), however much it shares. The Rust SDK's `VerifiedState::holds` answers the same.
   */
  def holds(key: StateKey, value: Option[CanonicalValue]): Either[VersionedTrieError, Boolean] =
    lookup(key).map { found =>
      value.filterNot(_ == CanonicalValue.CNull) match {
        case None    => found.isEmpty
        case Some(v) => found.exists(h => VersionedTrie.checkValue(key, v).isRight && h == VersionedTrie.valueHash[H](v))
      }
    }

  /** Every entry under the namespace, complete. */
  def namespace(ns: StateNamespace): Either[VersionedTrieError, List[(StateKey, Digest[H])]] =
    covered(s"namespace ${ns.value}")(proof.prefix(ns.prefix.bits)).flatMap(keyed)

  /** Every entry in `[from, until)`, complete. */
  def range(from: StateKey, until: Option[StateKey]): Either[VersionedTrieError, List[(StateKey, Digest[H])]] =
    covered(s"range [${from.value}, ${until.fold("")(_.value)})")(proof.range(from.bytes.bits, until.map(_.bytes.bits))).flatMap(keyed)
}

/**
 * Pure verification of committed proofs against a trusted commitment root (docs/specs/versioned-trie.md §6). Every check is
 * the one a JLVM program makes with the trie operations; the verifier supplies all keys, so no proof can redirect a
 * query. "Not covered" is an error, never a negative answer.
 */
object VersionedTrieVerifier {

  private def mismatch(component: String, expected: Digest[?], computed: Digest[?]): VersionedTrieError =
    VersionedTrieError.RootMismatch(component, expected.hex, computed.hex)

  private def verified[H: TrieHashing](mode: Either[VersionedTrieError, KeyMode], root: Digest[H], what: String)(
    proof: Option[TrieProof[H]]
  ): Either[VersionedTrieError, VerifiedProof[H]] =
    (mode, proof.toRight(VersionedTrieError.Malformed(s"the proof has no $what proof"))).tupled.flatMap {
      case (m, p) => VersionedTrie.trie(TrieProofs.verify(m, root, p))
    }

  /** The head `proof.top` binds to `root`: each slot must be present and hold the head's value. */
  private def bind[V: VersionKey, H](root: Digest[H], proof: VersionedTrieProof[V, H])(
    implicit T: TrieHashing[H]
  ): Either[VersionedTrieError, Head[V, H]] =
    for {
      claimed <- VersionedTrie.root(proof.head)
      _       <- Either.cond(claimed == root, (), mismatch("root", root, claimed))
      top     <- verified(VersionedTrie.topMode, root, "top")(Some(proof.top))
      vkey    <- VersionKey[V].encode(proof.head.version)
      _ <- List(
        VersionedTrie.VersionSlot -> vkey,
        VersionedTrie.StateSlot   -> proof.head.stateRoot.bytes,
        VersionedTrie.HistorySlot -> proof.head.historyRoot.bytes
      ).traverse_ {
        case (slot, value) =>
          top.lookup(slot.bits) match {
            case Lookup.Present(h) if h == VersionedTrie.bytesHash[H](value) => ().asRight[VersionedTrieError]
            case Lookup.NotCovered => VersionedTrieError.NotCovered(s"top slot ${slot.toHex}").asLeft[Unit]
            case _                 => VersionedTrieError.Malformed(s"top slot ${slot.toHex} does not hold the head's value").asLeft[Unit]
          }
      }
    } yield proof.head

  private def history[V: VersionKey, H: TrieHashing](
    head:  Head[V, H],
    proof: VersionedTrieProof[V, H]
  ): Either[VersionedTrieError, VerifiedProof[H]] =
    verified(VersionedTrie.historyMode[V], head.historyRoot, "history")(proof.history)

  private def asked[V, H](proof: VersionedTrieProof[V, H], u: V)(implicit V: VersionKey[V]): Either[VersionedTrieError, At[V, H]] =
    proof.at.toRight(VersionedTrieError.Malformed("the proof names no version")).flatMap { at =>
      (V.encode(at.version), V.encode(u)).tupled.flatMap {
        case (a, b) => Either.cond(a == b, at, VersionedTrieError.Malformed(s"the proof is about version ${a.toHex}, not ${b.toHex}"))
      }
    }

  /** The state root of `u` per the history trie: `Some` if committed, `None` if proven never committed. */
  private def versionRoot[V, H](head: Head[V, H], proof: VersionedTrieProof[V, H], u: V)(
    implicit V: VersionKey[V],
    T:          TrieHashing[H]
  ): Either[VersionedTrieError, Option[Digest[H]]] =
    for {
      at   <- asked(proof, u)
      hist <- history(head, proof)
      vkey <- V.encode(u)
      root <- (hist.lookup(vkey.bits), at.stateRoot) match {
        case (Lookup.Present(h), Some(s)) =>
          Either.cond(
            h == VersionedTrie.bytesHash[H](s.bytes),
            Option(s),
            VersionedTrieError.Malformed(s"history holds another state root for ${vkey.toHex}")
          )
        case (Lookup.Absent, None)  => Option.empty[Digest[H]].asRight[VersionedTrieError]
        case (Lookup.NotCovered, _) => VersionedTrieError.NotCovered(s"version ${vkey.toHex}").asLeft
        case (Lookup.Present(_), None) =>
          VersionedTrieError.Malformed(s"version ${vkey.toHex} is committed; the proof claims it is not").asLeft
        case (Lookup.Absent, Some(_)) =>
          VersionedTrieError.Malformed(s"version ${vkey.toHex} is not committed; the proof claims a state").asLeft
      }
    } yield root

  /**
   * The head bound to `root`, with the head invariant (§3): the history trie holds the head version with the head's
   * state root, and nothing after it. The proof's history part must be a range proof from the head version up.
   */
  def verifyHead[V, H](
    root:  Digest[H],
    proof: VersionedTrieProof[V, H]
  )(implicit V: VersionKey[V], T: TrieHashing[H]): Either[VersionedTrieError, Head[V, H]] =
    for {
      head <- bind(root, proof)
      hist <- history(head, proof)
      vkey <- V.encode(head.version)
      tail <- hist.range(vkey.bits, None).toRight(VersionedTrieError.NotCovered("history from the head version up"))
      _ <- Either.cond(
        tail == List(vkey.bits -> VersionedTrie.bytesHash[H](head.stateRoot.bytes)),
        (),
        VersionedTrieError.Malformed("the history trie does not end at the head version with the head's state root")
      )
    } yield head

  /** A proof about the current state: its state part checked against the head's state root. */
  def verifyState[V: VersionKey, H: TrieHashing](
    root:  Digest[H],
    proof: VersionedTrieProof[V, H]
  ): Either[VersionedTrieError, VerifiedState[H]] =
    for {
      head  <- bind(root, proof)
      _     <- Either.cond(proof.at.isEmpty, (), VersionedTrieError.Malformed("a historical proof is not about the current state"))
      state <- verified(VersionedTrie.stateMode, head.stateRoot, "state")(proof.state)
    } yield new VerifiedState[H](state)

  /** Whether version `u` was committed by the head: its state root, or `None` if it provably never was. */
  def verifyVersion[V: VersionKey, H: TrieHashing](
    root:  Digest[H],
    proof: VersionedTrieProof[V, H],
    u:     V
  ): Either[VersionedTrieError, Option[Digest[H]]] =
    bind(root, proof).flatMap(versionRoot(_, proof, u))

  /** The state of a past version `u`: the history trie gives its state root, which the state part is checked against. */
  def verifyHistoricalKey[V: VersionKey, H: TrieHashing](
    root:  Digest[H],
    proof: VersionedTrieProof[V, H],
    u:     V
  ): Either[VersionedTrieError, VerifiedState[H]] =
    for {
      head  <- bind(root, proof)
      uk    <- VersionKey[V].encode(u)
      s     <- versionRoot(head, proof, u).flatMap(_.toRight(VersionedTrieError.UnknownVersion(uk)))
      state <- verified(VersionedTrie.stateMode, s, "state")(proof.state)
    } yield new VerifiedState[H](state)

  /**
   * Whether `claimed` is exactly the committed versions in `[from, until)` with their state roots, in order: a complete
   * listing, as `trie_range_verify` (or, for a byte-aligned epoch, `trie_prefix_verify`) decides it.
   */
  def verifyVersions[V, H](root: Digest[H], proof: VersionedTrieProof[V, H], from: V, until: Option[V], claimed: List[(V, Digest[H])])(
    implicit V: VersionKey[V],
    T:          TrieHashing[H]
  ): Either[VersionedTrieError, Boolean] =
    for {
      head <- bind(root, proof)
      hist <- history(head, proof)
      fk   <- V.encode(from)
      uk   <- until.traverse(V.encode)
      listing <- hist
        .range(fk.bits, uk.map(_.bits))
        .toRight(VersionedTrieError.NotCovered(s"versions [${fk.toHex}, ${uk.fold("")(_.toHex)})"))
      expected <- claimed.traverse { case (v, s) => V.encode(v).map(k => k.bits -> VersionedTrie.bytesHash[H](s.bytes)) }
    } yield listing == expected

  /**
   * The latest committed version `p <= t` and its state root, or `None` if every committed version is after `t`. The
   * history part must list `[vkey(p), vkey(t) ‖ 0x00)` as exactly `p`; the bound sorts after `vkey(t)` and before every
   * longer-than-`t` key, because a proper prefix sorts first.
   */
  def verifyAsOf[V, H](root: Digest[H], proof: VersionedTrieProof[V, H], t: V)(
    implicit V: VersionKey[V],
    T:          TrieHashing[H]
  ): Either[VersionedTrieError, Option[(V, Digest[H])]] =
    for {
      head <- bind(root, proof)
      hist <- history(head, proof)
      tkey <- V.encode(t)
      bound = (tkey :+ 0x00.toByte).bits
      found <- proof.at.traverse { at =>
        (V.encode(at.version), at.stateRoot.toRight(VersionedTrieError.Malformed("an as-of answer names its state root"))).tupled.flatMap {
          case (pkey, s) =>
            Either.cond(
              xyz.kd5ujc.core.canonical.Cbor.compareBytes(pkey, tkey) <= 0,
              (pkey, s, at.version),
              VersionedTrieError.Malformed(s"the answer ${pkey.toHex} is after ${tkey.toHex}")
            )
        }
      }
      from = found.fold(BitVector.empty)(_._1.bits)
      listing <- hist.range(from, Some(bound)).toRight(VersionedTrieError.NotCovered(s"history up to ${tkey.toHex}"))
      expected = found.toList.map { case (pkey, s, _) => pkey.bits -> VersionedTrie.bytesHash[H](s.bytes) }
      _ <- Either.cond(listing == expected, (), VersionedTrieError.Malformed(s"the history up to ${tkey.toHex} is not the claimed answer"))
    } yield found.map { case (_, s, p) => p -> s }
}
