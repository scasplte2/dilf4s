package xyz.kd5ujc.jlvm

import cats.syntax.all._

import xyz.kd5ujc.accumulators.merkle.{MerkleCodec, MerkleError, MerkleHashing, MerkleVerifier}
import xyz.kd5ujc.accumulators.trie.{KeyMode, TrieError, TrieHashing, TrieProofs, VerifiedProof}
import xyz.kd5ujc.accumulators.{ProofInput, ValueCommitment}
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{ByteSource, CanonicalValue, ValueKind}
import xyz.kd5ujc.core.hash.{Digest, Sha256}
import xyz.kd5ujc.jlvm.JlvmError._

import scodec.bits.{BitVector, ByteVector}

/**
 * Verifiers over dilf4s's authenticated structures (docs/specs/jlvm.md §6, merkle.md, trie.md), with SHA-256:
 *
 *   - `merkle_verify [head, leaf, proof]` and `merkle_consistency_verify [oldHead, newHead, proof]`, where a head is
 *     `{root, size}` from one trusted tree head; an optional last argument `"rfc6962"` selects that profile;
 *   - `trie_verify [mode, root, key, value, proof]`, `trie_prefix_verify [mode, root, prefix, entries, proof]` and
 *     `trie_range_verify [mode, root, from, until, entries, proof]`, where a mode is `{"fixed": n}` or
 *     `{"variable": max}`, a null value is a proven absence, and entries are `[key, value]` pairs in key order.
 *
 * A value that is bytes is itself; any other value is its canonical CBOR encoding. A proof that is well formed but
 * does not prove the claim is `false`. Malformed input, and a proof that does not reach the queried key or range, are
 * `Crypto` errors: "not covered" is neither true nor false.
 */
object ProofOps {

  private type H = Sha256
  type Result = Either[JlvmError, CanonicalValue]

  def apply(op: Op, args: Vector[CanonicalValue]): Option[Result] =
    op match {
      case Op.MerkleVerify            => Option(merkle(op, args))
      case Op.MerkleConsistencyVerify => Option(consistency(op, args))
      case Op.TrieVerify              => Option(trie(op, args))
      case Op.TriePrefixVerify        => Option(triePrefix(op, args))
      case Op.TrieRangeVerify         => Option(trieRange(op, args))
      case _                          => scala.None
    }

  private def crypto(op: Op)(reason: CryptoReason): JlvmError = JlvmError.Crypto(op, reason)

  /** A Merkle refusal, as the `Crypto` error's cause (jlvm.md §3). */
  private def merkleFailure(op: Op)(e: MerkleError): JlvmError = JlvmError.Crypto(op, CryptoReason.Refused(CryptoCause.Merkle(e)))

  /** A trie refusal, as the `Crypto` error's cause (jlvm.md §3). */
  private def trieFailure(op: Op)(e: TrieError): JlvmError = JlvmError.Crypto(op, CryptoReason.Refused(CryptoCause.Trie(e)))

  /** How each structure refuses claims over a limit: its own `TooLarge`, as its verifiers do (conformance.md §3). */
  private def merkleTooLarge(op: Op)(limit: Long): JlvmError = merkleFailure(op)(MerkleError.TooLarge(limit))
  private def trieTooLarge(op:   Op)(limit: Long): JlvmError = trieFailure(op)(TrieError.TooLarge(limit))

  /**
   * Claims (a leaf, a value, a listing's values) together at most `ProofInput.MaxProofWeight`, checked from their
   * weights before any is hashed and before the proof is decoded: heavier is `Crypto` (docs/specs/conformance.md §5),
   * caused by the structure's `TooLarge`.
   */
  private def claims(tooLarge: Long => JlvmError)(values: Iterable[CanonicalValue]): Either[JlvmError, Unit] =
    ProofInput.claimed(values)(tooLarge)

  /** Every proof operation charges its arguments by weight, which pays for flattening bytes that are not ([[FlatBytes]]). */
  private def bytes(op: Op, what: Role)(v: CanonicalValue): Either[JlvmError, FlatBytes] =
    v match {
      case CBytes(b) => FlatBytes(b).asRight
      case other     => JlvmError.Type(op, TypeReason.Expected(Vector(ValueKind.Bytes), ValueKind.of(other), what)).asLeft
    }

  private def digest(op: Op, what: Role)(v: CanonicalValue): Either[JlvmError, Digest[H]] =
    bytes(op, what)(v).flatMap(Digest.fromBytes[H](_).leftMap(e => JlvmError.Crypto(op, CryptoReason.Refused(CryptoCause.Hash(e), what))))

  /**
   * The bytes a leaf or value commits to: tagged, so bytes and a value whose CBOR equals them never coincide. A
   * commitment past `Cbor.MaxEncodedBytes` is `Crypto`, as every refused claim is, and never hashed: a claim reaches it
   * only past `MaxProofWeight`, refused first.
   */
  private def committed(tooLarge: Long => JlvmError)(v: CanonicalValue): Either[JlvmError, ByteSource] =
    ValueCommitment.source(v).leftMap(e => tooLarge(e.limit))

  // ---- Merkle trees (merkle.md)

  private def head(op: Op, what: Role)(v: CanonicalValue): Either[JlvmError, (Digest[H], Long)] =
    v match {
      case CMap(m) if m.size == 2 && m.contains("root") && m.contains("size") =>
        (
          m.get("root").toRight(crypto(op)(CryptoReason.MissingHeadRoot(what))).flatMap(digest(op, Role.HeadRoot(what))),
          m.get("size") match {
            case scala.Some(CInt(n)) if n >= 0 && n.isValidLong => n.toLong.asRight
            case _                                              => crypto(op)(CryptoReason.TreeSize(what)).asLeft
          }
        ).tupled
      case _ => crypto(op)(CryptoReason.TreeHead(what)).asLeft
    }

  /**
   * The hashing, and how a leaf becomes bytes: the default profile commits any value by [[ValueCommitment]]; `"rfc6962"`
   * hashes as RFC 6962 and takes a leaf as raw bytes, as external logs do, so its leaves must be bytes. Its hashing differs
   * from the default's throughout, so no root verifies under both readings, whoever chooses the profile.
   */
  private def profile(
    op:   Op,
    rest: Vector[CanonicalValue]
  ): Either[JlvmError, (MerkleHashing[H], CanonicalValue => Either[JlvmError, ByteSource])] =
    rest match {
      case Vector() => (MerkleHashing.tagged[H], committed(merkleTooLarge(op))).asRight
      case Vector(CText("rfc6962")) =>
        (MerkleHashing.rfc6962[H], (v: CanonicalValue) => bytes(op, Role.Rfc6962Leaf)(v).map(ByteSource.bytes)).asRight
      case _ => JlvmError.Domain(op, DomainReason.MerkleProfile).asLeft
    }

  /** A proof that verifies is true; one that is well formed but proves something else is false. */
  private def outcome(op: Op)(r: Either[MerkleError, Unit]): Result =
    r match {
      case Right(())                                                          => True.asRight
      case Left(MerkleError.InvalidProof(_) | MerkleError.RootMismatch(_, _)) => False.asRight
      case Left(other)                                                        => merkleFailure(op)(other).asLeft
    }

  private def merkle(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case h +: leaf +: proof +: rest =>
        (
          head(op, Role.Head)(h),
          profile(op, rest)
        ).tupled.flatMap {
          case (hd, pr) =>
            // in order, each step only after the last succeeds: the proof is not decoded once the claim is refused
            claims(merkleTooLarge(op))(List(leaf)) >> MerkleCodec.decodeInclusion[H](proof).bimap(merkleFailure(op), (hd, pr, _))
        }.flatMap {
          case ((root, size), (m, leafBytes), p) =>
            // the size comes from the trusted head: a smaller tree's root can satisfy a proof claiming a larger size
            leafBytes(leaf).flatMap(d =>
              if (p.size != size) False.asRight else outcome(op)(MerkleVerifier.inclusion(root, m.leafOf(d), p)(using m))
            )
        }
      case _ => JlvmError.Arity(op, ArgumentCount.Between(3, 4), args.size).asLeft
    }

  private def consistency(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case o +: n +: proof +: rest =>
        (
          head(op, Role.OldHead)(o),
          head(op, Role.NewHead)(n),
          profile(op, rest),
          MerkleCodec.decodeConsistency[H](proof).leftMap(merkleFailure(op))
        ).tupled.flatMap {
          case ((oldRoot, oldSize), (newRoot, newSize), (m, _), p) =>
            if (p.oldSize != oldSize || p.newSize != newSize) False.asRight
            else outcome(op)(MerkleVerifier.consistency(oldRoot, newRoot, p)(using m))
        }
      case _ => JlvmError.Arity(op, ArgumentCount.Between(3, 4), args.size).asLeft
    }

  // ---- binary Patricia tries (trie.md)

  private val T = TrieHashing[H]

  private def mode(op: Op)(v: CanonicalValue): Either[JlvmError, KeyMode] =
    v match {
      case CMap(m) =>
        m.toList match {
          case List(("fixed", CInt(n))) if n.isValidInt =>
            KeyMode.fixed(n.toInt).leftMap(e => JlvmError.Domain(op, DomainReason.TrieMode(e)))
          case List(("variable", CInt(n))) if n.isValidInt =>
            KeyMode.variable(n.toInt).leftMap(e => JlvmError.Domain(op, DomainReason.TrieMode(e)))
          case _ => JlvmError.Domain(op, DomainReason.TrieModeShape).asLeft
        }
      case other => JlvmError.Type(op, TypeReason.Expected(Vector(ValueKind.Map), ValueKind.of(other), Role.Mode)).asLeft
    }

  /** A claimed value's hash, by its [[ValueCommitment]] bytes: bytes and a value whose CBOR equals them differ. */
  private def valueHash(op: Op)(v: CanonicalValue): Either[JlvmError, Digest[H]] = committed(trieTooLarge(op))(v).map(T.valueOf)

  private def path(op: Op, m: KeyMode)(key: CanonicalValue): Either[JlvmError, BitVector] =
    bytes(op, Role.Key)(key).flatMap(m.path(_).leftMap(trieFailure(op)))

  /** Checks the proof against the root: a mismatch is a well-formed proof of another trie, so `None`. */
  private def verified(op: Op, m: KeyMode, root: CanonicalValue, proof: CanonicalValue): Either[JlvmError, Option[VerifiedProof[H]]] =
    // the root first, and the proof decoded only once it is valid
    digest(op, Role.Root)(root).flatMap(r => TrieProofs.decode[H](proof).bimap(trieFailure(op), r -> _)).flatMap {
      case (r, p) =>
        TrieProofs.verify(m, r, p)(using T) match {
          case Right(v)                           => Option(v).asRight
          case Left(TrieError.RootMismatch(_, _)) => scala.None.asRight
          case Left(other)                        => trieFailure(op)(other).asLeft
        }
    }

  private def trie(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case Vector(md, root, key, value, proof) =>
        mode(op)(md)
          .flatMap(m =>
            for {
              p <- path(op, m)(key)
              _ <- claims(trieTooLarge(op))(List(value).filter(_ != CNull))
              v <- verified(op, m, root, proof)
            } yield (p, v)
          )
          .flatMap {
            case (_, scala.None) => False.asRight
            case (p, scala.Some(v)) =>
              Option
                .when(value != CNull)(value)
                .traverse(valueHash(op))
                .flatMap(claim => v.holds(p, claim).bimap(trieFailure(op), Values.bool))
          }
      case _ => JlvmError.Arity(op, ArgumentCount.Exactly(5), args.size).asLeft
    }

  private def entries(op: Op, m: KeyMode)(v: CanonicalValue): Either[JlvmError, List[(BitVector, Digest[H])]] =
    v match {
      case CArray(es) =>
        es.toList.traverse {
          case CArray(Vector(k, x)) if x != CNull => path(op, m)(k).map(_ -> x)
          case _                                  => JlvmError.Type(op, TypeReason.Entry).asLeft
        }
          .flatTap(pxs => claims(trieTooLarge(op))(pxs.map(_._2)))
          .flatMap(_.traverse { case (p, x) => valueHash(op)(x).map(p -> _) })
      case other => JlvmError.Type(op, TypeReason.Expected(Vector(ValueKind.Array), ValueKind.of(other), Role.Entries)).asLeft
    }

  /** Whether the claimed entries, in key order, are exactly the proven listing; one that reaches a stub is no answer. */
  private def listing(op: Op)(lists: Either[TrieError, Boolean]): Result = lists.bimap(trieFailure(op), Values.bool)

  private def triePrefix(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case Vector(md, root, prefix, es, proof) =>
        mode(op)(md).flatMap { m =>
          (for {
            pre     <- bytes(op, Role.Prefix)(prefix)
            claimed <- entries(op, m)(es)
            v       <- verified(op, m, root, proof)
          } yield (pre, claimed, v)).flatMap {
            case (_, _, scala.None)            => False.asRight
            case (pre, claimed, scala.Some(v)) => listing(op)(v.listsPrefix(pre.bits, claimed))
          }
        }
      case _ => JlvmError.Arity(op, ArgumentCount.Exactly(5), args.size).asLeft
    }

  private def trieRange(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case Vector(md, root, from, until, es, proof) =>
        val upper = until match {
          case CNull => scala.None.asRight[JlvmError]
          case other => bytes(op, Role.Until)(other).map(b => Option(b.bits))
        }
        mode(op)(md).flatMap { m =>
          (for {
            lo      <- bytes(op, Role.From)(from)
            hi      <- upper
            claimed <- entries(op, m)(es)
            v       <- verified(op, m, root, proof)
          } yield (lo, hi, claimed, v)).flatMap {
            case (_, _, _, scala.None)            => False.asRight
            case (lo, hi, claimed, scala.Some(v)) => listing(op)(v.listsRange(lo.bits, hi, claimed))
          }
        }
      case _ => JlvmError.Arity(op, ArgumentCount.Exactly(6), args.size).asLeft
    }
}
