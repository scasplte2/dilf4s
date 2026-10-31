package xyz.kd5ujc.jlvm

import cats.syntax.all._

import xyz.kd5ujc.accumulators.ValueCommitment
import xyz.kd5ujc.accumulators.merkle.{MerkleCodec, MerkleError, MerkleHashing, MerkleVerifier}
import xyz.kd5ujc.accumulators.trie.{KeyMode, Lookup, TrieError, TrieHashing, TrieProofs, VerifiedProof}
import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.hash.{Digest, Sha256}

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

  private def crypto(op: Op)(message: String): JlvmError = JlvmError.Crypto(op.wire, message)

  private def bytes(op: Op, what: String)(v: CanonicalValue): Either[JlvmError, ByteVector] =
    v match {
      case CBytes(b) => b.asRight
      case other     => JlvmError.Type(op.wire, s"$what is bytes, got ${Values.kind(other)}").asLeft
    }

  private def digest(op: Op, what: String)(v: CanonicalValue): Either[JlvmError, Digest[H]] =
    bytes(op, what)(v).flatMap(Digest.fromBytes[H](_).leftMap(e => crypto(op)(s"$what: $e")))

  /** The bytes a leaf or value commits to: tagged, so bytes and a value whose CBOR equals them never coincide. */
  private def committed(v: CanonicalValue): ByteVector = ValueCommitment.bytes(v)

  // ---- Merkle trees (merkle.md)

  private def head(op: Op, what: String)(v: CanonicalValue): Either[JlvmError, (Digest[H], Long)] =
    v match {
      case CMap(m) if m.keySet == Set("root", "size") =>
        (
          m.get("root").toRight(crypto(op)(s"$what.root")).flatMap(digest(op, s"$what.root")),
          m.get("size") match {
            case scala.Some(CInt(n)) if n >= 0 && n.isValidLong => n.toLong.asRight
            case _                                              => crypto(op)(s"$what.size is a tree size").asLeft
          }
        ).tupled
      case _ => crypto(op)(s"$what is a tree head {root, size}").asLeft
    }

  /**
   * The hashing, and how a leaf becomes bytes: the default profile commits any value by [[ValueCommitment]]; `"rfc6962"`
   * hashes as RFC 6962 and takes a leaf as raw bytes, as external logs do, so its leaves must be bytes. Its hashing differs
   * from the default's throughout, so no root verifies under both readings, whoever chooses the profile.
   */
  private def profile(
    op:   Op,
    rest: Vector[CanonicalValue]
  ): Either[JlvmError, (MerkleHashing[H], CanonicalValue => Either[JlvmError, ByteVector])] =
    rest match {
      case Vector()                 => (MerkleHashing.tagged[H], (v: CanonicalValue) => committed(v).asRight[JlvmError]).asRight
      case Vector(CText("rfc6962")) => (MerkleHashing.rfc6962[H], bytes(op, "an rfc6962 leaf")).asRight
      case _                        => JlvmError.Domain(op.wire, "the only optional profile is \"rfc6962\"").asLeft
    }

  /** A proof that verifies is true; one that is well formed but proves something else is false. */
  private def outcome(op: Op)(r: Either[MerkleError, Unit]): Result =
    r match {
      case Right(())                                                          => True.asRight
      case Left(MerkleError.InvalidProof(_) | MerkleError.RootMismatch(_, _)) => False.asRight
      case Left(other)                                                        => crypto(op)(other.toString).asLeft
    }

  private def merkle(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case h +: leaf +: proof +: rest =>
        (
          head(op, "head")(h),
          profile(op, rest),
          MerkleCodec.decodeInclusion[H](proof).leftMap(e => crypto(op)(e.toString))
        ).tupled.flatMap {
          case ((root, size), (m, leafBytes), p) =>
            // the size comes from the trusted head: a smaller tree's root can satisfy a proof claiming a larger size
            leafBytes(leaf).flatMap(d =>
              if (p.size != size) False.asRight else outcome(op)(MerkleVerifier.inclusion(root, m.leaf(d), p)(using m))
            )
        }
      case _ => JlvmError.Arity(op.wire, "takes [head, leaf, proof] or [head, leaf, proof, profile]").asLeft
    }

  private def consistency(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case o +: n +: proof +: rest =>
        (
          head(op, "oldHead")(o),
          head(op, "newHead")(n),
          profile(op, rest),
          MerkleCodec.decodeConsistency[H](proof).leftMap(e => crypto(op)(e.toString))
        ).tupled.flatMap {
          case ((oldRoot, oldSize), (newRoot, newSize), (m, _), p) =>
            if (p.oldSize != oldSize || p.newSize != newSize) False.asRight
            else outcome(op)(MerkleVerifier.consistency(oldRoot, newRoot, p)(using m))
        }
      case _ => JlvmError.Arity(op.wire, "takes [oldHead, newHead, proof] or [oldHead, newHead, proof, profile]").asLeft
    }

  // ---- binary Patricia tries (trie.md)

  private val T = TrieHashing[H]

  private def mode(op: Op)(v: CanonicalValue): Either[JlvmError, KeyMode] =
    v match {
      case CMap(m) =>
        m.toList match {
          case List(("fixed", CInt(n))) if n.isValidInt    => KeyMode.fixed(n.toInt).leftMap(e => JlvmError.Domain(op.wire, e.toString))
          case List(("variable", CInt(n))) if n.isValidInt => KeyMode.variable(n.toInt).leftMap(e => JlvmError.Domain(op.wire, e.toString))
          case _ => JlvmError.Domain(op.wire, "a mode is {\"fixed\": n} or {\"variable\": max}").asLeft
        }
      case other => JlvmError.Type(op.wire, s"a mode is an object, got ${Values.kind(other)}").asLeft
    }

  /** A claimed value's hash, by its [[ValueCommitment]] bytes: bytes and a value whose CBOR equals them differ. */
  private def valueHash(v: CanonicalValue): Digest[H] = T.value(committed(v))

  private def path(op: Op, m: KeyMode)(key: CanonicalValue): Either[JlvmError, BitVector] =
    bytes(op, "a key")(key).flatMap(m.path(_).leftMap(e => crypto(op)(s"a key invalid for the mode: $e")))

  /** Checks the proof against the root: a mismatch is a well-formed proof of another trie, so `None`. */
  private def verified(op: Op, m: KeyMode, root: CanonicalValue, proof: CanonicalValue): Either[JlvmError, Option[VerifiedProof[H]]] =
    (digest(op, "root")(root), TrieProofs.decode[H](proof).leftMap(e => crypto(op)(e.toString))).tupled.flatMap {
      case (r, p) =>
        TrieProofs.verify(m, r, p)(using T) match {
          case Right(v)                           => Option(v).asRight
          case Left(TrieError.RootMismatch(_, _)) => scala.None.asRight
          case Left(other)                        => crypto(op)(other.toString).asLeft
        }
    }

  private def trie(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case Vector(md, root, key, value, proof) =>
        mode(op)(md).flatMap(m => (path(op, m)(key), verified(op, m, root, proof)).tupled).flatMap {
          case (_, scala.None) => False.asRight
          case (p, scala.Some(v)) =>
            v.lookup(p) match {
              case Lookup.Present(h) => Values.bool(value != CNull && valueHash(value) == h).asRight
              case Lookup.Absent     => Values.bool(value == CNull).asRight
              case Lookup.NotCovered => crypto(op)("the proof does not reach this key").asLeft
            }
        }
      case _ => JlvmError.Arity(op.wire, "takes [mode, root, key, value, proof]").asLeft
    }

  private def entries(op: Op, m: KeyMode)(v: CanonicalValue): Either[JlvmError, List[(BitVector, Digest[H])]] =
    v match {
      case CArray(es) =>
        es.toList.traverse {
          case CArray(Vector(k, x)) if x != CNull => path(op, m)(k).map(_ -> valueHash(x))
          case _                                  => JlvmError.Type(op.wire, "an entry is [key, value], with a value").asLeft
        }
      case other => JlvmError.Type(op.wire, s"entries is an array, got ${Values.kind(other)}").asLeft
    }

  /** The claimed entries, in key order, are exactly what the proof lists; a listing that reaches a stub is no answer. */
  private def listing(op: Op, claimed: List[(BitVector, Digest[H])], proven: Option[List[(BitVector, Digest[H])]]): Result =
    proven.fold[Result](crypto(op)("the proof does not cover the whole query").asLeft)(l => Values.bool(l == claimed).asRight)

  private def triePrefix(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case Vector(md, root, prefix, es, proof) =>
        mode(op)(md).flatMap { m =>
          (bytes(op, "a prefix")(prefix), entries(op, m)(es), verified(op, m, root, proof)).tupled.flatMap {
            case (_, _, scala.None)            => False.asRight
            case (pre, claimed, scala.Some(v)) => listing(op, claimed, v.prefix(pre.bits))
          }
        }
      case _ => JlvmError.Arity(op.wire, "takes [mode, root, prefix, entries, proof]").asLeft
    }

  private def trieRange(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case Vector(md, root, from, until, es, proof) =>
        val upper = until match {
          case CNull => scala.None.asRight[JlvmError]
          case other => bytes(op, "until")(other).map(b => Option(b.bits))
        }
        mode(op)(md).flatMap { m =>
          (bytes(op, "from")(from), upper, entries(op, m)(es), verified(op, m, root, proof)).tupled.flatMap {
            case (_, _, _, scala.None)            => False.asRight
            case (lo, hi, claimed, scala.Some(v)) => listing(op, claimed, v.range(lo.bits, hi))
          }
        }
      case _ => JlvmError.Arity(op.wire, "takes [mode, root, from, until, entries, proof]").asLeft
    }
}
