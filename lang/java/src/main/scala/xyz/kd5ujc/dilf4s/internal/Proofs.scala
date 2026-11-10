package xyz.kd5ujc.dilf4s.internal

import cats.syntax.all._

import scala.jdk.CollectionConverters._

import xyz.kd5ujc.accumulators.merkle._
import xyz.kd5ujc.accumulators.poseidon.{PoseidonMerkleProof, PoseidonMerkleTree}
import xyz.kd5ujc.accumulators.trie.{KeyMode, Trie, TrieError, TrieHashing, TrieProofs}
import xyz.kd5ujc.accumulators.versioned._
import xyz.kd5ujc.accumulators.{ProofInput, ValueCommitment}
import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.hash.{Digest, Fr, Hasher, Poseidon}
import xyz.kd5ujc.crypto.zk.{Bn254Engine, Sp1Groth16}
import xyz.kd5ujc.dilf4s.Hashing.Algorithm
import xyz.kd5ujc.dilf4s.error.{
  Groth16Error => JGroth16Error,
  MerkleError => JMerkleError,
  PoseidonError => JPoseidonError,
  PoseidonMerkleError => JPoseidonMerkleError,
  StateKeyError => JStateKeyError,
  TrieError => JTrieError,
  VersionedTrieError => JVersionedTrieError,
  ZkError => JZkError,
  _
}
import xyz.kd5ujc.dilf4s.{Result, Verified}

import scodec.bits.ByteVector

/**
 * The proof verifiers for the Java API: Merkle trees (dilf4s's tagged hashing and RFC 6962), binary Patricia tries,
 * versioned tries with `Long` versions, Poseidon and its Merkle tree, BN254 and SP1's Groth16 wrapper. BN254 runs on the
 * reference's pure engine: the native one (gnark) is loaded only in an effect (`Bn254Engine.gnark[F]`).
 */
object Proofs {

  private def bv(a: Array[Byte]): ByteVector = ByteVector(a)

  private def merkle[A](e: Either[MerkleError, A]): Result[A, JMerkleError] = Values.result(e.left.map(Errors.merkleError))
  private def trie[A](e:   Either[TrieError, A]): Result[A, JTrieError] = Values.result(e.left.map(Errors.trieError))

  private def hashing[H](tagged: Boolean)(using Hasher[H]): MerkleHashing[H] =
    if (tagged) MerkleHashing.tagged[H] else MerkleHashing.rfc6962[H]

  private def digests[H](ds: java.util.List[Array[Byte]])(using h: Hasher[H]): Either[MerkleError, Vector[Digest[H]]] =
    ds.asScala.toVector.traverse(d => Digest.fromBytes[H](bv(d)).leftMap(e => MerkleError.Encoding(e.toString): MerkleError))

  private def digest[H](d: Array[Byte], what: String)(using h: Hasher[H]): Either[MerkleError, Digest[H]] =
    Digest.fromBytes[H](bv(d)).leftMap(e => MerkleError.Encoding(s"$what: $e"))

  // ---- Merkle trees (docs/specs/merkle.md)

  /** A dilf4s leaf's hash: the tagged hash of the value's commitment ([[ValueCommitment]]). */
  def merkleLeaf(a: Algorithm, v: CanonicalValue): Result[Array[Byte], EncodeError] = merkleLeafOf(Hashes.hasher(a), v)

  private def merkleLeafOf[H](h: Hasher[H], v: CanonicalValue): Result[Array[Byte], EncodeError] =
    Values.result(ValueCommitment.source(v).bimap(e => new TooLarge(e.limit), MerkleHashing.tagged[H](using h).leafOf(_).bytes.toArray))

  /** A leaf's hash over its bytes as they are (`MerkleHashing.leaf`): the tagged leaf hash, or RFC 6962's `H(0x00 || leaf)`. */
  def bytesLeaf(tagged: Boolean, a: Algorithm, leaf: Array[Byte]): Array[Byte] = bytesLeafOf(Hashes.hasher(a), tagged, leaf)

  private def bytesLeafOf[H](h: Hasher[H], tagged: Boolean, leaf: Array[Byte]): Array[Byte] =
    hashing[H](tagged)(using h).leaf(bv(leaf)).bytes.toArray

  def merkleRoot(tagged: Boolean, a: Algorithm, leafHashes: java.util.List[Array[Byte]]): Result[Array[Byte], JMerkleError] =
    merkleRootOf(Hashes.hasher(a), tagged, leafHashes)

  private def merkleRootOf[H](h: Hasher[H], tagged: Boolean, leafHashes: java.util.List[Array[Byte]]): Result[Array[Byte], JMerkleError] = {
    given Hasher[H] = h
    given MerkleHashing[H] = hashing[H](tagged)
    merkle(digests[H](leafHashes).map(ds => MerkleTree.fromLeafHashes[H](ds).root.bytes.toArray))
  }

  private def inclusionOf[H](p: InclusionProof[H]): xyz.kd5ujc.dilf4s.Merkle.InclusionProof =
    new xyz.kd5ujc.dilf4s.Merkle.InclusionProof(p.index, p.size, p.path.map(_.bytes.toArray).asJava)

  private def consistencyOf[H](p: ConsistencyProof[H]): xyz.kd5ujc.dilf4s.Merkle.ConsistencyProof =
    new xyz.kd5ujc.dilf4s.Merkle.ConsistencyProof(p.oldSize, p.newSize, p.path.map(_.bytes.toArray).asJava)

  private def inclusion[H: Hasher](p: xyz.kd5ujc.dilf4s.Merkle.InclusionProof): Either[MerkleError, InclusionProof[H]] =
    digests[H](p.path()).flatMap(InclusionProof.from(p.index(), p.size(), _))

  private def consistency[H: Hasher](p: xyz.kd5ujc.dilf4s.Merkle.ConsistencyProof): Either[MerkleError, ConsistencyProof[H]] =
    digests[H](p.path()).flatMap(ConsistencyProof.from(p.oldSize(), p.newSize(), _))

  def inclusionProof(
    tagged:     Boolean,
    a:          Algorithm,
    leafHashes: java.util.List[Array[Byte]],
    index:      Long
  ): Result[xyz.kd5ujc.dilf4s.Merkle.InclusionProof, JMerkleError] = inclusionProofOf(Hashes.hasher(a), tagged, leafHashes, index)

  private def inclusionProofOf[H](h: Hasher[H], tagged: Boolean, leafHashes: java.util.List[Array[Byte]], index: Long) = {
    given Hasher[H] = h
    given MerkleHashing[H] = hashing[H](tagged)
    merkle(digests[H](leafHashes).flatMap(ds => MerkleTree.fromLeafHashes[H](ds).inclusionProof(index)).map(inclusionOf))
  }

  def consistencyProof(
    tagged:     Boolean,
    a:          Algorithm,
    leafHashes: java.util.List[Array[Byte]],
    oldSize:    Long
  ): Result[xyz.kd5ujc.dilf4s.Merkle.ConsistencyProof, JMerkleError] = consistencyProofOf(Hashes.hasher(a), tagged, leafHashes, oldSize)

  private def consistencyProofOf[H](h: Hasher[H], tagged: Boolean, leafHashes: java.util.List[Array[Byte]], oldSize: Long) = {
    given Hasher[H] = h
    given MerkleHashing[H] = hashing[H](tagged)
    merkle(digests[H](leafHashes).flatMap(ds => MerkleTree.fromLeafHashes[H](ds).consistencyProof(oldSize)).map(consistencyOf))
  }

  /** RFC 9162 section 2.1.3.2 under either hashing: `false` for a proof of another root or a path that does not fit. */
  def verifyInclusion(
    tagged:   Boolean,
    a:        Algorithm,
    root:     Array[Byte],
    leafHash: Array[Byte],
    proof:    xyz.kd5ujc.dilf4s.Merkle.InclusionProof
  ): Result[java.lang.Boolean, JMerkleError] = verifyInclusionOf(Hashes.hasher(a), tagged, root, leafHash, proof)

  private def verdict(r: Either[MerkleError, Unit]): Either[MerkleError, java.lang.Boolean] =
    r match {
      case Right(())                                                          => java.lang.Boolean.TRUE.asRight
      case Left(MerkleError.InvalidProof(_) | MerkleError.RootMismatch(_, _)) => java.lang.Boolean.FALSE.asRight
      case Left(other)                                                        => other.asLeft
    }

  private def verifyInclusionOf[H](
    h:        Hasher[H],
    tagged:   Boolean,
    root:     Array[Byte],
    leafHash: Array[Byte],
    proof:    xyz.kd5ujc.dilf4s.Merkle.InclusionProof
  ) = {
    given Hasher[H] = h
    given MerkleHashing[H] = hashing[H](tagged)
    merkle((digest[H](root, "root"), digest[H](leafHash, "leaf hash"), inclusion[H](proof)).tupled.flatMap {
      case (r, l, p) => verdict(MerkleVerifier.inclusion(r, l, p))
    })
  }

  /** RFC 9162 section 2.1.4.2 under either hashing. */
  def verifyConsistency(
    tagged:  Boolean,
    a:       Algorithm,
    oldRoot: Array[Byte],
    newRoot: Array[Byte],
    proof:   xyz.kd5ujc.dilf4s.Merkle.ConsistencyProof
  ): Result[java.lang.Boolean, JMerkleError] = verifyConsistencyOf(Hashes.hasher(a), tagged, oldRoot, newRoot, proof)

  private def verifyConsistencyOf[H](
    h:       Hasher[H],
    tagged:  Boolean,
    oldRoot: Array[Byte],
    newRoot: Array[Byte],
    proof:   xyz.kd5ujc.dilf4s.Merkle.ConsistencyProof
  ) = {
    given Hasher[H] = h
    given MerkleHashing[H] = hashing[H](tagged)
    merkle((digest[H](oldRoot, "old root"), digest[H](newRoot, "new root"), consistency[H](proof)).tupled.flatMap {
      case (o, n, p) => verdict(MerkleVerifier.consistency(o, n, p))
    })
  }

  def encodeInclusion(a: Algorithm, p: xyz.kd5ujc.dilf4s.Merkle.InclusionProof): Result[CanonicalValue, JMerkleError] =
    encodeInclusionOf(Hashes.hasher(a), p)

  private def encodeInclusionOf[H](h: Hasher[H], p: xyz.kd5ujc.dilf4s.Merkle.InclusionProof) = {
    given Hasher[H] = h
    merkle(inclusion[H](p).flatMap(MerkleCodec.encodeInclusion[H]))
  }

  def decodeInclusion(a: Algorithm, v: CanonicalValue): Result[xyz.kd5ujc.dilf4s.Merkle.InclusionProof, JMerkleError] =
    decodeInclusionOf(Hashes.hasher(a), v)

  private def decodeInclusionOf[H](h: Hasher[H], v: CanonicalValue) = merkle(MerkleCodec.decodeInclusion[H](v)(using h).map(inclusionOf))

  def encodeConsistency(a: Algorithm, p: xyz.kd5ujc.dilf4s.Merkle.ConsistencyProof): Result[CanonicalValue, JMerkleError] =
    encodeConsistencyOf(Hashes.hasher(a), p)

  private def encodeConsistencyOf[H](h: Hasher[H], p: xyz.kd5ujc.dilf4s.Merkle.ConsistencyProof) = {
    given Hasher[H] = h
    merkle(consistency[H](p).flatMap(MerkleCodec.encodeConsistency[H]))
  }

  def decodeConsistency(a: Algorithm, v: CanonicalValue): Result[xyz.kd5ujc.dilf4s.Merkle.ConsistencyProof, JMerkleError] =
    decodeConsistencyOf(Hashes.hasher(a), v)

  private def decodeConsistencyOf[H](h: Hasher[H], v: CanonicalValue) = merkle(
    MerkleCodec.decodeConsistency[H](v)(using h).map(consistencyOf)
  )

  /** `merkle_verify`: whether `leaf` is at the proof's index of the dilf4s tree `(root, size)` (`MerkleCodec.verifyLeaf`). */
  def verifyLeaf(
    a:     Algorithm,
    root:  Array[Byte],
    size:  Long,
    leaf:  CanonicalValue,
    proof: CanonicalValue
  ): Result[java.lang.Boolean, JMerkleError] =
    verifyLeafOf(Hashes.hasher(a), root, size, leaf, proof)

  private def verifyLeafOf[H](h: Hasher[H], root: Array[Byte], size: Long, leaf: CanonicalValue, proof: CanonicalValue) = {
    given Hasher[H] = h
    merkle(
      digest[H](root, "head.root")
        .flatMap(r => MerkleCodec.verifyLeaf[H](r, size, leaf, proof)(using h, MerkleHashing.tagged[H]))
        .map(java.lang.Boolean.valueOf)
    )
  }

  // ---- binary Patricia tries (docs/specs/trie.md), tagged hashing

  def fixed(bytes:       Int): Result[KeyMode, JTrieError] = trie(KeyMode.fixed(bytes))
  def variable(maxBytes: Int): Result[KeyMode, JTrieError] = trie(KeyMode.variable(maxBytes))
  def describe(m:        KeyMode): String = m.toString

  def isFixed(m: KeyMode): Boolean =
    m match {
      case _: KeyMode.Fixed    => true
      case _: KeyMode.Variable => false
    }

  def modeBytes(m: KeyMode): Int =
    m match {
      case f: KeyMode.Fixed    => f.bytes
      case v: KeyMode.Variable => v.maxBytes
    }

  /** A value's hash in a trie: the tagged hash of its commitment. */
  def trieValue(a: Algorithm, v: CanonicalValue): Result[Array[Byte], EncodeError] = trieValueOf(Hashes.hasher(a), v)

  private def trieValueOf[H](h: Hasher[H], v: CanonicalValue): Result[Array[Byte], EncodeError] =
    Values.result(ValueCommitment.source(v).bimap(e => new TooLarge(e.limit), TrieHashing.tagged[H](using h).valueOf(_).bytes.toArray))

  def trieRoot(
    a:       Algorithm,
    m:       KeyMode,
    entries: java.util.List[java.util.Map.Entry[Array[Byte], Array[Byte]]]
  ): Result[Array[Byte], JTrieError] =
    trieRootOf(Hashes.hasher(a), m, entries)

  private def trieRootOf[H](h: Hasher[H], m: KeyMode, entries: java.util.List[java.util.Map.Entry[Array[Byte], Array[Byte]]]) = {
    given Hasher[H] = h
    trie(
      entries.asScala.toList
        .traverse(e =>
          Digest.fromBytes[H](bv(e.getValue)).bimap(x => TrieError.Encoding(s"a value hash: $x"): TrieError, bv(e.getKey) -> _)
        )
        .flatMap(Trie.fromEntries[H](m, _)(using TrieHashing.tagged[H]))
        .map(_.root.bytes.toArray)
    )
  }

  /** A trie proof whose root has been checked: lookups, prefix listings and range listings over what it covers. */
  final class VerifiedTrie private[Proofs] (
    lookupOf: Array[Byte] => Either[TrieError, xyz.kd5ujc.dilf4s.Trie.Lookup],
    prefixOf: Array[Byte] => Option[List[(ByteVector, Array[Byte])]],
    rangeOf:  (Array[Byte], Option[Array[Byte]]) => Option[List[(ByteVector, Array[Byte])]]
  ) {
    private def listing(l: Option[List[(ByteVector, Array[Byte])]]): java.util.Optional[java.util.List[xyz.kd5ujc.dilf4s.Trie.Entry]] =
      l.fold(java.util.Optional.empty[java.util.List[xyz.kd5ujc.dilf4s.Trie.Entry]]())(es =>
        java.util.Optional.of(es.map { case (k, h) => new xyz.kd5ujc.dilf4s.Trie.Entry(k.toArray, h) }.asJava)
      )

    def lookup(key: Array[Byte]): Result[xyz.kd5ujc.dilf4s.Trie.Lookup, JTrieError] = trie(lookupOf(key))
    def prefix(p:   Array[Byte]): java.util.Optional[java.util.List[xyz.kd5ujc.dilf4s.Trie.Entry]] = listing(prefixOf(p))
    def range(from: Array[Byte], until: java.util.Optional[Array[Byte]]): java.util.Optional[java.util.List[xyz.kd5ujc.dilf4s.Trie.Entry]] =
      listing(rangeOf(from, Option.when(until.isPresent)(until.orElse(Array.emptyByteArray))))
  }

  /** `TrieProofs.verify`: the proof decoded and its root recomputed under `m`; another root is `RootMismatch`. */
  def trieCheck(a: Algorithm, m: KeyMode, root: Array[Byte], proof: CanonicalValue): Result[VerifiedTrie, JTrieError] =
    trieCheckOf(Hashes.hasher(a), m, root, proof)

  private def trieCheckOf[H](h: Hasher[H], m: KeyMode, root: Array[Byte], proof: CanonicalValue): Result[VerifiedTrie, JTrieError] = {
    given Hasher[H] = h
    given TrieHashing[H] = TrieHashing.tagged[H]
    def entries(l: List[(scodec.bits.BitVector, Digest[H])]): List[(ByteVector, Array[Byte])] = l.map {
      case (p, d) => p.toByteVector -> d.bytes.toArray
    }
    trie(
      for {
        r <- Digest.fromBytes[H](bv(root)).leftMap(e => TrieError.Encoding(s"root: $e"): TrieError)
        p <- TrieProofs.decode[H](proof)
        v <- TrieProofs.verify(m, r, p)
      } yield
        new VerifiedTrie(
          key =>
            m.path(bv(key))
              .map(v.lookup(_) match {
                case xyz.kd5ujc.accumulators.trie.Lookup.Present(d) => new xyz.kd5ujc.dilf4s.Trie.Lookup.Present(d.bytes.toArray)
                case xyz.kd5ujc.accumulators.trie.Lookup.Absent     => new xyz.kd5ujc.dilf4s.Trie.Lookup.Absent()
                case xyz.kd5ujc.accumulators.trie.Lookup.NotCovered => new xyz.kd5ujc.dilf4s.Trie.Lookup.NotCovered()
              }),
          pre => v.prefix(bv(pre).bits).map(entries),
          (from, until) => v.range(bv(from).bits, until.map(bv(_).bits)).map(entries)
        )
    )
  }

  def trieVerify(
    a:     Algorithm,
    m:     KeyMode,
    root:  Array[Byte],
    key:   Array[Byte],
    claim: java.util.Optional[CanonicalValue],
    proof: CanonicalValue
  ): Result[java.lang.Boolean, JTrieError] = trieVerifyOf(Hashes.hasher(a), m, root, key, claim, proof)

  private def trieVerifyOf[H](
    h:     Hasher[H],
    m:     KeyMode,
    root:  Array[Byte],
    key:   Array[Byte],
    claim: java.util.Optional[CanonicalValue],
    proof: CanonicalValue
  ) =
    trie(
      TrieProofs
        .verifyClaim[H](m, bv(root), bv(key), Option.when(claim.isPresent)(claim.orElse(CanonicalValue.CNull)), proof)(
          using h,
          TrieHashing.tagged[H](using h)
        )
        .map(java.lang.Boolean.valueOf)
    )

  private def pairs(entries: java.util.List[java.util.Map.Entry[Array[Byte], CanonicalValue]]): List[(ByteVector, CanonicalValue)] =
    entries.asScala.toList.map(e => bv(e.getKey) -> e.getValue)

  def triePrefixVerify(
    a:       Algorithm,
    m:       KeyMode,
    root:    Array[Byte],
    prefix:  Array[Byte],
    entries: java.util.List[java.util.Map.Entry[Array[Byte], CanonicalValue]],
    proof:   CanonicalValue
  ): Result[java.lang.Boolean, JTrieError] = triePrefixOf(Hashes.hasher(a), m, root, prefix, entries, proof)

  private def triePrefixOf[H](
    h:       Hasher[H],
    m:       KeyMode,
    root:    Array[Byte],
    prefix:  Array[Byte],
    entries: java.util.List[java.util.Map.Entry[Array[Byte], CanonicalValue]],
    proof:   CanonicalValue
  ) =
    trie(
      TrieProofs
        .verifyPrefixClaims[H](m, bv(root), bv(prefix), pairs(entries), proof)(using h, TrieHashing.tagged[H](using h))
        .map(java.lang.Boolean.valueOf)
    )

  /**
   * `trie_range_verify`: whether `entries`, in key order, are exactly the trie's entries in `[from, until)`, checked in
   * the order of `verifyPrefixClaims`: each entry's key against the mode, the claims' weight, then the root, the proof.
   */
  def trieRangeVerify(
    a:       Algorithm,
    m:       KeyMode,
    root:    Array[Byte],
    from:    Array[Byte],
    until:   java.util.Optional[Array[Byte]],
    entries: java.util.List[java.util.Map.Entry[Array[Byte], CanonicalValue]],
    proof:   CanonicalValue
  ): Result[java.lang.Boolean, JTrieError] = trieRangeOf(Hashes.hasher(a), m, root, from, until, entries, proof)

  private def trieRangeOf[H](
    h:       Hasher[H],
    m:       KeyMode,
    root:    Array[Byte],
    from:    Array[Byte],
    until:   java.util.Optional[Array[Byte]],
    entries: java.util.List[java.util.Map.Entry[Array[Byte], CanonicalValue]],
    proof:   CanonicalValue
  ) = {
    given Hasher[H] = h
    given TrieHashing[H] = TrieHashing.tagged[H]
    val upper = Option.when(until.isPresent)(until.orElse(Array.emptyByteArray)).map(bv(_).bits)
    trie(
      for {
        paths <- pairs(entries).traverse {
          case (k, v) =>
            if (v == CanonicalValue.CNull) (TrieError.Encoding("an entry is [key, value], with a value"): TrieError).asLeft
            else m.path(k).map(_ -> v)
        }
        _ <- ProofInput.claimed(paths.map(_._2))(TrieError.TooLarge(_))
        claimed <- paths.traverse {
          case (p, v) =>
            ValueCommitment.source(v).bimap(e => TrieError.TooLarge(e.limit): TrieError, s => p -> summon[TrieHashing[H]].valueOf(s))
        }
        r <- Digest.fromBytes[H](bv(root)).leftMap(e => TrieError.Encoding(s"root: $e"): TrieError)
        p <- TrieProofs.decode[H](proof)
        v <- TrieProofs.verify(m, r, p) match {
          case Right(v)                           => Option(v).asRight
          case Left(TrieError.RootMismatch(_, _)) => None.asRight
          case Left(other)                        => other.asLeft
        }
        listed <- v.fold(false.asRight[TrieError])(_.listsRange(bv(from).bits, upper, claimed))
      } yield java.lang.Boolean.valueOf(listed)
    )
  }

  // ---- versioned tries (docs/specs/versioned-trie.md), versions as unsigned integers of a fixed width

  private def versioned[A](e: Either[VersionedTrieError, A]): Result[A, JVersionedTrieError] =
    Values.result(e.left.map(Errors.versionedTrieError))

  def stateKey(s:  String): Result[String, JStateKeyError] = Values.result(StateKey.parse(s).bimap(Errors.stateKeyError, _.value))
  def namespace(s: String): Result[String, JStateKeyError] = Values.result(StateNamespace.parse(s).bimap(Errors.stateKeyError, _.value))

  def versionKey(width: Int, version: Long): Result[Array[Byte], JVersionedTrieError] =
    versioned(VersionKey.unsignedLong(width).flatMap(_.encode(version)).map(_.toArray))

  def versionOf(width: Int, key: Array[Byte]): Result[java.lang.Long, JVersionedTrieError] =
    versioned(VersionKey.unsignedLong(width).flatMap(_.decode(bv(key))).map(java.lang.Long.valueOf))

  /** A verified state proof: lookups and listings against the proven state. */
  final class State private[Proofs] (
    lookupOf:    StateKey => Either[VersionedTrieError, Option[Array[Byte]]],
    holdsOf:     (StateKey, Option[CanonicalValue]) => Either[VersionedTrieError, Boolean],
    namespaceOf: StateNamespace => Either[VersionedTrieError, List[(String, Array[Byte])]],
    rangeOf:     (StateKey, Option[StateKey]) => Either[VersionedTrieError, List[(String, Array[Byte])]]
  ) {
    private def key(k: String): Either[VersionedTrieError, StateKey] = StateKey.parse(k).leftMap(VersionedTrieError.Key(_))

    private def listing(l: List[(String, Array[Byte])]): java.util.List[java.util.Map.Entry[String, Array[Byte]]] =
      l.map { case (k, h) => java.util.Map.entry(k, h) }.asJava

    def lookup(k: String): Result[java.util.Optional[Array[Byte]], JVersionedTrieError] =
      versioned(key(k).flatMap(lookupOf).map(_.fold(java.util.Optional.empty[Array[Byte]]())(java.util.Optional.of)))

    def holds(k: String, value: java.util.Optional[CanonicalValue]): Result[java.lang.Boolean, JVersionedTrieError] =
      versioned(key(k).flatMap(holdsOf(_, Option.when(value.isPresent)(value.orElse(CanonicalValue.CNull)))).map(java.lang.Boolean.valueOf))

    def namespace(ns: String): Result[java.util.List[java.util.Map.Entry[String, Array[Byte]]], JVersionedTrieError] =
      versioned(StateNamespace.parse(ns).leftMap(VersionedTrieError.Key(_)).flatMap(namespaceOf).map(listing))

    def range(
      from:  String,
      until: java.util.Optional[String]
    ): Result[java.util.List[java.util.Map.Entry[String, Array[Byte]]], JVersionedTrieError] =
      versioned(
        (key(from), Option.when(until.isPresent)(until.orElse("")).traverse(key)).tupled.flatMap { case (f, u) => rangeOf(f, u) }
          .map(listing)
      )
  }

  private def state[H](s: VerifiedState[H]): State = {
    def listed(l: List[(StateKey, Digest[H])]): List[(String, Array[Byte])] = l.map { case (k, d) => k.value -> d.bytes.toArray }
    new State(
      k => s.lookup(k).map(_.map(_.bytes.toArray)),
      (k, v) => s.holds(k, v),
      ns => s.namespace(ns).map(listed),
      (f, u) => s.range(f, u).map(listed)
    )
  }

  private def withVersions[H, A](width: Int, root: Array[Byte], proof: CanonicalValue)(
    f: (VersionKey[Long], Digest[H], VersionedTrieProof[Long, H]) => Either[VersionedTrieError, A]
  )(using h: Hasher[H]): Either[VersionedTrieError, A] =
    for {
      v <- VersionKey.unsignedLong(width)
      r <- Digest
        .fromBytes[H](bv(root))
        .leftMap(e => VersionedTrieError.Malformed(MalformedReason.Digest(e, DigestSubject.Root)): VersionedTrieError)
      p <- VersionedTrieWire.decodeProof[Long, H](proof)(using v, h)
      x <- f(v, r, p)
    } yield x

  def verifyState(width: Int, a: Algorithm, root: Array[Byte], proof: CanonicalValue): Result[State, JVersionedTrieError] =
    verifyStateOf(Hashes.hasher(a), width, root, proof)

  private def verifyStateOf[H](h: Hasher[H], width: Int, root: Array[Byte], proof: CanonicalValue) = {
    given Hasher[H] = h
    given TrieHashing[H] = TrieHashing.tagged[H]
    versioned(withVersions[H, State](width, root, proof)((v, r, p) => VersionedTrieVerifier.verifyState[Long, H](r, p)(using v).map(state)))
  }

  def verifyHistoricalKey(
    width:   Int,
    a:       Algorithm,
    root:    Array[Byte],
    proof:   CanonicalValue,
    version: Long
  ): Result[State, JVersionedTrieError] =
    verifyHistoricalOf(Hashes.hasher(a), width, root, proof, version)

  private def verifyHistoricalOf[H](h: Hasher[H], width: Int, root: Array[Byte], proof: CanonicalValue, version: Long) = {
    given Hasher[H] = h
    given TrieHashing[H] = TrieHashing.tagged[H]
    versioned(
      withVersions[H, State](width, root, proof)((v, r, p) =>
        VersionedTrieVerifier.verifyHistoricalKey[Long, H](r, p, version)(using v).map(state)
      )
    )
  }

  def verifyHead(
    width: Int,
    a:     Algorithm,
    root:  Array[Byte],
    proof: CanonicalValue
  ): Result[xyz.kd5ujc.dilf4s.VersionedTrie.Head, JVersionedTrieError] =
    verifyHeadOf(Hashes.hasher(a), width, root, proof)

  private def verifyHeadOf[H](h: Hasher[H], width: Int, root: Array[Byte], proof: CanonicalValue) = {
    given Hasher[H] = h
    given TrieHashing[H] = TrieHashing.tagged[H]
    versioned(
      withVersions[H, xyz.kd5ujc.dilf4s.VersionedTrie.Head](width, root, proof)((v, r, p) =>
        VersionedTrieVerifier
          .verifyHead[Long, H](r, p)(using v)
          .map(hd => new xyz.kd5ujc.dilf4s.VersionedTrie.Head(hd.version, hd.stateRoot.bytes.toArray, hd.historyRoot.bytes.toArray))
      )
    )
  }

  def verifyVersion(
    width:   Int,
    a:       Algorithm,
    root:    Array[Byte],
    proof:   CanonicalValue,
    version: Long
  ): Result[java.util.Optional[Array[Byte]], JVersionedTrieError] =
    verifyVersionOf(Hashes.hasher(a), width, root, proof, version)

  private def verifyVersionOf[H](h: Hasher[H], width: Int, root: Array[Byte], proof: CanonicalValue, version: Long) = {
    given Hasher[H] = h
    given TrieHashing[H] = TrieHashing.tagged[H]
    versioned(
      withVersions[H, java.util.Optional[Array[Byte]]](width, root, proof)((v, r, p) =>
        VersionedTrieVerifier
          .verifyVersion[Long, H](r, p, version)(using v)
          .map(_.fold(java.util.Optional.empty[Array[Byte]]())(d => java.util.Optional.of(d.bytes.toArray)))
      )
    )
  }

  def verifyVersions(
    width:   Int,
    a:       Algorithm,
    root:    Array[Byte],
    proof:   CanonicalValue,
    from:    Long,
    until:   java.util.OptionalLong,
    claimed: java.util.List[java.util.Map.Entry[java.lang.Long, Array[Byte]]]
  ): Result[java.lang.Boolean, JVersionedTrieError] = verifyVersionsOf(Hashes.hasher(a), width, root, proof, from, until, claimed)

  private def verifyVersionsOf[H](
    h:       Hasher[H],
    width:   Int,
    root:    Array[Byte],
    proof:   CanonicalValue,
    from:    Long,
    until:   java.util.OptionalLong,
    claimed: java.util.List[java.util.Map.Entry[java.lang.Long, Array[Byte]]]
  ) = {
    given Hasher[H] = h
    given TrieHashing[H] = TrieHashing.tagged[H]
    val upper = Option.when(until.isPresent)(until.orElse(0L))
    versioned(
      withVersions[H, java.lang.Boolean](width, root, proof)((v, r, p) =>
        claimed.asScala.toList
          .traverse(e =>
            Digest
              .fromBytes[H](bv(e.getValue))
              .bimap(
                x => VersionedTrieError.Malformed(MalformedReason.Digest(x, DigestSubject.StateRoot)): VersionedTrieError,
                e.getKey.longValue -> _
              )
          )
          .flatMap(cs => VersionedTrieVerifier.verifyVersions[Long, H](r, p, from, upper, cs)(using v))
          .map(java.lang.Boolean.valueOf)
      )
    )
  }

  def verifyAsOf(
    width:   Int,
    a:       Algorithm,
    root:    Array[Byte],
    proof:   CanonicalValue,
    version: Long
  ): Result[java.util.Optional[java.util.Map.Entry[java.lang.Long, Array[Byte]]], JVersionedTrieError] =
    verifyAsOfOf(Hashes.hasher(a), width, root, proof, version)

  private def verifyAsOfOf[H](h: Hasher[H], width: Int, root: Array[Byte], proof: CanonicalValue, version: Long) = {
    given Hasher[H] = h
    given TrieHashing[H] = TrieHashing.tagged[H]
    versioned(
      withVersions[H, java.util.Optional[java.util.Map.Entry[java.lang.Long, Array[Byte]]]](width, root, proof)((v, r, p) =>
        VersionedTrieVerifier
          .verifyAsOf[Long, H](r, p, version)(using v)
          .map(_.fold(java.util.Optional.empty[java.util.Map.Entry[java.lang.Long, Array[Byte]]]()) {
            case (u, s) => java.util.Optional.of(java.util.Map.entry(java.lang.Long.valueOf(u), s.bytes.toArray))
          })
      )
    )
  }

  // ---- Poseidon over BN254's scalar field, and its Merkle tree (docs/specs/zk.md sections 3 and 4)

  private def poseidon[A](e: Either[xyz.kd5ujc.core.hash.PoseidonError, A]): Result[A, JPoseidonError] =
    Values.result(e.left.map(Errors.poseidonError))
  private def poseidonTree[A](e: Either[xyz.kd5ujc.accumulators.poseidon.PoseidonMerkleError, A]): Result[A, JPoseidonMerkleError] =
    Values.result(e.left.map(Errors.poseidonMerkleError))

  val FrModulus: java.math.BigInteger = Fr.Modulus.bigInteger
  val PoseidonMaxInputs: Int = Poseidon.MaxInputs
  val PoseidonMaxDepth: Int = PoseidonMerkleTree.MaxDepth

  private def fr(f: java.math.BigInteger): Either[xyz.kd5ujc.core.hash.PoseidonError, Fr] = Fr.from(BigInt(f))
  private def frs(fs: java.util.List[java.math.BigInteger]): Either[xyz.kd5ujc.core.hash.PoseidonError, Vector[Fr]] =
    fs.asScala.toVector.traverse(fr)
  private def out(f: Fr): java.math.BigInteger = f.value.bigInteger

  def frCheck(f:  java.math.BigInteger): Result[java.math.BigInteger, JPoseidonError] = poseidon(fr(f).map(out))
  def frReduce(f: java.math.BigInteger): java.math.BigInteger = out(Fr.reduce(BigInt(f)))

  def poseidonHash(inputs: java.util.List[java.math.BigInteger]): Result[java.math.BigInteger, JPoseidonError] =
    poseidon(frs(inputs).flatMap(Poseidon.hash).map(out))

  def poseidonCompress(l: java.math.BigInteger, r: java.math.BigInteger): Result[java.math.BigInteger, JPoseidonError] =
    poseidon((fr(l), fr(r)).mapN(Poseidon.compress).map(out))

  private def proofOf(p: PoseidonMerkleProof): xyz.kd5ujc.dilf4s.Poseidon.MerkleProof =
    new xyz.kd5ujc.dilf4s.Poseidon.MerkleProof(p.position.bigInteger, p.siblings.map(out).asJava)

  /** The reference's proof of `p`: positions and siblings checked (`PoseidonMerkleProof.from`), each sibling in Fr. */
  private def proof(
    p: xyz.kd5ujc.dilf4s.Poseidon.MerkleProof
  ): Either[xyz.kd5ujc.accumulators.poseidon.PoseidonMerkleError, PoseidonMerkleProof] =
    p.siblings()
      .asScala
      .toVector
      .traverse(s => fr(s).leftMap(e => xyz.kd5ujc.accumulators.poseidon.PoseidonMerkleError.Encoding(s"a sibling: ${e.kind}")))
      .flatMap(PoseidonMerkleProof.from(BigInt(p.position()), _))

  def poseidonProofCheck(p: xyz.kd5ujc.dilf4s.Poseidon.MerkleProof): Result[xyz.kd5ujc.dilf4s.Poseidon.MerkleProof, JPoseidonMerkleError] =
    poseidonTree(proof(p).map(proofOf))

  /** `what`, an element of Fr (the reference's types hold no other), or `Encoding` naming it. */
  private def element(what: String, f: java.math.BigInteger): Either[xyz.kd5ujc.accumulators.poseidon.PoseidonMerkleError, Fr] =
    fr(f).leftMap(e => xyz.kd5ujc.accumulators.poseidon.PoseidonMerkleError.Encoding(s"$what: ${e.kind}"))

  def poseidonComputeRoot(
    leaf: java.math.BigInteger,
    p:    xyz.kd5ujc.dilf4s.Poseidon.MerkleProof
  ): Result[java.math.BigInteger, JPoseidonMerkleError] =
    poseidonTree((element("the leaf", leaf), proof(p)).mapN(PoseidonMerkleTree.computeRoot).map(out))

  /** `PoseidonMerkleTree.verifyInclusion`: the leaf, the proof and the root checked first, each as the reference types it. */
  def poseidonVerifyInclusion(
    leaf: java.math.BigInteger,
    p:    xyz.kd5ujc.dilf4s.Poseidon.MerkleProof,
    root: java.math.BigInteger
  ): Result[java.lang.Boolean, JPoseidonMerkleError] =
    poseidonTree(
      (element("the leaf", leaf), proof(p), element("the root", root))
        .mapN(PoseidonMerkleTree.verifyInclusion)
        .map(java.lang.Boolean.valueOf)
    )

  /** `PoseidonMerkleTree.verifyAbsence`: the proof and the root checked first. */
  def poseidonVerifyAbsence(
    p:    xyz.kd5ujc.dilf4s.Poseidon.MerkleProof,
    root: java.math.BigInteger
  ): Result[java.lang.Boolean, JPoseidonMerkleError] =
    poseidonTree((proof(p), element("the root", root)).mapN(PoseidonMerkleTree.verifyAbsence).map(java.lang.Boolean.valueOf))

  def poseidonToValue(p: xyz.kd5ujc.dilf4s.Poseidon.MerkleProof): Result[CanonicalValue, JPoseidonMerkleError] =
    poseidonTree(proof(p).flatMap(PoseidonMerkleProof.toCanonical))

  def poseidonFromValue(v: CanonicalValue): Result[xyz.kd5ujc.dilf4s.Poseidon.MerkleProof, JPoseidonMerkleError] =
    poseidonTree(PoseidonMerkleProof.fromCanonical(v).map(proofOf))

  private def tree(depth: Int, entries: java.util.List[java.util.Map.Entry[java.math.BigInteger, java.math.BigInteger]]) =
    entries.asScala.toList
      .traverse(e =>
        fr(e.getValue)
          .bimap(x => xyz.kd5ujc.accumulators.poseidon.PoseidonMerkleError.Encoding(s"a leaf: ${x.kind}"), BigInt(e.getKey) -> _)
      )
      .flatMap(es => PoseidonMerkleTree.empty(depth).flatMap(_.insertAll(es)))

  def poseidonTreeRoot(
    depth:   Int,
    entries: java.util.List[java.util.Map.Entry[java.math.BigInteger, java.math.BigInteger]]
  ): Result[java.math.BigInteger, JPoseidonMerkleError] =
    poseidonTree(tree(depth, entries).map(t => out(t.root)))

  def poseidonTreeProof(
    depth:    Int,
    entries:  java.util.List[java.util.Map.Entry[java.math.BigInteger, java.math.BigInteger]],
    position: java.math.BigInteger
  ): Result[xyz.kd5ujc.dilf4s.Poseidon.MerkleProof, JPoseidonMerkleError] =
    poseidonTree(tree(depth, entries).flatMap(_.proof(BigInt(position))).map(proofOf))

  def poseidonZeroHashes(depth: Int): Result[java.util.List[java.math.BigInteger], JPoseidonMerkleError] =
    poseidonTree(PoseidonMerkleTree.empty(depth).map(_ => PoseidonMerkleTree.zeroHashes(depth).map(out).asJava))

  // ---- BN254 (EIP-196/197) on the reference engine, and SP1's Groth16 wrapper (docs/specs/zk.md sections 1 and 2)

  private def zk[A](e: Either[xyz.kd5ujc.crypto.zk.ZkError, A]): Result[A, JZkError] = Values.result(e.left.map(Errors.zkError))

  private val engine: Bn254Engine = Bn254Engine.reference

  def bn254Add(a: Array[Byte], b: Array[Byte]): Result[Array[Byte], JZkError] = zk(engine.add(bv(a), bv(b)).map(_.toArray))
  def bn254Mul(p: Array[Byte], k: Array[Byte]): Result[Array[Byte], JZkError] = zk(engine.mul(bv(p), bv(k)).map(_.toArray))

  def bn254Pairing(pairs: java.util.List[java.util.Map.Entry[Array[Byte], Array[Byte]]]): Result[java.lang.Boolean, JZkError] =
    zk(engine.pairing(pairs.asScala.toVector.map(e => bv(e.getKey) -> bv(e.getValue))).map(java.lang.Boolean.valueOf))

  def sp1Verify(programVKey: Array[Byte], publicValues: Array[Byte], proof: Array[Byte]): Result[Verified, JGroth16Error] =
    Values.result(
      Sp1Groth16.verify(engine)(bv(programVKey), bv(publicValues), bv(proof)).bimap(Errors.groth16Error, _ => Verified.VERIFIED)
    )

  def sp1HashPublicValues(publicValues: Array[Byte]): java.math.BigInteger = Sp1Groth16.hashPublicValues(bv(publicValues)).bigInteger

  val Sp1ProofLength: Long = Sp1Groth16.ProofLength
  val Sp1VkRoot: java.math.BigInteger = Sp1Groth16.VkRoot.bigInteger
}
