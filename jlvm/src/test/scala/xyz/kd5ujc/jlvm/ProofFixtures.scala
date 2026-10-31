package xyz.kd5ujc.jlvm

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.syntax.all._

import xyz.kd5ujc.accumulators.ValueCommitment
import xyz.kd5ujc.accumulators.merkle.MerkleTree
import xyz.kd5ujc.accumulators.trie.{KeyMode, Trie, TrieHashing, TrieProofs}
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}
import xyz.kd5ujc.core.hash.Sha256

import io.circe.{ACursor, Json}
import scodec.bits.ByteVector

/** Arguments for the proof operations, built from the committed conformance vectors (merkle_v1, trie_v1). */
object ProofFixtures {

  def vector(name: String): IO[Json] =
    IO.blocking(new String(Files.readAllBytes(Paths.get("shared", "vectors", name)), UTF_8))
      .flatMap(t => io.circe.parser.parse(t).liftTo[IO])

  def get[A](e:    Either[?, A]): A = e.fold(err => sys.error(err.toString), identity)
  def hex(s:       String): ByteVector = get(ByteVector.fromHexDescriptive(s))
  def cbor(s:      String): CanonicalValue = get(Cbor.decode(hex(s)))
  def str(c:       ACursor): String = get(c.as[String])
  def m(kvs:       (String, CanonicalValue)*): CanonicalValue = get(CanonicalValue.map(kvs))
  def a(vs:        CanonicalValue*): CanonicalValue = get(CanonicalValue.array(vs.toVector))
  def headOf(root: String, size: Long): CanonicalValue = m("root" -> CBytes(hex(root)), "size" -> CInt(BigInt(size)))

  /**
   * merkle_v1 and trie_v1 are built with the byte-level APIs. Programs verify value-committed structures, so the
   * Merkle and trie tests rebuild the vectors' trees from their leaves and entries with ValueCommitment, proving the
   * same queries; the consistency test uses the vectors' heads as they are.
   */
  def committedTree(leaves: List[ByteVector], n: Long): MerkleTree[Sha256] =
    get(MerkleTree.fromLeaves[Sha256, CanonicalValue](leaves.take(n.toInt).map(CBytes(_))))

  def mode(j: Json): CanonicalValue =
    j.hcursor
      .downField("fixed")
      .as[Int]
      .map(n => m("fixed" -> CInt(BigInt(n))))
      .orElse(j.hcursor.downField("variable").as[Int].map(n => m("variable" -> CInt(BigInt(n)))))
      .fold(e => sys.error(e.toString), identity)

  def keyMode(fx: Json): KeyMode =
    fx.hcursor
      .downField("mode")
      .downField("fixed")
      .as[Int]
      .map(KeyMode.fixed)
      .orElse(
        fx.hcursor.downField("mode").downField("variable").as[Int].map(KeyMode.variable)
      )
      .fold(e => sys.error(e.toString), r => get(r))

  /** A trie_v1 fixture rebuilt with its values committed as programs claim them, proving the fixture's own queries. */
  def committedTrie(fx: Json): Trie[Sha256] =
    get(
      Trie.fromEntries[Sha256](
        keyMode(fx),
        values(fx).toList.map { case (k, v) => hex(k) -> TrieHashing[Sha256].value(ValueCommitment.bytes(CBytes(v))) }
      )
    )

  def rootOf(fx: Json): CanonicalValue = CBytes(committedTrie(fx).root.bytes)

  def proofOf(fx: Json): CanonicalValue = {
    val q = fx.hcursor.downField("proof")
    get(
      committedTrie(fx)
        .prove(
          get(q.downField("keys").as[List[String]]).map(hex),
          get(q.downField("prefixes").as[List[String]]).map(hex),
          get(q.downField("ranges").as[List[Json]]).map(r => hex(str(r.hcursor.downField("from"))) -> rangeEnd(r))
        )
        .flatMap(TrieProofs.encode(_))
    )
  }

  def rangeEnd(r: Json): Option[ByteVector] = r.hcursor.downField("until").as[String].toOption.map(hex)

  def values(fx: Json): Map[String, ByteVector] =
    get(fx.hcursor.downField("entries").as[List[Json]])
      .map(e => str(e.hcursor.downField("key")) -> hex(str(e.hcursor.downField("value"))))
      .toMap
}
