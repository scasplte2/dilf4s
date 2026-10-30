package xyz.kd5ujc.accumulators.schemas

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.syntax.all._

import scala.util.Random

import xyz.kd5ujc.accumulators.merkle.MerkleCodec
import xyz.kd5ujc.accumulators.trie.TrieProofs
import xyz.kd5ujc.core.canonical.{Cbor, Codec}
import xyz.kd5ujc.core.hash.Sha256
import xyz.kd5ujc.models.merkle.{MerkleConsistency, MerkleInclusion}
import xyz.kd5ujc.models.poseidon.PoseidonMerkleProof
import xyz.kd5ujc.models.trie.{TrieChildren, TrieProof, TrieSegment}
import xyz.kd5ujc.modelsgen.{CddlParser, CddlSampler, QName, Schema}

import io.circe.{Json, parser}
import scodec.bits.ByteVector
import weaver.SimpleIOSuite

/**
 * The generated wire models against everything else that defines the same bytes: the committed conformance vectors
 * (produced by the hand-written codecs), the hand-written decoders, CDDL-sampled instances and the specs' embedded
 * schemas. Generated decoders check shape only; the domain decoders remain the validators.
 */
object SchemaConformanceSuite extends SimpleIOSuite {

  private def read(path: String*): String = new String(Files.readAllBytes(Paths.get(path.mkString("/"))), UTF_8)
  private def json(path: String*): Json = parser.parse(read(path*)).toOption.get

  /** decode -> generated decode -> generated encode gives back the same bytes. */
  private def roundTrip[A](codec: Codec[A], bytes: ByteVector): Either[String, ByteVector] =
    for {
      value <- Cbor.decode(bytes).leftMap(_.toString)
      a     <- codec.decoder.decode(value).leftMap(_.message)
      again <- codec.encode(a).leftMap(_.toString)
    } yield Cbor.encode(again)

  pureTest("every Merkle proof in merkle_v1.json round-trips byte-identically and stays valid for the domain decoder") {
    val trees = json("shared", "vectors", "merkle_v1.json").hcursor.downField("trees").as[List[Json]].toOption.get
    def proofs(kind: String) = trees.flatMap(_.hcursor.downField(kind).as[List[String]].toOption.get).map(ByteVector.fromValidHex(_))
    val (inc, con) = (proofs("inclusion"), proofs("consistency"))
    expect(inc.size >= 30 && con.size >= 40, s"${inc.size} inclusion, ${con.size} consistency proofs")
      .and(inc.foldMap(b => expect.same(Right(b), roundTrip(MerkleInclusion.codec, b))))
      .and(con.foldMap(b => expect.same(Right(b), roundTrip(MerkleConsistency.codec, b))))
      .and(
        inc.foldMap(b =>
          expect(
            roundTrip(MerkleInclusion.codec, b)
              .flatMap(Cbor.decode(_).leftMap(_.toString))
              .flatMap(MerkleCodec.decodeInclusion[Sha256](_).leftMap(_.toString))
              .isRight
          )
        )
      )
  }

  pureTest("every trie proof in trie_v1.json round-trips byte-identically and stays valid for the domain decoder") {
    val fixtures = json("shared", "vectors", "trie_v1.json").hcursor.downField("fixtures").as[List[Json]].toOption.get
    val proofs = fixtures.map(f => ByteVector.fromValidHex(f.hcursor.downField("proof").get[String]("cbor").toOption.get))
    expect(proofs.nonEmpty, "trie fixtures present")
      .and(proofs.foldMap(b => expect.same(Right(b), roundTrip(TrieProof.codec, b))))
      .and(
        proofs.foldMap(b => expect(Cbor.decode(b).leftMap(_.toString).flatMap(TrieProofs.decode[Sha256](_).leftMap(_.toString)).isRight))
      )
  }

  private val schemas = List("merkle", "trie", "poseidon").map(m => m -> read("shared", "schemas", s"$m.cddl"))

  private val codecs: Map[String, Codec[?]] = Map(
    "merkle-inclusion"      -> MerkleInclusion.codec,
    "merkle-consistency"    -> MerkleConsistency.codec,
    "trie-proof"            -> TrieProof.codec,
    "trie-segment"          -> TrieSegment.codec,
    "trie-children"         -> TrieChildren.codec,
    "poseidon-merkle-proof" -> PoseidonMerkleProof.codec
  )

  /** Samples must be canonical CBOR the generated decoder accepts and re-encodes exactly: a rejection is a bug. */
  pureTest("CDDL-sampled instances of every rule decode and re-encode byte-identically (no filtering)") {
    val schema = Schema.build(schemas.toVector.map { case (m, text) => m -> Vector(CddlParser.parse(text).toOption.get) })
    val sampler = new CddlSampler(schema, maxDepth = 5)
    codecs.toList.foldMap {
      case (rule, codec) =>
        val module = rule.takeWhile(_ != '-')
        val rnd = new Random(20261029L + rule.hashCode.toLong)
        (1 to 200).toList.foldMap { _ =>
          val sample = sampler.sample(QName(module, rule), rnd).map(ByteVector(_)).leftMap(_.message)
          expect.same(sample, sample.flatMap(roundTrip(codec, _))).and(expect(sample.isRight, s"$rule sampled"))
        }
    }
  }

  pureTest("each spec embeds its schema verbatim") {
    List("merkle" -> "merkle.md", "trie" -> "trie.md", "poseidon" -> "zk.md").foldMap {
      case (m, spec) =>
        val schema = read("shared", "schemas", s"$m.cddl").trim
        val blocks = "(?s)```cddl\\n(.*?)```".r.findAllMatchIn(read("docs", "specs", spec)).map(_.group(1).trim).toList
        expect(blocks.contains(schema), s"docs/specs/$spec embeds shared/schemas/$m.cddl")
    }
  }

  pureTest("encoders refuse values outside the schema") {
    expect(MerkleInclusion.codec.encode(MerkleInclusion(-1L, 1L, Vector.empty)).isLeft, "negative index")
      .and(expect(TrieSegment.codec.encode(TrieSegment(-5L, ByteVector.empty)).isLeft, "negative bit length"))
      .and(expect.same(Right(xyz.kd5ujc.core.canonical.CanonicalValue.int(3)), TrieChildren.codec.encode(TrieChildren.Both)))
  }

}
