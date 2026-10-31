package xyz.kd5ujc.models

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.syntax.all._

import xyz.kd5ujc.core.canonical.{CanonicalJson, Codec}

import weaver.SimpleIOSuite

/**
 * Every committed conformance vector file conforms to its schema (shared/schemas/<name>-vectors.cddl), and the
 * generated types hold all of it: decode then encode gives back the same canonical JSON, so nothing in a vector file
 * escapes its schema.
 */
object VectorSchemasSuite extends SimpleIOSuite {

  private def read(path: String): String = new String(Files.readAllBytes(Paths.get(path)), UTF_8)

  private val files: List[(String, Codec[?])] = List(
    "canonical_encoding_v1.json" -> canonical_vectors.CanonicalVectors.codec,
    "merkle_v1.json"             -> merkle_vectors.MerkleVectors.codec,
    "trie_v1.json"               -> trie_vectors.TrieVectors.codec,
    "poseidon_v1.json"           -> poseidon_vectors.PoseidonVectors.codec,
    "jlvm_v1.json"               -> jlvm_vectors.JlvmVectors.codec,
    "signing_v1.json"            -> signing_vectors.SigningVectors.codec,
    "vrf_v1.json"                -> vrf_vectors.VrfVectors.codec,
    "kes_v1.json"                -> kes_vectors.KesVectors.codec,
    "sigma_v1.json"              -> sigma_vectors.SigmaVectors.codec,
    "sigma_dilf4s_v1.json"       -> sigma_vectors.SigmaVectors.codec,
    "versioned_trie_v1.json"     -> versioned_trie_vectors.VersionedTrieVectors.codec,
    "zk_v1.json"                 -> zk_vectors.ZkVectors.codec
  )

  private def lossless[A](codec: Codec[A], json: String): Either[String, Boolean] =
    for {
      value   <- CanonicalJson.parse(json).leftMap(_.toString)
      decoded <- codec.decoder.decode(value).leftMap(_.message)
      again   <- codec.encode(decoded).leftMap(_.toString)
      before  <- CanonicalJson.render(value).leftMap(_.toString)
      after   <- CanonicalJson.render(again).leftMap(_.toString)
    } yield before == after

  pureTest("every vector file in shared/vectors has a schema") {
    val present = Option(Paths.get("shared", "vectors").toFile.list()).fold(List.empty[String])(_.toList.filter(_.endsWith(".json")))
    expect.same(present.sorted, files.map(_._1).sorted)
  }

  files.foreach {
    case (file, codec) =>
      pureTest(s"$file conforms to its schema and round-trips losslessly") {
        expect.same(Right(true), lossless(codec, read(s"shared/vectors/$file")))
      }
  }

  pureTest("schema violations are reported with the failing path") {
    val bad = read("shared/vectors/merkle_v1.json").replaceFirst("\"leaves\"\\s*:\\s*\\[\\s*\"", "\"leaves\": [\"zz")
    val result =
      CanonicalJson.parse(bad).leftMap(_.toString).flatMap(merkle_vectors.MerkleVectors.codec.decoder.decode(_).leftMap(_.message))
    expect(result.left.exists(_.startsWith("leaves.[0]")), result.left.toOption.getOrElse("decoded"))
  }
}
