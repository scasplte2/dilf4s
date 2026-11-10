package xyz.kd5ujc.models

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import xyz.kd5ujc.core.canonical.{CanonicalError, CanonicalJson, Codec, DecodeFailure, DecodeReason, Preview}

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
    "diagnostics_v1.json"        -> diagnostic_vectors.DiagnosticVectors.codec,
    "merkle_v1.json"             -> merkle_vectors.MerkleVectors.codec,
    "trie_v1.json"               -> trie_vectors.TrieVectors.codec,
    "poseidon_v1.json"           -> poseidon_vectors.PoseidonVectors.codec,
    "refusals_v1.json"           -> refusal_vectors.RefusalVectors.codec,
    "jlvm_v1.json"               -> jlvm_vectors.JlvmVectors.codec,
    "signing_v1.json"            -> signing_vectors.SigningVectors.codec,
    "vrf_v1.json"                -> vrf_vectors.VrfVectors.codec,
    "kes_v1.json"                -> kes_vectors.KesVectors.codec,
    "sigma_v1.json"              -> sigma_vectors.SigmaVectors.codec,
    "sigma_dilf4s_v1.json"       -> sigma_vectors.SigmaVectors.codec,
    "sigma_refusals_v1.json"     -> sigma_refusal_vectors.SigmaRefusalVectors.codec,
    "versioned_trie_v1.json"     -> versioned_trie_vectors.VersionedTrieVectors.codec,
    "zk_jlvm_v1.json"            -> zk_jlvm_vectors.ZkJlvmVectors.codec,
    "zk_v1.json"                 -> zk_vectors.ZkVectors.codec
  )

  private def lossless[A](codec: Codec[A], json: String): Either[CanonicalError | DecodeFailure | CanonicalJson.RenderError, Boolean] =
    for {
      value   <- CanonicalJson.parse(json)
      decoded <- codec.decoder.decode(value)
      again   <- codec.encode(decoded)
      before  <- CanonicalJson.render(value)
      after   <- CanonicalJson.render(again)
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
      CanonicalJson.parse(bad).flatMap(merkle_vectors.MerkleVectors.codec.decoder.decode(_))
    result match {
      case Left(f: DecodeFailure) =>
        expect
          .same(List(Preview.of("leaves"), Preview.of("[0]")), f.path)
          .and(
            expect.same(DecodeReason.PatternMismatch("([0-9a-f]{2})*", Preview.of("zz")), f.reason)
          )
      case other => failure(s"expected a schema failure at leaves.[0], got $other")
    }
  }
}
