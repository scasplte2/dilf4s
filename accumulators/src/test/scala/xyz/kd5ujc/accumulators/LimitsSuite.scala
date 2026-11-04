package xyz.kd5ujc.accumulators

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import xyz.kd5ujc.accumulators.versioned.{StateKey, StateNamespace, VersionedTrie}
import xyz.kd5ujc.models.limits
import xyz.kd5ujc.modelsgen.{CddlParser, FieldType, ModelRule, Profile, ProfileField, QName, Schema}

import weaver.FunSuite

/**
 * The spec limits are defined once, in shared/schemas/limits.cddl. A limit the specs derive from others is stated
 * there as the spec states it; these check each derivation, and each bound that CDDL can only write as a literal.
 */
object LimitsSuite extends FunSuite {

  test("derived limits are what their specs derive them from") {
    expect
      .same(limits.CanonicalMaxDepth, VersionedTrie.MaxValueDepth + VersionedTrie.ValueWrapperDepth)
      .and(expect.same(StateKey.MaxLength - 2, StateNamespace.MaxLength))
      .and(expect.same(BigInt(10).pow(limits.CanonicalMaxDigits).bitLength, limits.CanonicalMaxDigitBits))
      .and(expect.same(BigInt(10).pow(limits.CanonicalMaxScale).bitLength, limits.CanonicalMaxRationalBits))
      .and(expect.same(limits.CanonicalMaxRationalBits, xyz.kd5ujc.core.canonical.CanonicalValue.MaxRationalBits))
      .and(expect.same(limits.CanonicalMaxRationalBits, limits.CanonicalMaxIntegerBits))
      .and(expect.same(4L * limits.VersionedMaxChunkBytes, limits.MaxProofWeight.toLong))
      .and(expect.same(Long.MaxValue, limits.CanonicalMaxWeight))
      .and(expect.same(Long.MaxValue, limits.MerkleMaxTreeIndex))
  }

  test("the canonical scale is the digit bound, so every decimal of an accepted scale has a form within it") {
    expect
      .same(limits.CanonicalMaxDigits, limits.CanonicalMaxScale)
      .and(expect.same(limits.CanonicalMaxScale, xyz.kd5ujc.core.canonical.CanonicalJson.MaxScale))
  }

  test("a snapshot chunk's occurrence bounds, which CDDL writes as literals, are versioned-max-chunk-entries") {
    val modules = Vector("trie", "versioned-trie", "limits").map { m =>
      m -> Vector(CddlParser.parse(new String(Files.readAllBytes(Paths.get("shared", "schemas", s"$m.cddl")), UTF_8)).toOption.get)
    }
    val profile = new Profile(Schema.build(modules))
    List("versioned-trie-snapshot-state" -> "state", "versioned-trie-snapshot-history" -> "history").foldLeft(success) {
      case (acc, (rule, key)) =>
        val bound = profile.rule(QName("versioned-trie", rule)) match {
          case Right(ModelRule.Record(Vector(ProfileField(`key`, false, FieldType.Vec(_, min, max))))) => Some((min, max))
          case _                                                                                       => None
        }
        acc.and(expect.same(Some((1, Some(limits.VersionedMaxChunkEntries))), bound))
    }
  }
}
