package xyz.kd5ujc.core.hash

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest

import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}

import scodec.bits.ByteVector
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object HasherSuite extends SimpleIOSuite with Checkers {

  implicit private val showBytes: cats.Show[Array[Byte]] = cats.Show.show(ByteVector(_).toHex)

  private val abc = ByteVector("abc".getBytes(UTF_8))

  pureTest("known-answer tests for \"abc\"") {
    expect.same("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad", Hasher[Sha256].digest(abc).hex) and
      expect.same("3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532", Hasher[Sha3_256].digest(abc).hex) and
      expect.same(
        "b751850b1a57168a5693cd924b6b096e08f621827444f70d884f5d0240d2712e10e116e9192af3c91a7ec57647e3934057340b4cf408d5a56592f8274eec53f0",
        Hasher[Sha3_512].digest(abc).hex
      ) and
      expect.same("bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319", Hasher[Blake2b256].digest(abc).hex) and
      expect.same(
        "ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d17d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923",
        Hasher[Blake2b512].digest(abc).hex
      )
  }

  test("SHA-256 agrees with the JDK implementation") {
    forall { (bytes: Array[Byte]) =>
      expect.same(ByteVector(MessageDigest.getInstance("SHA-256").digest(bytes)), Hasher[Sha256].digest(ByteVector(bytes)).bytes)
    }
  }

  test("tagged hash is H(H(tag) || H(tag) || msg)") {
    val tag = DomainTag("dilf4s/test/v1").toOption.get
    forall { (bytes: Array[Byte]) =>
      val sha = (b: Array[Byte]) => MessageDigest.getInstance("SHA-256").digest(b)
      val t = sha(tag.value.getBytes(UTF_8))
      expect.same(ByteVector(sha(t ++ t ++ bytes)), TaggedHash[Sha256](tag).apply(ByteVector(bytes)).bytes)
    }
  }

  pureTest("different tags separate domains for identical content") {
    val leaf = TaggedHash[Sha256](DomainTag("dilf4s/mpt/leaf/v1").toOption.get)
    val node = TaggedHash[Sha256](DomainTag("dilf4s/mpt/branch/v1").toOption.get)
    val v = CanonicalValue.int(42)
    expect(leaf.value(v) != node.value(v), "domains must not collide") and
      expect.same(leaf(Cbor.encode(v)), leaf.value(v))
  }

  pureTest("domain tags follow the grammar") {
    List("dilf4s/mpt/leaf/v1", "a/v0", "dilf4s/smt.node_x-y/v12").map(t => expect(DomainTag(t).isRight, s"$t is valid")).reduce(_ and _) and
      List("", "v1", "dilf4s/leaf", "Dilf4s/x/v1", "dilf4s//v1", "dilf4s/x/v", "dilf4s/x/v1/", "dilf4s/ x/v1").map(t =>
        expect(DomainTag(t).isLeft, s"'$t' is invalid")
      ).reduce(_ and _)
  }

  pureTest("digests validate their length per algorithm") {
    expect(Digest.fromBytes[Sha256](ByteVector.fill(32)(0)).isRight, "32 bytes is a SHA-256 digest") and
      expect(Digest.fromBytes[Sha256](ByteVector.fill(31)(0)).isLeft, "31 bytes is not") and
      expect(Digest.fromBytes[Sha3_512](ByteVector.fill(32)(0)).isLeft, "32 bytes is not a SHA3-512 digest") and
      expect(Digest.fromHex[Sha256]("zz").isLeft, "invalid hex is rejected")
  }
}
