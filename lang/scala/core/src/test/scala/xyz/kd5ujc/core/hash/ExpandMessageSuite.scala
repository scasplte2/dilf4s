package xyz.kd5ujc.core.hash

import java.nio.charset.StandardCharsets.{US_ASCII, UTF_8}
import java.nio.file.{Files, Paths}

import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{CanonicalJson, CanonicalValue}

import scodec.bits.ByteVector
import weaver.SimpleIOSuite

object ExpandMessageSuite extends SimpleIOSuite {

  private val vectors: CanonicalValue =
    CanonicalJson.parse(new String(Files.readAllBytes(Paths.get("docs", "test-vectors", "vrf", "rfc9380.json")), UTF_8)).toOption.get

  private def at(v: CanonicalValue, key: String): CanonicalValue =
    v match {
      case CMap(m) => m.getOrElse(key, CNull)
      case _       => CNull
    }

  private def text(v: CanonicalValue): String =
    v match {
      case CText(s) => s
      case other    => sys.error(s"expected text, got $other")
    }

  private def check[H: Hasher](section: String, expectedCount: Int) = {
    val c = at(vectors, section)
    val dst = ByteVector(text(at(at(c, "header"), "DST")).getBytes(US_ASCII))
    val rows = at(c, "vectors") match {
      case CArray(vs) => vs.toList
      case _          => Nil
    }
    expect
      .same(expectedCount, rows.size)
      .and(rows.foldMap { r =>
        val msg = ByteVector(text(at(r, "msg")).getBytes(US_ASCII))
        val len = Integer.decode(text(at(r, "len_in_bytes"))).intValue
        expect.same(Right(ByteVector.fromValidHex(text(at(r, "uniform_bytes")))), ExpandMessage.xmd[H](msg, dst, len))
      })
  }

  pureTest("expand_message_xmd(SHA-256): RFC 9380 K.1")(check[Sha256]("expand_message_xmd_sha256", 10))
  pureTest("expand_message_xmd(SHA-512): RFC 9380 K.3")(check[Sha512]("expand_message_xmd_sha512", 10))

  pureTest("an empty tag, then a length outside 1..limit, is InvalidExpansion by its refusal") {
    val length = HashError.InvalidExpansion(ExpansionRefusal.OutputLength, "SHA-256", 8160)
    expect
      .same(Left(length), ExpandMessage.xmd[Sha256](ByteVector.empty, ByteVector(1), 255 * 32 + 1))
      .and(expect.same(Left(length), ExpandMessage.xmd[Sha256](ByteVector.empty, ByteVector(1), 0)))
      .and(expect.same(Left(length), ExpandMessage.xmd[Sha256](ByteVector.empty, ByteVector.fill(256)(7), -1)))
      .and(
        expect.same(
          Left(HashError.InvalidExpansion(ExpansionRefusal.EmptyDst, "SHA-256", 8160)),
          ExpandMessage.xmd[Sha256](ByteVector.empty, ByteVector.empty, 0)
        )
      )
      .and(expect.same(16320L, ExpandMessage.limit[Sha512]))
      .and(expect.same(8160, ExpandMessage.xmd[Sha256](ByteVector.empty, ByteVector(1), 8160).map(_.size.toInt).getOrElse(0)))
      .and(expect(ExpandMessage.xmd[Sha512](ByteVector.empty, ByteVector.fill(256)(7), 64).isRight, "oversize DST is reduced"))
  }
}
