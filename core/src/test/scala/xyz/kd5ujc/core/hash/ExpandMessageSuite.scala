package xyz.kd5ujc.core.hash

import java.nio.charset.StandardCharsets.{US_ASCII, UTF_8}
import java.nio.file.{Files, Paths}

import cats.syntax.all._

import io.circe.{Json, parser}
import scodec.bits.ByteVector
import weaver.SimpleIOSuite

object ExpandMessageSuite extends SimpleIOSuite {

  private val vectors: Json =
    parser.parse(new String(Files.readAllBytes(Paths.get("docs", "test-vectors", "vrf", "rfc9380.json")), UTF_8)).toOption.get

  private def check[H: Hasher](section: String, expectedCount: Int) = {
    val c = vectors.hcursor.downField(section)
    val dst = ByteVector(c.downField("header").get[String]("DST").toOption.get.getBytes(US_ASCII))
    val rows = c.downField("vectors").as[List[Map[String, String]]].toOption.get
    expect
      .same(expectedCount, rows.size)
      .and(rows.foldMap { r =>
        val msg = ByteVector(r("msg").getBytes(US_ASCII))
        val len = Integer.decode(r("len_in_bytes")).intValue
        expect.same(Right(ByteVector.fromValidHex(r("uniform_bytes"))), ExpandMessage.xmd[H](msg, dst, len))
      })
  }

  pureTest("expand_message_xmd(SHA-256): RFC 9380 K.1")(check[Sha256]("expand_message_xmd_sha256", 10))
  pureTest("expand_message_xmd(SHA-512): RFC 9380 K.3")(check[Sha512]("expand_message_xmd_sha512", 10))

  pureTest("over-long outputs and empty tags are typed errors") {
    expect(ExpandMessage.xmd[Sha256](ByteVector.empty, ByteVector(1), 255 * 32 + 1).isLeft)
      .and(expect(ExpandMessage.xmd[Sha256](ByteVector.empty, ByteVector.empty, 32).isLeft))
      .and(expect(ExpandMessage.xmd[Sha512](ByteVector.empty, ByteVector.fill(256)(7), 64).isRight, "oversize DST is reduced"))
  }
}
