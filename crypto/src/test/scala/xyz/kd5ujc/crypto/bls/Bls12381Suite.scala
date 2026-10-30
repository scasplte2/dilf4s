package xyz.kd5ujc.crypto.bls

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.syntax.all._

import xyz.kd5ujc.crypto.signing._

import io.circe.{Json, parser}
import scodec.bits.{ByteVector, hex}
import weaver.{Expectations, IOSuite}

/** ethereum/bls12-381-tests v0.1.2 (every category) and the tessellation-bls known answer. */
object Bls12381Suite extends IOSuite {

  type Res = Bls12381
  def sharedResource: cats.effect.Resource[IO, Bls12381] = cats.effect.Resource.eval(Bls12381.load[IO])

  private val vectors: Json =
    parser
      .parse(new String(Files.readAllBytes(Paths.get("docs", "test-vectors", "bls", "eth2-bls12-381-tests-v0.1.2.json")), UTF_8))
      .toOption
      .get

  private def cases(category: String): List[Json] = vectors.hcursor.downField(category).as[List[Json]].toOption.get
  private def b(j:            Json): ByteVector = ByteVector.fromValidHex(j.asString.get.stripPrefix("0x"))
  private def in(c:           Json, k: String): Json = c.hcursor.downField("input").downField(k).focus.get
  private def name(c:         Json): String = c.hcursor.get[String]("name").toOption.get
  private def out(c:          Json): Json = c.hcursor.downField("output").focus.get

  private def each(category: String, count: Int)(f: Json => Expectations): Expectations = {
    val cs = cases(category)
    expect.same(count, cs.size).and(cs.foldMap(c => f(c).and(expect(true, name(c)))))
  }

  test("sign (10)") { bls =>
    IO(each("sign", 10) { c =>
      val sk = bls.codec.signingKey(b(in(c, "privkey")))
      val expected = out(c).asString.map(s => ByteVector.fromValidHex(s.stripPrefix("0x")))
      expect.same(expected, sk.toOption.flatMap(k => bls.sign(k, b(in(c, "message"))).toOption.map(_.bytes)))
    })
  }

  test("verify (29)") { bls =>
    IO(each("verify", 29) { c =>
      val ok = bls.verifier.verify(VerifyingKey(b(in(c, "pubkey"))), b(in(c, "message")), Signature(b(in(c, "signature"))))
      expect.same(out(c).asBoolean.get, ok)
    })
  }

  test("aggregate (6)") { bls =>
    IO(each("aggregate", 6) { c =>
      val sigs =
        c.hcursor.downField("input").as[List[String]].toOption.get.map(s => Signature[BlsPop](ByteVector.fromValidHex(s.stripPrefix("0x"))))
      expect.same(out(c).asString.map(s => ByteVector.fromValidHex(s.stripPrefix("0x"))), bls.aggregate(sigs).toOption.map(_.bytes))
    })
  }

  test("fast_aggregate_verify (12)") { bls =>
    IO(each("fast_aggregate_verify", 12) { c =>
      val keys = in(c, "pubkeys").asArray.get.toList.map(k => VerifyingKey[BlsPop](b(k)))
      expect.same(out(c).asBoolean.get, bls.fastAggregateVerify(keys, b(in(c, "message")), Signature(b(in(c, "signature")))))
    })
  }

  test("aggregate_verify (5)") { bls =>
    IO(each("aggregate_verify", 5) { c =>
      val keys = in(c, "pubkeys").asArray.get.toList.map(k => VerifyingKey[BlsPop](b(k)))
      val msgs = in(c, "messages").asArray.get.toList.map(b)
      expect.same(out(c).asBoolean.get, bls.aggregateVerify(keys.zip(msgs), Signature(b(in(c, "signature")))))
    })
  }

  test("batch_verify (4)") { bls =>
    IO(each("batch_verify", 4) { c =>
      val keys = in(c, "pubkeys").asArray.get.toList.map(k => VerifyingKey[BlsPop](b(k)))
      val msgs = in(c, "messages").asArray.get.toList.map(b)
      val sigs = in(c, "signatures").asArray.get.toList.map(s => Signature[BlsPop](b(s)))
      expect.same(out(c).asBoolean.get, bls.batchVerify(keys.zip(msgs).zip(sigs).map { case ((k, m), s) => (k, m, s) }))
    })
  }

  test("deserialization_G1 (13) and deserialization_G2 (15)") { bls =>
    IO(
      // The codec is KeyValidate, not bare deserialization: the identity decodes but is never a valid public key.
      each("deserialization_G1", 13) { c =>
        if (name(c) == "deserialization_succeeds_infinity_with_true_b_flag")
          expect(bls.codec.verifyingKey(b(in(c, "pubkey"))).isLeft, "KeyValidate rejects the identity")
        else expect.same(out(c).asBoolean.get, bls.codec.verifyingKey(b(in(c, "pubkey"))).isRight)
      }
        .and(each("deserialization_G2", 15)(c => expect.same(out(c).asBoolean.get, bls.codec.signature(b(in(c, "signature"))).isRight)))
    )
  }

  test("hash_to_G2 (4), RFC 9380 BLS12381G2_XMD:SHA-256_SSWU_RO_") { bls =>
    IO(each("hash_to_G2", 4) { c =>
      def coord(k: String) =
        out(c).hcursor.get[String](k).toOption.get.split(",").toList.map(s => ByteVector.fromValidHex(s.stripPrefix("0x")))
      // the vectors write Fp2 as "c0,c1"; blst serialises c1 first
      val expected = (coord("x").reverse ++ coord("y").reverse).reduce(_ ++ _)
      val msg = ByteVector(in(c, "msg").asString.get.getBytes(UTF_8))
      expect.same(Right(expected), bls.hashToG2(msg, "QUUX-V01-CS02-with-BLS12381G2_XMD:SHA-256_SSWU_RO_"))
    })
  }

  test("tessellation-bls known answer, proof of possession, and typed key errors") { bls =>
    IO {
      val sk = bls.codec.signingKey(hex"328388aff0d4a5b7dc9205abd374e7e98f3cd9f3418edb4eafda5fb16473d216").toOption.get
      val vk = bls.publicKey(sk).toOption.get
      val msg = ByteVector.fill(32)(0xab)
      val other = bls.codec.signingKey(ByteVector.fromLong(7).padLeft(32)).toOption.get
      val pop = bls.popProve(sk).toOption.get
      expect
        .same(hex"b53d21a4cfd562c469cc81514d4ce5a6b577d8403d32a394dc265dd190b47fa9f829fdd7963afdf972e5e77854051f6f", vk.bytes)
        .and(
          expect.same(
            Right(
              hex"ae82747ddeefe4fd64cf9cedb9b04ae3e8a43420cd255e3c7cd06a8d88b7c7f8638543719981c5d16fa3527c468c25f0026704a6951bde891360c7e8d12ddee0559004ccdbe6046b55bae1b257ee97f7cdb955773d7cf29adf3ccbb9975e4eb9"
            ),
            bls.sign(sk, msg).map(_.bytes)
          )
        )
        .and(expect(bls.popVerify(vk, pop), "PoP verifies"))
        .and(expect(!bls.popVerify(bls.publicKey(other).toOption.get, pop), "PoP is bound to its key"))
        .and(expect(!bls.verifier.verify(vk, vk.bytes, pop), "a PoP is not a message signature (distinct DST)"))
        .and(expect(bls.codec.signingKey(ByteVector.low(32)).isLeft, "zero key"))
        .and(expect(bls.hashToG2(hex"00", "bad\u0000dst").isLeft, "a DST that JNI would mangle is refused"))
        .and(expect(bls.codec.signingKey(ByteVector.view(Bls12381.R.toByteArray).takeRight(32)).isLeft, "key = r"))
    }
  }

  test("EIP-2333 test case 0 (master and child keys)") { bls =>
    IO {
      val seed =
        hex"c55257c360c07c72029aebc1b53c05ed0362ada38ead3e3e9efa3708e53495531f09a6987599d18264c1e1c92f2cf141630c7a3c4ab7c81b2f001698e7463b04"
      val master = bls.eip2333Master(seed)
      expect
        .same(
          Right(BigInt("6083874454709270928345386274498605044986640685124978867557563392430687146096")),
          master.map(k => BigInt(1, k.bytes.toArray))
        )
        .and(
          expect.same(
            Right(BigInt("20397789859736650942317412262472558107875392172444076792671091975210932703118")),
            master.flatMap(bls.eip2333Child(_, 0)).map(k => BigInt(1, k.bytes.toArray))
          )
        )
    }
  }
}
