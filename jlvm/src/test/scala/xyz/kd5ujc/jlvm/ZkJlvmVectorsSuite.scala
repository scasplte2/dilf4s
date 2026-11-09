package xyz.kd5ujc.jlvm

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.{IO, Resource}
import cats.syntax.all._

import xyz.kd5ujc.accumulators.ValueCommitment
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}
import xyz.kd5ujc.core.hash.{DomainTag, Sha256, TaggedHash}

import io.circe.syntax._
import io.circe.{Decoder, Json}
import scodec.bits.ByteVector
import weaver.{Expectations, IOSuite}

/**
 * shared/vectors/zk_jlvm_v1.json (docs/specs/zk-jlvm.md) against the reference: each call's statement, computed here
 * from its texts with the Scala JLVM, ValueCommitment and tagged hashing, is the file's, and its public values are that
 * statement's canonical CBOR (through the generated codec). Each pinned SP1 Groth16 proof verifies through the JLVM's
 * own `groth16_verify` under the guest's key, so a JLVM program verifies a proved JLVM call, and claims nothing else.
 */
object ZkJlvmVectorsSuite extends IOSuite {

  final case class Call(
    name:         String,
    program:      String,
    data:         String,
    env:          Option[Map[String, String]],
    gasLimit:     Long,
    statement:    Json,
    publicValues: String,
    proofBytes:   Option[String]
  )
  final case class File(programVKey: String, calls: List[Call])

  given Decoder[Call] =
    Decoder.forProduct8("name", "program", "data", "env", "gasLimit", "statement", "publicValues", "proofBytes")(Call.apply)
  given Decoder[File] = Decoder.forProduct2("programVKey", "calls")(File.apply)

  type Res = (Jlvm, File)

  private val path = Paths.get("shared", "vectors", "zk_jlvm_v1.json")

  def sharedResource: Resource[IO, Res] =
    Resource.eval(
      (
        Jlvm.load[IO],
        IO.blocking(new String(Files.readAllBytes(path), UTF_8)).flatMap(t => io.circe.parser.decode[File](t).liftTo[IO])
      ).tupled
    )

  private def tag(t: String): TaggedHash[Sha256] = TaggedHash[Sha256](DomainTag.literal(t))

  private def commitment(v: CanonicalValue): ByteVector =
    ValueCommitment.bytes(v).fold(e => sys.error(e.toString), b => ByteVector(b.toArray))

  private def digest(t: String, v: CanonicalValue): ByteVector = tag(t)(commitment(v)).bytes

  private def right[A](e: Either[?, A]): A = e.fold(x => sys.error(x.toString), identity)

  /** The bindings' digest: each binding's digest of `cbor(name) || commit(value)`, in UTF-8 byte order of the names. */
  private def bindings(env: Map[String, CanonicalValue]): ByteVector = {
    val ordered = env.toList.sortBy(_._1.getBytes(UTF_8).toSeq.map(_ & 0xff))(using Ordering.Implicits.seqOrdering)
    val each = ordered.map {
      case (n, v) =>
        tag("dilf4s/zk/jlvm/binding/v1")(ByteVector(right(Cbor.encode(right(CanonicalValue.text(n)))).toArray) ++ commitment(v)).bytes
    }
    tag("dilf4s/zk/jlvm/bindings/v1")(each.foldLeft(ByteVector.empty)(_ ++ _)).bytes
  }

  /** The statement of a call by the spec alone, as the file writes it (digests in hex), and as its canonical value. */
  private def statement(jlvm: Jlvm, c: Call): (Json, CanonicalValue) = {
    val program = right(Parser.parse(c.program))
    val data = right(JsonSurface.parse(c.data))
    val env = c.env.getOrElse(Map.empty).map { case (k, v) => k -> right(JsonSurface.parse(v)) }
    val o = jlvm.evaluate(program, data, env, Config(limit = GasLimit(Gas(c.gasLimit))))
    val id = right(Parser.id[Sha256](program)).bytes
    val (input, binds) = (digest("dilf4s/zk/jlvm/input/v1", data), bindings(env))
    val (outcomeJson, outcomeValue) = o.result match {
      case Right(v) =>
        val d = digest("dilf4s/zk/jlvm/result/v1", v)
        (Json.obj("result" -> d.toHex.asJson), right(CanonicalValue.map(List("result" -> CanonicalValue.bytes(d)))))
      case Left(e) =>
        val fields = List("error" -> (e.kind: String).asJson) ++ e.site.map(s => "op" -> s.wire.asJson) ++
          JlvmVectors.cause(e).map("cause" -> _)
        val cause = JlvmVectors.cause(e).map { j =>
          val (family, kind) = j.asObject.flatMap(_.toList.headOption).getOrElse(sys.error("a cause"))
          "cause" -> right(CanonicalValue.map(List(family -> right(CanonicalValue.text(kind.asString.getOrElse(""))))))
        }
        val value = List("error" -> right(CanonicalValue.text(e.kind: String))) ++
          e.site.map(s => "op" -> right(CanonicalValue.text(s.wire))) ++ cause
        (Json.fromFields(fields), right(CanonicalValue.map(value)))
    }
    val n = (x: Long) => right(CanonicalValue.int(BigInt(x)))
    val json = Json.obj(
      "language" -> Program.Version.asJson,
      "schedule" -> GasSchedule.Latest.version.asJson,
      "program"  -> id.toHex.asJson,
      "input"    -> input.toHex.asJson,
      "bindings" -> binds.toHex.asJson,
      "gasLimit" -> c.gasLimit.asJson,
      "gasUsed"  -> o.gasUsed.value.asJson,
      "outcome"  -> outcomeJson
    )
    val value = right(
      CanonicalValue.map(
        List(
          "version"  -> n(1),
          "language" -> n(Program.Version.toLong),
          "schedule" -> n(GasSchedule.Latest.version.toLong),
          "program"  -> CanonicalValue.bytes(id),
          "input"    -> CanonicalValue.bytes(input),
          "bindings" -> CanonicalValue.bytes(binds),
          "gasLimit" -> n(c.gasLimit),
          "gasUsed"  -> n(o.gasUsed.value),
          "outcome"  -> outcomeValue
        )
      )
    )
    (json, value)
  }

  test("each statement is its call's, and the public values are its canonical CBOR") {
    case (jlvm, file) =>
      IO {
        file.calls.foldLeft(expect(file.calls.nonEmpty, "zk_jlvm_v1.json has calls")) { (acc, c) =>
          val (json, value) = statement(jlvm, c)
          val public = ByteVector.fromValidHex(c.publicValues)
          val decoded = right(Cbor.decode(public))
          val generated = xyz.kd5ujc.models.zk_jlvm.ZkJlvmStatement.codec
          val again = generated.decoder.decode(decoded).leftMap(_.message).flatMap(generated.encode(_).leftMap(_.toString))
          val encoded = ByteVector(right(Cbor.encode(value)).toArray).toHex
          acc
            .and(expect(c.statement == json, s"${c.name}: the statement is ${json.noSpaces}, the file's ${c.statement.noSpaces}"))
            .and(expect(c.publicValues == encoded, s"${c.name}: the public values are $encoded, the file's ${c.publicValues}"))
            .and(expect(again == Right(decoded), s"${c.name}: the generated codec gives $again"))
        }
      }
  }

  private def verify(jlvm: Jlvm, vkey: String, public: String, proof: String): Either[JlvmError, CanonicalValue] =
    Parser
      .parse(s"""{"groth16_verify": ["0x$vkey", "0x$public", "0x$proof"]}""")
      .flatMap(p => jlvm.evaluate(p, CNull, Map.empty, Config()).result)

  test("each pinned Groth16 proof verifies through groth16_verify under the guest's key, and only for its public values") {
    case (jlvm, file) =>
      val proved = file.calls.flatMap(c => c.proofBytes.map(c -> _))
      if (proved.isEmpty)
        IO.pure(failure("zk_jlvm_v1.json pins no Groth16 proof: prove one on euler (sdk/rust/zk/README.md, \"On euler\")"))
      else
        IO {
          proved.foldLeft(Expectations.Helpers.success) {
            case (acc, (c, proof)) =>
              val flipped = {
                val b = ByteVector.fromValidHex(c.publicValues)
                b.update(b.size - 1, (b.last ^ 1).toByte).toHex
              }
              val otherKey = {
                val k = ByteVector.fromValidHex(file.programVKey)
                k.update(31, (k(31) ^ 1).toByte).toHex
              }
              acc
                .and(expect(verify(jlvm, file.programVKey, c.publicValues, proof) == Right(CBool(true)), s"${c.name}: verifies"))
                .and(expect(verify(jlvm, file.programVKey, flipped, proof) == Right(CBool(false)), s"${c.name}: not other bytes"))
                .and(expect(verify(jlvm, otherKey, c.publicValues, proof) == Right(CBool(false)), s"${c.name}: not under another key"))
          }
        }
  }

  test("docs/specs/zk-jlvm.md embeds shared/schemas/zk-jlvm.cddl verbatim") {
    case _ =>
      IO.blocking {
        val read = (p: java.nio.file.Path) => new String(Files.readAllBytes(p), UTF_8)
        val schema = read(Paths.get("shared", "schemas", "zk-jlvm.cddl")).trim
        val blocks = "(?s)```cddl\\n(.*?)```".r.findAllMatchIn(read(Paths.get("docs", "specs", "zk-jlvm.md"))).map(_.group(1).trim).toList
        expect(blocks.contains(schema), "the spec's cddl block is the schema")
      }
  }
}
