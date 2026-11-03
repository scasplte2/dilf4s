package xyz.kd5ujc.crypto.kes

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}

import xyz.kd5ujc.core.hash.{Blake2b256, Sha3_512}

import io.circe.Json
import io.circe.syntax._
import scodec.bits.{ByteVector, hex}
import weaver.SimpleIOSuite

/** Conformance vectors for docs/specs/kes.md: every period of small instances of each construction. */
object KesVectors {

  val path: Path = Paths.get("shared", "vectors", "kes_v1.json")
  val seed: ByteVector = ByteVector((0 until 32).map(_.toByte))
  val message: ByteVector = hex"6b6573"

  private def get[A](e: Either[KesError, A]): A = e.fold(err => sys.error(err.toString), identity)

  /** One record per period: the encoded key, and the signature on [[message]]. */
  private def periods[K](
    name:   String,
    params: Json,
    gen:    ByteVector => Either[KesError, K],
    vk:     K => ByteVector,
    bytes:  K => ByteVector,
    period: K => Long,
    sign:   K => Either[KesError, ByteVector],
    evolve: K => Either[KesError, K]
  ): Json = {
    val k0 = get(gen(seed))
    val keys = List.unfold(Option(k0))(_.map(k => (k, evolve(k).toOption)))
    Json.obj(
      "construction" -> name.asJson,
      "parameters"   -> params,
      "seed"         -> seed.toHex.asJson,
      "message"      -> message.toHex.asJson,
      "verifyingKey" -> vk(k0).toHex.asJson,
      "periods" -> keys.map { k =>
        Json.obj("period" -> period(k).asJson, "key" -> bytes(k).toHex.asJson, "signature" -> get(sign(k)).toHex.asJson)
      }.asJson
    )
  }

  /** One construction's operations, over bytes, for the refusal rows. */
  final case class Ops(
    construction: String,
    parameters:   Json,
    total:        Long,
    generate:     ByteVector => Either[KesError, ?],
    keyAt:        Long => ByteVector,
    load:         (ByteVector, Long) => Either[KesError, ?],
    evolve:       (ByteVector, Long) => Either[KesError, ?],
    vk:           ByteVector,
    signatureAt:  Long => ByteVector,
    verify:       (ByteVector, Long, ByteVector, ByteVector) => Either[KesError, Boolean]
  )

  private def outcome(r: Either[KesError, ?]): String = r.fold(_.productPrefix, _ => "accepted")

  private def refusal(name: String, check: String, construction: String, parameters: Json, fields: List[(String, Json)], expected: String)(
    actual: String
  ): Json = {
    require(actual == expected, s"$construction $check $name: expected $expected, got $actual")
    Json.obj(
      (List("name" -> name.asJson, "check" -> check.asJson, "construction" -> construction.asJson, "parameters" -> parameters) ++
      fields :+ ("outcome" -> actual.asJson))*
    )
  }

  /** The parameter checks: each height alone, and the two heights of a product together, are at most 30. */
  private def parameterRows: List[Json] = {
    def sum(height: Int, expected: String) =
      refusal(
        s"height-$height",
        "parameters",
        "dilf4s-sum",
        Json.obj("hash" -> "BLAKE2b-256".asJson, "height" -> height.asJson),
        Nil,
        expected
      )(
        outcome(KesSum[Blake2b256](height))
      )
    def product(sup: Int, sub: Int, expected: String) =
      refusal(
        s"heights-$sup-$sub",
        "parameters",
        "dilf4s-product",
        Json.obj("hash" -> "BLAKE2b-256".asJson, "superHeight" -> sup.asJson, "subHeight" -> sub.asJson),
        Nil,
        expected
      )(outcome(KesProduct[Blake2b256](sup, sub)))
    def cardano(depth: Int, expected: String) =
      refusal(s"depth-$depth", "parameters", "cardano-sum", Json.obj("depth" -> depth.asJson), Nil, expected)(outcome(CardanoSumKes(depth)))
    List(
      sum(30, "accepted"),
      sum(31, "Malformed"),
      sum(Int.MaxValue, "Malformed"),
      product(15, 15, "accepted"),
      product(0, 30, "accepted"),
      product(1, 30, "Malformed"),
      product(30, 1, "Malformed"),
      // added as 32-bit integers these wrap: 2^32 - 2 and 2^31, past every bound (issue #92)
      product(Int.MaxValue, Int.MaxValue, "Malformed"),
      product(Int.MaxValue, 1, "Malformed"),
      cardano(7, "accepted"),
      cardano(8, "Malformed"),
      cardano(Int.MaxValue, "Malformed")
    )
  }

  /** Every refusal of one construction, and the order of the verifier's checks. */
  private def operationRows(o: Ops): List[Json] = {
    def row(name: String, check: String, fields: List[(String, Json)], expected: String)(actual: String) =
      refusal(name, check, o.construction, o.parameters, fields, expected)(actual)
    def load(name: String, key: ByteVector, period: Long, expected: String) =
      row(name, "load", List("key" -> key.toHex.asJson, "period" -> period.asJson), expected)(outcome(o.load(key, period)))
    def evolve(name: String, key: ByteVector, period: Long, expected: String) =
      row(name, "evolve", List("key" -> key.toHex.asJson, "period" -> period.asJson), expected)(outcome(o.evolve(key, period)))
    def verify(name: String, vk: ByteVector, period: Long, m: ByteVector, sig: ByteVector, expected: String) =
      row(
        name,
        "verify",
        List("verifyingKey" -> vk.toHex.asJson, "message" -> m.toHex.asJson, "signature" -> sig.toHex.asJson, "period" -> period.asJson),
        expected
      )(o.verify(vk, period, m, sig).fold(_.productPrefix, ok => if (ok) "accepted" else "rejected"))
    val (k0, last) = (o.keyAt(0L), o.keyAt(o.total - 1))
    val sig1 = o.signatureAt(1L)
    List(
      row("seed-31-bytes", "generate", List("seed" -> seed.take(31).toHex.asJson), "Malformed")(outcome(o.generate(seed.take(31)))),
      row("seed-33-bytes", "generate", List("seed" -> (seed :+ 0.toByte).toHex.asJson), "Malformed")(outcome(o.generate(seed :+ 0.toByte))),
      load("period-0", k0, 0L, "accepted"),
      load("short", k0.dropRight(1), 0L, "Malformed"),
      load("past-the-last-period", k0, o.total, "PeriodOutOfRange"),
      load("short-and-past-the-last-period", k0.dropRight(1), o.total, "Malformed"),
      load("period-0-key-at-period-1", k0, 1L, "InconsistentKey"),
      load("last-key-at-period-0", last, 0L, "InconsistentKey"),
      evolve("period-0", k0, 0L, "accepted"),
      evolve("last-period", last, o.total - 1, "Exhausted"),
      verify("valid", o.vk, 1L, message, sig1, "accepted"),
      verify("other-period", o.vk, 0L, message, sig1, "rejected"),
      verify("other-message", o.vk, 1L, hex"6b6574", sig1, "rejected"),
      verify("short-key", o.vk.dropRight(1), 1L, message, sig1, "Malformed"),
      verify("short-signature", o.vk, 1L, message, sig1.dropRight(1), "Malformed"),
      verify("past-the-last-period", o.vk, o.total, message, sig1, "PeriodOutOfRange"),
      // the key's length, then the signature's, then the period
      verify("short-key-short-signature-past-the-last-period", o.vk.dropRight(1), o.total, message, sig1.dropRight(1), "Malformed"),
      verify("short-signature-past-the-last-period", o.vk, o.total, message, sig1.dropRight(1), "Malformed")
    )
  }

  private def keys[K](generate: Either[KesError, K], evolve: K => Either[KesError, K]): Vector[K] =
    Vector.unfold(Option(get(generate)))(_.map(k => (k, evolve(k).toOption)))

  def refusals: List[Json] = {
    val sum = get(KesSum[Blake2b256](2))
    val product = get(KesProduct[Blake2b256](1, 2))
    val cardano = get(CardanoSumKes(2))
    val sumKeys = keys(sum.generate(seed), sum.evolve)
    val productKeys = keys(product.generate(seed), product.evolve)
    val cardanoKeys = keys(cardano.generate(seed), cardano.evolve)
    def at[K](ks: Vector[K], p: Long): K = ks.lift(p.toInt).getOrElse(sys.error(s"no key at period $p"))
    parameterRows ++ List(
      Ops(
        "dilf4s-sum",
        Json.obj("hash" -> "BLAKE2b-256".asJson, "height" -> 2.asJson),
        sum.totalPeriods,
        sum.generate,
        p => at(sumKeys, p).bytes,
        sum.load,
        (k, p) => sum.load(k, p).flatMap(sum.evolve),
        sum.verifyingKey(at(sumKeys, 0L)),
        p => get(sum.sign(at(sumKeys, p), message)),
        sum.verify
      ),
      Ops(
        "dilf4s-product",
        Json.obj("hash" -> "BLAKE2b-256".asJson, "superHeight" -> 1.asJson, "subHeight" -> 2.asJson),
        product.totalPeriods,
        product.generate,
        p => at(productKeys, p).bytes,
        product.load,
        (k, p) => product.load(k, p).flatMap(product.evolve),
        product.verifyingKey(at(productKeys, 0L)),
        p => get(product.sign(at(productKeys, p), message)),
        product.verify
      ),
      Ops(
        "cardano-sum",
        Json.obj("depth" -> 2.asJson),
        cardano.totalPeriods,
        cardano.generate,
        p => at(cardanoKeys, p).bytes,
        cardano.load,
        (k, p) => cardano.load(k, p).flatMap(cardano.evolve),
        cardano.verifyingKey(at(cardanoKeys, 0L)),
        p => cardano.sign(at(cardanoKeys, p), message),
        cardano.verify
      )
    ).flatMap(operationRows)
  }

  def render: String = {
    val sum = get(KesSum[Blake2b256](2))
    val sum512 = get(KesSum[Sha3_512](1))
    val product = get(KesProduct[Blake2b256](1, 2))
    val cardano = get(CardanoSumKes(2))
    Json
      .obj(
        "spec"    -> "docs/specs/kes.md".asJson,
        "version" -> 1.asJson,
        "vectors" -> List(
          periods[sum.Key](
            "dilf4s-sum",
            Json.obj("hash" -> "BLAKE2b-256".asJson, "height" -> 2.asJson),
            sum.generate,
            sum.verifyingKey,
            _.bytes,
            _.period,
            sum.sign(_, message),
            sum.evolve
          ),
          periods[sum512.Key](
            "dilf4s-sum",
            Json.obj("hash" -> "SHA3-512".asJson, "height" -> 1.asJson),
            sum512.generate,
            sum512.verifyingKey,
            _.bytes,
            _.period,
            sum512.sign(_, message),
            sum512.evolve
          ),
          periods[product.Key](
            "dilf4s-product",
            Json.obj("hash" -> "BLAKE2b-256".asJson, "superHeight" -> 1.asJson, "subHeight" -> 2.asJson),
            product.generate,
            product.verifyingKey,
            _.bytes,
            _.period,
            product.sign(_, message),
            product.evolve
          ),
          periods[cardano.Key](
            "cardano-sum",
            Json.obj("depth" -> 2.asJson),
            cardano.generate,
            cardano.verifyingKey,
            _.bytes,
            _.period,
            k => Right(cardano.sign(k, message)),
            cardano.evolve
          )
        ).asJson,
        "refusals" -> refusals.asJson
      )
      .spaces2 + "\n"
  }
}

object KesVectorGen extends IOApp {
  def run(args: List[String]): IO[ExitCode] = IO(Files.write(KesVectors.path, KesVectors.render.getBytes(UTF_8))).as(ExitCode.Success)
}

object KesVectorsSuite extends SimpleIOSuite {
  test("committed KES vectors match the reference implementation") {
    IO(new String(Files.readAllBytes(KesVectors.path), UTF_8)).map { committed =>
      expect(
        committed == KesVectors.render,
        s"${KesVectors.path} is stale; regenerate with: sbt \"crypto/Test/runMain xyz.kd5ujc.crypto.kes.KesVectorGen\""
      )
    }
  }
}
