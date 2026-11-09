package xyz.kd5ujc.jlvm.difffuzz

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.syntax.all._

import xyz.kd5ujc.jlvm.{EthKeys, JlvmVectors, Op}

import io.circe.{ACursor, Json}

/**
 * A cryptographic operation's arguments from a vector row, each as surface JSON (byte strings as `0x` text, maps as
 * maps): what the crypto generator places and mutates, so true, false and refusal paths are all reached.
 */
final case class CryptoSeed(op: Op, args: Vector[Sj], from: String)

/**
 * What the generator draws on: every `jlvm_v1.json` input case (to mutate), split into the light ones and the heavy
 * ones (built bindings, a raised gas limit, or long text), which are drawn rarely; and the cryptographic arguments of
 * the vector rows (`signing_v1.json`, `sigma_v1.json`, `zk_v1.json`, `vrf_v1.json`, and the JLVM cases' own).
 */
final case class Corpus(light: Vector[FuzzCase], heavy: Vector[FuzzCase], crypto: Map[Op, Vector[CryptoSeed]]) {
  def all: Vector[FuzzCase] = light ++ heavy
}

object Corpus {

  private def vector(file: String): IO[Json] =
    IO.blocking(new String(Files.readAllBytes(Paths.get("shared", "vectors", file)), UTF_8)).flatMap(io.circe.parser.parse(_).liftTo[IO])

  private def str(c: ACursor, field: String): Option[String] = c.downField(field).as[String].toOption

  private def bytes(hex: String): Sj = Sj.Str("0x" + hex.stripPrefix("0x").toLowerCase(java.util.Locale.ROOT))

  private def rows(j: Json, field: String): Vector[ACursor] =
    j.hcursor.downField(field).values.fold(Vector.empty[ACursor])(_.toVector.map(_.hcursor))

  /** The vector cases, as the generator's cases: every binding sorted by name. */
  val cases: IO[Vector[FuzzCase]] =
    JlvmVectors.inputs.map(_.cases.toVector.map { i =>
      FuzzCase(
        i.name,
        FuzzCase.Source.Surface(i.program),
        i.data,
        i.env.getOrElse(Map.empty).toVector.sortBy(_._1),
        i.built.getOrElse(Map.empty).toVector.sortBy(_._1),
        i.gasLimit
      )
    })

  private def heavy(c: FuzzCase): Boolean =
    c.built.nonEmpty || c.gasLimit.exists(_ > (1L << 24)) || (c.source match {
      case FuzzCase.Source.Surface(t) => t.length > 20000
      case FuzzCase.Source.Form(b)    => b.size > 20000
    }) || c.data.length > 200000 || c.env.exists(_._2.length > 200000)

  /**
   * A case whose root calls a cryptographic operation, its arguments read from the data or written inline; not the
   * sweep's probes of argument counts and kinds, which hold no cryptographic input.
   */
  private def fromCase(c: FuzzCase): Option[CryptoSeed] =
    if (c.name.contains(" \u00d7 ") || c.name.endsWith(": no arguments")) None
    else
      (c.source match {
        case FuzzCase.Source.Surface(t) => io.circe.parser.parse(t).toOption
        case FuzzCase.Source.Form(_)    => None
      }).zip(io.circe.parser.parse(c.data).toOption).flatMap {
        case (program, data) =>
          program.asObject.filter(_.size == 1).flatMap(_.toList.headOption).flatMap {
            case (name, args) =>
              Op.fromWire(name).filter(crypto.contains).flatMap { op =>
                args.asArray
                  .flatMap(_.traverse { a =>
                    a.hcursor.get[String]("var").toOption match {
                      case Some(k) if !k.startsWith("$") => data.hcursor.downField(k).focus.map(Sj.fromCirce)
                      case Some(_)                       => None
                      case None                          => Some(Sj.fromCirce(a))
                    }
                  })
                  .map(as => CryptoSeed(op, as, c.name))
              }
          }
      }

  val crypto: Set[Op] = Set(
    Op.Ed25519Verify,
    Op.Secp256k1Verify,
    Op.Bip340Verify,
    Op.Ecrecover,
    Op.BlsVerify,
    Op.BlsAggregateVerify,
    Op.Poseidon,
    Op.PmtVerify,
    Op.Groth16Verify,
    Op.EcvrfVerify,
    Op.Bn254Add,
    Op.Bn254Mul,
    Op.Bn254Pairing,
    Op.SchnorrVerify,
    Op.SigmaVerify,
    Op.MerkleVerify,
    Op.MerkleConsistencyVerify,
    Op.TrieVerify,
    Op.TriePrefixVerify,
    Op.TrieRangeVerify
  )

  private def signing(j: Json): Vector[CryptoSeed] = {
    val scheme: Map[String, Op] =
      Map(
        "Ed25519"         -> Op.Ed25519Verify,
        "secp256k1-ECDSA" -> Op.Secp256k1Verify,
        "BIP-340"         -> Op.Bip340Verify,
        "BLS12-381-PoP"   -> Op.BlsVerify
      )
    val verification = rows(j, "verification").flatMap { r =>
      (str(r, "scheme").flatMap(scheme.get), str(r, "verifyingKey"), str(r, "message"), str(r, "signature")).mapN { (op, k, m, s) =>
        CryptoSeed(op, Vector(bytes(k), bytes(m), bytes(s)), s"signing_v1 verification ${str(r, "name").getOrElse("")}")
      }
    }
    val signatures = rows(j, "signing").flatMap { s =>
      str(s, "scheme").flatMap(scheme.get).toVector.flatMap { op =>
        rows(s.focus.getOrElse(Json.Null), "keys").flatMap { k =>
          str(k, "verifyingKey").toVector.flatMap { vk =>
            rows(k.focus.getOrElse(Json.Null), "signatures").flatMap { sig =>
              (str(sig, "message"), str(sig, "signature"))
                .mapN((m, x) => CryptoSeed(op, Vector(bytes(vk), bytes(m), bytes(x)), "signing_v1 signing"))
            }
          }
        }
      }
    }
    verification ++ signatures
  }

  private def sigma(j: Json): Vector[CryptoSeed] =
    rows(j, "cases").flatMap { r =>
      (r.downField("proposition").focus, str(r, "proof"), str(r, "message")).mapN { (prop, proof, msg) =>
        val name = s"sigma_v1 ${str(r, "name").getOrElse("")} ${str(r, "kind").getOrElse("")}"
        val leaf = prop.hcursor.get[String]("type").contains("dlog")
        Vector(CryptoSeed(Op.SigmaVerify, Vector(Sj.fromCirce(prop), Sj.Str(proof), Sj.Str(msg)), name)) ++
        prop.hcursor
          .get[String]("pk")
          .toOption
          .filter(_ => leaf)
          .map(pk => CryptoSeed(Op.SchnorrVerify, Vector(Sj.Str(pk), Sj.Str(msg), Sj.Str(proof)), name))
          .toVector
      }.toVector.flatten
    }

  private def zk(j: Json): Vector[CryptoSeed] =
    rows(j, "add").flatMap(r =>
      (str(r, "a"), str(r, "b")).mapN((a, b) => CryptoSeed(Op.Bn254Add, Vector(bytes(a), bytes(b)), "zk_v1 add"))
    ) ++
    rows(j, "mul").flatMap(r =>
      (str(r, "point"), str(r, "scalar")).mapN((p, s) => CryptoSeed(Op.Bn254Mul, Vector(bytes(p), bytes(s)), "zk_v1 mul"))
    ) ++
    rows(j, "pairing").map { r =>
      val pairs = rows(r.focus.getOrElse(Json.Null), "pairs").flatMap(p =>
        (str(p, "g1"), str(p, "g2")).mapN((a, b) => Sj.Arr(Vector(bytes(a), bytes(b))))
      )
      CryptoSeed(Op.Bn254Pairing, pairs, "zk_v1 pairing")
    } ++
    rows(j, "sp1Groth16").flatMap(r =>
      (str(r, "programVKey"), str(r, "publicValues"), str(r, "proofBytes")).mapN((k, v, p) =>
        CryptoSeed(Op.Groth16Verify, Vector(bytes(k), bytes(v), bytes(p)), s"zk_v1 groth16 ${str(r, "name").getOrElse("")}")
      )
    )

  private val Tai = "ECVRF-EDWARDS25519-SHA512-TAI"

  private def vrf(j: Json): Vector[CryptoSeed] =
    rows(j, "suites").filter(s => str(s, "suite").contains(Tai)).flatMap { s =>
      rows(s.focus.getOrElse(Json.Null), "keys").flatMap { k =>
        str(k, "verifyingKey").toVector.flatMap { vk =>
          rows(k.focus.getOrElse(Json.Null), "proofs").flatMap(p =>
            (str(p, "alpha"), str(p, "pi"))
              .mapN((a, pi) => CryptoSeed(Op.EcvrfVerify, Vector(bytes(vk), bytes(a), bytes(pi)), "vrf_v1 proof"))
          )
        }
      }
    } ++ rows(j, "rejections").filter(r => str(r, "suite").contains(Tai)).flatMap { r =>
      (str(r, "verifyingKey"), str(r, "alpha"), str(r, "pi")).mapN((vk, a, pi) =>
        CryptoSeed(Op.EcvrfVerify, Vector(bytes(vk), bytes(a), bytes(pi)), s"vrf_v1 rejection ${str(r, "name").getOrElse("")}")
      )
    }

  /** A field element, decimal in the vectors, as the JLVM's 32 big-endian bytes. */
  private def element(decimal: String): Option[Sj] =
    scala.util.Try(BigInt(decimal)).toOption.filter(_.signum >= 0).map { n =>
      bytes(scodec.bits.ByteVector(n.toByteArray).takeRight(32).padLeft(32).toHex)
    }

  private def poseidon(j: Json): Vector[CryptoSeed] =
    rows(j, "poseidon").flatMap(r =>
      r.downField("inputs").as[Vector[String]].toOption.flatMap(_.traverse(element)).map(CryptoSeed(Op.Poseidon, _, "poseidon_v1"))
    )

  /** Ethereum signatures, which no vector file holds: deterministic (RFC 6979), so the corpus is the same every run. */
  private val ecrecover: Vector[CryptoSeed] =
    for {
      key <- Vector(BigInt(1), BigInt(7), BigInt("deadbeef", 16), BigInt(2).pow(255) - 19)
      msg <- Vector("transfer 10 to bob", "", "a".repeat(100))
    } yield {
      val digest = EthKeys.keccak(msg)
      CryptoSeed(Op.Ecrecover, Vector(bytes(digest.toHex), bytes(EthKeys.sign(key, digest).toHex)), "EthKeys")
    }

  val load: IO[Corpus] =
    (
      cases,
      vector("signing_v1.json"),
      vector("sigma_v1.json"),
      vector("zk_v1.json"),
      vector("vrf_v1.json"),
      vector("poseidon_v1.json")
    ).mapN { (all, sg, sm, z, v, p) =>
      val seeds = all.flatMap(fromCase) ++ signing(sg) ++ sigma(sm) ++ zk(z) ++ vrf(v) ++ poseidon(p) ++ ecrecover
      val (h, l) = all.partition(heavy)
      Corpus(l, h, seeds.groupBy(_.op))
    }
}
