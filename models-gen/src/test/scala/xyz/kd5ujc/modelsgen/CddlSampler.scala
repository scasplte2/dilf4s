// SPDX-License-Identifier: Apache-2.0
package xyz.kd5ujc.modelsgen

import java.nio.charset.StandardCharsets.UTF_8

import cats.syntax.all._

import scala.util.Random

/**
 * Constructs canonical CBOR candidates from resolved CDDL. Random is supplied by the calling
 * test/tool; all encoding and domain failure propagation use values, trampolined over schema
 * depth.
 */
final class CddlSampler(schema: Schema, maxDepth: Int = 7):
  import GenResult.*
  private def unsupported[A](feature: UnsupportedFeature): GenResult[A] =
    failure(GenError.Unsupported("sampler", feature))

  private def head(major: Int, n: BigInt): Vector[Byte] =
    val m = major << 5
    if n < 24 then Vector((m | n.toInt).toByte)
    else
      val (flag, width) =
        if n <= 0xff then (24, 1)
        else if n <= 0xffff then (25, 2)
        else if n <= BigInt(0xffffffffL) then (26, 4)
        else (27, 8)
      (m | flag).toByte +: (width - 1 to 0 by -1).map(i => ((n >> (8 * i)) & 0xff).toByte).toVector

  private def bytes(value: Vector[Byte]): Vector[Byte] = head(2, value.size) ++ value
  private def text(value: String): Vector[Byte] =
    val bs = value.getBytes(UTF_8).toVector
    head(3, bs.size) ++ bs
  private def int(value: BigInt): Vector[Byte] =
    if value.signum >= 0 then head(0, value) else head(1, -1 - value)

  def sample(q: QName, rnd: Random): Either[GenError, Array[Byte]] =
    rule(q, rnd, 0, Set.empty).value.value.map(_.toArray)

  /** A group has no standalone CBOR representation; sample its members in an explicit array. */
  def sampleGroupArray(q: QName, rnd: Random): Either[GenError, Array[Byte]] =
    fromEither(schema.rule(q))
      .flatMap(_.body match
        case RuleBody.GroupBody(es) => ty(RT.Arr(es), rnd, 0, Set.empty)
        case RuleBody.Type(_)       => unsupported(UnsupportedFeature.StandaloneGroup(q))
      )
      .value
      .value
      .map(_.toArray)

  /**
   * Beyond the cutoff, choice and occurrence selection are deterministic. Re-entering a rule on
   * that ancestry cannot produce a finite encoding. Siblings use independent immutable paths,
   * while pre-cutoff random selections retain their original draws and output.
   */
  private def enter[A](q: QName, depth: Int, cutoffPath: Set[QName])(
    body: Set[QName] => GenResult[A]
  ): GenResult[A] =
    if depth > maxDepth && cutoffPath(q) then failure(GenError.SamplingCycle(q))
    else body(if depth > maxDepth then cutoffPath + q else Set.empty)

  private def rule(
    q:          QName,
    rnd:        Random,
    depth:      Int,
    cutoffPath: Set[QName]
  ): GenResult[Vector[Byte]] = deferred {
    enter(q, depth, cutoffPath) { next =>
      fromEither(schema.rule(q)).flatMap(_.body match
        case RuleBody.Type(t)      => ty(t, rnd, depth + 1, next)
        case RuleBody.GroupBody(_) => unsupported(UnsupportedFeature.StandaloneGroup(q))
      )
    }
  }

  private def big(rnd: Random, lo: BigInt, hi: BigInt): BigInt =
    val span = hi - lo + 1
    rnd.nextInt(6) match
      case 0 => lo
      case 1 => hi
      case _ =>
        if span.isValidLong && span <= Int.MaxValue then lo + rnd.nextInt(span.toInt)
        else lo + (BigInt(span.bitLength + 8, rnd).mod(span))

  private def randBytes(rnd: Random, n: Int): Vector[Byte] =
    val a = new Array[Byte](n)
    rnd.nextBytes(a)
    a.toVector

  private def ty(t: RT, rnd: Random, depth: Int, cutoffPath: Set[QName]): GenResult[Vector[Byte]] =
    deferred {
      t match
        case RT.Prim(PrimKind.Uint) => success(head(0, big(rnd, 0, BigInt(1) << rnd.nextInt(64))))
        case RT.Prim(PrimKind.Int) =>
          success(int(big(rnd, -(BigInt(1) << rnd.nextInt(40)), BigInt(1) << rnd.nextInt(40))))
        case RT.Prim(PrimKind.Nint)  => success(int(-1 - big(rnd, 0, BigInt(1) << rnd.nextInt(40))))
        case RT.Prim(PrimKind.Bytes) => success(bytes(randBytes(rnd, rnd.nextInt(10))))
        case RT.Prim(PrimKind.Text) =>
          success(text(rnd.alphanumeric.take(rnd.nextInt(10)).mkString))
        case RT.Prim(PrimKind.Bool) =>
          success(Vector((if rnd.nextBoolean() then 0xf5 else 0xf4).toByte))
        case RT.Prim(PrimKind.Nil) => success(Vector(0xf6.toByte))
        case RT.Prim(PrimKind.Any) =>
          success(rnd.nextInt(4) match
            case 0 => head(0, rnd.nextInt(1000))
            case 1 => bytes(randBytes(rnd, rnd.nextInt(6)))
            case 2 => text(rnd.alphanumeric.take(rnd.nextInt(6)).mkString)
            case _ => Vector(0x80.toByte)
          )
        case RT.Prim(PrimKind.Float)        => unsupported(UnsupportedFeature.FloatSampling)
        case RT.LitInt(n)                   => success(int(n))
        case RT.LitText(s)                  => success(text(s))
        case RT.LitBool(b)                  => success(Vector((if b then 0xf5 else 0xf4).toByte))
        case RT.LitBytes(bs)                => success(bytes(bs))
        case RT.IntRange(lo, hi) if lo > hi => failure(GenError.EmptyIntegerDomain(lo, hi))
        case RT.IntRange(lo, hi)            => success(int(big(rnd, lo, hi)))
        case RT.SizedBytes(lo, hi) =>
          success(
            bytes(
              randBytes(rnd, lo + (if hi == lo then 0 else rnd.nextInt(math.min(hi - lo, 8) + 1)))
            )
          )
        case RT.SizedText(lo, hi) =>
          success(
            text(
              rnd.alphanumeric
                .take(lo + (if hi == lo then 0 else rnd.nextInt(math.min(hi - lo, 8) + 1)))
                .mkString
            )
          )
        case RT.Ref(q) => rule(q, rnd, depth, cutoffPath)
        case RT.Choice(alts) =>
          ty(
            if depth > maxDepth then alts.head else alts(rnd.nextInt(alts.size)),
            rnd,
            depth,
            cutoffPath
          )
        case RT.Arr(es) =>
          arrayItems(es, rnd, depth, cutoffPath)
            .map(parts => head(4, parts.size) ++ parts.flatten)
        case RT.MapT(es) =>
          es.flatTraverse { en =>
            (0 until count(en.occ, rnd, depth)).toVector
              .foldLeftM((Set.empty[Vector[Byte]], Vector.empty[(Vector[Byte], Vector[Byte])])) {
                case ((seen, pairs), _) =>
                  val key = en.key match
                    case Some(RKey.IntKey(v))     => success(int(v))
                    case Some(RKey.TextKey(s))    => success(text(s))
                    case Some(RKey.OfType(kt, _)) => ty(kt, rnd, depth + 1, cutoffPath)
                    case None                     => unsupported(UnsupportedFeature.MapWithoutKey)
                  key.flatMap { kb =>
                    if seen(kb) then success((seen, pairs))
                    else
                      ty(en.value, rnd, depth + 1, cutoffPath)
                        .map(v => (seen + kb, pairs :+ ((kb, v))))
                  }
              }
              .map(_._2)
          }.map(pairs => head(5, pairs.size) ++ pairs.flatMap((k, v) => k ++ v))
        case RT.Tagged(n, inner)     => ty(inner, rnd, depth, cutoffPath).map(head(6, n) ++ _)
        case RT.CborOf(inner)        => ty(inner, rnd, depth, cutoffPath).map(bytes)
        case RT.GroupRef(q)          => unsupported(UnsupportedFeature.GroupAsType(q))
        case RT.Unsupported(feature) => unsupported(feature)
    }

  private def count(occ: Ast.Occ, rnd: Random, depth: Int): Int =
    val hi = occ.max.getOrElse(occ.min + 3)
    if depth > maxDepth then occ.min else occ.min + rnd.nextInt(math.min(hi - occ.min, 3) + 1)

  /**
   * Conservatively include optionals before a required suffix. CDDL matches greedily, so an absent
   * overlapping member could otherwise consume the suffix during validation. Propagate the
   * enclosing suffix through groups; no generated sample is filtered or retried.
   */
  private def arrayItems(
    es:            Vector[REntry],
    rnd:           Random,
    depth:         Int,
    cutoffPath:    Set[QName],
    requiredAfter: Boolean = false
  ): GenResult[Vector[Vector[Byte]]] =
    val (_, positioned) =
      es.reverseIterator.foldLeft((requiredAfter, List.empty[(REntry, Boolean)])) {
        case ((required, acc), entry) =>
          (required || entry.occ.min > 0, (entry, required) :: acc)
      }
    positioned.toVector.flatTraverse { (entry, required) =>
      entryItems(entry, rnd, depth, cutoffPath, required)
    }

  private def entryItems(
    en:            REntry,
    rnd:           Random,
    depth:         Int,
    cutoffPath:    Set[QName],
    requiredAfter: Boolean
  ): GenResult[Vector[Vector[Byte]]] =
    deferred {
      val occurrences = if requiredAfter && en.occ.isOptional then 1 else count(en.occ, rnd, depth)
      (0 until occurrences).toVector.flatTraverse { _ =>
        en.value match
          case RT.GroupRef(g) =>
            enter(g, depth, cutoffPath) { next =>
              fromEither(schema.rule(g)).flatMap(_.body match
                case RuleBody.GroupBody(es) => arrayItems(es, rnd, depth + 1, next, requiredAfter)
                case _                      => unsupported(UnsupportedFeature.NotGroup(g))
              )
            }
          case v => ty(v, rnd, depth + 1, cutoffPath).map(Vector(_))
      }
    }
