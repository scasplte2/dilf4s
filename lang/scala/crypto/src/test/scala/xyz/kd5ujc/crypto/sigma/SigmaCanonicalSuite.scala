package xyz.kd5ujc.crypto.sigma

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.crypto.sigma.SigmaError.{PointRole, PropositionKind, PropositionReason}

import scodec.bits.ByteVector
import weaver.SimpleIOSuite

/**
 * The order in which `SigmaProp.decode` refuses a proposition (docs/specs/sigma.md §3), as the Rust SDK's
 * `Proposition::from_value` does (`lang/rust/signing/tests/sigma_vectors.rs`): its weight before anything is decoded, a
 * connective's width before its children, the leaf bound before a leaf's points; everything else as before.
 */
object SigmaCanonicalSuite extends SimpleIOSuite {

  private def get[A](e:    Either[?, A]): A = e.fold(x => sys.error(x.toString), identity)
  private def text(s:      String): CanonicalValue = get(CanonicalValue.text(s))
  private def map(entries: (String, CanonicalValue)*): CanonicalValue = get(CanonicalValue.map(entries))
  private def array(vs:    Vector[CanonicalValue]): CanonicalValue = get(CanonicalValue.array(vs))

  private val generator: ByteVector = Secp256k1Group.Generator.bytes

  /** `02 ‖ x` with `x = 5`, which has no `y` on secp256k1. */
  private val offCurve: ByteVector = ByteVector(2) ++ ByteVector.fill(31)(0) :+ 5.toByte

  private def dlog(pk: ByteVector): CanonicalValue = map("type" -> text("dlog"), "pk" -> CBytes(pk))
  private def connective(ty: String, children: Vector[CanonicalValue]): CanonicalValue =
    map("type" -> text(ty), "children" -> array(children))
  private def threshold(k: Int, children: Vector[CanonicalValue]): CanonicalValue =
    map("type" -> text("threshold"), "k" -> CInt(k), "children" -> array(children))

  private def reason(v: CanonicalValue): Either[SigmaError, Unit] = SigmaProp.decode(v).map(_ => ())

  private val invalidPoint = Left(SigmaError.InvalidPoint(PointRole.PublicKey, Secp256k1Group.DecodeError.InvalidPoint))

  /** `n` children, the first `bad` of them with an off-curve point. */
  private def withBadFirst(n: Int, bad: Int): Vector[CanonicalValue] =
    Vector.tabulate(n)(i => dlog(if (i < bad) offCurve else generator))

  /** `groups` `or`s of `each` leaves under an `and`, the last leaf replaced by `last`. */
  private def grouped(groups: Int, each: Int, last: CanonicalValue): CanonicalValue = {
    val leaves = Vector.fill(groups * each - 1)(dlog(generator)) :+ last
    connective("and", leaves.grouped(each).map(connective("or", _)).toVector)
  }

  private val tooWide = Left(SigmaError.InvalidProposition(PropositionReason.Children(255)))
  private val tooMany = Left(SigmaError.InvalidProposition(PropositionReason.Leaves(4096)))
  private val tooHeavy = Left(SigmaError.InvalidProposition(PropositionReason.Weight(7900416L)))

  pureTest("a connective wider than 255 is refused before its children are decoded") {
    expect
      .same(tooWide, reason(connective("and", withBadFirst(256, 1))))
      .and(expect.same(tooWide, reason(connective("or", withBadFirst(256, 256)))))
      .and(expect.same(tooWide, reason(threshold(0, withBadFirst(256, 1)))))
      .and(expect.same(tooWide, reason(threshold(1, withBadFirst(256, 1)))))
      // within the width, the children are decoded before the connective's other checks
      .and(expect.same(invalidPoint, reason(connective("and", withBadFirst(255, 1)))))
      .and(expect.same(invalidPoint, reason(threshold(0, withBadFirst(2, 1)))))
      .and(expect.same(Left(SigmaError.InvalidProposition(PropositionReason.EmptyChildren)), reason(connective("and", Vector.empty))))
  }

  pureTest("the 4097th leaf is refused before its points are decoded") {
    expect
      .same(Right(()), reason(grouped(16, 255, dlog(generator))))
      .and(expect.same(invalidPoint, reason(grouped(32, 128, dlog(offCurve)))))
      .and(expect.same(Right(()), reason(grouped(32, 128, dlog(generator)))))
      .and(expect.same(tooMany, reason(grouped(17, 241, dlog(generator)))))
      .and(expect.same(tooMany, reason(grouped(17, 241, dlog(offCurve)))))
      // a leaf's own fields are checked before the bound
      .and(
        expect.same(
          Left(SigmaError.InvalidProposition(PropositionReason.Fields(PropositionKind.Dlog, 1))),
          reason(grouped(17, 241, map("type" -> text("dlog"))))
        )
      )
  }

  pureTest("depth is checked once the children are decoded") {
    def chain(leaf: CanonicalValue) = (1 to SigmaProp.MaxDepth).foldLeft(leaf)((c, _) => connective("and", Vector(c)))
    expect
      .same(Left(SigmaError.InvalidProposition(PropositionReason.Depth(64))), reason(chain(dlog(generator))))
      .and(expect.same(invalidPoint, reason(chain(dlog(offCurve)))))
  }

  /**
   * The heaviest valid proposition (docs/specs/sigma.md §3): a one-`k` threshold over 255 more, which share 4096 chains
   * of 61 one-`k` thresholds, each over a `dhtuple` leaf at depth 64.
   */
  private val heaviest: CanonicalValue = {
    val g = CBytes(generator)
    val leaf = map("type" -> text("dhtuple"), "g" -> g, "h" -> g, "u" -> g, "v" -> g)
    val chain = (1 to 61).foldLeft(leaf)((c, _) => threshold(1, Vector(c)))
    threshold(1, Vector.tabulate(255)(i => threshold(1, Vector.fill(if (i < 16) 17 else 16)(chain))))
  }

  pureTest("a proposition heavier than the heaviest valid one is refused before decoding") {
    // a dlog weighs 15 plus its key's length
    def at(n: Long) = dlog(ByteVector.fill(n)(2))
    val decoded = SigmaProp.decode(heaviest)
    expect
      .same(SigmaProp.MaxWeight, heaviest.weight)
      .and(expect.same(Right((SigmaProp.MaxDepth, SigmaProp.MaxLeaves)), decoded.map(p => (p.depth, p.leaves))))
      .and(expect.same(SigmaProp.MaxWeight, at(SigmaProp.MaxWeight - 15).weight))
      .and(
        expect.same(
          Left(SigmaError.InvalidPoint(PointRole.PublicKey, Secp256k1Group.DecodeError.WrongSize(SigmaProp.MaxWeight - 15, 33))),
          reason(at(SigmaProp.MaxWeight - 15))
        )
      )
      .and(expect.same(tooHeavy, reason(at(SigmaProp.MaxWeight - 14))))
      .and(expect.same(tooHeavy, reason(connective("and", Vector(dlog(offCurve), dlog(ByteVector.fill(7900400L)(2)))))))
      .and(expect.same(tooHeavy, reason(text("x" * 7900416))))
      .and(expect.same(Left(SigmaError.InvalidProposition(PropositionReason.ExpectedObject)), reason(text("x" * 7900415))))
  }

  pureTest("a shared value of exponential size is refused at once, whether too wide, too many leaves or too deep") {
    val leaf = dlog(generator)
    val doubled = (1 to 60).foldLeft(leaf)((c, _) => connective("and", Vector(c, c)))
    val subtree = grouped(17, 241, leaf)
    val wide = connective("or", Vector.fill(1 << 20)(subtree))
    expect
      .same(tooHeavy, reason(doubled))
      .and(expect.same(tooHeavy, reason(wide)))
      .and(expect.same(Long.MaxValue, doubled.weight))
  }
}
