package xyz.kd5ujc.crypto.sigma

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._

import scodec.bits.ByteVector
import weaver.SimpleIOSuite

/**
 * The order in which `SigmaProp.fromCanonical` refuses a proposition (docs/specs/sigma.md §3), as the Rust SDK's
 * `Proposition::from_value` does (`sdk/rust/signing/tests/sigma_vectors.rs`): its weight before anything is decoded, a
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

  /** The error, `InvalidPoint` for a point that does not decode (Rust's kind), or `Ok`. */
  private def reason(v: CanonicalValue): String =
    SigmaProp
      .fromCanonical(v)
      .fold(e => if (List("pk", "g", "h", "u", "v").exists(f => e.startsWith(s"$f: "))) "InvalidPoint" else e, _ => "Ok")

  /** `n` children, the first `bad` of them with an off-curve point. */
  private def withBadFirst(n: Int, bad: Int): Vector[CanonicalValue] =
    Vector.tabulate(n)(i => dlog(if (i < bad) offCurve else generator))

  /** `groups` `or`s of `each` leaves under an `and`, the last leaf replaced by `last`. */
  private def grouped(groups: Int, each: Int, last: CanonicalValue): CanonicalValue = {
    val leaves = Vector.fill(groups * each - 1)(dlog(generator)) :+ last
    connective("and", leaves.grouped(each).map(connective("or", _)).toVector)
  }

  private val tooWide = "a connective has at most 255 children"
  private val tooMany = "a proposition has at most 4096 leaves"
  private val tooHeavy = "a proposition weighs at most 7900416"

  pureTest("a connective wider than 255 is refused before its children are decoded") {
    expect
      .same(tooWide, reason(connective("and", withBadFirst(256, 1))))
      .and(expect.same(tooWide, reason(connective("or", withBadFirst(256, 256)))))
      .and(expect.same(tooWide, reason(threshold(0, withBadFirst(256, 1)))))
      .and(expect.same(tooWide, reason(threshold(1, withBadFirst(256, 1)))))
      // within the width, the children are decoded before the connective's other checks
      .and(expect.same("InvalidPoint", reason(connective("and", withBadFirst(255, 1)))))
      .and(expect.same("InvalidPoint", reason(threshold(0, withBadFirst(2, 1)))))
      .and(expect.same("a connective needs at least one child: an empty one holds for anyone", reason(connective("and", Vector.empty))))
  }

  pureTest("the 4097th leaf is refused before its points are decoded") {
    expect
      .same("Ok", reason(grouped(16, 255, dlog(generator))))
      .and(expect.same("InvalidPoint", reason(grouped(32, 128, dlog(offCurve)))))
      .and(expect.same("Ok", reason(grouped(32, 128, dlog(generator)))))
      .and(expect.same(tooMany, reason(grouped(17, 241, dlog(generator)))))
      .and(expect.same(tooMany, reason(grouped(17, 241, dlog(offCurve)))))
      // a leaf's own fields are checked before the bound
      .and(expect.same("dlog takes exactly the fields pk", reason(grouped(17, 241, map("type" -> text("dlog"))))))
  }

  pureTest("depth is checked once the children are decoded") {
    def chain(leaf: CanonicalValue) = (1 to SigmaProp.MaxDepth).foldLeft(leaf)((c, _) => connective("and", Vector(c)))
    expect
      .same("a proposition nests at most 64 deep", reason(chain(dlog(generator))))
      .and(expect.same("InvalidPoint", reason(chain(dlog(offCurve)))))
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
    val decoded = SigmaProp.fromCanonical(heaviest)
    expect
      .same(SigmaProp.MaxWeight, heaviest.weight)
      .and(expect.same(Right((SigmaProp.MaxDepth, SigmaProp.MaxLeaves)), decoded.map(p => (p.depth, p.leaves))))
      .and(expect.same(SigmaProp.MaxWeight, at(SigmaProp.MaxWeight - 15).weight))
      .and(expect.same("InvalidPoint", reason(at(SigmaProp.MaxWeight - 15))))
      .and(expect.same(tooHeavy, reason(at(SigmaProp.MaxWeight - 14))))
      .and(expect.same(tooHeavy, reason(connective("and", Vector(dlog(offCurve), dlog(ByteVector.fill(7900400L)(2)))))))
      .and(expect.same(tooHeavy, reason(text("x" * 7900416))))
      .and(expect.same("a proposition is an object", reason(text("x" * 7900415))))
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
