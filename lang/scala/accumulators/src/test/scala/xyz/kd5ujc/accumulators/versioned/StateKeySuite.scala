package xyz.kd5ujc.accumulators.versioned

import cats.syntax.all._

import scala.collection.immutable.SortedMap

import xyz.kd5ujc.accumulators.versioned.Harness._
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor, Preview}
import xyz.kd5ujc.storage.{Batch, KeyCodec}

import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

/** Keys, namespaces, version keys and the projection adapter (docs/specs/versioned-trie.md §2, §3, §8). */
object StateKeySuite extends SimpleIOSuite with Checkers {

  pureTest("state key grammar: valid keys parse, invalid keys are rejected") {
    val valid = List("a", "fiber/1", "a.b/c-d/e_f", "0/9", "x" * 64, List.fill(16)("a").mkString("/"), List.fill(4)("y" * 63).mkString("/"))
    val invalid = List(
      "",
      "/a",
      "a/",
      "a//b",
      "A",
      "a b",
      "-a",
      ".a",
      "_a",
      "a/.b",
      "é",
      "x" * 65,
      List.fill(17)("a").mkString("/"),
      List.fill(5)("y" * 63).mkString("/")
    )
    valid
      .foldMap(s => expect(StateKey.parse(s).isRight, s"'$s' is valid"))
      .and(invalid.foldMap(s => expect(StateKey.parse(s).isLeft, s"'$s' is invalid")))
      .and(expect.same(Right(key("a/b")), StateKey.fromBytes(key("a/b").bytes)))
      .and(expect(StateKey.fromBytes(scodec.bits.ByteVector(0xff)).isLeft, "not UTF-8"))
  }

  pureTest("a 1 MiB key or trie key is refused by its length, as a preview or a size (conformance.md §8)") {
    val huge = 1 << 20
    val text = StateKey.parse("a/" * (huge / 2))
    val bytes = StateKey.fromBytes(scodec.bits.ByteVector.fill(huge.toLong)(0xff))
    val namespace = StateNamespace.parse("x" * huge)
    def bounded(r: Either[StateKeyError, ?]) =
      r.left.toOption.fold(failure("refused")) { e =>
        expect(e.toString.length <= 512, s"the refusal renders ${e.toString.length} characters")
          .and(expect(e.input.forall(_.head.length <= 64), "a preview of at most 32 code points"))
      }
    bounded(text)
      .and(bounded(bytes))
      .and(bounded(namespace))
      .and(expect.same(Left(StateKeyError.TooLong(256)), text))
      .and(expect.same(Left(StateKeyError.TooLong(256)), bytes))
      .and(expect.same(Left(StateKeyError.InvalidSegment(Preview.of("a/B/c"), 1)), StateKey.parse("a/B/c")))
      .and(expect.same(Left(StateKeyError.TooLong(256)), namespace))
  }

  pureTest("refusal order (versioned-trie.md §2): empty, UTF-8 length, segment count, then each segment's grammar before its length") {
    val e = "é"
    def parsed(s: String) = StateKey.parse(s).left.toOption
    expect
      .same(Some(StateKeyError.Empty), parsed(""))
      // 129 code units, 258 UTF-8 bytes: the length is in UTF-8 bytes
      .and(expect.same(Some(StateKeyError.TooLong(256)), parsed(e * 129)))
      .and(expect.same(Some(StateKeyError.TooManySegments(Preview.of("A/" * 16 + "a"), 16)), parsed("A/" * 16 + "a")))
      // 40 two-byte characters: refused by the grammar, which comes before the segment's length
      .and(expect.same(Some(StateKeyError.InvalidSegment(Preview.of(e * 40), 0)), parsed(e * 40)))
      .and(expect.same(Some(StateKeyError.InvalidSegment(Preview.of("A" + "x" * 70), 0)), parsed("A" + "x" * 70)))
      .and(expect.same(Some(StateKeyError.SegmentTooLong(Preview.of("x" * 70 + "/A"), 0, 64)), parsed("x" * 70 + "/A")))
      .and(expect.same(Some(StateKeyError.NotUtf8), StateKey.fromBytes(scodec.bits.ByteVector(0xff)).left.toOption))
      .and(
        expect
          .same(Some(StateKeyError.InvalidSegment(Preview.of(e), 0)), StateKey.fromBytes(scodec.bits.ByteVector(0xc3, 0xa9)).left.toOption)
      )
      // an unpaired surrogate counts 3 UTF-8 bytes (no vector holds one: Rust's text cannot): 86 are 258 bytes, 85 are
      // 255, and 254 letters and one surrogate are 255 code units but 257 bytes
      .and(expect.same(Some(StateKeyError.TooLong(256)), parsed("\ud800" * 86)))
      .and(expect.same(Some(StateKeyError.InvalidSegment(Preview.of("\ud800" * 85), 0)), parsed("\ud800" * 85)))
      .and(expect.same(Some(StateKeyError.TooLong(256)), parsed("a" * 254 + "\ud800")))
      // a pair is 4 bytes: 253 letters and one are 257, 252 and one exactly 256
      .and(expect.same(Some(StateKeyError.TooLong(256)), parsed("a" * 253 + "😀")))
      .and(expect.same(Some(StateKeyError.InvalidSegment(Preview.of("a" * 252 + "😀"), 0)), parsed("a" * 252 + "😀")))
      .and(namespaceFull(List("a" * 64, "a" * 64, "a" * 64, "a" * 61).mkString("/")))
      .and(namespaceFull(List.fill(16)("a").mkString("/")))
  }

  /** A valid key that is no namespace: 255 or 256 characters, or 16 segments, leave no room for one more. */
  private def namespaceFull(s: String) =
    expect(StateKey.parse(s).isRight, s"$s is a key")
      .and(expect.same(Some(StateKeyError.NamespaceFull(Preview.of(s))), StateNamespace.parse(s).left.toOption))

  pureTest("a namespace prefix is segment-exact: `fiber` holds fiber/x, never fiber or fiberx/y") {
    val fiber = ns("fiber")
    expect(fiber.contains(key("fiber/x")))
      .and(expect(fiber.contains(key("fiber/x/y"))))
      .and(expect(!fiber.contains(key("fiber"))))
      .and(expect(!fiber.contains(key("fiberx/y"))))
      .and(expect(ns("a/b").contains(key("a/b/c")) && !ns("a/b").contains(key("a/bc"))))
      .and(expect(StateNamespace.parse("a" * 255).isLeft, "no room for a key under it"))
  }

  pureTest("a state key is a bare string on the wire, checked by the schema's pattern and round-tripping") {
    val codec = xyz.kd5ujc.models.versioned_trie.StateKey.codec
    val wire = codec.encode(key("fiber/1").value).map(xyz.kd5ujc.core.canonical.CanonicalGens.encoded)
    expect
      .same(Right(get(CanonicalValue.text("fiber/1").map(xyz.kd5ujc.core.canonical.CanonicalGens.encoded))), wire)
      .and(
        expect
          .same(
            Right("fiber/1"),
            wire
              .leftMap(MalformedReason.Canonical(_))
              .flatMap(b => Cbor.decode(b).leftMap(MalformedReason.Canonical(_)))
              .flatMap(codec.decoder.decode(_).leftMap(MalformedReason.Decode(_)))
          )
      )
      .and(expect(get(CanonicalValue.text("Fiber/1")).pipe(codec.decoder.decode).isLeft, "the pattern rejects upper case"))
  }

  test("version keys preserve order as unsigned bytes and round-trip; the default is 8 bytes, unlike KeyCodec.longKey") {
    forall(Gen.zip(Gen.choose(0L, Long.MaxValue), Gen.choose(0L, Long.MaxValue))) {
      case (a, b) =>
        val ka = vk(a)
        val kb = vk(b)
        expect
          .same(a.compare(b).sign, Cbor.compareBytes(ka, kb).sign)
          .and(expect.same(Right(a), VersionKey.long.decode(ka)))
          .and(expect.same(8L, ka.size))
          .and(expect(a == 0L || KeyCodec.longKey.encode(a) != ka, "the storage codec flips the sign bit"))
    }
  }

  pureTest("narrow version keys hold exactly their range; widths outside 1..8 and negative versions are refused") {
    val w2 = get(VersionKey.unsignedLong(2))
    expect
      .same(Right(scodec.bits.ByteVector(0xff, 0xff)), w2.encode(65535L))
      .and(expect(w2.encode(65536L).isLeft, "65536 needs three bytes"))
      .and(expect(VersionKey.long.encode(-1L).isLeft, "negative"))
      .and(expect(VersionKey.unsignedLong(0).isLeft && VersionKey.unsignedLong(9).isLeft, "width"))
      .and(expect(VersionKey.long.decode(scodec.bits.ByteVector(1, 2, 3)).isLeft, "a short key"))
      .and(expect(VersionKey.long.decode(scodec.bits.ByteVector.fromLong(-1L)).isLeft, "above Long.MaxValue"))
  }

  // ---- projection

  final private case class Ledger(balances: Map[String, Int])

  private val honest: StateView[Ledger] = new StateView[Ledger] {
    def entries(s: Ledger): SortedMap[StateKey, CanonicalValue] =
      SortedMap.from(s.balances.map { case (who, n) => key(s"balance/$who") -> int(n) })
  }

  /** A view whose hand-written delta forgets removals: a wiring bug the core cannot see. */
  private val lying: StateView[Ledger] = new StateView[Ledger] {
    def entries(s: Ledger): SortedMap[StateKey, CanonicalValue] = honest.entries(s)
    override def delta(prev: Ledger, next: Ledger): Batch[StateKey, CanonicalValue] =
      Batch.puts(next.balances.toList.map { case (who, n) => key(s"balance/$who") -> int(n) })
  }

  test("the default view delta commits exactly the next state's entries") {
    val s0 = Ledger(Map("a" -> 1, "b" -> 2))
    val s1 = Ledger(Map("a" -> 1, "c" -> 3))
    attempt { implicit h =>
      fresh.flatMap(s => s._2.commit(None, 0L, honest.delta(Ledger(Map.empty), s0)) >> s._2.commit(Some(0L), 1L, honest.delta(s0, s1)))
    }.map { head =>
      expect
        .same(Right(Right(())), head.map(hd => Projection.verifyAgainst[H](honest.entries(s1), hd.stateRoot)))
        .and(
          expect.same(Batch.delete[StateKey, CanonicalValue](key("balance/b")) ++ Batch.put(key("balance/c"), int(3)), honest.delta(s0, s1))
        )
    }
  }

  test("a lying view delta is caught by verifyAgainst as a RootMismatch on the state") {
    val s0 = Ledger(Map("a" -> 1, "b" -> 2))
    val s1 = Ledger(Map("a" -> 1))
    attempt { implicit h =>
      fresh.flatMap(s => s._2.commit(None, 0L, lying.delta(Ledger(Map.empty), s0)) >> s._2.commit(Some(0L), 1L, lying.delta(s0, s1)))
    }.map { head =>
      expect(
        head.exists(hd =>
          Projection.verifyAgainst[H](lying.entries(s1), hd.stateRoot).left.exists {
            case VersionedTrieError.RootMismatch(VtRootComponent.State, _, _) => true
            case _                                                            => false
          }
        )
      )
    }
  }

  implicit private class Pipe[A](a: A) {
    def pipe[B](f: A => B): B = f(a)
  }
}
