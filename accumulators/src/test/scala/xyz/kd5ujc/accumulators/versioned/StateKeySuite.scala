package xyz.kd5ujc.accumulators.versioned

import cats.syntax.all._

import scala.collection.immutable.SortedMap

import xyz.kd5ujc.accumulators.versioned.Harness._
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}
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
    val wire = codec.encode(key("fiber/1").value).map(Cbor.encode)
    expect
      .same(Right(CanonicalValue.text("fiber/1").map(Cbor.encode).toOption.get), wire)
      .and(
        expect
          .same(Right("fiber/1"), wire.flatMap(b => Cbor.decode(b).leftMap(_.toString)).flatMap(codec.decoder.decode(_).leftMap(_.message)))
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
            case VersionedTrieError.RootMismatch("state", _, _) => true
            case _                                              => false
          }
        )
      )
    }
  }

  implicit private class Pipe[A](a: A) {
    def pipe[B](f: A => B): B = f(a)
  }
}
