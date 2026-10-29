package xyz.kd5ujc.accumulators.merkle

import cats.effect.IO
import cats.mtl.{Handle, Raise}
import cats.syntax.all._

import xyz.kd5ujc.core.hash.Sha256
import xyz.kd5ujc.storage.{RawStore, StoreError}

import org.scalacheck.Gen
import scodec.bits.ByteVector
import weaver.scalacheck.Checkers
import weaver.{Expectations, SimpleIOSuite}

object MerkleLogSuite extends SimpleIOSuite with Checkers {

  implicit private val m: MerkleHashing[Sha256] = MerkleHashing.tagged[Sha256]

  sealed private trait Failure
  final private case class Store(e: StoreError) extends Failure
  final private case class Tree(e: MerkleError) extends Failure

  /** Both error channels, handled at the edge with cats-mtl. */
  private def run[A](f: (Raise[IO, StoreError], Raise[IO, MerkleError]) => IO[A]): IO[Either[Failure, A]] =
    Handle
      .allowF[IO, StoreError] { rs =>
        Handle.allowF[IO, MerkleError](rm => f(rs, rm).map(_.asRight[Failure])).rescue(e => IO.pure(Tree(e).asLeft[A]))
      }
      .rescue(e => IO.pure(Store(e).asLeft[A]))

  private def ok(f: (Raise[IO, StoreError], Raise[IO, MerkleError]) => IO[Expectations]): IO[Expectations] =
    run(f).map(_.fold(e => failure(s"unexpected $e"), identity))

  private def leaf(i: Int) = m.leaf(ByteVector.fromInt(i))

  sealed private trait Cmd
  final private case class Append(n: List[Int]) extends Cmd
  final private case class Update(i: Int, v: Int) extends Cmd
  final private case class Prepend(v: Int) extends Cmd
  final private case class Remove(i: Int) extends Cmd

  private val cmd: Gen[Cmd] = Gen.oneOf(
    Gen.listOf(Gen.choose(0, 99)).map(Append(_)),
    Gen.zip(Gen.choose(0, 20), Gen.choose(0, 99)).map { case (i, v) => Update(i, v) },
    Gen.choose(0, 99).map(Prepend(_)),
    Gen.choose(0, 20).map(Remove(_))
  )
  implicit private val showCmds: cats.Show[List[Cmd]] = cats.Show.fromToString

  /** Applies commands to the pure tree, skipping out-of-range ones (the log must reject those the same way). */
  private def model(cmds: List[Cmd]): MerkleTree[Sha256] =
    cmds.foldLeft(MerkleTree.empty[Sha256]) {
      case (t, Append(ns))   => t.appendAll(ns.map(leaf))
      case (t, Update(i, v)) => t.update(i.toLong, leaf(v)).getOrElse(t)
      case (t, Prepend(v))   => t.prependAll(List(leaf(v)))
      case (t, Remove(i))    => t.removeAt(i.toLong).getOrElse(t)
    }

  test("in-memory and persistent logs track the pure tree through arbitrary operations") {
    forall(Gen.listOf(cmd).map(_.take(25))) { cmds =>
      ok { (rs, rm) =>
        implicit val (s, t) = (rs, rm)
        for {
          raw        <- RawStore.inMemory[IO]
          memory     <- MerkleLog.inMemory[IO, Sha256]
          persistent <- MerkleLog.persistent[IO, Sha256](raw, "log")
          _          <- cmds.traverse_(c => applyBoth(memory, persistent, c))
          reopened   <- MerkleLog.persistent[IO, Sha256](raw, "log")
          roots      <- List(memory, persistent, reopened).traverse(_.root)
        } yield expect.same(List.fill(3)(model(cmds).root), roots)
      }
    }
  }

  /**
   * Runs one command on both logs, skipping out-of-range indices exactly as the model does. (A log raises through the
   * `Raise` it was built with, so rejections are covered by their own test rather than handled here.)
   */
  private def applyBoth(a: MerkleLog[IO, Sha256], b: MerkleLog[IO, Sha256], c: Cmd): IO[Unit] =
    a.size.flatMap { n =>
      def inRange(i: Int) = i.toLong < n
      List(a, b).traverse_ { log =>
        c match {
          case Append(ns)                 => log.append(ns.map(leaf))
          case Update(i, v) if inRange(i) => log.update(i.toLong, leaf(v))
          case Prepend(v)                 => log.prepend(List(leaf(v)))
          case Remove(i) if inRange(i)    => log.remove(i.toLong)
          case _                          => IO.unit
        }
      }
    }

  test("persistent proofs equal the in-memory tree's after every command") {
    forall(Gen.listOf(cmd).map(_.take(20))) { cmds =>
      ok { (rs, rm) =>
        implicit val (s, t) = (rs, rm)
        for {
          raw        <- RawStore.inMemory[IO]
          persistent <- MerkleLog.persistent[IO, Sha256](raw, "log")
          memory     <- MerkleLog.inMemory[IO, Sha256]
          checks <- cmds.traverse { c =>
            applyBoth(memory, persistent, c) >> persistent.size.flatMap { n =>
              val idx = (0L until n).toList
              (
                idx.traverse(memory.inclusionProof),
                idx.traverse(persistent.inclusionProof),
                (0L to n).toList.traverse(memory.consistencyProof),
                (0L to n).toList.traverse(persistent.consistencyProof),
                memory.leaves.compile.toList,
                persistent.leaves.compile.toList
              ).mapN((mi, pi, mc, pc, ml, pl) => expect.same(mi, pi).and(expect.same(mc, pc)).and(expect.same(ml, pl)))
            }
          }
        } yield checks.foldLeft(success)(_ and _)
      }
    }
  }

  test("a large persistent log opens without loading it and proves with logarithmic reads") {
    ok { (rs, rm) =>
      implicit val (s, t) = (rs, rm)
      for {
        raw      <- RawStore.inMemory[IO]
        log      <- MerkleLog.persistent[IO, Sha256](raw, "log")
        _        <- (0 until 20).toList.traverse_(b => log.append((b * 1000 until (b + 1) * 1000).toList.map(leaf)))
        reopened <- MerkleLog.persistent[IO, Sha256](raw, "log")
        root     <- reopened.root
        proof    <- reopened.inclusionProof(12345)
        stored   <- raw.scan(xyz.kd5ujc.storage.ByteRange.all).compile.count
      } yield
        expect(MerkleVerifier.inclusion(root, leaf(12345), proof).isRight)
          .and(expect.same(15, proof.path.size))
          .and(expect(stored < 2L * 20000 + 2, s"stores only perfect nodes: $stored records for 20000 leaves"))
    }
  }

  test("a second instance over the same store sees the first instance's writes") {
    ok { (rs, rm) =>
      implicit val (s, t) = (rs, rm)
      for {
        raw   <- RawStore.inMemory[IO]
        a     <- MerkleLog.persistent[IO, Sha256](raw, "log")
        b     <- MerkleLog.persistent[IO, Sha256](raw, "log")
        _     <- a.append(List(leaf(1), leaf(2)))
        seenB <- b.size
        _     <- b.append(List(leaf(3)))
        seenA <- a.root
      } yield expect.same(2L, seenB).and(expect.same(MerkleTree.fromLeafHashes[Sha256](List(leaf(1), leaf(2), leaf(3))).root, seenA))
    }
  }

  test("proofs from the log verify against its root") {
    ok { (rs, rm) =>
      implicit val (s, t) = (rs, rm)
      for {
        raw  <- RawStore.inMemory[IO]
        log  <- MerkleLog.persistent[IO, Sha256](raw, "log")
        _    <- log.append((0 until 13).toList.map(leaf))
        root <- log.root
        inc  <- log.inclusionProof(7)
        con  <- log.consistencyProof(5)
        old = MerkleTree.fromLeafHashes[Sha256]((0 until 5).map(leaf))
      } yield
        expect(MerkleVerifier.inclusion(root, leaf(7), inc).isRight).and(expect(MerkleVerifier.consistency(old.root, root, con).isRight))
    }
  }

  test("out-of-range operations raise typed Merkle errors") {
    run { (rs, rm) =>
      implicit val (s, t) = (rs, rm)
      MerkleLog.inMemory[IO, Sha256].flatMap(_.update(3, leaf(1)))
    }.map(r => expect.same(Left(Tree(MerkleError.IndexOutOfRange(3, 0))), r))
  }
}
