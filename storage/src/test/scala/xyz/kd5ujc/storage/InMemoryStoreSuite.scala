package xyz.kd5ujc.storage

import cats.effect.IO
import cats.mtl.Raise

import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object InMemoryStoreSuite extends SimpleIOSuite with Checkers with StoreBehaviours {
  def backend: String = "in-memory"
  def withRaw[A](use: RawStore[IO] => IO[A])(implicit R: Raise[IO, StoreError]): IO[A] = RawStore.inMemory[IO].flatMap(use)
}
