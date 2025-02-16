package xyz.kd5ujc.accumulators.merkle

import cats.effect.{IO, Ref, Sync}
import cats.syntax.functor._
import cats.syntax.traverse._
import io.circe.syntax._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import xyz.kd5ujc.accumulators.merkle.MerkleNode
import xyz.kd5ujc.accumulators.merkle.api.MerkleProducer.{OptimizedMerkleProducer, ProducerState, SimpleMerkleProducer}
import xyz.kd5ujc.accumulators.merkle.api.MerkleProducer
import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.hash.{Blake2b256Hasher, JsonHasher}

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
class MerkleProducerBenchmark {

  private var simpleProducer: MerkleProducer[IO] = _
  private var optimizedProducer: MerkleProducer[IO] = _
  private var testLeaves: List[MerkleNode.Leaf] = _
  private var updateLeaf: MerkleNode.Leaf = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    val runtime = cats.effect.unsafe.IORuntime.global

    val initialData = (1 to 1000).map(i => s"data$i".asJson).toList
    val result = for {
      implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
      implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
      leaves                                  <- initialData.traverse[IO, MerkleNode.Leaf](data => MerkleNode.Leaf[IO](data))
      updateLeaf                              <- MerkleNode.Leaf[IO]("updated".asJson)
      simple                                  <- simple[IO](leaves)
      optimized                               <- optimized[IO](leaves)
    } yield (leaves, updateLeaf, simple, optimized)

    val (leaves, uLeaf, sim, opt) = result.unsafeRunSync()(runtime)
    testLeaves = leaves
    updateLeaf = uLeaf
    simpleProducer = sim
    optimizedProducer = opt
  }

  def simple[F[_]: Sync: JsonHasher](
    initial: List[MerkleNode.Leaf]
  ): F[MerkleProducer[F]] =
    Ref
      .of[F, Vector[MerkleNode.Leaf]](Vector.from(initial))
      .map(new SimpleMerkleProducer[F](_))

  def optimized[F[_]: Sync: JsonHasher](
    initial: List[MerkleNode.Leaf]
  ): F[MerkleProducer[F]] =
    Ref
      .of[F, ProducerState](
        ProducerState(
          leaves = Vector.from(initial),
          nodeCache = Map.empty,
          dirtyNodes = Set.empty,
          currentRoot = None
        )
      )
      .map(new OptimizedMerkleProducer[F](_))

  @Benchmark
  def buildOriginal(blackhole: Blackhole): Unit = {
    val runtime = cats.effect.unsafe.IORuntime.global
    val tree = simpleProducer.build.unsafeRunSync()(runtime)
    blackhole.consume(tree)
  }

  @Benchmark
  def buildOptimized(blackhole: Blackhole): Unit = {
    val runtime = cats.effect.unsafe.IORuntime.global
    val tree = optimizedProducer.build.unsafeRunSync()(runtime)
    blackhole.consume(tree)
  }

  @Benchmark
  def updateAndBuildOriginal(blackhole: Blackhole): Unit = {
    val runtime = cats.effect.unsafe.IORuntime.global
    val result = for {
      _    <- simpleProducer.update(500, updateLeaf)
      tree <- simpleProducer.build
    } yield tree
    blackhole.consume(result.unsafeRunSync()(runtime))
  }

  @Benchmark
  def updateAndBuildOptimized(blackhole: Blackhole): Unit = {
    val runtime = cats.effect.unsafe.IORuntime.global
    val result = for {
      _    <- optimizedProducer.update(500, updateLeaf)
      tree <- optimizedProducer.build
    } yield tree
    blackhole.consume(result.unsafeRunSync()(runtime))
  }

  @Benchmark
  def appendAndBuildOriginal(blackhole: Blackhole): Unit = {
    val runtime = cats.effect.unsafe.IORuntime.global
    val result = for {
      _    <- simpleProducer.append(testLeaves.take(10))
      tree <- simpleProducer.build
    } yield tree
    blackhole.consume(result.unsafeRunSync()(runtime))
  }

  @Benchmark
  def appendAndBuildOptimized(blackhole: Blackhole): Unit = {
    val runtime = cats.effect.unsafe.IORuntime.global
    val result = for {
      _    <- optimizedProducer.append(testLeaves.take(10))
      tree <- optimizedProducer.build
    } yield tree
    blackhole.consume(result.unsafeRunSync()(runtime))
  }
}
