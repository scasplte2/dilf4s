package xyz.kd5ujc.jlvm

import cats.data.{EitherT, State}
import cats.effect.Sync
import cats.syntax.functor._

import scala.collection.immutable.TreeMap

import xyz.kd5ujc.core.canonical.CanonicalValue

/**
 * A program checked for evaluation ([[Node.compile]]): its special forms are well-formed. Not a case class, so no
 * product mirror can pair a program with a tree [[Node.compile]] did not produce.
 */
final class Compiled private (val program: Program, val root: Node) {
  override def equals(that: Any): Boolean = that match {
    case c: Compiled => c.program == program && c.root == root
    case _           => false
  }
  override def hashCode: Int = (program, root).hashCode
  override def toString: String = s"Compiled($program, $root)"
}

object Compiled {
  def apply(program: Program): Either[JlvmError, Compiled] = Node.compile(program).map(new Compiled(program, _))
}

/** How to evaluate: the gas limit and schedule, and which driver runs the semantics. */
final case class Config(
  limit:    GasLimit = GasLimit.Default,
  schedule: GasSchedule = GasSchedule.Latest,
  driver:   Config.Driver = Config.Driver.Direct
)

object Config {

  enum Driver {

    /**
     * The production driver: the explicit continuation stack as one pure tail-recursive function of (step, stack,
     * meter). Constant host stack, and no monad per step.
     */
    case Direct

    /** The same stack machine run by `tailRecM` over cats-mtl `Stateful`/`Raise`: a reference, ~12x slower per step. */
    case StackMachine

    /** Nested computations mirroring the program: the reference the stack machines are checked against. */
    case Recursive
  }
}

/**
 * The JLVM. Evaluation is a pure function of the program, the data, the host bindings, the language version and the gas
 * schedule; it reports the gas used whether it succeeds or fails. Obtain one with [[Jlvm.load]], which loads the
 * cryptographic backends once.
 */
final class Jlvm private (crypto: CryptoOps) {

  private type Run[A] = EitherT[State[Meter, *], JlvmError, A]

  /**
   * Evaluates `compiled` against `data`. `env` holds the host bindings, read as `$name` (e.g. `$ordinal`); `let` and
   * callback bindings shadow them. Out of gas, the whole limit is used.
   */
  def evaluate(
    compiled: Compiled,
    data:     CanonicalValue,
    env:      Map[String, CanonicalValue] = Map.empty,
    config:   Config = Config()
  ): Outcome = {
    val semantics = new Semantics(config.schedule, crypto)
    val first = semantics.start(compiled.root, Scope(data, TreeMap.from(env)))
    val start = Meter(Gas.Zero, config.limit.amount)
    val (meter, result) = config.driver match {
      case Config.Driver.StackMachine => Machine.stackMachine[Run](semantics, first).value.run(start).value
      case Config.Driver.Recursive    => Machine.recursive[Run](semantics, first).value.run(start).value
      case Config.Driver.Direct       => Machine.direct(semantics, first, start)
    }
    Outcome(meter.used, result, meter.warnings)
  }

  /** Compiles and evaluates; a program that does not compile uses no gas. */
  def evaluate(program: Program, data: CanonicalValue, env: Map[String, CanonicalValue], config: Config): Outcome =
    Compiled(program).fold(e => Outcome(Gas.Zero, Left(e)), evaluate(_, data, env, config))
}

object Jlvm {

  /** Loads the cryptographic backends (native libraries) and returns an evaluator. */
  def load[F[_]: Sync]: F[Jlvm] = CryptoOps.load[F].map(new Jlvm(_))
}
