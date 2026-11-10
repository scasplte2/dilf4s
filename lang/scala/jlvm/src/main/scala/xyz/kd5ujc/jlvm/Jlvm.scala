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
  def apply(program: Program): Either[JlvmError, Compiled] = apply(program, Kernels.Default)

  /** Compiles with `kernels` ordering the `let` names: production passes [[Kernels.Default]]. */
  private[jlvm] def apply(program: Program, kernels: Kernels): Either[JlvmError, Compiled] =
    Node.compile(program, kernels).map(new Compiled(program, _))
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
   * callback bindings shadow them. Out of gas, the whole limit is used. A limit over [[GasLimit.Max]] is `Limit`, with
   * no gas used.
   *
   * Every byte string in `data`, `env` and a program's literals is flat ([[FlatBytes]]: core's `CBytes` compacts what it
   * is given, once, when the host builds the value), so none is a rope. A host's lazy view (`ByteVector.viewAt`, `map`,
   * `reverse`) is one chunk too, whose reads run host code: a host should not pass one. Nothing is walked here: that
   * would be work proportional to the whole input on every evaluation.
   */
  def evaluate(
    compiled: Compiled,
    data:     CanonicalValue,
    env:      Map[String, CanonicalValue] = Map.empty,
    config:   Config = Config()
  ): Outcome = evaluate(compiled, data, env, config, Kernels.Default)

  /** [[evaluate]] with `kernels` ordering bindings and values: production passes [[Kernels.Default]]. */
  private[jlvm] def evaluate(
    compiled: Compiled,
    data:     CanonicalValue,
    env:      Map[String, CanonicalValue],
    config:   Config,
    kernels:  Kernels
  ): Outcome = GasLimit.refusal(config.limit).fold(run(compiled, data, env, config, kernels))(refused)

  private def refused(e: JlvmError): Outcome = Outcome(Gas.Zero, Left(e))

  private def run(compiled: Compiled, data: CanonicalValue, env: Map[String, CanonicalValue], config: Config, kernels: Kernels): Outcome = {
    val semantics = new Semantics(config.schedule, crypto, kernels)
    val first = semantics.start(compiled.root, Scope(data, TreeMap.from(env)(using kernels.names)))
    val start = Meter(Gas.Zero, config.limit.amount)
    val (meter, result) = config.driver match {
      case Config.Driver.StackMachine => Machine.stackMachine[Run](semantics, first).value.run(start).value
      case Config.Driver.Recursive    => Machine.recursive[Run](semantics, first).value.run(start).value
      case Config.Driver.Direct       => Machine.direct(semantics, first, start)
    }
    Outcome(meter.used, result, meter.warnings)
  }

  /**
   * Compiles and evaluates; a program that does not compile uses no gas. The limit is checked first: a limit over
   * [[GasLimit.Max]] is `Limit`, whatever the program.
   */
  def evaluate(program: Program, data: CanonicalValue, env: Map[String, CanonicalValue], config: Config): Outcome =
    GasLimit.refusal(config.limit).fold(Compiled(program).fold(refused, evaluate(_, data, env, config)))(refused)
}

object Jlvm {

  /** Loads the cryptographic backends (native libraries) and returns an evaluator. */
  def load[F[_]: Sync]: F[Jlvm] = CryptoOps.load[F].map(new Jlvm(_))
}
