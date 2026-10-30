package xyz.kd5ujc.jlvm

import cats.data.{EitherT, State}

import xyz.kd5ujc.core.canonical.CanonicalValue

/** A program checked for evaluation ([[Node.compile]]): its special forms are well-formed. */
final case class Compiled private (program: Program, root: Node)

object Compiled {
  def apply(program: Program): Either[JlvmError, Compiled] = Node.compile(program).map(new Compiled(program, _))
}

/** How to evaluate: the gas limit and schedule, and which driver runs the semantics. */
final case class Config(
  limit:    GasLimit = GasLimit.Default,
  schedule: GasSchedule = GasSchedule.Latest,
  driver:   Config.Driver = Config.Driver.StackMachine
)

object Config {

  enum Driver {

    /** Constant host stack: the production driver. */
    case StackMachine

    /** Nested computations mirroring the program: the reference driver, checked against the stack machine. */
    case Recursive
  }
}

/**
 * The JLVM entry point. Evaluation is a pure function of the program, the data, the host bindings, the language version
 * and the gas schedule; it reports the gas used whether it succeeds or fails.
 */
object Jlvm {

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
    val semantics = new Semantics(config.schedule)
    val first = semantics.start(compiled.root, Scope(data, env))
    val run: Run[CanonicalValue] = config.driver match {
      case Config.Driver.StackMachine => Machine.stackMachine[Run](semantics, first)
      case Config.Driver.Recursive    => Machine.recursive[Run](semantics, first)
    }
    val (meter, result) = run.value.run(Meter(Gas.Zero, config.limit.amount)).value
    Outcome(meter.used, result)
  }

  /** Compiles and evaluates; a program that does not compile uses no gas. */
  def evaluate(program: Program, data: CanonicalValue, env: Map[String, CanonicalValue], config: Config): Outcome =
    Compiled(program).fold(e => Outcome(Gas.Zero, Left(e)), evaluate(_, data, env, config))
}
