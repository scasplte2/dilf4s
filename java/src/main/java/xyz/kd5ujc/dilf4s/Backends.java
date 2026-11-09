package xyz.kd5ujc.dilf4s;

import java.util.Optional;

import cats.effect.SyncIO;
import scala.util.Either;

import xyz.kd5ujc.dilf4s.error.LoadError;
import xyz.kd5ujc.dilf4s.internal.Natives;

/**
 * The reference's native backends, loaded: the JLVM evaluator (with its secp256k1, BLS12-381 and gnark operations), and
 * secp256k1 and BLS12-381 for signing. Given one, every call that takes it ({@link Jlvm#evaluate}, {@link Secp256k1},
 * {@link Bls}) is a pure function of its arguments.
 *
 * <p>{@link #load} builds it: the one effect this API runs. It runs the reference's own loads ({@code
 * Secp256k1.load[F]}, {@code Bls12381.load[F]} with its self-check, {@code Jlvm.load[F]}) once per process, and keeps
 * the outcome, success or refusal, for every later call.
 */
public final class Backends {
  final xyz.kd5ujc.jlvm.Jlvm jlvm;
  final xyz.kd5ujc.crypto.signing.Secp256k1 secp256k1;
  final xyz.kd5ujc.crypto.bls.Bls12381 bls;

  Backends(xyz.kd5ujc.jlvm.Jlvm jlvm, xyz.kd5ujc.crypto.signing.Secp256k1 secp256k1, xyz.kd5ujc.crypto.bls.Bls12381 bls) {
    this.jlvm = jlvm;
    this.secp256k1 = secp256k1;
    this.bls = bls;
  }

  /** The most code points of a throwable's message a {@link LoadError} keeps. */
  public static final int MAX_MESSAGE = 256;

  /**
   * The native backends, loaded once per process: the first call runs the loads, and every call returns that outcome.
   * A backend whose library is missing or does not link on this platform is a {@link LoadError} naming it, never an
   * exception, and is not retried: a failed class initialiser cannot be run again.
   *
   * @return the backends, or why they did not load
   */
  public static Result<Backends, LoadError> load() {
    return Loaded.OUTCOME;
  }

  /** Initialised on the first {@link #load}, once, by the JVM's class initialisation (thread-safe and lazy). */
  private static final class Loaded {
    static final Result<Backends, LoadError> OUTCOME = loadWith(Loads.REFERENCE);
  }

  /** The loads, as the reference describes them, each attempted and none yet run. A seam for the tests. */
  record Loads(
    SyncIO<Either<Throwable, xyz.kd5ujc.crypto.signing.Secp256k1>> secp256k1,
    SyncIO<Either<Throwable, xyz.kd5ujc.crypto.bls.Bls12381>> bls,
    SyncIO<Either<Throwable, xyz.kd5ujc.jlvm.Jlvm>> jlvm
  ) {
    static final Loads REFERENCE = new Loads(Natives.secp256k1(), Natives.bls(), Natives.jlvm());
  }

  /** Runs {@code loads} in order, secp256k1, BLS12-381, then the JLVM, stopping at the first that fails. Uncached. */
  static Result<Backends, LoadError> loadWith(Loads loads) {
    return run(LoadError.Backend.SECP256K1, loads.secp256k1())
      .flatMap(s -> run(LoadError.Backend.BLS12_381, loads.bls())
        .flatMap(b -> run(LoadError.Backend.JLVM, loads.jlvm()).map(j -> new Backends(j, s, b))));
  }

  /**
   * The one place this API runs an effect: a load, in {@code SyncIO} (no runtime, no threads). The load's own {@code
   * attempt} holds a non-fatal failure as a value. A {@code LinkageError} from a native library that will not load is
   * fatal to the effect runtime, which rethrows it, so it is caught here, around this one call, and nothing broader is:
   * any other error (a {@code VirtualMachineError}) is the JVM's own and propagates.
   */
  private static <T> Result<T, LoadError> run(LoadError.Backend backend, SyncIO<Either<Throwable, T>> load) {
    try {
      return load.unsafeRunSync().<Result<T, LoadError>>fold(e -> Result.err(failed(backend, e)), Result::ok);
    } catch (LinkageError e) {
      return Result.err(new LoadError.Unlinked(backend, e.getClass().getName(), bounded(e.getMessage())));
    }
  }

  private static LoadError failed(LoadError.Backend backend, Throwable e) {
    return new LoadError.Failed(backend, e.getClass().getName(), bounded(e.getMessage()));
  }

  /** At most {@link #MAX_MESSAGE} code points of {@code message}, and an ellipsis if it was longer; none is empty. */
  static String bounded(String message) {
    final String m = Optional.ofNullable(message).orElse("");
    final String head = m.codePoints().limit(MAX_MESSAGE).collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append).toString();
    return head.length() < m.length() ? head + "…" : head;
  }

  @Override
  public String toString() {
    return "Backends(JLVM, secp256k1, BLS12-381)";
  }
}
