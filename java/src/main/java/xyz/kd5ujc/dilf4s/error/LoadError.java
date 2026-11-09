package xyz.kd5ujc.dilf4s.error;

/**
 * Why the native backends did not load ({@code Backends.load}): which backend, and what it raised, by the throwable's
 * class and its message, bounded in length.
 */
public sealed interface LoadError permits LoadError.Failed, LoadError.Unlinked {

  /** The kind: {@code Failed} or {@code Unlinked}. */
  String kind();

  /**
   * The backend that failed.
   *
   * @return the backend
   */
  Backend backend();

  /** The native backends, in the order they are loaded. */
  enum Backend {
    /** libsecp256k1, through ACINQ's JNI bindings. */
    SECP256K1,
    /** blst, through Teku's jblst, and its self-check. */
    BLS12_381,
    /** The JLVM evaluator, which loads its own secp256k1, BLS12-381 and gnark (BN254) backends. */
    JLVM
  }

  /**
   * The load raised a non-fatal failure (an unsupported platform, the BLS self-check).
   *
   * @param backend the backend
   * @param error the throwable's class
   * @param message its message, at most {@code Backends.MAX_MESSAGE} code points (and a trailing ellipsis if cut)
   */
  record Failed(Backend backend, String error, String message) implements LoadError {
    @Override
    public String kind() {
      return "Failed";
    }
  }

  /**
   * The native library did not link: a {@code LinkageError} ({@code UnsatisfiedLinkError}, {@code
   * ExceptionInInitializerError}, ...), which the effect runtime treats as fatal.
   *
   * @param backend the backend
   * @param error the throwable's class
   * @param message its message, at most {@code Backends.MAX_MESSAGE} code points (and a trailing ellipsis if cut)
   */
  record Unlinked(Backend backend, String error, String message) implements LoadError {
    @Override
    public String kind() {
      return "Unlinked";
    }
  }
}
