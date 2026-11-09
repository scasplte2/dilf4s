package xyz.kd5ujc.dilf4s;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.kd5ujc.dilf4s.V.*;

import cats.effect.SyncIO;
import cats.effect.SyncIO$;
import scala.util.Either;

import org.junit.jupiter.api.Test;

import xyz.kd5ujc.dilf4s.error.LoadError;
import xyz.kd5ujc.dilf4s.internal.Natives;

/**
 * {@link Backends#load}: it loads here, once per process; and, through its seam, a load that does not link (a {@code
 * LinkageError}, which the effect runtime rethrows) and one that fails (a non-fatal exception, held by {@code attempt})
 * each become the {@link LoadError} naming the backend, never an exception.
 */
public class BackendsLoadTest {

  @Test
  void loadsOnceHere() {
    final Result<Backends, LoadError> first = Backends.load();
    assertAll(
      () -> assertTrue(first.isOk(), "the backends load on this platform: " + first),
      () -> assertSame(first, Backends.load(), "a second call returns the outcome of the first, not a second load"),
      () -> assertEquals(true, ok(Ed25519.fromSeed(new byte[32]).flatMap(p -> Ed25519.sign(p.signingKey(), new byte[0])), "sign").length == 64, "and the API runs")
    );
  }

  /** A load that throws {@code e} when it runs (a SyncIO's delay rethrows what is fatal, and attempt holds the rest). */
  private static <T> SyncIO<Either<Throwable, T>> throwing(Throwable e) {
    return SyncIO$.MODULE$.<T>delay(() -> sneaky(e)).attempt();
  }

  @SuppressWarnings("unchecked")
  private static <T, X extends Throwable> T sneaky(Throwable e) throws X {
    throw (X) e;
  }

  @Test
  void aLoadThatDoesNotLinkIsUnlinked() {
    final Result<Backends, LoadError> secp = Backends.loadWith(new Backends.Loads(throwing(new UnsatisfiedLinkError("no secp256k1-jni in java.library.path")), Natives.bls(), Natives.jlvm()));
    final Result<Backends, LoadError> jlvm = Backends.loadWith(new Backends.Loads(Natives.secp256k1(), Natives.bls(), throwing(new ExceptionInInitializerError("gnark"))));
    assertAll(
      () -> assertEquals(new LoadError.Unlinked(LoadError.Backend.SECP256K1, "java.lang.UnsatisfiedLinkError", "no secp256k1-jni in java.library.path"), err(secp, "secp")),
      () -> assertEquals(new LoadError.Unlinked(LoadError.Backend.JLVM, "java.lang.ExceptionInInitializerError", "gnark"), err(jlvm, "jlvm"))
    );
  }

  @Test
  void aLoadThatFailsIsFailed() {
    final Result<Backends, LoadError> bls = Backends.loadWith(new Backends.Loads(Natives.secp256k1(), throwing(new IllegalStateException("blst self-check failed")), Natives.jlvm()));
    final Result<Backends, LoadError> unsupported = Backends.loadWith(new Backends.Loads(Natives.secp256k1(), Natives.bls(), throwing(new UnsupportedOperationException())));
    assertAll(
      () -> assertEquals(new LoadError.Failed(LoadError.Backend.BLS12_381, "java.lang.IllegalStateException", "blst self-check failed"), err(bls, "bls")),
      () -> assertEquals(new LoadError.Failed(LoadError.Backend.JLVM, "java.lang.UnsupportedOperationException", ""), err(unsupported, "no message"))
    );
  }

  @Test
  void theFirstFailureStopsTheRest() {
    final Result<Backends, LoadError> both = Backends.loadWith(new Backends.Loads(throwing(new IllegalStateException("one")), throwing(new UnsatisfiedLinkError("two")), Natives.jlvm()));
    assertEquals(LoadError.Backend.SECP256K1, err(both, "both").backend(), "secp256k1 loads first");
  }

  @Test
  void messagesAreBounded() {
    final String long1 = "x".repeat(Backends.MAX_MESSAGE + 10);
    final String pairs = "😀".repeat(Backends.MAX_MESSAGE + 1);
    assertAll(
      () -> assertEquals("x".repeat(Backends.MAX_MESSAGE) + "…", Backends.bounded(long1)),
      () -> assertEquals("😀".repeat(Backends.MAX_MESSAGE) + "…", Backends.bounded(pairs), "by code points, never splitting a pair"),
      () -> assertEquals("", Backends.bounded(null)),
      () -> assertEquals("short", Backends.bounded("short"))
    );
  }
}
