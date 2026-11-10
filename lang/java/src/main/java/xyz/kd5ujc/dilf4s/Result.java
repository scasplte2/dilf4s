package xyz.kd5ujc.dilf4s;

import java.util.Optional;
import java.util.function.Function;

/**
 * The outcome of every fallible call: {@link Ok} with the value, or {@link Err} with a typed refusal. No call in this
 * API throws; a refusal is always a value, so a {@code switch} over a result is exhaustive.
 *
 * <p>The combinators run the caller's functions, which must not be null; whatever they throw is theirs.
 *
 * @param <T> the value
 * @param <E> the refusal
 */
public sealed interface Result<T, E> permits Result.Ok, Result.Err {

  /**
   * A success.
   *
   * @param value the value
   * @param <T> the value
   * @param <E> the refusal it could have been
   */
  record Ok<T, E>(T value) implements Result<T, E> {}

  /**
   * A refusal.
   *
   * @param error the refusal
   * @param <T> the value it could have been
   * @param <E> the refusal
   */
  record Err<T, E>(E error) implements Result<T, E> {}

  /**
   * A success.
   *
   * @param value the value
   * @param <T> the value
   * @param <E> the refusal
   * @return {@code Ok(value)}
   */
  static <T, E> Result<T, E> ok(T value) {
    return new Ok<>(value);
  }

  /**
   * A refusal.
   *
   * @param error the refusal
   * @param <T> the value
   * @param <E> the refusal
   * @return {@code Err(error)}
   */
  static <T, E> Result<T, E> err(E error) {
    return new Err<>(error);
  }

  /**
   * Whether this is a success.
   *
   * @return true for {@link Ok}
   */
  default boolean isOk() {
    return this instanceof Ok;
  }

  /**
   * The value, if this is a success.
   *
   * @return the value, or empty
   */
  default Optional<T> success() {
    return switch (this) {
      case Ok<T, E> ok -> Optional.ofNullable(ok.value());
      case Err<T, E> ignored -> Optional.empty();
    };
  }

  /**
   * The refusal, if this is one.
   *
   * @return the refusal, or empty
   */
  default Optional<E> failure() {
    return switch (this) {
      case Ok<T, E> ignored -> Optional.empty();
      case Err<T, E> err -> Optional.ofNullable(err.error());
    };
  }

  /**
   * The value mapped, a refusal unchanged.
   *
   * @param f the mapping
   * @param <U> the new value
   * @return the mapped result
   */
  default <U> Result<U, E> map(Function<? super T, ? extends U> f) {
    return switch (this) {
      case Ok<T, E> ok -> new Ok<>(f.apply(ok.value()));
      case Err<T, E> err -> new Err<>(err.error());
    };
  }

  /**
   * The next fallible step on the value, a refusal unchanged.
   *
   * @param f the next step
   * @param <U> its value
   * @return its result, or this refusal
   */
  default <U> Result<U, E> flatMap(Function<? super T, Result<U, E>> f) {
    return switch (this) {
      case Ok<T, E> ok -> f.apply(ok.value());
      case Err<T, E> err -> new Err<>(err.error());
    };
  }

  /**
   * The refusal mapped, a value unchanged.
   *
   * @param f the mapping
   * @param <F> the new refusal
   * @return the mapped result
   */
  default <F> Result<T, F> mapErr(Function<? super E, ? extends F> f) {
    return switch (this) {
      case Ok<T, E> ok -> new Ok<>(ok.value());
      case Err<T, E> err -> new Err<>(f.apply(err.error()));
    };
  }

  /**
   * One of two functions, on the refusal or on the value.
   *
   * @param onErr on a refusal
   * @param onOk on a value
   * @param <R> what both return
   * @return what the chosen one returns
   */
  default <R> R fold(Function<? super E, ? extends R> onErr, Function<? super T, ? extends R> onOk) {
    return switch (this) {
      case Ok<T, E> ok -> onOk.apply(ok.value());
      case Err<T, E> err -> onErr.apply(err.error());
    };
  }
}
