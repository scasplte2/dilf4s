package xyz.kd5ujc.dilf4s.error;

/** Why a value was not encoded, hashed or committed: its encoding passes the limit, or the argument was null. */
public sealed interface EncodeError permits TooLarge, TypeMismatch {

  /** The kind: {@code TooLarge} or {@code TypeMismatch}. */
  String kind();
}
