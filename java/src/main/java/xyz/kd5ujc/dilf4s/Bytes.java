package xyz.kd5ujc.dilf4s;

import java.util.HexFormat;
import java.util.List;
import java.util.Objects;

/** Byte arrays at the boundary: every one in and out is a copy, since Java arrays alias. Internal to this package. */
final class Bytes {
  private Bytes() {}

  /** A copy, null-tolerant: a record's accessor and constructor copy what they hold. */
  static byte[] copy(byte[] b) {
    return b == null ? null : b.clone();
  }

  /** Copies of each array, as an unmodifiable list (null-tolerant, for records). */
  static List<byte[]> copies(List<byte[]> bs) {
    return bs == null ? null : bs.stream().map(Bytes::copy).toList();
  }

  /** An unmodifiable copy, null-tolerant (a record's constructor never throws): {@code List.copyOf} refuses nulls. */
  static <T> List<T> listCopy(List<T> l) {
    return l == null ? null : java.util.Collections.unmodifiableList(new java.util.ArrayList<>(l));
  }

  /** Whether any array of {@code bs}, or the list, is null. */
  static boolean anyNull(List<byte[]> bs) {
    return bs == null || bs.stream().anyMatch(Objects::isNull);
  }

  /** Whether two lists hold equal arrays, in order (null-tolerant). */
  static boolean same(List<byte[]> a, List<byte[]> b) {
    return a == null || b == null
      ? a == b
      : a.size() == b.size() && java.util.stream.IntStream.range(0, a.size()).allMatch(i -> java.util.Arrays.equals(a.get(i), b.get(i)));
  }

  static String hexes(List<byte[]> bs) {
    return bs == null ? "null" : bs.stream().map(Bytes::hex).toList().toString();
  }

  static String hex(byte[] b) {
    return b == null ? "null" : HexFormat.of().formatHex(b);
  }
}
