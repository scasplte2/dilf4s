package xyz.kd5ujc.core.canonical

/**
 * How canonical maps order and measure their keys: the one seam through which a map touches a key. Every map is a
 * `TreeMap` in this order, so a key is only ever compared, never hashed (keys chosen to collide cannot slow a map
 * down). Production uses [[KeyOrder.Default]]; a test passes one that counts its calls, which bounds the work of map
 * operations on adversarial keys without timing them. The order is the map's own: the encoder sorts entries by their
 * encoded keys whatever it is.
 */
abstract class KeyOrder private[canonical] () extends Ordering[String] {

  /** The UTF-8 length of `key`, or -1 when it holds an unpaired surrogate: one pass over it. */
  def measure(key: String): Long
}

object KeyOrder {

  /**
   * UTF-16 code unit order (`String.compareTo`): a comparison reads only up to the keys' first difference. The
   * canonical order of encoded keys (shorter UTF-8 first) would need both keys' UTF-8 lengths, which a JVM string does
   * not keep, so every comparison would scan both keys whole.
   */
  object Default extends KeyOrder {
    def compare(a:   String, b: String): Int = a.compareTo(b)
    def measure(key: String): Long = Utf16.measure(key)
  }
}
