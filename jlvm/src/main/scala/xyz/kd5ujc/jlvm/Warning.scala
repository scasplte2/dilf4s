package xyz.kd5ujc.jlvm

/**
 * Lint findings recorded during evaluation. They never change a result or its gas; they point at program text that
 * reads unambiguously only for the data it happened to meet.
 */
enum Warning {

  /**
   * Inside a callback, a plain path resolved in an element, and an outer element or the data holds the same first key.
   * `$current` (or an explicit outer path) says which one is meant.
   */
  case AmbiguousPath(path: String)
}

object Warning {

  /** At most this many distinct warnings are kept per evaluation. */
  val Max: Int = 64
}
