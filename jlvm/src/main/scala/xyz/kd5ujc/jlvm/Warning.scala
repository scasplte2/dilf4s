package xyz.kd5ujc.jlvm

import xyz.kd5ujc.core.canonical.Preview

/**
 * Lint findings recorded during evaluation. They never change a result or its gas; they point at program text that
 * reads unambiguously only for the data it happened to meet.
 */
enum Warning {

  /**
   * Inside a callback, a plain path resolved in an element, and an outer element or the data holds the same first key.
   * `$current` (or an explicit outer path) says which one is meant. The path is the caller's, so it is held as a
   * preview (docs/specs/conformance.md §8): two long paths that share their preview count as one warning.
   */
  case AmbiguousPath(path: Preview)
}

object Warning {

  /** At most this many distinct warnings are kept per evaluation. */
  val Max: Int = xyz.kd5ujc.models.limits.JlvmMaxWarnings
}
