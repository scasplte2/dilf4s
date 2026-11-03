package xyz.kd5ujc.core.canonical

import scala.annotation.tailrec

/**
 * Caller text as a diagnostic may show it (docs/specs/conformance.md §8): its first [[Preview.MaxCodePoints]] code
 * points, each unpaired surrogate replaced by U+FFFD, and its whole length in UTF-8 bytes, an unpaired surrogate
 * counting 3. [[Preview.of]] is the only way to build one, and it truncates, so an error holding a preview holds at most
 * that much of what the caller sent, however long the text was.
 */
sealed abstract case class Preview private (head: String, length: Long) {

  /** Whether the text was longer than [[head]]. */
  def truncated: Boolean = Preview.utf8Length(head) < length

  /** The head with JSON's escapes, then `…` when truncated: a path segment's form. */
  def short: String = Preview.escape(head) + (if (truncated) "…" else "")

  /** The head quoted, `…` when truncated, and the length: `"abc" (UTF-8 length 3)`. */
  override def toString: String = s"\"${Preview.escape(head)}\"${if (truncated) "…" else ""} (UTF-8 length $length)"
}

object Preview {

  /** The most code points a preview keeps. */
  val MaxCodePoints: Int = 32

  /** The preview of `s`: O(MaxCodePoints) to copy, and one scan, without allocation, to measure. */
  def of(s: String): Preview = new Preview(head(s, 0, 0, Vector.empty), utf8Length(s)) {}

  @tailrec
  private def head(s: String, i: Int, n: Int, acc: Vector[Int]): String =
    if (i >= s.length || n >= MaxCodePoints) new String(acc.toArray, 0, acc.size)
    else {
      val c = s.codePointAt(i)
      head(s, i + Character.charCount(c), n + 1, acc :+ (if (c >= 0xd800 && c <= 0xdfff) 0xfffd else c))
    }

  /** The UTF-8 length of `s`, an unpaired surrogate counting 3 (as its replacement, U+FFFD). */
  private def utf8Length(s: String): Long = measure(s, 0, 0L)

  @tailrec
  private def measure(s: String, i: Int, acc: Long): Long =
    if (i >= s.length) acc
    else {
      val c = s.codePointAt(i)
      measure(s, i + Character.charCount(c), acc + (if (c < 0x80) 1 else if (c < 0x800) 2 else if (c < 0x10000) 3 else 4))
    }

  private def escape(s: String): String =
    s.flatMap {
      case '"'                       => "\\\""
      case '\\'                      => "\\\\"
      case c if c < ' ' || c == 0x7f => f"\\u${c.toInt}%04x"
      case c                         => c.toString
    }
}
