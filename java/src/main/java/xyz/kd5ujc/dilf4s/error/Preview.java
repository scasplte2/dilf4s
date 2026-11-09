package xyz.kd5ujc.dilf4s.error;

/**
 * Caller text as a refusal shows it ({@code docs/specs/conformance.md} section 8): its first 32 code points, each
 * unpaired surrogate replaced by U+FFFD, and its whole length in UTF-8 bytes. A refusal never holds more of what the
 * caller sent than this.
 *
 * @param head the first code points of the text
 * @param length the text's whole length in UTF-8 bytes, an unpaired surrogate counting 3
 * @param truncated whether the text was longer than {@code head}
 */
public record Preview(String head, long length, boolean truncated) {}
