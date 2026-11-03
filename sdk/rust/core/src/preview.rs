//! [`Preview`]: caller text as a diagnostic may show it (docs/specs/conformance.md section 8).

use alloc::string::String;
use core::fmt;

/// The most code points a [`Preview`] keeps.
pub const MAX_CODE_POINTS: usize = 32;

/// Caller text as an error may hold it: its first [`MAX_CODE_POINTS`] code points, each unpaired surrogate replaced by
/// U+FFFD, and its whole length in UTF-8 bytes, an unpaired surrogate counting 3. The fields are private and
/// [`Preview::of`] truncates, so an error holding a preview holds at most that much of what the caller sent, however
/// long the text was.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Preview {
    head: String,
    length: usize,
}

impl Preview {
    /// The preview of `text`: O([`MAX_CODE_POINTS`]), its length read, not measured.
    pub fn of(text: &str) -> Self {
        Self::of_wtf8(text.as_bytes())
    }

    /// The preview of generalised UTF-8 (WTF-8), where an unpaired surrogate is its 3-byte form: the JSON parser's
    /// member names before they are checked. Any other ill-formed sequence also previews as U+FFFD.
    pub(crate) fn of_wtf8(bytes: &[u8]) -> Self {
        let head = (0..MAX_CODE_POINTS)
            .scan(0usize, |at, _| {
                let lead = *bytes.get(*at)?;
                let width = match lead {
                    0xc0..=0xdf => 2,
                    0xe0..=0xef => 3,
                    0xf0..=0xf7 => 4,
                    _ => 1,
                };
                let end = at.saturating_add(width).min(bytes.len());
                let unit = bytes.get(*at..end).unwrap_or_default();
                *at = end;
                Some(
                    core::str::from_utf8(unit)
                        .ok()
                        .and_then(|s| s.chars().next())
                        .unwrap_or(char::REPLACEMENT_CHARACTER),
                )
            })
            .collect();
        Self { head, length: bytes.len() }
    }

    /// The first [`MAX_CODE_POINTS`] code points.
    pub fn head(&self) -> &str {
        &self.head
    }

    /// The whole text's length in UTF-8 bytes.
    pub const fn length(&self) -> usize {
        self.length
    }

    /// Whether the text was longer than [`Preview::head`].
    pub fn is_truncated(&self) -> bool {
        self.head.len() < self.length
    }

    /// The head with JSON's escapes, then `…` when truncated: a path segment's form.
    pub fn short(&self) -> Short<'_> {
        Short(self)
    }
}

/// [`Preview::short`].
pub struct Short<'a>(&'a Preview);

impl fmt::Display for Short<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        escaped(&self.0.head, f)?;
        f.write_str(if self.0.is_truncated() { "…" } else { "" })
    }
}

/// The head quoted, `…` when truncated, and the length: `"abc" (UTF-8 length 3)`.
impl fmt::Display for Preview {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("\"")?;
        escaped(&self.head, f)?;
        write!(f, "\"{} (UTF-8 length {})", if self.is_truncated() { "…" } else { "" }, self.length)
    }
}

fn escaped(head: &str, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    head.chars().try_for_each(|c| match c {
        '"' => f.write_str("\\\""),
        '\\' => f.write_str("\\\\"),
        c if c < ' ' || c == '\u{7f}' => write!(f, "\\u{:04x}", u32::from(c)),
        c => write!(f, "{c}"),
    })
}
