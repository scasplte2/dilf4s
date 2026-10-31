//! Typed errors of the canonical data model, its CBOR and JSON forms, and typed decoding.

use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;

/// Why a canonical value, encoding or JSON text was refused (docs/specs/canonical-encoding.md).
///
/// [`CanonicalError::kind`] names the variant exactly as the conformance vectors do.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CanonicalError {
    /// Text with an unpaired UTF-16 surrogate; only JSON `\u` escapes can produce one.
    UnpairedSurrogate,
    /// A map or JSON object with the same key twice.
    DuplicateKey {
        /// The repeated key.
        key: String,
    },
    /// JSON text that is not RFC 8259 syntax.
    JsonSyntax {
        /// Byte offset into the JSON text.
        offset: usize,
        /// What was expected.
        reason: JsonSyntaxError,
    },
    /// A JSON number that is malformed or outside the accepted scale.
    InvalidNumber {
        /// The number as written.
        source: String,
        /// The scale bound it exceeds, when that is the problem.
        scale: Option<ScaleOutOfRange>,
    },
    /// A value with no JSON form: bytes, or a rational with no terminating decimal.
    NotJsonRepresentable {
        /// What could not be written.
        kind: NotJson,
    },
    /// A typed value outside its schema: a range, a size, a pattern, a literal.
    TypeMismatch {
        /// What does not fit.
        reason: String,
    },
    /// The input ended inside an item.
    UnexpectedEnd {
        /// Byte offset where more input was needed.
        offset: usize,
    },
    /// Bytes after the top-level item.
    TrailingBytes {
        /// Byte offset of the first trailing byte.
        offset: usize,
    },
    /// Arrays and maps nested deeper than the limit.
    DepthExceeded {
        /// The limit in force.
        limit: usize,
    },
    /// A well-formed CBOR item that is not the canonical encoding of its value.
    NonCanonical {
        /// Byte offset of the item.
        offset: usize,
        /// Which rule it breaks.
        reason: NonCanonicalReason,
    },
    /// CBOR outside the data model: floats, indefinite lengths, other simple values or tags.
    Unsupported {
        /// Byte offset of the item.
        offset: usize,
        /// What it is.
        item: UnsupportedItem,
    },
    /// A CBOR text string that is not well-formed UTF-8.
    InvalidUtf8 {
        /// Byte offset of the string's content.
        offset: usize,
    },
}

impl CanonicalError {
    /// The error's kind as the conformance vectors name it (`"NonCanonical"`, `"DuplicateKey"`, ...).
    pub const fn kind(&self) -> &'static str {
        match self {
            Self::UnpairedSurrogate => "UnpairedSurrogate",
            Self::DuplicateKey { .. } => "DuplicateKey",
            Self::JsonSyntax { .. } => "JsonSyntax",
            Self::InvalidNumber { .. } => "InvalidNumber",
            Self::NotJsonRepresentable { .. } => "NotJsonRepresentable",
            Self::TypeMismatch { .. } => "TypeMismatch",
            Self::UnexpectedEnd { .. } => "UnexpectedEnd",
            Self::TrailingBytes { .. } => "TrailingBytes",
            Self::DepthExceeded { .. } => "DepthExceeded",
            Self::NonCanonical { .. } => "NonCanonical",
            Self::Unsupported { .. } => "Unsupported",
            Self::InvalidUtf8 { .. } => "InvalidUtf8",
        }
    }
}

impl fmt::Display for CanonicalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnpairedSurrogate => f.write_str("text contains an unpaired surrogate"),
            Self::DuplicateKey { key } => write!(f, "duplicate key {key:?}"),
            Self::JsonSyntax { offset, reason } => {
                write!(f, "JSON syntax error at {offset}: {reason}")
            }
            Self::InvalidNumber { source, scale } => match scale {
                Some(s) => write!(f, "invalid number {source}: {s}"),
                None => write!(f, "invalid number {source}"),
            },
            Self::NotJsonRepresentable { kind } => write!(f, "{kind} has no JSON form"),
            Self::TypeMismatch { reason } => write!(f, "type mismatch: {reason}"),
            Self::UnexpectedEnd { offset } => write!(f, "unexpected end of input at {offset}"),
            Self::TrailingBytes { offset } => write!(f, "trailing bytes at {offset}"),
            Self::DepthExceeded { limit } => write!(f, "nesting deeper than {limit}"),
            Self::NonCanonical { offset, reason } => {
                write!(f, "non-canonical encoding at {offset}: {reason}")
            }
            Self::Unsupported { offset, item } => write!(f, "unsupported item at {offset}: {item}"),
            Self::InvalidUtf8 { offset } => write!(f, "invalid UTF-8 at {offset}"),
        }
    }
}

impl core::error::Error for CanonicalError {}

/// A decimal whose scale exceeds the bound (canonical-encoding.md section 3).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ScaleOutOfRange {
    /// `fraction digits - exponent`, saturated at the `i128` range.
    pub scale: i128,
    /// The largest accepted `|scale|`.
    pub max_scale: u32,
}

impl fmt::Display for ScaleOutOfRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "scale {} outside -{}..{}", self.scale, self.max_scale, self.max_scale)
    }
}

/// What JSON could not hold.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NotJson {
    /// A byte string.
    Bytes,
    /// A rational whose decimal does not terminate.
    NonTerminatingRational,
}

impl fmt::Display for NotJson {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Bytes => "a byte string",
            Self::NonTerminatingRational => "a non-terminating rational",
        })
    }
}

/// The JSON syntax rule that failed.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum JsonSyntaxError {
    /// A value was expected.
    ExpectedValue,
    /// An object member name was expected.
    ExpectedMemberName,
    /// `:` was expected after a member name.
    ExpectedColon,
    /// `,` or `]` was expected.
    ExpectedCommaOrBracket,
    /// `,` or `}` was expected.
    ExpectedCommaOrBrace,
    /// Content after the top-level value.
    TrailingContent,
    /// A string with no closing quote.
    UnterminatedString,
    /// A control character inside a string.
    ControlCharacter,
    /// An unknown or truncated escape.
    InvalidEscape,
}

impl fmt::Display for JsonSyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::ExpectedValue => "expected a value",
            Self::ExpectedMemberName => "expected a member name",
            Self::ExpectedColon => "expected ':'",
            Self::ExpectedCommaOrBracket => "expected ',' or ']'",
            Self::ExpectedCommaOrBrace => "expected ',' or '}'",
            Self::TrailingContent => "unexpected trailing content",
            Self::UnterminatedString => "unterminated string",
            Self::ControlCharacter => "unescaped control character in string",
            Self::InvalidEscape => "invalid escape",
        })
    }
}

/// The deterministic-encoding rule a CBOR item breaks.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NonCanonicalReason {
    /// An argument not in its shortest form.
    NonShortestArgument,
    /// Map keys not unique or not in ascending encoded order.
    KeyOrder,
    /// A null map value (null and absent are the same value).
    NullMapValue,
    /// A bignum magnitude that is empty or has a leading zero byte.
    BignumLeadingZero,
    /// A bignum for a value that fits major type 0 or 1.
    BignumInRange,
    /// Tag 2 or 3 around something other than a byte string.
    BignumNotBytes,
    /// Tag 30 around something other than a 2-element array.
    RationalNotPair,
    /// A rational component that is not an integer.
    RationalNotInteger,
    /// A rational denominator below 2.
    RationalDenominator,
    /// A rational not in lowest terms.
    RationalNotReduced,
}

impl fmt::Display for NonCanonicalReason {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::NonShortestArgument => "argument not in shortest form",
            Self::KeyOrder => "map keys must be unique and in ascending encoded order",
            Self::NullMapValue => "map values must not be null",
            Self::BignumLeadingZero => "bignum magnitude must be non-empty without leading zeros",
            Self::BignumInRange => "bignum used for a value that fits major type 0/1",
            Self::BignumNotBytes => "bignum tag must wrap a byte string",
            Self::RationalNotPair => "rational must wrap a 2-element array",
            Self::RationalNotInteger => "expected an integer",
            Self::RationalDenominator => "rational denominator must be >= 2",
            Self::RationalNotReduced => "rational must be reduced",
        })
    }
}

/// A CBOR item outside the data model.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnsupportedItem {
    /// A float, or a simple value with a one-byte argument (major type 7, additional information 24 to 31).
    FloatOrSimple(u8),
    /// A simple value other than false, true and null.
    SimpleValue(u8),
    /// An indefinite-length item.
    IndefiniteLength,
    /// Reserved additional information 28 to 30.
    ReservedInfo(u8),
    /// A tag other than 2, 3 and 30.
    Tag(u64),
    /// A map key that is not text.
    NonTextKey,
}

impl fmt::Display for UnsupportedItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FloatOrSimple(info) => write!(f, "floating point or simple value (additional information {info})"),
            Self::SimpleValue(v) => write!(f, "simple value {v}"),
            Self::IndefiniteLength => f.write_str("indefinite length"),
            Self::ReservedInfo(info) => write!(f, "reserved additional information {info}"),
            Self::Tag(t) => write!(f, "tag {t}"),
            Self::NonTextKey => f.write_str("map keys must be text"),
        }
    }
}

/// Why a canonical value is not the expected type, and where: `path` runs from the root (map keys and `[i]`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DecodeFailure {
    /// Map keys and `[i]` array indices from the root to the failing value.
    pub path: Vec<String>,
    /// What is wrong there.
    pub reason: String,
}

impl DecodeFailure {
    /// A failure at the value itself.
    pub fn new(reason: impl Into<String>) -> Self {
        Self { path: Vec::new(), reason: reason.into() }
    }

    /// This failure, one level further down: `segment` is prepended to the path.
    #[must_use]
    pub fn at(mut self, segment: impl Into<String>) -> Self {
        self.path.insert(0, segment.into());
        self
    }
}

impl fmt::Display for DecodeFailure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.path.is_empty() {
            write!(f, "<root>: {}", self.reason)
        } else {
            write!(f, "{}: {}", self.path.join("."), self.reason)
        }
    }
}

impl core::error::Error for DecodeFailure {}
