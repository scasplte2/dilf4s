//! Both directions between typed data and the canonical data model, with the same shape checks: an encoder refuses
//! every value its decoder would reject, so a schema-described type never encodes to bytes outside its schema. This is
//! the runtime of the models generated from `shared/schemas` (see [`crate::generated`]).

use alloc::boxed::Box;
use alloc::collections::BTreeMap;
use alloc::format;
use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;
use core::marker::PhantomData;

pub use num_bigint::BigInt;
use num_bigint::Sign;

pub use crate::error::{CanonicalError, DecodeFailure};
use crate::value::Map;
pub use crate::value::Value;
use crate::{cbor, json};

/// Encodes `Target` to a canonical value and decodes it back.
pub trait Codec {
    /// The typed value.
    type Target;

    /// The canonical value of `value`, or why `value` is outside the schema.
    fn encode(&self, value: &Self::Target) -> Result<Value, CanonicalError>;

    /// The typed value of `value`, or why it does not fit the schema, and where.
    fn decode(&self, value: &Value) -> Result<Self::Target, DecodeFailure>;
}

/// A type generated from a schema rule: a record, an enumeration or a choice.
pub trait Model: Sized {
    /// The canonical value of `self`.
    fn to_value(&self) -> Result<Value, CanonicalError>;

    /// The typed value of `value`.
    fn from_value(value: &Value) -> Result<Self, DecodeFailure>;

    /// The canonical CBOR of `self`.
    fn to_cbor(&self) -> Result<Vec<u8>, CanonicalError> {
        self.to_value().map(|v| cbor::encode(&v))
    }

    /// The typed value of canonical CBOR.
    fn from_cbor(bytes: &[u8]) -> Result<Self, ModelError> {
        let value = cbor::decode(bytes).map_err(ModelError::Canonical)?;
        Self::from_value(&value).map_err(ModelError::Decode)
    }

    /// The typed value of JSON text (canonical-encoding.md section 3).
    fn from_json(text: &str) -> Result<Self, ModelError> {
        let value = json::parse(text).map_err(ModelError::Canonical)?;
        Self::from_value(&value).map_err(ModelError::Decode)
    }
}

/// Why bytes or text are not a model: not canonical, or not the model's shape.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ModelError {
    /// Not a canonical encoding or JSON text.
    Canonical(CanonicalError),
    /// A canonical value of the wrong shape.
    Decode(DecodeFailure),
}

impl fmt::Display for ModelError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Canonical(e) => e.fmt(f),
            Self::Decode(e) => e.fmt(f),
        }
    }
}

impl core::error::Error for ModelError {}

fn mismatch(reason: String) -> CanonicalError {
    CanonicalError::TypeMismatch { reason }
}

fn expected(what: &str, value: &Value) -> DecodeFailure {
    DecodeFailure::new(format!("expected {what}, got {}", value.kind()))
}

/// Any canonical value, unchanged (CDDL `any`).
#[derive(Clone, Copy, Debug)]
pub struct Any;

impl Codec for Any {
    type Target = Value;
    fn encode(&self, value: &Value) -> Result<Value, CanonicalError> {
        Ok(value.clone())
    }
    fn decode(&self, value: &Value) -> Result<Value, DecodeFailure> {
        Ok(value.clone())
    }
}

/// `bool`.
#[derive(Clone, Copy, Debug)]
pub struct Bool;

impl Codec for Bool {
    type Target = bool;
    fn encode(&self, value: &bool) -> Result<Value, CanonicalError> {
        Ok(Value::Bool(*value))
    }
    fn decode(&self, value: &Value) -> Result<bool, DecodeFailure> {
        match value {
            Value::Bool(b) => Ok(*b),
            other => Err(expected("bool", other)),
        }
    }
}

/// A bound of a big-integer range, as a sign and big-endian magnitude so that it can be a constant.
#[derive(Clone, Copy, Debug)]
pub struct BigBound {
    /// Whether the bound is negative.
    pub negative: bool,
    /// Its big-endian magnitude.
    pub magnitude: &'static [u8],
}

impl BigBound {
    fn value(self) -> BigInt {
        BigInt::from_bytes_be(if self.negative { Sign::Minus } else { Sign::Plus }, self.magnitude)
    }
}

/// An integer in `lo..=hi`, either side unbounded when `None` (CDDL `int`, `uint`, `nint` and wide ranges).
#[derive(Clone, Copy, Debug)]
pub struct BigIntIn {
    /// The least value, if bounded.
    pub lo: Option<BigBound>,
    /// The greatest value, if bounded.
    pub hi: Option<BigBound>,
}

/// Any integer (CDDL `int`).
pub const INT: BigIntIn = BigIntIn { lo: None, hi: None };
/// A non-negative integer (CDDL `uint`; like the reference implementation, not bounded by 2^64).
pub const UINT: BigIntIn = BigIntIn { lo: Some(BigBound { negative: false, magnitude: &[] }), hi: None };
/// A negative integer (CDDL `nint`).
pub const NINT: BigIntIn = BigIntIn { lo: None, hi: Some(BigBound { negative: true, magnitude: &[1] }) };

impl BigIntIn {
    fn within(&self, n: &BigInt) -> bool {
        self.lo.is_none_or(|lo| *n >= lo.value()) && self.hi.is_none_or(|hi| *n <= hi.value())
    }

    fn describe(&self) -> String {
        let show = |b: Option<BigBound>| b.map(|b| b.value().to_str_radix(10)).unwrap_or_default();
        format!("{}..{}", show(self.lo), show(self.hi))
    }
}

impl Codec for BigIntIn {
    type Target = BigInt;
    fn encode(&self, value: &BigInt) -> Result<Value, CanonicalError> {
        if self.within(value) {
            Ok(Value::Int(value.clone()))
        } else {
            Err(mismatch(format!("integer {value} outside {}", self.describe())))
        }
    }
    fn decode(&self, value: &Value) -> Result<BigInt, DecodeFailure> {
        match value {
            Value::Int(n) if self.within(n) => Ok(n.clone()),
            Value::Int(n) => Err(DecodeFailure::new(format!("integer {n} outside {}", self.describe()))),
            other => Err(expected("int", other)),
        }
    }
}

macro_rules! native_range {
    ($(#[$doc:meta])* $name:ident, $t:ty) => {
        $(#[$doc])*
        #[derive(Clone, Copy, Debug)]
        pub struct $name {
            /// The least value.
            pub lo: $t,
            /// The greatest value.
            pub hi: $t,
        }

        impl Codec for $name {
            type Target = $t;
            fn encode(&self, value: &$t) -> Result<Value, CanonicalError> {
                if (self.lo..=self.hi).contains(value) {
                    Ok(Value::Int(BigInt::from(*value)))
                } else {
                    Err(mismatch(format!("integer {value} outside {}..{}", self.lo, self.hi)))
                }
            }
            fn decode(&self, value: &Value) -> Result<$t, DecodeFailure> {
                match value {
                    Value::Int(n) => <$t>::try_from(n)
                        .ok()
                        .filter(|v| (self.lo..=self.hi).contains(v))
                        .ok_or_else(|| DecodeFailure::new(format!("integer {n} outside {}..{}", self.lo, self.hi))),
                    other => Err(expected("int", other)),
                }
            }
        }
    };
}

native_range!(
    /// An integer range within `u32`.
    U32In, u32
);
native_range!(
    /// An integer range within `u64`.
    U64In, u64
);
native_range!(
    /// An integer range within `i32`.
    I32In, i32
);
native_range!(
    /// An integer range within `i64`.
    I64In, i64
);

/// A byte string of any length.
#[derive(Clone, Copy, Debug)]
pub struct Bytes;

impl Codec for Bytes {
    type Target = Vec<u8>;
    fn encode(&self, value: &Vec<u8>) -> Result<Value, CanonicalError> {
        Ok(Value::Bytes(value.clone()))
    }
    fn decode(&self, value: &Value) -> Result<Vec<u8>, DecodeFailure> {
        match value {
            Value::Bytes(b) => Ok(b.clone()),
            other => Err(expected("bytes", other)),
        }
    }
}

/// A byte string of `lo` to `hi` bytes (CDDL `bytes .size (lo..hi)`).
#[derive(Clone, Copy, Debug)]
pub struct SizedBytes {
    /// The least length.
    pub lo: usize,
    /// The greatest length, if bounded.
    pub hi: Option<usize>,
}

impl SizedBytes {
    fn fits(&self, n: usize) -> bool {
        n >= self.lo && self.hi.is_none_or(|hi| n <= hi)
    }
}

impl Codec for SizedBytes {
    type Target = Vec<u8>;
    fn encode(&self, value: &Vec<u8>) -> Result<Value, CanonicalError> {
        if self.fits(value.len()) {
            Ok(Value::Bytes(value.clone()))
        } else {
            Err(mismatch(format!("byte string of {} bytes outside the schema size", value.len())))
        }
    }
    fn decode(&self, value: &Value) -> Result<Vec<u8>, DecodeFailure> {
        match value {
            Value::Bytes(b) if self.fits(b.len()) => Ok(b.clone()),
            Value::Bytes(b) => Err(DecodeFailure::new(format!(
                "byte string of {} bytes, expected {}..{}",
                b.len(),
                self.lo,
                show_max(self.hi)
            ))),
            other => Err(expected("bytes", other)),
        }
    }
}

fn show_max(hi: Option<usize>) -> String {
    hi.map(|h| format!("{h}")).unwrap_or_default()
}

/// A byte string of exactly `N` bytes (CDDL `bytes .size N`), held as an array.
#[derive(Clone, Copy, Debug)]
pub struct FixedBytes<const N: usize>;

impl<const N: usize> Codec for FixedBytes<N> {
    type Target = [u8; N];
    fn encode(&self, value: &[u8; N]) -> Result<Value, CanonicalError> {
        Ok(Value::Bytes(value.to_vec()))
    }
    fn decode(&self, value: &Value) -> Result<[u8; N], DecodeFailure> {
        match value {
            Value::Bytes(b) => <[u8; N]>::try_from(b.as_slice())
                .map_err(|_| DecodeFailure::new(format!("byte string of {} bytes, expected {N}", b.len()))),
            other => Err(expected("bytes", other)),
        }
    }
}

/// Text of any length.
#[derive(Clone, Copy, Debug)]
pub struct Text;

impl Codec for Text {
    type Target = String;
    fn encode(&self, value: &String) -> Result<Value, CanonicalError> {
        Ok(Value::Text(value.clone()))
    }
    fn decode(&self, value: &Value) -> Result<String, DecodeFailure> {
        match value {
            Value::Text(t) => Ok(t.clone()),
            other => Err(expected("text", other)),
        }
    }
}

/// Text whose UTF-8 encoding is `lo` to `hi` bytes long (RFC 8610 `.size` on a text string counts bytes).
#[derive(Clone, Copy, Debug)]
pub struct SizedText {
    /// The least length in bytes.
    pub lo: usize,
    /// The greatest length in bytes, if bounded.
    pub hi: Option<usize>,
}

impl Codec for SizedText {
    type Target = String;
    fn encode(&self, value: &String) -> Result<Value, CanonicalError> {
        let n = value.len();
        if n >= self.lo && self.hi.is_none_or(|hi| n <= hi) {
            Ok(Value::Text(value.clone()))
        } else {
            Err(mismatch(format!("text of {n} UTF-8 bytes outside the schema size")))
        }
    }
    fn decode(&self, value: &Value) -> Result<String, DecodeFailure> {
        match value {
            Value::Text(t) if t.len() >= self.lo && self.hi.is_none_or(|hi| t.len() <= hi) => Ok(t.clone()),
            Value::Text(t) => Err(DecodeFailure::new(format!(
                "text of {} UTF-8 bytes, expected {}..{}",
                t.len(),
                self.lo,
                show_max(self.hi)
            ))),
            other => Err(expected("text", other)),
        }
    }
}

/// The `.regexp` patterns the schemas use. A generator accepts only these, so every SDK checks them identically
/// without a regular-expression engine.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TextPattern {
    /// `([0-9a-f]{2})*`: lowercase hex of whole bytes.
    Hex,
    /// `0x([0-9a-f]{2})*`: the same, `0x`-prefixed.
    Hex0x,
    /// `0|[1-9][0-9]*`: a non-negative decimal with no leading zero.
    Decimal,
}

impl TextPattern {
    /// The pattern as written in the schema.
    pub const fn source(self) -> &'static str {
        match self {
            Self::Hex => "([0-9a-f]{2})*",
            Self::Hex0x => "0x([0-9a-f]{2})*",
            Self::Decimal => "0|[1-9][0-9]*",
        }
    }

    /// Whether the whole of `text` matches.
    pub fn matches(self, text: &str) -> bool {
        let lower_hex =
            |s: &str| s.len() % 2 == 0 && s.bytes().all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b));
        match self {
            Self::Hex => lower_hex(text),
            Self::Hex0x => text.strip_prefix("0x").is_some_and(lower_hex),
            Self::Decimal => {
                text == "0" || (!text.starts_with('0') && !text.is_empty() && text.bytes().all(|b| b.is_ascii_digit()))
            }
        }
    }
}

/// Text matching a [`TextPattern`] (CDDL `text .regexp`).
#[derive(Clone, Copy, Debug)]
pub struct Matching(pub TextPattern);

impl Codec for Matching {
    type Target = String;
    fn encode(&self, value: &String) -> Result<Value, CanonicalError> {
        if self.0.matches(value) {
            Ok(Value::Text(value.clone()))
        } else {
            Err(mismatch(format!("text does not match {}", self.0.source())))
        }
    }
    fn decode(&self, value: &Value) -> Result<String, DecodeFailure> {
        match value {
            Value::Text(t) if self.0.matches(t) => Ok(t.clone()),
            Value::Text(t) => Err(DecodeFailure::new(format!("{t:?} does not match {}", self.0.source()))),
            other => Err(expected("text", other)),
        }
    }
}

/// Exactly the text `.0`; the typed value carries nothing, so a record leaves the field out.
#[derive(Clone, Copy, Debug)]
pub struct LiteralText(pub &'static str);

impl Codec for LiteralText {
    type Target = ();
    fn encode(&self, (): &()) -> Result<Value, CanonicalError> {
        Ok(Value::Text(String::from(self.0)))
    }
    fn decode(&self, value: &Value) -> Result<(), DecodeFailure> {
        match value {
            Value::Text(t) if t == self.0 => Ok(()),
            Value::Text(t) => Err(DecodeFailure::new(format!("expected {:?}, got {t:?}", self.0))),
            other => Err(expected("text", other)),
        }
    }
}

/// Exactly the integer `.0`; the typed value carries nothing, so a record leaves the field out.
#[derive(Clone, Copy, Debug)]
pub struct LiteralInt(pub i64);

impl Codec for LiteralInt {
    type Target = ();
    fn encode(&self, (): &()) -> Result<Value, CanonicalError> {
        Ok(Value::Int(BigInt::from(self.0)))
    }
    fn decode(&self, value: &Value) -> Result<(), DecodeFailure> {
        match value {
            Value::Int(n) if *n == BigInt::from(self.0) => Ok(()),
            Value::Int(n) => Err(DecodeFailure::new(format!("expected {}, got {n}", self.0))),
            other => Err(expected("int", other)),
        }
    }
}

/// A homogeneous array of `min` to `max` elements.
#[derive(Clone, Copy, Debug)]
pub struct VecOf<C> {
    /// The element codec.
    pub element: C,
    /// The least number of elements.
    pub min: usize,
    /// The greatest number of elements, if bounded.
    pub max: Option<usize>,
}

impl<C: Codec> VecOf<C> {
    fn fits(&self, n: usize) -> bool {
        n >= self.min && self.max.is_none_or(|m| n <= m)
    }
}

impl<C: Codec> Codec for VecOf<C> {
    type Target = Vec<C::Target>;
    fn encode(&self, value: &Vec<C::Target>) -> Result<Value, CanonicalError> {
        if !self.fits(value.len()) {
            return Err(mismatch(format!("array of {} elements outside the schema size", value.len())));
        }
        let items = value.iter().map(|x| self.element.encode(x)).collect::<Result<Vec<_>, _>>()?;
        Value::array(items)
    }
    fn decode(&self, value: &Value) -> Result<Vec<C::Target>, DecodeFailure> {
        match value {
            Value::Array(a) if self.fits(a.items().len()) => a
                .items()
                .iter()
                .enumerate()
                .map(|(i, v)| self.element.decode(v).map_err(|e| e.at(format!("[{i}]"))))
                .collect(),
            Value::Array(a) => Err(DecodeFailure::new(format!(
                "array of {} elements, expected {}..{}",
                a.items().len(),
                self.min,
                show_max(self.max)
            ))),
            other => Err(expected("array", other)),
        }
    }
}

/// A map with arbitrary text keys and values of one type (CDDL `{ * text => T }`).
#[derive(Clone, Copy, Debug)]
pub struct TextMapOf<C>(pub C);

impl<C: Codec> Codec for TextMapOf<C> {
    type Target = BTreeMap<String, C::Target>;
    fn encode(&self, value: &BTreeMap<String, C::Target>) -> Result<Value, CanonicalError> {
        let entries =
            value.iter().map(|(k, v)| self.0.encode(v).map(|e| (k.clone(), e))).collect::<Result<Vec<_>, _>>()?;
        Value::map(entries)
    }
    fn decode(&self, value: &Value) -> Result<BTreeMap<String, C::Target>, DecodeFailure> {
        match value {
            Value::Map(m) => m
                .entries()
                .iter()
                .map(|(k, v)| self.0.decode(v).map(|d| (k.clone(), d)).map_err(|e| e.at(k.clone())))
                .collect(),
            other => Err(expected("map", other)),
        }
    }
}

/// A generated [`Model`] type.
pub struct ModelOf<T>(PhantomData<fn() -> T>);

impl<T> ModelOf<T> {
    /// The codec of `T`.
    pub const NEW: Self = Self(PhantomData);
}

impl<T> Clone for ModelOf<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for ModelOf<T> {}

impl<T> fmt::Debug for ModelOf<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("ModelOf")
    }
}

impl<T: Model> Codec for ModelOf<T> {
    type Target = T;
    fn encode(&self, value: &T) -> Result<Value, CanonicalError> {
        value.to_value()
    }
    fn decode(&self, value: &Value) -> Result<T, DecodeFailure> {
        T::from_value(value)
    }
}

/// A boxed value, for fields that make a type recursive.
#[derive(Clone, Copy, Debug)]
pub struct Boxed<C>(pub C);

impl<C: Codec> Codec for Boxed<C> {
    type Target = Box<C::Target>;
    fn encode(&self, value: &Box<C::Target>) -> Result<Value, CanonicalError> {
        self.0.encode(value)
    }
    fn decode(&self, value: &Value) -> Result<Box<C::Target>, DecodeFailure> {
        self.0.decode(value).map(Box::new)
    }
}

// ------------------------------------------------------------------------------------------------ records

/// One entry of an encoded record.
pub fn field<C: Codec>(key: &str, codec: &C, value: &C::Target) -> Result<Option<(String, Value)>, CanonicalError> {
    codec.encode(value).map(|v| Some((String::from(key), v)))
}

/// One optional entry: `None` (an absent field) is left out, never written as null.
pub fn optional_field<C: Codec>(
    key: &str,
    codec: &C,
    value: Option<&C::Target>,
) -> Result<Option<(String, Value)>, CanonicalError> {
    value.map_or(Ok(None), |v| field(key, codec, v))
}

/// The map of a record's entries.
pub fn record(entries: impl IntoIterator<Item = Option<(String, Value)>>) -> Result<Value, CanonicalError> {
    Value::map(entries.into_iter().flatten())
}

/// The entries of a record being decoded: a map with no keys beyond those allowed.
pub struct Fields<'a>(&'a Map);

impl<'a> Fields<'a> {
    /// The fields of `value`, which must be a map whose keys are all in `allowed`.
    pub fn of(value: &'a Value, allowed: &[&str]) -> Result<Self, DecodeFailure> {
        match value {
            Value::Map(m) => {
                let mut unknown: Vec<&str> =
                    m.entries().iter().map(|(k, _)| k.as_str()).filter(|k| !allowed.contains(k)).collect();
                if unknown.is_empty() {
                    Ok(Self(m))
                } else {
                    unknown.sort_unstable();
                    Err(DecodeFailure::new(format!("unknown keys {}", unknown.join(", "))))
                }
            }
            other => Err(expected("map", other)),
        }
    }

    /// The value at `key`, which must be present.
    pub fn required<C: Codec>(&self, key: &str, codec: &C) -> Result<C::Target, DecodeFailure> {
        let v = self.0.get(key).ok_or_else(|| DecodeFailure::new(format!("missing required key {key:?}")))?;
        codec.decode(v).map_err(|e| e.at(key))
    }

    /// The value at `key`, if present.
    pub fn optional<C: Codec>(&self, key: &str, codec: &C) -> Result<Option<C::Target>, DecodeFailure> {
        self.0.get(key).map(|v| codec.decode(v).map_err(|e| e.at(key))).transpose()
    }
}

/// Which alternative of a choice a value is.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Alternative {
    /// The record alternative at this index.
    Record(usize),
    /// The text alternative at this index.
    Text(usize),
}

/// The alternative of a choice that `value` fits: a record whose required keys are all present and which allows
/// every key present, or one of the text literals. The generator only accepts choices where at most one record fits.
pub fn select_alternative(
    value: &Value,
    records: &[(&[&str], &[&str])],
    texts: &[&str],
) -> Result<Alternative, DecodeFailure> {
    match value {
        Value::Map(m) => {
            let keys: Vec<&str> = m.entries().iter().map(|(k, _)| k.as_str()).collect();
            records
                .iter()
                .position(|(required, all)| {
                    required.iter().all(|r| keys.contains(r)) && keys.iter().all(|k| all.contains(k))
                })
                .map(Alternative::Record)
                .ok_or_else(|| {
                    let mut sorted = keys.clone();
                    sorted.sort_unstable();
                    DecodeFailure::new(format!("no alternative has keys {}", sorted.join(", ")))
                })
        }
        Value::Text(t) => texts
            .iter()
            .position(|x| x == t)
            .map(Alternative::Text)
            .ok_or_else(|| DecodeFailure::new(format!("{t:?} is not an alternative"))),
        _ => Err(DecodeFailure::new("expected a map or one of the text alternatives")),
    }
}

/// The error for an alternative index a generated decoder does not know; unreachable when generated code is current.
pub fn unknown_alternative(alternative: Alternative) -> DecodeFailure {
    DecodeFailure::new(format!("unknown alternative {alternative:?}"))
}
