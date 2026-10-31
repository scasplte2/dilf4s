//! Bit strings, most significant bit first (docs/specs/trie.md §1), and their key order.

use alloc::vec::Vec;
use core::cmp::Ordering;
use core::fmt;

/// A bit string: byte 0 bit 7, byte 0 bit 6, and so on. The bits sit in whole bytes, padded with zero bits.
#[derive(Clone, Default, PartialEq, Eq, Hash)]
pub struct Bits {
    bytes: Vec<u8>,
    len: u64,
}

/// The mask of bit `i` within its byte.
fn mask(i: u64) -> u8 {
    let shift = u32::try_from(i % 8).unwrap_or(0);
    0x80u8.checked_shr(shift).unwrap_or(0)
}

fn byte_index(i: u64) -> Option<usize> {
    usize::try_from(i / 8).ok()
}

/// A length in bytes as bits.
fn bits_of(len: usize) -> u64 {
    u64::try_from(len).unwrap_or(u64::MAX).saturating_mul(8)
}

impl Bits {
    /// The empty bit string.
    pub const fn new() -> Self {
        Self { bytes: Vec::new(), len: 0 }
    }

    /// The bits of `bytes`: `8 * len` bits.
    pub fn from_bytes(bytes: &[u8]) -> Self {
        Self { bytes: bytes.to_vec(), len: bits_of(bytes.len()) }
    }

    /// The first `n` bits of `bytes`, if `bytes` is exactly `ceil(n / 8)` long and its padding bits are zero.
    pub fn from_padded(n: u64, bytes: Vec<u8>) -> Option<Self> {
        let expected = n.div_ceil(8);
        if u64::try_from(bytes.len()).ok()? != expected {
            return None;
        }
        let pad = n % 8;
        let last_ok = match (pad, bytes.last()) {
            (0, _) | (_, None) => true,
            (p, Some(&last)) => {
                // the low 8 - p bits of the last byte are padding
                let keep = u32::try_from(p).ok()?;
                let padding_mask = 0xffu8.checked_shr(keep).unwrap_or(0);
                last & padding_mask == 0
            }
        };
        last_ok.then_some(Self { bytes, len: n })
    }

    /// The number of bits.
    pub const fn len(&self) -> u64 {
        self.len
    }

    /// Whether there are no bits.
    pub const fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// The bits in whole bytes, padded with zero bits.
    pub fn padded_bytes(&self) -> &[u8] {
        &self.bytes
    }

    /// The padded bytes, by value. For a whole number of bytes these are exactly the bits.
    pub fn into_padded_bytes(self) -> Vec<u8> {
        self.bytes
    }

    /// Bit `i`, if `i < len`.
    pub fn get(&self, i: u64) -> Option<bool> {
        if i >= self.len {
            return None;
        }
        self.bytes.get(byte_index(i)?).map(|b| b & mask(i) != 0)
    }

    /// Appends one bit.
    pub fn push(&mut self, bit: bool) {
        if self.len % 8 == 0 {
            self.bytes.push(0);
        }
        if bit {
            let m = mask(self.len);
            if let Some(b) = self.bytes.last_mut() {
                *b |= m;
            }
        }
        self.len = self.len.saturating_add(1);
    }

    /// Keeps the first `len` bits (no change if `len >= self.len()`).
    pub fn truncate(&mut self, len: u64) {
        if len >= self.len {
            return;
        }
        let keep = usize::try_from(len.div_ceil(8)).unwrap_or(usize::MAX);
        self.bytes.truncate(keep);
        if len % 8 != 0 {
            let used = u32::try_from(len % 8).unwrap_or(0);
            let padding = 0xffu8.checked_shr(used).unwrap_or(0);
            if let Some(b) = self.bytes.last_mut() {
                *b &= !padding;
            }
        }
        self.len = len;
    }

    /// Appends every bit of `other`.
    pub fn extend(&mut self, other: &Self) {
        if self.len % 8 == 0 {
            self.bytes.extend_from_slice(&other.bytes);
            self.len = self.len.saturating_add(other.len);
        } else {
            for i in 0..other.len {
                self.push(other.get(i).unwrap_or(false));
            }
        }
    }

    /// `self ++ other`.
    #[must_use]
    pub fn concat(&self, other: &Self) -> Self {
        let mut out = self.clone();
        out.extend(other);
        out
    }

    /// `self ++ [bit]`.
    #[must_use]
    pub fn with_bit(&self, bit: bool) -> Self {
        let mut out = self.clone();
        out.push(bit);
        out
    }

    /// The bits `start..end`, clamped to the string.
    #[must_use]
    pub fn slice(&self, start: u64, end: u64) -> Self {
        let end = end.min(self.len);
        let mut out = Self::new();
        let mut i = start;
        while i < end {
            out.push(self.get(i).unwrap_or(false));
            i = i.saturating_add(1);
        }
        out
    }

    /// Whether `self[offset .. offset + len]` equals `other[other_offset .. other_offset + len]`, both in range.
    pub fn range_eq(&self, offset: u64, other: &Self, other_offset: u64, len: u64) -> bool {
        let (Some(a_end), Some(b_end)) = (offset.checked_add(len), other_offset.checked_add(len)) else {
            return false;
        };
        if a_end > self.len || b_end > other.len {
            return false;
        }
        (0..len).all(|k| self.get(offset.saturating_add(k)) == other.get(other_offset.saturating_add(k)))
    }

    /// Whether `prefix` is a prefix of `self`.
    pub fn starts_with(&self, prefix: &Self) -> bool {
        self.range_eq(0, prefix, 0, prefix.len)
    }

    /// The length of the longest common prefix of `self[a_off..]` and `other[b_off..]`.
    pub fn common_prefix_from(&self, a_off: u64, other: &Self, b_off: u64) -> u64 {
        let mut count = 0u64;
        loop {
            let (Some(at_self), Some(at_other)) = (a_off.checked_add(count), b_off.checked_add(count)) else {
                return count;
            };
            match (self.get(at_self), other.get(at_other)) {
                (Some(mine), Some(theirs)) if mine == theirs => count = count.saturating_add(1),
                _ => return count,
            }
        }
    }

    /// Bitwise lexicographic order, a proper prefix before its extensions (docs/specs/trie.md §4).
    pub fn key_cmp(&self, other: &Self) -> Ordering {
        let c = self.common_prefix_from(0, other, 0);
        match (self.get(c), other.get(c)) {
            (None, None) => Ordering::Equal,
            (None, Some(_)) => Ordering::Less,
            (Some(_), None) => Ordering::Greater,
            (Some(a), Some(b)) => a.cmp(&b),
        }
    }

    /// Whether `self < other` in key order.
    pub fn key_lt(&self, other: &Self) -> bool {
        self.key_cmp(other) == Ordering::Less
    }
}

impl fmt::Debug for Bits {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Bits(")?;
        for i in 0..self.len {
            f.write_str(if self.get(i) == Some(true) { "1" } else { "0" })?;
        }
        f.write_str(")")
    }
}

/// Does the subtree of keys starting with `prefix` meet `[from, until)`?
pub(crate) fn intersects(prefix: &Bits, from: &Bits, until: Option<&Bits>) -> bool {
    let wholly_below = !from.starts_with(prefix) && prefix.key_lt(from);
    let wholly_above = until.is_some_and(|u| u.key_cmp(prefix) != Ordering::Greater);
    !wholly_below && !wholly_above
}

/// `from <= key < until`.
pub(crate) fn within(key: &Bits, from: &Bits, until: Option<&Bits>) -> bool {
    from.key_cmp(key) != Ordering::Greater && until.is_none_or(|u| key.key_lt(u))
}
