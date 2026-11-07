//! Hash functions, domain tags and tagged hashing (docs/specs/canonical-encoding.md section 4).
//!
//! Each hash function is a type implementing [`Hasher`], and a [`Digest<H>`] carries its function in its type, so
//! digests of different functions never compare or mix. [`HashAlgorithm`] selects one at run time by name.

use alloc::string::String;
use alloc::vec;
use alloc::vec::Vec;
use core::fmt;
use core::marker::PhantomData;

use sha2::Digest as _;

use crate::cbor;
use crate::preview::Preview;
use crate::value::Value;

/// A hash refusal, by kind (docs/specs/conformance.md section 6): generated from `shared/schemas/hash-errors.cddl`, as
/// the reference's and the TypeScript SDK's are. Caller text is held as a [`Preview`], never as itself.
pub use crate::generated::hash_errors::{ExpansionRefusal, HashError, HashErrorKind};

/// A size as an error holds it.
fn size(n: usize) -> u64 {
    u64::try_from(n).unwrap_or(u64::MAX)
}

/// A hash function with a fixed output.
pub trait Hasher {
    /// The name the specs and vectors use (`"SHA-256"`, `"BLAKE2b-256"`, ...).
    const NAME: &'static str;
    /// The output size in bytes.
    const OUTPUT_SIZE: usize;
    /// The output, as a fixed-size byte array.
    type Output: AsRef<[u8]> + Clone + fmt::Debug + PartialEq + Eq;
    /// The running state of an incremental hash: the chaining value and at most one block of input.
    type State: Clone;

    /// A fresh state, with nothing absorbed.
    fn start() -> Self::State;

    /// Absorbs `data`.
    fn absorb(state: &mut Self::State, data: &[u8]);

    /// The hash of everything absorbed.
    fn finish(state: Self::State) -> Self::Output;

    /// The hash of the concatenation of `parts`.
    fn hash_parts(parts: &[&[u8]]) -> Self::Output {
        let mut state = Self::start();
        for p in parts {
            Self::absorb(&mut state, p);
        }
        Self::finish(state)
    }

    /// An output from its bytes, if the length is right.
    fn output_from_slice(bytes: &[u8]) -> Option<Self::Output>;
}

macro_rules! hasher {
    ($(#[$doc:meta])* $name:ident, $label:literal, $size:literal, $imp:ty) => {
        $(#[$doc])*
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum $name {}

        impl Hasher for $name {
            const NAME: &'static str = $label;
            const OUTPUT_SIZE: usize = $size;
            type Output = [u8; $size];
            type State = $imp;

            fn start() -> Self::State {
                <$imp>::new()
            }

            fn absorb(state: &mut Self::State, data: &[u8]) {
                state.update(data);
            }

            fn finish(state: Self::State) -> Self::Output {
                state.finalize().into()
            }

            fn output_from_slice(bytes: &[u8]) -> Option<Self::Output> {
                <[u8; $size]>::try_from(bytes).ok()
            }
        }
    };
}

hasher!(
    /// SHA-256 (FIPS 180-4).
    Sha256, "SHA-256", 32, sha2::Sha256
);
hasher!(
    /// SHA-512 (FIPS 180-4).
    Sha512, "SHA-512", 64, sha2::Sha512
);
hasher!(
    /// SHA3-256 (FIPS 202).
    Sha3_256, "SHA3-256", 32, sha3::Sha3_256
);
hasher!(
    /// SHA3-512 (FIPS 202).
    Sha3_512, "SHA3-512", 64, sha3::Sha3_512
);
hasher!(
    /// Keccak-256 as Ethereum uses it: the pre-standard padding, not SHA3-256's.
    Keccak256, "Keccak-256", 32, sha3::Keccak256
);
hasher!(
    /// `BLAKE2b` with a 32-byte output parameter (RFC 7693), not a truncated BLAKE2b-512.
    Blake2b256, "BLAKE2b-256", 32, blake2::Blake2b<blake2::digest::consts::U32>
);
hasher!(
    /// `BLAKE2b-512` (RFC 7693).
    Blake2b512, "BLAKE2b-512", 64, blake2::Blake2b512
);

/// The bytes of a digest of `width` bytes from its hex (canonical-encoding.md section 4): an optional `0x`, then exactly
/// `2 * width` ASCII hex digits, either case. Digits past that width are refused as `InvalidLength` before the text is
/// decoded, reading at most one digit past it (conformance.md section 8): a byte that is not a hex digit within that
/// reach is `InvalidHex` at its offset, the same offset in every language's string units.
fn digest_bytes(algorithm: &str, width: usize, text: &str) -> Result<Vec<u8>, HashError> {
    let (from, digits): (usize, &str) = text.strip_prefix("0x").map_or((0, text), |d| (2, d));
    let at = |offset: usize| HashError::InvalidHex { offset: size(from.saturating_add(offset)) };
    let length = |actual: usize| HashError::InvalidLength {
        algorithm: algorithm.into(),
        expected: size(width),
        actual: size(actual),
    };
    let span = width.saturating_mul(2);
    if digits.len() > span {
        let reach = digits.as_bytes().get(..span.saturating_add(1)).unwrap_or(digits.as_bytes());
        return Err(reach
            .iter()
            .position(|b| !b.is_ascii_hexdigit())
            .map_or_else(|| length(digits.len().div_ceil(2)), at));
    }
    let bytes = crate::hex::decode(digits).map_err(|e| match e {
        HashError::InvalidHex { offset } => HashError::InvalidHex { offset: offset.saturating_add(size(from)) },
        other => other,
    })?;
    if bytes.len() == width { Ok(bytes) } else { Err(length(bytes.len())) }
}

/// RFC 9380 section 5.3.1 `expand_message_xmd` under `H`, whose input block is `block` bytes. A DST over 255 bytes is
/// first reduced to `H("H2C-OVERSIZE-DST-" || DST)` (section 5.3.3). Refused before anything is hashed, in this order:
/// an empty DST (`EmptyDst`), and a length outside `1..=`[`HashAlgorithm::expansion_limit`] (`OutputLength`).
fn xmd<H: Hasher>(block: usize, message: &[&[u8]], dst: &[u8], length: usize) -> Result<Vec<u8>, HashError> {
    let limit = H::OUTPUT_SIZE.saturating_mul(255).min(65_535);
    let refused = |refusal| HashError::InvalidExpansion { refusal, algorithm: H::NAME.into(), limit: size(limit) };
    if dst.is_empty() {
        return Err(refused(ExpansionRefusal::EmptyDst));
    }
    if length == 0 || length > limit {
        return Err(refused(ExpansionRefusal::OutputLength));
    }
    let oversize = (dst.len() > 255).then(|| H::hash_parts(&[b"H2C-OVERSIZE-DST-", dst]));
    let dst = oversize.as_ref().map_or(dst, AsRef::as_ref);
    // both fit: a DST is now at most 255 bytes, and a length at most the limit
    let suffix = [dst, &[u8::try_from(dst.len()).unwrap_or(u8::MAX)]].concat();
    let [high, low] = u16::try_from(length).unwrap_or(u16::MAX).to_be_bytes();
    let zeros = vec![0; block];
    let tail = [high, low, 0];
    let parts: Vec<&[u8]> =
        core::iter::once(zeros.as_slice()).chain(message.iter().copied()).chain([&tail[..], &suffix]).collect();
    let b0 = H::hash_parts(&parts);
    let b1 = H::hash_parts(&[b0.as_ref(), &[1], &suffix]);
    let ell = length.div_ceil(H::OUTPUT_SIZE);
    let (_, mut out) = (2..=ell).fold((b1.clone(), b1.as_ref().to_vec()), |(prev, mut out), i| {
        let mixed: Vec<u8> = b0.as_ref().iter().zip(prev.as_ref()).map(|(a, b)| a ^ b).collect();
        let bi = H::hash_parts(&[&mixed, &[u8::try_from(i).unwrap_or(u8::MAX)], &suffix]);
        out.extend_from_slice(bi.as_ref());
        (bi, out)
    });
    out.truncate(length);
    Ok(out)
}

/// A digest produced by `H`.
pub struct Digest<H: Hasher> {
    bytes: H::Output,
    hasher: PhantomData<H>,
}

impl<H: Hasher> Digest<H> {
    /// The digest of `data`.
    pub fn of(data: &[u8]) -> Self {
        Self::from_output(H::hash_parts(&[data]))
    }

    /// A digest from its output array.
    pub const fn from_output(bytes: H::Output) -> Self {
        Self { bytes, hasher: PhantomData }
    }

    /// A digest from its bytes, which must be exactly the output size.
    pub fn from_slice(bytes: &[u8]) -> Result<Self, HashError> {
        H::output_from_slice(bytes).map(Self::from_output).ok_or_else(|| HashError::InvalidLength {
            algorithm: H::NAME.into(),
            expected: size(H::OUTPUT_SIZE),
            actual: size(bytes.len()),
        })
    }

    /// A digest from its hex: an optional `0x`, then exactly twice the output size in hex digits, either case
    /// ([`HashAlgorithm::digest_from_hex`]).
    pub fn from_hex(text: &str) -> Result<Self, HashError> {
        digest_bytes(H::NAME, H::OUTPUT_SIZE, text).and_then(|b| Self::from_slice(&b))
    }

    /// The digest bytes.
    pub fn as_bytes(&self) -> &[u8] {
        self.bytes.as_ref()
    }

    /// The digest bytes as an array.
    pub const fn output(&self) -> &H::Output {
        &self.bytes
    }
}

impl<H: Hasher> Clone for Digest<H> {
    fn clone(&self) -> Self {
        Self::from_output(self.bytes.clone())
    }
}

impl<H: Hasher> PartialEq for Digest<H> {
    fn eq(&self, other: &Self) -> bool {
        self.bytes == other.bytes
    }
}

impl<H: Hasher> Eq for Digest<H> {}

impl<H: Hasher> fmt::Debug for Digest<H> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Digest<{}>({})", H::NAME, crate::hex::encode(self.as_bytes()))
    }
}

/// An incremental hash under `H`: absorb bytes as they come, then [`Hashing::finish`]. It holds `H`'s state only (the
/// chaining value and at most one block), so an encoding streamed into it takes no memory proportional to its length.
pub struct Hashing<H: Hasher> {
    state: H::State,
}

impl<H: Hasher> Clone for Hashing<H> {
    fn clone(&self) -> Self {
        Self { state: self.state.clone() }
    }
}

impl<H: Hasher> Default for Hashing<H> {
    fn default() -> Self {
        Self::new()
    }
}

impl<H: Hasher> fmt::Debug for Hashing<H> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Hashing<{}>", H::NAME)
    }
}

impl<H: Hasher> Hashing<H> {
    /// A hash with nothing absorbed.
    pub fn new() -> Self {
        Self { state: H::start() }
    }

    /// Absorbs `data`.
    pub fn update(&mut self, data: &[u8]) {
        H::absorb(&mut self.state, data);
    }

    /// Absorbs the canonical encoding of `value`, streamed: [`cbor::encode_into`].
    pub fn update_value(&mut self, value: &Value) {
        cbor::encode_into(self, value);
    }

    /// The digest of everything absorbed.
    pub fn finish(self) -> Digest<H> {
        Digest::from_output(H::finish(self.state))
    }
}

/// A versioned domain-separation tag matching `^[a-z0-9]+(/[a-z0-9][a-z0-9._-]*)*/v[0-9]+$`, such as
/// `dilf4s/mpt/leaf/v1`.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DomainTag(String);

impl DomainTag {
    /// The tag, if it matches the grammar.
    pub fn new(tag: &str) -> Result<Self, HashError> {
        if is_domain_tag(tag) {
            Ok(Self(String::from(tag)))
        } else {
            Err(HashError::InvalidDomainTag { tag: Preview::of(tag) })
        }
    }

    /// The tag text.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// The grammar, segment by segment: a first segment of `[a-z0-9]+`, middle segments of `[a-z0-9][a-z0-9._-]*`, and a
/// last segment `v[0-9]+`; at least two segments.
fn is_domain_tag(tag: &str) -> bool {
    let lower_digit = |b: u8| b.is_ascii_lowercase() || b.is_ascii_digit();
    let segments: Vec<&[u8]> = tag.as_bytes().split(|&b| b == b'/').collect();
    let (Some((last, init)), true) = (segments.split_last(), segments.len() >= 2) else {
        return false;
    };
    let Some((first, middle)) = init.split_first() else {
        return false;
    };
    let first_ok = !first.is_empty() && first.iter().all(|&b| lower_digit(b));
    let middle_ok = middle.iter().all(|s| {
        s.split_first().is_some_and(|(&h, t)| {
            lower_digit(h) && t.iter().all(|&b| lower_digit(b) || matches!(b, b'.' | b'_' | b'-'))
        })
    });
    let last_ok = last
        .split_first()
        .is_some_and(|(&v, digits)| v == b'v' && !digits.is_empty() && digits.iter().all(u8::is_ascii_digit));
    first_ok && middle_ok && last_ok
}

/// `taggedHash_H(tag, msg) = H(H(tag) || H(tag) || msg)`, the BIP-340 construction over any `H`. Building one hashes
/// the tag once.
pub struct TaggedHash<H: Hasher> {
    tag: DomainTag,
    tag_hash: H::Output,
}

impl<H: Hasher> TaggedHash<H> {
    /// The tagged hash for `tag`.
    pub fn new(tag: DomainTag) -> Self {
        let tag_hash = H::hash_parts(&[tag.as_str().as_bytes()]);
        Self { tag, tag_hash }
    }

    /// The tag.
    pub const fn tag(&self) -> &DomainTag {
        &self.tag
    }

    /// A hash with the prefix `H(tag) || H(tag)` absorbed: absorbing a message, then finishing, gives its tagged hash.
    pub fn start(&self) -> Hashing<H> {
        let t = self.tag_hash.as_ref();
        let mut h = Hashing::new();
        h.update(t);
        h.update(t);
        h
    }

    /// The tagged hash of `message`.
    pub fn hash(&self, message: &[u8]) -> Digest<H> {
        let mut h = self.start();
        h.update(message);
        h.finish()
    }

    /// The content hash of a canonical value: the tagged hash of its canonical encoding, streamed into the hash.
    /// O([`Value::weight`]) time, as [`cbor::encode`], and no memory proportional to the encoding.
    pub fn content(&self, value: &Value) -> Digest<H> {
        let mut h = self.start();
        h.update_value(value);
        h.finish()
    }
}

/// A hash function chosen at run time.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HashAlgorithm {
    /// [`Sha256`].
    Sha256,
    /// [`Sha512`].
    Sha512,
    /// [`Sha3_256`].
    Sha3_256,
    /// [`Sha3_512`].
    Sha3_512,
    /// [`Keccak256`].
    Keccak256,
    /// [`Blake2b256`].
    Blake2b256,
    /// [`Blake2b512`].
    Blake2b512,
}

impl HashAlgorithm {
    /// Every algorithm.
    pub const ALL: [Self; 7] = [
        Self::Sha256,
        Self::Sha512,
        Self::Sha3_256,
        Self::Sha3_512,
        Self::Keccak256,
        Self::Blake2b256,
        Self::Blake2b512,
    ];

    /// The name the specs and vectors use.
    pub const fn name(self) -> &'static str {
        match self {
            Self::Sha256 => Sha256::NAME,
            Self::Sha512 => Sha512::NAME,
            Self::Sha3_256 => Sha3_256::NAME,
            Self::Sha3_512 => Sha3_512::NAME,
            Self::Keccak256 => Keccak256::NAME,
            Self::Blake2b256 => Blake2b256::NAME,
            Self::Blake2b512 => Blake2b512::NAME,
        }
    }

    /// The algorithm with this name.
    pub fn from_name(name: &str) -> Result<Self, HashError> {
        Self::ALL
            .into_iter()
            .find(|a| a.name() == name)
            .ok_or_else(|| HashError::UnknownAlgorithm { name: Preview::of(name) })
    }

    /// The output size in bytes.
    pub const fn output_size(self) -> usize {
        match self {
            Self::Sha256 | Self::Sha3_256 | Self::Keccak256 | Self::Blake2b256 => 32,
            Self::Sha512 | Self::Sha3_512 | Self::Blake2b512 => 64,
        }
    }

    /// The input block size in bytes (a sponge's rate): `expand_message_xmd`'s `s_in_bytes`.
    pub const fn block_size(self) -> usize {
        match self {
            Self::Sha256 => 64,
            Self::Sha3_512 => 72,
            Self::Sha512 | Self::Blake2b256 | Self::Blake2b512 => 128,
            Self::Sha3_256 | Self::Keccak256 => 136,
        }
    }

    /// The most bytes [`HashAlgorithm::expand_message_xmd`] expands to: `min(255 * output size, 65535)`.
    pub const fn expansion_limit(self) -> usize {
        let limit = self.output_size().saturating_mul(255);
        if limit < 65_535 { limit } else { 65_535 }
    }

    /// A digest's bytes from its hex: an optional `0x`, then exactly twice the output size in ASCII hex digits, either
    /// case. `InvalidHex` at the first character that is not a hex digit (or, for an odd number of digits, the text's
    /// length), `InvalidLength` for any other number of digits; digits past the width are refused reading at most one
    /// past it, before the text is decoded.
    pub fn digest_from_hex(self, text: &str) -> Result<Vec<u8>, HashError> {
        digest_bytes(self.name(), self.output_size(), text)
    }

    /// RFC 9380 section 5.3.1 `expand_message_xmd`: `length` uniform bytes from the message, the concatenation of
    /// `message` (as [`HashAlgorithm::hash_parts`] takes it, never copied whole), under the domain separation tag `dst`.
    /// A DST over 255 bytes is first reduced to `H("H2C-OVERSIZE-DST-" || DST)` (section 5.3.3). Refused as
    /// `InvalidExpansion`, before anything is hashed, for an empty DST (`EmptyDst`), then for a length outside
    /// `1..=`[`HashAlgorithm::expansion_limit`] (`OutputLength`).
    pub fn expand_message_xmd(self, message: &[&[u8]], dst: &[u8], length: usize) -> Result<Vec<u8>, HashError> {
        let block = self.block_size();
        match self {
            Self::Sha256 => xmd::<Sha256>(block, message, dst, length),
            Self::Sha512 => xmd::<Sha512>(block, message, dst, length),
            Self::Sha3_256 => xmd::<Sha3_256>(block, message, dst, length),
            Self::Sha3_512 => xmd::<Sha3_512>(block, message, dst, length),
            Self::Keccak256 => xmd::<Keccak256>(block, message, dst, length),
            Self::Blake2b256 => xmd::<Blake2b256>(block, message, dst, length),
            Self::Blake2b512 => xmd::<Blake2b512>(block, message, dst, length),
        }
    }

    /// The hash of the concatenation of `parts`.
    pub fn hash_parts(self, parts: &[&[u8]]) -> Vec<u8> {
        fn run<H: Hasher>(parts: &[&[u8]]) -> Vec<u8> {
            H::hash_parts(parts).as_ref().to_vec()
        }
        match self {
            Self::Sha256 => run::<Sha256>(parts),
            Self::Sha512 => run::<Sha512>(parts),
            Self::Sha3_256 => run::<Sha3_256>(parts),
            Self::Sha3_512 => run::<Sha3_512>(parts),
            Self::Keccak256 => run::<Keccak256>(parts),
            Self::Blake2b256 => run::<Blake2b256>(parts),
            Self::Blake2b512 => run::<Blake2b512>(parts),
        }
    }

    /// The hash of `data`.
    pub fn digest(self, data: &[u8]) -> Vec<u8> {
        self.hash_parts(&[data])
    }

    /// `taggedHash(tag, message)` with this algorithm.
    pub fn tagged_hash(self, tag: &DomainTag, message: &[u8]) -> Vec<u8> {
        let t = self.digest(tag.as_str().as_bytes());
        self.hash_parts(&[&t, &t, message])
    }

    /// The content hash of a canonical value with this algorithm, as [`TaggedHash::content`]: O([`Value::weight`])
    /// time, as [`cbor::encode`], and no memory proportional to the encoding.
    pub fn content_hash(self, tag: &DomainTag, value: &Value) -> Vec<u8> {
        fn run<H: Hasher>(tag: &DomainTag, value: &Value) -> Vec<u8> {
            let t = H::hash_parts(&[tag.as_str().as_bytes()]);
            let mut h = Hashing::<H>::new();
            h.update(t.as_ref());
            h.update(t.as_ref());
            h.update_value(value);
            h.finish().as_bytes().to_vec()
        }
        match self {
            Self::Sha256 => run::<Sha256>(tag, value),
            Self::Sha512 => run::<Sha512>(tag, value),
            Self::Sha3_256 => run::<Sha3_256>(tag, value),
            Self::Sha3_512 => run::<Sha3_512>(tag, value),
            Self::Keccak256 => run::<Keccak256>(tag, value),
            Self::Blake2b256 => run::<Blake2b256>(tag, value),
            Self::Blake2b512 => run::<Blake2b512>(tag, value),
        }
    }
}
