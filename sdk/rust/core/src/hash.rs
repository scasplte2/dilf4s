//! Hash functions, domain tags and tagged hashing (docs/specs/canonical-encoding.md section 4).
//!
//! Each hash function is a type implementing [`Hasher`], and a [`Digest<H>`] carries its function in its type, so
//! digests of different functions never compare or mix. [`HashAlgorithm`] selects one at run time by name.

use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;
use core::marker::PhantomData;

use sha2::Digest as _;

use crate::cbor;
use crate::value::Value;

/// A hash function with a fixed output.
pub trait Hasher {
    /// The name the specs and vectors use (`"SHA-256"`, `"BLAKE2b-256"`, ...).
    const NAME: &'static str;
    /// The output size in bytes.
    const OUTPUT_SIZE: usize;
    /// The output, as a fixed-size byte array.
    type Output: AsRef<[u8]> + Clone + fmt::Debug + PartialEq + Eq;

    /// The hash of the concatenation of `parts`.
    fn hash_parts(parts: &[&[u8]]) -> Self::Output;

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

            fn hash_parts(parts: &[&[u8]]) -> Self::Output {
                let mut h = <$imp>::new();
                for p in parts {
                    h.update(p);
                }
                h.finalize().into()
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
        H::output_from_slice(bytes).map(Self::from_output).ok_or(HashError::InvalidLength {
            algorithm: H::NAME,
            expected: H::OUTPUT_SIZE,
            actual: bytes.len(),
        })
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

/// Errors of hashing inputs: digest lengths and domain tags.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HashError {
    /// Bytes of the wrong length for a digest.
    InvalidLength {
        /// The hash function.
        algorithm: &'static str,
        /// Its output size.
        expected: usize,
        /// The length given.
        actual: usize,
    },
    /// A tag outside the `DomainTag` grammar.
    InvalidDomainTag {
        /// The tag given.
        tag: String,
    },
    /// A hash function name that is not one of [`HashAlgorithm`]'s.
    UnknownAlgorithm {
        /// The name given.
        name: String,
    },
}

impl fmt::Display for HashError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidLength { algorithm, expected, actual } => {
                write!(f, "{algorithm} digest of {actual} bytes, expected {expected}")
            }
            Self::InvalidDomainTag { tag } => write!(f, "invalid domain tag {tag:?}"),
            Self::UnknownAlgorithm { name } => write!(f, "unknown hash function {name:?}"),
        }
    }
}

impl core::error::Error for HashError {}

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
            Err(HashError::InvalidDomainTag { tag: String::from(tag) })
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

    /// The tagged hash of `message`.
    pub fn hash(&self, message: &[u8]) -> Digest<H> {
        let t = self.tag_hash.as_ref();
        Digest::from_output(H::hash_parts(&[t, t, message]))
    }

    /// The content hash of a canonical value: the tagged hash of its canonical encoding.
    pub fn content(&self, value: &Value) -> Digest<H> {
        self.hash(&cbor::encode(value))
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
            .ok_or_else(|| HashError::UnknownAlgorithm { name: String::from(name) })
    }

    /// The output size in bytes.
    pub const fn output_size(self) -> usize {
        match self {
            Self::Sha256 | Self::Sha3_256 | Self::Keccak256 | Self::Blake2b256 => 32,
            Self::Sha512 | Self::Sha3_512 | Self::Blake2b512 => 64,
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

    /// The content hash of a canonical value with this algorithm.
    pub fn content_hash(self, tag: &DomainTag, value: &Value) -> Vec<u8> {
        self.tagged_hash(tag, &cbor::encode(value))
    }
}
