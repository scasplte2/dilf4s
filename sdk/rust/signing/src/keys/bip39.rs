//! BIP-39 (`signing.md` section 2.1): entropy to mnemonic, mnemonic to entropy, and mnemonic to seed.
//!
//! The English list is embedded; other languages are supplied as a validated [`WordList`].

use alloc::collections::BTreeMap;
use alloc::format;
use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;

use unicode_normalization::UnicodeNormalization as _;
use zeroize::{Zeroize, ZeroizeOnDrop, Zeroizing};

use crate::error::KeyError;
use crate::secret::{Secret, SecretBytes};
use crate::util::sha256;

const ENGLISH: &str = include_str!("bip39-english.txt");

/// The entropy lengths BIP-39 allows, in bytes.
pub const ENTROPY_LENGTHS: [usize; 5] = [16, 20, 24, 28, 32];
/// The word counts BIP-39 allows.
pub const WORD_COUNTS: [usize; 5] = [12, 15, 18, 21, 24];

fn nfkd(s: &str) -> String {
    s.nfkd().collect()
}

/// A validated BIP-39 word list: exactly 2048 distinct words (NFKD-normalised), and the separator its phrases use.
#[derive(Clone, PartialEq, Eq)]
pub struct WordList {
    words: Vec<String>,
    index: BTreeMap<String, u16>,
    separator: char,
}

impl fmt::Debug for WordList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "WordList({} words, separator {:?})", self.words.len(), self.separator)
    }
}

impl WordList {
    /// A list from its words, which are NFKD-normalised; anything but 2048 distinct words is `InvalidWordList`.
    /// `separator` joins phrases (U+3000 for Japanese, a space otherwise).
    pub fn new(words: Vec<String>, separator: char) -> Result<Self, KeyError> {
        let words: Vec<String> = words.into_iter().map(|w| nfkd(&w)).collect();
        let index: BTreeMap<String, u16> = words.iter().zip(0u16..).map(|(w, i)| (w.clone(), i)).collect();
        if words.len() == 2048 && index.len() == 2048 && !words.iter().any(String::is_empty) {
            Ok(Self { words, index, separator })
        } else {
            Err(KeyError::InvalidWordList {
                reason: format!("expected 2048 distinct words, got {} ({} distinct)", words.len(), index.len()),
            })
        }
    }

    /// A list from text with one word per line (as the BIP-39 repository's files); blank lines are skipped.
    pub fn from_text(text: &str, separator: char) -> Result<Self, KeyError> {
        Self::new(
            text.lines()
                .map(|l| l.trim_matches(|c: char| c <= ' '))
                .filter(|l| !l.is_empty())
                .map(String::from)
                .collect(),
            separator,
        )
    }

    /// The embedded English list.
    pub fn english() -> Self {
        let words: Vec<String> = ENGLISH.lines().filter(|l| !l.is_empty()).map(String::from).collect();
        let index = words.iter().zip(0u16..).map(|(w, i)| (w.clone(), i)).collect();
        Self { words, index, separator: ' ' }
    }

    /// The word at `index` (below 2048).
    pub fn word(&self, index: u16) -> Option<&str> {
        self.words.get(usize::from(index)).map(String::as_str)
    }

    /// The index of an NFKD-normalised word.
    pub fn index_of(&self, word: &str) -> Option<u16> {
        self.index.get(word).copied()
    }

    /// The phrase separator.
    pub const fn separator(&self) -> char {
        self.separator
    }
}

/// A mnemonic phrase: its NFKD-normalised words (secret: never printed, zeroized on drop).
#[derive(Clone, Zeroize, ZeroizeOnDrop)]
pub struct Mnemonic {
    words: Vec<String>,
    #[zeroize(skip)]
    separator: char,
}

impl Mnemonic {
    /// The words joined by the list's separator.
    pub fn phrase(&self) -> Zeroizing<String> {
        let mut out = Zeroizing::new(String::new());
        for (i, w) in self.words.iter().enumerate() {
            if i > 0 {
                out.push(self.separator);
            }
            out.push_str(w);
        }
        out
    }

    /// The number of words.
    pub fn len(&self) -> usize {
        self.words.len()
    }

    /// Whether there are no words (never, for a parsed or generated mnemonic).
    pub fn is_empty(&self) -> bool {
        self.words.is_empty()
    }
}

impl fmt::Debug for Mnemonic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Mnemonic(<{} words redacted>)", self.words.len())
    }
}

impl PartialEq for Mnemonic {
    fn eq(&self, other: &Self) -> bool {
        use subtle::ConstantTimeEq as _;
        let (a, b) = (self.phrase(), other.phrase());
        bool::from(a.as_bytes().ct_eq(b.as_bytes()))
    }
}

impl Eq for Mnemonic {}

/// The bits of `bytes`, most significant first, as 0/1 bytes.
fn bits_of(bytes: &[u8]) -> Zeroizing<Vec<u8>> {
    let mut bits = Zeroizing::new(Vec::with_capacity(bytes.len().saturating_mul(8)));
    for b in bytes {
        for j in (0u32..8).rev() {
            bits.push(b.checked_shr(j).unwrap_or(0) & 1);
        }
    }
    bits
}

/// Bits (0/1 bytes, most significant first) packed into an integer.
fn pack(bits: &[u8]) -> u16 {
    bits.iter().fold(0u16, |acc, b| acc.wrapping_shl(1) | u16::from(*b))
}

/// `entropy` (16, 20, 24, 28 or 32 bytes) as 12 to 24 words: each encodes 11 bits of
/// `entropy || SHA-256(entropy)[0 : ENT/32 bits]`.
pub fn to_mnemonic(entropy: &[u8], list: &WordList) -> Result<Mnemonic, KeyError> {
    let n = entropy.len();
    if !ENTROPY_LENGTHS.contains(&n) {
        return Err(KeyError::InvalidEntropy {
            reason: format!("entropy must be 16..32 bytes in steps of 4, got {n}"),
        });
    }
    let checksum = sha256(&[entropy]);
    let mut bits = bits_of(entropy);
    let cs_bits = bits_of(checksum.get(..1).unwrap_or_default());
    // ENT / 32 checksum bits: n bytes are 8n bits, so n / 4
    bits.extend(cs_bits.iter().take(n / 4));
    let words = bits.chunks(11).map(|group| String::from(list.word(pack(group)).unwrap_or_default())).collect();
    Ok(Mnemonic { words, separator: list.separator })
}

/// A fresh mnemonic from `entropy_bytes` (16, 20, 24, 28 or 32) of randomness drawn from `rng`.
pub fn generate<R: rand_core::CryptoRng + ?Sized>(
    entropy_bytes: usize,
    list: &WordList,
    rng: &mut R,
) -> Result<Mnemonic, KeyError> {
    if !ENTROPY_LENGTHS.contains(&entropy_bytes) {
        return Err(KeyError::InvalidEntropy {
            reason: format!("entropy must be 16..32 bytes in steps of 4, got {entropy_bytes}"),
        });
    }
    let mut entropy = Zeroizing::new(alloc::vec![0u8; entropy_bytes]);
    rng.fill_bytes(&mut entropy);
    to_mnemonic(&entropy, list)
}

/// Java's `\s` (the reference splits on it) and U+3000.
const fn is_separator(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\n' | '\u{0b}' | '\u{0c}' | '\r' | '\u{3000}')
}

/// Parses and validates a phrase: NFKD-normalised, split on whitespace (including U+3000), with a valid word count,
/// every word in the list, and a matching checksum. Returns the mnemonic and its entropy.
pub fn to_entropy(phrase: &str, list: &WordList) -> Result<(Mnemonic, SecretBytes), KeyError> {
    let normalised = Zeroizing::new(nfkd(phrase));
    let tokens: Vec<String> = normalised.split(is_separator).filter(|t| !t.is_empty()).map(String::from).collect();
    let mnemonic = Mnemonic { words: tokens, separator: list.separator };
    let count = mnemonic.words.len();
    if !WORD_COUNTS.contains(&count) {
        return Err(KeyError::InvalidMnemonic { reason: format!("expected 12, 15, 18, 21 or 24 words, got {count}") });
    }
    let mut bits = Zeroizing::new(Vec::with_capacity(count.saturating_mul(11)));
    for w in &mnemonic.words {
        let i = list
            .index_of(w)
            .ok_or_else(|| KeyError::InvalidMnemonic { reason: "phrase contains a word not in the list".into() })?;
        for j in (0u32..11).rev() {
            bits.push(u8::from(i.checked_shr(j).unwrap_or(0) & 1 == 1));
        }
    }
    // 11 bits per word, of which one in 33 is checksum
    let cs = count / 3;
    let (entropy_bits, checksum_bits) = bits.split_at(bits.len().saturating_sub(cs));
    let entropy =
        SecretBytes::from_vec(entropy_bits.chunks(8).map(|byte| u8::try_from(pack(byte)).unwrap_or(0)).collect());
    let expected = bits_of(sha256(&[entropy.expose()]).get(..1).unwrap_or_default());
    if expected.get(..cs) != Some(checksum_bits) {
        return Err(KeyError::InvalidMnemonic { reason: "checksum mismatch".into() });
    }
    Ok((mnemonic, entropy))
}

/// The 64-byte seed: `PBKDF2-HMAC-SHA512(NFKD(words joined by " "), "mnemonic" || NFKD(passphrase), 2048, 64)`.
pub fn seed(mnemonic: &Mnemonic, passphrase: &str) -> Secret<64> {
    let mut password = Zeroizing::new(String::new());
    for (i, w) in mnemonic.words.iter().enumerate() {
        if i > 0 {
            password.push(' ');
        }
        password.push_str(w);
    }
    let password = Zeroizing::new(nfkd(&password));
    let mut salt = Zeroizing::new(String::from("mnemonic"));
    salt.push_str(&nfkd(passphrase));
    let mut out = [0u8; 64];
    pbkdf2::pbkdf2_hmac::<sha2::Sha512>(password.as_bytes(), salt.as_bytes(), 2048, &mut out);
    let secret = Secret::new(out);
    out.zeroize();
    secret
}
