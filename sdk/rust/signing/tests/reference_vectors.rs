//! The normative reference corpora in `docs/test-vectors` for signatures and key derivation: ethereum/bls12-381-tests
//! v0.1.2, RFC 8032 (`Ed25519.json`), the BIP32-Ed25519 files (`ExtendedEd25519.json`, `Bip32Ed25519Ckd.json`,
//! `Bip32Ed25519Icarus.json`), `Bip39MnemonicToEntropy.json`, and published BIP-39, SLIP-10, BIP32, CIP-0003 and
//! EIP-2333 vectors.

#![cfg(feature = "sign")]
#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::indexing_slicing,
    clippy::arithmetic_side_effects,
    clippy::std_instead_of_alloc,
    clippy::std_instead_of_core,
    clippy::as_conversions,
    clippy::many_single_char_names,
    clippy::items_after_statements,
    clippy::doc_markdown,
    reason = "tests fail by panicking, and run with std"
)]

#[path = "../../core/tests/common/mod.rs"]
mod common;

use std::path::PathBuf;

use common::bytes;
use dilf4s_core::{Value, hex};
use dilf4s_signing::keys::path::{self, ChildIndex};
use dilf4s_signing::keys::{ExtendedKey, bip32, bip39, icarus, slip10};
use dilf4s_signing::{bls, ed25519, ed25519_bip32};

fn docs(path: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../docs/test-vectors").join(path)
}

fn json(path: &str) -> Value {
    dilf4s_core::json::parse(&std::fs::read_to_string(docs(path)).unwrap()).unwrap()
}

fn get<'a>(v: &'a Value, path: &[&str]) -> &'a Value {
    path.iter().fold(v, |v, k| match v {
        Value::Map(m) => m.get(k).unwrap_or_else(|| panic!("missing {k}")),
        other => panic!("not an object at {k}: {other:?}"),
    })
}

fn text<'a>(v: &'a Value, path: &[&str]) -> &'a str {
    match get(v, path) {
        Value::Text(t) => t,
        other => panic!("{path:?}: {other:?}"),
    }
}

fn hx(v: &Value, path: &[&str]) -> Vec<u8> {
    bytes(text(v, path).trim_start_matches("0x"))
}

fn items(v: &Value) -> &[Value] {
    match v {
        Value::Array(a) => a.items(),
        other => panic!("not an array: {other:?}"),
    }
}

fn boolean(v: &Value) -> bool {
    match v {
        Value::Bool(b) => *b,
        other => panic!("not a boolean: {other:?}"),
    }
}

fn english() -> bip39::WordList {
    bip39::WordList::english()
}

// ------------------------------------------------------------------ Ed25519 and BIP32-Ed25519

#[test]
fn rfc8032_ed25519() {
    let cases = json("Ed25519.json");
    assert_eq!(items(&cases).len(), 5);
    for c in items(&cases) {
        let sk = ed25519::SigningKey::from_bytes(&hx(c, &["inputs", "secretKey"])).unwrap();
        let m = hx(c, &["inputs", "message"]);
        let vk = sk.verifying_key();
        assert_eq!(vk.as_bytes().to_vec(), hx(c, &["outputs", "verificationKey"]));
        let sig = sk.sign(&m);
        assert_eq!(sig.as_bytes().to_vec(), hx(c, &["outputs", "signature"]));
        assert!(ed25519::verify(&vk, &m, &sig));
        assert!(ed25519::verify_libsodium(&vk, &m, &sig));
    }
}

/// The BramblSc signing vectors use unclamped `kL`, which the decoder rightly refuses; they exercise the signing
/// equation only. Vector 3's published signature is valid but was not produced by the RFC 8032 nonce rule from the
/// stated key (the Scala reference agrees), so it is checked by verification only.
#[test]
fn bip32_ed25519_signing_known_answers() {
    let cases = json("ExtendedEd25519.json");
    assert_eq!(items(&cases).len(), 3);
    for (i, c) in items(&cases).iter().enumerate() {
        let secret = hx(c, &["inputs", "secretKey"]);
        assert!(ed25519_bip32::SigningKey::from_bytes(&secret[..64]).is_err() || i > 0, "unclamped kL is refused");
        let sk = ed25519_bip32::SigningKey::from_parts_unchecked(
            secret[..32].try_into().unwrap(),
            secret[32..64].try_into().unwrap(),
        );
        let m = hx(c, &["inputs", "message"]);
        let vk = sk.verifying_key();
        assert_eq!(vk.as_bytes().to_vec(), hx(c, &["outputs", "verificationKey"])[..32].to_vec());
        let sig = sk.sign(&m);
        assert!(ed25519::verify(&vk, &m, &sig), "our signature verifies");
        let published = ed25519::Signature::from_bytes(&hx(c, &["outputs", "signature"])).unwrap();
        if i == 2 {
            assert!(ed25519::verify(&vk, &m, &published), "the published signature verifies");
        } else {
            assert_eq!(sig, published);
        }
    }
}

fn extended(raw: &[u8]) -> ExtendedKey<ed25519_bip32::SigningKey> {
    ExtendedKey {
        key: ed25519_bip32::SigningKey::from_bytes(&raw[..64]).unwrap(),
        chain_code: raw[64..].try_into().unwrap(),
    }
}

#[test]
fn bip32_ed25519_child_derivation_private_and_public() {
    let cases = json("Bip32Ed25519Ckd.json");
    assert_eq!(items(&cases).len(), 10);
    for c in items(&cases) {
        let root = extended(&hx(c, &["inputs", "rootSecretKey"]));
        let path: Vec<ChildIndex> = items(get(c, &["inputs", "path"]))
            .iter()
            .map(|step| {
                let step = items(step);
                let i = match &step[1] {
                    Value::Int(i) => u32::try_from(i).unwrap(),
                    other => panic!("{other:?}"),
                };
                match &step[0] {
                    Value::Text(t) if t == "hard" => ChildIndex::hardened(i).unwrap(),
                    _ => ChildIndex::soft(i).unwrap(),
                }
            })
            .collect();
        let child = icarus::derive(&root, &path).unwrap();
        let mut secret = child.key.to_bytes().to_vec();
        secret.extend_from_slice(&child.chain_code);
        assert_eq!(hex::encode(&secret), text(c, &["outputs", "childSecretKey"]));
        let mut public = child.key.verifying_key().as_bytes().to_vec();
        public.extend_from_slice(&child.chain_code);
        assert_eq!(hex::encode(&public), text(c, &["outputs", "childVerificationKey"]));

        // public derivation from the deepest hardened ancestor through the trailing soft indices
        let split = path.iter().rposition(|i| i.is_hardened()).map_or(0, |p| p + 1);
        let anchor = icarus::derive(&root, &path[..split]).unwrap();
        let (mut vk, mut cc) = (anchor.key.verifying_key(), anchor.chain_code);
        for i in &path[split..] {
            (vk, cc) = icarus::public_child(&vk, &cc, *i).unwrap();
        }
        let mut via_public = vk.as_bytes().to_vec();
        via_public.extend_from_slice(&cc);
        assert_eq!(hex::encode(&via_public), text(c, &["outputs", "childVerificationKey"]));
    }
}

#[test]
fn icarus_master_keys_from_mnemonics() {
    let cases = json("Bip32Ed25519Icarus.json");
    assert_eq!(items(&cases).len(), 32);
    for c in items(&cases) {
        let (_, entropy) = bip39::to_entropy(text(c, &["inputs", "mnemonic"]), &english()).unwrap();
        let key = icarus::from_entropy(entropy.expose(), text(c, &["inputs", "password"])).unwrap();
        let mut out = key.key.to_bytes().to_vec();
        out.extend_from_slice(&key.chain_code);
        assert_eq!(hex::encode(&out), text(c, &["outputs", "extendedEd25519"]));
    }
}

#[test]
fn cip0003_icarus_reference() {
    let phrase = "eight country switch draw meat scout mystery blade tip drift useless good keep usage title";
    let (_, entropy) = bip39::to_entropy(phrase, &english()).unwrap();
    let key = icarus::from_entropy(entropy.expose(), "").unwrap();
    let mut out = key.key.to_bytes().to_vec();
    out.extend_from_slice(&key.chain_code);
    assert_eq!(
        hex::encode(&out),
        "c065afd2832cd8b087c4d9ab7011f481ee1e0721e78ea5dd609f3ab3f156d245d176bd8fd4ec60b4731c3918a2a72a0226c0cd119ec35b47e4d55884667f552a23f7fdcd4a10c6cd2c7393ac61d877873e248f417634aa3d812af327ffe9d620"
    );
}

#[test]
fn bip32_ed25519_rejections() {
    let root = icarus::root(&[7; 96]).unwrap();
    assert!(
        icarus::public_child(&root.key.verifying_key(), &root.chain_code, ChildIndex::hardened(0).unwrap()).is_err()
    );
    let mut unclamped = [0u8; 64];
    unclamped[0] = 1;
    assert!(matches!(
        ed25519_bip32::SigningKey::from_bytes(&unclamped),
        Err(dilf4s_signing::SigningError::InvalidKey { .. })
    ));
    assert!(matches!(
        ed25519_bip32::SigningKey::from_bytes(&[0; 63]),
        Err(dilf4s_signing::SigningError::InvalidEncoding { .. })
    ));
    assert!(icarus::root(&[0; 95]).is_err());
    // a non-canonical encoding of the parent key is refused for public derivation
    let non_canonical =
        ed25519::VerifyingKey::from_bytes(&bytes("eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f"))
            .unwrap();
    assert!(icarus::public_child(&non_canonical, &[0; 32], ChildIndex::soft(0).unwrap()).is_err());
}

// ------------------------------------------------------------------ BIP-39

#[test]
fn bip39_mnemonic_to_entropy() {
    let cases = json("Bip39MnemonicToEntropy.json");
    assert_eq!(items(&cases).len(), 25);
    for c in items(&cases) {
        let (_, entropy) = bip39::to_entropy(text(c, &["inputs", "mnemonic"]), &english()).unwrap();
        assert_eq!(hex::encode(entropy.expose()), text(c, &["outputs", "entropy"]));
    }
}

#[test]
fn bip39_trezor_vectors() {
    let cases = [
        (
            "00000000000000000000000000000000",
            "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about",
            "c55257c360c07c72029aebc1b53c05ed0362ada38ead3e3e9efa3708e53495531f09a6987599d18264c1e1c92f2cf141630c7a3c4ab7c81b2f001698e7463b04",
        ),
        (
            "7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f",
            "legal winner thank year wave sausage worth useful legal winner thank yellow",
            "2e8905819b8723fe2c1d161860e5ee1830318dbf49a83bd451cfb8440c28bd6fa457fe1296106559a3c80937a1c1069be3a3a5bd381ee6260e8d9739fce1f607",
        ),
        (
            "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
            "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo vote",
            "dd48c104698c30cfe2b6142103248622fb7bb0ff692eebb00089b32d22484e1613912f0a5b694407be899ffd31ed3992c456cdf60f5d4564b8ba3f05a69890ad",
        ),
    ];
    for (entropy, phrase, seed) in cases {
        let m = bip39::to_mnemonic(&bytes(entropy), &english()).unwrap();
        assert_eq!(*m.phrase(), phrase);
        let (parsed, e) = bip39::to_entropy(phrase, &english()).unwrap();
        assert_eq!(hex::encode(e.expose()), entropy);
        assert_eq!(parsed, m);
        assert_eq!(hex::encode(bip39::seed(&m, "TREZOR").expose()), seed);
    }
}

fn word_list(name: &str, separator: char) -> bip39::WordList {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../../crypto/src/main/resources/bip-0039")
        .join(format!("{name}.txt"));
    bip39::WordList::from_text(&std::fs::read_to_string(path).unwrap(), separator).unwrap()
}

#[test]
fn bip39_japanese_vectors() {
    use unicode_normalization::UnicodeNormalization as _;
    let japanese = word_list("japanese", '\u{3000}');
    let passphrase = "㍍ガバヴァぱばぐゞちぢ十人十色";
    let cases = [
        (
            "00000000000000000000000000000000",
            "あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あおぞら",
            "a262d6fb6122ecf45be09c50492b31f92e9beb7d9a845987a02cefda57a15f9c467a17872029a9e92299b5cbdf306e3a0ee620245cbd508959b6cb7ca637bd55",
        ),
        (
            "7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f",
            "そつう　れきだい　ほんやく　わかす　りくつ　ばいか　ろせん　やちん　そつう　れきだい　ほんやく　わかめ",
            "aee025cbe6ca256862f889e48110a6a382365142f7d16f2b9545285b3af64e542143a577e9c144e101a6bdca18f8d97ec3366ebf5b088b1c1af9bc31346e60d9",
        ),
        (
            "000000000000000000000000000000000000000000000000",
            "あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あらいぐま",
            "d99e8f1ce2d4288d30b9c815ae981edd923c01aa4ffdc5dee1ab5fe0d4a3e13966023324d119105aff266dac32e5cd11431eeca23bbd7202ff423f30d6776d69",
        ),
    ];
    for (entropy, phrase, seed) in cases {
        let m = bip39::to_mnemonic(&bytes(entropy), &japanese).unwrap();
        let expected: Vec<String> = phrase.split('\u{3000}').map(|w| w.nfkd().collect()).collect();
        assert_eq!(*m.phrase(), expected.join("\u{3000}"));
        let (parsed, e) = bip39::to_entropy(phrase, &japanese).unwrap();
        assert_eq!(hex::encode(e.expose()), entropy);
        assert_eq!(hex::encode(bip39::seed(&parsed, passphrase).expose()), seed);
        assert_eq!(hex::encode(bip39::seed(&m, passphrase).expose()), seed);
    }
}

#[test]
fn bip39_every_bundled_word_list_round_trips() {
    for (name, sep) in [
        ("english", ' '),
        ("japanese", '\u{3000}'),
        ("korean", ' '),
        ("spanish", ' '),
        ("chinese_simplified", ' '),
        ("chinese_traditional", ' '),
        ("french", ' '),
        ("italian", ' '),
        ("czech", ' '),
        ("portuguese", ' '),
    ] {
        let list = word_list(name, sep);
        let entropy = [0x7f; 16];
        let m = bip39::to_mnemonic(&entropy, &list).unwrap();
        let (_, back) = bip39::to_entropy(&m.phrase(), &list).unwrap();
        assert_eq!(back.expose(), &entropy, "{name}");
    }
    assert_eq!(word_list("english", ' '), english(), "the embedded list is the bundled one");
}

#[test]
fn bip39_typed_errors() {
    let words = english();
    let kind = |phrase: &str| bip39::to_entropy(phrase, &words).map(|_| ()).unwrap_err().kind();
    assert_eq!(kind(&["abandon"; 12].join(" ")), "InvalidMnemonic", "checksum mismatch");
    assert_eq!(kind(&format!("{} abou", ["abandon"; 11].join(" "))), "InvalidMnemonic", "unknown word");
    assert_eq!(kind(&["abandon"; 11].join(" ")), "InvalidMnemonic", "11 words");
    assert_eq!(bip39::to_mnemonic(&[0; 17], &words).unwrap_err().kind(), "InvalidEntropy");
    assert!(
        bip39::to_entropy(
            "  abandon  abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon   about ",
            &words
        )
        .is_ok(),
        "whitespace is normalised"
    );
    let short: Vec<String> = (0..2047).map(|i| format!("w{i}")).collect();
    assert_eq!(bip39::WordList::new(short, ' ').unwrap_err().kind(), "InvalidWordList");
    let mut dup: Vec<String> = (0..2048).map(|i| format!("w{i}")).collect();
    dup[1] = "w0".into();
    assert_eq!(bip39::WordList::new(dup, ' ').unwrap_err().kind(), "InvalidWordList");
    // a word with a separator in it would make generated phrases unparsable, and so would a separator parsing ignores
    let mut spaced: Vec<String> = (0..2048).map(|i| format!("w{i}")).collect();
    spaced[0] = "abandon abandon".into();
    assert_eq!(bip39::WordList::new(spaced, ' ').unwrap_err().kind(), "InvalidWordList");
    let mut ideographic: Vec<String> = (0..2048).map(|i| format!("w{i}")).collect();
    ideographic[0] = "a\u{3000}b".into();
    assert_eq!(bip39::WordList::new(ideographic, ' ').unwrap_err().kind(), "InvalidWordList");
    let plain: Vec<String> = (0..2048).map(|i| format!("w{i}")).collect();
    assert_eq!(bip39::WordList::new(plain.clone(), ',').unwrap_err().kind(), "InvalidWordList");
    let custom = bip39::WordList::new(plain, '\u{3000}').unwrap();
    let generated = bip39::to_mnemonic(&[0; 16], &custom).unwrap();
    assert_eq!(
        bip39::to_entropy(&generated.phrase(), &custom).unwrap().1.expose(),
        &[0; 16],
        "a generated phrase parses back"
    );
    let m = bip39::to_mnemonic(&[1; 16], &words).unwrap();
    assert!(!format!("{m:?}").contains(&m.phrase()[..5]), "mnemonics are redacted");
}

// ------------------------------------------------------------------ paths, SLIP-10, BIP32

#[test]
fn paths() {
    let serialized = |p: &str| path::parse(p).map(|v| v.into_iter().map(ChildIndex::serialized).collect::<Vec<_>>());
    assert_eq!(serialized("m/44'/0/7").unwrap(), vec![0x8000_002c, 0, 7]);
    assert_eq!(serialized("m/0h").unwrap(), vec![0x8000_0000]);
    assert_eq!(serialized("m").unwrap(), Vec::<u32>::new());
    // an empty segment is malformed (section 3), as in Scala and TypeScript
    for bad in [" m/1/ ", "m/1/", "44'/0", "m/x", "m/2147483648", "m/-1", "m//1", "m/'", "m/1''", "M/1", "m/4294967296"]
    {
        assert_eq!(path::parse(bad).unwrap_err().kind(), "InvalidDerivation", "{bad}");
    }
    assert_eq!(serialized("m/2147483647'").unwrap(), vec![u32::MAX]);
}

#[test]
fn slip10_vector_1() {
    let seed = bytes("000102030405060708090a0b0c0d0e0f");
    let m = slip10::master(&seed).unwrap();
    assert_eq!(hex::encode(m.key.as_bytes()), "2b4be7f19ee27bbf30c667b642d5f4aa69fd169872f8fc3059c08ebae2eb19e7");
    assert_eq!(hex::encode(&m.chain_code), "90046a93de5380a72b5e45010748567d5ea02bbf6522f979e05c0d8d8ca9fffb");
    let c = slip10::derive(&seed, &path::parse("m/0'").unwrap()).unwrap();
    assert_eq!(hex::encode(c.key.as_bytes()), "68e0fe46dfb67e368c75379acec591dad19df3cde26e63b93a8e704f1dade7a3");
    assert_eq!(hex::encode(&c.chain_code), "8b59aa11380b624e81507a27fedda59fea6d0b779a778918a2fd3590e16e9c69");
    assert!(slip10::derive(&seed, &path::parse("m/0").unwrap()).is_err(), "soft indices are rejected");
    assert!(slip10::master(&[0; 15]).is_err() && slip10::master(&[0; 65]).is_err());
}

#[test]
fn bip32_secp256k1_vector_1() {
    let seed = bytes("000102030405060708090a0b0c0d0e0f");
    let at = |p: &str| bip32::derive(&seed, &path::parse(p).unwrap()).unwrap();
    let check = |p: &str, key: &str, cc: &str| {
        let k = at(p);
        assert_eq!(hex::encode(&*k.key.to_bytes()), key, "{p}");
        assert_eq!(hex::encode(&k.chain_code), cc, "{p}");
    };
    check(
        "m",
        "e8f32e723decf4051aefac8e2c93c9c5b214313817cdb01a1494b917c8436b35",
        "873dff81c02f525623fd1fe5167eac3a55a049de3d314bb42ee227ffed37d508",
    );
    check(
        "m/0'",
        "edb2e14f9ee77d26dd93b4ecede8d16ed408ce149b6cd80b0715a2d911a0afea",
        "47fdacbd0f1097043b78c63c20c34ef4ed9a111d980047ad16282c7ae6236141",
    );
    check(
        "m/0'/1",
        "3c6cb8d0f6a264c91ea8b5030fadaa8e538b020f0a387421a12de9319dc93368",
        "2a7857631386ba23dacac34180dd1983734e444fdbf774041578e9b6adb37c19",
    );
    let parent = at("m/0'");
    let (vk, cc) =
        bip32::public_child(&parent.key.verifying_key(), &parent.chain_code, ChildIndex::soft(1).unwrap()).unwrap();
    assert_eq!(hex::encode(vk.as_bytes()), "03501e454bf00751f24b1b489aa925215d66af2234e3891c3b21a52bedb3cd711c");
    let private = at("m/0'/1");
    assert_eq!(vk, private.key.verifying_key());
    assert_eq!(cc, private.chain_code);
    assert!(bip32::public_child(&vk, &cc, ChildIndex::hardened(1).unwrap()).is_err(), "hardened rejected");
}

// ------------------------------------------------------------------ BLS12-381

fn bls_cases(category: &str, count: usize) -> Vec<Value> {
    let doc = json("bls/eth2-bls12-381-tests-v0.1.2.json");
    let cases = items(get(&doc, &[category])).to_vec();
    assert_eq!(cases.len(), count, "{category}");
    cases
}

/// A member that may be `null`, which the canonical JSON mapping drops.
fn optional<'a>(v: &'a Value, key: &str) -> Option<&'a Value> {
    match v {
        Value::Map(m) => m.get(key).filter(|v| !matches!(v, Value::Null)),
        other => panic!("not an object: {other:?}"),
    }
}

fn hex0x(v: &Value) -> Vec<u8> {
    match v {
        Value::Text(t) => bytes(t.trim_start_matches("0x")),
        other => panic!("{other:?}"),
    }
}

/// The eth2 suite treats undecodable keys and signatures as verification failures; this API reports them as typed
/// errors, so `Err` is mapped to `false` here.
fn keys(v: &Value) -> Option<Vec<bls::VerifyingKey>> {
    items(v).iter().map(|k| bls::VerifyingKey::from_bytes(&hex0x(k)).ok()).collect()
}

#[test]
fn eth2_sign() {
    for c in bls_cases("sign", 10) {
        let expected = optional(&c, "output").map(hex0x);
        let actual = bls::SigningKey::from_bytes(&hex0x(get(&c, &["input", "privkey"])))
            .ok()
            .map(|sk| sk.sign(&hex0x(get(&c, &["input", "message"]))).as_bytes().to_vec());
        assert_eq!(actual, expected, "{}", text(&c, &["name"]));
    }
}

#[test]
fn eth2_verify() {
    for c in bls_cases("verify", 29) {
        let ok = bls::verify_bytes(
            &hex0x(get(&c, &["input", "pubkey"])),
            &hex0x(get(&c, &["input", "message"])),
            &hex0x(get(&c, &["input", "signature"])),
        )
        .unwrap_or(false);
        assert_eq!(ok, boolean(get(&c, &["output"])), "{}", text(&c, &["name"]));
    }
}

#[test]
fn eth2_aggregate() {
    for c in bls_cases("aggregate", 6) {
        let sigs: Option<Vec<bls::Signature>> =
            items(get(&c, &["input"])).iter().map(|s| bls::Signature::from_bytes(&hex0x(s)).ok()).collect();
        let actual = sigs.and_then(|s| bls::aggregate(&s).ok()).map(|s| s.as_bytes().to_vec());
        let expected = optional(&c, "output").map(hex0x);
        assert_eq!(actual, expected, "{}", text(&c, &["name"]));
    }
}

#[test]
fn eth2_fast_aggregate_verify() {
    for c in bls_cases("fast_aggregate_verify", 12) {
        let sig = bls::Signature::from_bytes(&hex0x(get(&c, &["input", "signature"]))).ok();
        let ok = match (keys(get(&c, &["input", "pubkeys"])), sig) {
            (Some(k), Some(s)) => bls::fast_aggregate_verify(&k, &hex0x(get(&c, &["input", "message"])), &s),
            _ => false,
        };
        assert_eq!(ok, boolean(get(&c, &["output"])), "{}", text(&c, &["name"]));
    }
}

#[test]
fn eth2_aggregate_verify() {
    for c in bls_cases("aggregate_verify", 5) {
        let messages: Vec<Vec<u8>> = items(get(&c, &["input", "messages"])).iter().map(hex0x).collect();
        let sig = bls::Signature::from_bytes(&hex0x(get(&c, &["input", "signature"]))).ok();
        let ok = match (keys(get(&c, &["input", "pubkeys"])), sig) {
            (Some(k), Some(s)) => {
                let pairs: Vec<(bls::VerifyingKey, &[u8])> =
                    k.into_iter().zip(messages.iter().map(Vec::as_slice)).collect();
                bls::aggregate_verify(&pairs, &s)
            }
            _ => false,
        };
        assert_eq!(ok, boolean(get(&c, &["output"])), "{}", text(&c, &["name"]));
    }
}

#[test]
fn eth2_batch_verify() {
    for c in bls_cases("batch_verify", 4) {
        let messages: Vec<Vec<u8>> = items(get(&c, &["input", "messages"])).iter().map(hex0x).collect();
        let sigs: Option<Vec<bls::Signature>> = items(get(&c, &["input", "signatures"]))
            .iter()
            .map(|s| bls::Signature::from_bytes(&hex0x(s)).ok())
            .collect();
        let ok = match (keys(get(&c, &["input", "pubkeys"])), sigs) {
            (Some(k), Some(s)) => {
                let triples: Vec<(bls::VerifyingKey, &[u8], bls::Signature)> =
                    k.into_iter().zip(messages.iter().map(Vec::as_slice)).zip(s).map(|((k, m), s)| (k, m, s)).collect();
                bls::batch_verify(&triples)
            }
            _ => false,
        };
        assert_eq!(ok, boolean(get(&c, &["output"])), "{}", text(&c, &["name"]));
    }
}

#[test]
fn eth2_deserialization() {
    // KeyValidate, not bare deserialization: the identity decodes but is never a valid public key
    for c in bls_cases("deserialization_G1", 13) {
        let name = text(&c, &["name"]);
        let ok = bls::VerifyingKey::from_bytes(&hex0x(get(&c, &["input", "pubkey"]))).is_ok();
        if name == "deserialization_succeeds_infinity_with_true_b_flag" {
            assert!(!ok, "KeyValidate rejects the identity");
        } else {
            assert_eq!(ok, boolean(get(&c, &["output"])), "{name}");
        }
    }
    for c in bls_cases("deserialization_G2", 15) {
        let ok = bls::Signature::from_bytes(&hex0x(get(&c, &["input", "signature"]))).is_ok();
        assert_eq!(ok, boolean(get(&c, &["output"])), "{}", text(&c, &["name"]));
    }
}

#[test]
fn eth2_hash_to_g2() {
    for c in bls_cases("hash_to_G2", 4) {
        // the vectors write Fp2 as "c0,c1"; the uncompressed encoding is c1 first
        let coord = |k: &str| -> Vec<u8> {
            text(&c, &["output", k]).split(',').rev().flat_map(|s| bytes(s.trim_start_matches("0x"))).collect()
        };
        let expected = [coord("x"), coord("y")].concat();
        let point = bls::hash_to_g2(
            text(&c, &["input", "msg"]).as_bytes(),
            b"QUUX-V01-CS02-with-BLS12381G2_XMD:SHA-256_SSWU_RO_",
        );
        assert_eq!(point.to_uncompressed().to_vec(), expected, "{}", text(&c, &["name"]));
    }
}

#[test]
fn bls_tessellation_known_answer_and_proof_of_possession() {
    let sk = bls::SigningKey::from_bytes(&bytes("328388aff0d4a5b7dc9205abd374e7e98f3cd9f3418edb4eafda5fb16473d216"))
        .unwrap();
    let vk = sk.verifying_key();
    assert_eq!(
        hex::encode(vk.as_bytes()),
        "b53d21a4cfd562c469cc81514d4ce5a6b577d8403d32a394dc265dd190b47fa9f829fdd7963afdf972e5e77854051f6f"
    );
    assert_eq!(
        hex::encode(sk.sign(&[0xab; 32]).as_bytes()),
        "ae82747ddeefe4fd64cf9cedb9b04ae3e8a43420cd255e3c7cd06a8d88b7c7f8638543719981c5d16fa3527c468c25f0026704a6951bde891360c7e8d12ddee0559004ccdbe6046b55bae1b257ee97f7cdb955773d7cf29adf3ccbb9975e4eb9"
    );
    let pop = sk.pop_prove();
    assert!(bls::pop_verify(&vk, &pop));
    let other = bls::SigningKey::from_bytes(&[[0u8; 31].as_slice(), &[7]].concat()).unwrap();
    assert!(!bls::pop_verify(&other.verifying_key(), &pop), "a PoP is bound to its key");
    assert!(!bls::verify(&vk, vk.as_bytes(), &pop), "a PoP is not a message signature (distinct DST)");
    assert!(bls::SigningKey::from_bytes(&[0; 32]).is_err(), "zero key");
    let r = bytes("73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001");
    assert!(bls::SigningKey::from_bytes(&r).is_err(), "key = r");
    // the identity signature decodes but never verifies, even over the identity aggregate
    let identity = bls::Signature::from_bytes(&bytes(&format!("c0{}", "00".repeat(95)))).unwrap();
    assert!(!bls::verify(&vk, b"", &identity));
    assert!(!bls::aggregate_verify(&[], &sk.sign(b"")), "no pairs");
    assert!(!bls::fast_aggregate_verify(&[], b"", &sk.sign(b"")), "no keys");
    assert!(bls::aggregate(&[]).is_err());
    assert!(!format!("{sk:?}").contains("3283"), "secret keys are redacted");
}

#[test]
fn eip2333_test_case_0() {
    let seed = bytes(
        "c55257c360c07c72029aebc1b53c05ed0362ada38ead3e3e9efa3708e53495531f09a6987599d18264c1e1c92f2cf141630c7a3c4ab7c81b2f001698e7463b04",
    );
    let decimal = |b: &[u8]| dilf4s_core::BigInt::from_bytes_be(num_sign(), b).to_string();
    let master = bls::derive_master_sk(&seed).unwrap();
    assert_eq!(
        decimal(master.as_bytes()),
        "6083874454709270928345386274498605044986640685124978867557563392430687146096"
    );
    let child = bls::derive_child_sk(&master, 0).unwrap();
    assert_eq!(
        decimal(child.as_bytes()),
        "20397789859736650942317412262472558107875392172444076792671091975210932703118"
    );
    assert!(bls::derive_master_sk(&[0; 31]).is_err(), "EIP-2333 seeds are at least 32 bytes");
}

fn num_sign() -> num_bigint::Sign {
    num_bigint::Sign::Plus
}
