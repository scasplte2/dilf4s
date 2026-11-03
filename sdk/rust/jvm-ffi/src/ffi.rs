//! The C ABI, and the crate's only `unsafe`: exporting unmangled symbols, and reading caller-provided buffers.
//!
//! Every buffer is a pointer and an `int32_t` length (a Java array's length). A length of zero is the empty slice
//! whatever the pointer is (JNA may pass null for an empty array); a negative length, or a null pointer with a positive
//! length, is [`Verdict::BadArgument`]. No call writes through a pointer, keeps one past its return, or panics.

use core::slice;

use crate::{ABI_VERSION, Rule, Verdict, check_key, verify};

/// `len` bytes at `ptr`, or `None` for a negative length or a null pointer with bytes.
///
/// # Safety
///
/// If `len > 0` and `ptr` is not null, `ptr` must point to `len` initialised bytes that nothing writes for `'a`.
unsafe fn bytes<'a>(ptr: *const u8, len: i32) -> Option<&'a [u8]> {
    let len = usize::try_from(len).ok()?;
    if len == 0 {
        Some(&[])
    } else if ptr.is_null() {
        None
    } else {
        // SAFETY: `ptr` is non-null and, by this function's contract, points to `len` initialised bytes not written
        // for `'a`. `u8` has alignment 1, and `len` came from an `i32`, so `len <= isize::MAX`.
        Some(unsafe { slice::from_raw_parts(ptr, len) })
    }
}

/// [`verify`] over three caller buffers.
///
/// # Safety
///
/// Each pointer and length must satisfy [`bytes`]'s contract for the duration of the call.
unsafe fn verify_raw(
    rule: Rule,
    key: *const u8,
    key_len: i32,
    msg: *const u8,
    msg_len: i32,
    sig: *const u8,
    sig_len: i32,
) -> i32 {
    // SAFETY: the key buffer, by this function's contract.
    let key = unsafe { bytes(key, key_len) };
    // SAFETY: the message buffer, by this function's contract.
    let msg = unsafe { bytes(msg, msg_len) };
    // SAFETY: the signature buffer, by this function's contract.
    let sig = unsafe { bytes(sig, sig_len) };
    match (key, msg, sig) {
        (Some(k), Some(m), Some(s)) => verify(rule, k, m, s).code(),
        _ => Verdict::BadArgument.code(),
    }
}

/// The ABI version, [`ABI_VERSION`]: the loader checks it before using any other symbol.
#[unsafe(no_mangle)]
pub extern "C" fn dilf4s_jvm_ffi_abi_version() -> i32 {
    ABI_VERSION
}

/// Whether `key` is an Ed25519 verifying key under ZIP-215: `1` if it decodes, else `-1` (not 32 bytes), `-2` (not a
/// curve point) or `-4` (bad arguments).
///
/// # Safety
///
/// If `key_len > 0` and `key` is not null, `key` must point to `key_len` readable bytes that nothing writes during the
/// call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn dilf4s_ed25519_check_key(key: *const u8, key_len: i32) -> i32 {
    // SAFETY: the key buffer, by this function's contract.
    unsafe { bytes(key, key_len) }.map_or(Verdict::BadArgument, check_key).code()
}

/// ZIP-215 verification (`dilf4s_signing::ed25519::verify`): `1` valid, `0` invalid, `-1` key not 32 bytes, `-2` key
/// not a curve point, `-3` signature not 64 bytes, `-4` bad arguments.
///
/// # Safety
///
/// For each buffer with a positive length and a non-null pointer, the pointer must point to that many readable bytes
/// that nothing writes during the call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn dilf4s_ed25519_verify_zip215(
    key: *const u8,
    key_len: i32,
    msg: *const u8,
    msg_len: i32,
    sig: *const u8,
    sig_len: i32,
) -> i32 {
    // SAFETY: forwarded: this function's contract is `verify_raw`'s.
    unsafe { verify_raw(Rule::Zip215, key, key_len, msg, msg_len, sig, sig_len) }
}

/// libsodium-rule verification (`dilf4s_signing::ed25519::verify_libsodium`), with the codes of
/// [`dilf4s_ed25519_verify_zip215`].
///
/// # Safety
///
/// As for [`dilf4s_ed25519_verify_zip215`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn dilf4s_ed25519_verify_libsodium(
    key: *const u8,
    key_len: i32,
    msg: *const u8,
    msg_len: i32,
    sig: *const u8,
    sig_len: i32,
) -> i32 {
    // SAFETY: forwarded: this function's contract is `verify_raw`'s.
    unsafe { verify_raw(Rule::Libsodium, key, key_len, msg, msg_len, sig, sig_len) }
}

#[cfg(test)]
mod tests {
    #![allow(clippy::unwrap_used, clippy::indexing_slicing, reason = "tests fail by panicking")]

    use core::ptr;

    use super::{
        dilf4s_ed25519_check_key, dilf4s_ed25519_verify_libsodium, dilf4s_ed25519_verify_zip215,
        dilf4s_jvm_ffi_abi_version,
    };
    use crate::tests::{bytes, expected, rule, vectors};
    use crate::{ABI_VERSION, Rule, Verdict};

    fn len(b: &[u8]) -> i32 {
        i32::try_from(b.len()).unwrap()
    }

    fn call(rule: Rule, k: &[u8], m: &[u8], s: &[u8]) -> i32 {
        let f = match rule {
            Rule::Zip215 => dilf4s_ed25519_verify_zip215,
            Rule::Libsodium => dilf4s_ed25519_verify_libsodium,
        };
        // SAFETY: each pointer and length is a live slice's.
        unsafe { f(k.as_ptr(), len(k), m.as_ptr(), len(m), s.as_ptr(), len(s)) }
    }

    #[test]
    fn every_ed25519_verdict_through_the_c_abi() {
        let v = vectors();
        for c in &v.verification {
            let Some(rule) = rule(&c.scheme) else { continue };
            let (k, m, s) = (bytes(&c.verifying_key), bytes(&c.message), bytes(&c.signature));
            assert_eq!(call(rule, &k, &m, &s), expected(&c.result, &k).code(), "{} {}", c.scheme, c.name);
        }
    }

    #[test]
    fn an_empty_buffer_may_be_null_but_a_non_empty_one_may_not() {
        let v = vectors();
        let scheme = v.signing.iter().find(|s| s.scheme == "Ed25519").unwrap();
        let key = &scheme.keys[0];
        let empty = key.signatures.iter().find(|s| s.message.is_empty()).unwrap();
        let (vk, sig) = (bytes(&key.verifying_key), bytes(&empty.signature));
        // SAFETY: the key and signature are live slices; the message is null with length 0.
        let null_message = unsafe { dilf4s_ed25519_verify_zip215(vk.as_ptr(), 32, ptr::null(), 0, sig.as_ptr(), 64) };
        assert_eq!(null_message, Verdict::Valid.code());
        // SAFETY: null pointers with positive lengths, which the ABI refuses without reading.
        let null_key = unsafe { dilf4s_ed25519_verify_zip215(ptr::null(), 32, ptr::null(), 0, sig.as_ptr(), 64) };
        assert_eq!(null_key, Verdict::BadArgument.code());
        // SAFETY: as above, for the signature.
        let null_sig = unsafe { dilf4s_ed25519_verify_libsodium(vk.as_ptr(), 32, ptr::null(), 0, ptr::null(), 64) };
        assert_eq!(null_sig, Verdict::BadArgument.code());
        // SAFETY: as above, for the key alone.
        assert_eq!(unsafe { dilf4s_ed25519_check_key(ptr::null(), 32) }, Verdict::BadArgument.code());
        // SAFETY: a null key of length 0 is the empty slice.
        assert_eq!(unsafe { dilf4s_ed25519_check_key(ptr::null(), 0) }, Verdict::KeyLength.code());
        // SAFETY: a live slice.
        assert_eq!(unsafe { dilf4s_ed25519_check_key(vk.as_ptr(), 32) }, Verdict::Valid.code());
    }

    #[test]
    fn a_negative_length_is_refused_without_reading() {
        let buf = [0u8; 64];
        for n in [-1, i32::MIN] {
            // SAFETY: a negative length is refused before the pointer is read.
            let r = unsafe { dilf4s_ed25519_verify_zip215(buf.as_ptr(), 32, buf.as_ptr(), n, buf.as_ptr(), 64) };
            assert_eq!(r, Verdict::BadArgument.code());
            // SAFETY: as above.
            assert_eq!(unsafe { dilf4s_ed25519_check_key(buf.as_ptr(), n) }, Verdict::BadArgument.code());
        }
    }

    #[test]
    fn the_abi_version() {
        assert_eq!(dilf4s_jvm_ffi_abi_version(), ABI_VERSION);
    }
}
