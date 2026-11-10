package xyz.kd5ujc.storage

import xyz.kd5ujc.core.canonical.Preview

import scodec.bits.ByteVector

/**
 * Typed storage failures, raised through `cats.mtl.Raise[F, StoreError]`: generated from
 * `shared/schemas/store-errors.cddl`, so a versioned trie's `Store` kind holds the same type in every SDK.
 */
export xyz.kd5ujc.models.store_errors.{StoreError, StoreErrorKind}

/**
 * No value at `key`: the key's length in bytes, and the hex of at most its first 16 bytes as a preview, so a caller's
 * key of any length is never hex-encoded or kept whole (docs/specs/conformance.md §8).
 */
def notFound(key: ByteVector): StoreError = StoreError.NotFound(key.size, Preview.of(key.take(16).toHex))
