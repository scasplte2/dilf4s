package xyz.kd5ujc.dilf4s.internal

import xyz.kd5ujc.accumulators.versioned.{VersionedTrieError, VtRootComponent}
import xyz.kd5ujc.core.canonical.Preview
import xyz.kd5ujc.dilf4s.error.{Preview => JPreview, RootComponent, StoreError => JStoreError, VersionedTrieError => JVersionedTrieError}
import xyz.kd5ujc.storage.{BackendOperation, CorruptionReason, EncodingReason, StoreContext, StoreError, StoreErrorKind, StoredRecord}

import scodec.bits.ByteVector
import weaver.SimpleIOSuite

object StoreErrorAdapterSuite extends SimpleIOSuite {

  pureTest("all seven store cases preserve their Java fields and render only at the boundary") {
    val cases: Vector[(StoreError, JStoreError)] = Vector(
      StoreError.Encoding(StoreContext.Key, EncodingReason.FixedWidth(8, 7)) -> new JStoreError.Encoding("key", "expected 8 bytes, got 7"),
      StoreError.NotFound(99, Preview.of("ab"))                              -> new JStoreError.NotFound(99, new JPreview("ab", 2, false)),
      StoreError.VersionConflict(2, 3)                                       -> new JStoreError.VersionConflict(2, 3),
      StoreError.UnknownVersion(4, 3)                                        -> new JStoreError.UnknownVersion(4, 3),
      StoreError.Pruned(2, 3)                                                -> new JStoreError.Pruned(2, 3),
      StoreError.Backend(BackendOperation.Get, new RuntimeException("native failure")) -> new JStoreError.Backend("get", "native failure"),
      StoreError.Corrupt(CorruptionReason.Width(StoredRecord.Version, 1, 8)) -> new JStoreError.Corrupt("version record has 1 bytes")
    )
    expect
      .same(7, cases.size)
      .and(expect.same(StoreErrorKind.values.toVector, cases.map(_._1.kind)))
      .and(expect.same(cases.map(_._2), cases.map(c => Errors.storeError(c._1))))
  }

  pureTest("a backend exception without a message renders its class at the boundary") {
    expect.same(
      new JStoreError.Backend("get", "java.lang.RuntimeException"),
      Errors.storeError(StoreError.Backend(BackendOperation.Get, new RuntimeException()))
    )
  }

  pureTest("an absent parent root remains optional until Java rendering") {
    val error = VersionedTrieError.RootMismatch(VtRootComponent.Parent, None, Some(ByteVector(0xab)))
    expect.same(new JVersionedTrieError.RootMismatch(RootComponent.PARENT, "none", "ab"), Errors.versionedTrieError(error))
  }
}
