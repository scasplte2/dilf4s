package xyz.kd5ujc.storage

import java.nio.charset.StandardCharsets.UTF_8

import scodec.bits.ByteVector

/**
 * Two histories that reach the same state store the same bytes except each store's generation, which counts the
 * mutations that got there. These helpers compare everything else exactly.
 */
object Generations {

  private def utf8(s: String): ByteVector = ByteVector(s.getBytes(UTF_8))

  private val versionedGeneration = utf8("/metageneration")
  private val trieState = utf8("/metastate")

  /** Every record, with the generation records removed and the trie state records cut to latest ‖ floor. */
  def masked(records: List[(ByteVector, ByteVector)]): List[(ByteVector, ByteVector)] =
    records.flatMap {
      case (k, _) if k.endsWith(versionedGeneration)        => Nil
      case (k, v) if k.endsWith(trieState) && v.size == 24L => List(k -> v.take(16L))
      case other                                            => List(other)
    }

  /** The generations recorded in `records`, by key. */
  def of(records: List[(ByteVector, ByteVector)]): Map[ByteVector, Long] =
    records.collect {
      case (k, v) if k.endsWith(versionedGeneration)        => k -> v.toLong(signed = false)
      case (k, v) if k.endsWith(trieState) && v.size == 24L => k -> v.drop(16L).toLong(signed = false)
    }.toMap
}
