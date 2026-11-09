package xyz.kd5ujc.dilf4s.internal

import cats.effect.SyncIO

import xyz.kd5ujc.crypto.bls.Bls12381
import xyz.kd5ujc.crypto.signing.Secp256k1
import xyz.kd5ujc.jlvm.Jlvm

/**
 * The reference's own loads of its native backends, each attempted (`attempt` holds a non-fatal failure as a value):
 * descriptions of effects, built here and run nowhere in Scala. `Backends.load` (Java) runs each once per process, the
 * one effect the Java API runs. BLS keeps its self-check, and the JLVM loads its secp256k1, BLS12-381 and gnark
 * backends of its own.
 */
object Natives {

  val secp256k1: SyncIO[Either[Throwable, Secp256k1]] = Secp256k1.load[SyncIO].attempt
  val bls: SyncIO[Either[Throwable, Bls12381]] = Bls12381.load[SyncIO].attempt
  val jlvm: SyncIO[Either[Throwable, Jlvm]] = Jlvm.load[SyncIO].attempt
}
