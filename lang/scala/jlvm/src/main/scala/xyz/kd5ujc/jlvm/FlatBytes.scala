package xyz.kd5ujc.jlvm

/**
 * Core's flat bytes ([[xyz.kd5ujc.core.canonical.FlatBytes]]): one chunk, so reading any byte is O(1) and never recurses.
 * A program that prepends in a loop (`reduce` over `cat`) would otherwise build a rope about `sqrt(2 * gas)` levels deep,
 * and every indexed read (`in`, `==`) would walk every level, work no size charge prices. Every canonical byte string is
 * flat by construction (`CBytes` compacts what it is given), so no value a program reads or builds can be a rope; the
 * operations still take and make `FlatBytes` so the type states it where they read and build.
 */
type FlatBytes = xyz.kd5ujc.core.canonical.FlatBytes

val FlatBytes: xyz.kd5ujc.core.canonical.FlatBytes.type = xyz.kd5ujc.core.canonical.FlatBytes
