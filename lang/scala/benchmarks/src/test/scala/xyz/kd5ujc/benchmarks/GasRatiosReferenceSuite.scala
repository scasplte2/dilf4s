package xyz.kd5ujc.benchmarks

import xyz.kd5ujc.core.canonical.Preview

import weaver.SimpleIOSuite

import GasRatios.Reference
import GasRatios.Reference.{Settings, Violation}

/** The calibration's configuration check reads effective settings, so a later flag cannot override the reference. */
object GasRatiosReferenceSuite extends SimpleIOSuite {

  private val reference =
    Settings(
      javaFeature = 21,
      initialHeap = Reference.Heap,
      maxHeap = Reference.Heap,
      preTouch = true,
      serial = true,
      activeProcessorCount = -1L,
      collectors = Reference.Collectors,
      allowedCpus = Some(1)
    )

  pureTest("the reference configuration passes, and is named") {
    expect
      .same(Right(Reference.Verified), Reference.verify(reference))
      .and(
        expect.same(
          "-Xms4g -Xmx4g -XX:+UseSerialGC -XX:+AlwaysPreTouch; Java 21; collectors Copy, MarkSweepCompact; 1 CPU",
          Reference.Verified.label
        )
      )
  }

  pureTest("each override has its exact ordered reference violations") {
    val g1 = Set("G1 Young Generation", "G1 Old Generation")
    val overrides = List(
      reference.copy(preTouch = false)       -> List(Violation.HeapNotPretouched),
      reference.copy(maxHeap = 5L << 30)     -> List(Violation.HeapSize(Reference.Heap, 5L << 30)),
      reference.copy(initialHeap = 2L << 30) -> List(Violation.HeapSize(2L << 30, Reference.Heap)),
      reference
        .copy(activeProcessorCount = 1L, allowedCpus = Some(16)) -> List(Violation.ProcessorOverride(1L), Violation.CpuAffinity(Some(16))),
      reference.copy(activeProcessorCount = 1L)                  -> List(Violation.ProcessorOverride(1L)),
      reference.copy(allowedCpus = Some(16))                     -> List(Violation.CpuAffinity(Some(16))),
      reference.copy(allowedCpus = None)                         -> List(Violation.CpuAffinity(None)),
      reference.copy(serial = false, collectors = g1)            -> List(Violation.Collector(false, g1)),
      reference.copy(javaFeature = 17)                           -> List(Violation.JavaVersion(17))
    )
    overrides.foldLeft(success) {
      case (acc, (settings, expected)) => acc.and(expect.same(Left(expected), Reference.verify(settings).left.map(_.toList)))
    }
  }

  pureTest("gas case input reports the exact invalid field and preserves valid fields") {
    val valid = GasCases.GasCase("name", "group", "program", "baseline", "data", -2L)
    expect
      .same(Right(valid), GasRatios.parseCase(valid.line))
      .and(expect.same(Left(GasRatios.Error.FieldCount(2)), GasRatios.parseCase("name\tgroup")))
      .and(
        expect
          .same(Left(GasRatios.Error.InvalidAdjustment(Preview.of("x"))), GasRatios.parseCase("name\tgroup\tx\tprogram\tbaseline\tdata"))
      )
  }

  pureTest("a CPU list counts its CPUs") {
    expect
      .same(Some(1), Reference.cpuCount("4"))
      .and(expect.same(Some(1), Reference.cpuCount(" 7\t")))
      .and(expect.same(Some(4), Reference.cpuCount("0-3")))
      .and(expect.same(Some(6), Reference.cpuCount("0-3,8,10")))
      .and(expect.same(None, Reference.cpuCount("3-1")))
      .and(expect.same(None, Reference.cpuCount("x")))
  }
}
