package xyz.kd5ujc.benchmarks

import weaver.SimpleIOSuite

import GasRatios.Reference
import GasRatios.Reference.Settings

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
    val named = Reference.verify(reference)
    expect(named.isRight, s"refused: $named")
      .and(expect(named.exists(_.contains("-XX:+UseSerialGC")), s"named: $named"))
  }

  pureTest("each override of the reference is refused, whatever the command line said") {
    val overrides = List(
      "-XX:-AlwaysPreTouch after the reference flags" -> reference.copy(preTouch = false),
      "-Xmx5g after -Xmx4g"                           -> reference.copy(maxHeap = 5L << 30),
      "-Xms2g after -Xms4g"                           -> reference.copy(initialHeap = 2L << 30),
      "-XX:ActiveProcessorCount=1 on 16 CPUs"         -> reference.copy(activeProcessorCount = 1L, allowedCpus = Some(16)),
      "-XX:ActiveProcessorCount=1 on one CPU"         -> reference.copy(activeProcessorCount = 1L),
      "no CPU pinning"                                -> reference.copy(allowedCpus = Some(16)),
      "an unreadable CPU affinity"                    -> reference.copy(allowedCpus = None),
      "G1 after the serial flag" -> reference.copy(serial = false, collectors = Set("G1 Young Generation", "G1 Old Generation")),
      "Java 17"                  -> reference.copy(javaFeature = 17)
    )
    overrides.foldLeft(success) {
      case (acc, (name, settings)) => acc.and(expect(Reference.verify(settings).isLeft, s"$name is accepted"))
    }
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
