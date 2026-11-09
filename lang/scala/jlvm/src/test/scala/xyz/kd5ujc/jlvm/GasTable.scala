package xyz.kd5ujc.jlvm

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}

import weaver.SimpleIOSuite

/**
 * The gas table of docs/specs/jlvm.md §7, rendered from [[GasSchedule.V1]]: the spec holds it between two markers, and
 * [[GasTableSuite]] fails when it is stale. Regenerate with `sbt "jlvm/Test/runMain xyz.kd5ujc.jlvm.GasTableGen"`.
 */
object GasTable {

  val path: Path = Paths.get("docs", "specs", "jlvm.md")
  val Begin: String = "<!-- gas-table: generated from GasSchedule.V1 by GasTableGen; do not edit -->"
  val End: String = "<!-- gas-table: end -->"

  private def unit(u: SizeUnit): String = u match {
    case SizeUnit.None => "none"
    case other         => s"`$other`"
  }

  private def rate(r: Rate): String = s"${r.perUnit.value} per ${unit(r.unit)}"

  private def row(op: Op, c: OpCost): String = {
    val others =
      c.byArgument.toList.sortBy(_._1).map { case (i, r) => s"argument $i: ${rate(r)}" } ++
      c.extra.map(r => s"plus ${rate(r)}") ++
      c.search.map(s => s"search: ${s.perLevel.value} per level, ${s.perBlock.value} per level and key block") ++
      c.merging.map(m => s"merging: ${m.perLevel.value} per level and entry, ${m.perBlock.value} per level and weight block")
    val per = if (c.unit == SizeUnit.None && c.perUnit == Gas.Zero) "" else rate(Rate(c.perUnit, c.unit))
    s"| `${op.wire}` | ${c.base.value} | $per | ${others.mkString("; ")} |"
  }

  def render(s: GasSchedule): String = {
    val scalars = List(
      "literal"        -> s.literal,
      "element"        -> s.element,
      "varAccess"      -> s.varAccess,
      "perPathSegment" -> s.perPathSegment,
      "callbackRun"    -> s.callbackRun,
      "rendering"      -> s.rendering
    )
    List(
      Begin,
      "",
      s"Schedule v${s.version}. Scalar charges: " + scalars.map { case (k, g) => s"`$k` ${g.value}" }.mkString(", ") + ".",
      "",
      "| Operation | Base | Per unit | Other rates |",
      "|---|---|---|---|"
    ) ++
    Op.values.toList.map(op => row(op, s.cost(op))) ++
    List("", End)
  }.mkString("\n")

  /** The spec with its table replaced by `table`, if it has the markers. */
  def replaced(spec: String, table: String): Option[String] = {
    val (b, e) = (spec.indexOf(Begin), spec.indexOf(End))
    Option.when(b >= 0 && e > b)(spec.substring(0, b) + table + spec.substring(e + End.length))
  }

  def read: IO[String] = IO.blocking(new String(Files.readAllBytes(path), UTF_8))
}

object GasTableGen extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    GasTable.read.flatMap { spec =>
      IO.fromOption(GasTable.replaced(spec, GasTable.render(GasSchedule.V1)))(new IllegalStateException("no gas-table markers"))
        .flatMap(s => IO.blocking(Files.write(GasTable.path, s.getBytes(UTF_8))))
    }.as(ExitCode.Success)
}

object GasTableSuite extends SimpleIOSuite {
  test("the spec's gas table is GasSchedule.V1") {
    GasTable.read.map { spec =>
      val table = GasTable.render(GasSchedule.V1)
      expect(
        GasTable.replaced(spec, table).contains(spec),
        s"${GasTable.path}'s gas table is stale; regenerate with: sbt \"jlvm/Test/runMain xyz.kd5ujc.jlvm.GasTableGen\""
      )
    }
  }
}
