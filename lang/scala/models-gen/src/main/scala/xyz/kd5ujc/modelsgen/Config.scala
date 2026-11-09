// SPDX-License-Identifier: Apache-2.0
package xyz.kd5ujc.modelsgen

/**
 * Generator configuration: what to generate and the declared properties the CDDL cannot express.
 *
 * A line-oriented file (`#` starts a comment); every directive names a rule as `module:rule`:
 * {{{
 * package   xyz.kd5ujc.scarab.models        base Scala package
 * module    conway conway path/a.cddl path/b.cddl     CDDL module, Scala subpackage, files (later override)
 * root      conway:header                    generate this rule and everything it reaches
 * preserve  conway:header_body               keep the original bytes (hashed or signed by the spec)
 * rename    conway:hash32 Hash               Scala type name
 * cases     base:point Origin Block          Scala names of the alternatives of a choice, in order
 * strip     handshake:msg_x Msg              prefix removed from the names of alternatives
 * field     conway:transaction_body 0 inputs name of an integer-keyed map entry
 * external  base:tip                         not generated: a raw `Node` (hand-written boundary)
 * }}}
 */
final case class Config(
  basePackage: String,
  modules:     Vector[Config.ModuleSpec],
  roots:       Vector[QName],
  preserve:    Set[QName],
  rename:      Map[QName, String],
  cases:       Map[QName, Vector[String]],
  strip:       Map[QName, Vector[String]],
  fields:      Map[(QName, String), String],
  external:    Set[QName],
  chunks:      Map[QName, Int] = Map.empty
)

object Config:
  final case class ModuleSpec(name: String, subpackage: String, files: Vector[String])

  private val empty = Config(
    "",
    Vector.empty,
    Vector.empty,
    Set.empty,
    Map.empty,
    Map.empty,
    Map.empty,
    Map.empty,
    Set.empty
  )

  def parse(text: String): Either[GenError, Config] =
    text.linesIterator.zipWithIndex
      .foldLeft[Either[GenError, Config]](Right(empty)) {
        case (acc, (raw, i)) =>
          acc.flatMap { cfg =>
            val line = i + 1
            val words = raw.takeWhile(_ != '#').trim.split("\\s+").toVector.filter(_.nonEmpty)
            def q(s: String): Either[GenError, QName] = s.split(":", 2) match
              case Array(m, n) => Right(QName(m, n))
              case _           => Left(GenError.BadConfig(line, ConfigProblem.QualifiedName(s)))
            words match
              case Vector()             => Right(cfg)
              case Vector("package", p) => Right(cfg.copy(basePackage = p))
              case "module" +: n +: sub +: files if files.nonEmpty =>
                Right(cfg.copy(modules = cfg.modules :+ ModuleSpec(n, sub, files)))
              case Vector("root", r)     => q(r).map(n => cfg.copy(roots = cfg.roots :+ n))
              case Vector("preserve", r) => q(r).map(n => cfg.copy(preserve = cfg.preserve + n))
              case Vector("rename", r, n) =>
                q(r).map(r => cfg.copy(rename = cfg.rename.updated(r, n)))
              case "cases" +: r +: names if names.nonEmpty =>
                q(r).map(r => cfg.copy(cases = cfg.cases.updated(r, names)))
              case "strip" +: r +: ps if ps.nonEmpty =>
                q(r).map(r => cfg.copy(strip = cfg.strip.updated(r, ps)))
              case Vector("field", r, k, n) =>
                q(r).map(r => cfg.copy(fields = cfg.fields.updated((r, k), n)))
              case Vector("external", r) => q(r).map(r => cfg.copy(external = cfg.external + r))
              case Vector("chunks", r, size) if size.toIntOption.exists(_ > 0) =>
                q(r).map(r => cfg.copy(chunks = cfg.chunks.updated(r, size.toInt)))
              case other => Left(GenError.BadConfig(line, ConfigProblem.Directive(other)))
          }
      }
      .flatMap(cfg =>
        if cfg.basePackage.isEmpty then Left(GenError.BadConfig(0, ConfigProblem.MissingPackage))
        else Right(cfg)
      )
