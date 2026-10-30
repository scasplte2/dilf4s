// SPDX-License-Identifier: Apache-2.0
package xyz.kd5ujc.modelsgen

import cats.data.NonEmptyList
import cats.parse.{Numbers, Parser, Parser0}

import xyz.kd5ujc.modelsgen.Ast._

/** A CDDL parser (RFC 8610) on cats-parse. It accepts the whole grammar used by the pinned Cardano
  * specifications: rules, generics, group and type choices, occurrence indicators, member keys
  * (`name:`, `value:`, `type =>`), ranges, control operators, tags, `&` enumerations, sockets (`$`,
  * `$$` names) and `/=`, `//=` extensions. Floating-point literals are parsed, never interpreted.
  */
object CddlParser:
  final case class ParseFailure(detail: String, line: Int, column: Int):
    def message: String = s"CDDL parse error at $line:$column: $detail"

  // Lexical ------------------------------------------------------------------------------------
  private val comment: Parser[Unit] = (Parser.char(';') *> Parser.charsWhile0(_ != '\n')).void
  private val space: Parser[Unit] = Parser.charIn(" \t\r\n").void
  private val sp: Parser0[Unit] = (space | comment).rep0.void
  private def lexeme[A](p: Parser[A]): Parser[A] = p <* sp
  private def tok(s: String): Parser[Unit] = lexeme(Parser.string(s))

  private def isAlpha(c: Char) = (c.isLetter && c < 128) || c == '@' || c == '_' || c == '$'
  private def isDigit(c: Char) = c >= '0' && c <= '9'
  private val idStart: Parser[Char] = Parser.charWhere(isAlpha)
  private val idRest: Parser0[String] =
    Parser.charsWhile0(c => isAlpha(c) || isDigit(c) || c == '-' || c == '.')

  /** RFC 8610 `id`: dots and dashes only between alphanumerics, so `a..b` is not one identifier. */
  private val ident: Parser[String] =
    (idStart ~ idRest).string.flatMap { s =>
      val bad =
        s.endsWith(".") || s.endsWith("-") || s.contains("..") || s.contains(".-") || s.contains(
          "-."
        )
      if bad then Parser.failWith("malformed identifier") else Parser.pure(s)
    }.backtrack

  private val digits: Parser[String] = Numbers.digits
  private val uintBig: Parser[BigInt] = digits.map(BigInt(_))
  private def isHex(c: Char) = isDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
  private val hexInt: Parser[BigInt] =
    (Parser.string("0x") *> Parser.charsWhile(isHex)).map(BigInt(_, 16))
  private val binInt: Parser[BigInt] =
    (Parser.string("0b") *> Parser.charsWhile(c => c == '0' || c == '1')).map(BigInt(_, 2))
  private val intLit: Parser[BigInt] =
    (Parser.char('-').string.?.with1 ~ (hexInt.backtrack | binInt.backtrack | uintBig)).map {
      (neg, n) =>
        if neg.isDefined then -n else n
    }
  private val floatLit: Parser[String] =
    (Parser.char('-').?.with1 ~ digits ~ Parser.char('.') ~ digits ~
      (Parser.charIn("eE") ~ Parser.charIn("+-").? ~ digits).?).string.backtrack

  private val escape: Parser[Char] = Parser.char('\\') *> Parser.anyChar.map {
    case 'n' => '\n'
    case 't' => '\t'
    case 'r' => '\r'
    case c   => c
  }
  private val strChar: Parser[Char] = escape | Parser.charWhere(c => c != '"' && c != '\\')
  private val textLit: Parser[String] =
    (Parser.char('"') *> strChar.rep0 <* Parser.char('"')).map(_.mkString)
  private def hexBytes(s: String): Vector[Byte] =
    s.filterNot(_.isWhitespace).grouped(2).map(h => Integer.parseInt(h, 16).toByte).toVector
  private val bytesLit: Parser[Vector[Byte]] =
    (Parser.string("h'") *> Parser.charsWhile0(_ != '\'') <* Parser.char('\'')).map(hexBytes) |
      (Parser.char('\'') *> Parser.charsWhile0(_ != '\'') <* Parser.char('\''))
        .map(_.getBytes("UTF-8").toVector)

  private val lit: Parser[Lit] =
    textLit.map(Lit.TextL(_)) | bytesLit.backtrack.map(Lit.BytesL(_)) | floatLit.map(
      Lit.FloatL(_)
    ) |
      intLit.map(Lit.IntL(_))

  // Types and groups ---------------------------------------------------------------------------
  private val bound: Parser[Int] = uintBig.map(_.toInt)
  private val occ: Parser[Occ] =
    tok("?").as(Occ.Optional) |
      tok("+").as(Occ.Plus) |
      (((bound.?.with1 <* Parser.char('*')) ~ bound.?) <* sp)
        .map((lo, hi) => Occ(lo.getOrElse(0), hi))
        .backtrack

  private val slashChoice: Parser[Unit] =
    ((Parser.char('/') ~ Parser.not(Parser.charIn("/="))).void *> sp).backtrack
  private val groupSep: Parser[Unit] = (Parser.string("//") *> sp).backtrack
  private val comma: Parser[Unit] = tok(",")

  private def choiceOf(alts: NonEmptyList[Ty]): Ty =
    if alts.tail.isEmpty then alts.head else Ty.Choice(alts.toList.toVector)

  private val ty1: Parser[Ty] = Parser.recursive[Ty] { self =>
    val ty: Parser[Ty] = self.repSep(slashChoice).map(choiceOf)
    val genericArgs: Parser[Vector[Ty]] =
      (Parser.char('<') *> sp *> ty.repSep(tok(",")) <* tok(">")).map(_.toList.toVector)
    val nameRef: Parser[Ty] =
      (ident ~ genericArgs.?).map((n, a) => Ty.Name(n, a.getOrElse(Vector.empty)))

    val bareKey: Parser[Key] =
      (ident <* sp <* Parser.char(':') <* Parser.not(Parser.char(':')) <* sp)
        .map(Key.Bare(_))
        .backtrack
    val valueKey: Parser[Key] = (lit <* sp <* Parser.char(':') <* sp).map(Key.Value(_)).backtrack
    val cut: Parser0[Boolean] = (Parser.char('^') *> sp).as(true).orElse(Parser.pure(false))
    val arrow: Parser[Boolean] = (cut.with1 <* Parser.string("=>") <* sp).backtrack
    val keyed: Parser[Entry] =
      (occ.?.with1 ~ (bareKey | valueKey) ~ ty).map { case ((o, k), t) =>
        Entry(o.getOrElse(Occ.Once), Some(k), t)
      }
    val typeKeyOrPlain: Parser[Entry] =
      (occ.?.with1 ~ self ~
        (arrow ~ ty).map(Left(_)).orElse((slashChoice *> self).rep0.map(Right(_)))).map {
        case ((o, first), Left((c, v))) =>
          Entry(o.getOrElse(Occ.Once), Some(Key.OfType(first, c)), v)
        case ((o, first), Right(rest)) =>
          val value = if rest.isEmpty then first else Ty.Choice(first +: rest.toVector)
          Entry(o.getOrElse(Occ.Once), None, value)
      }
    val entry: Parser[Entry] = keyed.backtrack | typeKeyOrPlain
    val choice: Parser0[Vector[Entry]] = (entry <* comma.?).rep0.map(_.toVector)
    val grp: Parser0[Group] =
      (choice ~ (groupSep *> choice).rep0).map((h, t) => Group(h +: t.toVector))

    val tagP: Parser[Ty] =
      (Parser.char('#') *> ((digits.? ~ (Parser.char('.') *> uintBig).?) ~
        (Parser.char('(') *> sp *> ty <* sp <* Parser.char(')')).?))
        .map { case ((major, num), inner) => Ty.Tag(major.fold(-1)(_.toInt), num, inner) }
    val arrayP = (Parser.char('[') *> sp *> grp <* sp <* Parser.char(']')).map(Ty.Array(_))
    val mapP = (Parser.char('{') *> sp *> grp <* sp <* Parser.char('}')).map(Ty.MapT(_))
    val paren: Parser[Ty] = (Parser.char('(') *> sp *> grp <* sp <* Parser.char(')')).map { g =>
      g.choices match
        case Vector(Vector(Entry(o, None, t))) if o.isOnce => t
        case _                                             => Ty.GroupT(g)
    }
    val enumP = (Parser.char('&') *> sp *> (paren | nameRef)).map(Ty.Enumeration(_))
    val unwrapP = (Parser.char('~') *> sp *> nameRef).map(Ty.Unwrap(_))
    val ty2: Parser[Ty] =
      lexeme(
        tagP | arrayP | mapP | paren | enumP | unwrapP | lit.backtrack.map(Ty.Literal(_)) | nameRef
      )
    val rangeOp: Parser[Boolean] = tok("...").as(false) | tok("..").as(true)
    val ctrlName: Parser[String] = lexeme(Parser.char('.') *> ident)
    val tail: Parser[Ty => Ty] =
      (rangeOp ~ ty2).map { (inc, hi) => (lo: Ty) => Ty.Range(lo, hi, inc) } |
        (ctrlName ~ ty2).map { (op, arg) => (b: Ty) => Ty.Ctrl(b, op, arg) }
    (ty2 ~ tail.?).map((a, t) => t.fold(a)(_(a)))
  }

  private val tyAll: Parser[Ty] = ty1.repSep(slashChoice).map(choiceOf)

  private val assign: Parser[Assign] =
    tok("//=").as(Assign.GroupChoice) | tok("/=").as(Assign.TypeChoice) | tok("=").as(Assign.Eq)
  private val params: Parser[Vector[String]] =
    (Parser.char('<') *> sp *> lexeme(ident).repSep(tok(",")) <* Parser.char('>'))
      .map(_.toList.toVector)
  private val rule: Parser[Rule] =
    (ident ~ params.? <* sp).flatMap { case (n, ps) =>
      (assign ~ tyAll).map((a, b) => Rule(n, ps.getOrElse(Vector.empty), a, b))
    }
  private val fileP: Parser0[Vector[Rule]] = sp *> rule.rep0.map(_.toVector) <* Parser.end

  private val pragmaRe = """(?m)^;#\s*(import|include)\s+(\S+)\s+as\s+(\S+)\s*$""".r

  def parse(text: String): Either[ParseFailure, File] =
    fileP.parse(text) match
      case Right((_, rules)) =>
        val pragmas =
          pragmaRe
            .findAllMatchIn(text)
            .map(m => Pragma(m.group(1), m.group(2), m.group(3)))
            .toVector
        Right(File(rules, pragmas))
      case Left(err) =>
        val off = err.failedAtOffset
        val before = text.take(off)
        val line = before.count(_ == '\n') + 1
        val col = off - (before.lastIndexOf('\n') + 1) + 1
        val near = text.slice(off, off + 40).takeWhile(_ != '\n')
        Left(ParseFailure(s"near `$near`", line, col))
