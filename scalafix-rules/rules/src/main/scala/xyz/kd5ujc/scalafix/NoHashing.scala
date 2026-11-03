package xyz.kd5ujc.scalafix

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Paths

import scala.annotation.tailrec
import scala.meta._
import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.{AbsolutePath, Classpath}
import scala.util.Using

import scalafix.lint.LintSeverity
import scalafix.v1._

/**
 * Refuses every way core's sources could hash a key, by the symbols and types the compiler resolved (SemanticDB), not
 * by the words written:
 *   - a reference to, or a value, parameter or method of, a type whose class is a `scala.collection.Map` / `Set` or a
 *     `java.util.Map` / `Set` / `Dictionary` and not a sorted one (`HashMap`, `VectorMap`, `TreeSeqMap`, `Hashtable`,
 *     `Map(...)`, `toMap`, `groupBy`, `unsorted`, ...);
 *   - a collection method that hashes its elements internally though its result is a sequence (`distinct`, `diff`,
 *     `combinations`, ...), resolved to the Scala collections, so a method of the same name elsewhere is not one;
 *   - `hashCode`, `##`, `String.intern` and the hashing utilities;
 *   - a runtime type test or cast to a collection type, which would recover a canonical map's tree.
 *
 * A Scala 3 build's SemanticDB describes the project's own symbols only, and from Scala 3.8 its Scala library carries
 * TASTy rather than Scala 2 signatures: the library's and the JDK's symbols come from a symbol table over a Scala 2.13
 * build of the same library (located when this rule is built) and the JDK. If that table cannot be read, every file is
 * refused, rather than passed unchecked.
 */
final class NoHashing extends SemanticRule("NoHashing") {

  import NoHashing._

  override def description: String = "core never hashes: keys are compared through a KeyOrder"

  override def fix(implicit doc: SemanticDocument): Patch =
    library.fold(Patch.lint(Finding(doc.tree.pos, "NoHashing cannot read the Scala library's symbols, so cannot check this file")))(_ =>
      check
    )

  private def check(implicit doc: SemanticDocument): Patch =
    doc.tree.collect {
      case p: Pat.Typed => typeTest(p.rhs, p.pos).toList
      case Term.ApplyType.After_4_6_0(Term.Select(_, n @ Term.Name("isInstanceOf" | "asInstanceOf")), targs) =>
        targs.values.headOption.flatMap(typeTest(_, n.pos)).toList
      case n: Term.Name => term(n).map(finding(n.pos, _)).toList
      case n: Type.Name => typeName(n).map(finding(n.pos, _)).toList
    }.flatten
      .groupBy(_.position.start)
      .values
      .flatMap(_.headOption)
      .map(Patch.lint)
      .asPatch

  // ---- what a symbol is

  private def lookup(sym: String)(implicit doc: SemanticDocument): Option[Info] =
    Symbol(sym).info.map(fromDocument).orElse(library.flatMap(_.info(sym)).map(fromLibrary))

  /** The class symbols `sym` is or extends, itself included, through parents, aliases, bounds and companions. */
  private def ancestors(sym: String)(implicit doc: SemanticDocument): Set[String] = {
    @tailrec
    def go(todo: List[String], seen: Set[String]): Set[String] =
      todo match {
        case Nil                           => seen
        case s :: rest if seen.contains(s) => go(rest, seen)
        case s :: rest                     => go(lookup(s).toList.flatMap(i => i.parents ++ companion(s, i)) ++ rest, seen + s)
      }
    go(List(sym), Set.empty)
  }

  private def hashed(sym: String)(implicit doc: SemanticDocument): Boolean = {
    val all = ancestors(sym)
    all.exists(Unsorted) && !all.exists(Sorted)
  }

  // ---- references

  private def term(n: Term.Name)(implicit doc: SemanticDocument): Option[String] = {
    val sym = n.symbol.value
    val name = n.value
    if (n.symbol.isNone) None
    else if (name == "hashCode" || name == "##" || name == "anyHash") Some(s"$name hashes")
    else if (sym.startsWith("scala/util/hashing/") || sym == "java/util/Objects#hash().") Some(s"$name is a hash function")
    else if (sym == "java/lang/String#intern().") Some("intern puts the text in the JVM's hashed string table")
    else if (HashingMethods(name) && fromCollections(sym)) Some(s"$name counts its elements in a hash map")
    else
      lookup(sym).flatMap { i =>
        if (i.result.exists(hashed)) Some(s"$name is or returns a hashed collection")
        else if (i.result.isEmpty && hashed(sym)) Some(s"$name is a hashed collection")
        else None
      }
  }

  private def typeName(n: Type.Name)(implicit doc: SemanticDocument): Option[String] =
    Option.when(!n.symbol.isNone && hashed(n.symbol.value))(s"${n.value} is a hashed collection")

  // ---- runtime type tests

  private def typeTest(target: Type, pos: Position)(implicit doc: SemanticDocument): Option[Finding] = {
    val head = target match {
      case Type.Apply.After_4_6_0(t, _) => t
      case t                            => t
    }
    val sym = head.symbol
    Option.when(!sym.isNone && ancestors(sym.value).exists(Collection))(
      finding(pos, s"a type test against ${head.syntax} would recover a collection by its runtime type")
    )
  }
}

object NoHashing {

  private sealed trait Kind
  private case object ObjectKind extends Kind
  private case object OtherKind extends Kind

  /** A symbol as the rule needs it: whether it is an object, the heads of its parents or bounds, and of its type. */
  private final case class Info(kind: Kind, parents: List[String], result: List[String])

  private val Unsorted = Set("scala/collection/Map#", "scala/collection/Set#", "java/util/Map#", "java/util/Set#", "java/util/Dictionary#")
  private val Sorted = Set("scala/collection/SortedMap#", "scala/collection/SortedSet#", "java/util/SortedMap#", "java/util/SortedSet#")
  private val Collection = Set("scala/collection/IterableOnce#", "java/util/Collection#", "java/util/Map#")
  private val HashingMethods = Set("distinct", "distinctBy", "diff", "intersect", "combinations", "permutations", "occCounts")

  private def finding(pos: Position, message: String): Finding =
    Finding(pos, s"$message: core never hashes; compare keys through a KeyOrder")

  /** An object stands for its class: `HashMap.` for `HashMap#`. */
  private def companion(sym: String, i: Info): List[String] =
    if (i.kind == ObjectKind) List(sym.dropRight(1) + "#") else Nil

  private def fromCollections(sym: String): Boolean =
    sym.startsWith("scala/collection/") || sym.startsWith("scala/runtime/") || sym.startsWith("scala/Predef") ||
    sym.startsWith("java/lang/String#")

  /**
   * A Scala 2.13 build of the Scala library, whose class files carry the signatures this table reads, and the JDK: the
   * symbols a Scala 3 SemanticDB leaves out. The build writes the library's path into this rule's resources; a table
   * that does not know `HashMap` is no table.
   */
  private lazy val library: Option[GlobalSymbolTable] =
    Option(getClass.getResourceAsStream("/xyz/kd5ujc/scalafix/scala-library.path"))
      .flatMap(in => Using(in)(s => new String(s.readAllBytes, UTF_8).trim).toOption)
      .map(path => GlobalSymbolTable(Classpath(AbsolutePath(Paths.get(path))), includeJdk = true))
      .filter(_.info("scala/collection/immutable/HashMap#").isDefined)

  private def fromDocument(i: SymbolInformation): Info = {
    val kind = if (i.isObject) ObjectKind else OtherKind
    i.signature match {
      case ClassSignature(_, ps, _, _) => Info(kind, ps.flatMap(heads), Nil)
      case TypeSignature(_, lo, hi)    => Info(kind, heads(lo) ++ heads(hi), Nil)
      case MethodSignature(_, _, ret)  => Info(kind, Nil, heads(ret))
      case ValueSignature(t)           => Info(kind, Nil, heads(t))
      case _                           => Info(kind, Nil, Nil)
    }
  }

  private def heads(t: SemanticType): List[String] = t match {
    case TypeRef(_, sym, _)    => List(sym.value)
    case SingleType(_, sym)    => List(sym.value)
    case AnnotatedType(_, u)   => heads(u)
    case ExistentialType(u, _) => heads(u)
    case UniversalType(_, u)   => heads(u)
    case ByNameType(u)         => heads(u)
    case RepeatedType(u)       => heads(u)
    case WithType(us)          => us.flatMap(heads)
    case IntersectionType(us)  => us.flatMap(heads)
    case UnionType(us)         => us.flatMap(heads)
    case _                     => Nil
  }

  private def fromLibrary(i: s.SymbolInformation): Info = {
    val kind = if (i.kind == s.SymbolInformation.Kind.OBJECT) ObjectKind else OtherKind
    i.signature match {
      case c: s.ClassSignature  => Info(kind, c.parents.toList.flatMap(protoHeads), Nil)
      case t: s.TypeSignature   => Info(kind, protoHeads(t.lowerBound) ++ protoHeads(t.upperBound), Nil)
      case m: s.MethodSignature => Info(kind, Nil, protoHeads(m.returnType))
      case v: s.ValueSignature  => Info(kind, Nil, protoHeads(v.tpe))
      case _                    => Info(kind, Nil, Nil)
    }
  }

  private def protoHeads(t: s.Type): List[String] = t match {
    case r: s.TypeRef          => List(r.symbol)
    case r: s.SingleType       => List(r.symbol)
    case a: s.AnnotatedType    => protoHeads(a.tpe)
    case e: s.ExistentialType  => protoHeads(e.tpe)
    case u: s.UniversalType    => protoHeads(u.tpe)
    case b: s.ByNameType       => protoHeads(b.tpe)
    case r: s.RepeatedType     => protoHeads(r.tpe)
    case w: s.WithType         => w.types.toList.flatMap(protoHeads)
    case i: s.IntersectionType => i.types.toList.flatMap(protoHeads)
    case u: s.UnionType        => u.types.toList.flatMap(protoHeads)
    case _                     => Nil
  }
}

/** One way a source hashes, where it does it. */
final case class Finding(position: Position, message: String) extends Diagnostic {
  override def severity: LintSeverity = LintSeverity.Error
}
