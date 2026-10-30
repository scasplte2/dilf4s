// SPDX-License-Identifier: Apache-2.0
package xyz.kd5ujc.modelsgen

import cats.syntax.all._

import xyz.kd5ujc.modelsgen.Ast.{Assign, Occ}

/** A rule, qualified by the module it lives in (`conway:header_body`). */
final case class QName(module: String, name: String):
  override def toString: String = s"$module:$name"

enum PrimKind:
  case Uint, Nint, Int, Bytes, Text, Bool, Nil, Any, Float

/** A resolved type: names are qualified, generics are expanded, groups spliced into entries. */
enum RT:
  case Prim(kind: PrimKind)
  case LitInt(value: BigInt)
  case LitText(value: String)
  case LitBool(value: Boolean)
  case LitBytes(value: Vector[Byte])

  /** An integer in `lo..hi` (inclusive); from `uint .size n` or a `lo..hi` range. */
  case IntRange(lo: BigInt, hi: BigInt)
  case SizedBytes(lo: Int, hi: Int)
  case SizedText(lo: Int, hi: Int)
  case Ref(target: QName)
  case Choice(alts: Vector[RT])
  case Arr(entries: Vector[REntry])
  case MapT(entries: Vector[REntry])
  case Tagged(tag: BigInt, inner: RT)

  /** `bytes .cbor inner`: a byte string holding the encoding of `inner`. */
  case CborOf(inner: RT)

  /** A reference to a group rule, spliced into the enclosing array or map when encoded. */
  case GroupRef(target: QName)
  case Unsupported(feature: UnsupportedFeature)

enum RKey:
  case IntKey(value: BigInt)
  case TextKey(value: String)
  case OfType(ty: RT, cut: Boolean)

/**
 * An array or map entry. `label` is the CDDL member name (a comment in arrays, the key in maps).
 */
final case class REntry(occ: Occ, label: Option[String], key: Option[RKey], value: RT)

enum RuleBody:
  case Type(ty: RT)
  case GroupBody(entries: Vector[REntry])

final case class RuleDef(name: QName, body: RuleBody)

/** The rules of one module: the files configured for it, later files overriding earlier ones. */
final case class Module(name: String, rules: Map[String, (Vector[Ast.Rule], Scope)])

/** Name resolution scope: the module of the defining file and its `;# import … as alias` table. */
final case class Scope(module: String, aliases: Map[String, String])

final class Schema(val modules: Map[String, Module]):
  import GenResult.*

  def modulesNamed: Set[String] = modules.keySet

  def lookup(q: QName): Option[(Vector[Ast.Rule], Scope)] =
    modules.get(q.module).flatMap(_.rules.get(q.name))

  def rule(q: QName): Either[GenError, RuleDef] = resolveRule(q).value.value

  private def resolveRule(q: QName): GenResult[RuleDef] = deferred {
    lookup(q) match
      case None => failure(GenError.UnresolvedName(q.module, q.name))
      case Some((defs, _)) if defs.headOption.exists(_.params.nonEmpty) =>
        failure(GenError.Unsupported(q.toString, UnsupportedFeature.UnappliedGeneric))
      case Some((defs, scope)) =>
        val isGroup =
          defs.exists(r => r.assign == Assign.GroupChoice || isGroupBody(r.body))
        if isGroup then
          val alts = defs.flatMap { r =>
            r.body match
              case Ast.Ty.GroupT(g) => g.choices
              case other            => Vector(Vector(Ast.Entry(Occ.Once, None, other)))
          }
          alts match
            case Vector(only) =>
              entries(only, scope, Map.empty, q.toString, inMap = false)
                .map(es => RuleDef(q, RuleBody.GroupBody(es)))
            case _ => failure(GenError.Unsupported(q.toString, UnsupportedFeature.GroupAlternatives))
        else
          defs
            .traverse(r => ty(r.body, scope, Map.empty, q.toString))
            .map(tys => RuleDef(q, RuleBody.Type(tys match { case Vector(one) => one; case _ => flattenChoice(tys) })))
  }

  private def flattenChoice(alts: Vector[RT]): RT =
    val flat = alts.flatMap {
      case RT.Choice(xs) => xs
      case other         => Vector(other)
    }
    flat match
      case Vector(one) => one
      case _           => RT.Choice(flat)

  private def isGroupBody(t: Ast.Ty): Boolean = t match
    case _: Ast.Ty.GroupT => true
    case _                => false

  // The standard prelude of RFC 8610 appendix D (what cuddle's postlude also defines). `time`, `number`, `decfrac`
  // and `bigfloat` are left out: they need floats or tag content this generator does not model.
  private val tagged: Map[String, RT] =
    val bytes = RT.Prim(PrimKind.Bytes)
    val text = RT.Prim(PrimKind.Text)
    val any = RT.Prim(PrimKind.Any)
    val biguint = RT.Tagged(2, bytes)
    val bignint = RT.Tagged(3, bytes)
    val bigint = RT.Choice(Vector(biguint, bignint))
    Map(
      "tdate"        -> RT.Tagged(0, text),
      "biguint"      -> biguint,
      "bignint"      -> bignint,
      "bigint"       -> bigint,
      "integer"      -> RT.Choice(Vector(RT.Prim(PrimKind.Int), biguint, bignint)),
      "unsigned"     -> RT.Choice(Vector(RT.Prim(PrimKind.Uint), biguint)),
      "eb64url"      -> RT.Tagged(21, any),
      "eb64legacy"   -> RT.Tagged(22, any),
      "eb16"         -> RT.Tagged(23, any),
      "encoded-cbor" -> RT.Tagged(24, bytes),
      "uri"          -> RT.Tagged(32, text),
      "b64url"       -> RT.Tagged(33, text),
      "b64legacy"    -> RT.Tagged(34, text),
      "regexp"       -> RT.Tagged(35, text),
      "mime-message" -> RT.Tagged(36, text),
      "cbor-any"     -> RT.Tagged(55799, any)
    )

  private val prelude: Map[String, RT] = tagged ++ Map(
    "uint"    -> RT.Prim(PrimKind.Uint),
    "nint"    -> RT.Prim(PrimKind.Nint),
    "int"     -> RT.Prim(PrimKind.Int),
    "bytes"   -> RT.Prim(PrimKind.Bytes),
    "bstr"    -> RT.Prim(PrimKind.Bytes),
    "text"    -> RT.Prim(PrimKind.Text),
    "tstr"    -> RT.Prim(PrimKind.Text),
    "bool"    -> RT.Prim(PrimKind.Bool),
    "true"    -> RT.LitBool(true),
    "false"   -> RT.LitBool(false),
    "nil"     -> RT.Prim(PrimKind.Nil),
    "null"    -> RT.Prim(PrimKind.Nil),
    "any"     -> RT.Prim(PrimKind.Any),
    "float"   -> RT.Prim(PrimKind.Float),
    "float16" -> RT.Prim(PrimKind.Float),
    "float32" -> RT.Prim(PrimKind.Float),
    "float64" -> RT.Prim(PrimKind.Float)
  )

  /**
   * Find the rule an identifier names from `scope`: a local rule, or `alias.rule` through an
   * import.
   */
  private def locate(id: String, scope: Scope): GenResult[Option[QName]] =
    val local = QName(scope.module, id)
    if lookup(local).isDefined then success(Some(local))
    else
      val dot = id.indexOf('.')
      if dot < 0 then success(None)
      else
        scope.aliases.get(id.substring(0, dot)) match
          case None => success(None)
          case Some(m) =>
            val q = QName(m, id.substring(dot + 1))
            if lookup(q).isDefined then success(Some(q))
            else if modules.contains(m) then failure(GenError.UnresolvedName(m, q.name))
            else failure(GenError.UnknownModule(m, scope.module))

  private def intOf(
    t:     Ast.Ty,
    scope: Scope,
    env:   Map[String, RT],
    ctx:   String,
    seen:  Set[QName] = Set.empty
  ): GenResult[BigInt] = deferred {
    t match
      case Ast.Ty.Literal(Ast.Lit.IntL(n)) => success(n)
      case Ast.Ty.Name(id, Vector()) if !env.contains(id) =>
        locate(id, scope).flatMap {
          case Some(q) if seen(q) => failure(GenError.CyclicExpansion(q))
          case Some(q) =>
            lookup(q) match
              case Some((Vector(r), defScope)) if r.params.isEmpty =>
                intOf(r.body, defScope, Map.empty, ctx, seen + q)
              case _ => failure(GenError.Unsupported(ctx, UnsupportedFeature.NonLiteralBound(t)))
          case None => failure(GenError.Unsupported(ctx, UnsupportedFeature.NonLiteralBound(t)))
        }
      case _ => failure(GenError.Unsupported(ctx, UnsupportedFeature.NonLiteralBound(t)))
  }

  private def ty(
    t:         Ast.Ty,
    scope:     Scope,
    env:       Map[String, RT],
    ctx:       String,
    expanding: Set[QName] = Set.empty
  ): GenResult[RT] = deferred {
    def resolve(value: Ast.Ty): GenResult[RT] = ty(value, scope, env, ctx, expanding)
    t match
      case Ast.Ty.Literal(l) =>
        success(l match
          case Ast.Lit.IntL(n)    => RT.LitInt(n)
          case Ast.Lit.TextL(s)   => RT.LitText(s)
          case Ast.Lit.BytesL(bs) => RT.LitBytes(bs)
          case Ast.Lit.FloatL(s)  => RT.Unsupported(UnsupportedFeature.FloatLiteral(s))
        )
      case Ast.Ty.Name(id, args) =>
        env.get(id) match
          case Some(bound) => success(bound)
          case None =>
            locate(id, scope).flatMap {
              case Some(q) =>
                lookup(q) match
                  case None => failure(GenError.UnresolvedName(q.module, q.name))
                  case Some((defs, defScope)) =>
                    val params = defs.headOption.fold(Vector.empty[String])(_.params)
                    if params.size != args.size then failure(GenError.ArityMismatch(q.toString, params.size, args.size))
                    else if params.isEmpty then success(RT.Ref(q))
                    else if expanding(q) then failure(GenError.CyclicExpansion(q))
                    else
                      for
                        resolved <- args.traverse(resolve)
                        bound = params.zip(resolved).toMap
                        tys <- defs.traverse(r => ty(r.body, defScope, bound, q.toString, expanding + q))
                      yield flattenChoice(tys)
              case None =>
                fromEither(prelude.get(id).toRight(GenError.UnresolvedName(scope.module, id)))
            }
      case Ast.Ty.Range(lo, hi, inclusive) =>
        (intOf(lo, scope, env, ctx), intOf(hi, scope, env, ctx))
          .mapN((a, b) => RT.IntRange(a, if inclusive then b else b - 1))
      case Ast.Ty.Ctrl(base, op, arg) => control(base, op, arg, scope, env, ctx, expanding)
      case Ast.Ty.Choice(alts)        => alts.traverse(resolve).map(flattenChoice)
      case Ast.Ty.Array(g)            => composite(g, scope, env, ctx, inMap = false, expanding)
      case Ast.Ty.MapT(g)             => composite(g, scope, env, ctx, inMap = true, expanding)
      case Ast.Ty.Tag(major, number, inner) =>
        (major, number, inner) match
          case (6, Some(n), Some(i)) => resolve(i).map(RT.Tagged(n, _))
          case (6, Some(n), None)    => success(RT.Tagged(n, RT.Prim(PrimKind.Any)))
          case (-1, None, None)      => success(RT.Prim(PrimKind.Any))
          case _                     => success(RT.Unsupported(UnsupportedFeature.TagForm(major, number)))
      case Ast.Ty.GroupT(_) => success(RT.Unsupported(UnsupportedFeature.GroupInTypePosition))
      case Ast.Ty.Enumeration(of) =>
        of match
          case Ast.Ty.GroupT(Ast.Group(Vector(only))) =>
            only.traverse(e => resolve(e.value)).map(RT.Choice(_))
          case other => success(RT.Unsupported(UnsupportedFeature.Enumeration(other)))
      case Ast.Ty.Unwrap(_) => success(RT.Unsupported(UnsupportedFeature.Unwrap))
  }

  private def control(
    base:      Ast.Ty,
    op:        String,
    arg:       Ast.Ty,
    scope:     Scope,
    env:       Map[String, RT],
    ctx:       String,
    expanding: Set[QName]
  ): GenResult[RT] =
    def underlying(value: RT, seen: Set[QName]): GenResult[RT] = deferred {
      value match
        case RT.Ref(q) if seen(q) => failure(GenError.CyclicExpansion(q))
        case RT.Ref(q) =>
          resolveRule(q).flatMap {
            case RuleDef(_, RuleBody.Type(t)) => underlying(t, seen + q)
            case _                            => success(value)
          }
        case other => success(other)
    }
    def constrained(lo: BigInt, hi: BigInt, bound: BigInt): RT =
      val ranges = op match
        case "gt" => Vector((lo.max(bound + 1), hi))
        case "ge" => Vector((lo.max(bound), hi))
        case "lt" => Vector((lo, hi.min(bound - 1)))
        case "le" => Vector((lo, hi.min(bound)))
        case "ne" => Vector((lo, hi.min(bound - 1)), (lo.max(bound + 1), hi))
        case _    => Vector.empty
      val valid = ranges.collect { case (a, b) if a <= b => RT.IntRange(a, b) }
      if valid.isEmpty then RT.IntRange(1, 0) else flattenChoice(valid)
    ty(base, scope, env, ctx, expanding).flatMap { resolved =>
      val comparison = Set("gt", "ge", "lt", "le", "ne")(op)
      (if comparison then underlying(resolved, Set.empty) else success(resolved)).flatMap { b =>
        (b, op) match
          case (RT.Prim(PrimKind.Uint), "size") =>
            arg match
              case Ast.Ty.Literal(Ast.Lit.IntL(n)) if n >= 0 && n <= Int.MaxValue / 8 =>
                success(RT.IntRange(0, (BigInt(1) << (8 * n.toInt)) - 1))
              case _ => success(RT.Unsupported(UnsupportedFeature.UnsignedSize(arg)))
          case (RT.Prim(PrimKind.Bytes), "size") =>
            success(
              sizes(arg).fold(RT.Unsupported(UnsupportedFeature.SizeBound(arg)))(
                RT.SizedBytes(_, _)
              )
            )
          case (RT.Prim(PrimKind.Text), "size") =>
            success(
              sizes(arg).fold(RT.Unsupported(UnsupportedFeature.SizeBound(arg)))(RT.SizedText(_, _))
            )
          case (RT.Prim(PrimKind.Bytes), "cbor") =>
            ty(arg, scope, env, ctx, expanding).map(RT.CborOf(_))
          case (RT.Prim(PrimKind.Uint), _) if comparison =>
            intOf(arg, scope, env, ctx).map(constrained(0, (BigInt(1) << 64) - 1, _))
          case (RT.IntRange(lo, hi), _) if comparison =>
            intOf(arg, scope, env, ctx).map(constrained(lo, hi, _))
          case (_, "default") => success(b)
          case _              => success(RT.Unsupported(UnsupportedFeature.Control(op)))
      }
    }

  private def sizes(arg: Ast.Ty): Option[(Int, Int)] = arg match
    case Ast.Ty.Literal(Ast.Lit.IntL(n)) if n.isValidInt && n >= 0 => Some((n.toInt, n.toInt))
    case Ast.Ty.Range(Ast.Ty.Literal(Ast.Lit.IntL(a)), Ast.Ty.Literal(Ast.Lit.IntL(b)), inc)
        if a.isValidInt && b.isValidInt && a >= 0 && b >= a && (inc || b > a) =>
      Some((a.toInt, if inc then b.toInt else b.toInt - 1))
    case _ => None

  private def composite(
    g:         Ast.Group,
    scope:     Scope,
    env:       Map[String, RT],
    ctx:       String,
    inMap:     Boolean,
    expanding: Set[QName]
  ): GenResult[RT] =
    g.choices
      .traverse(es =>
        entries(es, scope, env, ctx, inMap, expanding)
          .map(xs => if inMap then RT.MapT(xs) else RT.Arr(xs))
      )
      .map {
        case Vector(one) => one
        case alts        => RT.Choice(alts)
      }

  private def entries(
    es:        Vector[Ast.Entry],
    scope:     Scope,
    env:       Map[String, RT],
    ctx:       String,
    inMap:     Boolean,
    expanding: Set[QName] = Set.empty
  ): GenResult[Vector[REntry]] = deferred {
    es.flatTraverse { e =>
      e.value match
        case Ast.Ty.GroupT(Ast.Group(Vector(only))) if e.key.isEmpty =>
          entries(only, scope, env, ctx, inMap, expanding)
        case Ast.Ty.Name(id, Vector()) if e.key.isEmpty && !env.contains(id) =>
          locate(id, scope).flatMap { located =>
            val group = located.filter(q =>
              lookup(q).exists { (defs, _) =>
                defs.exists(r => r.assign == Assign.GroupChoice || isGroupBody(r.body))
              }
            )
            group match
              case Some(q) => success(Vector(REntry(e.occ, None, None, RT.GroupRef(q))))
              case None    => entry(e, scope, env, ctx, inMap, expanding).map(Vector(_))
          }
        case _ => entry(e, scope, env, ctx, inMap, expanding).map(Vector(_))
    }
  }

  private def entry(
    e:         Ast.Entry,
    scope:     Scope,
    env:       Map[String, RT],
    ctx:       String,
    inMap:     Boolean,
    expanding: Set[QName]
  ): GenResult[REntry] =
    val key: GenResult[(Option[String], Option[RKey])] = e.key match
      case None => success((None, None))
      case Some(Ast.Key.Bare(n)) =>
        success((Some(n), if inMap then Some(RKey.TextKey(n)) else None))
      case Some(Ast.Key.Value(Ast.Lit.IntL(n))) =>
        success((None, if inMap then Some(RKey.IntKey(n)) else None))
      case Some(Ast.Key.Value(Ast.Lit.TextL(s))) =>
        success((None, if inMap then Some(RKey.TextKey(s)) else None))
      case Some(Ast.Key.Value(lit)) =>
        failure(GenError.Unsupported(ctx, UnsupportedFeature.LiteralKey(lit)))
      case Some(Ast.Key.OfType(kt, cut)) =>
        if inMap then ty(kt, scope, env, ctx, expanding).map(k => (None, Some(RKey.OfType(k, cut))))
        else success((None, None))
    for
      v <- ty(e.value, scope, env, ctx, expanding)
      k <- key
    yield REntry(e.occ, k._1, k._2, v)

object Schema:
  def build(modules: Vector[(String, Vector[Ast.File])]): Schema =
    val built = modules.map { (name, files) =>
      val rules = files.foldLeft(Map.empty[String, (Vector[Ast.Rule], Scope)]) { (rules, f) =>
        val scope = Scope(name, f.pragmas.map(p => p.alias -> p.module).toMap)
        val perFile = f.rules.foldLeft(Map.empty[String, Vector[Ast.Rule]]) { (acc, r) =>
          val prior = acc.getOrElse(r.name, Vector.empty)
          acc.updated(r.name, if r.assign == Assign.Eq then Vector(r) else prior :+ r)
        }
        perFile.foldLeft(rules) {
          case (acc, (n, rs)) =>
            val merged =
              if rs.forall(_.assign != Assign.Eq) then acc.get(n).fold(Vector.empty[Ast.Rule])(_._1) ++ rs
              else rs
            acc.updated(n, (merged, scope))
        }
      }
      name -> Module(name, rules)
    }
    new Schema(built.toMap)
