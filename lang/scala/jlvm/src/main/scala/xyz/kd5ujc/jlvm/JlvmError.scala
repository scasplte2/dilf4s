package xyz.kd5ujc.jlvm

import xyz.kd5ujc.core.canonical.{CanonicalError, CanonicalJson, DecodeFailure, Preview, ValueKind}

export xyz.kd5ujc.models.jlvm_errors.{JlvmCryptoCauseKind => CryptoCauseKind, JlvmErrorKind}

/** Native failures retain their causes; only kinds, sites and crypto cause kinds are protocol fields. */
enum JlvmError {
  case Malformed(reason: JlvmError.MalformedReason)
  case Type(op: Site, reason: JlvmError.TypeReason)
  case Arity(op: Site, expected: JlvmError.ArgumentCount, actual: Int)
  case Arithmetic(op: Site, reason: JlvmError.ArithmeticReason)
  case Domain(op: Site, reason: JlvmError.DomainReason)
  case Crypto(op: Site, reason: JlvmError.CryptoReason)
  case Limit(reason: JlvmError.LimitReason)
  case OutOfGas(required: Long, available: Long)

  def kind: JlvmErrorKind = this match {
    case Malformed(_)     => JlvmErrorKind.Malformed
    case Type(_, _)       => JlvmErrorKind.Type
    case Arity(_, _, _)   => JlvmErrorKind.Arity
    case Arithmetic(_, _) => JlvmErrorKind.Arithmetic
    case Domain(_, _)     => JlvmErrorKind.Domain
    case Crypto(_, _)     => JlvmErrorKind.Crypto
    case Limit(_)         => JlvmErrorKind.Limit
    case OutOfGas(_, _)   => JlvmErrorKind.OutOfGas
  }

  def site: Option[Site] = this match {
    case Type(op, _)                              => Some(op)
    case Arity(op, _, _)                          => Some(op)
    case Arithmetic(op, _)                        => Some(op)
    case Domain(op, _)                            => Some(op)
    case Crypto(op, _)                            => Some(op)
    case Malformed(_) | Limit(_) | OutOfGas(_, _) => None
  }

  def cause: Option[CryptoCause] = this match {
    case Crypto(_, JlvmError.CryptoReason.Refused(c, _))                                                                         => Some(c)
    case Malformed(_) | Type(_, _) | Arity(_, _, _) | Arithmetic(_, _) | Domain(_, _) | Crypto(_, _) | Limit(_) | OutOfGas(_, _) => None
  }

  def message: String = this match {
    case Malformed(r)                  => r.message
    case Type(_, r)                    => r.message
    case Arity(_, expected, actual)    => s"takes ${expected.message} arguments, got $actual"
    case Arithmetic(_, r)              => r.message
    case Domain(_, r)                  => r.message
    case Crypto(_, r)                  => r.message
    case Limit(r)                      => r.message
    case OutOfGas(required, available) => s"out of gas: $required required, $available available"
  }
}

object JlvmError {
  enum Role {
    case Argument, Key, Keys, Mode, Entries, Prefix, From, Until, Root, Leaf, Rfc6962Leaf
    case Head, OldHead, NewHead, Index, Siblings, Vkey, PublicValues, Proof, PublicKey, Alpha, A, B, Point, Scalar, Sum, Product, Pairing
    case Input(index: Int)
    case Sibling(index: Int)
    case Pair(index: Int)
    case PairG1(index: Int)
    case PairG2(index: Int)
    case HeadRoot(head: Role)

    def message: String = this match {
      case Input(i)     => s"input[$i]"
      case Sibling(i)   => s"sibling[$i]"
      case Pair(i)      => s"pair[$i]"
      case PairG1(i)    => s"pair[$i].g1"
      case PairG2(i)    => s"pair[$i].g2"
      case HeadRoot(h)  => s"${h.message}.root"
      case Argument     => "argument"
      case Key          => "key"
      case Keys         => "keys"
      case Mode         => "mode"
      case Entries      => "entries"
      case Prefix       => "prefix"
      case From         => "from"
      case Until        => "until"
      case Root         => "root"
      case Leaf         => "leaf"
      case Rfc6962Leaf  => "an rfc6962 leaf"
      case Head         => "head"
      case OldHead      => "oldHead"
      case NewHead      => "newHead"
      case Index        => "index"
      case Siblings     => "siblings"
      case Vkey         => "vkey"
      case PublicValues => "publicValues"
      case Proof        => "proof"
      case PublicKey    => "pk"
      case Alpha        => "alpha"
      case A            => "a"
      case B            => "b"
      case Point        => "point"
      case Scalar       => "scalar"
      case Sum          => "a + b"
      case Product      => "point * scalar"
      case Pairing      => "a pair"
    }
  }

  enum Construct {
    case Operation(op: Op)
    case ArrayLiteral, ObjectLiteral
    def message: String = this match {
      case Operation(op) => op.wire
      case ArrayLiteral  => "array"
      case ObjectLiteral => "object"
    }
  }

  enum ArgumentCount {
    case Exactly(count: Int)
    case Between(min: Int, max: Int)
    case AtLeast(min: Int)
    def message: String = this match {
      case Exactly(n)      => n.toString
      case Between(lo, hi) => s"$lo to $hi"
      case AtLeast(lo)     => s"at least $lo"
    }
  }

  enum OperationForm { case Strict, Lookup, ZeroKnowledge }

  enum RemovedOp(val wire: String, val hint: String) {
    case ProveDlog extends RemovedOp("prove_dlog_verify", "use a one-leaf sigma_verify over {\"type\": \"dlog\"} (docs/specs/sigma.md)")
    case ProveDhTuple
        extends RemovedOp("prove_dhtuple_verify", "use a one-leaf sigma_verify over {\"type\": \"dhtuple\"} (docs/specs/sigma.md)")
    case Mpt extends RemovedOp("mpt_verify", "use trie_verify (docs/specs/trie.md)")
    case MptPrefix extends RemovedOp("mpt_prefix_verify", "use trie_prefix_verify (docs/specs/trie.md)")
    case Smt extends RemovedOp("smt_verify", "use trie_verify (docs/specs/trie.md)")
    case Noop extends RemovedOp("noop", "omit it: it does nothing")
  }

  object RemovedOp {
    private val byWire = RemovedOp.values.map(op => op.wire -> op).toMap
    def unapply(text: String): Option[RemovedOp] = byWire.get(text)
  }

  enum CallName {
    case Operation(op: Op, name: Preview)
    case Variable
    case Removed(op: RemovedOp)
    def spelling: String = this match {
      case Operation(_, name) => name.short
      case Variable           => "var"
      case Removed(op)        => op.wire
    }
    def suggestion: String = this match {
      case Removed(op)                => s"\"${op.wire}\" is not an operation: ${op.hint}"
      case Operation(_, _) | Variable => s"write the call as {\"$spelling\": [...]}"
    }
  }

  object CallName {
    def parse(text: String): Option[CallName] = text match {
      case "var"         => Some(CallName.Variable)
      case RemovedOp(op) => Some(CallName.Removed(op))
      case name          => Ops.named(name).map(op => CallName.Operation(op, Preview.of(name)))
    }
  }

  enum MalformedReason {
    case Canonical(cause: CanonicalError)
    case Render(cause: CanonicalJson.RenderError)
    case Decode(cause: DecodeFailure)
    case NotJson, HexTextSurface, MissingSubexpression, ObjectShape, VarPathShape, VarArguments, UnquotedDefault
    case EmptyPathSegment(path: Preview)
    case EmptyBindingName(path: Preview)
    case UnspellablePath(path: Preview)
    case NullDefault
    case SpreadUnsupported(op: Op)
    case SpreadArgumentList(op: Op)
    case SpreadKind(op: Op, actual: ValueKind)
    case BindingShadow(path: Preview, name: Preview)
    case ObjectKey(cause: CanonicalError)
    case FormArguments(op: Op, expected: ArgumentCount, actual: Int)
    case BindingName(name: Preview)
    case BindingShape, BindingsShape, DuplicateBinding
    case UnboundBinding(name: Preview)
    case UnsupportedOperation(op: Op, form: OperationForm)
    case ArrayCall(call: CallName)
    case RemovedOperation(op: RemovedOp)
    case UnknownOperation(name: Preview)

    def message: String = this match {
      case Canonical(e)           => e.toString
      case Render(e)              => e.toString
      case Decode(e)              => e.message
      case NotJson                => "not JSON"
      case HexTextSurface         => "a value with 0x-hex text has no JSON surface form"
      case MissingSubexpression   => "a node without its sub-expression"
      case ObjectShape            => "object takes an object: {\"object\": {key: expression, ...}}"
      case VarPathShape           => "a var path is text, an integer, or an expression computing one"
      case VarArguments           => "var takes a path, or [path, default]"
      case UnquotedDefault        => "a var default is a literal value: quote an object or array default with {\"literal\": ...}"
      case EmptyPathSegment(p)    => s"empty segment in var path $p"
      case EmptyBindingName(p)    => s"empty binding name in $p"
      case UnspellablePath(p)     => s"the var path $p has no text form: use the exact-segment form"
      case NullDefault            => "a var default is never null: the canonical form cannot tell it from no default"
      case SpreadUnsupported(op)  => s"${op.wire} does not spread its argument"
      case SpreadArgumentList(op) => s"a spread ${op.wire} takes one expression, not an argument list"
      case SpreadKind(op, k)      => s"a spread ${op.wire} takes an array, got ${k.label}"
      case BindingShadow(p, n)    => s"$p starts with $n, which let binds: read the binding with a leading '$$'"
      case ObjectKey(e)           => s"an object key is not well-formed text: $e"
      case FormArguments(op, expected, actual) => s"${op.wire} takes ${expected.message} arguments, got $actual"
      case BindingName(n)                      => s"let cannot bind $n: names are non-empty, without '.' or a leading '$$'"
      case BindingShape                        => "a let binding is [name, expression]"
      case BindingsShape                       => "let takes [bindings, body]"
      case DuplicateBinding                    => "let binds a name twice"
      case UnboundBinding(n)                   => s"the binding $n is not bound"
      case UnsupportedOperation(op, form)      => s"${op.wire} has no $form form"
      case ArrayCall(call) =>
        s"an array may not start with \"${call.spelling}\", which reads as a call: ${call.suggestion}; quote data with literal"
      case RemovedOperation(op) => s"\"${op.wire}\" is not an operation: ${op.hint}"
      case UnknownOperation(n)  => s"unknown operation $n: write single-key data as {\"object\": {...}} or {\"literal\": {...}}"
    }
  }

  enum TypeReason {
    case Expected(expected: Vector[ValueKind], actual: ValueKind, role: Role = Role.Argument)
    case Ordering(left: ValueKind, right: ValueKind)
    case Length(actual: ValueKind)
    case TextRendering(actual: ValueKind)
    case Membership(needle: ValueKind, haystack: ValueKind)
    case ResultText(cause: CanonicalError)
    case Entry
    def message: String = this match {
      case Expected(ks, actual, role) => s"${role.message}: expected ${ks.map(_.label).mkString(" or ")}, got ${actual.label}"
      case Ordering(a, b)             => s"cannot order ${a.label} and ${b.label}"
      case TextRendering(k)           => s"cannot render ${k.label} as text"
      case Length(k)                  => s"cannot take the length of ${k.label}"
      case Membership(n, h)           => s"cannot look for ${n.label} in ${h.label}"
      case ResultText(e)              => s"the result is not well-formed text: $e"
      case Entry                      => "an entry is [key, value], with a value"
    }
  }

  enum ArithmeticReason {
    case DivisionByZero
    case ExponentRange(limit: Int)
    case NonTerminatingDecimal(numeratorBits: Int, denominatorBits: Int)
    def message: String = this match {
      case DivisionByZero              => "division by zero"
      case ExponentRange(n)            => s"the exponent must be an integer in -$n..$n"
      case NonTerminatingDecimal(n, d) => s"a rational of $n/$d bits has no finite decimal form"
    }
  }

  enum DomainReason {
    case EmptySeparator, InvalidHex, NegativeInteger, PathSegmentWidth, MerkleProfile, TrieModeShape
    case IntegerWidth(max: Long)
    case IntegerDoesNotFit(width: Long)
    case ComputedPath(cause: JlvmError)
    case TrieMode(cause: xyz.kd5ujc.accumulators.trie.TrieError)
    def message: String = this match {
      case EmptySeparator       => "the separator must not be empty"
      case InvalidHex           => "expected 0x followed by an even number of lowercase hex digits"
      case NegativeInteger      => "negative integer"
      case PathSegmentWidth     => "an integer path segment must fit in 64 bits"
      case MerkleProfile        => "the only optional profile is \"rfc6962\""
      case TrieModeShape        => "a mode is {\"fixed\": n} or {\"variable\": max}"
      case IntegerWidth(n)      => s"the width must be in 0..$n"
      case IntegerDoesNotFit(w) => s"the integer does not fit in $w bytes"
      case ComputedPath(e)      => e.message
      case TrieMode(e)          => e.toString
    }
  }

  enum CryptoReason {
    case Refused(cause: CryptoCause, role: Role = Role.Argument)
    case Unavailable, NoRecoveredKey
    case ByteWidth(role: Role, expected: Long, actual: Long)
    case SiblingCount(max: Int, actual: Int)
    case PairCount(max: Int, actual: Int)
    case PairWidth(index: Int, actual: Int)
    case MissingHeadRoot(role: Role)
    case TreeSize(role: Role)
    case TreeHead(role: Role)
    def message: String = this match {
      case Refused(e, role)           => s"${role.message}: ${e.message}"
      case Unavailable                => "not available yet"
      case NoRecoveredKey             => "the signature recovers no key (r or s out of range, high s, or r not an x-coordinate)"
      case ByteWidth(role, n, actual) => s"${role.message}: expected $n bytes, got $actual"
      case SiblingCount(n, actual)    => s"at most $n siblings, got $actual"
      case PairCount(n, actual)       => s"at most $n pairs, got $actual"
      case PairWidth(i, n)            => s"pair[$i]: expected [g1, g2], got $n elements"
      case MissingHeadRoot(role)      => s"${role.message}.root"
      case TreeSize(role)             => s"${role.message}.size is a tree size"
      case TreeHead(role)             => s"${role.message} is a tree head {root, size}"
    }
  }

  enum LimitReason {
    case NumberBits(op: Op, max: Int)
    case ValueWeight(where: Construct, max: Long)
    case Canonical(where: Construct, cause: CanonicalError)
    case ValueEncoding(op: Op, max: Long)
    case NegativeGas(requested: Long)
    case GasRange(requested: Long, max: Long)
    case SurfaceDepth(actual: Int, max: Int)
    case ProgramSize(max: Long)
    case ProgramDepth(max: Int)
    case CanonicalDepth(max: Long)
    case CanonicalWeight(max: Long)
    case ProgramEncoding(max: Long)
    def message: String = this match {
      case NumberBits(op, n)    => s"${op.wire}: a number exceeds $n bits"
      case ValueWeight(at, n)   => s"${at.message}: the result exceeds weight $n"
      case Canonical(at, e)     => s"${at.message}: $e"
      case ValueEncoding(op, n) => s"${op.wire}: a value's encoding is over $n bytes"
      case NegativeGas(_)       => "a gas limit is negative"
      case GasRange(n, _)       => s"a gas limit of ${64 - java.lang.Long.numberOfLeadingZeros(n)} bits is over 2^53 - 1"
      case SurfaceDepth(n, max) => s"the program's JSON surface nests $n deep, deeper than $max"
      case ProgramSize(n)       => s"the program is larger than $n"
      case ProgramDepth(n)      => s"the program nests deeper than $n"
      case CanonicalDepth(n)    => s"the program nests deeper than its canonical form allows ($n levels)"
      case CanonicalWeight(n)   => s"the form weighs more than any program's, $n"
      case ProgramEncoding(n)   => s"the program's encoding is over $n bytes"
    }
  }
}
