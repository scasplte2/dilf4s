package xyz.kd5ujc.crypto.sigma

import xyz.kd5ujc.core.canonical.ValueKind

export xyz.kd5ujc.models.crypto_errors.SigmaErrorKind

/** Malformed sigma input; a well-formed proof that fails remains Right(false). */
enum SigmaError {
  case InvalidProposition(reason: SigmaError.PropositionReason)
  case InvalidPoint(role: SigmaError.PointRole, cause: Secp256k1Group.DecodeError)
  case ProofLength(expected: Long, actual: Long)
  case ResponseOutOfRange

  def kind: SigmaErrorKind = this match {
    case InvalidProposition(_) => SigmaErrorKind.InvalidProposition
    case InvalidPoint(_, _)    => SigmaErrorKind.InvalidPoint
    case ProofLength(_, _)     => SigmaErrorKind.ProofLength
    case ResponseOutOfRange    => SigmaErrorKind.ResponseOutOfRange
  }

  def message: String = this match {
    case InvalidProposition(reason)    => reason.message
    case InvalidPoint(role, cause)     => s"${role.wire}: ${cause.message}"
    case ProofLength(expected, actual) => s"a proof of this proposition is $expected bytes, got $actual"
    case ResponseOutOfRange            => "a response must be below the group order"
  }
}

object SigmaError {
  enum PointRole(val wire: String) {
    case PublicKey extends PointRole("pk")
    case G extends PointRole("g")
    case H extends PointRole("h")
    case U extends PointRole("u")
    case V extends PointRole("v")
  }

  enum PropositionKind(val wire: String, val fields: Vector[String]) {
    case Dlog extends PropositionKind("dlog", Vector("pk"))
    case DhTuple extends PropositionKind("dhtuple", Vector("g", "h", "u", "v"))
    case And extends PropositionKind("and", Vector("children"))
    case Or extends PropositionKind("or", Vector("children"))
    case Threshold extends PropositionKind("threshold", Vector("k", "children"))
  }

  enum PropositionReason {
    case Identity(role: PointRole)
    case Depth(max: Int)
    case Leaves(max: Int)
    case Threshold(k: Int, children: Int)
    case EmptyChildren
    case Children(max: Int)
    case Weight(max: Long)
    case PointBytes(role: PointRole, actual: Option[ValueKind])
    case ChildrenArray, ThresholdInteger, UnknownType, ExpectedObject
    case Fields(kind: PropositionKind, actualCount: Int)
    case ChildIndex(cause: ChildPoint.Error)

    def message: String = this match {
      case Identity(role)      => s"${role.wire} is the identity, for which anyone can prove knowledge (w = 0)"
      case Depth(n)            => s"a proposition nests at most $n deep"
      case Leaves(n)           => s"a proposition has at most $n leaves"
      case Threshold(k, n)     => s"a threshold needs 1 <= k <= $n children, got k = $k"
      case EmptyChildren       => "a connective needs at least one child: an empty one holds for anyone"
      case Children(n)         => s"a connective has at most $n children"
      case Weight(n)           => s"a proposition weighs at most $n"
      case PointBytes(role, _) => s"${role.wire} must be bytes"
      case ChildrenArray       => "children must be an array"
      case ThresholdInteger    => "k must be an integer"
      case UnknownType         => "a proposition's type is dlog, dhtuple, and, or or threshold"
      case ExpectedObject      => "a proposition is an object"
      case Fields(kind, _)     => s"${kind.wire} takes exactly the fields ${kind.fields.mkString(", ")}"
      case ChildIndex(e)       => e.message
    }
  }
}
