package xyz.kd5ujc.core.canonical

enum ValueKind(val label: String) {
  case Null extends ValueKind("null")
  case Bool extends ValueKind("bool")
  case Int extends ValueKind("int")
  case Rational extends ValueKind("rational")
  case Text extends ValueKind("text")
  case Bytes extends ValueKind("bytes")
  case Array extends ValueKind("array")
  case Map extends ValueKind("map")
}

object ValueKind {
  def of(value: CanonicalValue): ValueKind = value match {
    case CanonicalValue.CNull        => Null
    case CanonicalValue.CBool(_)     => Bool
    case CanonicalValue.CInt(_)      => Int
    case CanonicalValue.CRational(_) => Rational
    case CanonicalValue.CText(_)     => Text
    case CanonicalValue.CBytes(_)    => Bytes
    case CanonicalValue.CArray(_)    => Array
    case CanonicalValue.CMap(_)      => Map
  }
}

enum DecodeReason {
  case ExpectedType(expected: Vector[ValueKind], actual: ValueKind)
  case IntegerRange(bits: Int, lower: Option[BigInt], upper: Option[BigInt])
  case ByteLength(actual: Long, lower: Long, upper: Option[Long])
  case TextLength(actual: Long, lower: Int, upper: Option[Int])
  case ArrayLength(actual: Int, lower: Int, upper: Option[Int])
  case MissingKey(key: Preview)
  case UnknownKeys(count: Int, first: Preview)
  case IntegerLiteral(expected: BigInt, actualBits: Int)
  case TextLiteral(expected: Preview, actual: Preview)
  case PatternMismatch(pattern: String, actual: Preview)
  case UnknownTextAlternative(actual: Preview)
  case NoMapAlternative(keys: Int)
  case UnknownEnumInteger(enumName: String, actualBits: Int)
  case Canonical(cause: CanonicalError)

  def message: String = this match {
    case ExpectedType(expected, actual)   => s"expected ${expected.map(_.label).mkString(" or ")}, got ${actual.label}"
    case IntegerRange(bits, lo, hi)       => s"integer of $bits bits outside ${lo.fold("")(_.toString)}..${hi.fold("")(_.toString)}"
    case ByteLength(n, lo, hi)            => s"byte string of $n bytes, expected $lo..${hi.fold("")(_.toString)}"
    case TextLength(n, lo, hi)            => s"text of $n UTF-8 bytes, expected $lo..${hi.fold("")(_.toString)}"
    case ArrayLength(n, lo, hi)           => s"array of $n elements, expected $lo..${hi.fold("")(_.toString)}"
    case MissingKey(key)                  => s"missing required key $key"
    case UnknownKeys(count, first)        => s"$count unknown ${if (count == 1) "key" else "keys"}, the first $first"
    case IntegerLiteral(expected, bits)   => s"expected $expected, got an integer of $bits bits"
    case TextLiteral(expected, actual)    => s"expected $expected, got $actual"
    case PatternMismatch(pattern, actual) => s"$actual does not match $pattern"
    case UnknownTextAlternative(actual)   => s"$actual is not an alternative"
    case NoMapAlternative(keys)           => s"no alternative fits a map of $keys keys"
    case UnknownEnumInteger(name, bits)   => s"an integer of $bits bits is not a $name"
    case Canonical(cause)                 => cause.toString
  }
}
