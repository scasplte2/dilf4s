package xyz.kd5ujc.dilf4s;

import xyz.kd5ujc.core.canonical.CanonicalValue;
import xyz.kd5ujc.dilf4s.error.TypeMismatch;
import xyz.kd5ujc.dilf4s.internal.ValueCases;
import xyz.kd5ujc.dilf4s.internal.Values;

/** Between the Java values and the reference's: O(1) each way, no copy and no walk. Internal to this package. */
final class Wrap {
  private Wrap() {}

  private static final ValueCases<Value> CASES = new ValueCases<>() {
    @Override
    public Value nul() {
      return Value.Null.INSTANCE;
    }

    @Override
    public Value bool(CanonicalValue.CBool b) {
      return new Value.Bool(b);
    }

    @Override
    public Value integer(CanonicalValue.CInt i) {
      return new Value.Int(i);
    }

    @Override
    public Value rational(CanonicalValue.CRational r) {
      return new Value.Rational(r);
    }

    @Override
    public Value text(CanonicalValue.CText t) {
      return new Value.Text(t);
    }

    @Override
    public Value bytes(CanonicalValue.CBytes b) {
      return new Value.Bytes(b);
    }

    @Override
    public Value array(CanonicalValue.CArray a) {
      return new Value.Array(a);
    }

    @Override
    public Value map(CanonicalValue.CMap m) {
      return new Value.Map(m);
    }
  };

  static Value of(CanonicalValue v) {
    return Values.visit(v, CASES);
  }

  static CanonicalValue scala(Value v) {
    return switch (v) {
      case Value.Null n -> Values.nul();
      case Value.Bool b -> b.scala;
      case Value.Int i -> i.scala;
      case Value.Rational r -> r.scala;
      case Value.Text t -> t.scala;
      case Value.Bytes b -> b.scala;
      case Value.Array a -> a.scala;
      case Value.Map m -> m.scala;
    };
  }

  /** The refusal of a null argument: every family holds {@link TypeMismatch}. */
  static TypeMismatch nullArgument(String what) {
    return new TypeMismatch("expected " + what + ", got null");
  }
}
