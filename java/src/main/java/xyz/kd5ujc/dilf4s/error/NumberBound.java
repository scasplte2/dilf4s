package xyz.kd5ujc.dilf4s.error;

/** A JSON number bound that a number exceeds. */
public sealed interface NumberBound
  permits
    NumberBound.Digits,
    NumberBound.Scale {

  /** The kind, as the schema and the vectors name it. */
  String kind();

  /** {@code Digits}. */
  record Digits(long digits, long limit) implements NumberBound {
    @Override
    public String kind() {
      return "Digits";
    }
  }

  /** {@code Scale}. */
  record Scale(int scale, long limit) implements NumberBound {
    @Override
    public String kind() {
      return "Scale";
    }
  }
}
