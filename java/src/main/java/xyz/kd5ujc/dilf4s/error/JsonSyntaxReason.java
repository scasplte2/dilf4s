package xyz.kd5ujc.dilf4s.error;

/** The JSON syntax rule that failed. */
public enum JsonSyntaxReason {
  /** {@code expectedValue}. */
  EXPECTED_VALUE("expectedValue"),
  /** {@code expectedMemberName}. */
  EXPECTED_MEMBER_NAME("expectedMemberName"),
  /** {@code expectedColon}. */
  EXPECTED_COLON("expectedColon"),
  /** {@code expectedCommaOrBracket}. */
  EXPECTED_COMMA_OR_BRACKET("expectedCommaOrBracket"),
  /** {@code expectedCommaOrBrace}. */
  EXPECTED_COMMA_OR_BRACE("expectedCommaOrBrace"),
  /** {@code trailingContent}. */
  TRAILING_CONTENT("trailingContent"),
  /** {@code unterminatedString}. */
  UNTERMINATED_STRING("unterminatedString"),
  /** {@code controlCharacter}. */
  CONTROL_CHARACTER("controlCharacter"),
  /** {@code invalidEscape}. */
  INVALID_ESCAPE("invalidEscape");

  private final String wire;

  JsonSyntaxReason(String wire) {
    this.wire = wire;
  }

  /** Its name in the schema and the vectors. */
  public String wire() {
    return wire;
  }
}
