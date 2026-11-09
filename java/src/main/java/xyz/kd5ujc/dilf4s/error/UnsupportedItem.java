package xyz.kd5ujc.dilf4s.error;

/** A CBOR item outside the data model. */
public sealed interface UnsupportedItem
  permits
    UnsupportedItem.FloatOrSimple,
    UnsupportedItem.SimpleValue,
    UnsupportedItem.IndefiniteLength,
    UnsupportedItem.ReservedInfo,
    UnsupportedItem.Tag,
    UnsupportedItem.NonTextKey {

  /** The kind, as the schema and the vectors name it. */
  String kind();

  /** {@code FloatOrSimple}. */
  record FloatOrSimple(int info) implements UnsupportedItem {
    @Override
    public String kind() {
      return "FloatOrSimple";
    }
  }

  /** {@code SimpleValue}. */
  record SimpleValue(int value) implements UnsupportedItem {
    @Override
    public String kind() {
      return "SimpleValue";
    }
  }

  /** {@code IndefiniteLength}. */
  record IndefiniteLength() implements UnsupportedItem {
    @Override
    public String kind() {
      return "IndefiniteLength";
    }
  }

  /** {@code ReservedInfo}. */
  record ReservedInfo(int info) implements UnsupportedItem {
    @Override
    public String kind() {
      return "ReservedInfo";
    }
  }

  /** {@code Tag}. */
  record Tag(java.math.BigInteger tag) implements UnsupportedItem {
    @Override
    public String kind() {
      return "Tag";
    }
  }

  /** {@code NonTextKey}. */
  record NonTextKey() implements UnsupportedItem {
    @Override
    public String kind() {
      return "NonTextKey";
    }
  }
}
