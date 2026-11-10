package xyz.kd5ujc.modelsgen

/** A fixed-width integer selected from schema bounds, before a target language renders it. */
enum NativeInteger(val rustType: String, val rustCodec: String, val goType: String, val goCodec: String) {
  case U32 extends NativeInteger("u32", "U32In", "uint32", "Uint32In")
  case U64 extends NativeInteger("u64", "U64In", "uint64", "Uint64In")
  case I32 extends NativeInteger("i32", "I32In", "int32", "Int32In")
  case I64 extends NativeInteger("i64", "I64In", "int64", "Int64In")
}

object NativeInteger {
  def forRange(lo: BigInt, hi: BigInt): Option[NativeInteger] =
    if (lo >= 0 && hi <= BigInt("4294967295")) Some(NativeInteger.U32)
    else if (lo >= 0 && hi <= BigInt("18446744073709551615")) Some(NativeInteger.U64)
    else if (lo >= Int.MinValue && hi <= Int.MaxValue) Some(NativeInteger.I32)
    else if (lo >= Long.MinValue && hi <= Long.MaxValue) Some(NativeInteger.I64)
    else None
}
