/-
The canonical value model (docs/specs/canonical-encoding.md section 1) and the JLVM's measures of it
(docs/specs/jlvm.md section 2: weight, depth, elements, truthiness, kinds).
-/

namespace Jlvm

/-- Java's `BigInteger.bitLength` of a natural number: 0 for 0. -/
def natBits (n : Nat) : Nat := if n = 0 then 0 else n.log2 + 1

/-- Java's `BigInteger.bitLength`: the bits of the minimal two's-complement form, without the sign bit, so a negative
`n` has the bits of `-n - 1` (jlvm.md section 2). -/
def bitLength : Int → Nat
  | .ofNat n => natBits n
  | .negSucc n => natBits n

/-- Weights and gas saturate at `2^63 - 1` (Scala's `Long.MaxValue`). -/
def satMax : Nat := 2 ^ 63 - 1

/-- A canonical value. A number is one exact rational (`Rat` is always in lowest terms), so `2.0` *is* `2`: the integer
kind is a number whose denominator is 1. A map is a list of entries kept in canonical key order, with no null value
(`WF` states both), so structural equality is equality of canonical values. -/
inductive Value where
  | null
  | bool (b : Bool)
  | num (q : Rat)
  | text (s : String)
  | bytes (b : List UInt8)
  | arr (xs : List Value)
  | map (es : List (String × Value))
  deriving Inhabited, Repr

namespace Value

mutual
/-- Structural equality (`deriving DecidableEq` does not handle this nested inductive). -/
def decEqValue : (a b : Value) → Decidable (a = b)
  | .null, .null => isTrue rfl
  | .null, .bool _ => isFalse (fun e => nomatch e)
  | .null, .num _ => isFalse (fun e => nomatch e)
  | .null, .text _ => isFalse (fun e => nomatch e)
  | .null, .bytes _ => isFalse (fun e => nomatch e)
  | .null, .arr _ => isFalse (fun e => nomatch e)
  | .null, .map _ => isFalse (fun e => nomatch e)
  | .bool _, .null => isFalse (fun e => nomatch e)
  | .bool x, .bool y => if h : x = y then isTrue (h ▸ rfl) else isFalse (fun e => h (Value.bool.inj e))
  | .bool _, .num _ => isFalse (fun e => nomatch e)
  | .bool _, .text _ => isFalse (fun e => nomatch e)
  | .bool _, .bytes _ => isFalse (fun e => nomatch e)
  | .bool _, .arr _ => isFalse (fun e => nomatch e)
  | .bool _, .map _ => isFalse (fun e => nomatch e)
  | .num _, .null => isFalse (fun e => nomatch e)
  | .num _, .bool _ => isFalse (fun e => nomatch e)
  | .num x, .num y => if h : x = y then isTrue (h ▸ rfl) else isFalse (fun e => h (Value.num.inj e))
  | .num _, .text _ => isFalse (fun e => nomatch e)
  | .num _, .bytes _ => isFalse (fun e => nomatch e)
  | .num _, .arr _ => isFalse (fun e => nomatch e)
  | .num _, .map _ => isFalse (fun e => nomatch e)
  | .text _, .null => isFalse (fun e => nomatch e)
  | .text _, .bool _ => isFalse (fun e => nomatch e)
  | .text _, .num _ => isFalse (fun e => nomatch e)
  | .text x, .text y => if h : x = y then isTrue (h ▸ rfl) else isFalse (fun e => h (Value.text.inj e))
  | .text _, .bytes _ => isFalse (fun e => nomatch e)
  | .text _, .arr _ => isFalse (fun e => nomatch e)
  | .text _, .map _ => isFalse (fun e => nomatch e)
  | .bytes _, .null => isFalse (fun e => nomatch e)
  | .bytes _, .bool _ => isFalse (fun e => nomatch e)
  | .bytes _, .num _ => isFalse (fun e => nomatch e)
  | .bytes _, .text _ => isFalse (fun e => nomatch e)
  | .bytes x, .bytes y => if h : x = y then isTrue (h ▸ rfl) else isFalse (fun e => h (Value.bytes.inj e))
  | .bytes _, .arr _ => isFalse (fun e => nomatch e)
  | .bytes _, .map _ => isFalse (fun e => nomatch e)
  | .arr _, .null => isFalse (fun e => nomatch e)
  | .arr _, .bool _ => isFalse (fun e => nomatch e)
  | .arr _, .num _ => isFalse (fun e => nomatch e)
  | .arr _, .text _ => isFalse (fun e => nomatch e)
  | .arr _, .bytes _ => isFalse (fun e => nomatch e)
  | .arr x, .arr y => match decEqList x y with
    | isTrue h => isTrue (h ▸ rfl)
    | isFalse h => isFalse (fun e => h (Value.arr.inj e))
  | .arr _, .map _ => isFalse (fun e => nomatch e)
  | .map _, .null => isFalse (fun e => nomatch e)
  | .map _, .bool _ => isFalse (fun e => nomatch e)
  | .map _, .num _ => isFalse (fun e => nomatch e)
  | .map _, .text _ => isFalse (fun e => nomatch e)
  | .map _, .bytes _ => isFalse (fun e => nomatch e)
  | .map _, .arr _ => isFalse (fun e => nomatch e)
  | .map x, .map y => match decEqEntries x y with
    | isTrue h => isTrue (h ▸ rfl)
    | isFalse h => isFalse (fun e => h (Value.map.inj e))
def decEqList : (xs ys : List Value) → Decidable (xs = ys)
  | [], [] => isTrue rfl
  | [], _ :: _ => isFalse (fun e => nomatch e)
  | _ :: _, [] => isFalse (fun e => nomatch e)
  | x :: xs, y :: ys =>
    match decEqValue x y, decEqList xs ys with
    | isTrue h, isTrue t => isTrue (h ▸ t ▸ rfl)
    | isFalse h, _ => isFalse (fun e => h (List.cons.inj e).1)
    | _, isFalse t => isFalse (fun e => t (List.cons.inj e).2)
def decEqEntries : (xs ys : List (String × Value)) → Decidable (xs = ys)
  | [], [] => isTrue rfl
  | [], _ :: _ => isFalse (fun e => nomatch e)
  | _ :: _, [] => isFalse (fun e => nomatch e)
  | (k, x) :: xs, (l, y) :: ys =>
    if hk : k = l then
      match decEqValue x y, decEqEntries xs ys with
      | isTrue h, isTrue t => isTrue (hk ▸ h ▸ t ▸ rfl)
      | isFalse h, _ => isFalse (fun e => h (Prod.mk.inj (List.cons.inj e).1).2)
      | _, isFalse t => isFalse (fun e => t (List.cons.inj e).2)
    else isFalse (fun e => hk (Prod.mk.inj (List.cons.inj e).1).1)
end

instance : DecidableEq Value := decEqValue


def int (n : Int) : Value := .num n

/-- `n` as `k` big-endian bytes. -/
def beBytes : Nat → Nat → List UInt8
  | 0, _ => []
  | k + 1, n => beBytes k (n / 256) ++ [UInt8.ofNat (n % 256)]

/-- A CBOR head: the major type and its argument in shortest form (canonical-encoding.md section 2). The argument is
below `2^64` wherever it is used. -/
def head (major arg : Nat) : List UInt8 :=
  if arg < 24 then [UInt8.ofNat (major * 32 + arg)]
  else if arg < 2 ^ 8 then UInt8.ofNat (major * 32 + 24) :: beBytes 1 arg
  else if arg < 2 ^ 16 then UInt8.ofNat (major * 32 + 25) :: beBytes 2 arg
  else if arg < 2 ^ 32 then UInt8.ofNat (major * 32 + 26) :: beBytes 4 arg
  else UInt8.ofNat (major * 32 + 27) :: beBytes 8 arg

/-- Text's canonical CBOR: major type 3, then its UTF-8 bytes. -/
def encodeText (s : String) : List UInt8 := head 3 s.utf8ByteSize ++ s.toUTF8.toList

/-- A map's entries are ordered by their keys' encoded form, bytewise (RFC 8949 section 4.2.1). -/
def keyBytes (k : String) : List UInt8 := encodeText k

/-- Bytewise lexicographic order; a proper prefix comes first. -/
def lexLt : List UInt8 → List UInt8 → Bool
  | [], [] => false
  | [], _ :: _ => true
  | _ :: _, [] => false
  | a :: as, b :: bs => if a < b then true else if a = b then lexLt as bs else false

/-- The weight of a value written out in full, before saturation (jlvm.md section 2). -/
def rawWeight : Value → Nat
  | .null => 1
  | .bool _ => 1
  | .num q => if q.den = 1 then 1 + bitLength q.num / 64 else 2 + bitLength q.num / 64 + natBits q.den / 64
  | .text s => 1 + s.utf8ByteSize
  | .bytes b => 1 + b.length
  | .arr xs => 1 + rawWeightList xs
  | .map es => 1 + rawWeightEntries es
where
  rawWeightList : List Value → Nat
    | [] => 0
    | x :: xs => rawWeight x + rawWeightList xs
  rawWeightEntries : List (String × Value) → Nat
    | [] => 0
    | (k, v) :: es => 1 + k.utf8ByteSize + rawWeight v + rawWeightEntries es

/-- The weight, saturating at `2^63 - 1`. A saturating left fold of non-negative terms is the saturated sum, so this
is the reference's `foldLeft(1L)(Weight.add)` (core `CanonicalValue.weight`). -/
def weight (v : Value) : Nat := min v.rawWeight satMax

/-- Nesting depth: 0 for scalars, one more than the deepest element for arrays and maps (1 when empty). -/
def depth : Value → Nat
  | .arr xs => 1 + depthList xs
  | .map es => 1 + depthEntries es
  | _ => 0
where
  depthList : List Value → Nat
    | [] => 0
    | x :: xs => max (depth x) (depthList xs)
  depthEntries : List (String × Value) → Nat
    | [] => 0
    | (_, v) :: es => max (depth v) (depthEntries es)

/-- Top-level entries of arrays and maps, 1 for anything else (the `Elements` size unit, jlvm.md section 7). -/
def elements : Value → Nat
  | .arr xs => xs.length
  | .map es => es.length
  | _ => 1

/-- JsonLogic truthiness: null, false, 0, "", empty bytes, [] and {} are falsy; every non-integer rational is truthy. -/
def truthy : Value → Bool
  | .null => false
  | .bool b => b
  | .num q => q != 0
  | .text s => !s.isEmpty
  | .bytes b => !b.isEmpty
  | .arr xs => !xs.isEmpty
  | .map es => !es.isEmpty

/-- The kind `typeof` names (jlvm.md section 2). -/
def kind : Value → String
  | .null => "null"
  | .bool _ => "boolean"
  | .num _ => "number"
  | .text _ => "string"
  | .bytes _ => "bytes"
  | .arr _ => "array"
  | .map _ => "object"

def isNull : Value → Bool
  | .null => true
  | _ => false

/-- The value a map holds at `k`. -/
def lookup (es : List (String × Value)) (k : String) : Option Value :=
  (es.find? (·.1 == k)).map (·.2)

/-- `es` with `k` set to `v` (removed when `v` is null), kept in canonical key order. -/
def insertEntry (k : String) (v : Value) : List (String × Value) → List (String × Value)
  | [] => if v.isNull then [] else [(k, v)]
  | (k', v') :: es =>
    if k = k' then (if v.isNull then es else (k, v) :: es)
    else if lexLt (keyBytes k) (keyBytes k') then (if v.isNull then (k', v') :: es else (k, v) :: (k', v') :: es)
    else (k', v') :: insertEntry k v es

/-- A map from entries in any order, later keys winning, nulls dropped. -/
def mapOf (es : List (String × Value)) : Value := .map (es.foldl (fun acc (k, v) => insertEntry k v acc) [])

end Value

/-- The data model's number width: `MaxIntegerBits` = `MaxRationalBits` = 33219 (canonical-encoding.md section 1). -/
def maxIntegerBits : Nat := 33219

/-- The canonical data model's invariants beyond the type: numbers within 33219 bits, map keys in strictly ascending
encoded order and no null map value (canonical-encoding.md sections 1 and 2). Text is a `String`, so it is always
well-formed; a `Rat` is always in lowest terms. -/
inductive WF : Value → Prop where
  | null : WF .null
  | bool (b : Bool) : WF (.bool b)
  | num (q : Rat) : bitLength q.num ≤ maxIntegerBits → natBits q.den ≤ maxIntegerBits → WF (.num q)
  | text (s : String) : WF (.text s)
  | bytes (b : List UInt8) : WF (.bytes b)
  | arr (xs : List Value) : (∀ x ∈ xs, WF x) → WF (.arr xs)
  | map (es : List (String × Value)) :
      (∀ e ∈ es, WF e.2) → (∀ e ∈ es, e.2 ≠ .null) →
      List.Pairwise (fun a b => Value.lexLt (Value.keyBytes a.1) (Value.keyBytes b.1) = true) es → WF (.map es)

/-- A string's length in UTF-16 units, as the reference measures key and segment lengths. -/
def utf16Length (s : String) : Nat := s.toList.foldl (fun n c => n + if c.val.toNat ≥ 0x10000 then 2 else 1) 0

end Jlvm
