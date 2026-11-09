import Jlvm.Value

/-
Canonical CBOR (docs/specs/canonical-encoding.md section 2): the encoder, and the strict decoder with the normative
refusal order. The decoder recurses only into arrays and maps, and refuses a container opened at depth 256, so it is
defined by well-founded recursion on the depth still allowed: no fuel.
-/

namespace Jlvm.Cbor

open Value

/-- The bytes of `n`, big-endian, with no leading zero byte (empty for 0). -/
def magnitude (n : Nat) : List UInt8 :=
  if _h : n = 0 then [] else magnitude (n / 256) ++ [UInt8.ofNat (n % 256)]
termination_by n
decreasing_by omega

/-- A bignum: tag 2 (or 3) and the byte string of its magnitude. -/
def bignum (tag m : Nat) : List UInt8 := head 6 tag ++ head 2 (magnitude m).length ++ magnitude m

/-- An integer: major type 0 or 1 within 64 bits, a bignum outside. -/
def encodeInt : Int → List UInt8
  | .ofNat n => if n < 2 ^ 64 then head 0 n else bignum 2 n
  | .negSucc n => if n < 2 ^ 64 then head 1 n else bignum 3 n

def encodeNum (q : Rat) : List UInt8 :=
  if q.den = 1 then encodeInt q.num else head 6 30 ++ head 4 2 ++ encodeInt q.num ++ encodeInt q.den

mutual
/-- A value's canonical CBOR. A map's entries are written in list order, which `WF` requires to be canonical. -/
def encode : Value → List UInt8
  | .null => [0xf6]
  | .bool false => [0xf4]
  | .bool true => [0xf5]
  | .num q => encodeNum q
  | .text s => encodeText s
  | .bytes b => head 2 b.length ++ b
  | .arr xs => head 4 xs.length ++ encodeList xs
  | .map es => head 5 es.length ++ encodeEntries es
def encodeList : List Value → List UInt8
  | [] => []
  | x :: xs => encode x ++ encodeList xs
def encodeEntries : List (String × Value) → List UInt8
  | [] => []
  | (k, v) :: es => encodeText k ++ encode v ++ encodeEntries es
end

/-- The decoder's refusals, `CanonicalError`'s kinds (conformance.md section 1). -/
inductive Error where
  | unexpectedEnd | nonCanonical | unsupported | invalidUtf8 | depthExceeded | integerTooWide | rationalTooWide
  | trailingBytes
  deriving DecidableEq, Repr

def Error.name : Error → String
  | .unexpectedEnd => "UnexpectedEnd"
  | .nonCanonical => "NonCanonical"
  | .unsupported => "Unsupported"
  | .invalidUtf8 => "InvalidUtf8"
  | .depthExceeded => "DepthExceeded"
  | .integerTooWide => "IntegerTooWide"
  | .rationalTooWide => "RationalTooWide"
  | .trailingBytes => "TrailingBytes"

/-- The default and only decoding depth bound: 256 containers. -/
def maxDepth : Nat := 256

/-- Big-endian bytes as a natural number. -/
def beValue (bs : List UInt8) : Nat := bs.foldl (fun n b => n * 256 + b.toNat) 0

/-- `n` bytes, or `UnexpectedEnd`. -/
def span (n : Nat) (bs : List UInt8) : Except Error (List UInt8 × List UInt8) :=
  if n ≤ bs.length then .ok (bs.take n, bs.drop n) else .error .unexpectedEnd

/-- An item's head: its major type, its argument (in shortest form) and the bytes after it. Major type 7 with
additional information 24 and up (a float, or a simple value with a one-byte argument) is unsupported, as are the
reserved values and indefinite lengths. -/
def readHead (bs : List UInt8) : Except Error (Nat × Nat × List UInt8) :=
  match bs with
  | [] => .error .unexpectedEnd
  | b :: rest =>
    let major := b.toNat / 32
    let info := b.toNat % 32
    let sized (len minimum : Nat) : Except Error (Nat × Nat × List UInt8) := do
      let (arg, after) ← span len rest
      let n := beValue arg
      if n ≥ minimum then pure (major, n, after) else throw .nonCanonical
    if info < 24 then .ok (major, info, rest)
    else if major = 7 then .error .unsupported
    else if info = 24 then sized 1 24
    else if info = 25 then sized 2 0x100
    else if info = 26 then sized 4 0x10000
    else if info = 27 then sized 8 (2 ^ 32)
    else .error .unsupported

/-- A bignum's magnitude, in order: a byte string (`NonCanonical`), present in full (`UnexpectedEnd`), non-empty with
no leading zero and longer than 8 bytes (`NonCanonical`), at most `limit` bits wide (`tooWide`), all before it is
built. Either tag's width is its magnitude's. -/
def readBignum (tooWide : Error) (bs : List UInt8) : Except Error (Nat × List UInt8) := do
  let (major, len, body) ← readHead bs
  if major ≠ 2 then throw .nonCanonical
  let (b, after) ← span len body
  match b with
  | [] => throw .nonCanonical
  | first :: _ =>
    if first = 0 then throw .nonCanonical
    if b.length ≤ 8 then throw .nonCanonical
    if 8 * (b.length - 1) + natBits first.toNat > maxIntegerBits then throw tooWide
    pure (beValue b, after)

/-- A rational's part: a canonical integer (`NonCanonical`), at most `MaxRationalBits` wide (`RationalTooWide`). -/
def readPart (bs : List UInt8) : Except Error (Int × List UInt8) := do
  let (major, n, after) ← readHead bs
  if major = 0 then pure (n, after)
  else if major = 1 then pure (-1 - (n : Int), after)
  else if major = 6 ∧ (n = 2 ∨ n = 3) then
    let (m, after') ← readBignum .rationalTooWide after
    pure (if n = 2 then (m : Int) else -1 - (m : Int), after')
  else throw .nonCanonical

/-- A tagged item: a bignum (tags 2 and 3) or a rational (tag 30), in the orders of canonical-encoding.md section 2. -/
def readTagged (tag : Nat) (bs : List UInt8) : Except Error (Value × List UInt8) :=
  if tag = 2 ∨ tag = 3 then do
    let (m, after) ← readBignum .integerTooWide bs
    pure (.num (if tag = 2 then (m : Int) else -1 - (m : Int)), after)
  else if tag = 30 then do
    let (major, n, afterPair) ← readHead bs
    if ¬ (major = 4 ∧ n = 2) then throw .nonCanonical
    let (nu, afterNum) ← readPart afterPair
    let (den, afterDen) ← readPart afterNum
    if den < 2 then throw .nonCanonical
    if h : nu.natAbs.Coprime den.toNat ∧ den.toNat ≠ 0 then
      pure (.num ⟨nu, den.toNat, h.2, h.1⟩, afterDen)
    else throw .nonCanonical
  else .error .unsupported

/-- Text: present in full (`UnexpectedEnd`), then well-formed UTF-8 (`InvalidUtf8`). -/
def readText (len : Nat) (bs : List UInt8) : Except Error (String × List UInt8) := do
  let (b, after) ← span len bs
  match String.fromUTF8? ⟨b.toArray⟩ with
  | some s => pure (s, after)
  | none => throw .invalidUtf8

mutual
/-- One item at `depth` containers deep. -/
def item (depth : Nat) (bs : List UInt8) : Except Error (Value × List UInt8) := do
  let (major, arg, rest) ← readHead bs
  if major = 0 then pure (.num (arg : Int), rest)
  else if major = 1 then pure (.num (-1 - (arg : Int)), rest)
  else if major = 2 then do
    let (b, after) ← span arg rest
    pure (.bytes b, after)
  else if major = 3 then do
    let (s, after) ← readText arg rest
    pure (.text s, after)
  else if major = 4 then
    if arg > rest.length then throw .unexpectedEnd
    else if h : depth ≥ maxDepth then throw .depthExceeded
    else items (depth + 1) arg rest []
  else if major = 5 then
    if 2 * arg > rest.length then throw .unexpectedEnd
    else if h : depth ≥ maxDepth then throw .depthExceeded
    else entries (depth + 1) arg rest none []
  else if major = 6 then readTagged arg rest
  else if arg = 20 then pure (.bool false, rest)
  else if arg = 21 then pure (.bool true, rest)
  else if arg = 22 then pure (.null, rest)
  else throw .unsupported
termination_by (maxDepth + 1 - depth, 0, 0)
/-- The `n` elements still to read of an array whose elements are at `depth`. -/
def items (depth n : Nat) (bs : List UInt8) (acc : List Value) : Except Error (Value × List UInt8) :=
  match n with
  | 0 => pure (.arr acc.reverse, bs)
  | n + 1 => do
    let (v, after) ← item depth bs
    items depth n after (v :: acc)
termination_by (maxDepth + 1 - depth, 1, n)
/-- The `n` entries still to read of a map whose entries are at `depth`: each key once read whole is text
(`Unsupported`) after the previous key in encoded order (`NonCanonical`), then each value is not null
(`NonCanonical`). -/
def entries (depth n : Nat) (bs : List UInt8) (last : Option String) (acc : List (String × Value)) :
    Except Error (Value × List UInt8) :=
  match n with
  | 0 => pure (.map acc.reverse, bs)
  | n + 1 => do
    let (k, afterKey) ← item depth bs
    match k with
    | .text key =>
      if last.any (fun l => !lexLt (keyBytes l) (keyBytes key)) then throw .nonCanonical
      let (v, afterValue) ← item depth afterKey
      if v = .null then throw .nonCanonical
      entries depth n afterValue (some key) ((key, v) :: acc)
    | _ => throw .unsupported
termination_by (maxDepth + 1 - depth, 1, n)
end

/-- The whole input as one value: anything left after it is `TrailingBytes`. -/
def decode (bs : List UInt8) : Except Error Value := do
  let (v, rest) ← item 0 bs
  if rest.isEmpty then pure v else throw .trailingBytes

end Jlvm.Cbor
