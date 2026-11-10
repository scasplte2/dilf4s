import Jlvm.Value

/-
The strict JSON parser (docs/specs/canonical-encoding.md section 3, `CanonicalJson.parse`), and the JLVM's JSON
surface values (jlvm.md section 1: `0x`-hex strings are bytes).

The parser reads the text once, left to right, and the refusal is the first fault it meets. Strings are read as UTF-16
units, as the reference reads them, so an escaped unpaired surrogate is seen and refused: a string value's at its
closing quote, a member name's when its object closes. The tree keeps member order and null members, which the
program surface needs (`SurfaceTree`).

Lean's own `Lean.Json` is not used for either: it keeps the last of repeated member names, bounds neither depth nor a
number's digits or scale, and does not keep member order.
-/

namespace Jlvm.Json

/-- A JSON value as read, member order and null members kept. Numbers are exact. -/
inductive Json where
  | null
  | bool (b : Bool)
  | num (q : Rat)
  | str (s : String)
  | arr (xs : List Json)
  | obj (fs : List (String × Json))
  deriving Inhabited, Repr

/-- `CanonicalError`'s kinds that JSON reading can give. -/
inductive Error where
  | syntax | depthExceeded | invalidNumber | integerTooWide | rationalTooWide | unpairedSurrogate | duplicateKey
  deriving DecidableEq, Repr

def Error.name : Error → String
  | .syntax => "JsonSyntax"
  | .depthExceeded => "DepthExceeded"
  | .invalidNumber => "InvalidNumber"
  | .integerTooWide => "IntegerTooWide"
  | .rationalTooWide => "RationalTooWide"
  | .unpairedSurrogate => "UnpairedSurrogate"
  | .duplicateKey => "DuplicateKey"

/-- 10000: the most significant digits, exponent digits and scale a number may have (`MaxDigits`, `MaxScale`). -/
def maxDigits : Nat := 10000

def isWs (c : Char) : Bool := c = ' ' || c = '\t' || c = '\n' || c = '\r'

def skipWs : List Char → List Char
  | c :: cs => if isWs c then skipWs cs else c :: cs
  | [] => []

def isDigit (c : Char) : Bool := '0' ≤ c && c ≤ '9'

def hexVal (c : Char) : Option Nat :=
  if '0' ≤ c && c ≤ '9' then some (c.toNat - '0'.toNat)
  else if 'a' ≤ c && c ≤ 'f' then some (c.toNat - 'a'.toNat + 10)
  else if 'A' ≤ c && c ≤ 'F' then some (c.toNat - 'A'.toNat + 10)
  else none

/-- A character's UTF-16 units. -/
def units (c : Char) : List Nat :=
  let v := c.toNat
  if v < 0x10000 then [v] else
    let u := v - 0x10000
    [0xD800 + u / 0x400, 0xDC00 + u % 0x400]

/-- Well-formed UTF-16 as text; `none` when a surrogate is unpaired. -/
def fromUnits : List Nat → Option (List Char)
  | [] => some []
  | u :: rest =>
    if 0xD800 ≤ u ∧ u < 0xDC00 then
      match rest with
      | l :: rest' =>
        if 0xDC00 ≤ l ∧ l < 0xE000 then
          (fromUnits rest').map (Char.ofNat (0x10000 + (u - 0xD800) * 0x400 + (l - 0xDC00)) :: ·)
        else none
      | [] => none
    else if 0xDC00 ≤ u ∧ u < 0xE000 then none
    else (fromUnits rest).map (Char.ofNat u :: ·)

/-- A string's units, from just past its opening quote to just past its closing quote. -/
def stringUnits : List Char → List Nat → Except Error (List Nat × List Char)
  | [], _ => .error .syntax
  | '"' :: rest, acc => .ok (acc.reverse, rest)
  | '\\' :: e :: rest, acc =>
    match e with
    | '"' => stringUnits rest (0x22 :: acc)
    | '\\' => stringUnits rest (0x5C :: acc)
    | '/' => stringUnits rest (0x2F :: acc)
    | 'b' => stringUnits rest (0x08 :: acc)
    | 'f' => stringUnits rest (0x0C :: acc)
    | 'n' => stringUnits rest (0x0A :: acc)
    | 'r' => stringUnits rest (0x0D :: acc)
    | 't' => stringUnits rest (0x09 :: acc)
    | 'u' =>
      match rest with
      | a :: b :: c :: d :: rest' =>
        match hexVal a, hexVal b, hexVal c, hexVal d with
        | some a, some b, some c, some d => stringUnits rest' ((((a * 16 + b) * 16 + c) * 16 + d) :: acc)
        | _, _, _, _ => .error .syntax
      | _ => .error .syntax
    | _ => .error .syntax
  | '\\' :: [], _ => .error .syntax
  | c :: rest, acc => if c.toNat < 0x20 then .error .syntax else stringUnits rest ((units c).reverse ++ acc)

def digitsOf (cs : List Char) : List Char × List Char := (cs.takeWhile isDigit, cs.dropWhile isDigit)

def digitValue (ds : List Char) : Nat := ds.foldl (fun n c => n * 10 + (c.toNat - '0'.toNat)) 0

/-- 2^24: the most a text's numbers may spend on their exponents, each its exponent's magnitude, at most
`maxDigits + 1` (`MaxJsonExponents`, canonical-encoding.md section 3). -/
def maxJsonExponents : Nat := 2 ^ 24

/-- `-? digits (. digits)? ([eE] [+-]? digits)?`, converted exactly: malformed (`InvalidNumber`), then the
significand's digits, the exponent's digits and the scale (each `InvalidNumber`), all on the text before any
conversion; then the exponent budget, `spent` so far (`InvalidNumber`); then the value's width (`IntegerTooWide`,
`RationalTooWide`). Returns what the text has spent once the number is read. -/
def number (spent : Nat) (cs : List Char) : Except Error (Rat × List Char × Nat) :=
  let (negative, cs1) := match cs with
    | '-' :: r => (true, r)
    | r => (false, r)
  let (intDigits, cs2) := digitsOf cs1
  let (hasFrac, fracDigits, cs3) := match cs2 with
    | '.' :: r => let (f, r') := digitsOf r; (true, f, r')
    | r => (false, [], r)
  let (hasExp, expNegative, expDigits, cs4) := match cs3 with
    | c :: r =>
      if c = 'e' ∨ c = 'E' then
        match r with
        | '+' :: r' => let (e, r'') := digitsOf r'; (true, false, e, r'')
        | '-' :: r' => let (e, r'') := digitsOf r'; (true, true, e, r'')
        | r' => let (e, r'') := digitsOf r'; (true, false, e, r'')
      else (false, false, [], c :: r)
    | [] => (false, false, [], [])
  let malformed := intDigits.isEmpty || (intDigits.length > 1 && intDigits.head? = some '0') ||
    (hasFrac && fracDigits.isEmpty) || (hasExp && expDigits.isEmpty)
  let significand :=
    if intDigits = ['0'] then (fracDigits.dropWhile (· = '0')).length else intDigits.length + fracDigits.length
  let exponentDigits := (expDigits.dropWhile (· = '0')).length
  if malformed then .error .invalidNumber
  else if significand > maxDigits then .error .invalidNumber
  else if exponentDigits > maxDigits then .error .invalidNumber
  else
    -- an exponent of more than 18 digits saturates (Long.MaxValue), past every scale this text can reach
    let magnitude : Int := if exponentDigits > 18 then (2 ^ 63 - 1 : Nat) else digitValue expDigits
    let exponent : Int := if expNegative then -magnitude else magnitude
    let scale : Int := fracDigits.length - exponent
    -- its exponent's magnitude, at most one past the largest scale
    let total := spent + min magnitude.toNat (maxDigits + 1)
    if scale.natAbs > maxDigits then .error .invalidNumber
    else if total > maxJsonExponents then .error .invalidNumber
    else
      let unscaled : Int := (if negative then -1 else 1) * (digitValue (intDigits ++ fracDigits) : Int)
      let q : Rat := if scale ≥ 0 then mkRat unscaled (10 ^ scale.toNat) else (unscaled * 10 ^ (-scale).toNat : Int)
      if q.den = 1 then
        if bitLength q.num > maxIntegerBits then .error .integerTooWide else .ok (q, cs4, total)
      else if bitLength q.num > maxIntegerBits ∨ natBits q.den > maxIntegerBits then .error .rationalTooWide
      else .ok (q, cs4, total)

/-- The maximum number of containers open at once (`MaxDepth`). -/
def maxDepth : Nat := 256

/-- Whether `key` (as UTF-16 units) is among `seen`. -/
def seenKey (seen : List (List Nat)) (key : List Nat) : Bool := seen.contains key

mutual
/-- A value at `depth` open containers, `spent` already spent on exponents; it returns what is spent once it is read.
`fuel` is twice the text's length plus two: each nesting level takes two calls (`value`, then `elements` or
`members`), and each of those reads a character before calling again. -/
def value (fuel depth spent : Nat) (cs : List Char) : Except Error (Json × List Char × Nat) :=
  match fuel with
  | 0 => .error .syntax
  | fuel + 1 =>
    match skipWs cs with
    | '{' :: rest =>
      if depth ≥ maxDepth then .error .depthExceeded
      else match skipWs rest with
        | '}' :: rest' => .ok (.obj [], rest', spent)
        | _ => members fuel (depth + 1) spent rest [] []
    | '[' :: rest =>
      if depth ≥ maxDepth then .error .depthExceeded
      else match skipWs rest with
        | ']' :: rest' => .ok (.arr [], rest', spent)
        | _ => elements fuel (depth + 1) spent rest []
    | '"' :: rest => do
      let (us, rest') ← stringUnits rest []
      match fromUnits us with
      | some s => pure (.str (String.ofList s), rest', spent)
      | none => throw .unpairedSurrogate
    | 't' :: 'r' :: 'u' :: 'e' :: rest => .ok (.bool true, rest, spent)
    | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: rest => .ok (.bool false, rest, spent)
    | 'n' :: 'u' :: 'l' :: 'l' :: rest => .ok (.null, rest, spent)
    | c :: rest =>
      if c = '-' ∨ isDigit c then do
        let (q, rest', now) ← number spent (c :: rest)
        pure (.num q, rest', now)
      else .error .syntax
    | [] => .error .syntax
/-- The elements of an array, after its `[` or a `,`. -/
def elements (fuel depth spent : Nat) (cs : List Char) (acc : List Json) : Except Error (Json × List Char × Nat) :=
  match fuel with
  | 0 => .error .syntax
  | fuel + 1 => do
    let (v, rest, now) ← value fuel depth spent cs
    match skipWs rest with
    | ',' :: rest' => elements fuel depth now rest' (v :: acc)
    | ']' :: rest' => pure (.arr (v :: acc).reverse, rest', now)
    | _ => throw .syntax
/-- The members of an object, after its `{` or a `,`: a repeated name is refused as soon as it is read, and a name's
unpaired surrogate only when the object closes. -/
def members (fuel depth spent : Nat) (cs : List Char) (seen : List (List Nat)) (acc : List (List Nat × Json)) :
    Except Error (Json × List Char × Nat) :=
  match fuel with
  | 0 => .error .syntax
  | fuel + 1 =>
    match skipWs cs with
    | '"' :: rest => do
      let (key, afterKey) ← stringUnits rest []
      if seenKey seen key then throw .duplicateKey
      match skipWs afterKey with
      | ':' :: afterColon => do
        let (v, afterValue, now) ← value fuel depth spent afterColon
        match skipWs afterValue with
        | ',' :: rest' => members fuel depth now rest' (key :: seen) ((key, v) :: acc)
        | '}' :: rest' =>
          let fields := ((key, v) :: acc).reverse
          match fields.mapM (fun (k, x) => (fromUnits k).map (fun s => (String.ofList s, x))) with
          | some fs => pure (.obj fs, rest', now)
          | none => throw .unpairedSurrogate
        | _ => throw .syntax
      | _ => throw .syntax
    | _ => .error .syntax
end

/-- JSON text: one value and nothing after it. -/
def parse (text : String) : Except Error Json := do
  let cs := text.toList
  let (v, rest, _) ← value (2 * cs.length + 2) 0 0 cs
  if (skipWs rest).isEmpty then pure v else throw .syntax

/-- The bytes a string denotes in the JLVM's JSON surface, when it is `0x` and an even number of lowercase hex digits. -/
def hexBytes (s : String) : Option (List UInt8) :=
  match s.toList with
  | '0' :: 'x' :: ds =>
    let rec go : List Char → Option (List UInt8)
      | [] => some []
      | a :: b :: rest =>
        if ('0' ≤ a && a ≤ '9' || 'a' ≤ a && a ≤ 'f') && ('0' ≤ b && b ≤ '9' || 'a' ≤ b && b ≤ 'f') then
          match hexVal a, hexVal b, go rest with
          | some x, some y, some r => some (UInt8.ofNat (x * 16 + y) :: r)
          | _, _, _ => none
        else none
      | [_] => none
    go ds
  | _ => none

mutual
/-- The canonical value of JSON as the JLVM surface reads it: maps drop null members, and `0x`-hex strings (not map
keys) are bytes (`JsonSurface.fromCanonicalJson`). -/
def toValue : Json → Value
  | .null => .null
  | .bool b => .bool b
  | .num q => .num q
  | .str s => match hexBytes s with
    | some b => .bytes b
    | none => .text s
  | .arr xs => .arr (toValueList xs)
  | .obj fs => Value.mapOf (toValueFields fs)
def toValueList : List Json → List Value
  | [] => []
  | x :: xs => toValue x :: toValueList xs
def toValueFields : List (String × Json) → List (String × Value)
  | [] => []
  | (k, x) :: fs => (k, toValue x) :: toValueFields fs
end

mutual
/-- The canonical value of JSON, strings as text (`CanonicalJson.parse` itself, for the encoding vectors). -/
def toCanonical : Json → Value
  | .null => .null
  | .bool b => .bool b
  | .num q => .num q
  | .str s => .text s
  | .arr xs => .arr (toCanonicalList xs)
  | .obj fs => Value.mapOf (toCanonicalFields fs)
def toCanonicalList : List Json → List Value
  | [] => []
  | x :: xs => toCanonical x :: toCanonicalList xs
def toCanonicalFields : List (String × Json) → List (String × Value)
  | [] => []
  | (k, x) :: fs => (k, toCanonical x) :: toCanonicalFields fs
end

end Jlvm.Json
