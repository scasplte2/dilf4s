import Jlvm.Program

/-
Metered evaluation of the slice (docs/specs/jlvm.md sections 2, 4, 5 and 7; the reference is `Machine.scala`'s
`Semantics`, `PureOps.scala` and `Gas.scala`).

The evaluator is big-step and defined by well-founded recursion on the program: a callback runs its body, a
subterm, once per element of a finite list, so no fuel is needed. Its monad `Ev` carries, as part of every
computation, the proof that it keeps the meter sound (`Sound`): only `pure`, `bind`, `charge`, `chargeStep`, `fail`
and `warn` build one, and each is proved sound once. So every evaluation is sound by construction (Theorems.lean).
-/

namespace Jlvm

/-! ### Gas schedule v1, for the slice -/

def literalGas : Nat := 10
def elementGas : Nat := 11
def varAccess : Nat := 70
def perPathSegment : Nat := 4
def callbackRun : Nat := 18
def renderingGas : Nat := 2
/-- `perUnit(split)`, the rate a path parsed at run time pays per unit of its weight. -/
def splitPerUnit : Nat := 6

def maxNumberBits : Nat := 4096
def maxWeight : Nat := 2 ^ 24
def maxWarnings : Nat := 64

/-- Saturating gas arithmetic (`Gas.scala`). -/
def gasAdd (a b : Nat) : Nat := min (a + b) satMax
def gasMul (g n : Nat) : Nat := min (g * n) satMax

/-- An operation's base charge. -/
def base : String → Nat
  | "if" => 20
  | "and" | "or" | "default" => 15
  | "!" | "!!" => 25
  | "==" | "!=" | "===" | "!==" => 50
  | "<" | "<=" | ">" | ">=" => 65
  | "+" | "-" | "*" => 40
  | "cat" => 65
  | "merge" => 210
  | "map" | "all" => 40
  | _ => 1

/-- Every slice operation's base is at least the smallest node charge, 10. -/
def baseAtLeast (op : String) : Nat := max (base op) 10

inductive SizeUnit where
  | elements | weight | rationalLimbs2 | rationalWeight | arguments

/-- One argument's size in a unit (jlvm.md section 7, "Size units"). -/
def units : SizeUnit → Value → Nat
  | .elements, v => v.elements
  | .weight, v => v.weight
  | .rationalLimbs2, .num q =>
    if q.den = 1 then 0 else (8 + bitLength q.num / 64 + natBits q.den / 64) * (8 + bitLength q.num / 64 + natBits q.den / 64)
  | .rationalLimbs2, _ => 0
  | .rationalWeight, .num q => if q.den = 1 then 0 else (Value.num q).weight
  | .rationalWeight, _ => 0
  | .arguments, _ => 1

/-- The rates charged on every argument of a strict operation. -/
def rates : String → List (Nat × SizeUnit)
  | "==" | "!=" | "===" | "!==" => [(1, .weight)]
  | "<" | "<=" | ">" | ">=" => [(1, .weight), (14, .rationalWeight)]
  | "+" => [(4, .weight), (7, .rationalLimbs2), (40, .elements)]
  | "-" => [(3, .weight), (7, .rationalLimbs2), (60, .elements)]
  | "*" => [(10, .weight), (7, .rationalLimbs2), (40, .elements)]
  | "cat" => [(1, .weight), (4, .elements), (14, .arguments)]
  | "merge" => [(8, .weight)]
  | _ => []

/-- A callback's per-element charge on its collection. -/
def perElement : String → Nat
  | "map" => 15
  | "all" => 8
  | _ => 0

/-- `⌈log2(n + 1)⌉ + 1`: a map search's levels. -/
def levels (n : Nat) : Nat := natBits n + 1

/-- `merge`'s charge for merging maps: 0 unless every argument is a map. -/
def merging (vs : List Value) : Nat :=
  let maps := vs.filterMap (fun | .map es => some es | _ => none)
  if vs.isEmpty ∨ maps.length ≠ vs.length then 0 else
    let total := (maps.map List.length).sum
    let later := maps.drop 1
    let entries := (later.map List.length).sum
    let weight := (later.map (fun es => (Value.map es).weight)).foldl gasAdd 0
    gasMul (gasAdd (gasMul 12 entries) (gasMul 2 (weight / 128))) (levels total)

/-- The size charge of `op` on its evaluated arguments. -/
def sized (op : String) (vs : List Value) : Nat :=
  let each := vs.foldl (fun t v => (rates op).foldl (fun t (r, u) => gasAdd t (gasMul r (units u v))) t) 0
  if op = "merge" then gasAdd each (merging vs) else each

/-- The bits of a number's decimal expansion, bounded from its size (`Values.renderedBits`). -/
def renderedBits : Value → Nat
  | .num q => if q.den = 1 then bitLength q.num else bitLength q.num + (7 * natBits q.den + 2) / 3
  | _ => 0

/-- `rendering` per unit of `(6 + ⌊b/64⌋)^2`, for each number rendered. -/
def renderCharge (numbers : List Value) : Nat :=
  numbers.foldl (fun g v => gasAdd g (gasMul renderingGas ((6 + renderedBits v / 64) * (6 + renderedBits v / 64)))) 0

/-! ### The meter and the evaluation monad -/

/-- Gas used, the limit, the distinct warnings (as previews), and a ghost count of evaluation steps. -/
structure Meter where
  used : Nat
  limit : Nat
  warnings : List (String × Nat)
  steps : Nat

/-- How an evaluation fails: an error of the program, or running out of gas, which only `charge` raises. -/
inductive Fault where
  | err (e : Err)
  | outOfGas (required available : Nat)

inductive Res (α : Type) where
  | ok (a : α) (m : Meter)
  | fail (f : Fault) (m : Meter)

def Res.meter {α : Type} : Res α → Meter
  | .ok _ m => m
  | .fail _ m => m

/-- The meter's invariant: within the limit, and at least 10 gas per step. -/
def Inv (m : Meter) : Prop := m.used ≤ m.limit ∧ 10 * m.steps ≤ m.used

/-- A sound step of evaluation, for a limit under the saturation point: it keeps the invariant and the limit, never
lowers the gas used, and runs out of gas only with the whole limit used. -/
def Sound {α : Type} (f : Meter → Res α) : Prop :=
  ∀ m, Inv m → m.limit < satMax →
    Inv (f m).meter ∧ (f m).meter.limit = m.limit ∧ m.used ≤ (f m).meter.used ∧
    (∀ r a m', f m = .fail (.outOfGas r a) m' → m'.used = m'.limit)

structure Ev (α : Type) where
  run : Meter → Res α
  sound : Sound run

def Ev.pure {α : Type} (a : α) : Ev α :=
  ⟨fun m => .ok a m, by
    intro m hm _
    refine ⟨hm, rfl, Nat.le_refl _, ?_⟩
    intro r a m' h; cases h⟩

def Ev.bind {α β : Type} (x : Ev α) (f : α → Ev β) : Ev β :=
  ⟨fun m => match x.run m with
    | .ok a m' => (f a).run m'
    | .fail e m' => .fail e m', by
    intro m hm hl
    have hx := x.sound m hm hl
    cases h : x.run m with
    | ok a m1 =>
      simp only [h, Res.meter] at hx ⊢
      obtain ⟨hi, hlim, hle, _⟩ := hx
      have hf := (f a).sound m1 hi (hlim ▸ hl)
      obtain ⟨hi', hlim', hle', hoog'⟩ := hf
      exact ⟨hi', hlim'.trans hlim, Nat.le_trans hle hle', hoog'⟩
    | fail e m1 =>
      simp only [h, Res.meter] at hx ⊢
      obtain ⟨hi, hlim, hle, hoog⟩ := hx
      refine ⟨hi, hlim, hle, ?_⟩
      intro r a m' he
      cases he
      exact hoog r a m1 rfl⟩

instance : Monad Ev where
  pure := Ev.pure
  bind := Ev.bind

/-- Refuses with a program error (never `OutOfGas`). -/
def fail {α : Type} (e : Err) : Ev α :=
  ⟨fun m => .fail (.err e) m, by
    intro m hm _
    refine ⟨hm, rfl, Nat.le_refl _, ?_⟩
    intro r a m' h; cases h⟩

/-- Pays `g` before the work it pays for (jlvm.md section 7): past the limit, stops with the whole limit used. -/
def charge (g : Nat) : Ev Unit :=
  ⟨fun m =>
    if gasAdd m.used g ≤ m.limit then .ok () { m with used := gasAdd m.used g }
    else .fail (.outOfGas (gasAdd m.used g) m.limit) { m with used := m.limit }, by
    intro m hm hl
    obtain ⟨hu, hs⟩ := hm
    have hle : m.used ≤ gasAdd m.used g := by unfold gasAdd satMax at *; omega
    dsimp only
    split
    · rename_i ht
      exact ⟨⟨ht, Nat.le_trans hs hle⟩, rfl, hle, fun r a m' h => by cases h⟩
    · exact ⟨⟨Nat.le_refl _, Nat.le_trans hs hu⟩, rfl, hu, fun r a m' h => by cases h; rfl⟩⟩

/-- `charge`, counting one evaluation step: every node pays at least 10 when it starts. -/
def chargeStep (g : Nat) (hg : 10 ≤ g) : Ev Unit :=
  ⟨fun m =>
    if gasAdd m.used g ≤ m.limit then .ok () { m with used := gasAdd m.used g, steps := m.steps + 1 }
    else .fail (.outOfGas (gasAdd m.used g) m.limit) { m with used := m.limit }, by
    intro m hm hl
    obtain ⟨hu, hs⟩ := hm
    dsimp only
    split
    · rename_i ht
      have hnosat : gasAdd m.used g = m.used + g := by unfold gasAdd at *; omega
      have hstep : 10 * (m.steps + 1) ≤ gasAdd m.used g := by rw [hnosat]; omega
      exact ⟨⟨ht, hstep⟩, rfl, (by rw [hnosat]; omega : m.used ≤ gasAdd m.used g), fun r a m' h => by cases h⟩
    · exact ⟨⟨Nat.le_refl _, Nat.le_trans hs hu⟩, rfl, hu, fun r a m' h => by cases h; rfl⟩⟩

/-- A preview of caller text (conformance.md section 8): its first 32 code points and its UTF-8 length. -/
def preview (s : String) : String × Nat := (String.ofList (s.toList.take 32), s.utf8ByteSize)

/-- Records a distinct warning, at most 64; costs nothing. -/
def warn (w : String × Nat) : Ev Unit :=
  ⟨fun m => .ok () (if m.warnings.length ≥ maxWarnings ∨ m.warnings.contains w then m
    else { m with warnings := m.warnings ++ [w] }), by
    intro m hm _
    dsimp only
    split
    · exact ⟨hm, rfl, Nat.le_refl _, fun r a m' h => by cases h⟩
    · exact ⟨hm, rfl, Nat.le_refl _, fun r a m' h => by cases h⟩⟩

def liftE {α : Type} : Except Err α → Ev α
  | .ok a => Ev.pure a
  | .error e => fail e

/-! ### Values (`Values.scala`) -/

/-- A number operand within the number bound: not a number is `Type`, too wide `Limit`. -/
def numeric (op : String) : Value → Except Err Rat
  | .num q => if bitLength q.num ≤ maxNumberBits ∧ natBits q.den ≤ maxNumberBits then .ok q else .error .limit
  | _ => .error (.type op)

/-- A number result within the number bound. -/
def numResult (q : Rat) : Except Err Value :=
  if bitLength q.num ≤ maxNumberBits ∧ natBits q.den ≤ maxNumberBits then .ok (.num q) else .error .limit

/-- A built value within the depth and weight bounds (`CanonicalValue.array` and `Values.bounded`). -/
def bounded (v : Value) : Except Err Value :=
  if v.depth > 256 then .error .limit else if v.weight > maxWeight then .error .limit else .ok v

/-- An index segment: `0` or a decimal of at most 9 digits without a leading zero (`Values.index`). -/
def indexOf (seg : String) : Option Nat :=
  let cs := seg.toList
  if cs = ['0'] then some 0
  else match cs with
    | c :: _ => if c ≠ '0' ∧ cs.length ≤ 9 ∧ cs.all Json.isDigit then some (Json.digitValue cs) else none
    | [] => none

/-- The value at a path; anything absent is null. -/
def valueAt (v : Value) : List String → Value
  | [] => v
  | seg :: rest =>
    match v with
    | .map es => match Value.lookup es seg with
      | some x => valueAt x rest
      | none => .null
    | .arr xs => match indexOf seg with
      | some i => match xs[i]? with
        | some x => valueAt x rest
        | none => .null
      | none => .null
    | _ => .null

/-! ### Scope -/

/-- What a node sees: the data, the `$` bindings (innermost first), and the enclosing callbacks' elements. -/
structure Scope where
  data : Value
  bindings : List (String × Value)
  frames : List Value

/-- A segment's count: one, and one more per 32 UTF-16 units. -/
def segmentCount (s : String) : Nat := 1 + utf16Length s / 32

def readCost (segments : Nat) : Nat := gasAdd varAccess (gasMul perPathSegment segments)

/-- Each enclosing frame compares the first segment. -/
def frameCost (sc : Scope) (first : Nat) : Nat := gasMul (gasMul perPathSegment sc.frames.length) first

theorem readCost_ge (n x : Nat) : 10 ≤ gasAdd (readCost n) x := by
  unfold readCost gasAdd varAccess satMax; omega

/-- A plain data path: the innermost frame binding its first key, else the data; ambiguous when it is not the only
place that binds it (`Scope.resolve`). -/
def resolve (sc : Scope) : List String → Value × Bool
  | [] => (sc.frames.head?.getD sc.data, false)
  | key :: rest =>
    let hits := sc.frames.filterMap (fun f => match f with
      | .map es => Value.lookup es key
      | _ => none)
    match hits with
    | inner :: outer => (valueAt inner rest, !outer.isEmpty || valueAt sc.data [key] != .null)
    | [] => (valueAt sc.data (key :: rest), false)

/-- Reads a path already paid for. -/
def lookupRef (ref : VarRef) (dflt : Option Value) (sc : Scope) : Ev Value :=
  let orDefault (v : Value) : Value := if v = .null then dflt.getD .null else v
  match ref with
  | .data p =>
    let (v, ambiguous) := resolve sc p
    if ambiguous then do warn (preview ref.render); Ev.pure (orDefault v) else Ev.pure (orDefault v)
  | .bound n p =>
    match Value.lookup sc.bindings n with
    | some v => Ev.pure (orDefault (valueAt v p))
    | none => match dflt with
      | some d => Ev.pure d
      | none => fail .malformed

def pathCount (p : List String) : Nat := p.foldl (fun n s => n + segmentCount s) 0

/-- `var`'s charge: the access and each segment (a binding name counts as one), and for a plain path each frame it may
search. -/
def readCharge (ref : VarRef) (sc : Scope) : Nat :=
  match ref with
  | .data (first :: rest) => gasAdd (readCost (pathCount (first :: rest))) (frameCost sc (segmentCount first))
  | .data [] => gasAdd (readCost 0) 0
  | .bound n p => gasAdd (readCost (segmentCount n + pathCount p)) 0

theorem readCharge_ge (ref : VarRef) (sc : Scope) : 10 ≤ readCharge ref sc := by
  unfold readCharge; split <;> exact readCost_ge _ _

/-- `var`: its charge, then the read. -/
def read (ref : VarRef) (dflt : Option Value) (sc : Scope) : Ev Value := do
  chargeStep (readCharge ref sc) (readCharge_ge ref sc)
  lookupRef ref dflt sc

/-- A computed path's segment: text is a key, an integer within 64 bits an index (or its decimal key). -/
def segment : Value → Except Err String
  | .text t => .ok t
  | .num q =>
    if q.den = 1 then
      if -(2 ^ 63 : Int) ≤ q.num ∧ q.num < 2 ^ 63 then .ok (toString q.num) else .error (.domain "var")
    else .error (.type "var")
  | _ => .error (.type "var")

def segCountOf : Value → Nat
  | .text t => segmentCount t
  | _ => 1

/-! ### Strict operations (`PureOps.scala`) -/

def unary (op : String) (vs : List Value) (f : Value → Except Err Value) : Except Err Value :=
  match vs with
  | [a] => f a
  | _ => .error (.arity op)

def binary (op : String) (vs : List Value) (f : Value → Value → Except Err Value) : Except Err Value :=
  match vs with
  | [a, b] => f a b
  | _ => .error (.arity op)

/-- Text by code point (the order of UTF-8 bytes), numbers exactly; anything else cannot be ordered. -/
def compareValues (op : String) (a b : Value) : Except Err Ordering :=
  match a, b with
  | .text x, .text y => .ok (compare x y)
  | .num _, .num _ => do
    let x ← numeric op a
    let y ← numeric op b
    pure (if x < y then .lt else if x = y then .eq else .gt)
  | _, _ => .error (.type op)

def ordering (op : String) (vs : List Value) (holds : Ordering → Bool) (between : Bool) : Except Err Value :=
  match vs with
  | [a, b] => do pure (.bool (holds (← compareValues op a b)))
  | [a, b, c] =>
    if between then do
      let x ← compareValues op a b
      let y ← compareValues op b c
      pure (.bool (holds x && holds y))
    else .error (.arity op)
  | _ => .error (.arity op)

/-- An exact decimal, when the rational terminates (`Ratio.toDecimalString`). -/
def decimal (q : Rat) : Option String :=
  if q.den = 1 then some (toString q.num) else
    let rec twos (d k : Nat) (fuel : Nat) : Nat × Nat :=
      match fuel with
      | 0 => (d, k)
      | f + 1 => if d % 2 = 0 ∧ d > 0 then twos (d / 2) (k + 1) f else (d, k)
    let rec fives (d k : Nat) (fuel : Nat) : Nat × Nat :=
      match fuel with
      | 0 => (d, k)
      | f + 1 => if d % 5 = 0 ∧ d > 0 then fives (d / 5) (k + 1) f else (d, k)
    let (d1, a) := twos q.den 0 (natBits q.den)
    let (d2, c) := fives d1 0 (natBits q.den)
    if d2 ≠ 1 then none else
      let s := max a c
      let scaled := q.num.natAbs * 5 ^ (s - c) * 2 ^ (s - a)
      let digits := (toString scaled).toList
      let padded := List.replicate (s + 1 - digits.length) '0' ++ digits
      let intPart := padded.take (padded.length - s)
      let frac := padded.drop (padded.length - s)
      some ((if q.num < 0 then "-" else "") ++ String.ofList intPart ++ "." ++ String.ofList frac)

/-- A piece of `cat`, with its UTF-8 length: text, a number (exact decimal) or a boolean; null is empty. -/
def piece (op : String) : Value → Except Err String
  | .text s => .ok s
  | .null => .ok ""
  | .bool b => .ok (if b then "true" else "false")
  | .num q =>
    if q.den = 1 then
      if bitLength q.num ≤ maxNumberBits then .ok (toString q.num) else .error .limit
    else do
      let r ← numeric op (.num q)
      match decimal r with
      | some s => pure s
      | none => throw (.arithmetic op)
  | _ => .error (.type op)

def isArr : Value → Bool
  | .arr _ => true
  | _ => false

def isBytes : Value → Bool
  | .bytes _ => true
  | _ => false

def isMap : Value → Bool
  | .map _ => true
  | _ => false

/-- An array argument's elements, any other argument itself (`merge` and `cat` of arrays). -/
def spreadOne : Value → List Value
  | .arr xs => xs
  | v => [v]

/-- The numbers `cat` renders, priced before rendering. -/
def catRendered (vs : List Value) : List Value :=
  if !vs.isEmpty ∧ (vs.all isArr ∨ vs.all isBytes) then [] else vs.filter (fun | .num _ => true | _ => false)

/-- `cat`: text, numbers and booleans, null as empty; arrays when every argument is one, bytes likewise. -/
def cat (vs : List Value) : Except Err Value :=
  if !vs.isEmpty ∧ vs.all isArr then bounded (.arr (vs.flatMap spreadOne))
  else if !vs.isEmpty ∧ vs.all isBytes then
    let bs := vs.flatMap (fun | .bytes b => b | _ => [])
    if 1 + bs.length > maxWeight then .error .limit else bounded (.bytes bs)
  else do
    let ps ← vs.mapM (piece "cat")
    let s := String.join ps
    if 1 + s.utf8ByteSize > maxWeight then throw .limit
    bounded (.text s)

/-- `merge`: maps left to right, the last key winning; otherwise arrays concatenated and other values appended. -/
def merge (vs : List Value) : Except Err Value :=
  if vs.all isMap then
    let es := vs.foldl (fun acc v => match v with
      | .map es => es.foldl (fun acc (k, x) => Value.insertEntry k x acc) acc
      | _ => acc) []
    bounded (.map es)
  else bounded (.arr (vs.flatMap spreadOne))

def subtract (vs : List Value) : Except Err Value := do
  let qs ← vs.mapM (numeric "-")
  match qs with
  | [a] => numResult (-a)
  | [a, b] => numResult (a - b)
  | _ => throw (.arity "-")

/-- `+` or `*`, left to right: every intermediate within the number bound, and each step after the first paying the
size charge of its accumulator. -/
def accumulate (op : String) : List Rat → Nat → Rat → Ev Value
  | [], _, acc => liftE (numResult acc)
  | x :: xs, i, acc => do
    if i ≠ 0 then
      let a ← liftE (numResult acc)
      charge (sized op [a])
    let r := if op = "*" then acc * x else acc + x
    let _ ← liftE (numResult r)
    accumulate op xs (i + 1) r

/-- An operation after its size charge. -/
def operate (op : String) (vs : List Value) : Ev Value :=
  if op = "+" ∨ op = "*" then do
    let qs ← liftE (vs.mapM (numeric op))
    match qs with
    | first :: rest => accumulate op rest 0 first
    | [] => Ev.pure (.num (if op = "*" then 1 else 0))
  else if op = "cat" then do
    charge (renderCharge (catRendered vs))
    liftE (cat vs)
  else liftE <| match op with
    | "!" => unary op vs (fun v => .ok (.bool !v.truthy))
    | "!!" => unary op vs (fun v => .ok (.bool v.truthy))
    | "==" | "===" => binary op vs (fun a b => .ok (.bool (a == b)))
    | "!=" | "!==" => binary op vs (fun a b => .ok (.bool (a != b)))
    | "<" => ordering op vs (· == .lt) true
    | "<=" => ordering op vs (· != .gt) true
    | ">" => ordering op vs (· == .gt) false
    | ">=" => ordering op vs (· != .lt) false
    | "-" => subtract vs
    | "merge" => merge vs
    | _ => .error .malformed

/-! ### Evaluation -/

theorem le_gasAdd (a b : Nat) (h : 10 ≤ a) : 10 ≤ gasAdd a b := by unfold gasAdd satMax; omega

theorem baseAtLeast_ge (op : String) : 10 ≤ baseAtLeast op := Nat.le_max_right _ _

mutual
/-- Evaluates a node: every node charges first (a computed path once its path is evaluated), then its parts left to
right, depth first. -/
def eval (n : Node) (sc : Scope) : Ev Value :=
  match n with
  | .const v => do
    chargeStep literalGas (by decide)
    Ev.pure v
  | .read ref d => read ref d sc
  | .readAt p d => do
    let pv ← eval p sc
    match pv with
    | .text t => do
      charge (gasMul splitPerUnit (Value.text t).weight)
      match VarRef.parse t with
      | some r => read r d sc
      | none => fail (.domain "var")
    | .arr ss => do
      let first := (ss.head?.map segCountOf).getD 0
      chargeStep (gasAdd (readCost ((ss.map segCountOf).sum)) (frameCost sc first)) (readCost_ge _ _)
      let p ← liftE (ss.mapM segment)
      lookupRef (.data p) d sc
    | _ => fail (.type "var")
  | .arrayOf items => do
    chargeStep (gasAdd literalGas (gasMul elementGas items.length)) (le_gasAdd _ _ (by decide))
    let vs ← evalAll items sc
    liftE (bounded (.arr vs))
  | .objectOf fields => do
    chargeStep (gasAdd literalGas (gasMul elementGas fields.length)) (le_gasAdd _ _ (by decide))
    let vs ← evalFields fields sc
    liftE (bounded (Value.mapOf vs))
  | .strict op args => do
    chargeStep (baseAtLeast op) (baseAtLeast_ge op)
    let vs ← evalAll args sc
    charge (sized op vs)
    operate op vs
  | .spread op a => do
    chargeStep (baseAtLeast op) (baseAtLeast_ge op)
    let v ← eval a sc
    match v with
    | .arr vs => do
      charge (gasMul elementGas vs.length)
      charge (sized op vs)
      operate op vs
    | _ => fail (.type op)
  | .cond branches otherwise => do
    chargeStep (base "if") (by decide)
    cond branches otherwise sc
  | .logic op args => do
    chargeStep (baseAtLeast op) (baseAtLeast_ge op)
    logic op args sc
  | .each op over body => do
    chargeStep (baseAtLeast op) (baseAtLeast_ge op)
    let c ← eval over sc
    match c with
    | .arr vs => do
      charge (gasMul (perElement op) vs.length)
      iterate op body sc vs 0 (vs.length > 0) []
    | .null => iterate op body sc [] 0 false []
    | _ => fail (.type op)
  | .other _ => fail .malformed
termination_by (sizeOf n, 0)
/-- Arguments left to right. -/
def evalAll (ns : List Node) (sc : Scope) : Ev (List Value) :=
  match ns with
  | [] => Ev.pure []
  | x :: xs => do
    let v ← eval x sc
    let vs ← evalAll xs sc
    Ev.pure (v :: vs)
termination_by (sizeOf ns, 0)
def evalFields (fs : List (String × Node)) (sc : Scope) : Ev (List (String × Value)) :=
  match fs with
  | [] => Ev.pure []
  | (k, x) :: rest => do
    let v ← eval x sc
    let vs ← evalFields rest sc
    Ev.pure ((k, v) :: vs)
termination_by (sizeOf fs, 0)
/-- `if`: each condition tested pays one element; only the branch taken is evaluated. -/
def cond (bs : List (Node × Node)) (otherwise : Node) (sc : Scope) : Ev Value :=
  match bs with
  | (c, t) :: rest => do
    charge elementGas
    let v ← eval c sc
    if v.truthy then eval t sc else cond rest otherwise sc
  | [] => eval otherwise sc
termination_by (sizeOf bs + sizeOf otherwise, 0)
/-- `and` stops at the first falsy value, `or` and `default` at the first truthy one. -/
def logic (op : String) (args : List Node) (sc : Scope) : Ev Value :=
  match args with
  | [] => Ev.pure (if op = "and" then .bool true else if op = "or" then .bool false else .null)
  | x :: rest => do
    let v ← eval x sc
    let decisive := if op = "and" then !v.truthy else v.truthy
    if decisive ∨ (rest.isEmpty ∧ op ≠ "default") then Ev.pure v else logic op rest sc
termination_by (sizeOf args, 0)
/-- A callback's body, once per element, with `$current`, `$index` and the element's frame. -/
def iterate (op : String) (body : Node) (sc : Scope) (vs : List Value) (i : Nat) (nonEmpty : Bool)
    (acc : List Value) : Ev Value :=
  match vs with
  | [] => if op = "map" then liftE (bounded (.arr acc.reverse)) else Ev.pure (.bool nonEmpty)
  | x :: rest => do
    charge callbackRun
    let inner : Scope := { sc with bindings := ("current", x) :: ("index", .num (i : Int)) :: sc.bindings,
                                   frames := x :: sc.frames }
    let r ← eval body inner
    if op = "map" then iterate op body sc rest (i + 1) nonEmpty (r :: acc)
    else if r.truthy then iterate op body sc rest (i + 1) nonEmpty acc
    else Ev.pure (.bool false)
termination_by (sizeOf body, vs.length + 1)
end

/-- The result of an evaluation: the gas used (whether it succeeded or failed), the outcome and the warnings. -/
structure Outcome where
  gasUsed : Nat
  result : Except Fault Value
  warnings : Nat
  steps : Nat

def initial (limit : Nat) : Meter := ⟨0, limit, [], 0⟩

/-- The largest gas limit, `2^53 - 1` (`jlvm-max-gas-limit`): every language holds it exactly. -/
def maxGasLimit : Nat := 2 ^ 53 - 1

/-- Evaluates a compiled program against the data and the host bindings, with a gas limit. A limit over
`maxGasLimit` is `Limit`, with no gas used (jlvm.md section 7). -/
def evaluate (root : Node) (data : Value) (env : List (String × Value)) (limit : Nat) : Outcome :=
  if limit ≤ maxGasLimit then
    match (eval root ⟨data, env, []⟩).run (initial limit) with
    | .ok v m => ⟨m.used, .ok v, m.warnings.length, m.steps⟩
    | .fail f m => ⟨m.used, .error f, m.warnings.length, m.steps⟩
  else ⟨0, .error (.err .limit), 0, 0⟩

end Jlvm
