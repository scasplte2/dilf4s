import Jlvm

/-
Runs the Lean model against the pinned vectors:
- shared/vectors/canonical_encoding_v1.json: every `json` row (strict parsing, then canonical CBOR, or the refusal's
  kind), every `cborRejections` row through the decoder, and each accepted encoding decoded back;
- shared/vectors/jlvm_v1.json: every case in scope (below), comparing the outcome (the value's canonical CBOR, or the
  error's kind and `op`), the gas used and the number of warnings exactly.

A case is in scope when it has no `built` binding and its program text names only operations of the slice: every
key of a single-key object outside a `{"literal": ...}` is checked, aliases included. The test is on the program's
text alone, never on its outcome; a program whose text is not JSON is in scope, since its outcome is the parser's.
-/

open Jlvm Jlvm.Json

def field (j : Json) (k : String) : Option Json :=
  match j with
  | .obj fs => fs.lookup k
  | _ => none

def str? : Option Json → Option String
  | some (.str s) => some s
  | _ => none

def nat? : Option Json → Option Nat
  | some (.num q) => if q.den = 1 ∧ q.num ≥ 0 then some q.num.toNat else none
  | _ => none

def hexDigit (n : Nat) : Char := if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)

def toHex (bs : List UInt8) : String :=
  String.ofList (bs.flatMap (fun b => [hexDigit (b.toNat / 16), hexDigit (b.toNat % 16)]))

def fromHex (s : String) : Option (List UInt8) :=
  let rec go : List Char → Option (List UInt8)
    | [] => some []
    | a :: b :: rest => do
      let x ← hexVal a
      let y ← hexVal b
      pure (UInt8.ofNat (x * 16 + y) :: (← go rest))
    | [_] => none
  go s.toList

mutual
/-- The operations a program's text names: keys of single-key objects, outside `literal`. -/
def opsIn : Json → List String
  | .obj [(k, a)] => if k = "literal" then [] else ((opNamed k).toList ++ opsIn a)
  | .obj fs => opsInFields fs
  | .arr xs => opsInList xs
  | _ => []
def opsInList : List Json → List String
  | [] => []
  | x :: xs => opsIn x ++ opsInList xs
def opsInFields : List (String × Json) → List String
  | [] => []
  | (_, x) :: fs => opsIn x ++ opsInFields fs
end

def inScope (program : String) (case : Json) : Bool :=
  (field case "built").isNone &&
  match Json.parse program with
  | .ok j => (opsIn j).all (sliceOps.contains ·)
  | .error _ => true

def errName : Err → String × Option String
  | .malformed => ("Malformed", none)
  | .type op => ("Type", some op)
  | .arity op => ("Arity", some op)
  | .arithmetic op => ("Arithmetic", some op)
  | .domain op => ("Domain", some op)
  | .limit => ("Limit", none)

/-- The outcome as the vectors record it: `cbor:<hex>`, or `error:<kind>[:<op>]`. -/
def render : Except Fault Value → String
  | .ok v => "cbor:" ++ toHex (Cbor.encode v)
  | .error (.outOfGas _ _) => "error:OutOfGas"
  | .error (.err e) => match errName e with
    | (k, some op) => s!"error:{k}:{op}"
    | (k, none) => s!"error:{k}"

def expected (o : Json) : String :=
  match str? (field o "cbor") with
  | some h => "cbor:" ++ h
  | none => match str? (field o "error"), str? (field o "op") with
    | some k, some op => s!"error:{k}:{op}"
    | some k, none => s!"error:{k}"
    | none, _ => "?"

/-- A surface value (data, a binding): the strict parser, then `0x`-hex strings as bytes. -/
def surface (text : String) : Except Err Value :=
  match Json.parse text with
  | .ok j => .ok (Json.toValue j)
  | .error _ => .error .malformed

def runCase (case : Json) (fileLimit : Nat) : Option (String × Nat × Nat) := do
  let program ← str? (field case "program")
  let data ← str? (field case "data")
  let env : List (String × String) := match field case "env" with
    | some (.obj fs) => fs.filterMap (fun (k, v) => match v with | .str s => some (k, s) | _ => none)
    | _ => []
  let limit := (nat? (field case "gasLimit")).getD fileLimit
  let parsed : Except Err (Jlvm.Expr × Value × List (String × Value)) := do
    let e ← readProgram program
    let d ← surface data
    let bs ← env.mapM (fun (k, v) => do pure (k, ← surface v))
    pure (e, d, bs)
  -- as the reference does: the program, data and bindings are read, then the limit is checked, then the program is
  -- compiled (JlvmVectors.run, Jlvm.evaluate)
  match parsed with
  | .error e => pure (render (.error (.err e)), 0, 0)
  | .ok (e, d, bs) =>
    if maxGasLimit < limit then pure (render (.error (.err .limit)), 0, 0)
    else match compile e with
      | .error err => pure (render (.error (.err err)), 0, 0)
      | .ok root =>
        let o := evaluate root d bs limit
        pure (render o.result, o.gasUsed, o.warnings)

def checkJlvm (path : String) : IO Bool := do
  let text ← IO.FS.readFile path
  let file ← match Json.parse text with
    | .ok j => pure j
    | .error e => throw (IO.userError s!"{path}: {e.name}")
  let fileLimit := (nat? (field file "gasLimit")).getD (2 ^ 24)
  let cases := match field file "cases" with
    | some (.arr xs) => xs
    | _ => []
  let mut inScopeCount := 0
  let mut passed := 0
  let mut failures : Array String := #[]
  let mut errors := 0
  let mut outOfGas := 0
  for c in cases do
    let program := (str? (field c "program")).getD ""
    if inScope program c then
      inScopeCount := inScopeCount + 1
      let kind := str? ((field c "outcome").bind (field · "error"))
      if kind.isSome then errors := errors + 1
      if kind = some "OutOfGas" then outOfGas := outOfGas + 1
      let want := (expected ((field c "outcome").getD .null), (nat? (field c "gasUsed")).getD 0,
        (nat? (field c "warnings")).getD 0)
      match runCase c fileLimit with
      | some got =>
        if got == want then passed := passed + 1
        else failures := failures.push s!"{(str? (field c "name")).getD "?"}: got {got}, want {want}"
      | none => failures := failures.push s!"{(str? (field c "name")).getD "?"}: unreadable case"
  IO.println s!"jlvm_v1.json: {cases.length} cases, {inScopeCount} in scope ({inScopeCount - errors} values, {errors} errors, {outOfGas} of them OutOfGas), {passed} pass, {failures.size} fail"
  for f in failures.toList.take 40 do IO.println s!"  FAIL {f}"
  pure (failures.isEmpty && inScopeCount > 0)

/-- An accepted row's encoding, decoded: the value its JSON parses to, or a failure. -/
def decodeBack (r : Json) (input : String) : Option (Except String Unit) :=
  match str? (field r "cbor"), Json.parse input with
  | some h, .ok j =>
    some <| match fromHex h with
      | some bs => match Cbor.decode bs with
        | .ok v => if v == Json.toCanonical j then .ok () else .error s!"decode {h.take 40}: another value"
        | .error e => .error s!"decode {h.take 40}: {e.name}"
      | none => .error s!"bad hex {h.take 40}"
  | _, _ => none

def checkEncoding (path : String) : IO Bool := do
  let text ← IO.FS.readFile path
  let file ← match Json.parse text with
    | .ok j => pure j
    | .error e => throw (IO.userError s!"{path}: {e.name}")
  let rows (k : String) : List Json := match field file k with
    | some (.arr xs) => xs
    | _ => []
  let mut ok := 0
  let mut failures : Array String := #[]
  for r in rows "json" do
    let input := (str? (field r "json")).getD ""
    let got := match Json.parse input with
      | .ok j => "cbor:" ++ toHex (Cbor.encode (Json.toCanonical j))
      | .error e => "error:" ++ e.name
    let want := match str? (field r "cbor"), str? (field r "error") with
      | some h, _ => "cbor:" ++ h
      | none, some e => "error:" ++ e
      | none, none => "?"
    if got == want then ok := ok + 1 else failures := failures.push s!"json {input.take 60}: got {got}, want {want}"
    -- an accepted encoding decodes back to the value parsed
    match decodeBack r input with
    | some (.ok ()) => ok := ok + 1
    | some (.error msg) => failures := failures.push msg
    | none => pure ()
  for r in rows "cborRejections" do
    let h := (str? (field r "cbor")).getD ""
    let want := (str? (field r "error")).getD "?"
    let got := match fromHex h with
      | some bs => match Cbor.decode bs with
        | .ok _ => "accepted"
        | .error e => e.name
      | none => "bad hex"
    if got == want then ok := ok + 1 else failures := failures.push s!"cbor {h.take 40}: got {got}, want {want}"
  IO.println s!"canonical_encoding_v1.json: {ok} checks pass, {failures.size} fail"
  for f in failures.toList.take 40 do IO.println s!"  FAIL {f}"
  pure failures.isEmpty

/-- Theorem 2 counts paid steps only: 50 computed paths around a literal enter 51 nodes, pay one step (the literal's 10
gas) and fail at the innermost path, as the reference does (README). -/
def checkUnpaidChain : IO Bool := do
  let program := String.join (List.replicate 50 "{\"var\":") ++ "{\"literal\":true}" ++ String.join (List.replicate 50 "}")
  match parseProgram program with
  | .ok root =>
    let o := evaluate root .null [] (2 ^ 24)
    let ok := render o.result == "error:Type:var" && o.gasUsed == 10 && o.steps == 1
    IO.println s!"unpaid computed-path chain: {render o.result}, gas {o.gasUsed}, steps {o.steps}: {if ok then "as stated" else "FAIL"}"
    pure ok
  | .error _ =>
    IO.println "unpaid computed-path chain: FAIL (does not parse)"
    pure false

def main (args : List String) : IO UInt32 := do
  let dir := args.headD "../shared/vectors"
  let enc ← checkEncoding s!"{dir}/canonical_encoding_v1.json"
  let jlvm ← checkJlvm s!"{dir}/jlvm_v1.json"
  let chain ← checkUnpaidChain
  pure (if enc && jlvm && chain then 0 else 1)
