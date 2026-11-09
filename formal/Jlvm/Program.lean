import Jlvm.Json

/-
Programs (docs/specs/jlvm.md section 1): the syntax tree, the JSON surface parser, the bounds checked before anything
runs, and compilation into the evaluation form (`Node`), whose shape errors are `Malformed` with no gas used.
The parser and the bounds follow the reference (`Parser.scala`, `Ast.scala`, `Node.scala`); only evaluation is
restricted to the slice (`Eval.lean`).
-/

namespace Jlvm

open Json

-- the match equations named in the parser are used by its termination proofs
set_option linter.unusedVariables false

/-- The operations of language version 1 (`jlvm-op` in shared/schemas/jlvm.cddl). -/
def opNames : List String :=
  ["if", "and", "or", "!", "!!", "default", "let", "==", "!=", "===", "!==", "<", "<=", ">", ">=", "+", "-", "*", "/",
   "%", "min", "max", "abs", "round", "round_half_up", "floor", "ceil", "pow", "map", "filter", "reduce", "all", "some",
   "none", "find", "count", "merge", "in", "intersect", "unique", "slice", "reverse", "flatten", "length", "cat",
   "substr", "lower", "upper", "join", "split", "trim", "startsWith", "endsWith", "keys", "values", "entries", "get",
   "has", "set", "unset", "exists", "missing", "missing_some", "typeof", "hex", "unhex", "hex_to_int", "bytes_to_int",
   "int_to_bytes", "sha256", "sha3_256", "keccak256", "blake2b256", "ed25519_verify", "secp256k1_verify",
   "bip340_verify", "ecrecover", "bls_verify", "bls_aggregate_verify", "poseidon", "pmt_verify", "groth16_verify",
   "ecvrf_verify", "bn254_add", "bn254_mul", "bn254_pairing", "schnorr_verify", "sigma_verify", "merkle_verify",
   "merkle_consistency_verify", "trie_verify", "trie_prefix_verify", "trie_range_verify"]

/-- Surface aliases, recorded as the operation's own name. -/
def aliasOf : String → Option String
  | "getKey" => some "get"
  | "setKey" => some "set"
  | "deleteKey" => some "unset"
  | "size" => some "length"
  | _ => none

/-- The operation a surface name denotes: its own name, or an alias. -/
def opNamed (s : String) : Option String := if opNames.contains s then some s else aliasOf s

/-- Names that read like operations but are not (jlvm.md section 1). -/
def removedNames : List String :=
  ["prove_dlog_verify", "prove_dhtuple_verify", "mpt_verify", "mpt_prefix_verify", "smt_verify", "noop"]

/-- The operations whose single non-array argument is spread. -/
def variadic : List String := ["+", "*", "min", "max", "cat", "merge"]

/-- Names that may not head an array, which would read as a call. -/
def callName (s : String) : Bool := s = "var" || (opNamed s).isSome || removedNames.contains s

/-- A `var` path: `$name.path` reads a binding; any other path reads the data. -/
inductive VarRef where
  | data (path : List String)
  | bound (name : String) (path : List String)
  deriving DecidableEq, Repr, Inhabited

def VarRef.render : VarRef → String
  | .data p => ".".intercalate p
  | .bound n p => ".".intercalate (("$" ++ n) :: p)

/-- The `.`-separated segments of a path; `""` is the whole data. An empty segment, or an empty binding name, does not
parse. -/
def VarRef.parse (s : String) : Option VarRef :=
  if s.isEmpty then some (.data []) else
    let segs := s.splitOn "."
    if segs.any (·.isEmpty) then none else
      match segs with
      | h :: rest =>
        if h.startsWith "$" then
          let name := h.drop 1 |>.toString
          if name.isEmpty then none else some (.bound name rest)
        else some (.data segs)
      | [] => some (.data [])

/-- Parse and run-time errors (jlvm.md section 3). `op` is the wire name of the operation that refused, or `var`. -/
inductive Err where
  | malformed
  | type (op : String)
  | arity (op : String)
  | arithmetic (op : String)
  | domain (op : String)
  | limit
  deriving DecidableEq, Repr, Inhabited

/-- The syntax tree of a program. Map expressions' fields are in code-point order of their keys. -/
inductive Expr where
  | lit (v : Value)
  | var (ref : VarRef) (dflt : Option Value)
  | varAt (path : Expr) (dflt : Option Value)
  | apply (op : String) (args : List Expr)
  | spread (op : String) (arg : Expr)
  | arr (items : List Expr)
  | obj (fields : List (String × Expr))
  deriving Inhabited, Repr

/-- Insertion in code-point order of the keys (the reference's `Kernels.keys`). -/
def insertField {α : Type} (k : String) (x : α) : List (String × α) → List (String × α)
  | [] => [(k, x)]
  | (k', y) :: fs => if k < k' then (k, x) :: (k', y) :: fs else (k', y) :: insertField k x fs

def sortFields {α : Type} (fs : List (String × α)) : List (String × α) :=
  fs.foldl (fun acc (k, x) => insertField k x acc) []

/-- A scalar, literal or default: its JSON value, with `0x`-hex strings as bytes. -/
def literal (j : Json) : Value := Json.toValue j

/-- A default is a literal value: an object or array default is quoted. -/
def defaultValue : Json → Except Err Value
  | .obj [("literal", x)] => .ok (literal x)
  | .obj _ => .error .malformed
  | .arr _ => .error .malformed
  | j => .ok (literal j)

mutual
/-- One JSON value of the surface as an expression (`Parser.expression`). -/
def expression : Json → Except Err Expr
  | .null => .ok (.lit .null)
  | .bool b => .ok (.lit (.bool b))
  | .num q => .ok (.lit (.num q))
  | .str s => .ok (.lit (literal (.str s)))
  | .arr items => do
    match items with
    | .str s :: _ => if callName s then throw .malformed else pure ()
    | _ => pure ()
    .arr <$> elementsOf items
  | .obj [(k, a)] => single k a
  | .obj fs => .obj <$> sortFields <$> fieldsOf fs
termination_by j => (sizeOf j, 1)
def elementsOf : List Json → Except Err (List Expr)
  | [] => .ok []
  | x :: xs => do pure ((← expression x) :: (← elementsOf xs))
termination_by xs => (sizeOf xs, 1)
def fieldsOf : List (String × Json) → Except Err (List (String × Expr))
  | [] => .ok []
  | (k, x) :: fs => do pure ((k, ← expression x) :: (← fieldsOf fs))
termination_by fs => (sizeOf fs, 1)
/-- A single-key object: a keyword, an operation, a `_` map expression, or a parse error. -/
def single (k : String) (a : Json) : Except Err Expr :=
  if k = "literal" then .ok (.lit (literal a))
  else if k = "var" then varForm a
  else if k = "object" then
    match h : a with
    | .obj fs => .obj <$> sortFields <$> fieldsOf fs
    | _ => .error .malformed
  else if k.startsWith "_" then (fun x => .obj [(k, x)]) <$> expression a
  else if removedNames.contains k then .error .malformed
  else match opNamed k with
    | none => .error .malformed
    | some op =>
      match h : a with
      | .arr items => .apply op <$> elementsOf items
      | _ => if variadic.contains op then .spread op <$> expression a else (fun x => .apply op [x]) <$> expression a
termination_by (sizeOf a, 4)
decreasing_by all_goals (try subst h) <;> simp_wf <;> omega
/-- `{"var": p}` and `{"var": [p, default]}`. -/
def varForm : Json → Except Err Expr
  | .arr [p] => path p none
  | .arr [p, d] => do
    let dv ← defaultValue d
    path p (if dv = .null then none else some dv)
  | .arr _ => .error .malformed
  | p => path p none
termination_by j => (sizeOf j, 3)
/-- Text or an integer is a path; an object or array computes one. -/
def path (j : Json) (d : Option Value) : Except Err Expr :=
  match h : j with
  | .str s => match VarRef.parse s with
    | some r => .ok (.var r d)
    | none => .error .malformed
  | .num q =>
    if q.den = 1 then
      match VarRef.parse (toString q.num) with
      | some r => .ok (.var r d)
      | none => .error .malformed
    else .error .malformed
  | .obj _ => (fun x => .varAt x d) <$> expression j
  | .arr items => (fun xs => .varAt (.arr xs) d) <$> elementsOf items
  | _ => .error .malformed
termination_by (sizeOf j, 2)
decreasing_by all_goals (try subst h) <;> simp_wf <;> omega
end

/-! ### Bounds checked before anything runs (`Program.from`) -/

/-- The largest program (`Program.MaxSize`, 2^18). -/
def maxProgramSize : Nat := 2 ^ 18

def segSize (p : List String) : Nat := p.foldl (fun n s => n + 1 + utf16Length s) 0

mutual
/-- One per expression, plus the weight of each literal and default, plus the length (UTF-16 units) of each object
key, binding name and path segment, plus one per segment. -/
def Expr.size : Expr → Nat
  | .lit v => 1 + v.weight
  | .var (.data p) d => 1 + segSize p + (d.map Value.weight).getD 0
  | .var (.bound n p) d => 2 + utf16Length n + segSize p + (d.map Value.weight).getD 0
  | .varAt x d => 1 + (d.map Value.weight).getD 0 + x.size
  | .apply _ as => 1 + Expr.sizeList as
  | .spread _ a => 1 + a.size
  | .arr xs => 1 + Expr.sizeList xs
  | .obj fs => 1 + Expr.sizeFields fs
def Expr.sizeList : List Expr → Nat
  | [] => 0
  | x :: xs => x.size + Expr.sizeList xs
def Expr.sizeFields : List (String × Expr) → Nat
  | [] => 0
  | (k, x) :: fs => 1 + utf16Length k + x.size + Expr.sizeFields fs
end

mutual
/-- Expression nesting: arguments, items and fields are one level deeper (`Program.MaxDepth` bounds it at 256). -/
def Expr.nesting : Expr → Nat
  | .varAt x _ => 1 + x.nesting
  | .apply _ as => 1 + Expr.nestingList as
  | .spread _ a => 1 + a.nesting
  | .arr xs => 1 + Expr.nestingList xs
  | .obj fs => 1 + Expr.nestingFields fs
  | _ => 0
def Expr.nestingList : List Expr → Nat
  | [] => 0
  | x :: xs => max x.nesting (Expr.nestingList xs)
def Expr.nestingFields : List (String × Expr) → Nat
  | [] => 0
  | (_, x) :: fs => max x.nesting (Expr.nestingFields fs)
end

mutual
/-- The depth of the expression's canonical form (shared/schemas/jlvm.cddl): each node is a map, an argument list an
array, a literal or default a value inside its map. -/
def Expr.canonicalDepth : Expr → Nat
  | .lit .null => 1
  | .lit v => 1 + v.depth
  | .var _ d => 1 + (d.map Value.depth).getD 0
  | .varAt x d => 1 + max x.canonicalDepth ((d.map Value.depth).getD 0)
  | .apply _ as => 2 + Expr.canonicalDepthList as
  | .spread _ a => 1 + a.canonicalDepth
  | .arr xs => 2 + Expr.canonicalDepthList xs
  | .obj fs => 2 + Expr.canonicalDepthFields fs
def Expr.canonicalDepthList : List Expr → Nat
  | [] => 0
  | x :: xs => max x.canonicalDepth (Expr.canonicalDepthList xs)
def Expr.canonicalDepthFields : List (String × Expr) → Nat
  | [] => 0
  | (_, x) :: fs => max x.canonicalDepth (Expr.canonicalDepthFields fs)
end

/-- The bounds of `Program.from`, all `Limit`: size, expression nesting, and the canonical form's depth (the program
record `{"jlvm": 1, "expr": ...}` adds one level). The rendered surface's depth is not modelled (README). -/
def checkProgram (e : Expr) : Except Err Unit :=
  if e.size > maxProgramSize then .error .limit
  else if e.nesting > 256 then .error .limit
  else if 1 + e.canonicalDepth > 256 then .error .limit
  else .ok ()

/-! ### Compilation (`Node.compile`) -/

/-- The evaluation form. `other` is an operation outside the modelled slice: no in-scope program has one. -/
inductive Node where
  | const (v : Value)
  | read (ref : VarRef) (dflt : Option Value)
  | readAt (path : Node) (dflt : Option Value)
  | arrayOf (items : List Node)
  | objectOf (fields : List (String × Node))
  | strict (op : String) (args : List Node)
  | spread (op : String) (arg : Node)
  | cond (branches : List (Node × Node)) (otherwise : Node)
  | logic (op : String) (args : List Node)
  | each (op : String) (over body : Node)
  | other (op : String)
  deriving Inhabited, Repr

/-- The strict operations of the slice. -/
def strictOps : List String := ["!", "!!", "==", "!=", "===", "!==", "<", "<=", ">", ">=", "+", "-", "*", "cat", "merge"]

/-- Every operation the slice evaluates. -/
def sliceOps : List String := ["if", "and", "or", "default", "map", "all"] ++ strictOps

def pairs : List Node → List (Node × Node)
  | c :: t :: rest => (c, t) :: pairs rest
  | _ => []

mutual
def compile : Expr → Except Err Node
  | .lit v => .ok (.const v)
  | .var r d => .ok (.read r d)
  | .varAt p d => (fun n => .readAt n d) <$> compile p
  | .spread op a => do
    let n ← compile a
    match n with
    | .const (.arr _) => pure (.spread op n)
    | .const _ => throw .malformed
    | .objectOf _ => throw .malformed
    | _ => pure (.spread op n)
  | .arr xs => .arrayOf <$> compileList xs
  | .obj fs => .objectOf <$> compileFields fs
  | .apply op args => do
    if op = "if" then
      let ns ← compileList args
      if ns.length ≥ 2 ∧ ns.length % 2 = 0 then pure (.cond (pairs ns) (.const .null))
      else if ns.length ≥ 3 then pure (.cond (pairs ns.dropLast) (ns.getLast?.getD (.const .null)))
      else throw .malformed
    else if op = "and" ∨ op = "or" ∨ op = "default" then .logic op <$> compileList args
    else if op = "map" ∨ op = "all" then
      match args with
      | [over, body] => pure (.each op (← compile over) (← compile body))
      | _ => throw .malformed
    else if strictOps.contains op then .strict op <$> compileList args
    else pure (.other op)
def compileList : List Expr → Except Err (List Node)
  | [] => .ok []
  | x :: xs => do pure ((← compile x) :: (← compileList xs))
def compileFields : List (String × Expr) → Except Err (List (String × Node))
  | [] => .ok []
  | (k, x) :: fs => do pure ((k, ← compile x) :: (← compileFields fs))
end

/-- Program text to its evaluation form: the strict JSON parser (a depth refusal is `Limit`, anything else
`Malformed`), the surface, the bounds, then compilation. -/
def parseProgram (text : String) : Except Err Node := do
  let j ← match Json.parse text with
    | .ok j => pure j
    | .error .depthExceeded => throw .limit
    | .error _ => throw .malformed
  let e ← expression j
  checkProgram e
  compile e

end Jlvm
