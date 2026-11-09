import Jlvm.Eval

/-
The theorems of the spike (formal/README.md states them in plain words).

Termination is not a theorem here but a property Lean's kernel checks: `eval` and its helpers are defined by
well-founded recursion, with no `partial` and no fuel, on (the size of the node, the elements a callback has left),
so every evaluation terminates. The theorems below bound what it does.
-/

namespace Jlvm

theorem initial_inv (limit : Nat) : Inv (initial limit) := ⟨Nat.zero_le _, Nat.le_refl _⟩

/-- The evaluation's soundness, read off the meter it ends with. -/
theorem evaluate_sound (root : Node) (data : Value) (env : List (String × Value)) (limit : Nat)
    (hl : limit < satMax) :
    let r := (eval root ⟨data, env, []⟩).run (initial limit)
    Inv r.meter ∧ r.meter.limit = limit ∧
      (∀ q a m', r = .fail (.outOfGas q a) m' → m'.used = m'.limit) :=
  let h := (eval root ⟨data, env, []⟩).sound (initial limit) (initial_inv limit) hl
  ⟨h.1, h.2.1, h.2.2.2⟩

/-- The largest gas limit lies below the saturation point, so a limit an evaluation accepts never saturates. -/
theorem maxGasLimit_lt_satMax : maxGasLimit < satMax := by decide

/-- **Theorem 1 (gas).** For any program, data, bindings and limit, the gas used never exceeds the limit, and an
evaluation that runs out of gas reports the whole limit as used (jlvm.md section 7, zk-jlvm.md section 2). A limit
over `2^53 - 1` is refused with no gas used, so no accepted limit reaches `2^63 - 1`, where gas saturates. -/
theorem gas_within_limit (root : Node) (data : Value) (env : List (String × Value)) (limit : Nat) :
    (evaluate root data env limit).gasUsed ≤ limit ∧
      (∀ q a, (evaluate root data env limit).result = .error (.outOfGas q a) →
        (evaluate root data env limit).gasUsed = limit) := by
  unfold evaluate
  split
  · rename_i hmax
    obtain ⟨hinv, hlim, hoog⟩ := evaluate_sound root data env limit (Nat.lt_of_le_of_lt hmax maxGasLimit_lt_satMax)
    split
    · rename_i v m h
      simp only [h, Res.meter] at hinv hlim
      exact ⟨hlim ▸ hinv.1, fun q a he => by cases he⟩
    · rename_i f m h
      simp only [h, Res.meter] at hinv hlim hoog
      refine ⟨hlim ▸ hinv.1, fun q a he => ?_⟩
      cases he
      exact hlim ▸ hoog q a m rfl
  · exact ⟨Nat.zero_le _, fun q a he => by cases he⟩

/-- **Theorem 2 (gas bounds paid work).** `steps` counts the node evaluations that paid their charge, each at least
10 gas (`chargeStep`), so there are at most `gasUsed / 10` of them, and so at most `limit / 10`, whatever the program
and its data. Every node pays before it evaluates any of its parts, except a computed path (`{"var": e}`), which pays
once `e` is evaluated (jlvm.md section 7). An evaluation that fails inside a computed path may therefore also have
entered, unpaid, the computed-path nodes around the failure: one chain of them, at most the program's nesting (256),
which is bounded before anything runs. That bound is not proved here (README). -/
theorem steps_bounded_by_gas (root : Node) (data : Value) (env : List (String × Value)) (limit : Nat) :
    10 * (evaluate root data env limit).steps ≤ (evaluate root data env limit).gasUsed ∧
      10 * (evaluate root data env limit).steps ≤ limit := by
  unfold evaluate
  split
  · rename_i hmax
    obtain ⟨hinv, hlim, _⟩ := evaluate_sound root data env limit (Nat.lt_of_le_of_lt hmax maxGasLimit_lt_satMax)
    split
    · rename_i v m h
      simp only [h, Res.meter] at hinv hlim
      exact ⟨hinv.2, hlim ▸ Nat.le_trans hinv.2 hinv.1⟩
    · rename_i f m h
      simp only [h, Res.meter] at hinv hlim
      exact ⟨hinv.2, hlim ▸ Nat.le_trans hinv.2 hinv.1⟩
  · exact ⟨Nat.le_refl _, Nat.zero_le _⟩

end Jlvm

-- No `sorry` and no axiom beyond Lean's standard ones (propext, Classical.choice, Quot.sound): CI checks this output.
#print axioms Jlvm.gas_within_limit
#print axioms Jlvm.steps_bounded_by_gas
