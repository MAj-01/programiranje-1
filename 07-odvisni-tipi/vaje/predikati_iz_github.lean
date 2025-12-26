
variable (α : Type) (p q : α → Prop) (r : Prop)
variable (r : Prop)

-- Izjave napišite na list papirja, nato pa jih dokažite v datoteki.

theorem eq1 : (¬ ∃ x, p x) ↔ (∀ x, ¬ p x) :=
  by
    apply Iff.intro
    · intro h1
      intro x
      intro h2
      apply h1
      exact ( x, h2 )
    · intro h1
      intro h2
      let  ⟨x, h2⟩ := h2
      exact h1
      exact x
      exact h2

theorem eq2 : (r → ∀ x, p x) ↔ (∀ x, r → p x) :=
  sorry

theorem eq3 : r ∧ (∃ x, p x) ↔ (∃ x, r ∧ p x) := by
  sorry

theorem eq4 : r ∨ (∀ x, p x) → (∀ x, r ∨ p x) :=
  sorry

-- Tu pa nam bo v pomoč klasična logika
-- namig: `Classical.byContradiction` in `Classical.em` sta lahko v pomoč
open Classical
#check Classical.byContradiction
#check Classical.em

theorem eq5 : (¬ ∀ x, p x) ↔ (∃ x, ¬ p x) :=
 sorry

theorem eq6 : r ∨ (∀ x, p x) ↔ (∀ x, r ∨ p x) :=
  sorry
