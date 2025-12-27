
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
      exact ⟨x, h2⟩
    · intro h1
      intro h2
      let  ⟨x, h2⟩ := h2
      exact h1 x h2


theorem eq2 : (r → ∀ x, p x) ↔ (∀ x, r → p x) :=
  by
    apply Iff.intro
    intro h x r
    exact h r x
    intro h r x
    exact h x r

theorem eq3 : r ∧ (∃ x, p x) ↔ (∃ x, r ∧ p x) := by
    apply Iff.intro
    intro ⟨ hr, ⟨ x, hp ⟩ ⟩
    apply Exists.intro x ⟨ hr, hp ⟩
    intro ⟨ x, ⟨ hr, hp ⟩ ⟩
    constructor
    exact hr
    exists x


theorem eq4 : r ∨ (∀ x, p x) → (∀ x, r ∨ p x) :=
  by
    intro h x
    cases h
    case inl hr => exact Or.inl hr
    case inr hpx => exact Or.inr (hpx x)

-- Tu pa nam bo v pomoč klasična logika
-- namig: `Classical.byContradiction` in `Classical.em` sta lahko v pomoč
open Classical
#check Classical.byContradiction
#check Classical.em

theorem eq5 : (¬ ∀ x, p x) ↔ (∃ x, ¬ p x) :=
 by
   apply Iff.intro
   intro h
   apply Classical.byContradiction
   intro npx
   apply h
   intro x
   apply Classical.byContradiction
   intro px
   apply npx
   exact ⟨ x, px ⟩
   intro h npx
   let ⟨ x, npx' ⟩ := h
   apply npx' (npx x)


theorem eq6 : r ∨ (∀ x, p x) ↔ (∀ x, r ∨ p x) :=
  by
    apply Iff.intro
    intro h x
    cases h
    case inl hr => exact Or.inl hr
    case inr hpx => exact Or.inr (hpx x)
    intro h
    -- Can't do cases h
    have x := Classical.em r
    cases x
    case inl hr => exact Or.inl hr
    case inr nhr =>
      right
      intro x
      have xx := h x
      cases xx
      case inl hr => contradiction
      case inr hpx => exact hpx
