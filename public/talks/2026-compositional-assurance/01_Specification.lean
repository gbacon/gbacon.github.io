-- Tested with Lean v4.37.0-rc1
-- Act 1 (Slides 4-5): Incomplete requirements vs. a full sorting specification.
--
-- Quick Lean syntax guide:
--   `def` defines a function; `:` announces a type; `:=` gives the body.
--   `List Nat` is a finite list of natural numbers (0, 1, 2, ...); `[]` is empty.
--   `#eval` runs an expression (click its line to read the InfoView).
--   `∀` means "for every input"; `∧` means "and"; `by decide` checks concrete claims.

-- Slide 4: A component that always returns the empty list.
def candidate (_xs : List Nat) : List Nat := []

#eval candidate [3, 1, 2]

-- Slide 5: Complete sorting contract — ordered output AND exact occurrence preservation.
/-- Ascending order, allowing equal entries. -/
abbrev Sorted (xs : List Nat) : Prop := xs.Pairwise (· ≤ ·)

def SortingSpec (f : List Nat → List Nat) : Prop :=
  ∀ xs, Sorted (f xs) ∧ (f xs).Perm xs

-- Sortedness alone accepts `candidate`, even though every input element was lost:
example : Sorted (candidate [3, 1, 2]) := by decide
example : ¬ (candidate [3, 1, 2]).Perm [3, 1, 2] := by decide

-- Therefore `candidate` violates the full specification:
example : ¬ SortingSpec candidate := by
  intro h
  exact (by decide : ¬ (candidate [3, 1, 2]).Perm [3, 1, 2]) (h [3, 1, 2]).2

-- Try it: `Perm` preserves duplicates (unlike set equality).
-- Remove one `1` from the first list below and watch the result flip to `false`.
#eval decide (([1, 1, 2] : List Nat).Perm [2, 1, 1])
