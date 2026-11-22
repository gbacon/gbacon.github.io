-- Tested with Lean v4.37.0-rc1
-- Act 1 (Slides 11-14): Repaired mergesort and universal correctness.
-- Self-contained export of the verified `CompositionalAssurance.Sorting` proofs
-- (zero imports, standard Lean 4 only). Every finite `List Nat` — duplicates included —
-- is proved to satisfy `SortingSpec sort`.

namespace CompositionalAssurance.Sorting

-- 1. Alternating split (polymorphic in the library; `sort` specializes to `List Nat`).
-- [Split.lean]
def split : List α → List α × List α
  | [] => ([], [])
  | [x] => ([x], [])
  | x :: y :: xs =>
      let (left, right) := split xs
      (x :: left, y :: right)

-- 2. Ordered merge (equality takes the right head).
-- [Merge.lean]
-- Equality follows the right branch, as in Koenig's example.
def merge [LT α] [DecidableLT α] : List α → List α → List α
  | [], ys => ys
  | xs, [] => xs
  | x :: xs, y :: ys =>
      if x < y then x :: merge xs (y :: ys)
      else y :: merge (x :: xs) ys

-- 3. Singleton repair & length-based termination (Slides 11-12).
--    Click before `omega` inside `decreasing_by` to see Slide 12's length goal,
--    which mirrors `strict_progress` from Slide 6.
-- [Repaired.lean]
-- Non-strict bounds suffice because recursion consumes at least two elements.
theorem split_lengths (xs : List α) :
    (split xs).1.length ≤ xs.length ∧
      (split xs).2.length ≤ xs.length
    := by
  match xs with
  | [] | [_] => simp [split]
  | x :: y :: xs =>
      have := split_lengths xs
      simp only [split, List.length_cons]
      omega

def sort : List Nat → List Nat
  | [] => []
  | [x] => [x]
  | x :: y :: xs =>
      let halves := split (x :: y :: xs)
      merge (sort halves.1) (sort halves.2)
termination_by xs => xs.length
decreasing_by
  all_goals
    have := split_lengths xs
    simp only [split, List.length_cons]
    omega

-- Executable regression checks for these examples, not a correctness proof.
#guard sort ([] : List Nat) == []
#guard sort [7] == [7]
#guard sort [1, 2, 3, 4] == [1, 2, 3, 4]
#guard sort [5, 4, 3, 2, 1] == [1, 2, 3, 4, 5]
#guard sort [3, 1, 3, 2, 1] == [1, 1, 2, 3, 3]

-- 4. Split contracts: occurrence preservation and strict length decrease.
-- [SplitProperties.lean]
/-- Alternating distribution preserves every occurrence. -/
theorem split_perm (xs : List α) :
    ((split xs).1 ++ (split xs).2).Perm xs := by
  match xs with
  | [] => simp [split]
  | [x] => simp [split]
  | x :: y :: xs =>
      have ih := split_perm xs
      simp only [split, List.cons_append]
      exact (List.perm_middle).cons x |>.trans ((ih.cons y).cons x)

/-- On the recursive branch both pieces are strictly shorter. -/
theorem split_shorter (x y : α) (xs : List α) :
    (split (x :: y :: xs)).1.length < (x :: y :: xs).length ∧
    (split (x :: y :: xs)).2.length < (x :: y :: xs).length := by
  have := split_lengths xs
  simp only [split, List.length_cons]
  omega

-- 5. Specification & merge contracts (Slide 13: `merge_sorted` needs BOTH inputs sorted).
-- [MergeProperties.lean]
/-- Ascending order, allowing equal entries. This is the discovery contract. -/
abbrev Sorted (xs : List Nat) : Prop := xs.Pairwise (· ≤ ·)

def SortingSpec (f : List Nat → List Nat) : Prop :=
  ∀ xs, Sorted (f xs) ∧ (f xs).Perm xs

/-- Merge preserves occurrences, even when its inputs are not sorted. -/
theorem merge_perm (left right : List Nat) :
    (merge left right).Perm (left ++ right) := by
  match left, right with
  | [], ys => simp [merge]
  | x :: xs, [] => simp [merge]
  | x :: xs, y :: ys =>
      by_cases h : x < y
      · simp only [merge, ite_eq_left h, List.cons_append]
        exact (merge_perm xs (y :: ys)).cons x
      · simp only [merge, ite_eq_right h]
        exact ((merge_perm (x :: xs) ys).cons y).trans List.perm_middle.symm
termination_by left.length + right.length

/-- A common lower bound on the inputs is retained by merge. -/
theorem merge_lower_bound (a : Nat) (left right : List Nat)
    (hl : ∀ b ∈ left, a ≤ b) (hr : ∀ b ∈ right, a ≤ b) :
    ∀ b ∈ merge left right, a ≤ b := by
  intro b hb
  exact (List.mem_append.mp ((merge_perm left right).mem_iff.mp hb)).elim (hl b) (hr b)

/-- Merge requires ordered inputs to guarantee an ordered output. -/
theorem merge_sorted (left right : List Nat)
    (hl : Sorted left) (hr : Sorted right) :
    Sorted (merge left right) := by
  match left, right with
  | [], ys => simpa [merge] using hr
  | x :: xs, [] => simpa [merge] using hl
  | x :: xs, y :: ys =>
      obtain ⟨hx, hxs⟩ := List.pairwise_cons.mp hl
      obtain ⟨hy, hys⟩ := List.pairwise_cons.mp hr
      simp only [merge]
      split
      · refine List.pairwise_cons.mpr ⟨merge_lower_bound x xs (y :: ys) hx ?_,
          merge_sorted xs (y :: ys) hxs hr⟩
        intro b hb
        rcases List.mem_cons.mp hb with rfl | hb
        · omega
        · have := hy b hb; omega
      · refine List.pairwise_cons.mpr ⟨merge_lower_bound y (x :: xs) ys ?_ hy,
          merge_sorted (x :: xs) ys hl hys⟩
        intro b hb
        rcases List.mem_cons.mp hb with rfl | hb
        · omega
        · have := hx b hb; omega
termination_by left.length + right.length

-- 6. Composing component contracts into universal correctness (Slide 14).
--    `fun_induction sort xs` reuses `sort`'s recursion structure directly.
-- [Correctness.lean]
/-- Recursive sorting and merging preserve every occurrence. -/
theorem sort_perm (xs : List Nat) : (sort xs).Perm xs := by
  fun_induction sort xs
  · simp
  · simp
  · refine (merge_perm _ _).trans (List.Perm.trans ?_ (split_perm _))
    exact List.Perm.append (by assumption) (by assumption)

/-- Recursive results discharge merge's two sorted-input preconditions. -/
theorem sort_sorted (xs : List Nat) : Sorted (sort xs) := by
  fun_induction sort xs
  · simp [Sorted]
  · simp [Sorted]
  · exact merge_sorted _ _ (by assumption) (by assumption)

/-- Universal correctness of the existing repaired mergesort, not a finite test. -/
theorem mergeSort_correct (xs : List Nat) :
    Sorted (sort xs) ∧ (sort xs).Perm xs :=
  ⟨sort_sorted xs, sort_perm xs⟩

/-- The algorithm satisfies the same contract used in the discovery sequence. -/
theorem mergeSort_satisfies_spec : SortingSpec sort := mergeSort_correct

-- Concrete checks (illustrative) and the universal theorem signature:
#eval sort [5, 4, 3, 2, 1]
#eval sort [3, 1, 3, 2, 1]
#check mergeSort_correct

end CompositionalAssurance.Sorting
