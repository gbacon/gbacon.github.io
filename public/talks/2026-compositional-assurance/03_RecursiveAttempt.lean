-- Tested with Lean v4.37.0-rc1
-- Act 1 (Slides 7-10): Split, merge, and Lean's termination rejection.
--
-- EXPECTED ERROR on `def sort`:
--   Lean rejects termination because a singleton never shrinks (`split [7] = ([7], [])`).
--   Click the red squiggle on `sort` to read the goal `⊢ sizeOf left < sizeOf xs`.
--
-- Try it:
--   1. Click `#eval split [7]` (Slide 10) and `#eval merge [5, 4, 3] [1, 2]` (Slide 8).
--   2. Add `| [x] => [x]` to `sort` (Slide 11) to see Lean's new default-measure goal.
--   (Note: `partial def` would silence the error without proving termination; never
--   evaluate a defective partial sort on a nonempty list.)

-- Slide 7: Alternating split deals elements into two lists (not contiguous halves).
def split : List Nat → List Nat × List Nat
  | [] => ([], [])
  | [x] => ([x], [])
  | x :: y :: xs =>
      let (left, right) := split xs
      (x :: left, y :: right)

#eval split [1, 2, 3, 4, 5]
#eval split [7]

-- Slide 8: Types check that both inputs are `List Nat`, not that they are sorted.
def merge : List Nat → List Nat → List Nat
  | [], ys => ys
  | xs, [] => xs
  | x :: xs, y :: ys =>
      if x < y then x :: merge xs (y :: ys)
      else y :: merge (x :: xs) ys

#eval merge [1, 3, 5] [2, 4]
#eval merge [5, 4, 3] [1, 2]  -- compatible types, unsorted inputs → unsorted output

-- Slide 9: Natural recursive composition — missing the singleton base case.
def sort : List Nat → List Nat
  | [] => []
  | xs =>
      let (left, right) := split xs
      merge (sort left) (sort right)
