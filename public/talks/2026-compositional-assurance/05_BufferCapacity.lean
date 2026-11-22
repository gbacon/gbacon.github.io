-- Tested with Lean v4.37.0-rc1
-- Act 2, Scene 1 (Slide 18): First component — a buffer with room to receive.
--
-- Modeled scenario inspired by Wooley et al. (2025); hypothetical 1-place teaching buffer.
-- Parameter roles in `receive_capacity`:
--   `b` : current buffer contents   `p` : arriving pallet
--   `c` : buffer capacity           `hSpace` : evidence of room (`b.length < c`)
--
-- Try it: click before `omega` in `receive_capacity`, or change `[]` to `[9]` in
-- `buffer_capacity_demo` and watch the room premise fail.
-- Note: this is an operational capacity invariant; it does NOT establish observation accuracy.

namespace CompositionalAssurance.PalletElevator

def receive (buffer : List Nat) (p : Nat) : List Nat := buffer ++ [p]

theorem receive_capacity (b : List Nat) (p c : Nat) (hSpace : b.length < c) :
    (receive b p).length ≤ c := by simp [receive]; omega

/-- Hypothetical teaching buffer, not a measured plant capacity. -/
theorem buffer_capacity_demo :
    (receive [] 7).length ≤ 1 :=
  receive_capacity [] 7 1 (by decide)

#eval receive [] 7

end CompositionalAssurance.PalletElevator
