-- Tested with Lean v4.37.0-rc1
-- Act 2, Scene 2 (Slides 19-20): Local observers and the missing boundary premise.
--
-- EXPECTED ERROR on `local_truth_suffices` (at the bottom of this file):
--   Click the red squiggle to inspect Lean's single remaining goal: `⊢ e.cut = b.cut`.
--   Each modeled observer is exact at its own boundary (`rfl`), but local exactness
--   does not establish that both readings refer to the SAME event boundary.
--
-- Try it: replace `: NaiveComposition := by` with an explicit `(hCut : e.cut = b.cut)`
-- hypothesis and finish the proof with `exact hCut`.

namespace CompositionalAssurance.PalletElevator

-- 1. Operational model & transition invariant (`Model.lean`).
/-- Abstract identity lists; uniqueness is a separate premise, not a property of List. -/
@[ext]
structure State where
  elevator : List Nat
  buffer : List Nat
  deriving DecidableEq, Repr

def Inventory (s : State) : List Nat := s.elevator ++ s.buffer

def Invariant (initial : List Nat) (capacity : Nat) (s : State) : Prop :=
  s.elevator.length ≤ 1 ∧ s.buffer.length ≤ capacity ∧ (Inventory s).Perm initial

instance : Decidable (Invariant initial capacity s) :=
  inferInstanceAs (Decidable (_ ∧ _ ∧ _))

/-- Settled event boundaries only; motion, transit and position are outside this slice. -/
inductive Cut where
  | before | after
  deriving DecidableEq, Repr

/-- Elevator release and destination receipt are independently specified functions. -/
def release (_p : Nat) : List Nat := []
def receive (buffer : List Nat) (p : Nat) : List Nat := buffer ++ [p]

theorem release_capacity (p : Nat) : (release p).length ≤ 1 := by simp [release]
theorem receive_capacity (b : List Nat) (p c : Nat) (hSpace : b.length < c) :
    (receive b p).length ≤ c := by simp [receive]; omega

theorem transfer_inventory (p : Nat) (b : List Nat) :
    (Inventory ⟨release p, receive b p⟩).Perm (Inventory ⟨[p], b⟩) := by
  simp [Inventory, release, receive]

/-- A legal unload presupposes the elevator is at Head Buffer. No other boundary flow. -/
theorem unload_preserves (p : Nat) (b : List Nat) (c : Nat) (initial : List Nat)
    (hInitial : Invariant initial c ⟨[p], b⟩) (hSpace : b.length < c) :
    Invariant initial c ⟨release p, receive b p⟩ :=
  ⟨release_capacity p, receive_capacity b p c hSpace,
    (transfer_inventory p b).trans hInitial.2.2⟩

/-- Zero or one legitimate unload, not a complete production run or PLC implementation. -/
inductive Trace (capacity : Nat) where
  | idle (state : State)
  | unload (p : Nat) (buffer : List Nat) (space : buffer.length < capacity)

def Trace.at : Trace c → Cut → State
  | .idle s, _ => s
  | .unload p b _, .before => ⟨[p], b⟩
  | .unload p b _, .after => ⟨release p, receive b p⟩

theorem trace_preserves (tr : Trace c) (initial : List Nat)
    (hInitial : Invariant initial c (tr.at .before)) (cut : Cut) :
    Invariant initial c (tr.at cut) :=
  match tr, cut with
  | .idle _, _ => hInitial
  | .unload _ _ _, .before => hInitial
  | .unload p b hSpace, .after => unload_preserves p b c initial hInitial hSpace

-- 2. Independent local observers (Slide 19: `rfl` is model agreement by construction,
--    not physical sensor calibration) and `common_cut_correct` (`Observations.lean`).
/-- Hypothetical exact snapshot API, not a verified deployed sensor interface. -/
structure Observation where
  value : List Nat
  cut : Cut
  deriving DecidableEq, Repr

def ExactElevator (tr : Trace c) (o : Observation) : Prop :=
  o.value = (tr.at o.cut).elevator

def ExactBuffer (tr : Trace c) (o : Observation) : Prop :=
  o.value = (tr.at o.cut).buffer

instance : Decidable (ExactElevator tr o) := inferInstanceAs (Decidable (_ = _))
instance : Decidable (ExactBuffer tr o) := inferInstanceAs (Decidable (_ = _))

def observeElevator (tr : Trace c) (cut : Cut) : Observation :=
  ⟨(tr.at cut).elevator, cut⟩
def observeBuffer (tr : Trace c) (cut : Cut) : Observation :=
  ⟨(tr.at cut).buffer, cut⟩

theorem elevator_observer_exact (tr : Trace c) (cut : Cut) :
    ExactElevator tr (observeElevator tr cut) := rfl

theorem buffer_observer_exact (tr : Trace c) (cut : Cut) :
    ExactBuffer tr (observeBuffer tr cut) := rfl

def naiveJoin (e b : Observation) : State := ⟨e.value, b.value⟩

/-- The tempting universal claim. Both local observations can be truthful at different cuts. -/
def NaiveComposition : Prop :=
  ∀ (c : Nat) (tr : Trace c) (initial : List Nat) (e b : Observation),
    Invariant initial c (tr.at .before) →
    ExactElevator tr e → ExactBuffer tr b →
    Invariant initial c (naiveJoin e b)

theorem common_cut_correct (tr : Trace c) (initial : List Nat) (e b : Observation)
    (hInitial : Invariant initial c (tr.at .before))
    (he : ExactElevator tr e) (hb : ExactBuffer tr b) (hCut : e.cut = b.cut) :
    Invariant initial c (naiveJoin e b) := by
  have hState : naiveJoin e b = tr.at b.cut :=
    State.ext (by simpa [naiveJoin, ExactElevator, hCut] using he) hb
  rw [hState]
  exact trace_preserves tr initial hInitial b.cut

-- 3. Slide 20: Attempted composition from local contracts alone (fails at `⊢ e.cut = b.cut`).
-- Intentionally false: each observation is exact, but their event cuts are unrelated.
theorem local_truth_suffices : NaiveComposition := by
  intro c tr initial e b hInitial he hb
  apply common_cut_correct tr initial e b hInitial he hb

end CompositionalAssurance.PalletElevator
