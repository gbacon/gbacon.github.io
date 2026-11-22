-- Tested with Lean v4.37.0-rc1
-- Act 2, Scene 5 (Slides 24-25): Make the observations describe one state.
--
-- Two model guarantees meet at the SAME target state (`ev.target`):
--   1. `reconciliation_aligns`: truthful reports + matched event evidence → `reconcile` returns `tr.at ev.target`.
--   2. `trace_preserves`: initial invariant + legal transition → `Invariant` holds at `tr.at ev.target`.
--   3. `reconstructed_invariant`: composes them into successful, invariant-preserving reconstruction.
--
-- Timed specializations (Slide 24):
--   - Case A (`collection_without_crossing_correct`): no unload between captures → direct join is valid.
--   - Case B (`crossing_reconstruction_correct`): straddling captures + matched unload evidence → repair is valid.
--
-- Trust boundary reminder: Lean checks consequences of the model; justifying real sensors,
-- PLC records, and event completeness remains an engineering validation obligation.

namespace CompositionalAssurance.PalletElevator

-- 1. Operational model & transition guarantee `trace_preserves` (Slide 25) (`Model.lean`).
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

-- 2. Local observers & common-cut composition `common_cut_correct` (`Observations.lean`).
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

theorem common_cut_correct (tr : Trace c) (initial : List Nat) (e b : Observation)
    (hInitial : Invariant initial c (tr.at .before))
    (he : ExactElevator tr e) (hb : ExactBuffer tr b) (hCut : e.cut = b.cut) :
    Invariant initial c (naiveJoin e b) := by
  have hState : naiveJoin e b = tr.at b.cut :=
    State.ext (by simpa [naiveJoin, ExactElevator, hCut] using he) hb
  rw [hState]
  exact trace_preserves tr initial hInitial b.cut

/-- Illustrative identity 7, capacity 1; no claim of a reported industrial defect. -/
def witnessTrace : Trace 1 := .unload 7 [] (by decide)
def earlyElevator : Observation := observeElevator witnessTrace .before
def lateBuffer : Observation := observeBuffer witnessTrace .after

-- 3. Reconciliation & composed guarantee `reconstructed_invariant` (Slide 25) (`Reconstruction.lean`).
/-- Supplied transfer metadata. Association with the modeled trace is checked below;
    association with a real PLC record remains an external evidence obligation. -/
structure Evidence where
  source : Cut
  target : Cut
  pallet : Option Nat
  deriving DecidableEq, Repr

/-- Only event identity and interval; no inventory, capacity or reconstruction conclusion. -/
def Aligned (tr : Trace c) (ev : Evidence) : Prop :=
  match tr, ev.pallet with
  | .idle _, none => True
  | .idle _, some _ => False
  | .unload _ _ _, none => ev.source = ev.target
  | .unload p _ _, some q => p = q ∧ ev.source = .before ∧ ev.target = .after

instance (tr : Trace c) (ev : Evidence) : Decidable (Aligned tr ev) := by
  unfold Aligned
  split <;> infer_instance

/-- A total checked assembly function, not a plant controller. Missing, mismatched or
    nonexistent modeled event evidence is rejected. Exact sensor values remain premises. -/
def reconcile (tr : Trace c) (e b : Observation) (evidence : Option Evidence) : Option State :=
  match evidence with
  | none => none
  | some ev =>
      if Aligned tr ev ∧ e.cut = ev.source ∧ b.cut = ev.target then
        match ev.pallet with
        | none => some (naiveJoin e b)
        | some p =>
            if e.value = [p] then some ⟨release p, b.value⟩ else none
      else none

/-- Local evidence identifies the reconstructed components with the target boundary.
    This theorem does not assume either initial or final inventory correctness. -/
theorem reconciliation_aligns (tr : Trace c) (e b : Observation) (ev : Evidence)
    (he : ExactElevator tr e) (hb : ExactBuffer tr b)
    (ha : Aligned tr ev) (hs : e.cut = ev.source) (ht : b.cut = ev.target) :
    reconcile tr e b (some ev) = some (tr.at ev.target) := by
  rcases ev with ⟨src, tgt, pal⟩
  simp only [ExactElevator, ExactBuffer] at he hb
  dsimp only at hs ht
  subst hs ht
  cases tr <;> cases pal <;> simp_all [reconcile, Aligned, naiveJoin, Trace.at]

/-- Universal composition: initial invariant, local truth and event alignment suffice.
    Successful reconstruction is established, not merely assumed in a vacuous implication. -/
theorem reconstructed_invariant (tr : Trace c) (initial : List Nat)
    (e b : Observation) (ev : Evidence)
    (hInitial : Invariant initial c (tr.at .before))
    (he : ExactElevator tr e) (hb : ExactBuffer tr b)
    (ha : Aligned tr ev) (hs : e.cut = ev.source) (ht : b.cut = ev.target) :
    ∃ result, reconcile tr e b (some ev) = some result ∧ Invariant initial c result := by
  exact ⟨tr.at ev.target, reconciliation_aligns tr e b ev he hb ha hs ht,
    trace_preserves tr initial hInitial ev.target⟩

/-- Concrete modeled observers discharge both local preconditions independently. -/
theorem observed_reconstruction (tr : Trace c) (initial : List Nat) (ev : Evidence)
    (hInitial : Invariant initial c (tr.at .before)) (ha : Aligned tr ev) :
    ∃ result, reconcile tr (observeElevator tr ev.source) (observeBuffer tr ev.target)
      (some ev) = some result ∧ Invariant initial c result :=
  reconstructed_invariant tr initial _ _ ev hInitial
    (elevator_observer_exact tr ev.source) (buffer_observer_exact tr ev.target) ha rfl rfl

-- 4. Concrete repaired witness `witness_repaired` (Slide 24) (`Examples.lean`).
def unloadEvidence (p : Nat) : Evidence := ⟨.before, .after, some p⟩

theorem witness_repaired :
    reconcile witnessTrace earlyElevator lateBuffer (some (unloadEvidence 7)) =
      some ⟨[], [7]⟩ ∧ Invariant [7] 1 ⟨[], [7]⟩ := by decide

-- 5. Timed Case A & Case B contracts (Slide 24) (`Collection.lean`).
/-- Teaching clock in Nat seconds, relative to one hypothetical unload.
    Equality is the settled after boundary; transit and clock uncertainty are excluded. -/
def cutAt (eventTime captureTime : Nat) : Cut :=
  if captureTime < eventTime then .before else .after

def sampledElevator (tr : Trace c) (eventTime captureTime : Nat) : Observation :=
  observeElevator tr (cutAt eventTime captureTime)

def sampledBuffer (tr : Trace c) (eventTime captureTime : Nat) : Observation :=
  observeBuffer tr (cutAt eventTime captureTime)

/-- Knowing no unload lies between the captures supplies the missing event relation.
    This is an event-semantics premise, not something established by a collection cadence. -/
theorem no_crossing_common_cut (eventTime tE tB : Nat)
    (hNoCrossing : eventTime ≤ tE ∧ eventTime ≤ tB ∨ tE < eventTime ∧ tB < eventTime) :
    cutAt eventTime tE = cutAt eventTime tB := by
  simp only [cutAt]; split <;> split <;> first | rfl | omega

/-- Local exactness and evidence of no crossing compose through the existing theorem. -/
theorem collection_without_crossing_correct (tr : Trace c) (initial : List Nat)
    (eventTime tE tB : Nat) (hInitial : Invariant initial c (tr.at .before))
    (hNoCrossing : eventTime ≤ tE ∧ eventTime ≤ tB ∨ tE < eventTime ∧ tB < eventTime) :
    Invariant initial c (naiveJoin (sampledElevator tr eventTime tE)
      (sampledBuffer tr eventTime tB)) :=
  common_cut_correct tr initial _ _ hInitial
    (elevator_observer_exact _ _) (buffer_observer_exact _ _)
    (no_crossing_common_cut eventTime tE tB hNoCrossing)

/-- If the unload separates the captures, matched pallet/interval evidence repairs it.
    All capacities, initial inventories, pallets and buffers are arbitrary within this model. -/
theorem crossing_reconstruction_correct (p : Nat) (buffer initial : List Nat) (c : Nat)
    (hSpace : buffer.length < c) (eventTime tE tB : Nat)
    (hEarly : tE < eventTime) (hLate : eventTime ≤ tB)
    (hInitial : Invariant initial c ((Trace.unload p buffer hSpace).at .before)) :
    ∃ result, reconcile (.unload p buffer hSpace)
      (sampledElevator (.unload p buffer hSpace) eventTime tE)
      (sampledBuffer (.unload p buffer hSpace) eventTime tB)
      (some ⟨.before, .after, some p⟩) = some result ∧ Invariant initial c result := by
  simpa [sampledElevator, sampledBuffer, cutAt, hEarly, Nat.not_lt.mpr hLate] using
    observed_reconstruction (.unload p buffer hSpace) initial
      ⟨.before, .after, some p⟩ hInitial (by simp [Aligned])

#check collection_without_crossing_correct
#check crossing_reconstruction_correct
#check witness_repaired
#check reconstructed_invariant

end CompositionalAssurance.PalletElevator
