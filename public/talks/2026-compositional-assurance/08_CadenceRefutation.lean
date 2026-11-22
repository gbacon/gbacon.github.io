-- Tested with Lean v4.37.0-rc1
-- Act 2, Scene 4 (Slide 23): Collection frequency cannot ensure consistency.
--
-- Separate minimal 0/1/P construction (distinct from Scene 3's 1/2/3/4 witness):
--   For EVERY discrete collection period `P ≥ 2` seconds, Lean constructs truthful
--   captures 1 second apart (at t = 0 and t = 1) straddling an unload at t = 1,
--   both retrieved within `[0, P)` yet violating the inventory invariant.
--
-- Wooley's 30-minute cadence (`collectionSeconds = 1800`) is one instance of `P ≥ 2`
-- (`collection_frequency_does_not_guarantee_coherence`). Possible, not inevitable;
-- discrete model, not a physical safety threshold.

namespace CompositionalAssurance.PalletElevator

-- 1. Modeled state, invariant, and single-unload trace (`Model.lean`).
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

/-- Zero or one legitimate unload, not a complete production run or PLC implementation. -/
inductive Trace (capacity : Nat) where
  | idle (state : State)
  | unload (p : Nat) (buffer : List Nat) (space : buffer.length < capacity)

def Trace.at : Trace c → Cut → State
  | .idle s, _ => s
  | .unload p b _, .before => ⟨[p], b⟩
  | .unload p b _, .after => ⟨release p, receive b p⟩

-- 2. Local observers and mixed-cut refutation `naive_composition_false` (`Observations.lean`).
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

/-- Illustrative identity 7, capacity 1; no claim of a reported industrial defect. -/
def witnessTrace : Trace 1 := .unload 7 [] (by decide)
def earlyElevator : Observation := observeElevator witnessTrace .before
def lateBuffer : Observation := observeBuffer witnessTrace .after

theorem witness_local_contracts :
    Invariant [7] 1 (witnessTrace.at .before) ∧
    ExactElevator witnessTrace earlyElevator ∧ ExactBuffer witnessTrace lateBuffer ∧
    Invariant [7] 1 (witnessTrace.at .after) ∧
    (naiveJoin earlyElevator lateBuffer).elevator.length ≤ 1 ∧
    (naiveJoin earlyElevator lateBuffer).buffer.length ≤ 1 := by
  decide

theorem witness_double_count :
    Inventory (naiveJoin earlyElevator lateBuffer) = [7, 7] ∧
    ¬ Invariant [7] 1 (naiveJoin earlyElevator lateBuffer) := by decide

theorem naive_composition_false : ¬ NaiveComposition := by
  intro h
  exact witness_double_count.2
    (h 1 witnessTrace [7] earlyElevator lateBuffer witness_local_contracts.1
      witness_local_contracts.2.1 witness_local_contracts.2.2.1)

-- 3. Slide 23: General cadence refutation `cadence_contract_false` (`Collection.lean`).
/-- Teaching clock in Nat seconds, relative to one hypothetical unload.
    Equality is the settled after boundary; transit and clock uncertainty are excluded. -/
def cutAt (eventTime captureTime : Nat) : Cut :=
  if captureTime < eventTime then .before else .after

def sampledElevator (tr : Trace c) (eventTime captureTime : Nat) : Observation :=
  observeElevator tr (cutAt eventTime captureTime)

def sampledBuffer (tr : Trace c) (eventTime captureTime : Nat) : Observation :=
  observeBuffer tr (cutAt eventTime captureTime)

/-- Same nominal collection bucket; says nothing about events inside it. -/
def SameCollectionWindow (period first second : Nat) : Prop :=
  first / period = second / period

/-- Any window of at least two discrete seconds permits truthful captures only one
    second apart on opposite sides of an unload. Both are retrieved within the period.
    These stronger age/window assumptions are constructed, not reported plant guarantees. -/
theorem cadence_alone_insufficient (period : Nat) (hPeriod : 2 ≤ period) :
    ∃ eventTime tE tB retrievedAt : Nat,
      tB = tE + 1 ∧ tB ≤ retrievedAt ∧ retrievedAt - tE ≤ period ∧
      SameCollectionWindow period tE tB ∧
      ExactElevator witnessTrace (sampledElevator witnessTrace eventTime tE) ∧
      ExactBuffer witnessTrace (sampledBuffer witnessTrace eventTime tB) ∧
      ¬ Invariant [7] 1 (naiveJoin
        (sampledElevator witnessTrace eventTime tE)
        (sampledBuffer witnessTrace eventTime tB)) := by
  refine ⟨1, 0, 1, period, by omega, by omega, by omega, ?_, ?_, ?_, ?_⟩
  · unfold SameCollectionWindow
    rw [Nat.zero_div, Nat.div_eq_of_lt (by omega : 1 < period)]
  · exact elevator_observer_exact _ _
  · exact buffer_observer_exact _ _
  · simpa [sampledElevator, sampledBuffer, cutAt, earlyElevator, lateBuffer] using
      witness_double_count.2

/-- A deliberately overoptimistic contract: even a shared bucket and bounded
    retrieval age would be enough to justify the assembled state. -/
def CadenceCoherent (period : Nat) : Prop :=
  ∀ eventTime tE tB retrievedAt : Nat,
    tB = tE + 1 → tB ≤ retrievedAt → retrievedAt - tE ≤ period →
    SameCollectionWindow period tE tB →
    ExactElevator witnessTrace (sampledElevator witnessTrace eventTime tE) →
    ExactBuffer witnessTrace (sampledBuffer witnessTrace eventTime tB) →
    Invariant [7] 1 (naiveJoin (sampledElevator witnessTrace eventTime tE)
      (sampledBuffer witnessTrace eventTime tB))

theorem cadence_contract_false (period : Nat) (hPeriod : 2 ≤ period) :
    ¬ CadenceCoherent period := by
  intro h
  obtain ⟨eventTime, tE, tB, retrievedAt, hGap, hRetrieved, hAge, hWindow, he, hb, hBad⟩ :=
    cadence_alone_insufficient period hPeriod
  exact hBad (h eventTime tE tB retrievedAt hGap hRetrieved hAge hWindow he hb)

/-- Published selected collection frequency converted to seconds, not an age guarantee. -/
def collectionSeconds : Nat := 30 * 60

/-- The paper's cadence motivates the question; it does not establish coherence. -/
theorem collection_frequency_does_not_guarantee_coherence :
    ¬ CadenceCoherent collectionSeconds :=
  cadence_contract_false collectionSeconds (by decide)

example : collectionSeconds = 1800 := by decide
example := cadence_alone_insufficient collectionSeconds (by decide)

#check cadence_contract_false
#check collection_frequency_does_not_guarantee_coherence

end CompositionalAssurance.PalletElevator
