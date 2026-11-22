-- Tested with Lean v4.37.0-rc1
-- Act 2, Scene 3 (Slides 21-22): One pallet moves; two truthful captures double-count it.
--
-- Strictly crossing 1/2/3/4 witness (hypothetical integration example, not a defect
-- attributed to Wooley et al.):
--   t = 1 s: elevator capture (sees pallet 7)
--   t = 2 s: unload occurs
--   t = 3 s: buffer capture (sees pallet 7)
--   t = 4 s: both reports retrieved within the same window [0, 4)
--
-- Both local contracts hold (`noncoincident_local_contracts`), yet `naiveJoin` produces
-- inventory `[7, 7]` and violates `Invariant` (`noncoincident_double_count`).

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

-- 2. Local observers, `naiveJoin`, and `witnessTrace` (`Observations.lean`).
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

/-- Illustrative identity 7, capacity 1; no claim of a reported industrial defect. -/
def witnessTrace : Trace 1 := .unload 7 [] (by decide)

-- 3. Discrete-second capture semantics (`Collection.lean`).
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

-- 4. Slides 21-22: The 1/2/3/4 non-coincident witness (`Noncoincident.lean`).
-- A second concrete witness, independent of the minimal cadence construction.
def ncCaptureEarly : Nat := 1
def ncEventTime : Nat := 2
def ncCaptureLate : Nat := 3
def ncRetrievedAt : Nat := 4
def ncPeriod : Nat := 4

def ncElevator : Observation :=
  sampledElevator witnessTrace ncEventTime ncCaptureEarly
def ncBuffer : Observation :=
  sampledBuffer witnessTrace ncEventTime ncCaptureLate

/-- Both captures are interior to [0,4), strictly straddle the unload, and
    precede retrieval. This is a hypothetical common Nat-second clock. -/
theorem noncoincident_schedule :
    0 < ncCaptureEarly ∧ ncCaptureEarly < ncEventTime ∧
    ncEventTime < ncCaptureLate ∧ ncCaptureLate < ncPeriod ∧
    ncCaptureLate < ncRetrievedAt ∧ ncRetrievedAt = ncPeriod ∧
    SameCollectionWindow ncPeriod ncCaptureEarly ncCaptureLate ∧
    ncRetrievedAt - ncCaptureEarly ≤ ncPeriod := by
  unfold SameCollectionWindow
  decide

/-- Exactness is relative to the same existing reference trace at each own cut;
    the physical state satisfies the original invariant at both cuts. -/
theorem noncoincident_local_contracts :
    ncElevator.cut = .before ∧ ncBuffer.cut = .after ∧
    ExactElevator witnessTrace ncElevator ∧
    ExactBuffer witnessTrace ncBuffer ∧
    Invariant [7] 1 (witnessTrace.at .before) ∧
    Invariant [7] 1 (witnessTrace.at .after) := by decide

-- BEGIN noncoincident double count
theorem noncoincident_double_count :
    Inventory (naiveJoin ncElevator ncBuffer) = [7, 7] ∧
    ¬ Invariant [7] 1 (naiveJoin ncElevator ncBuffer) := by decide
-- END noncoincident double count

#eval Inventory (naiveJoin ncElevator ncBuffer)
#check noncoincident_double_count

end CompositionalAssurance.PalletElevator
