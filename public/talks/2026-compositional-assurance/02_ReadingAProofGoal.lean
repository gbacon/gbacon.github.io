-- Tested with Lean v4.37.0-rc1
-- Act 1 (Slide 6): Reading a proof goal ("known facts → still need to prove").
--
-- Click right before `omega` below to inspect the exact goal state from Slide 6
-- in Lean Web's InfoView:
--   part rest total : Nat        -- available variables
--   hPart : part ≤ rest          -- known hypothesis 1
--   hTotal : total = rest + 2    -- known hypothesis 2
--   ⊢ part + 1 < total           -- goal still to prove
--
-- `omega` checks linear arithmetic from the stated hypotheses.
-- Try it: change `rest + 2` to `rest + 1` and watch `omega` reject the claim.
-- This exact two-premise pattern returns in mergesort's termination proof (Slide 12).

-- Two arithmetic premises suffice; no sorting fact is built into omega.
theorem strict_progress (part rest total : Nat)
    (hPart : part ≤ rest) (hTotal : total = rest + 2) :
    part + 1 < total := by omega
