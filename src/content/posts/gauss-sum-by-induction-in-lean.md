---
title: "Proving the Gauss Sum by Induction in Lean"
description: "A short Lean 4 proof by induction of the Gauss sum formula, using doubled sums to stay in natural-number arithmetic."
author: "Greg Bacon"
date: "2026-11-23"
tags:
  - "Lean"
  - "Formal Methods"
  - "Math"
---

The classic Gauss sum formula states that the sum of the first $n$ natural numbers is

$$\sum_{k=0}^{n} k = \frac{n(n+1)}{2}.$$

An intuitive way to think of it is we fold the list of numbers in half, and each corresponding pair sums to $n + 1$, e.g., $1 + 100$, $2 + 99$, etc.

Proving that the sum holds is straightforward by induction. For $n=1$, we have $\frac{1(2)}{2} = 1$. Now for our induction hypothesis, we grant that the above sum holds for some arbitrary $i$. Then for the sum of the first $i + 1$ we have

$$1 + 2 + 3 + \ldots + i + (i + 1)$$

Substituting the induction hypothesis gives

$$\frac{i(i + 1)}{2} + (i + 1)$$

Multiplying the latter term by $\frac{2}{2}$ and factoring out $(i + 1)$ gives

$$\frac{i(i + 1)}{2} + \frac{2(i + 1)}{2} = \frac{(i + 1)(i + 2)}{2}$$

which is the result when $n = i + 1$. QED.

When formalizing this in Lean over `Nat`, dividing by $2$ introduces truncating integer division, which makes algebraic manipulation unnecessarily awkward. Multiplying both sides by $2$ keeps everything in clean natural-number arithmetic; we also include the case when $n = 0$, so

$$2 \sum_{k=0}^{n} k = n(n+1).$$

The Lean formalization below uses a similar structure.

```lean
-- Tested with Lean v4.37.0-rc1 / Mathlib
import Mathlib

theorem sum_of_first_n_doubled (n : Nat) :
    -- doubled to avoid dealing with halves from dividing by two
    2 * (List.range' 0 (n + 1)).sum = n * (n + 1) := by
  induction n with
  | zero => rfl
  | succ n ih =>
    -- Break off final list element:
    -- `[0, 1, …, n, n + 1] = [0, 1, …, n] ++ [n + 1]`
    rw [List.range'_concat]
    -- Distribute sum over append and unwrap singleton’s sum
    simp only [List.sum_append, List.sum_singleton]
    -- Distribute 2 over the addition to expose 2 * sum …
    rw [Nat.mul_add]
    -- … which `ih` gives as n * (n + 1)
    rw [ih]
    -- Polynomial normalization to close the equality goal
    ring -- or `ring!` but not `omega`
```

[Open the above proof in Lean Web](https://live.lean-lang.org/#codez=LTAEBUFMGcBdICagO4EtYAtQBlIEMA7UANwBYA6AZgHZyAGYAJwGMBGUAelAFk9MAbVACMAUKgC2ABwD2jWDz4ZBokZkizI40NACu4gPrSAZvqOpGcfQX0JpOof0SgAFEQBcoAHJ8AlKDcioEGgIKC29o5IsNKgeMTSqEgI%2BIIEAOYo6FgYePzEMKBGjNJaCKjEiajpoEIAnqCwyNKBwQBMoABULtiocOSMhGmQAOSgdC5EANSgrD4%2B5LpaALygRF2uoNOz%2Fit1LVUIOsywqNJERGiYLQA%2BoABekMWgSwB8oIxG%2FDfaR8yroKgsK8WkFQgAhRj4ADWoGMRkKVVyoEEcFAkEc4kgBFgAWCITAAAMANp0AA0M3JgDICckEGmbGYAXWeoBJ5NYVJpTMm0yJU0ZBJB72QLJ6fQG6RG%2BmYZ2YfAZgtCABFerBGMIdPAflppPlGLFJJIsUhCEgdARkANJNoqmlHNECIBMAmgWsF0AkVrO%2FHqRNFsAWen0eANRvJvv9Bjd6TtZ3leKVKrVQg1kFA7R1jwaGBTeAQZROZwaMUgAA8ZNAU%2B0uotQJTBYxhUTvH7xDp%2BIHc7HgqFKSgMKhmFgCYCCaA0uUCnhnWsJvTZnWG4DO6CwAAFaRegglVBIzeMcS5VB3PinIjRUDMfjScuZlOQACOOgPsHqaWkuTrNvxsL1BLV6QAhCOSbyJu8gEiUkBpHgBJAA) and move your cursor around to see the progression of the tactic states until all goals are closed.

## How the Inductive Step Works

1. **Base case (`zero`)**: `List.range' 0 1` is `[0]`, whose sum is `0`, so both sides reduce definitionally to `0` via `rfl`.
2. **Peeling off the last term**: `List.range'_concat` splits `[0, 1, …, n + 1]` into `[0, 1, …, n] ++ [n + 1]`.
3. **Distributing `.sum` and `2 * _`**: `List.sum_append` and `List.sum_singleton` turn the list sum into `(List.range' 0 (n + 1)).sum + (n + 1)`, and `Nat.mul_add` distributes the factor of `2` so the inductive hypothesis `ih` applies directly.
4. **Closing the algebra**: After `rw [ih]`, the goal is quadratic in `n` (`n * (n + 1) + 2 * (n + 1) = (n + 1) * (n + 2)`), which is why `omega` (a linear arithmetic solver) cannot close it, whereas Mathlib’s `ring` normalizes both polynomials to match.
