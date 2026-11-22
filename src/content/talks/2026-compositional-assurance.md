---
title: "From Formal Requirements to Compositional Assurance: Functional Models, Machine-Checked Proofs, and Digital Twins"
author: "Greg Bacon"
date: "2026-11-18"
description: "A UAH seminar exploring formal requirements, compositional correctness, machine-checked Lean proofs, and assurance for software and digital twins."
tags:
  - "Formal Methods"
  - "Lean"
  - "Systems Engineering"
  - "Digital Twins"
  - "Software Verification"
  - "Artificial Intelligence"
---
**November 18, 2026 · 10:00 a.m.**  
**University of Alabama in Huntsville · OKT N132**

## Abstract

As engineering systems become increasingly complex and artificial intelligence accelerates software development, how can we establish confidence that individually correct components will behave correctly when combined?

This talk explores the intersection of functional modeling, formal requirements, and machine-checked reasoning using Lean, a modern programming language and theorem prover. Through accessible examples from software engineering and digital twins, it examines how mathematical specifications and compositional reasoning can strengthen engineering assurance.

The discussion highlights opportunities for collaboration between computer science and industrial and systems engineering, particularly in developing systems whose correctness arguments can evolve alongside their designs.

## About the Talk

Correct components do not necessarily compose into a correct system.

Using [Lean](https://lean-lang.org/), this seminar explores how formal specifications, component contracts, and machine-checked proofs can expose requirements that ordinary interfaces do not capture.

Examples include mergesort and a manufacturing digital-twin scenario inspired by published research on an automotive pallet elevator.

The talk also introduces **Compositional Autoformalization** as a prospective research direction combining AI-assisted formalization of engineering models with machine-checked assurance of their composition.

## Selected References

- Wooley, A., Bitencourt, J., and Silva, D. (2025). [Bridging the Gap Between Discrete Event Simulation and Digital Twin: A Manufacturing Case Study](https://doi.org/10.1016/j.mfglet.2025.06.147). *Manufacturing Letters*, 44, 1274–1284.
- Morris, J. H., Gibbons, D. W., Gregory, J., and Mocko, G. (2026). [Technical Strategies for Semantic Aggregation of Interoperable Digital Twins](https://www.searchlaboratory.org/assets/papers/morrisInteroperabilityCSER2026.pdf). *Conference on Systems Engineering Research*.

## Speaker

**Greg Bacon, Ph.D.** is CTO of OASYS, INC., an employee-owned defense technology company in Huntsville, Alabama, and an adjunct faculty member in the Department of Computer Science at UAH. His professional and academic interests include software engineering, systems engineering, formal methods, and artificial intelligence.

## Presentation Materials

- **[Presentation Slides (PDF)](./compositional-assurance.pdf)**

### Interactive Lean 4 Examples (Lean Web)

Each link below opens a complete, self-contained, zero-dependency Lean 4 file in [Lean Web](https://live.lean-lang.org/) following the arc of the talk. Two examples intentionally display a Lean error so you can inspect the exact diagnostic shown on the slides.

#### Part 1: Specifications, Progress, and Mergesort

- **Incomplete Requirements vs. Full Specification** (Slides 4–5) — [Open in Lean Web](https://live.lean-lang.org/#url=https%3A%2F%2Fblog.gbacon.com%2Ftalks%2F2026-compositional-assurance%2F01_Specification.lean)\
  Why `Sorted` alone accepts the empty-output `candidate`, and how `SortingSpec` adds `Perm` to preserve duplicates.
- **Reading a Proof Goal (`strict_progress`)** (Slide 6) — [Open in Lean Web](https://live.lean-lang.org/#url=https%3A%2F%2Fblog.gbacon.com%2Ftalks%2F2026-compositional-assurance%2F02_ReadingAProofGoal.lean)\
  Click before `omega` in the InfoView to inspect the exact two-premise arithmetic goal that reappears in mergesort termination.
- **Split, Merge, and the Singleton Termination Rejection (expected error)** (Slides 7–10) — [Open in Lean Web](https://live.lean-lang.org/#url=https%3A%2F%2Fblog.gbacon.com%2Ftalks%2F2026-compositional-assurance%2F03_RecursiveAttempt.lean)\
  Alternating `split`, `merge` on unsorted inputs, and Lean rejecting `sort` because `split [7] = ([7], [])` never shrinks.
- **Repaired Mergesort and Universal Correctness (`mergeSort_correct`)** (Slides 11–14) — [Open in Lean Web](https://live.lean-lang.org/#url=https%3A%2F%2Fblog.gbacon.com%2Ftalks%2F2026-compositional-assurance%2F04_MergesortCorrectness.lean)\
  Complete self-contained proof: `split_lengths`, length-based `termination_by`/`decreasing_by`, `merge_sorted`, and universal `mergeSort_correct`.

#### Part 2: Digital Twin — Pallet Elevator

- **Pallet Elevator: A Buffer with Room to Receive (`receive_capacity`)** (Slide 18) — [Open in Lean Web](https://live.lean-lang.org/#url=https%3A%2F%2Fblog.gbacon.com%2Ftalks%2F2026-compositional-assurance%2F05_BufferCapacity.lean)\
  First component contract: `receive`, universal `receive_capacity`, and the one-place `buffer_capacity_demo`.
- **Pallet Elevator: Local Contracts and the Missing Boundary Premise (expected error)** (Slides 19–20) — [Open in Lean Web](https://live.lean-lang.org/#url=https%3A%2F%2Fblog.gbacon.com%2Ftalks%2F2026-compositional-assurance%2F06_MissingBoundary.lean)\
  Exact local observers (`rfl`) and `local_truth_suffices` stopping at the single unproved goal `⊢ e.cut = b.cut`.
- **Pallet Elevator: Truthful Captures, Impossible Combined State (`noncoincident_double_count`)** (Slides 21–22) — [Open in Lean Web](https://live.lean-lang.org/#url=https%3A%2F%2Fblog.gbacon.com%2Ftalks%2F2026-compositional-assurance%2F07_NoncoincidentDoubleCount.lean)\
  Strictly crossing 1/2/3/4 physical waveform witness: both local contracts hold, yet `naiveJoin` counts pallet 7 twice (`[7, 7]`).
- **Pallet Elevator: Collection Frequency Cannot Ensure Consistency (`cadence_contract_false`)** (Slide 23) — [Open in Lean Web](https://live.lean-lang.org/#url=https%3A%2F%2Fblog.gbacon.com%2Ftalks%2F2026-compositional-assurance%2F08_CadenceRefutation.lean)\
  Separate minimal 0/1/P construction proving that every discrete period `P ≥ 2` s (including 30 min = 1800 s) admits an incoherent snapshot.
- **Pallet Elevator: Event-Reconciled Repair and Composed Guarantee (`reconstructed_invariant`)** (Slides 24–25) — [Open in Lean Web](https://live.lean-lang.org/#url=https%3A%2F%2Fblog.gbacon.com%2Ftalks%2F2026-compositional-assurance%2F09_RepairedComposition.lean)\
  Case A (`collection_without_crossing_correct`), Case B (`crossing_reconstruction_correct`), `witness_repaired`, and `reconciliation_aligns` + `trace_preserves` → `reconstructed_invariant`.
