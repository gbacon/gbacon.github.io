---
title: "Theseus Maze-Solving Machine"
description: "An experiment from 1950 reimagined with Rust and WASM."
tags:
  - "AI-ML"
  - "Robotics"
  - "Hardware"
---

## Overview

Theseus was an experimental maze-solving machine created by Claude Shannon at Bell Labs around 1950. It is one of the earliest and best-known demonstrations of a machine using stored experience to improve its behavior.

Shannon’s original system used an electromechanical mouse moving through a configurable maze. The control machinery remembered information about the maze so that, after exploration, the mouse could navigate toward its goal more effectively.

I recently revisited the idea and built a modern version in Rust. Rather than reproducing Shannon’s relay logic exactly, this version asks a different question:

> What might a small, transparent maze-learning experiment inspired by Theseus look like using modern Rust and reinforcement learning?

* [*The Rust Programming Language*](https://link.amazon/B0f5kyeaW)
* [*Deep Reinforcement Learning Hands-On: A practical and easy-to-follow guide to RL from Q-learning and DQNs to PPO and RLHF*](https://link.amazon/B01rWY82O)

## Interactive Rust/WASM Version

The current implementation runs entirely in the browser using Rust compiled to WebAssembly.

<div style="width: 100%; margin: 1.5rem 0;">
  <iframe
    src="https://gbacon.github.io/theseus-rs/"
    title="Theseus Q-learning maze demo"
    style="width: 100%; height: 960px; border: 1px solid #ccc; border-radius: 8px;"
    loading="lazy">
  </iframe>
</div>

[View the source code on GitHub.](https://github.com/gbacon/theseus-rs)

## What You’re Watching

The visualization contains two mice.

- **Blue mouse** — the actively learning agent. It explores the maze using an epsilon-greedy policy and updates its Q-table after every action.
- **Green mouse** — a frozen snapshot of the learned policy taken at the beginning of the current training episode. It does not learn while it runs, so it shows what the blue mouse knew before that episode began.

The arrows visualize the learned policy:

- **Gray arrows** show the current live training policy.
- **Green arrows** show the frozen evaluation policy.
- **Gold marker** represents the cheese.

Early in training, the green mouse may make poor choices or become trapped in small cycles. As learning progresses, the policy becomes more reliable. Eventually, the green mouse follows a direct route to the cheese consistently.

One interesting feature of the experiment is that learning is not necessarily monotonic. A policy can successfully reach the cheese in one snapshot and regress temporarily in the next as additional Q-value updates change which actions appear best. After enough training, the policy stabilizes.

### Modern Learning Algorithm

The new implementation uses tabular Q-learning.

For a transition from state $s$, taking action $a$, receiving reward $r$, and arriving at state $s'$, the learner applies

$$
Q(s,a)\gets Q(s,a) + \alpha\left[r + \gamma\max_{\alpha'} Q(s',a')-Q(s,a) \right]
$$

The training agent uses an epsilon-greedy policy:

- with probability $\epsilon$, choose a random action
- otherwise, choose an action with the highest known Q-value.

The current demonstration uses approximately:

- $\epsilon = 0.20$
- $\alpha = 0.50$
- $\gamma = 0.99$

This is deliberately a simple learner. The objective is not to hide maze solving behind a sophisticated model, but to make the learning process visible.

## Maze Generation

The Rust version generates a new perfect maze using randomized depth-first search.

A perfect maze:

1. Connects every cell
2. Contains no loops
3. Has exactly one simple path between any two cells

The maze therefore has a unique simple route from the starting position to the cheese, while still giving the learning agent a substantial exploration problem.

Different random seeds produce different mazes.

## Implementation

The new version is written in Rust and uses:

- **Rust** for the maze, environment, agents, training logic, and application
- **egui / eframe** for the interactive visualization
- **WebAssembly** for execution in the browser
- **Trunk** for building and serving the web application

The implementation is divided into small modules for maze generation, environment transitions, episodes, agents, Q-learning, training, evaluation, and visualization.

The same basic Rust application can also run as a native program.

## Shannon’s Original Theseus

Shannon’s Theseus was fundamentally different from the modern implementation above.

The original system used electromechanical components rather than software reinforcement learning. The mouse itself was comparatively simple; much of the intelligence resided beneath the maze in the control apparatus.

The system consisted broadly of:

1. **The Mouse** — a small movable device representing Theseus
2. **The Maze** — a configurable grid with movable walls
3. **Control Machinery** — electromechanical circuitry used to detect position and retain information about the maze
4. **Goal** — a target location that the mouse learned to reach

The importance of the machine was not that it used today’s definition of reinforcement learning. Rather, it demonstrated an early and compelling idea: a machine could use information acquired during previous behavior to perform better later.

## How the Original System Learned

At a conceptual level, Shannon’s machine operated through exploration and memory.

1. **Exploration** — Theseus moved through an unfamiliar maze.
2. **Discovery** — the control system accumulated information about viable paths.
3. **Memory** — information about the maze was retained by the electromechanical control system.
4. **Improved Navigation** — later traversals could exploit stored knowledge instead of behaving as though the maze were completely unknown.

This made Theseus an unusually concrete demonstration of adaptive machine behavior years before artificial intelligence became an established academic field.

## Why Theseus Matters

Theseus sits at an interesting intersection of several ideas that later became central to computer science and artificial intelligence:

- machines maintaining internal state about an environment;
- learning from interaction rather than receiving a complete solution in advance;
- separating exploration from exploitation;
- representing a problem as states and possible actions;
- using stored experience to improve future behavior.

The implementation technology has changed dramatically, but those underlying questions remain recognizable.

That is what makes Theseus interesting to revisit with reinforcement learning.

Shannon’s machine used relays and electromechanical memory. The modern version uses a Q-table and WebAssembly. Both make the learning process small enough to watch.

---

*Theseus remains a useful reminder that some of the most important ideas in machine learning predate the term itself.*
