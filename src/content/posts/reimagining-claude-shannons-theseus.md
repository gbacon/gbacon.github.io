---
title: "Reimagining Claude Shannon’s Theseus in Rust"
description: "A little Q-learning, a perfect maze, and two mice running in WebAssembly."
author: "Greg Bacon"
date: "2026-09-26"
tags:
  - "AI-ML"
  - "Rust"
  - "Computer Science"
---

Around 1950, Claude Shannon built **Theseus**, an electromechanical mouse that could explore a maze, retain what it learned, and then make better use of that information on later runs.

I recently built a modern version of the idea in Rust.

* [*The Rust Programming Language*](https://link.amazon/B0f5kyeaW)
* [*Deep Reinforcement Learning Hands-On: A practical and easy-to-follow guide to RL from Q-learning and DQNs to PPO and RLHF*](https://link.amazon/B01rWY82O)

Mine is not an attempt to simulate Shannon’s relay circuitry. Instead, I started with a related question:

> What might a small, transparent maze-learning experiment inspired by Theseus look like using modern Rust and reinforcement learning?

The result is [Theseus](/projects/theseus-mouse), a Rust application compiled to WebAssembly that runs entirely in the browser. []

<div style="width: 100%; margin: 1.5rem 0;">
  <iframe
    src="https://gbacon.github.io/theseus-rs/"
    title="Theseus Q-learning maze demo"
    style="width: 100%; height: 960px; border: 1px solid #ccc; border-radius: 8px;"
    loading="lazy">
  </iframe>
</div>

The learner is deliberately simple: tabular Q-learning with an epsilon-greedy policy. Each run takes place in a randomly generated *perfect maze*—one with no loops and exactly one simple path between any two cells.

What makes the visualization interesting to me is that it shows **two mice**.

The blue mouse is the training agent. It explores the maze and updates its Q-table after every action.

The green mouse is different. At the beginning of each training episode, I freeze a snapshot of the learned policy and let a second mouse run using that snapshot without learning anything new. The green mouse therefore shows what the learner knew *before* the current episode began.

Early on, it may wander into bad choices or small cycles. After more training, it begins finding the cheese reliably and eventually follows a direct route.

There is also a mildly surprising behavior along the way: improvement need not be monotonic. A policy that reaches the cheese successfully in one snapshot can temporarily get worse after subsequent Q-value updates alter which action looks best. Given enough training, the policy settles down.

That makes the process considerably more interesting to watch than simply displaying the final shortest path.

The implementation uses Rust for the maze, environment, agents, training, and evaluation; `egui`/`eframe` for the visualization; WebAssembly for browser execution; and Trunk for the web build.

You can [run the experiment and read more about Shannon’s original Theseus on the project page](/projects/theseus-mouse), or [browse the Rust source on GitHub](https://github.com/gbacon/theseus-rs).

More than seventy-five years separate Shannon’s relays from a Q-table running in WebAssembly, but the underlying question remains recognizable: **how can a machine use what happened before to behave better the next time?**