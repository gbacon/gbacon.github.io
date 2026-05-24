---
title: "Surprising Haskell Type Inference Result"
author: "Greg Bacon"
date: "2017-06-01"
description: "Haskell type inference appears to diagnose an infinite loop at compile time, but what’s really happening?"
tags:
  - "Haskell"
  - "Type Inference"
---

This talk, presented to HuntFunc, adapts Andy Koenig’s 1994 USENIX paper [An anecdote about ML type inference][ak] to Haskell and walks through step by step how Hindley–Milner arrives at the surprising result.

[ak]: https://www.usenix.org/legacy/publications/library/proceedings/vhll/koenig.html
