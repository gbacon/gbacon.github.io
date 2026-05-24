---
title: "Use of Large Language Model Embeddings to Predict Research Topic Suitability Based on Organizational Capabilities"
author: "Greg Bacon and Vineetha Menon"
date: "2024-03-20"
journal: "IEEE SoutheastCon 2024"
external_url: "https://doi.org/10.1109/SoutheastCon56624.2025.10971523"
description: "Identification of technical fits based on opportunity descriptions and a team’s summary of capabilities. Unsupervised multiclass spectral clustering selected 3 out of 5 topics (in a corpus of 89) chosen by a team of human experts."
tags:
  - "AI/ML"
  - "LLM"
  - "Embeddings"
  - "Natural Language Processing"
  - "SBIR/STTR"
---

## Abstract

We performed a pilot study on the use of large language model technology to help researchers in industry and academia identify prospective opportunities to pursue for funding or grant awards, especially those that they might otherwise overlook due to reading volume, time pressure, and non-obvious connections. Our goal is to help researchers offload some of the burden to technology. As a use case, we query a recent Department of Defense (DoD) Small Business Innovation Research (SBIR) solicitation with natural language inputs in the form of real-world marketing documents and abstract areas of relevance. We experiment with clustering algorithms to determine which best use embeddings to predict solicitation topics that human team members would recommend for proposal. Investigation into this nascent yet practical application of technology will move toward human-centric automation and personalization of results through human reinforcement learning.

## Contribution

Our LLM-based embedding clustering approach performed on an expertise level similar to that of undergraduate research interns: some hits, some misses, and some thought-provoking selections. Multiclass spectral clustering stood alone in its performance on clustering topics with the capabilities statement. For clusters focused on relevance queries, formulating performance metrics was challenging. We used subjective “eye tests” as experts to determine whether the LLM embedding framework was indeed identifying the correct clusters for topics as expected. Even so, the clusters and intersections did appear mostly reasonable, albeit with some surprises such as the physiological topics that matched cybersecurity.

We introduced the heuristic threshold

$$t_k = \frac{ \max d_k - \min d_k }{ \sigma_{d_k} }$$

where where $d_k$ are the Euclidean distances of embeddings from cluster $k$’s centroid and $\sigma_{d_k}$ is their standard deviation.
