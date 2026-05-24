---
title: "IRSim: AI Generation of Infrared Images"
author: "Michael Barnett, Daniel Kolenich, Greg Bacon, Lysle E. Shields III, Stacy Lukins, and Eric Tsibertzopoulos"
date: "2026-03-06"
journal: "IEEE SoutheastCon 2026"
external_url: "https://doi.org/10.1109/SoutheastCon63549.2026.11476640"
description: "Generation of IR images from EO inputs based on training against paired and unpaired samples from the Recognition of Combatant - Vehicles (ROCV) dataset."
tags:
  - "AI-ML"
  - "Infrared"
  - "Style Transfer"
---

## Abstract

The Army Game Studio (AGS) team at the U.S. Army Combat Capabilities Development Command Aviation & Missile Center (DEVCOM AvMC) Software, Simulation, Systems Engineering & Integration Directorate (S3I) on Redstone Arsenal uses the Unreal commercial game engine to develop systems supporting training, simulation, education, and research. To enhance the depiction of sensor views of the environment, we conducted a pilot study into artificial intelligence and machine learning (AI/ML) models to produce realistic infrared (IR) imagery. The end goal was to generate IR scenes in [Rocket&nbsp;IG][rocketig] without having to create new IR textures for every vehicle, which will reduce the cost of generating IR scenes and improve scene accuracy. After initial research, IRSim generates IR scenes that correspond to still electro-optical (EO) images. For additional information, please contact AGS by email at hello@armygamestudio.com.

## Contribution

This work is a pilot feasibility study that demonstrates a practical unsupervised EO-to-IR generation pipeline for simulation and sensor-operator training. Using a military dataset with limitations in size and image registration, the style transfer approach on EO-to-IR produced results that warrant further investigation.

The research team conducted initial experiments with the Stable Diffusion and CycleGAN methods for generating IR images from text prompts and EO inputs. The IR LoRA established a working pipeline and produced visually interesting images that did not resemble images from IR sensors. IRSim used the CycleGAN architecture because of its flexibility in working with both paired and unpaired image datasets. We quantify IRSim results with Complex Wavelet Structural Similarity (CW-SSIM) and Kullback-Leibler Divergence (KLD).

[rocketig]: https://milgaming.army.mil/RocketIG
