---
title: "Where does the radical in the maneuvering speed formula come from?"
description: "We don’t need no steenking square roots!"
date: "2026-06-23"
author: "Greg Bacon"
tags:
 - "Aviation"
---
During primary training, you were likely given a formula to memorize for calculating maneuvering speed when the airplane is loaded at lower than max gross:

$$v_{op} = v_a \sqrt{w_{op} / w_{max}}$$

where $w_{max}$ is the maximum gross takeoff weight, $v_a$ is the maneuvering speed *at max gross*, and $w_{op}$ and $v_{op}$ are the operating weight and maneuvering speed *at that day’s operating weight*.

For small general aviation aircraft, the Pilot’s Operating Handbook or POH may publish only a single maneuvering speed. **A key point is this maneuvering speed is valid at max gross only.** On checkride day, the Designated Pilot Examiner or DPE will expect the candidate to calculate weight and balance to show that the center of gravity is within a safe range and to calculate maneuvering speed for the combined weight of the airplane, candidate, DPE, fuel, and any baggage on board. When $w_{op}$ decreases, $v_{op}$ does too. (**Possible checkride answer:** lower operating weight also means slower stall speed.)

But where does the square root sign (“radical”) come from?

The story begins with the lift equation, which according to Chapter 5 of the [Pilot’s Handbook of Aeronautical Knowledge][phak], is

$$L = 0.5 \cdot C_L\cdot \rho\cdot V^2\cdot S$$

where $L$ is lift, $V$ is velocity, $C_L$ is the coefficient of lift, $\rho$ is air density, and $S$ is the wing’s surface area. In straight-and-level flight, lift must equal weight, so substituting the day’s operating weight $w_{op}$ for $L$, renaming $V$ to $v_{op}$, and swapping sides in the equality results in

$$0.5 \cdot C_L\cdot \rho\cdot v_{op}^2\cdot S = w_{op}$$

For the same airplane flying around at max gross, the weight and maneuvering speed are different, so they get different unknowns.

$$0.5 \cdot C_L\cdot \rho\cdot v_{a}^2\cdot S = w_{max}$$

Dividing the first by the second cancels out constants.

$$v_{op}^2 / v_a^2 = w_{op} / w_{max}$$

Taking the (positive) square root of both sides is where the radical comes from.

$$v_{op} / v_a = \sqrt{w_{op} / w_{max}}$$

Multiply both sides by $v_a$ to give the (formerly) rote forumla.

$$v_{op} = v_a\sqrt{w_{op} / w_{max}}$$

The [FAA Weight and Balance Handbook][wb] in Figure 2-15 gives a few examples that we can check our formula against.

* S/N 1005 thru 1147:
  * 2900lbs - 135 KIAS
  * 2600lbs - 126 KIAS (calculated: 127.8)
  * 2200lbs - 116 KIAS (calculated: 117.6)
* S/N 1148 thru 1877 \[&hellip;]:
  * 3000lbs - 131 KIAS
  * 2600lbs - 122 KIAS (calculated: 122.0)
  * 2300lbs - 114 KIAS (calculated: 114.7)

The figures are within 2% for the top lot and off by a fraction of a percent for the bottom.

[phak]: https://www.amazon.com/Pilots-Handbook-Aeronautical-Knowledge-FAA-H-8083-25C/dp/1510779876?&linkCode=ll2&tag=bloggbaconcom-20&linkId=49940643c9943d5bc485a8eebdb91850&language=en_US&ref_=as_li_ss_tl
[wb]: https://www.amazon.com/Weight-Balance-Handbook-FAA-H-8083-1B-Color/dp/B09MG914N6?&linkCode=ll2&tag=bloggbaconcom-20&linkId=4190d29e9e596d416092239157da8d98&language=en_US&ref_=as_li_ss_tl
