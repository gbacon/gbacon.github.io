---
title: "What is induced drag?"
description: "What is induced drag? Clearing up the PHAK’s vague answer: aircraft induced drag is the rearward part of the lift vector, shown with a diagram and vector math."
author: "Greg Bacon"
date: "2026-06-15"
tags:
  - "Aviation"
---
I have always found the explanation in the [Pilot’s Handbook of Aeronautical Knowledge][phak] to be completely vague and unhelpful.

> &hellip; the aerodynamic properties of a wing or rotor produce a required lift, but this can be obtained only at the expense of a certain penalty. The name given to this penalty is induced drag. Induced drag is inherent whenever an airfoil is producing lift and, in fact, this type of drag is inseparable from the production of lift. Consequently, it is always present if lift is produced. 

Okay, but what is it? The diagram below is a simplification but tells a good part of the story.

![How induced drag happens as part of lift creation](/images/induced-drag.png)

The lift vector $L$ does point generally upward, but also backward against the direction the airplane is flying. Just like we have to add backpressure in a turn to compensate for a portion of lift going off to the side, part of the lift in a climb points toward the airplane’s tail.

We can separate the vertical and horizontal components through *vector decomposition*. The lift vector $L$ points “straight away” from the top of the airplane. We can separate it into a vertical component (against gravity) of effective lift or $L_{eff}$ and a horizontal, rearward component. This rearward component is induced drag or $L_{ind}$.

At lower angles of attack, induced drag is lower. Induced drag increases for higher angles of attack because $L$ points farther toward the empennage.

Again, the explanation above is a simplification and does not tell the entire story. It is intended to give you an idea what’s happening and show you that it’s not some mysterious voodoo.

[phak]: https://www.amazon.com/dp/1510779876
