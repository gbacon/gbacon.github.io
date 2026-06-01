---
title: "Why are transponder digits only 0 to 7?"
description: "Octal and human factors."
date: "2026-05-01"
author: "Greg Bacon"
tags:
  - "Aviation"
---
### Short Answer

Entering squawk codes in [octal, or base-8][octal] where allowable digits are 0 through 7, makes the [human-machine interface][HMI] between pilot and transponder as simple as possible.

### Background

[Mode A and Mode C][XPNDR] compatible transponders use 12-bit identity or beacon codes. Each bit has two options, high or low, which works out to a total of 2<sup>12</sup>&nbsp;=&nbsp;4,096 *representable* (*i.e.*, that fit within 12 bits) identifiers, ranging from 0 to 4,095 in base-10, written 4095<sub>10</sub>. Now, not all representable indentifiers are *valid*. For example, 7500<sub>8</sub> is reserved for specific situations, but that is not for the circuitry to worry about.

Quantities that fit exactly within some number of bits do not line up cleanly with powers of ten. Consider a hypothetical transponder that accepts four decimal digits 0000 through 9999 or 10<sup>4</sup>&nbsp;=&nbsp;10,000 possible inputs. The pilot is more likely to handjam a bogus squawk than a representable one. This “simpler” interface now has an error condition that it must deal with: representable and unrepresentable inputs. If the pilot types or spins the dials to anything above 4095, how will the transponder reply? Should it blink an error light that the pilot may miss? How should it reply to interrogation while in a bad state? Silently clip the beacon code to all bits high (111&nbsp;111&nbsp;111&nbsp;111<sub>2</sub>&nbsp;=&nbsp;4095<sub>10</sub>&nbsp;=&nbsp;7777<sub>8</sub>)? Stop replying entirely? More modes mean more testing and more expense.

### Tradeoffs

‘Ah,’ thinks the clever old-school engineer. ‘No more fancy buttons; we will give the leftmost dial only five positions, 0 through 4.’ But what about the second dial that must be 0 when the first is 4 but can be any digit otherwise?

We have twelve bits to fill. Perhaps the pilot enters the binary directly. “Cessna 123AB, squawk zero-zero-one-zero-one-zero-zero-zero-&hellip;” Just imagine the frequency congestion with such long instructions, readbacks, corrections, and confirmations.

Our short-term memory is good for [about seven items, give or take two][7pm2]. Twelve binary digits (“bits”) are too unwieldy. Other factors of 12 are 6×2 and 4×3.

Factoring the twelve identity bits into six and two means squawk codes in either two 6-bit quantities (0-63) or six 2-bit (0-3) chunks. Imagine dealing with 64-position dials in the old days. Doing it with decimal buttons gets us back to the problem of unrepresentable codes. Six dials each with 0 to 3 may work, but that makes the dials smaller and forces the pilot to deal near the limit of short-term memory, both making this option error-prone and thus less desirable.

Three groups of 4-bit or [hexadecimal] quantities could work. Although hex values range from 0 to 15<sub>10</sub>, a common convention is to use digits 0 through 9 and then A through F for ten to fifteen. In other words, we are down to one digit per place, but in the old days, that would have meant using 16-place dials. Today, that would mean sixteen buttons on the face of the transponder. However, “Approach, Cessna tree-alpha-bravo declaring an emergency and squawking foxtrot-charlie-zero for this charlie foxtrot!” does have some mnemonic value.

### The Sweet Spot

Considering it the other way, we have four groups of three bits. In binary, place values are powers of two (not powers of ten like we’re used to thinking in decimal). Working with three bits each, the maximum value for any beacon code digit is

$$(1 \cdot 2^2=4) + (1 \cdot 2^1=2) + (1 \cdot 2^0=1) = 7$$

Here we find several advantages:

* Squawk codes have four places — easily manageable in short-term memory.
* All digits are familiar 0 through 7, so
  * Dial inputs have only eight positions or
  * Eight buttons suffice.
* All four-digit pilot inputs are representable in Mode A or Mode C.
* The transponder doesn’t have to worry about what to do with bad inputs.

[octal]: https://en.wikipedia.org/wiki/Octal
[HMI]: https://en.wikipedia.org/wiki/User_interface
[XPNDR]: https://en.wikipedia.org/wiki/Transponder_(aeronautics)
[7pm2]: https://en.wikipedia.org/wiki/The_Magical_Number_Seven,_Plus_or_Minus_Two
[hexadecimal]: https://en.wikipedia.org/wiki/Hexadecimal
