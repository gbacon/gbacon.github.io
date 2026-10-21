---
title: "15-Peg Solitaire: Brute Force vs. an SMT Solver"
description: "Stop telling the computer how to search. Tell it what a win looks like."
author: "Greg Bacon"
date: "2026-10-21"
tags:
  - "Python"
  - "Computer Science"
  - "Games"
  - "SMT"
---

If you’ve waited for a table at Cracker Barrel, you’ve played the peg game: a wooden triangle, fifteen holes, fourteen golf tees, and a little scorecard that ranks you from genius all the way down to “eg-no-ra-moose.” I’ve used it in a talk called “What I Learned About Theoretical CS at Cracker Barrel,” which I gave to the [UAH ACM-W chapter in 2022](/talks/2022-crbarrel) and to [HSV.py in 2024](/talks/2024-crbarrel). This post is the written version of the first half of that talk: solving the puzzle the obvious way, then solving it again by describing what a win looks like and letting a solver do the rest.

The slides are a Jupyter notebook rendered with reveal.js. You can [click through the live slides](https://blog.gbacon.com/puzzle15/) or [read the notebook and code on GitHub](https://github.com/gbacon/puzzle15). Every code excerpt below comes from there.

*As an Amazon Associate I earn from qualifying purchases.*

## The rules

The game goes by 15-peg solitaire, peg solitaire, or Tricky Triangle. The rules fit on one slide:

* Start with a single empty hole.
* Remove pegs with checkers-style jumps: over an immediate neighbor and into an empty hole.
* Win by clearing the board down to a single peg.
* If no legal move remains and you have more than one peg, you lose.

It’s mostly a way to kill time and keep kids amused. It also happens to be a tidy little search problem.

## Encoding the board

Number the holes 0 through 14, top to bottom and left to right, and a jump is completely described by three holes in a line: the peg that moves, the peg it jumps, and the hole where it lands. There are only eighteen such lines on the board, so I listed them by hand.

```python
'''
    0
   1 2
  3 4 5
 6 7 8 9
A B C D E
'''
NEIGHBORS = [
  [ 0,  1,  3], [0, 2,  5],
  [ 1,  3,  6], [1, 4,  8],
  [ 2,  4,  7], [2, 5,  9],
  [ 3,  4,  5], [3, 6, 10], [3, 7, 12],
  [ 4,  7, 11], [4, 8, 13],
  [ 5,  8, 12], [5, 9, 14],
  [ 6,  7,  8],
  [ 7,  8,  9],
  [10, 11, 12],
  [11, 12, 13],
  [12, 13, 14],
]
```

Each triple works in both directions. A peg at 0 can jump 1 into 3, and a peg at 3 can jump 1 into 0, so the eighteen lines give 36 possible jumps.

## Brute force

The talk lists the usual suspects: depth-first search, breadth-first search, working backward from a winning board, and bidirectional search. Depth-first is the least code, so that’s where I started. Represent the board as a list of fifteen booleans, try every legal jump, recurse, and keep score.

```python
def depth_first_search(board):
    def loop(b):
        if b.count(True) == 1: loop.winners += 1
        else:
            can_move = False
            for n in NEIGHBORS:
                for i, j, k in [n, reversed(n)]:
                    if b[i] and b[j] and not b[k]:  # legal move?
                        can_move = True
                        bnew = copy.copy(b)
                        bnew[i] = bnew[j] = False
                        bnew[k] = True
                        loop(bnew)
            if not can_move: loop.losers += 1

    loop.winners = loop.losers = 0
    loop(board)
    print(f'winners: {loop.winners}', f'losers: {loop.losers}', sep='\n')

depth_first_search([False, *[True] * 14])
```

Starting with the top hole empty, it reports

```text
winners: 29760
losers: 538870
```

In other words, 29,760 distinct move sequences end with one peg, and 538,870 strand you with two or more. When I built the deck in 2022, that run took 11.9 seconds of wall time in the notebook. Rerunning it today on a newer machine and a newer Python, it takes a little over three seconds. Either way, if you pick moves at random, the odds aren’t in your favor: about one game in nineteen is a winner.

## Why it’s slow

Twelve seconds for a fifteen-hole toy should bother you a little. The search plays out every one of the 568,630 complete games, and most of that work is repeated. The same position shows up again and again by different routes, and the search has no memory, so it solves each one from scratch every time. For this post I counted the distinct positions reachable from that opening, and there are only 3,016 of them.

So the deck lists the obvious fixes:

* **Exploit the threefold rotational symmetry.** Turn the triangle a third of a turn and it’s the same game, so the three corner starts are really one start, and the same goes for the three mid-edge starts.
* **Memoize to deal with the duplication.** Cache the win/loss counts for each position you’ve already seen. When I tried a memoized version for this post, with each board packed into an integer, it produced the same two numbers in about a hundredth of a second.

Both work, and both are exactly what I’d expect from my algorithms students. But notice what they have in common: they’re cleverer ways of telling the computer *how* to search. Let’s go another direction.

## Declare what you want

The Boolean satisfiability problem (SAT) asks a simple question: given an arbitrary Boolean formula, is there an assignment to the variables that makes the whole thing true? Formulas usually come in conjunctive normal form, something like $(x_1 \lor \lnot x_2 \lor \lnot x_3) \land (\lnot x_4 \lor x_5 \lor \lnot x_6) \land \cdots$. SAT solvers are astonishingly good at that question, which is why Knuth devotes the back half of [*The Art of Computer Programming, Volume 4B*][taocp4b] to it.

Satisfiability Modulo Theories (SMT) solvers put higher-level languages – integers, reals, bit vectors, arrays – on top of that Boolean “assembly language.” The paradigm is declarative. You *declare* what you want and let the machine figure it out. The solver I used is Microsoft Research’s [Z3](https://github.com/Z3Prover/z3), which has friendly Python bindings (`pip install z3-solver`). If you want to know what’s going on under the hood, Kroening and Strichman’s [*Decision Procedures*][kroening] covers SAT, SMT, and the theory of bit vectors this puzzle leans on.

The first Z3 program in the talk is a sanity check. Ask for a value of `a` that is both true and false:

```python
from z3 import *

def a_AND_NOTa():
    a = Bool('a')

    s = Solver()
    s.add(a, Not(a))

    if s.check() == unsat:
        print('Phew! The universe is still sane.')
    else:
        print('All bets are off.')
```

It prints the reassuring answer. The same trick proves theorems. To check De Morgan’s laws, $\lnot(p \land q) \iff (\lnot p \lor \lnot q)$ and $\lnot(p \lor q) \iff (\lnot p \land \lnot q)$, assert that at least one of them *fails* and ask for a counterexample. When Z3 answers `unsat`, no counterexample exists, so the laws hold for every $p$ and $q$. That flip from “find me a solution” to “prove there isn’t one” is worth sitting with.

## The puzzle as constraints

Back to Cracker Barrel. A board state is a 15-bit vector with each bit set if that hole has a peg. A winning game is the start board plus the boards that result from 13 jumps, so 14 boards in all. To Z3, every one of them is an unknown:

```python
HOLES = 15

Hole  = [BitVecVal(1 << i,    HOLES) for i in range(HOLES)]
Board = [BitVec(f'board_{i}', HOLES) for i in range(HOLES - 1)]
```

`Board` and `Hole` live in the SMT world, not in Python, so we can’t just look at them. We can only describe them in hypothetical terms:

```python
def used(b, h): return Board[b] & Hole[h] == Hole[h]

# important: capital-N Not, i.e., z3.Not, rather than Python not
def empty(b, h): return Not(used(b, h))

# in future perfect tense: hole h will not have changed from p to q
def unchanged(p, q, h): return used(p, h) == used(q, h)
```

Now describe a legal move. For a peg in hole *i* to jump *j* and land in *k*, the prior board *p* must have *i* and *j* used and *k* empty, the next board *q* must have *i* and *j* empty and *k* used, and every other hole stays put:

```python
def valid(p, q, i, j, k):
    return And(
        used(p, i), empty(q, i),
        used(p, j), empty(q, j),
        empty(p, k), used(q, k),
        *[unchanged(p, q, h) for h in range(HOLES) if h not in [i, j, k]])
```

A legal transition is any of the 36 valid jumps along a line in `NEIGHBORS`:

```python
def legal_transition(p, q):
    return Or(
        [valid(p, q, *l)
            for n in NEIGHBORS
            for l in [n, reversed(n)]])
```

And then the whole solver:

```python
def solve_tricky_triangle():
    s = Solver()
    for i in range(HOLES - 2):
        s.add(legal_transition(i, i+1))

    s.add(empty(0, 0))

    if s.check() == sat:
        global winning_model, smt2
        winning_model = s.model()
        smt2 = s.to_smt2()
    else:
        print('Failed to solve')
```

The `global` business is just notebook plumbing so later slides can print the boards. The constraints themselves are two lines: thirteen legal transitions in a row, and the top hole starts empty. There’s no loop over moves, no backtracking, and no win condition. I didn’t need one. Every jump removes exactly one peg, and a board with hole 0 empty holds at most fourteen pegs, so the only way to fit thirteen jumps is to start full and finish with one. The arithmetic of the constraints *is* the win condition. Z3 hands back a model in about a second, and printing `Board[0]` through `Board[13]` walks through a winning game, fourteen pegs down to one.

To be fair to brute force, the two programs answer different questions. The search counted every game. Z3 found one winner. If you want a different one, say, finishing with the last peg in a particular hole, you add a constraint rather than rewriting a search. The [standalone script in the repo](https://github.com/gbacon/puzzle15/blob/main/puzzle15.py) does exactly that: it starts with a corner empty and requires the last peg to land in that same corner.

## A peek at the assembly language

Z3 will show you what it actually solved. `s.to_smt2()` dumps the problem in SMT-LIB format, and for this puzzle it runs to nearly 1,500 lines of `declare-fun`, `bvand`, and deeply nested `let` expressions. Nobody wants to write that by hand, which is the point. The Python is the high-level language. The SMT-LIB is the assembly.

## Where the talk goes next

The second half of the talk asks where else the same idea applies and ends up at Turing machines. If you can write down “a nondeterministic machine accepts this input in polynomially many steps” as one enormous but polynomially bounded Boolean formula, then any problem in NP reduces to SAT. That’s Stephen Cook’s 1971 result, and it made SAT the first known NP-complete problem. The slides walk through the construction. For a gentler textbook treatment, Sipser’s [*Introduction to the Theory of Computation*][sipser] covers the Cook–Levin theorem.

That’s also why the peg game makes a better example than it has any right to. SAT is hard in the worst case, P versus NP is still open, and yet a modern solver dispatches a real constraint problem in about a second. Attacking the problem backward, by describing the answer instead of the search, is a powerful technique for your bag of tricks.

## Further reading

* [*The Art of Computer Programming, Volume 4B: Combinatorial Algorithms, Part 2*][taocp4b] by Donald Knuth: backtracking, dancing links, and a long, playful treatment of satisfiability.
* [*Decision Procedures: An Algorithmic Point of View*][kroening] by Daniel Kroening and Ofer Strichman: how SAT and SMT solvers work, including bit vectors, with a chapter co-written by Z3’s Nikolaj Bjørner and Leonardo de Moura.
* [*Introduction to the Theory of Computation*][sipser] by Michael Sipser: Turing machines, NP-completeness, and the Cook–Levin theorem.

The [slides](https://blog.gbacon.com/puzzle15/) and [code](https://github.com/gbacon/puzzle15) are free. The next time you’re stuck waiting for a table, you can win the peg game and still say a computer did it.

[taocp4b]: https://www.amazon.com/dp/0201038064
[kroening]: https://www.amazon.com/dp/3662504960
[sipser]: https://www.amazon.com/dp/113318779X
