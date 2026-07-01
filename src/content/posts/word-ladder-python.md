---
title: "Word Ladder in Python"
description: "It just looks so boring."
date: "2026-06-16"
author: "Greg Bacon"
tags:
  - "Python"
---
[In an earlier post](/2007/06/word-ladder.html), I described an implementation in Common Lisp of a breadth-first search to find word ladders.

This time I practiced the [kata](http://codekata.pragprog.com/) using Python. Python's [list comprehensions](https://docs.python.org/3/tutorial/datastructures.html#list-comprehensions) help to make the solution concise, but apparently the lunch isn't free. For example, I could have written `one_hop` as

```python
def one_hop(a, b):
  return len([aa for aa, bb in zip(a, b) if aa != bb]) == 1
```

but that resulted in about a twenty percent slowdown.

The code falls out pretty easily:

```python
#! /usr/bin/env python

"""Usage: %(prog)s start-word goal-word [ dictionary-path ]
"""

import sys

prog = sys.argv[0]

def read_words(path):
  words = []

  try:
    f = open(path, "r")
  except IOError, (errno, error):
    sys.stderr.write("%s: open %s: %s\n" % (prog, path, error))
    sys.exit(1)

  for word in f:
    words.append(word[:-1].lower())

  return words

def unpack_args(args):
  dict = "/usr/dict/words"

  if len(args) < 2 or len(args) > 3:
    sys.stderr.write(__doc__ % globals())
    sys.exit(1)

  start, goal = args[0:2]
  if len(args) == 3:
    dict = args[2]

  return (start, goal, dict)

def one_hop(a, b):
  #  return len([aa for aa,bb in zip(a, b) if aa != bb]) == 1
  hops = 0
  for aa, bb in zip(a, b):
    if aa != bb:
      hops += 1

  return hops == 1

def rungs(start, goal, begat):
  path = [goal]
  while path[-1] != start:
    path.append(begat[path[-1]])
  path.reverse()

  return path

def ladder(start, goal, dict):
  if len(start) != len(goal):
    return None

  words = read_words(dict)
  candidates = set([w for w in words if len(w) == len(start)])

  start = start.lower()
  goal  = goal.lower()

  begat = {}

  last = [start]
  while len(last) > 0:
    fringe = []

    for w in last:
      neighbors = [n for n in candidates if one_hop(n, w)]

      for n in neighbors:
        begat[n] = w
        candidates.remove(n)

      if goal in neighbors:
        return rungs(start, goal, begat)
      else:
        fringe.extend(neighbors)

    last = fringe
  else:
    return None

def main(args):
  start, goal, dict = unpack_args(args)

  path = ladder(start, goal, dict)
  if path is None:
    print "%s: no path from '%s' to '%s'" % (prog, start, goal)
  else:
    for w, i in zip(path, range(1, len(path) + 1)):
      print "%3d. %s" % (i, w)

  return 0

if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
```
