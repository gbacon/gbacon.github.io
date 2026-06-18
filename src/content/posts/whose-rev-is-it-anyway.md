---
title: "Whose rev is it anyway?"
description: "Using git to prevent the finger-pointing game before it starts."
date: "2026-06-18"
author: "Greg Bacon"
tags:
  - "git"
---
Once upon a time, a teammate reported an inconsistency between our code and documentation: the cloud altitude in the rain model is supposed to be in units of meters with a default of 3km, but the default in the code was `10`.

We checked both snapshots we thought they had, but both were in order. The last time the default changed in the trunk was over a year ago, and that was a change in units (*i.e.*, `3.0` to `3000.0`). ‘Maybe they changed the code,’ I thought but then remembered that the finger-pointing game is an evil at whose very root we must strike!

Principle is great — in principle — but now I had to hunt through more than a hundred tags to clear our name. That’s a lot of clicky-clicky in the HTTP view. Instead, I could pull copies of `rain_model.c` from all hundred-plus tags and grep those.

Ugh. There ought to be a quicker way.

Then I remembered importing our Subversion repository into a Git repository using `git-svn`. With `git-grep`, searching through all those revisions is straightforward:

```bash
$ git grep 'cloud_altitude *= *[^3 ]' \
  `git branch -a | grep tags` -- \
  libs/env/rain_model.c
```

Joy!

The `[^3 ]` bit in the search pattern means find a character that’s neither a `3` nor a space, the latter being necessary to prevent spuriously matching a space to the left of the value being assigned — effectively asking for all assignments in all tags to `cloud_altitude`. Not what we want.

Unlike Subversion, Git’s operations are almost all local. That means fast! The above search ran in less than a quarter of a second.

Turns out the weird default was our doing after all, from a nearly two-year-old engineering release. Here’s to keeping egg off our faces!
