---
title: "Setting up a simple test with Cabal"
description: "Testing the Haskell way."
date: "2009-06-23"
author: "Greg Bacon"
tags:
  - "Haskell"
---
With the [Cabal](http://www.haskell.org/cabal/) build and packaging system for Haskell, add a simple test program to your build with a couple of easy steps.

First, add the following to your project's cabal file:

```haskell
Build-Type: Custom

...

flag test
  description: Build test program.
  default:     False

Executable test
  hs-source-dirs:  src, test
  other-modules:   MyModule1, MyModule2
  main-is:         Main.hs
  build-depends:   base
  if !flag(test)
    buildable:     False
```

When enabled (via `cabal configure -ftest` but otherwise off), this builds an extra program called `test`.

The custom build type gives you more flexibility in your setup script, so add code such as the following to `Setup.hs`:

```haskell
main = defaultMainWithHooks hooks
  where hooks = simpleUserHooks { runTests = runTests' }

runTests' :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runTests' _ _ _ lbi = system testprog >> return ()
  where testprog = (buildDir lbi) </> "test" </> "test"
```

When you run `cabal test`, it will kick off your test program whose source is in `test/Main.hs`.

This approach has a few drawbacks. Users must explicitly enable the test builds. Building the test program entails *rebuilding* the other libraries in your package. Installing from a `-ftest` configuration will also install your test program.
