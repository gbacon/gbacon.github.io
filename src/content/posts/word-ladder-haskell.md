---
title: "Word Ladder in Haskell"
description: "So pure"
date: "2026-06-17"
author: "Greg Bacon"
tags:
  - "Haskell"
---
Another followup to [my earlier post about searching for word ladders](/2007/06/word-ladder.html), this time using
[Haskell](https://www.haskell.org/)!

This blog post is a Haskell program, written using the
[“literate comment” convention](http://www.haskell.org/onlinereport/literate.html).

First a bit of front matter. This implementation of the word-ladder
search will use the <a href="http://www.haskell.org/ghc/docs/latest/html/libraries/mtl/Control-Monad-State.html">State</a>
and <a href="http://www.haskell.org/all_about_monads/html/meet.html#list">list</a>
monads.

```haskell
> module Main where
> import Control.Monad.State
> import Data.Char
> import Data.List (find)
> import Data.Set (Set, member, difference)
> import qualified Data.Set as Set
> import System.Environment (getArgs)
> import System.Exit
```

The idea is simple: read the dictionary, search for the desired
ladder, and show it to the user:

```haskell
> main :: IO ()
> main = do
>   (start, goal, dict) <- getArgs >>= parse
>   fullDictionary <- readDictionary dict
>   print $ search start goal (trim fullDictionary start)
```

Remember that <a href="http://www.haskell.org/ghc/docs/latest/html/libraries/base/System-Environment.html#v%3AgetArgs">getArgs</a>
is an action that returns the list of command-line arguments. We bind
this action to the following:

```haskell
>   where parse [start,goal,dict] = return (start,goal,dict)
>         parse [start,goal]      = return (start,goal,"/usr/dict/words")
>         parse _ =
>           putStrLn "Usage: ladder start goal [ dictionary ]" >>
>           exitWith (ExitFailure 1)
```

Haskell's pattern matching shows that the program takes two or three
arguments. The first two are the start and goal words. The optional third
argument is the path to a dictionary (one word per line) to use.

We condition the dictionary by eliminating words whose lengths differ
from the length of the start word and also converting everything to
lowercase.

```haskell
>         trim :: [String] -> String -> [String]
>         trim words start = filter (sameLength start) (lc words)
>
>         sameLength start = (== length start) . length
>
>         lc = map (map toLower)
```

The search can fail, so result is of type
[Maybe](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-Maybe.html) \[String]. Handling both cases is straightforward:

```haskell
>         print Nothing = putStrLn "No ladder found."
>         print (Just a) = mapM_ putStrLn a
```

The dictionary's format is simple, so reading it is a matter of
extracting the lines from the file:

```haskell
> readDictionary :: FilePath -> IO [String]
> readDictionary path = liftM lines $ readFile path
```

Now for the fun bits. Imagine a graph where nodes are words from
the dictionary and where edges are between words that are "one hop'
from each other, *i.e.*, words that could be on consecutive
“rungs” of a ladder.

Beginning with the start word, the program performs a breadth-first
search of this graph. We call the set of words reached in the most
recent iteration the “fringe.” When the fringe contains the goal
word, we're done.

The state monad simulates destructive update in imperative
programming languages. Haskell is purely functional. Without
it, we'd have to explicitly thread the state value through the
call chain, but with it, we retrieve and update the state value
with get and put as below:

```haskell
> search start goal words =
>   evalState (loop [[start]]) (Set.fromList $ filter (/=start) words)
>   where
>     loop :: [[String]] -> State (Set String) (Maybe [String])
>     loop [] = return Nothing
>     loop paths = do
>       next <- step paths
>       let newFringe = fringe next
>       words <- get
>       put $ words `difference` newFringe
>       if goal `member` newFringe
>         then return $ Just (winner next)
>         else loop next
```

The list monad is handy for representing nondeterministic
computations. In concept at least, the search carries around a
list of lists that has all of the partial results computed so far.

For example,
if the start word is dog, the state value on the second iteration
might be `[["dog", "dig"], ["dog", "fog"], ["dog", "bog"]]`. This
approach might seems to be a memory pig, but it remains surprisingly
frugal.

To proceed to the next iteration of the search, for each partial
result (one ladder beginning with the start word) we find the
as-yet unseen neighbors of its last element (a member of the
current fringe) and replace the current partial result with new ones
for each of the neighbors. Again, consider the partial results at
the second iteration in the previous paragraph.

```haskell
>     step :: [[String]] -> State (Set String) [[String]]
>     step paths = do
>       words <- get
>       return $ paths >>= augment words
>
>     augment :: Set String -> [String] -> [[String]]
>     augment words path = [ path ++ [n] | n <- ns ]
>       where ns = Set.elems $ neighbors (last path) words
>     
>     neighbors :: String -> Set String -> Set String
>     neighbors word words = Set.filter (oneHop word) words
>       where oneHop [] [] = False
>             oneHop (x:xs) (y:ys) | x /= y = xs == ys
>                                  | otherwise = oneHop xs ys
```

As described above, the fringe is the set of words at the
ends of the partial ladders computed so far:

```haskell
>     fringe :: [[String]] -> Set String
>     fringe paths = Set.fromList (map last paths)
```

Once we've seen the goal in the fringe, we return
the ladder that ends with the goal word:

```haskell
>     winner :: [[String]] -> [String]
>     winner paths =
>       case (find ((== goal) . last) paths) of
>         Nothing -> undefined
>         Just a -> a
```
