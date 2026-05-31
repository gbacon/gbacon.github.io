---
title: "Gyrigrams"
description: "Pairs of words equivalent up to rot13"
author: "Greg Bacon"
date: "2009-07-09"
tags:
  - "Haskell"
---
One way to hide spoilers or off-color comments in plain sight is rot13. The popular Usenet newsreader `trn` even has a builtin command to unmask text protected in this fashion.

To understand rot13, imagine an analog clock face. Instead of the numbers one to twelve, this face has the letters A to Z. To get the secret code for any letter, find the letter on the clock face and advance 13 spots. For example, A becomes N, and X becomes K.

The [jargon file defines rot13](http://www.catb.org/jargon/html/R/rot13.html) as

> [Usenet: from ‘rotate alphabet 13 places’] The simple Caesar-cypher encryption that replaces each English letter with the one 13 places forward or back along the alphabet, so that “The butler did it!” becomes “Gur ohgyre qvq vg!” Most Usenet news reading and posting programs include a rot13 feature. It is used to enclose the text in a sealed wrapper that the reader must choose to open — e.g., for posting things that might offend some readers, or [spoilers](http://www.catb.org/jargon/html/S/spoiler.html). A major advantage of rot13 over rot(N) for other N is that it is self-inverse, so the same code can be used for encoding and decoding. See also [spoiler space](http://www.catb.org/jargon/html/S/spoiler-space.html), which has partly displaced rot13 since non-Unix-based newsreaders became common.

Implementing rot13 is straightforward with the [tr command](https://man7.org/linux/man-pages/man1/tr.1.html):

```bash
tr A-Za-z N-ZA-Mn-za-m
```

The word anagram comes from [a Greek word for shuffling letters](http://www.etymonline.com/index.php?search=anagram&searchmode=none). What about gyrigrams, pairs of words equivalent up to rot13? (The Greek word [γυρίζω](http://en.wiktionary.org/wiki/%CE%B3%CF%85%CF%81%CE%AF%CE%B6%CF%89) means turn or return, so it indicates rotation and also the cipher’s symmetry.)

This post is a literate Haskell program that will find interesting gyrigrams in a dictionary file. Copy-and-paste it into a file named gyrigram.lhs to get a runnable program.

Some front matter:

```haskell
> module Main where
> import Data.Char (toLower)
> import Data.List (sort)
> import qualified Data.Map as M
> import qualified Data.Set as S
> import System.Environment (getArgs, getProgName)
> import System.Exit (ExitCode(ExitFailure), exitWith)
> import System.IO (hPutStrLn, stderr)
```

To run the program, either provide the path to your dictionary file as the sole command-line argument, or omit it to use `/usr/share/dict/words`:

```haskell
> usage :: IO a
> usage = do
>   me <- getProgName
>   hPutStrLn stderr $ "Usage: " ++ me ++ " [ dictionary ]"
>   exitWith (ExitFailure 1)
```

The implementation of rot13 below performs a table lookup for all characters in the input. Characters outside the set [A-Za-z] pass through unchanged.

```haskell
> rot13 :: String -> String
> rot13 = map $ \c -> maybe c id (M.lookup c table)
>   where table = M.fromList $ zip (uc ++ lc) (uc' ++ lc')
>         (uc,  lc)  = (['A'..'Z'], ['a'..'z'])
>         (uc', lc') = (rot uc,     rot lc)
>         rot xs = [drop,take] >>= \f -> f 13 xs
```

To find all gyrigrams, we stuff the input list, normalizing to lowercase, in a [`Set`](https://hackage-content.haskell.org/package/containers/docs/Data-Set.html) for quick lookups. Then for each word in the input, probe for its rot13 counterpart and add hits to the result. Removing matches from the dictionary prevents duplicated values. Note also that we ignore single-letter words.

```haskell
> gyrigrams :: [String] -> [(String,String)]
> gyrigrams xs = go dict xs
>   where go _ [] = []
>         go d (w:ws)
>           | d `has` w' = (w,w') : go d' ws
>           | otherwise  =          go d  ws
>           where has = flip $ S.member . lc
>                 w' = rot13 w
>                 d' = foldr (S.delete . lc) d [w,w']
>         dict = S.fromList $ map lc $ filter ((>1) . length) xs
>         lc = map toLower
```

The main program reads the input and prints a sorted list of pairs:

```haskell
> main :: IO ()
> main =
>   getPath >>= readFile >>= mapM_ (putStrLn . show') .
>                              sort . gyrigrams . lines
>   where show' (a,b) = a ++ " => " ++ b
Argument processing:
> getPath :: IO FilePath
> getPath = getArgs >>= go
>   where go [path] = return path
>         go []     = return "/usr/share/dict/words"
>         go _      = usage
```

One pair is especially interesting because they’re both gyrigrams and synonyms: irk and vex.
