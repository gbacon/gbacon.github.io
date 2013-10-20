---
layout: post
title: Cleaning up your Haskell imports
permalink: /2009/06/cleaning-up-your-haskell-imports.html
tags: [Haskell]
comments: true
---
[1]: http://www.haskell.org/ghc/docs/6.0/html/users_guide/sooner-faster-quicker.html
[2]: http://www.haskell.org/ghc/docs/6.10.3/html/users_guide/ghci-commands.html#id2899386
[Hoogle]: http://www.haskell.org/hoogle/

Explicit imports have a couple of benefits. For one, doing so [reduces
compile times with ghc][1]. Another is giving a hand to your future self
(or other maintainers) and especially to those who are reading your code
to learn. We’ve all been there: scratching our heads wondering, ‘Where
does *that* function live?’ Yes, [ghci’s `:info` command][2] and
[Hoogle] are your friends, but explicit imports right there in your code
will give the answer in a snap.

[3]: http://neilmitchell.blogspot.com/2009/01/small-scripts-with-haskell.html

[Neil Mitchell calls explicit imports “needlessly verbose,”][3]
certainly a fair point in the context where he made it, so this is a
matter of polish, not strict necessity. There’s also a certain
aspy-appeal to it.

[4]: http://www.haskell.org/ghc/docs/6.10.3/html/users_guide/separate-compilation.html#id2915722

The [`-ddump-minimal-imports` option to ghc][4] writes the cleaned–up
list to `M.imports`, where <i>M</i> is the module being compiled.
For example, consider the following code that finds anagrams in a
dictionary file:

{% highlight haskell %}
module Main where

import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import Data.Map hiding (filter, map)
import System.Environment
import System.Exit
import System.IO

usage :: IO a
usage = do
  me <- getProgName
  hPutStrLn stderr $ "Usage: " ++ me ++ " [ dictionary ]"
  exitWith (ExitFailure 1)

main :: IO ()
main =
  getPath >>= readFile >>= mapM_ (putStrLn . unwords) . sorted
  where sorted = sort . map sort . anagrams . lines

anagrams :: [String] -> [[String]]
anagrams words = filter ((>1) . length) equiv
  where equiv = elems $
                  fromListWith (++)
                    [ (normal w, [w]) | w <- words ]
        normal = sort . map toLower

getPath :: IO FilePath
getPath = getArgs >>= go
  where go [path] = return path
        go []     = return "/usr/share/dict/words"
        go _      = usage
{% endhighlight %}

To get the minimal set of imports:

<pre>$ ghc-6.10.3 -ddump-minimal-imports --make anagram.hs
$ cat Main.imports
import System.IO(IO, FilePath, putStrLn, readFile, hPutStrLn,
                 stderr)
import Data.Map(elems, fromListWith)
import Control.Arrow()    -- Instances only
import Control.Monad(Monad(return, (>>=)), mapM_)
import Data.Char(String, toLower)
import Data.List((++), filter, map, length, lines, unwords, sort)
import System.Environment(getArgs, getProgName)
import System.Exit(ExitCode(ExitFailure), exitWith)</pre>

[5]: http://help.eclipse.org/help33/index.jsp?topic=/org.eclipse.jdt.doc.user/reference/ref-22.htm

Although nice, the result is less than satisfying. The cuddled lists are
ugly. The imports are in an odd order. Having to do run a separate
compilation by hand followed by copy-paste, as opposed to automatically
à la [Eclipse’s organize imports for Java][5], is a bit of a pain.

Notice that although Control.Arrow is unnecessary, it remains in the
“minimal” set with an empty import list. Its presence is an artifact of
the list comprehension being equivalent to

{% highlight haskell %}
map (normal &&& (:[])) words
{% endhighlight %}

Cool, yes. Readable, not so much.

[6]: http://hackage.haskell.org/trac/ghc/ticket/1792

Note also there’s an [open ticket against ghc concerning the
interaction between `-ddump-minimal-imports` and qualified imports][6].
