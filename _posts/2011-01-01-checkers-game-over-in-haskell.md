---
layout: post
title: Checkers game-over in Haskell
permalink: /2011/01/checkers-game-over-in-haskell.html
tags: [Haskell]
comments: true
---
[1]: http://www.reddit.com/r/compsci/comments/esbpe/anyone_have_a_good_algorithm_for_checking_the/
[2]: http://haskell.org/tutorial/patterns.html

The programming subreddit recently had a discussion about [testing a
checkers board for game-over][1]. I wondered how specifying the rules
for legal moves would look with [Haskell’s pattern matching][2], and
this post is a study of that technique. In fact, you can run yourself.
Copy-and-paste the post body to a file named `Checkers.lhs` to get a
working program!

[American checkers]: http://en.wikipedia.org/wiki/English_draughts

The game is [American checkers] or English draughts, played on an
eight–by–eight checkerboard, of all surfaces.

{% highlight literate-haskell %}
> {-# LANGUAGE ViewPatterns #-}
> module Checkers where
> import Data.Char (toLower,toUpper)
> import Data.List (tails,transpose)
> import Test.HUnit
> data Board = Board [String] deriving (Show)
> size :: Int
> size = 8
{% endhighlight %}

For a rough idea of the punchline, I was hoping for code along the lines of

{% highlight haskell %}
move ('w':' ':_)     = 1
move ('W':' ':_)     = 1
move (' ':'W':_)     = 1
move ('w':'b':' ':_) = 1
move ('w':'B':' ':_) = 1
move ('W':'b':' ':_) = 1
move ('W':'B':' ':_) = 1
move (' ':'b':'W':_) = 1
move (' ':'B':'W':_) = 1
move _ = 0
{% endhighlight %}

and eventually

{% highlight literate-haskell %}
> gameOver :: Board -> Bool
> gameOver b = blueMoves b == 0 || whiteMoves b == 0
{% endhighlight %}

The OP on reddit chose white and blue for the sides’ colors, and above
we have more–or–less declarative rules for legal white moves. A pawn or
king (`w` and `W` respectively) can move to an empty space before it.
Kings are special in that they can move backwards. The list ends with
legal jumps, and everything else is invalid.

The code is repetitive, but I’ll clean that up later.

[IArray]: http://en.wikibooks.org/wiki/Haskell/Hierarchical_libraries/Arrays
[bangbang]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v%3A-33--33-

An immediate problem is the patterns are linear, but all legal moves in
checkers are along diagonals. I kicked around ideas such as using [IArray]
or nasty double-applications of [`!!`][bangbang]. Then I realized I could
rotate the board by 45° with a shear, a transposition, and removal of
placeholders.

{% highlight haskell %}
-- diagonals with positive slopes
posdiags = map reverse . filter used . transpose . map shear . zip [0..]
  where shear (i,s) = (replicate i              '#') ++ s ++
                      (replicate (k - size - i) '#')
        k = 2 * size - 1
        used = not . all (`elem` "#.")
{% endhighlight %}

Getting the other diagonals is similar, but again brings too much
repetition.

{% highlight haskell %}
negdiags = map reverse . filter used . transpose . map shear . zip [0..]
  where shear (i,s) = (replicate (k - size - i) '#') ++ s ++
                      (replicate i              '#')
        k = 2 * size - 1
        used = not . all (`elem` "#.")</pre></div></div>
{% endhighlight %}

Having `Board` values to play with is trivial:

{% highlight literate-haskell %}
> board :: String -> Board
> board s = Board $ go s
>   where go [] = []
>         go xs = let (a,bs) = splitAt size xs
>                 in a : go bs
{% endhighlight %}

It chops one long string into rows, but with Haskell’s usually–awkward
multiline strings, it’s not so bad. For example

{% highlight haskell %}
startBoard =
  ".b.b.b.b\
  \b.b.b.b.\
  \.b.b.b.b\
  \ . . . .\
  \. . . . \
  \w.w.w.w.\
  \.w.w.w.w\
  \w.w.w.w."
{% endhighlight %}

An early cut at `blueMoves` and reducing the repetition in the rules for
moves was

{% highlight haskell %}
blueMoves :: Board -> Int
blueMoves (diagonals -> (p,n)) =
  sum $ map move $ concatMap tails $ p ++ n
  where move ( b :' ':_) | b `elem` "Bb" = 1
        move (' ':'B':_) = 1
        move ('b': w :' ':_) | w `elem` "Ww" = 1
        move (' ': w :'B':_) | w `elem` "Ww" = 1
        move _ = 0
{% endhighlight %}

Sticking with the theme of repetition, `whiteMoves` is nearly identical
with little breadcrumbs of differences. That was all good because I
wanted to have a testsuite before I started refactoring.

{% highlight haskell %}
tests :: Test
tests = test
  [ assertEqual "white must have piece to move"
      0 (nw ".b.b.b.b\
            \b.b.b.b.\
            \.b.b.b.b\
            \ . . . .\
            \. . . . \
            \ . . . .\
            \. . . . \
            \ . . . .")
  ]
  where nw = whiteMoves . board
{% endhighlight %}

Not bad for a start, but each testcase will have a dual for the other
side—way too much copy-and-paste.

<pre>*Checkers> runTestTT tests
Loading package HUnit-1.2.2.1 ... linking ... done.
Cases: 1  Tried: 1  Errors: 0  Failures: 0
Counts {cases = 1, tried = 1, errors = 0, failures = 0}</pre>

Whee!

[th]: http://www.haskell.org/haskellwiki/Template_Haskell

To wring out the duplication in the code for each side’s moves, I
considered using [Template Haskell][th]—a cousin of Lisp macros for
Haskell. I decided to push lexical closures as far as I could, and the
result is below.

{% highlight literate-haskell %}
> blueMoves, whiteMoves :: Board -> Int
> [blueMoves, whiteMoves] =
>   let blueOrder = id  -- diagonals emerge in blue’s perspective
>       whiteOrder = map reverse
>       count (direction,side) (diagonals -> ds) =
>         sum $ map sideCanMove $ concatMap tails $ direction $ ds
>         where sideCanMove ( p :' ':_)     | same p = 1
>               sideCanMove (' ': k :_)     | king k = 1
>               sideCanMove ( p : o :' ':_) | same p && opponent o = 1
>               sideCanMove (' ': o : k :_) | king k && opponent o = 1
>               sideCanMove _ = 0
>               same p     = piece p && toLower p == toLower side
>               opponent p = piece p && toLower p /= toLower side
>               king p     =  same p &&         p == toUpper side
>               piece p    = p `elem` "BbWw"  -- filter empty spaces
>   in map count [ (blueOrder, 'b'), (whiteOrder, 'w') ]</pre></div></div>
{% endhighlight %}

[view pattern]: http://hackage.haskell.org/trac/ghc/wiki/ViewPatterns

The code in `count` (notice the [view pattern]?) is a skeleton to be
customized for the blue side and the white side, and it distills the
repeated code. The definition of `sideCanMove` generalizes the rules
for legal moves on either side. We have to reverse the diagonals to
make them usable on the white side.

To get both sets of diagonals, the only difference is how to shear the
board: bottom-away for positive slopes and top-away for negative.

{% highlight literate-haskell %}
> -- positive slopes slice from NW to SE
> -- negative slopes slice from SW to NE
> -- both extend in blue’s direction (north-to-south)
> diagonals :: Board -> [String]
> diagonals (Board rows) = positiveSlopes rows ++ negativeSlopes rows
>   where positiveSlopes = go $ \(i,xs) -> (i, k - size - i, xs)
>         negativeSlopes = go $ \(i,xs) -> (k - size - i, i, xs)
>         k = 2 * size - 1
>         used = not . all ignored
>         go order = filter (not . null)
>                  . map (filter $ not . ignored)
>                  . transpose
>                  . map (shear . order)
>                  . zip [0..]
>         ignored = (`elem` "#.")
>         shear (l,r,s) = (replicate l '#') ++ s ++ (replicate r '#')
{% endhighlight %}

Finally come the tests that I added as I went. To factor out
duplication, each board becomes two testcases. The first is as–is,
and the same condition should hold for the other side. See the
definition of `invert` in the `where` clause.

I had hoped for a more elegant result, but it was an interesting
exercise and a fun problem!

{% highlight literate-haskell %}
> tests :: Test
> tests = test $ concat
>   [ checkMoves "must have piece to move"
>       0 ".b.b.b.b\
>         \b.b.b.b.\
>         \.b.b.b.b\
>         \ . . . .\
>         \. . . . \
>         \ . . . .\
>         \. . . . \
>         \ . . . ."
>   , checkMoves "one move"
>       1 ". . . . \
>         \ . . . .\
>         \. . . . \
>         \ . . . .\
>         \. . . . \
>         \ . . . .\
>         \. . . . \
>         \w. . . ."
>   , checkMoves "one king move"
>       1 ". . . . \
>         \ . . . .\
>         \. . . . \
>         \ . . . .\
>         \. . . . \
>         \ . . . .\
>         \. . . . \
>         \W. . . ."
>   , checkMoves "two moves"
>       2 ". . . . \
>         \ . . . .\
>         \. . . . \
>         \ . . . .\
>         \. . . . \
>         \ . . . .\
>         \. . . . \
>         \ .w. . ."
>   , checkMoves "king can move back from end"
>       2 ". . .W. \
>         \ . . . .\
>         \. . . . \
>         \ . . . .\
>         \. . . . \
>         \ . . . .\
>         \. . . . \
>         \ . . . ."
>   , checkMoves "can jump opponent pawn"
>       1 ". . . . \
>         \ . . . .\
>         \. . . . \
>         \ . . . .\
>         \. . . . \
>         \ . . . .\
>         \.b. . . \
>         \w. . . ."
>   , checkMoves "can't jump blocked opponent"
>       0 ". . . . \
>         \ . . . .\
>         \. . . . \
>         \ . . . .\
>         \. . . . \
>         \ .b. . .\
>         \.b. . . \
>         \w. . . ."
>   , checkMoves "can jump opponent king"
>       1 ". . . . \
>         \ . . . .\
>         \. . . . \
>         \ . . . .\
>         \. . . . \
>         \ . . . .\
>         \.B. . . \
>         \w. . . ."
>   , checkMoves "king can jump trailing opponent"
>       1 ". . . .W\
>         \ . . .b.\
>         \. . . . \
>         \ . . . .\
>         \. . . . \
>         \ . . . .\
>         \. . . . \
>         \ . . . ."
>   , checkMoves "king can't jump protected opponent"
>       0 ". . . .W\
>         \ . . .b.\
>         \. . .b. \
>         \ . . . .\
>         \. . . . \
>         \ . . . .\
>         \. . . . \
>         \ . . . ."
>   , checkMoves "king can't jump onto own piece"
>       1 ". . . .W\
>         \ . . .b.\
>         \. . .w. \
>         \ . . . .\
>         \. . . . \
>         \ . . . .\
>         \. . . . \
>         \ . . . ."
>   , checkMoves "king has four moves"
>       4 ". . . . \
>         \ . . . .\
>         \. . . . \
>         \ . .W. .\
>         \. . . . \
>         \ . . . .\
>         \. . . . \
>         \ . . . ."
>   , checkMoves "cannot displace opponent on king row"
>       0 ". . .b.b\
>         \ . . .w.\
>         \. . . . \
>         \ . . . .\
>         \. . . . \
>         \ . . . .\
>         \. . . . \
>         \ . . . ."
>   ]
>   where nw = whiteMoves . board
>         nb = blueMoves  . board
>         checkMoves name expect b =
>           [ assertEqual ("white: " ++ name) expect (nw b)
>           , assertEqual ("blue: "  ++ name) expect (nb $ invert b)
>           ]
>         invert = reverse . replace [('W','B'), ('w','b'), ('B','W'), ('b','w')]
>         replace tbl = map (\c -> maybe c id $ lookup c tbl)
{% endhighlight %}
