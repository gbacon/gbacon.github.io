---
layout: post
title: My first monadic program!
permalink: /2007/02/my-first-monadic-program.html
comments: true
tags: [Haskell, Project Euler]
---
[Problem 32]: http://projecteuler.net/index.php?section=problems&id=32
[Project Euler]: http://projecteuler.net/

[Problem 32] from [Project Euler] reads

> The product 7254 is unusual, as the identity, 39 × 186 = 7254,
> containing multiplicand, multiplier, and product is 1 through 9
> pandigital.
>
> Find the sum of all products whose multiplicand/multiplier/product
> identity can be written as a 1 through 9 pandigital.

Not one of my brighter moments, the first approach I considered was
applying *goon*–force to permute a list of eleven characters
(`'1' .. '9'`, `'x'`, and `'='`) — a search space of almost 40 million
— looking for well-formed and valid statements.

[Text.Regex]: http://haskell.org/ghc/docs/latest/html/libraries/regex-compat/Text-Regex.html
[Parsec]: http://www.cs.uu.nl/~daan/parsec.html

Initially, I looked at using [Text.Regex]. As I was fishing for examples
to crib from, I saw a suggestion that people may as well use
[Parsec], the monadic parser combinator library. So let’s knock out
the preliminaries:

{% highlight literate-haskell %}
> module Main where
>
> import Data.List hiding (map)
> import Text.ParserCombinators.Parsec
{% endhighlight %}

A parser for the simple expressions reads easily:

{% highlight literate-haskell %}
> num :: Parser Int
> num = do ns <- many1 digit
>          return $ read ns
>
> expression :: Parser (Int,Int,Int)
> expression = do multiplier <- num
>                 times <- char 'x'
>                 multiplicand <- num
>                 equals <- char '='
>                 product <- num
>                 return (multiplier, multiplicand, product)
{% endhighlight %}

Generating permutations is no problem in Haskell:

{% highlight literate-haskell %}
> permutations :: [a] -> [[a]]
> permutations [x] = [[x]]
> permutations xs =
>   [ y : zs
>   | (y,ys) <- selections xs
>   , zs     <- permutations ys
>   ]
>
> selections []     = []
> selections (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- selections xs]
{% endhighlight %}

I’d been wanting to solve as a learning example one of the
Project&nbsp;Euler problems using monads. As I tried to shoehorn the
problem into a monadic solution, I remembered the characterization of
the `Maybe` monad as being useful for computations that can fail,
and I saw two possibilities for failure: garbage input (*e.g.*,
`"x=123456789"`) and false statements (*e.g.*, `"1x2=3456789"`).

Having a particular permutation, `check` tests whether it’s
well–formed and valid:

{% highlight haskell %}
check :: String -> Maybe Int
check s = do
  result <- parseExpr s
  p <- validProduct result
  return p

parseExpr :: String -> Maybe (Int,Int,Int)
parseExpr s =
  case parse expression "expression" s of
    Left err -> Nothing
    Right tuple@(mr,md,pr) -> return tuple

validProduct (mr, md, pr)
  | mr * md == pr = Just pr
  | otherwise = Nothing
{% endhighlight %}

The test goes just as you would describe it to someone else: parse the
input to extract the components of the mulitiplication and then check
whether the multiplication holds. Simple.

You wonder, *‘But what about when the parse fails or when the statement
is bogus?’* Those checks are still happening, but the `Maybe` monad and
the do-notation syntactic sugar are performing those checks implicitly!
“Control” (to borrow an imperative concept) reaches the `return p` line
only if both the parse and `validProduct` succeed. Otherwise, `check`
bails and returns `Nothing`.

All that’s left to do is feed it input and sum the result. Note: it’s
very s-l-o-w.

{% highlight haskell %}
p32 = sum $ elems $ fromList $ catMaybes $ map check (permutations cs)
  where cs = ['1','2','3','4','5','6','7','8','9','x','=']
{% endhighlight %}

[nub]: http://haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html#v%3Anub

An obvious improvement is to use [`nub`][nub] instead of
`(elems . fromList)`. Even better, lists are also monads, so
`Maybe` distractions disappear with only a few very minor changes:

{% highlight literate-haskell %}
> p32 = sum (nub $ concatMap check (permutations cs))
>   where cs = ['1','2','3','4','5','6','7','8','9','x','=']
>
> main = print p32
>
> check :: (Monad m) => String -> m Int
> check s = do
>   result <- parseExpr s
>   p <- validProduct result
>   return p
>
> parseExpr :: (Monad m) => String -> m (Int,Int,Int)
> parseExpr s =
>     case parse expression "expression" s of
>       Left err -> fail (show err)
>       Right tuple@(mr,md,pr) -> return tuple
>
> validProduct :: (Monad m) => (Int,Int,Int) -> m Int
> validProduct (mr, md, pr)
>   | mr * md == pr = return pr
>   | otherwise = fail "invalid product"
{% endhighlight %}

This works because failure in the list monad is the empty list, and
`concatMap` gets rid of all the empties.
