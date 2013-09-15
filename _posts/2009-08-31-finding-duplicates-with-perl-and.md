---
layout: post
title: Finding duplicates with Perl and Haskell
permalink: /2009/08/finding-duplicates-with-perl-and.html
tags: [Perl, Haskell]
comments: true
---
A coworker wanted to check a family of log files to be sure that a given
task never appeared on multiple nodes at the same time. Log entries are
on single, whitespace-separated lines, and the last field records a
task’s start time, *e.g.*, 

<pre>1251475056672590000_1732248586_4</pre>

Of the three underscore–separated fields, the first is a timestamp, the
second we don’t care about, and the third is a task identifier. 

[diamond]: http://perldoc.perl.org/perlop.html#I/O-Operators

This task is straightforward with Perl. The diamond operator (or
[null filehandle, as described in the “I/O Operators” section of the
perlop manpage][diamond]) takes care of the boilerplate for iterating
over the paths on the command line, opening them, and reading each
line. The scalar `$ARGV` contains the name of the current file. 

[split]: http://perldoc.perl.org/functions/split.html
[push]: http://perldoc.perl.org/functions/push.html

By default, [`split`][split] separates fields by whitespace, so
`(split)[-1]` gives us the last field, from which we then grab the
time and task with a regular expression and record its presence by
[pushing][push] the entry’s path and line number onto an array
associated with that time/task pair. After we’ve processed the logs,
these arrays should all be singletons. 

[dollardot]: http://perldoc.perl.org/perlvar.html#%24.

The continue clause is a little weird but necessary because the
[special variable `$.`][dollardot], the current line number, does not
reset on `<>`’s implicit opens. `ARGV` is a handle on the file being
read. 

With this data structure, detecting duplicates is a search for time/task
pairs with multiple hits. We count duplicates and let the user know what
we found. 

{% highlight perl %}
#! /usr/bin/perl
use warnings;
use strict;

# e.g., $hits = @{ $seen{$time}{$task} };
my %seen;

sub num { $a <=> $b }

while (<>) {
    if ((split)[-1] =~ /^(\d+)_\d+_(\d+)$/) {
        my($time,$task) = ($1,$2);
        push @{ $seen{$time}{$task} } => "$ARGV:$.";
    }
    else {
        die "$0: $ARGV:$.: bad timestamp/task field\n";
    }
}
continue {
    close ARGV if eof;
}

my $duplicates = 0;
foreach my $time (sort num keys %seen) {
    foreach my $task (sort num keys %{ $seen{$time} }) {
        my @hits = @{ $seen{$time}{$task} };
        next if @hits == 1;

        $duplicates += @hits - 1;
        warn "$0: duplicates for time=$time, task=$task:\n",
             map "    - $_\n", @hits;
    }
}

my $s = $duplicates == 1 ? "" : "s";
print "$0: $duplicates duplicate$s detected.\n";

exit $duplicates == 0 ? 0 : 1;
{% endhighlight %}

For comparison, I implemented the same log checker in Haskell. The
function `allInputs` emulates Perl’s diamond operator, and instead
of a multi–level hash, the association is more direct: time/task
pair to a list of hits. 

{% highlight haskell %}
module Main where

import Control.Monad (liftM)
import Data.List (sort)
import Data.Map (empty,filter,fromListWith,toList,unionWith)
import Prelude hiding (filter)
import System.Environment (getArgs,getProgName)
import System.Exit (ExitCode(..),exitWith)
import Text.Printf (printf)

type Time = String
type Task = String
data Duplicates =
   Duplicates { timestamp :: Time
              , taskId    :: Task
              , locations :: [(FilePath, Int)]
              }

main :: IO ()
main = do
    logs <- allInputs
    let multi = dups logs
        n = sum $ map (subtract 1 . length . locations) multi
     mapM_ (msg . lines . dupmsg) multi
     msg $ ndups n
     exitWith $ if n == 0
                then ExitSuccess
                else ExitFailure 1
    where
        msg info = do me <- getProgName
                      putStrLn $ me ++ ": " ++ head info
                      mapM_ putStrLn (tail info)

        ndups 1 = ["1 duplicate detected"]
        ndups n = [show n ++ " duplicates detected"]

        dupmsg (Duplicates tm task ls) = unlines $
            printf "duplicates for time=%s, task=%s:" tm task :
            map (\(path,n) -> printf "    - %s:%d" path n) ls

allInputs :: IO [(FilePath, String)]
allInputs = getArgs >>= go
    where go [] = ((:[]) . (,) "-") `liftM` getContents
           go fs = mapM readFile fs >>= return . zip fs

dups :: [(FilePath, String)] -> [Duplicates]
dups = map (\((tm,task),ds) -> Duplicates tm task ds) .
       sort .
       toList .
       filter ((> 1) . length) .
       foldl (unionWith (++)) empty .
       map (\(path, contents) ->
              fromListWith (++) $
              map (wrap path . getTimeTask) $
              zip [1..] $ lines contents)
    where
        wrap path (tm,task,n) = ((tm,task), [(path,n)])

getTimeTask :: (Int,String) -> (Time,Task,Int)
getTimeTask (n,line) = (tm,tsk,n)
    where
        [tm,_,tsk] = splitBy '_' (last $ words line)

        splitBy :: Eq a => a -> [a] -> [[a]]
        splitBy _ [] = []
        splitBy x xs = h : splitBy x t
            where (h,rest) = break (== x) xs
                  t = drop 1 rest
{% endhighlight %}
