---
title: "Learning Regular Expressions"
description: "Don’t fear the regex!"
author: "Greg Bacon"
date: "2026-06-30"
tags:
  - "Regex"
  - "Computer Science"
---
The most important part to learning regular expressions or “regex” is the concepts. Once you understand how the building blocks work, differences in syntax amount to little more than mild dialects. A layer on top of your regular expression engine’s syntax is the syntax of the programming language you’re using. Languages such as Perl remove most of this complication, but you’ll have to keep in mind other considerations if you’re using regular expressions in a C program.

If you think of regular expressions as building blocks that you can mix and match as you please, it helps you learn how to write and debug your own patterns but also how to understand patterns written by others.

## Start simple

Conceptually, the simplest regular expressions are literal characters. The pattern `N` matches the character ‘N’.

Regular expressions next to each other match sequences. For example, the pattern `Nick` matches the sequence ‘N’ followed by ‘i’ followed by ‘c’ followed by ‘k’.

If you’ve ever used `grep` on Unix—even if only to search for ordinary looking strings—you’ve already been using regular expressions! (The `re` in `grep` refers to regular expressions.)

## Order from the menu

Adding just a little complexity, you can match either ‘Nick’ or ‘nick’ with the pattern `[Nn]ick`. The part in square brackets is a *character class*, which means it matches exactly one of the enclosed characters. You can also use ranges in character classes, so `[a-c]` matches either ‘a’ or ‘b’ or ‘c’ — but only once, not repeated.

The pattern `.` is special: rather than matching a literal dot only, it matches *any* character<sup>†</sup>. It’s the same conceptually as the really big character class `[-.?+%$A-Za-z0-9...]`.

Think of character classes as menus: pick just one.

## Helpful shortcuts

Using `.` can save you lots of typing, and there are other shortcuts for common patterns. Say you want to match a digit: one way to write that is `[0-9]`. Digits are a frequent match target, so you could instead use the shortcut `\d`. Others are `\s` (whitespace) and `\w` (word characters: alphanumerics or underscore).

The uppercased variants are their complements, so `\S` matches any *non*-whitespace character, for example.

## Once is not enough

From there, you can repeat parts of your pattern with *quantifiers*. For example, the pattern `ab?c` matches ‘abc’ or ‘ac’ because the `?` quantifier makes the subpattern it modifies optional. Other quantifiers are

* `*` (zero or more times)
* `+` (one or more times)
* `{n}` (exactly *n* times)
* `{n,}` (at least *n* times)
* `{n,m}` (at least *n* times but no more than *m* times)

Putting some of these blocks together, the pattern `[Nn]*ick` matches all of

* ick
* Nick
* nick
* Nnick
* nNick
* nnick
* *(and so on)*

The first match demonstrates an important lesson: *`*` always succeeds!* Any pattern can match zero times. The same is true for the `?` quantifier.

A few other useful examples:

 * `[0-9]+` (and its equivalent `\d+`) matches any non-negative integer
 * `\d{4}-\d{2}-\d{2}` matches dates formatted like 2019-01-01
   * &hellip; but also matches invalid dates like 9999-99-99

## Grouping

A quantifier modifies the pattern to its immediate left. You might expect `0abc+0` to match ‘0abc0’, ‘0abcabc0’, and so forth, but the pattern *immediately* to the left of the plus quantifier is `c`. This means `0abc+0` matches ‘0abc0’, ‘0abcc0’, ‘0abccc0’, and so on.

To match one or more sequences of ‘abc’ with zeros on the ends, use `0(abc)+0`. The parentheses denote a subpattern that can be quantified as a unit. It’s also common for regular expression engines to save or “capture” the portion of the input text that matches a parenthesized group. Extracting bits this way is much more flexible and less error-prone than counting indices and `substr`.

## Alternation

Earlier, we saw one way to match either ‘Nick’ or ‘nick’. Another is with alternation as in `Nick|nick`. Remember that alternation includes everything to its left and everything to its right. Use grouping parentheses to limit the scope of `|`, *e.g.*, `(Nick|nick)`.

For another example, you could equivalently write `[a-c]` as `a|b|c`, but this is likely to be suboptimal because many implementations assume alternatives will have lengths greater than 1.

## Escaping

Although some characters match themselves, others have special meanings. The pattern `\d+` doesn’t match backslash followed by lowercase D followed by a plus sign: to get that, we’d use `\\d\+`. A backslash removes the special meaning from the following character.

## Greediness

Regular expression quantifiers are greedy. This means they match as much text as they possibly can while allowing the entire pattern to match successfully.

For example, say the input is

> "Hello," she said, "How are you?"

You might expect `".+"` to match only ‘Hello,’ and will then be surprised when you see that it matched from ‘Hello’ all the way through ‘you?’.

To switch from greedy to what you might think of as cautious, add an extra `?` to the quantifier.

This helps you understand how `\((.+?)\)` works. It matches the sequence of a literal left-parenthesis, followed by one or more characters, and terminated by a right-parenthesis. If your input is ‘(123) (456)’, then the first capture will be ‘123’. Non-greedy quantifiers want to allow the rest of the pattern to start matching as soon as possible.

## Anchors

Use the special pattern `^` to match only at the beginning of your input and `$` to match only at the end. Making “bookends” with your patterns where you say, “I know what’s at the front and back, but give me everything between” is a useful technique.

Say you want to match comments of the form

> `-- This is a comment --`

you’d write `^--\s+(.+)\s+--$`.

## Build your own

Regular expressions are recursive, so now that you understand these basic rules, you can combine them however you like.

## Tools for writing and debugging regexes:

 - [RegExr][1] (for JavaScript)
 - Perl: [YAPE: Regex Explain][2]
 - [Regex Coach][3] (engine backed by [CL-PPCRE](https://edicl.github.io/cl-ppcre/))
 - [RegexPal][4] (for JavaScript)
 - [Regular Expressions Online Tester][5] 
 - [Regex Buddy][6]
 - [Regex 101][7] (for PCRE, JavaScript, Python, Golang, Java 8)
 - [I Hate Regex][8]
 - [Visual RegExp][9]
 - [Expresso][10] (for .NET)
 - [Rubular][11] (for Ruby)
 - [Regular Expression Library][12] (Predefined Regexes for common scenarios)
 - [Txt2RE][13]
 - [Regex Tester][14] (for JavaScript)
 - [Regex Storm][15] (for .NET)
 - [Debuggex][16] (visual regex tester and helper)

## Books
 
 - [Mastering Regular Expressions][17]
 - [Regex Pocket Guide][rpg]
 - [Regular Expressions Cookbook][21]
 - [Learning Regular Expressions][22]

## Free resources

 - [Regular Expressions Cheat Sheet][20]
 - [RegexOne - Learn with simple, interactive exercises.][23]
 - [Regular Expressions - Everything you should know][24] (PDF Series)
 - [Regex Syntax Summary][25]
 - [How Regexes Work][26]
 - [JavaScript Regular Expressions][27]

## Footnote

**†:** The statement above that `.` matches any character is a simplification for pedagogical purposes that is not strictly true. Dot matches any character except newline, `"\n"`, but in practice you rarely expect a pattern such as `.+` to cross a newline boundary. Perl regexes have a [`/s` switch](https://perldoc.perl.org/perlre.html#s) and Java [`Pattern.DOTALL`](https://docs.oracle.com/javase/1.5.0/docs/api/java/util/regex/Pattern.html#DOTALL), for example, to make `.` match any character at all. For languages that don’t have such a feature, you can use something like `[\s\S]` to match “any whitespace or any non-whitespace,” in other words anything.


  [1]: https://regexr.com/
  [2]: https://metacpan.org/release/YAPE-Regex-Explain
  [3]: http://weitz.de/regex-coach/
  [4]: https://www.regexpal.com/
  [5]: https://www.regular-expressions.info/
  [6]: https://www.regexbuddy.com/
  [7]: https://regex101.com/
  [8]: https://ihateregex.io/
  [9]: http://laurent.riesterer.free.fr/regexp/
  [10]: http://www.ultrapico.com/Expresso.htm
  [11]: https://rubular.com/
  [12]: http://regexlib.com/Default.aspx
  [13]: http://www.txt2re.com/
  [14]: https://www.regextester.com/
  [15]: http://regexstorm.net/
  [16]: https://www.debuggex.com/
  [17]: https://www.amazon.com/Mastering-Regular-Expressions-Jeffrey-Friedl/dp/0596528124?&linkCode=ll2&tag=bloggbaconcom-20&linkId=6a2f90a7e9b938df133fe5dbdd5c7e97&language=en_US&ref_=as_li_ss_tl
  [20]: http://www.addedbytes.com/cheat-sheets/regular-expressions-cheat-sheet/
  [rpg]: https://www.amazon.com/Regex-Pocket-Guide-Expressions-Professionals-ebook/dp/B0FQ4DMHKP?&linkCode=ll2&tag=bloggbaconcom-20&linkId=093b24d7dd5a45efaca74064f81dd31c&language=en_US&ref_=as_li_ss_tl
  [21]: https://www.amazon.com/Regular-Expressions-Cookbook-Solutions-Programming-ebook/dp/B008Y4OP1O?&linkCode=ll2&tag=bloggbaconcom-20&linkId=70595e05fbe31b40d2216e61052a0612&language=en_US&ref_=as_li_ss_tl
  [22]: https://www.amazon.com/Learning-Regular-Expressions-Ben-Forta-ebook/dp/B07CGNFKQ4?&linkCode=ll2&tag=bloggbaconcom-20&linkId=55861d967f78f1ea56312de18c68aa5b&language=en_US&ref_=as_li_ss_tl
  [23]: https://regexone.com/
  [24]: http://neverfear.org/blog/view/Regex_tutorial_for_people_who_should_know_Regex__but_do_not___Part_1
  [25]: http://www.greenend.org.uk/rjk/2002/06/regexp.html
  [26]: http://perl.plover.com/Regex/
  [27]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions

### Source & Attribution

This post is based on [my Stack Overflow answer](https://stackoverflow.com/a/2759417) to the question “[Learning Regular Expressions](https://stackoverflow.com/questions/4736/learning-regular-expressions),” which I originally wrote on May&nbsp;3, 2010.

The answer was converted to a [community wiki](https://stackoverflow.com/posts/2759417/revisions) on November 11, 2010 and has since been improved and expanded by many members of the Stack Overflow community ([full revision history](https://stackoverflow.com/posts/2759417/revisions)).

The content is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0)](https://creativecommons.org/licenses/by-sa/4.0/) license (with earlier revisions originally under CC BY-SA 2.5 and 3.0).

### Modifications in this version

I have reformatted the content for readability on this blog, updated several tool and book recommendations, and lightly edited phrasing for a 2026 audience”.

If you found this helpful, please consider visiting the original answer on Stack Overflow to upvote it or contribute further improvements.
