---
title: "Moral: a regular expression is a poor substitute for a parser"
description: "Now you have two problems. Why a regular expression is a poor substitute for a parser, shown by a unit-tested C# email validator built on a monster regex."
author: "Greg Bacon"
date: "2006-10-05"
tags:
  - "Regex"
  - "C#"
  - "Testing"
---
A famous [jwz](http://www.jwz.org/) soundbite is [“Some people, when confronted with a problem, think ‘I know, I’ll use regular expressions.’ Now they have two problems.”](https://web.archive.org/web/20060905102808/http://www.jwz.org/hacks/marginal.html)

Snarky, yes, but a good sanity check.

Section 9 of the Perl FAQ addresses the issue of [validating email addresses](https://perldoc.perl.org/perlfaq9#How-do-I-check-a-valid-mail-address). [RFC 2822](https://www.rfc-editor.org/rfc/rfc2822) gives a specification for Internet email addresses, and the language is much broader than that matched by `/^\w+@\w+\.\w+$/`

The proper solution is to use a parser. To see why, consider the task of maintaining the following [elegant](http://careycilyok.blogspot.com/2007/02/thats-elegant.html) — but [unit-tested](http://www.nunit.org/)! — solution:

```csharp
using System;
using System.Text.RegularExpressions;

using NUnit.Framework;

namespace Application
{
  public interface SyntaxChecker
  {
    bool WellFormed(string input);
  }

  public class EmailAddressChecker : SyntaxChecker
  {
    // derivative of work with the following copyright and license:
    // Copyright (c) 2004 Casey West.  All rights reserved.
    // This module is free software; you can redistribute it and/or
    // modify it under the same terms as Perl itself.

    // see http://search.cpan.org/~cwest/Email-Address-1.80/

    #region dumped regular expression
    private static string gibberish = @"
    (?-xism:(?:(?-xism:(?-xism:(?-xism:(?-xism:(?-xism:(?-xism:\
    s*\((?:\s*(?-xism:(?-xism:(?>[^()\\]+))|(?-xism:\\(?-xism:[^
    \x0A\x0D]))|(?-xism:\s*\((?:\s*(?-xism:(?-xism:(?>[^()\\]+))
    |(?-xism:\\(?-xism:[^\x0A\x0D]))|)+)*\s*\)\s*))+)*\s*\)\s*)+
    |\s+)*[^\x00-\x1F\x7F()<>\[\]:;@\,.<DQ>\s]+(?-xism:(?-xism:\
    s*\((?:\s*(?-xism:(?-xism:(?>[^()\\]+))|(?-xism:\\(?-xism:[^
    \x0A\x0D]))|(?-xism:\s*\((?:\s*(?-xism:(?-xism:(?>[^()\\]+))
    |(?-xism:\\(?-xism:[^\x0A\x0D]))|)+)*\s*\)\s*))+)*\s*\)\s*)+
    |\s+)*)|(?-xism:(?-xism:(?-xism:\s*\((?:\s*(?-xism:(?-xism:(
    ?>[^()\\]+))|(?-xism:\\(?-xism:[^\x0A\x0D]))|(?-xism:\s*\((?
    :\s*(?-xism:(?-xism:(?>[^()\\]+))|(?-xism:\\(?-xism:[^\x0A\x
    0D]))|)+)*\s*\)\s*))+)*\s*\)\s*)+|\s+)*<DQ>(?-xism:(?-xism:[
    ^\\<DQ>])|(?-xism:\\(?-xism:[^\x0A\x0D])))+<DQ>(?-xism:(?-xi
    sm:\s*\((?:\s*(?-xism:(?-xism:(?>[^()\\]+))|(?-xism:\\(?-xis
    m:[^\x0A\x0D]))|(?-xism:\s*\((?:\s*(?-xism:(?-xism:(?>[^()\\
    ]+))|(?-xism:\\(?-xism:[^\x0A\x0D]))|)+)*\s*\)\s*))+)*\s*\)\
    s*)+|\s+)*))+)?(?-xism:(?-xism:(?-xism:\s*\((?:\s*(?-xism:(?
    -xism:(?>[^()\\]+))|(?-xism:\\(?-xism:[^\x0A\x0D]))|(?-xism:
    \s*\((?:\s*(?-xism:(?-xism:(?>[^()\\]+))|(?-xism:\\(?-xism:[
    ^\x0A\x0D]))|)+)*\s*\)\s*))+)*\s*\)\s*)+|\s+)*<(?-xism:(?-xi
    sm:(?-xism:(?-xism:(?-xism:\s*\((?:\s*(?-xism:(?-xism:(?>[^(
    )\\]+))|(?-xism:\\(?-xism:[^\x0A\x0D]))|(?-xism:\s*\((?:\s*(
    ?-xism:(?-xism:(?>[^()\\]+))|(?-xism:\\(?-xism:[^\x0A\x0D]))
    |)+)*\s*\)\s*))+)*\s*\)\s*)+|\s+)*(?-xism:[^\x00-\x1F\x7F()<
    >\[\]:;@\,.<DQ>\s]+(?:\.[^\x00-\x1F\x7F()<>\[\]:;@\,.<DQ>\s]
    +)*)(?-xism:(?-xism:\s*\((?:\s*(?-xism:(?-xism:(?>[^()\\]+))
    |(?-xism:\\(?-xism:[^\x0A\x0D]))|(?-xism:\s*\((?:\s*(?-xism:
    (?-xism:(?>[^()\\]+))|(?-xism:\\(?-xism:[^\x0A\x0D]))|)+)*\s
    *\)\s*))+)*\s*\)\s*)+|\s+)*)|(?-xism:(?-xism:(?-xism:\s*\((?
    :\s*(?-xism:(?-xism:(?>[^()\\]+))|(?-xism:\\(?-xism:[^\x0A\x
    0D]))|(?-xism:\s*\((?:\s*(?-xism:(?-xism:(?>[^()\\]+))|(?-xi
    sm:\\(?-xism:[^\x0A\x0D]))|)+)*\s*\)\s*))+)*\s*\)\s*)+|\s+)*
    <DQ>(?-xism:(?-xism:[^\\<DQ>])|(?-xism:\\(?-xism:[^\x0A\x0D]
    )))+<DQ>(?-xism:(?-xism:\s*\((?:\s*(?-xism:(?-xism:(?>[^()\\
    ]+))|(?-xism:\\(?-xism:[^\x0A\x0D]))|(?-xism:\s*\((?:\s*(?-x
    ism:(?-xism:(?>[^()\\]+))|(?-xism:\\(?-xism:[^\x0A\x0D]))|)+
    )*\s*\)\s*))+)*\s*\)\s*)+|\s+)*))\@(?-xism:(?-xism:(?-xism:(
    ?-xism:\s*\((?:\s*(?-xism:(?-xism:(?>[^()\\]+))|(?-xism:\\(?
    -xism:[^\x0A\x0D]))|(?-xism:\s*\((?:\s*(?-xism:(?-xism:(?>[^
    ()\\]+))|(?-xism:\\(?-xism:[^\x0A\x0D]))|)+)*\s*\)\s*))+)*\s
    *\)\s*)+|\s+)*(?-xism:[^\x00-\x1F\x7F()<>\[\]:;@\,.<DQ>\s]+(
    ?:\.[^\x00-\x1F\x7F()<>\[\]:;@\,.<DQ>\s]+)*)(?-xism:(?-xism:
    \s*\((?:\s*(?-xism:(?-xism:(?>[^()\\]+))|(?-xism:\\(?-xism:[
    ^\x0A\x0D]))|(?-xism:\s*\((?:\s*(?-xism:(?-xism:(?>[^()\\]+)
    )|(?-xism:\\(?-xism:[^\x0A\x0D]))|)+)*\s*\)\s*))+)*\s*\)\s*)
    +|\s+)*)|(?-xism:(?-xism:(?-xism:\s*\((?:\s*(?-xism:(?-xism:
    (?>[^()\\]+))|(?-xism:\\(?-xism:[^\x0A\x0D]))|(?-xism:\s*\((
    ?:\s*(?-xism:(?-xism:(?>[^()\\]+))|(?-xism:\\(?-xism:[^\x0A\
    x0D]))|)+)*\s*\)\s*))+)*\s*\)\s*)+|\s+)*\[(?:\s*(?-xism:(?-x
    ism:[^\[\]\\])|(?-xism:\\(?-xism:[^\x0A\x0D])))+)*\s*\](?-xi
    sm:(?-xism:\s*\((?:\s*(?-xism:(?-xism:(?>[^()\\]+))|(?-xism:
    \\(?-xism:[^\x0A\x0D]))|(?-xism:\s*\((?:\s*(?-xism:(?-xism:(
    ?>[^()\\]+))|(?-xism:\\(?-xism:[^\x0A\x0D]))|)+)*\s*\)\s*))+
    )*\s*\)\s*)+|\s+)*)))>(?-xism:(?-xism:\s*\((?:\s*(?-xism:(?-
    xism:(?>[^()\\]+))|(?-xism:\\(?-xism:[^\x0A\x0D]))|(?-xism:\
    s*\((?:\s*(?-xism:(?-xism:(?>[^()\\]+))|(?-xism:\\(?-xism:[^
    \x0A\x0D]))|)+)*\s*\)\s*))+)*\s*\)\s*)+|\s+)*))|(?-xism:(?-x
    ism:(?-xism:(?-xism:(?-xism:\s*\((?:\s*(?-xism:(?-xism:(?>[^
    ()\\]+))|(?-xism:\\(?-xism:[^\x0A\x0D]))|(?-xism:\s*\((?:\s*
    (?-xism:(?-xism:(?>[^()\\]+))|(?-xism:\\(?-xism:[^\x0A\x0D])
    )|)+)*\s*\)\s*))+)*\s*\)\s*)+|\s+)*(?-xism:[^\x00-\x1F\x7F()
    <>\[\]:;@\,.<DQ>\s]+(?:\.[^\x00-\x1F\x7F()<>\[\]:;@\,.<DQ>\s
    ]+)*)(?-xism:(?-xism:\s*\((?:\s*(?-xism:(?-xism:(?>[^()\\]+)
    )|(?-xism:\\(?-xism:[^\x0A\x0D]))|(?-xism:\s*\((?:\s*(?-xism
    :(?-xism:(?>[^()\\]+))|(?-xism:\\(?-xism:[^\x0A\x0D]))|)+)*\
    s*\)\s*))+)*\s*\)\s*)+|\s+)*)|(?-xism:(?-xism:(?-xism:\s*\((
    ?:\s*(?-xism:(?-xism:(?>[^()\\]+))|(?-xism:\\(?-xism:[^\x0A\
    x0D]))|(?-xism:\s*\((?:\s*(?-xism:(?-xism:(?>[^()\\]+))|(?-x
    ism:\\(?-xism:[^\x0A\x0D]))|)+)*\s*\)\s*))+)*\s*\)\s*)+|\s+)
    *<DQ>(?-xism:(?-xism:[^\\<DQ>])|(?-xism:\\(?-xism:[^\x0A\x0D
    ])))+<DQ>(?-xism:(?-xism:\s*\((?:\s*(?-xism:(?-xism:(?>[^()\
    \]+))|(?-xism:\\(?-xism:[^\x0A\x0D]))|(?-xism:\s*\((?:\s*(?-
    xism:(?-xism:(?>[^()\\]+))|(?-xism:\\(?-xism:[^\x0A\x0D]))|)
    +)*\s*\)\s*))+)*\s*\)\s*)+|\s+)*))\@(?-xism:(?-xism:(?-xism:
    (?-xism:\s*\((?:\s*(?-xism:(?-xism:(?>[^()\\]+))|(?-xism:\\(
    ?-xism:[^\x0A\x0D]))|(?-xism:\s*\((?:\s*(?-xism:(?-xism:(?>[
    ^()\\]+))|(?-xism:\\(?-xism:[^\x0A\x0D]))|)+)*\s*\)\s*))+)*\
    s*\)\s*)+|\s+)*(?-xism:[^\x00-\x1F\x7F()<>\[\]:;@\,.<DQ>\s]+
    (?:\.[^\x00-\x1F\x7F()<>\[\]:;@\,.<DQ>\s]+)*)(?-xism:(?-xism
    :\s*\((?:\s*(?-xism:(?-xism:(?>[^()\\]+))|(?-xism:\\(?-xism:
    [^\x0A\x0D]))|(?-xism:\s*\((?:\s*(?-xism:(?-xism:(?>[^()\\]+
    ))|(?-xism:\\(?-xism:[^\x0A\x0D]))|)+)*\s*\)\s*))+)*\s*\)\s*
    )+|\s+)*)|(?-xism:(?-xism:(?-xism:\s*\((?:\s*(?-xism:(?-xism
    :(?>[^()\\]+))|(?-xism:\\(?-xism:[^\x0A\x0D]))|(?-xism:\s*\(
    (?:\s*(?-xism:(?-xism:(?>[^()\\]+))|(?-xism:\\(?-xism:[^\x0A
    \x0D]))|)+)*\s*\)\s*))+)*\s*\)\s*)+|\s+)*\[(?:\s*(?-xism:(?-
    xism:[^\[\]\\])|(?-xism:\\(?-xism:[^\x0A\x0D])))+)*\s*\](?-x
    ism:(?-xism:\s*\((?:\s*(?-xism:(?-xism:(?>[^()\\]+))|(?-xism
    :\\(?-xism:[^\x0A\x0D]))|(?-xism:\s*\((?:\s*(?-xism:(?-xism:
    (?>[^()\\]+))|(?-xism:\\(?-xism:[^\x0A\x0D]))|)+)*\s*\)\s*))
    +)*\s*\)\s*)+|\s+)*))))(?-xism:\s*\((?:\s*(?-xism:(?-xism:(?
    >[^()\\]+))|(?-xism:\\(?-xism:[^\x0A\x0D]))|(?-xism:\s*\((?:
    \s*(?-xism:(?-xism:(?>[^()\\]+))|(?-xism:\\(?-xism:[^\x0A\x0
    D]))|)+)*\s*\)\s*))+)*\s*\)\s*)*)"
      .Replace("<DQ>", "\"")
      .Replace("\t", "")
      .Replace(" ", "")
      .Replace("\r", "")
      .Replace("\n", "");
    #endregion

    private static Regex mailbox =
      new Regex(gibberish, RegexOptions.ExplicitCapture);

    public bool WellFormed(string address)
    {
      return mailbox.IsMatch(address);
    }
  }

  [TestFixture]
  public class Test
  {
    private SyntaxChecker checker = null;

    [SetUp]
    public void SetUp()
    {
      checker = new EmailAddressChecker();
    }

    [Test]
    public void SimpleAddress()
    {
      Assert.IsTrue(checker.WellFormed("foo@example.com"));
    }

    [Test]
    public void MustHaveLocalAndDomain()
    {
      Assert.IsFalse(checker.WellFormed("justlocal"));
    }

    [Test]
    public void AmpersandInLocalPart()
    {
      Assert.IsTrue(checker.WellFormed("\"foo & bar\"@example.com"));
    }

    [Test]
    public void PhraseAndAngles()
    {
      Assert.IsTrue(checker.WellFormed("Joe <joe@example.com>"));
    }

    [Test]
    public void AddressAndComment()
    {
      Assert.IsTrue(checker.WellFormed("joe@example.com (Joe)"));
    }

    [Test]
    public void ContainsSingleQuoteInParenComment()
    {
      Assert.IsTrue(checker.WellFormed("oleary@example.com (Mr. O'Leary)"));
    }

    [Test]
    public void ContainsSingleQuoteInBareComment()
    {
      Assert.IsTrue(checker.WellFormed("Mr. O'Leary <oleary@example.com"));
    }

    [Test]
    public void ContainsSingleQuoteInLocalPart()
    {
      Assert.IsTrue(checker.WellFormed("o'leary@example.com"));
    }
  }
}
```

## Further reading

* [*Mastering Regular Expressions*][friedl], 3rd edition, by Jeffrey E. F. Friedl: how regex engines work, including .NET, and how to write patterns that match what you intend.
* [*Language Implementation Patterns*][parr] by Terence Parr: practical patterns for lexers, recursive-descent parsers, and tree walkers, from the creator of ANTLR.
* [*Introduction to the Theory of Computation*][sipser], 3rd edition, by Michael Sipser: regular languages, context-free grammars, and the pumping lemmas that show why a regular expression cannot match arbitrarily nested structure.
* [*An Introduction to Formal Languages and Automata*][linz], 7th edition, by Peter Linz and Susan H. Rodger: finite automata, regular and context-free languages, and new chapters on parsing.
* [*Test-Driven Development: By Example*][beck] by Kent Beck: the xUnit style of testing that NUnit follows.

[friedl]: https://www.amazon.com/dp/0596528124
[parr]: https://www.amazon.com/dp/193435645X
[sipser]: https://www.amazon.com/dp/113318779X
[linz]: https://www.amazon.com/dp/1284231607
[beck]: https://www.amazon.com/dp/0321146530

Related: [Learning Regular Expressions](/posts/learning-regex/)
