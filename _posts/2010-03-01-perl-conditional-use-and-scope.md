---
layout: post
title: "Perl: conditional use and scope"
permalink: /2010/03/perl-conditional-use-and-scope.html
tags: Perl
comments: true
---
A reader asks

> If I conditionally load a perl module, do those module variables get
> passed to the whole perl script?
>
>     if ( some_test ) {
>       use "perlmodule_001";
>     }
>     else {
>       use "perlmodule_002";
>     }
>
> Are the elements of either perl module available outside the `if`
> statement?

The main program from the question has a syntax error:

<pre>syntax error at prog0 line 2, near "use "perlmodule_001""</pre>

[use]: http://perldoc.perl.org/functions/use.html

Perl’s [documentation for `use`][use] explains:

> **use Module**  
> Imports some semantics into the current package from the named module,
> generally by aliasing certain subroutine or variable names into your
> package. It is exactly equivalent to
>
>     BEGIN { require Module; Module->import( LIST ); }
>
> except that Module *must* be a bareword.

[require]: http://perldoc.perl.org/functions/require.html

Note the bareword constraint at the end: the compiler doesn’t like the
double quotes around the argument to `use`. Our friend was likely
thinking of the older [`require`][require] operator that *does* accept
strings and arbitrary expressions in general.

Say we have two modules with alternative definitions of `$Foo` and `$Bar`:

{% highlight perl %}
package Perlmodule_001;

use Exporter 'import';
our @EXPORT = qw/ $Foo $Bar /;
our $Foo = 'apple';
our $Bar = 'orange';

1;
{% endhighlight %}

and

{% highlight perl %}
package Perlmodule_002;

use Exporter 'import';
our @EXPORT = qw/ $Foo $Bar /;
our $Foo = 42;
our $Bar = 'w00t!';

1;
{% endhighlight %}

[perlmodlib]: http://perldoc.perl.org/perlmodlib.html
[integer]: http://perldoc.perl.org/integer.html
[strict]: http://perldoc.perl.org/strict.html

Note the use of `Perlmodule_001`, for example, rather than `perlmodule_001`: the
[perlmodlib documentation][perlmodlib] notes, “Perl informally reserves
lowercase module names for pragma modules like [`integer`][integer] and
[`strict`][strict].”

Consider the following simple driver:

{% highlight perl %}
#! /usr/bin/perl

use warnings;
use strict;

if (@ARGV && $ARGV[0] eq "two") {
    use Perlmodule_002;
}
else {
    use Perlmodule_001;
}

sub maybeUndef {
    defined $_[0] ? $_[0] : "<undefined>";
    # got 5.10?
    # $_[0] // "<undefined>";
}

print "Foo = ", maybeUndef($Foo), "\n",
      "Bar = ", maybeUndef($Bar), "\n";
{% endhighlight %}

It uses `maybeUndef` to explicitly show when a value is undefined and
also to silence potential undefined-value warnings.

The program seems to run as intended

<pre>$ ./prog1
Foo = apple
Bar = orange</pre>

but the output is the same even when an argument of `two` is supplied on
the command line!

<pre>$ ./prog1 two
Foo = apple
Bar = orange</pre>

The good news is that the imported variables are in scope for the rest
of the program, as indicated in the above documentation for `use`
(with emphasis added):

> Imports some semantics *into the current package* from the named
> module &hellip;

[BEGIN]: http://perldoc.perl.org/perlmod.html#BEGIN%2C-UNITCHECK%2C-CHECK%2C-INIT-and-END

To understand why we never see `Perlmodule_002`’s `$Foo` and `$Bar`,
note that `use` “is exactly equivalent to” `require` at
[`BEGIN`][BEGIN] time, and the perlmod documentation explains exactly
when that is (with added emphasis):

> A `BEGIN` code block is executed as soon as possible, that is, the
> moment it is completely defined, *even before the rest of the
> containing file (or string) is parsed*.

So the compiler sees `use Perlmodule_002` and processes it. Then it sees
`use Perlmodule_001` and processes it. When the compiler finishes
digesting the rest of the code, it’s time for the execution phase, when
the `@ARGV` check finally takes place. As written, `Perlmodule_001` will
always win!

Because ordinary modules affect the current package, `use`–ing an
ordinary module inside a conditional block is entirely misleading. I was
careful to qualify the previous statement for ordinary modules because
the effects of some pragmatic modules (*e.g.*, `strict` and
`integer`—note the lowercase names!) are limited tightly to the
enclosing block only.

The fix is to process `@ARGV` at `BEGIN` time and conditionalize the
module imports with the equivalent `require` and `import`:

{% highlight perl %}
#! /usr/bin/perl

use warnings;
use strict;

BEGIN {
    if (@ARGV && $ARGV[0] eq "two") {
        require Perlmodule_002;
        Perlmodule_002->import;
    }
    else {
        require Perlmodule_001;
        Perlmodule_001->import;
    }
}

sub maybeUndef {
    defined $_[0] ? $_[0] : "<undefined>";
    # got 5.10?
    # $_[0] // "<undefined>";
}

print "Foo  = ", maybeUndef($Foo), "\n",
      "Bar  = ", maybeUndef($Bar), "\n";
{% endhighlight %}

[eval]: http://perldoc.perl.org/functions/eval.html

An alternative is protecting `use` with [`eval`][eval] as in

{% highlight perl %}
BEGIN {
    if (@ARGV && $ARGV[0] eq "two") {
        eval "use Perlmodule_002";
    }
    # ...
{% endhighlight %}

so a particular `use` runs only when control reaches its `eval` but is
ignored otherwise. This is a safe, sensible use of `eval`.

Either way, the program now does what we expect!

<pre>$ ./prog2
Foo  = apple
Bar  = orange
$ ./prog2 two
Foo  = 42
Bar  = w00t!</pre>

You might wonder why the code has to be inside a `BEGIN` block after the
`use` lines are conditionalized. If you have the `strict` pragma
enabled—and you should!—it wants variables to be imported and declared
before execution begins. Otherwise, compilation will fail because for
all it knows, `$Foo` and `$Bar` in the `main` package were typos.
