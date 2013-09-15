---
layout: post
title: Extracting comma-separated integers with Perl
permalink: /2011/03/extracting-comma-separated-integers.html
tags: [Perl]
comments: true
---
A friend writes asking which of

    my @data = ( $data =~ m|(-?\d+),(-?\d+),(\-?\d+)\r| );
 
or

    ($data) = (split "\r", $data);
    my @data = split ',', $data;

would be better for extracting integers from a line of input, and my
reply is below.

The best approach depends on a few factors. Who generates the data
you’re processing? How much slop in the input format do you need to
accommodate? How flexible do you need to be with respect to future
changes in the input, *e.g.*, extra fields, different types, and
so on?

The CR (`\r`) in your input is a red flag. Are you running on a Unix
platform with input generated on Windows? Can you be more specific
about “possibly some other stuff” after the comma-separated numbers?

[1]: http://perldoc.perl.org/perlvar.html#%24/
[2]: http://perldoc.perl.org/perlport.html#Newlines

Perl’s [`$/` special variable][1] can handle oddball line endings. Its
default value varies with what makes sense for the current platform:
*e.g.*, `\n` on Unix and `\r\n` on Windows. Macs introduce another
twist (see [Newlines in the perlport documentation][2]), but I assume
you’re not using that platform.

On Windows for files opened in text mode (the default), the C library
silently transforms CR followed by LF into LF, so if this matches your
setup, I’m surprised you’re seeing the `\r` at all.

Say your input is plain text generated on Windows, and you’re running
on Linux. Then you’d process it with code of the form

{% highlight perl %}
$/ = "\r\n";
 
while (defined($data = <>)) {
    chomp;
    ...;
}
{% endhighlight %}

[chomp]: http://perldoc.perl.org/functions/chomp.html

Remember that [`chomp`][chomp] removes the value of `$/` from the end
of the target.

[Randal Schwartz]: http://www.stonehenge.com/merlyn/
[Learning Perl]: http://oreilly.com/catalog/9780596520113

As for extracting the data, [Randal Schwartz][] (author of
*[Learning Perl]*, a.k.a. the llama book) has a rule of thumb:

> Use capturing or `m//g` when you know what you want to **keep**.
>
> Use `split` when you know what you want to **throw away**.

[Regular Expression Mastery]: http://perl.plover.com/yak/regex/samples/slide070.html

I first saw this useful guideline in [Regular Expression Mastery] by
Mark Dominus.

If this is a quick-and-dirty utility, I’d be inclined to write

{% highlight perl %}
@data = split /\s*,\s*/, $data;
{% endhighlight %}

This allows for and removes any whitespace around the commas.

If it’s important to nail down the format (maybe as a sanity check that
you’re in the section of the config file where you think you are), you
could write

{% highlight perl %}
if ($data =~ /^\s*(-?\d+)\s*,\s*(-?\d+)\s*,\s*(-?\d+)\s*$/) {
    my($x,$y,$z) = ($1,$2,$3);
    ...;
}
else {
    die "$0: $ARGV:$.: unexpected format";
}
{% endhighlight %}

Note the use of `$1` and friends inside the conditional only. Always,
*always*, **always** protect uses of capture variables with conditionals.

[named capture buffers]: http://perldoc.perl.org/perlre.html#Capture-buffers

The pattern is just at the annoyance threshold of repetition and
illegibility. Perl version 5.10 opens up nice possibilities with [named
capture buffers]:

{% highlight perl %}
#! /usr/bin/env perl
 
use strict;
use warnings;
 
use 5.10.0;
 
my $record = qr/
    ^
    (?&ws)
    (?<num>(?&n)) (?&sep) (?<num>(?&n)) (?&sep) (?<num>(?&n))
    (?&ws)
    $
 
    (?(DEFINE)
        (?<n> -? \d+)
        (?<ws> \s* )
        (?<sep> (?&ws) , (?&ws))
    )
/x;
 
while (<DATA>) {
    if (/$record/) {
        my($x,$y,$z) = @{ $-{num} };
        print "got: x=$x, y=$y, z=$z\n";
    }
    else {
        warn "$0: line $.: no match\n";
    }
}
 
__DATA__
1,2,3
4,5,6
7,8
{% endhighlight %}

Its output:

<pre>got: x=1, y=2, z=3
got: x=4, y=5, z=6
./prog: line 3: no match</pre>

[hashdash]: http://perldoc.perl.org/perlvar.html#%25-
[DEFINE]: http://perldoc.perl.org/perlre.html#%28DEFINE%29

Notice its use of [the special `%-` hash][hashdash] that records all
captures named “num” in this case. With [`(DEFINE)`][DEFINE],
subpatterns get meaningful names, and the `/x` modifier allow for
judicious use of whitespace inside the pattern.
