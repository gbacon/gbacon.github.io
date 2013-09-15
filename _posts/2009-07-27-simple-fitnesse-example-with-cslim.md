---
layout: post
title: Simple FitNesse example with CSlim
permalink: /2009/07/simple-fitnesse-example-with-cslim.html
tags: [FitNesse,TDD]
comments: true
---
[FitNesse]: http://fitnesse.org/
[CSlim repo]: http://github.com/dougbradbury/cslim/tree/master
[README]: http://github.com/dougbradbury/cslim/blob/d97a33f6cd881fdf283061f89c2ea9ddd714ac93/README

Tinkering with the [FitNesse] acceptance testing system, I cloned the
[CSlim repo], but its [README] was short on detail about getting the thing
to talk to FitNesse.

Attempting to build (on Ubuntu karmic) out of the box failed:

{% highlight bash %}
$ make
compiling ListExecutor.c
compiling SlimConnectionHandler.c
compiling SlimList.c
compiling SlimListDeserializer.c
compiling SlimListSerializer.c
compiling SlimUtil.c
compiling StatementExecutor.c
compiling SymbolTable.c
compiling SocketServer.c
compiling TcpComLink.c
Building archive lib/libCSlim.a
ar: lib/libCSlim.a: No such file or directory
make: *** [lib/libCSlim.a] Error 1
{% endhighlight %}

The workaround is straightforward: `mkdir lib` followed by `make`.

So now what?

{% highlight bash %}
$ ./CSlim_server --help
getaddrinfo: Servname not supported for ai_socktype
{% endhighlight %}

[SLiM docs]: http://fitnesse.org/FitNesse.UserGuide.SliM

Following the recommendation in the [SLiM docs], I created a page
called CslimTest (by editing FrontPage to contain a new line with
CslimTest) that contained merely

    !define TEST_SYSTEM {slim}

After I clicked the Test button, the page cleared, paused for a few
seconds, and then gave a red box with “Testing was interupted and
results are incomplete.” Clicking Output Captured, I saw a stacktrace
that ended with

<pre>java.lang.ClassNotFoundException: fitnesse.slim.SlimService</pre>

[FitNesse download page]: http://fitnesse.org/FrontPage.FitNesseDevelopment.DownLoad
[Getting Started page for fitSharp]: http://www.syterra.com/Slim/GettingStarted.html

Maybe I could crib from the docs for another server. The
[FitNesse download page] points to servers for various languages, and
from there I arrived at a [Getting Started page for fitSharp], a C#
server for FitNesse, which had the following config:

    !define TEST_SYSTEM {slim}
    !path c:\myfolder\mytest.dll
    !define COMMAND_PATTERN {{ "{%m" }} -r fitSharp.Slim.Service.Runner,c:\program files\fitsharp\fitsharp.dll %p}
    !define TEST_RUNNER {c:\program files\fitsharp\Runner.exe}

So next I try

    !define TEST_SYSTEM {slim}
    !define TEST_RUNNER {/home/gbacon/src/cslim/CSlim_server}

I got a similar failure, but this time the class that failed to load was
`.home.gbacon.src.cslim.CSlim_server`.

Even though the fitSharp used `!path` for test assemblies, Drew suggested
pointing it to `CSlim_server` as in

    !define TEST_SYSTEM {slim}
    !path /home/gbacon/src/cslim/CSlim_server

But the test still died with `ClassNotFoundException`.

[CP]: http://fitnesse.org/FitNesse.UserGuide.CustomizingTestExecution

I monkeyed more with [`COMMAND_PATTERN`][CP], and finally got a quick error using

    !define TEST_SYSTEM {slim}
    !path /home/gbacon/src/cslim/CSlim_server
    !define COMMAND_PATTERN {{ "{%m " }}%p}

[2min]: http://fitnesse.org/FitNesse.UserGuide.TwoMinuteExample

Instead of Output Captured, I see Tests Executed OK. I notice in
`src/Main/DecisionTableExample.c` there’s a `Division` fixture that
seems to match the example in the [two–minute example][2min], so I
copy-and-paste to get

    !define TEST_SYSTEM {slim}
    !path /home/gbacon/src/cslim/CSlim_server
    !define COMMAND_PATTERN {{ "{%m " }}%p}

    |eg.Division|
    |setNumerator|setDenominator|Quotient?|
    |10          |2             |5        |
    |12.6        |3             |4.2      |
    |100         |4             |33       |

Red box still, and the tests aren’t running. Not much help in the output
log, but FitNesse complained:

<pre>Cannot run program "fitnesse.slim.SlimService".</pre>

Remove `%m` from `COMMAND_PATTERN`. Progress! Nothing’s running, but I
see a bunch of exceptions and text with yellow backgrounds, such as
“eg.Division Could not find class eg.Division.” Maybe it doesn’t like
the leading `eg`.

    !define TEST_SYSTEM {slim}
    !path /home/gbacon/src/cslim/CSlim_server
    !define COMMAND_PATTERN {{ "{%p" }}}

    |Division|
    |setNumerator|setDenominator|Quotient?|
    |10          |2             |5        |
    |12.6        |3             |4.2      |
    |100         |4             |33       |

Now Division goes green, but it doesn’t like the input methods
(<tt>"Method setSetNumerator\[1] not found in Division."</tt> for
example). The method names in the fixture are `setNumerator`,
`setDenominator`, and `Quotient`, so mangling must be happening
somewhere.

That’s what I get for trying to think ahead of it:

    !define TEST_SYSTEM {slim}
    !define COMMAND_PATTERN {{ "{%p" }}}
    !path /home/gbacon/src/cslim/CSlim_server

    |Division|
    |numerator|denominator|Quotient?|
    |10       |2          |5        |
    |12.6     |3          |4.2      |
    |100      |4          |33       |

Now the first two rows pass, and the third fails as expected.
