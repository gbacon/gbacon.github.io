---
layout: post
title: "Net::OpenID::Consumer failure: naive verify failed network"
permalink: /2011/05/netopenidconsumer-fails-with-naiveverifyfaile.html
tags: [Perl]
comments: true
---
[Mojolicious]: http://mojolicious.org/
[DotCloud]: http://www.dotcloud.com/
[sample]: http://groups.google.com/group/mojolicious/msg/42c75d993f485753
[noidc]: http://search.cpan.org/~mart/Net-OpenID-Consumer-1.03/

I was working on adding OpenID to a [Mojolicious] webapp on
[DotCloud]—thanks to [helpful sample code][sample] from Henry that
[Net::OpenID::Consumer-1.03][noidc]—and hit a strange failure. The
call to `handle_server_response` was failing in the `error` callback
with a generic error code `naive_verify_failed_network`.

Going source diving, I learned slightly more about this particular
error: “Could not contact provider to verify signature.” Details, man;
I’m a fact finder! The good news is this particular error code is
returned in a single place, at line 849:

{% highlight perl %}
return $self->_fail("naive_verify_failed_network")
    unless $res && $res->is_success;
{% endhighlight %}

[HTTP::Response]: http://search.cpan.org/perldoc?HTTP::Response
[LWPx::ParanoidAgent]: http://search.cpan.org/perldoc?LWPx::ParanoidAgent

Here `$res` is an [HTTP::Response] instance returned from the
`request` method on [LWPx::ParanoidAgent]. HTTP::Response has another
helpful method:

> ### `$r->error_as_HTML`
>
> Returns a string containing a complete HTML document indicating
> what error occurred. This method should only be called when
> `$r->is_error` is TRUE.

[Mojolicious::Lite]: http://search.cpan.org/perldoc?Mojolicious::Lite

So I first do a little plumbing for my [Mojolicious::Lite] prototype.

{% highlight perl %}
my $log = app->log;
$SIG{"__WARN__"} = sub {
  unshift @_, $log;
  goto $log->can("warn");
};
{% endhighlight %}

[debug]: https://github.com/kraih/mojo/wiki/Debugging-for-non-lite-apps#STDERR_Redirection_to_an_Application&#39;s_Log

Grungy, yes, but it’s a quick adaptation of a nicer technique for
[debugging non-lite apps][debug].

Just before `return $self->_fail ...`, I added

{% highlight perl %}
if ($res && $res->is_error) {
    warn $res->error_as_HTML;
}
{% endhighlight %}

After retrying the OpenID verification, my log contained

<pre>501 Attempt to reload LWPx/Protocol/https_paranoid.pm aborted</pre>

but running `grep -rl Attempt\ to\ reload ~/perl5/lib` produced no hits.

Same with Google.

[perldiag entry]: http://search.cpan.org/~jesse/perl-5.14.0/pod/perldiag.pod#Attempt_to_reload_%s_aborted.

Chopping the specific bits out of my query, leaving “Attempt to reload”
aborted, the first hit was a [perldiag entry].

> ### Attempt to reload %s aborted.
>
> (F) You tried to load a file with use or require that failed to
> compile once already. Perl will not try to compile this file again
> unless you delete its entry from `%INC`. See `require` in perlfunc
> and `%INC` in perlvar.

Okay, so let’s see why the module is unhappy:

<pre>$ perl -Iperl5 -MLWPx::Protocol::https_paranoid -de 0

Loading DB routines from perl5db.pl version 1.33
Editor support available.

Enter h or `h h' for help, or `man perldebug' for more help.

Can't locate IO/Socket/SSL.pm in @INC at ...</pre>

[cookie]: http://toroid.org/ams/etc/mojolicious-session-cookies

Hidden dependency! [IO::Socket::SSL is good to have with
Mojolicious][cookie], so I edited my `Makefile.PL` to include

{% highlight perl %}
requires "IO::Socket::SSL" => '1.43';
{% endhighlight %}

[Cache::File]: http://search.cpan.org/perldoc?Cache::File
[Redis]: http://search.cpan.org/perldoc?Redis%C2%A0

A sequence of `perl Makefile.PL; make; dotcloud push ...` gave me
successful OpenID verification! As an extra goodie thanks to DotCloud,
instead of a [Cache::File] instead, my code uses [Redis] in the `cache`
parameter to Net::OpenID::Consumer’s constructor.
