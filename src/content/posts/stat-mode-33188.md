---
title: "Why does stat give 33188 for a file mode?"
description: "How Unix represents file permissions."
author: "Greg Bacon"
date: "2026-05-15"
tags:
  - "Unix"
  - "Linux"
  - "Perl"
---
Someone asked what a Unix file mode of 33188 could possibly mean.

The mode from your question corresponds to a regular file with 644 permissions (read-write for the owner and read-only for everyone else), but don’t take my word for it.

```bash
$ touch foo
$ chmod 644 foo
$ perl -le 'print +(stat "foo")[2]'
33188
```

The value of `$mode` *can* be viewed as a decimal integer, but doing so is not particularly enlightening. Seeing the octal representation gives something a bit more familiar.

```bash
$ perl -e 'printf "%o\n", (stat "foo")[2]'
100644
```

Bitwise AND with `07777` gives the last twelve bits of a number’s binary representation. With a Unix mode, this operation gives the permission or mode bits and discards any type information.

```bash
$ perl -e 'printf "%d\n", (stat "foo")[2] & 07777'  # decimal, not useful
420
$ perl -e 'printf "%o\n", (stat "foo")[2] & 07777'  # octal, eureka!
644
```

A nicer way to do this is below. Read on for all the details.

----

## Mode Bits

The third element returned from `stat` (which corresponds to `st_mode` in `struct stat`) is a [bit field](https://en.wikipedia.org/wiki/Bit_field) where the different bit positions are binary flags.

For example, one bit in `st_mode` POSIX names `S_IWUSR`. A file or directory whose mode has this bit set is writable by its owner. A related bit is `S_IROTH` that when set means other users (*i.e.*, neither the owner nor in the group) may read that particular file or directory.

[1]: http://perldoc.perl.org/functions/stat.html

The [perlfunc documentation for `stat`][1] gives the names of commonly available mode bits. We can examine their values.

```perl
#! /usr/bin/env perl

use strict;
use warnings;
use Fcntl ':mode';

my $perldoc_f_stat = q(
    # Permissions: read, write, execute, for user, group, others.
    S_IRWXU S_IRUSR S_IWUSR S_IXUSR
    S_IRWXG S_IRGRP S_IWGRP S_IXGRP
    S_IRWXO S_IROTH S_IWOTH S_IXOTH

    # Setuid/Setgid/Stickiness/SaveText.
    # Note that the exact meaning of these is system dependent.
    S_ISUID S_ISGID S_ISVTX S_ISTXT

    # File types.  Not necessarily all are available on your system.
    S_IFREG S_IFDIR S_IFLNK S_IFBLK S_IFCHR S_IFIFO S_IFSOCK S_IFWHT S_ENFMT
);

my %mask;
foreach my $sym ($perldoc_f_stat =~ /\b(S_I\w+)\b/g) {
    my $val = eval { no strict 'refs'; &$sym() };
    if (defined $val) {
    $mask{$sym} = $val;
    }
    else {
    printf "%-10s - undefined\n", $sym;
    }
}

my @descending = sort { $mask{$b} <=> $mask{$a} } keys %mask;
printf "%-10s - %9o\n", $_, $mask{$_} for @descending;
```

On Red Hat Enterprise Linux and other operating systems in the System V family, the output of the above program will be

<pre>S_ISTXT    - undefined
S_IFWHT    - undefined
S_IFSOCK   -    140000
S_IFLNK    -    120000
S_IFREG    -    100000
S_IFBLK    -     60000
S_IFDIR    -     40000
S_IFCHR    -     20000
S_IFIFO    -     10000
S_ISUID    -      4000
S_ISGID    -      2000
S_ISVTX    -      1000
S_IRWXU    -       700
S_IRUSR    -       400
S_IWUSR    -       200
S_IXUSR    -       100
S_IRWXG    -        70
S_IRGRP    -        40
S_IWGRP    -        20
S_IXGRP    -        10
S_IRWXO    -         7
S_IROTH    -         4
S_IWOTH    -         2
S_IXOTH    -         1</pre>

## Bit twiddling

The numbers above are octal (base 8), so any given digit must be 0-7 and has place value 8<sup>*n*</sup>, where *n* is the zero-based number of places to the left of the radix point. To see how they map to bits, octal has the convenient property that each digit corresponds to three bits. Four, two, and 1 are all exact powers of two, so in binary, they are 100, 10, and 1 respectively. Seven (= 4 + 2 + 1) in binary is 111, so then 70<sub>8</sub> is 111000<sub>2</sub>. The latter example shows how converting back and forth is straightforward.

With a bit field, you don’t care exactly *what* the value of a bit in that position is but *whether* it is zero or non-zero, so

```perl
if ($mode & $mask) {
```

tests whether any bit in `$mode` corresponding to `$mask` is set. For a simple example, given the 4-bit integer 1011 and a mask 0100, their bitwise AND is

<pre>  1011
& 0100
------
  0000</pre>

So the bit in that position is clear—as opposed to a mask of, say, 0010 or 1100.

Clearing the most significant bit of 1011 looks like

<pre>    1011      1011
& ~(1000) = & 0111
            ------
              0011</pre>

Recall that `~` in Perl is bitwise complement.

For completeness, set a bit with bitwise OR as in

```perl
$bits |= $mask;
```

## Octal and file permissions

An octal digit’s direct mapping to three bits is convenient for Unix permissions because they come in groups of three. For example, the permissions for the program that produced the output above are

<pre>-rwxr-xr-x 1 gbacon users 1096 Feb 24 20:34 modebits</pre>

That is, the owner may read, write, and execute; but everyone else may read and execute. In octal, this is 755—a compact shorthand. In terms of the table above, the set bits in the mode are

* `S_IRUSR`
* `S_IWUSR`
* `S_IXUSR`
* `S_IRGRP`
* `S_IXGRP`
* `S_IROTH`
* `S_IXOTH`

We can decompose the mode from your question by adding a few lines to the program above.

```perl
my $mode = 33188;
print "\nBits set in mode $mode:\n";
foreach my $sym (@descending) {
    if (($mode & $mask{$sym}) == $mask{$sym}) {
        print "  - $sym\n";
        $mode &= ~$mask{$sym};
    }
}

printf "extra bits: %o\n", $mode if $mode;
```

The mode test has to be more careful because some of the masks are shorthand for multiple bits. Testing that we get the exact mask back avoids false positives when some of the bits are set but not all.

The loop also clears the bits from all detected hits so at the end we can check that we have accounted for each bit. The output is

<pre>Bits set in mode 33188:
  - S_IFREG
  - S_IRUSR
  - S_IWUSR
  - S_IRGRP
  - S_IROTH</pre>

No extra warning, so we got everything.

## That magic 07777

Converting 7777<sub>8</sub> to binary gives `0b111_111_111_111`. Recall that 7<sub>8</sub> is 111<sub>2</sub>, and four 7s correspond to 4×3 ones. This mask is useful for selecting the set bits in the last twelve. Looking back at the bit masks we generated earlier

<pre>S_ISUID    -      4000
S_ISGID    -      2000
S_ISVTX    -      1000
S_IRWXU    -       700
S_IRWXG    -        70
S_IRWXO    -         7</pre>

we see that the last 9 bits are the permissions for user, group, and other. The three bits preceding those are the setuid, setgroupid, and what is sometimes called the sticky bit. For example, the full mode of `sendmail` on my system is `-rwxr-sr-x` or 34285<sub>10</sub>. The bitwise AND works out to be

<pre>  (dec)      (oct)                (bin)
  34285     102755     1000010111101101
&  4095 = &   7777 = &     111111111111
-------   --------   ------------------
   1517 =     2755 =        10111101101</pre>

The high bit in the mode that gets discarded is `S_IFREG`, the indicator that it is a regular file. Notice how much clearer the mode expressed in octal is when compared with the same information in decimal or binary.

The [`stat` documentation][1] mentions a helpful function.

> &hellip; and the `S_IF*` functions are
>
> **`S_IMODE($mode)`**  
> the part of `$mode` containing the permission bits and the setuid/setgid/sticky bits

[2]: https://github.com/mirrors/perl/blob/v5.17.8/ext/Fcntl/Fcntl.xs#L66

[In `ext/Fcntl/Fcntl.xs`][2], we find its implementation and a familiar constant on the last line.

```c
    void
    S_IMODE(...)
        PREINIT:
            dXSTARG;
            SV *mode;
        PPCODE:
            if (items > 0)
                mode = ST(0);
            else {
                mode = &PL_sv_undef;
                EXTEND(SP, 1);
            }
            PUSHu(SvUV(mode) & 07777);
```

To avoid the bad practice of [magic numbers][3] in source code, write

```perl
my $permissions = S_IMODE $mode;
```

[3]: https://en.wikipedia.org/wiki/Magic_number_%28programming%29

Using `S_IMODE` and other functions available from the Fcntl module also hides the low-level bit twiddling and focuses on the domain-level information the program wants. The documentation continues

> **`S_IFMT($mode)`**  
> the part of `$mode` containing the file type which can be bit-anded with (for example) `S_IFREG` or with the following functions
>
> ```perl
> # The operators -f, -d, -l, -b, -c, -p, and -S.
> S_ISREG($mode) S_ISDIR($mode) S_ISLNK($mode)
> S_ISBLK($mode) S_ISCHR($mode) S_ISFIFO($mode) S_ISSOCK($mode)
>
> # No direct -X operator counterpart, but for the first one
> # the -g operator is often equivalent.  The ENFMT stands for
> # record flocking enforcement, a platform-dependent feature.
> S_ISENFMT($mode) S_ISWHT($mode)
> ```

Using these constants and functions will make your programs clearer by more directly expressing your intent.
