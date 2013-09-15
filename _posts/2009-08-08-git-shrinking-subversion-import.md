---
title: "git: shrinking Subversion import"
layout: post
permalink: /2009/08/git-shrinking-subversion-import.html
tags: git
comments: true
---
At `$WORK` we’ve been attempting for years—but fairly infrequently—to
do distributed development with centralized Subversion. We finally had
enough and decided to move to git.

Part of that move involved importing a couple of projects with 6+ years
of history. Early revisions carried lots of binary test data, so
[git svn] clone produced repositories weighing in at 3.5 and 4.5
gigabytes.

[git svn]: http://www.kernel.org/pub/software/scm/git/docs/git-svn.html

Another less than satisfactory result was the umpteen bazillion git
branches corresponding to git tags. Some of the git branches formed
families with names of the form *name-x.y.z@1234*, where
*name-x.y.z* is the name of a Subversion release tag and 1234 was a
Subversion revision that modified the tag. A happy design choice made
the branch *name-x.y.z* (with no <tt>@<i>nnn</i></tt>) the head revision
of that Subversion tag, so we easily picked off some targets:

{% highlight bash %}
$ git branch -r -D $(git branch -r | grep @)
{% endhighlight %}

Cribbing from [svn2git], converting the git branches to git tags was a series of commands of the form

[svn2git]: http://github.com/jcoglan/svn2git/

{% highlight bash %}
$ git checkout 1.2.3
$ git tag -a -m 'Tagging release 1.2.3' v1.2.3
$ git branch -r -D 1.2.3
{% endhighlight %}

Then to make the Subversion trunk the git master branch:

{% highlight bash %}
$ git branch -D master
$ git checkout trunk
$ git checkout -f -b master
$ git branch -r -D trunk
{% endhighlight %}

Here’s a good point to checkpoint your work in case you hose something
later.

Using [Antony Stubbs’s script to find the biggest objects in a repo’s
packs][bigobj], we determined that much of the bulk came from huge
[HDF5-format] test baselines along with a few others. So we cut them out:

[bigobj]: http://stubbisms.wordpress.com/2009/07/10/git-script-to-show-largest-pack-objects-and-trim-your-waist-line/
[HDF5-format]: http://www.hdfgroup.org/HDF5/

{% highlight bash %}
$ git filter-branch -d /dev/shm/scratch --index-filter \
  "git rm --cached -f --ignore-unmatch '*.h5'; \
   git rm --cached -f --ignore-unmatch '*.sig'; \
   git rm --cached -f --ignore-unmatch '*.2dsc'" \
  --tag-name-filter cat -- --all
{% endhighlight %}

The use of `--index-filter` makes the long process (remember, it has to
hit all possible revisions) quicker because it operates directly on the
index rather than checking out every snapshot, munging the filesystem,
and shoving the new snapshot back in. Also, `/dev/shm` is a [tmpfs]
mount for better throughput, and the directory named with `-d` shouldn’t
exist.

[tmpfs]: http://en.wikipedia.org/wiki/TMPFS

The `git filter-branch` manpage has a [checklist for shrinking a
repository][checklist] that recommends running `filter-branch` and then
cloning to leave behind the cruft.

[checklist]: http://www.kernel.org/pub/software/scm/git/docs/git-filter-branch.html#_checklist_for_shrinking_a_repository

Cloning with a filesystem path makes hardlinks, so use a URL:

{% highlight bash %}
$ git clone file:///home/gbacon/src/repo.git
{% endhighlight %}

Even after doing this, some big unnamed blobs had survived the clone.
Thanks to [#git on freenode][poundgit] for the suggestion to excise
the [reflog]:

[poundgit]: irc://irc.freenote.net/git
[reflog]: http://www.gitready.com/intermediate/2009/02/09/reflog-your-safety-net.html

{% highlight bash %}
$ git reflog expire --verbose --expire=0 --all
$ git gc --prune=0
{% endhighlight %}

Note that these options will require a fairly recent git.

After all these steps, the git repositories were went from gigabytes to 75 and 100 megabytes, *much* nicer!
