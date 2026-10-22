---
title: "Waiting for Go-done"
description: "Which batch jobs are still running? List files that lack a given line using grep -c, a regex anchor, and sed, then the one-flag fix: grep -L."
author: "Greg Bacon"
date: "2009-06-05"
tags:
  - "Regex"
  - "Unix"
---
A coworker called this morning wanting to know how to get a list of files that do *not* contain the text ‘Program complete’ on any line. They run jobs in big batches, and each process writes its output to a separate file. The running times vary, and they wanted an easy way to see at a glance which processes are still running.

Running `grep -v` will print all lines that don’t match the given pattern, but that doesn’t help in this case because we want to treat the output files as though each contained a single line.

With the `-c` option, grep outputs the number of lines that matched. Say we have outputs named `output1` through `output4`, and the odd-numbered jobs are finished. This would give us

```console
$ grep -c 'Program complete' output*
output1:1
output2:0
output3:1
output4:0
```

The pattern requires quotes because it contains a space. Without the quotes, grep would search for Program in files named `complete`, `output1`, and so on.

The outputs for the processes still running are the ones containing zero matches, so let’s look for those:

```console
$ grep -c 'Program complete' output* | grep ':0$'
output2:0
output4:0
```

Remember that a dollar sign in a regular expression anchors the match to the end.

Quick cleanup with sed gives us the names of the outputs (backslash is the shell’s line-continuation marker that lets us split long lines):

```console
$ grep -c 'Program complete' output* | \
  grep ':0$' | \
  sed -e 's/:0$//'
output2
output4
```

**UPDATE:** Turns out there’s a much easier way to do it. GNU grep has a [`--files-without-match`](http://www.gnu.org/software/grep/manual/grep.html#General-Output-Control) option (aka `-L`), so the command is the simple

```console
$ grep -L 'Program complete' output*
output2
output4
```

## Further reading

* [*sed & awk*][sedawk], 2nd edition, by Dale Dougherty and Arnold Robbins: Unix regular expression syntax and stream editing with sed.
* [*Classic Shell Scripting*][css] by Arnold Robbins and Nelson H. F. Beebe: combining grep, sed, and other Unix tools in portable shell pipelines.
* [*The Linux Command Line*][tlcl], 3rd edition, by William Shotts: an introduction to the shell, including regular expressions and text processing with grep and sed.

[sedawk]: https://www.amazon.com/dp/1565922255
[css]: https://www.amazon.com/dp/0596005954
[tlcl]: https://www.amazon.com/dp/1718504527

Related: [Learning Regular Expressions](/posts/learning-regex/)
