# Experiment on speed of `diff` and `cmp`

In a comment on an Ask Ubuntu question, someone noted that `cmp` was
slower than `cmp`. This  prompted me to ask a question on [Unix &
Linux](http://unix.stackexchange.com/questions/153286/is-cmp-faster-than-diff-q).
I got [two](http://unix.stackexchange.com/a/153305/70524)
[answers](http://unix.stackexchange.com/a/153422/70524) contradicting
each other and a comment prompting to test them out myself. 

So I wrote these scripts to compare them in (what I hope is) a fair
manner. The script uses `/dev/urandom` and `dd` (base64-encoded) to
generate two files.  I use `sed` two replace a single character on the
last line (picking the first appearance of `[+0-9]`, which should be
random, so no patterns here. Then I `dd` a bit more `urandom` data to
see if trailing differing data makes any difference.

The specs:

 - Intel(R) Core(TM) i5-3550 CPU @ 3.30GHz, 8GB 1333 MHz RAM
 - One round on a `tmpfs` filesystem, limited to 1.5GB files.
 - One round on an ext4 partition on 5400 RPM disk, limited to 5GB
   files.

The files grew in O(n<sup>2</sup>). If you notice, in each iteration:

    COUNT += i
So `COUNT = i*(i+1)/2`.
