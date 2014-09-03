#! /usr/bin/gnuplot

set term png size 1920, 450 font "Ubuntu Mono, 12"
set output 'user-sys.png'

set grid
set mxtics
set key top left
set xlabel 'Size of input files (GB)'
set ylabel 'Time taken (user+sys, seconds)'
set title 'cmp vs diff - user+sys'

plot 'time-cmp-log' using ($47/1024/1024):($20+$22) with lp title 'cmp -s', \
	'time-diff-log' using ($47/1024/1024):($20+$22) with lp title 'diff -q'

set output 'real.png'

set ylabel 'Time taken (real, seconds)'
set title 'cmp vs diff - real'

plot 'time-cmp-log' using ($47/1024/1024):32 with lp title 'cmp -s', \
	'time-diff-log' using ($47/1024/1024):32 with lp title 'diff -q'
