#! /usr/bin/python3

import sys
import getopt
import itertools

def usage (exit_code = 0):
    print ('Usage:')
    print (sys.argv[0] + ' [--help] [-x|--max-depth=N] [-n|--min-depth=M] [-r|--repeat] [file]')
    sys.exit (exit_code)

try:
    optlist, args = getopt.gnu_getopt (sys.argv, 'x:n:hr', ['max-depth=', 'min-depth=', '--help', '--repeat'])
except getopt.GetoptError as err:
    print (err)
    usage (2)

min_depth = 1
max_depth = 1
repeat = False

for (opt, val) in optlist:
    if opt in ('-x', '--max-depth'):
        max_depth = int(val)
    elif opt in ('-n', '--min-depth'):
        min_depth = int(val)
    elif opt in ('-h', '--help'):
        usage ()
    elif opt in ('-r', '--repeat'):
        repeat = True

if repeat:
    mix = itertools.product
else:
    mix = itertools.permutations

if len (args) == 1:
    in_file = sys.stdin
else:
    in_file = open (args[1])

phrases = [ line.strip () for line in in_file.readlines () ]

for i in range (min_depth, max_depth + 1):
    passwords = mix (phrases, i)
    for pw in passwords:
        print (''.join(pw))
