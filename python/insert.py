#! /usr/bin/python3

import sys, os

class Block:
    block_id = ''
    source1 = ''
    source2 = ''
    mixtures = []
    def __init__(self, block_id = '', source1 = '', source2 = '', mixtures = []):
        self.block_id = block_id
        self.mixtures = mixtures
        self.source1 = source1
        self.source2 = source2

        # Convert mixtures to a set of characters. For example, 
        #     ''.join(["AT", "TT"]) 
        # creates the string "ATTT". set() then converts that string to 
        # a set of characters {'A', 'T'}
        sources = set(''.join(mixtures))

        # If a source is empty, we take from the set the first element (pop()) 
        # after removing the other source (difference()). Since the set 
        # contains single characters, we double it to get "AA", "TT", etc.
        if self.source1 == '':
            self.source1 = sources.difference(set(self.source2)).pop()*2
        sources.remove (self.source1[0])
        if self.source2 == '':
            self.source2 = sources.pop()*2

    def print (self):
        print (self.block_id, "source1", self.source1)
        print (self.block_id, "source2", self.source2)
        for mix in self.mixtures:
            print (self.block_id, "mixture", mix)

if len(sys.argv) == 1:
    files = [os.stdin]
else:
    files = (open(f) for f in sys.argv[1:])

for f in files:
    # Read in all the lines
    data = [line for rawline in f for line in [rawline.strip().split(' ')]]
    # Get the unique block IDs
    blocks = set (lines[0] for line in data)
    # For each block ID
    for b in blocks:
        # The corresponding mixtures
        mix = [line[2] for line in data if line[0] == b and "mixture" == line[1]]

        # If "source1 XX" is present, we will get the list ['XX'], and [] if 
        # source1 is not present. ''.join() allows us to flatten ['XX']  to 
        # just 'XX' (and doesn't affect []). Similarly for source2.
        source1 = ''.join(d[2] for line in data if line[0] == b and "source1" == line[1])
        source2 = ''.join(d[2] for line in data if line[0] == b and "source2" == line[1])

        # Create an object of the class defined above, and print it.
        # Initialization fills up the blank values.
        Block(b, source1, source2, mix).print()
