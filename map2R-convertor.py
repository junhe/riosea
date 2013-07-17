#!/usr/bin/env python

# Aaron Torres wrote it at first
#
# It conversts PLFS map to R-input-compatible format

import sys

def main():
    f = open(sys.argv[1], "r")

    print "PID IO Logical_offset Length Begin_timestamp End_timestamp  Logical_tail ORG.PID Chunk_offset"
    for line in f:
        if " w " in line:
            line = line.replace("[", "")
            line = line.replace("]", "")
            vals = line.split(".")
            print ".".join(vals[0:-1]) + " " +vals[-1].strip()



    f.close()


if __name__ == "__main__":
    main()
