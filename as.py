import sys
from util import *

def assemble(source):
    for line in lineiter(source):
        parts = line.split()
        if len(parts) == 1: parts.append(None)
        print Opcode.Encoded(parts[0], parts[1])

def main():
    readOpDesc(open("opcodes.txt").read())
    assemble(open(sys.argv[1]).read())

if __name__ == "__main__": main()
