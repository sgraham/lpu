import sys
from util import *

def disassemble(ops):
    c = 0
    for line in lineiter(ops):
        asInt = int(line)
        op, arg = Opcode.Decode(asInt)
        print "%05d: <%8d | %8s> %s%s" % (c, asInt, itobin8(asInt), op, '' if arg == None else (" " + str(arg)))
        c += 1

def main():
    readOpDesc(open("opcodes.txt").read())
    if len(sys.argv) == 1:
        f = sys.stdin
    else:
        f = open(sys.argv[1])
    disassemble(f.read())

if __name__ == "__main__": main()
