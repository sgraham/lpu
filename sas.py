import sys

opcodes = """
00000000 nop
00000001 car
    A[7-0] = MEM[P*4]
    A[f-8] = MEM[P*4+1]
00000010 cdr
    D[7-0] = MEM[P*4+2]
    D[f-8] = MEM[P*4+3]
00000011 load
    A[7-0] = MEM[P*4]
    A[f-8] = MEM[P*4+1]
    D[7-0] = MEM[P*4+2]
    D[f-8] = MEM[P*4+3]
00000100 cons
    if HEAP == 0:
        CDR(GC1) = IP
        IP = CAR(GC1)
    else:
        HEAP -= 1
        CAR(HEAP) = NIL
        CDR(HEAP) = P
        P = HEAP
00000101 rplaca
    MEM[P*4] = A[7-0]
    MEM[P*4+1] = A[f-8]
00000110 rplacd
    MEM[P*4+2] = D[7-0]
    MEM[P*4+3] = D[f-8]
00000111 rplacb
    MEM[P*4] = A[7-0]
    MEM[P*4+1] = A[f-8]
    MEM[P*4+2] = D[7-0]
    MEM[P*4+3] = D[f-8]

00001000 ?
00001001 ?
00001010 ?
00001100 ?
00001101 ?
00001110 ?
00001111 ?

00010000 add
    A[b-0] = A[b-0] + D[b-0]
00010001 inc
    A[b-0] = A[b-0] + 1
00010010 xor
    A[b-0] = A[b-0] ^ D[b-0]
00010011 not
    A[b-0] = ~A[b-0]
00010100 and
    A[b-0] = A[b-0] & D[b-0]
00010101 or
    A[b-0] = A[b-0] | D[b-0]
00010110 rol
    A[b-0] = A[b-0] rol 1
00010111 ror
    A[b-0] = A[b-0] ror 1

00011000 movAD
    D[f-0] = A[f-0]
00011001 movDA
    A[f-0] = D[f-0]
00011010 movAP
    P[f-0] = A[f-0]
00011011 movPA
    A[f-0] = P[f-0]
00011100 movDP
    P[f-0] = D[f-0]
00011101 movPD
    D[f-0] = P[f-0]
00011110 swapAD
    tmp = A[f-0]
    A[f-0] = D[f-0]
    D[f-0] = tmp
00011111 swapAP
    tmp = A[f-0]
    A[f-0] = P[f-0]
    P[f-0] = tmp

00100000 decA
    A[b-0] = A[b-0] - 1
00100001 decP
    P[b-0] = P[b-0] - 1
00100010 ?
00100011 ?
00100100 atom
    A[0-0] = A[f-f]
00100100 ?
00100110 lt
00100111 j
    IP = A[b-0]*4

# GC helpers
00101000 togglehs
    HS = HS ^ 1
00101001 ?
00101010 ?
00101011 ?
00101100 ?
00101101 ?
00101110 ?
00101111 ?

0011xxxx setty N
    A[f-c] = INSTR[3-0]
01xxxxxx jz L
    # todo; sign extension
    IP = IP + INSTR[5-0]
10xxxxxx immlo N
    A[5-0] = INSTR[5-0]
    A[b-6] = 0 # todo; want this?
11xxxxxx immhi N
    A[b-6] = INSTR[5-0]
"""

MnemonicToBits = {}

def getBits(instr):
    return 0

def readOpDesc():
    global MnemonicToBits
    for line in opcodes.split("\n"):
        line = line.partition("#")[0]
        line = line.rstrip()
        if line.lstrip() == "": continue
        print line
        if line[0] == " ":
            # operation for current opcode
            pass
        elif line[0] == "0" or line[0] == "1":
            # opcode definition
            bits, mnemonic = line.split(None, 1)
            MnemonicToBits[mnemonic] = bits
            print mnemonic,'=',bits
        else:
            raise Exception("confused by '%s'" % line)

def assemble(source):
    print source

def main():
    readOpDesc()
    assemble(open(sys.argv[1]).read())

if __name__ == "__main__": main()
