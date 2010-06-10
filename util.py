import string

def countOfXs(bits):
    return len(bits.split('x')) - 1

class Opcode:
    _opcodes = {}
    _intToOp = [None]*256
    def __init__(self, name, bits, oparg=None):
        self.name = name
        self.bits = bits
        numXs = countOfXs(bits)
        self.opcode = string.atoi(bits.strip('x'), 2) << numXs
        self.numImmBits = numXs
        self.oparg = oparg # None, 'N' or 'L' for number/label
        Opcode._opcodes[name] = self
        for i in range(2**numXs):
            Opcode._intToOp[self.opcode | i] = self

    def encode(self, arg):
        if arg == None:
            assert self.oparg == None
            return self.opcode
        if self.oparg == 'N':
            asInt = int(arg)
            return self.opcode | asInt & ((2**self.numImmBits) - 1)
        elif self.oparg == 'L':
            assert "todo;"
        else:
            print self, arg, self.oparg
            raise Exception("unrecognized oparg type")

    def decode(self, asInt):
        if self.oparg == None:
            assert asInt == self.opcode
            return self.name, None
        else:
            return self.name, asInt & ((2**self.numImmBits) - 1)

    @staticmethod
    def Encoded(name, arg=None):
        op = Opcode._opcodes[name]
        return op.encode(arg)

    @staticmethod
    def Decode(asInt):
        return Opcode._intToOp[asInt].decode(asInt)

    def __repr__(self):
        return "<%s=%d>" % (self.name, self.opcode)

def lineiter(over):
    for line in over.split("\n"):
        line = line.partition("#")[0]
        line = line.rstrip()
        if line.lstrip() == "": continue
        yield line

def readOpDesc(opcodedesc):
    for line in lineiter(opcodedesc):
        if line[0] == " ":
            # operation for current opcode
            pass
        elif line[0] == "0" or line[0] == "1":
            # opcode definition
            bits, mnemonic = line.split(None, 1)
            oparg = None
            if ' ' in mnemonic: mnemonic, oparg = mnemonic.split()
            Opcode(mnemonic, bits, oparg)
        else:
            raise Exception("confused by '%s'" % line)
    #print Opcode._opcodes.keys()

def itobin8(num):
    digits = string.digits + string.uppercase
    d = []
    while num > 0:
        num, lastDigit = divmod(num, 2)
        d.append(digits[lastDigit])
    while len(d) != 8:
        d.append('0')
    d.reverse()
    return ''.join(d)
