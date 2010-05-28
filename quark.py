import ast
import _ast
import sys

"""

000 nop

001 dup

010 one

011 zero

100 load

101 pop

110 sub

111 jz

nand
xor
shl
add
not
inc

"""

# note, currently non-de-packing, each opcode is a full word
def sim():
    ir = 0
    pc = 0
    acc = 0
    sp = 0
    t = 0
    mem = [0] * 32768

    while True:
        ir = mem[pc]
        pc += 1
        if ir == 0: # nop
            pass
        elif ir == 1: # dup
            mem[sp] = acc
            sp += 1
        elif ir == 2: # one
            acc = (acc << 1) | 1
        elif ir == 3: # zero
            acc = (acc << 1) | 0
        elif ir == 4: # load
            acc = mem[acc]
        elif ir == 5: # pop
            sp -= 1
            t = mem[sp]
            m[t] = acc
            sp -= 1
            acc = m[sp]
        elif ir == 6: # sub
            sp -= 1
            acc = m[sp] - acc
        elif ir == 7: # jz
            sp -= 1
            t = m[sp]
            sp -= 1
            if t == 0:
                pc = acc
                acc = m[sp]
                break
            acc = m[sp]

class GetStaticData(ast.NodeVisitor):
    def __init__(self, ctx):
        self.ctx = ctx
        self.inFunc = 0
    def visit_FunctionDef(self, node):
        self.inFunc += 1
        for n in ast.iter_child_nodes(node):
            self.visit(n)
        self.inFunc -= 1
    def visit_Assign(self, node):
        if self.inFunc == 0:
            if len(node.targets) != 1:
                raise CompileError("expecting only one value on lhs", node)
            if node.targets[0].id in self.ctx.staticData:
                raise CompileError("duplicate static data name '%s'" % node.targets[0].id, node)
            if isinstance(node.value, _ast.Call):
                if node.value.func.id != "Array":
                    raise CompileError("call that wasn't Array", node)
                if len(node.value.args) != 1 or not isinstance(node.value.args[0], _ast.Num):
                    raise CompileError("Array expects one numeric argument", node)
                self.ctx.staticData[node.targets[0].id] = [node.value.args[0].n]
            elif isinstance(node.value, _ast.Num):
                self.ctx.staticData[node.targets[0].id] = node.value.n
            else:
                raise CompileError("unexpected rhs", node)

class FunctionsToAsm(ast.NodeVisitor):
    def __init__(self, ctx):
        self.ctx = ctx
        self.inFunc = 0
    def visit_FunctionDef(self, node):
        self.inFunc += 1
        for n in ast.iter_child_nodes(node):
            self.visit(n)
        self.inFunc -= 1
    def visit_Assign(self, node):
        if self.inFunc:
            print ast.dump(node)
    def visit_AugAssign(self, node):
        if self.inFunc:
            print ast.dump(node)
    def visit_Call(self, node):
        if self.inFunc:
            print ast.dump(node)

class Context:
    def __init__(self):
        self.staticData = {}

def main():
    prog = ast.parse(open(sys.argv[1]).read(), sys.argv[1])
    #print ast.dump(prog)
    ctx = Context()
    GetStaticData(ctx).visit(prog)
    FunctionsToAsm(ctx).visit(prog)
    #print ctx.staticData

if __name__ == "__main__":
    main()
