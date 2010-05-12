from string import whitespace
import pprint

class TypedValue:
    pass

class Pair:
    def __init__(self, car, cdr):
        self.Car = car
        self.Cdr = cdr

def IsAtom(x):

    pass

def IsNumber(x):
    pass

def Value(exp, env):
    pass



atom_end = set('()"\'') | set(whitespace)

# orig from http://gist.github.com/240957
def parse(sexp):
    stack, i, length = [[]], 0, len(sexp)
    while i < length:
        c = sexp[i]

        #print c, stack
        reading = type(stack[-1])
        if reading == list:
            if c == '(':
                stack.append([])
            elif c == ')': 
                stack[-2].append(stack.pop())
                if stack[-1][0] == ('quote',): stack[-2].append(stack.pop())
            elif c == '"':
                stack.append('')
            elif c == "'":
                stack.append([('quote',)])
            elif c in whitespace:
                pass
            else:
                stack.append((c,))
        elif reading == str:
            if c == '"': 
                stack[-2].append(stack.pop())
                if stack[-1][0] == ('quote',): stack[-2].append(stack.pop())
            elif c == '\\': 
                i += 1
                stack[-1] += sexp[i]
            else:
                stack[-1] += c
        elif reading == tuple:
            if c in atom_end:
                atom = stack.pop()
                if atom[0][0].isdigit():
                    stack[-1].append(eval(atom[0]))
                else:
                    stack[-1].append(atom)
                if stack[-1][0] == ('quote',):
                    stack[-2].append(stack.pop())
                continue
            else:
                stack[-1] = ((stack[-1][0] + c),)
        i += 1
    return stack.pop()

def toSimple(tree):
    """ todo; """
    return tree

# our 5 registers
EXP = None     # expression under evaluation
ENV = None     # pointer to environment which is the conetxt of
                    # evaluation of the current expression under evaluation
VAL = None     # result of evaluation
ARGS = None    # list of parameters
CLINK = None   # head of control list (return addr like a link reg)

tramp = None # workaround for no TCO

def Car(pair):
    return pair.Car

def Cdr(pair):
    return pair.Cdr

def Cadr(pair):
    return pair.Cdr.Car

def Caddr(pair):
    return pair.Cdr.Cdr.Car

def Cons(a, d):
    return Pair(a, d)

def Null(x):
    return x == None

def List(*args):
    pass

def Run():
    global EXP, ENV, VAL, ARGS, CLINK, tramp
    tramp = EvalDispatch
    while True:
        to = tramp
        if to == None:
            print "done"
            break
        tramp = None
        to()

def EvalDispatch():
    global EXP, ENV, VAL, ARGS, CLINK, tramp
    if IsAtom(EXP):
        if IsNumber(EXP):           # numbers eval to themselves
            VAL = EXP
        else:
            VAL = Value(EXP, ENV)   # look up symbols in env
        PopJReturn()
    elif Car(EXP) == ('quote',):    # return the quoted constant
        VAL = Cadr(EXP)
        PopJReturn()
    elif Car(EXP) == ('lambda',):
        VAL = Cadr(EXP)             # formal parameters
        EXP = Caddr(EXP)            # body
        VAL = List(('&procedure',), VAL, EXP, ENV) # construct a closure with env
        PopJReturn()
    elif Car(EXP) == ('if',):
        CLINK = Cons(ENV, CLINK)    # save the environment
        CLINK = Cons(EXP, CLINK)    # save the expression
        CLINK = Cons(EvifDecide, CLINK) # set up a return addr
        EXP = Cadr(EXP)             # extract the predicate
        tramp = EvalDispatch
    elif Null(Cdr(EXP)):            # call w/o args
        CLINK = Cons(ApplyNoArgs, CLINK) # set up a return addr
        EXP = Car(EXP)              # get function position
        tramp = EvalDispatch
    else:
        CLINK = Cons(ENV, CLINK)
        CLINK = Cons(EXP, CLINK)
        CLINK = Cons(Evargs, CLINK)
        EXP = Car(EXP)
        tramp = EvalDispatch

def PopJReturn():
    """ return to caller
    save return addr in EXP and pop off CLINK, and then jump
    """
    global EXP, ENV, VAL, ARGS, CLINK, tramp
    EXP = Car(CLINK)
    CLINK = Cdr(CLINK)
    tramp = EXP

def EvifDecide(self):
    """ after predicate of conditional is evaluated, come back here and
    look at VAL to see which branch to take. select one and set EXP. """
    global EXP, ENV, VAL, ARGS, CLINK, tramp
    EXP = Car(CLINK)
    CLINK = Cdr(CLINK)

def ApplyNoArgs(self):
    """ just set a null argument list and apply fucntion in VAL """
    global EXP, ENV, VAL, ARGS, CLINK, tramp
    ARGS = None
    tramp = Apply()

def Evargs():
    """ general argument evaluation """
    global EXP, ENV, VAL, ARGS, CLINK, tramp
    EXP = Car(CLINK)                # restore EXP
    CLINK = Cdr(CLINK)

    ENV = Car(CLINK)                # restore ENV
    CLINK = Cdr(CLINK)

    CLINK = Cons(VAL, CLINK)        # save function
    EXP = Cdr(EXP)                  # get rid of function part
    ARGS = None
    Evargs1()

def Evargs1():
    """ top of the loop """
    global EXP, ENV, VAL, ARGS, CLINK, tramp
    if Null(Cdr(EXP)):
        """ if it's the last arg, save the arg list, set up the return addr,
        then set up the last argument and evaluate it """
        CLINK = Cons(ARGS, CLINK)
        CLINK = Cons(LastArg, CLINK)
        EXP = Car(EXP)
    else:
        """ otherwise, save env, exp, and argument list, set up return
        address, set up next argument and evaluate it """
        CLINK = Cons(ENV, CLINK)
        CLINK = Cons(EXP, CLINK)
        CLINK = Cons(ARGS, CLINK)
        CLINK = Cons(Evargs2, CLINK)
        EXP = Car(EXP)
    tramp = EvalDispatch

def Evargs2():
    """ come here after each arg is evaluated. the evaluated argument is
    accumulated into ARGS """
    global EXP, ENV, VAL, ARGS, CLINK, tramp

    ARGS = Car(CLINK)               # restore ARGS
    CLINK = Cdr(CLINK)

    EXP = Car(CLINK)                # restore EXP
    CLINK = Cdr(CLINK)

    ENV = Car(CLINK)                # restore ENV
    CLINK = Cdr(CLINK)

    ARGS = Cons(VAL, ARGS)          # add to argument list
    EXP = Cdr(EXP)                  # flush form just evaluated

    tramp = Evargs1                 # go to next argument

def LastArg():
    global EXP, ENV, VAL, ARGS, CLINK, tramp
    ARGS = Car(CLINK)               # restore ARGS
    CLINK = Cdr(CLINK)

    ARGS = Cons(VAL, ARGS)          # add last value to it

    VAL = Car(CLINK)                # get function
    CLINK = Cdr(CLINK)

    tramp = Apply

def Apply():
    global EXP, ENV, VAL, ARGS, CLINK, tramp
    pass


def main():
    pprint.pprint(toSimple(parse("""
(append '(a b c) '(d 32.9 e f "dog"))
""")))

if __name__ == "__main__":
    main()
