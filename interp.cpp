#include <cstdio>
#include <cassert>
#include <cstdlib>

// the unit of allocation is a U64, laid out as:
//
//        31       30      29-24         23-0
//      +--------+------+-------------+----------------------------------+
// CAR  | in use | !ptr | type of car | pointer or immediate data of car |
//      +--------+------+-------------+----------------------------------+
// CDR  | gc use | !ptr | type of cdr | pointer or immediate data of cdr |
//      +--------+------+-------------+----------------------------------+
//
// bit 30 and 29-24 are really just a 7 bit type, but the top bit determines
// pointer-ness. this is helpful for the GC.

class Type
{
    // the pointer types (ptr bit not set)
    enum
    {
        SelfEvaluatingPointer = 0x00,
        Symbol = 0x01,
        Global = 0x02,
        SetGlobal = 0x3,
        Conditional = 0x4,
        Procedure = 0x5,
        FirstArgument = 0x6,
        NextArgument = 0x7,
        LastArgument = 0x8,
        ApplyNoArgs = 0x9,
        Apply1Arg = 0xa,
        PrimitiveApply1 = 0xb,
        PrimitiveApply2 = 0xc,
        Sequence = 0xd,
        SpreadArgument = 0xe,
        Closure = 0xf,
        GetControlPoint = 0x10,
        ControlPoint = 0x11,
        InterruptPoint = 0x12,
        // user types
        UserSelfEvalPointer1 = 0x14,
        UserSelfEvalPointer2 = 0x15,
        UserSelfEvalPointer3 = 0x16,
        UserSelfEvalPointer4 = 0x17,
    };

    // immediate type (ptr bit set)
    enum
    {
        SelfEvaluatingImmediate = 0x40,
        Local = 0x41,
        TailLocal = 0x42,
        SetLocal = 0x43,
        SetTailLocal = 0x44,
        SetOnlyTailLocal = 0x45,
        PrimitiveCar = 0x46,
        PrimitiveCdr = 0x47,
        PrimitiveCons = 0x48,
        PrimitiveRplaca = 0x49,
        PrimitiveRplacd = 0x4a,
        PrimitiveEq = 0x4b,
        PrimitiveTypeP = 0x4c,
        PrimitiveTypeS = 0x4d,
        GcSpecialType = 0x4e,
        UserSelfEvalImmediate1 = 0x4f,
        UserSelfEvalImmediate2 = 0x50,
        UserSelfEvalImmediate3 = 0x51,
        UserSelfEvalImmediate4 = 0x52,
        Mark = 0x53,
        Done = 0x54,
        PrimitiveAdd1 = 0x55,
        PrimitiveSub1 = 0x56,
        PrimitiveZeroP = 0x57,
    };
};

typedef __uint32_t U32;

static const int MaxMemSize = 16*1024*1024;
static U32 sMemory[MaxMemSize];
static U32 sAllocPoint = 4;

#if 0
    #define CHECK_ADDR(_c_) (assert((_c_) % 8 == 0 && (_c_) < MaxMemSize))
#else
    #define CHECK_ADDR(_c_)
#endif


U32 Car(U32 cell)
{
    CHECK_ADDR(cell);
    return sMemory[cell] & 0xffffff;
}

U32 Cdr(U32 cell)
{
    CHECK_ADDR(cell);
    return sMemory[cell + 1] & 0xffffff;
}

bool IsMarked(U32 cell)
{
    CHECK_ADDR(cell);
    return (sMemory[cell] & 0x80000000) != 0;
}

void SetMark(U32 cell)
{
    CHECK_ADDR(cell);
    sMemory[cell] |= 0x80000000;
}

void ClearMark(U32 cell)
{
    CHECK_ADDR(cell);
    sMemory[cell] &= ~0x80000000;
}

bool IsFlagged(U32 cell)
{
    CHECK_ADDR(cell);
    return (sMemory[cell + 1] & 0x80000000) != 0;
}

void SetFlag(U32 cell)
{
    CHECK_ADDR(cell);
    sMemory[cell + 1] |= 0x80000000;
}

void ClearFlag(U32 cell)
{
    CHECK_ADDR(cell);
    sMemory[cell + 1] &= ~0x80000000;
}

void Rplaca(U32 cell, U32 value)
{
    CHECK_ADDR(cell);
    sMemory[cell] &= ~0xffffff;
    sMemory[cell] |= value;
}

void Rplacd(U32 cell, U32 value)
{
    CHECK_ADDR(cell);
    sMemory[cell + 1] &= ~0xffffff;
    sMemory[cell + 1] |= value;
}

bool IsAtom(U32 cell)
{
    CHECK_ADDR(cell);
    return (sMemory[cell] & 0x40000000) != 0;
}

void DumpDot(U32 root)
{
    FILE* f = fopen("tmp.dot", "wt");
    fprintf(f, "digraph nodes {\n");
    fprintf(f, "graph [\nrankdir = \"LR\"\n];\n");
    for (int i = 4; i < 14; i += 2)
    {
        //if (Car(i) != 0 || Cdr(i) != 0)
        {
            fprintf(f, "\"0x%x\" [\n  label = \"<addr> 0x%x (%s%s) | <car> car = ", i, i, IsMarked(i) ? "M" : "", IsFlagged(i) ? "F" : "");
            if (Car(i) == 0) fprintf(f, "(nil)");
            else fprintf(f, "0x%x", Car(i));
            fprintf(f, "| <cdr> cdr = ");
            if (Cdr(i) == 0) fprintf(f, "(nil)");
            else fprintf(f, "0x%x", Cdr(i));
            fprintf(f, "\"\n  shape = \"record\"\n];\n");
            if (Car(i) != 0) fprintf(f, "\"0x%x\":car -> \"0x%x\":addr;\n", i, Car(i));
            if (Cdr(i) != 0) fprintf(f, "\"0x%x\":cdr -> \"0x%x\":addr;\n", i, Cdr(i));
        }
    }
    fprintf(f, "}\n");
    fclose(f);
    system("dot -Tpng tmp.dot -otmp.png && gnome-open tmp.png");
}

U32 Cons()
{
    U32 ret = sAllocPoint;
    sAllocPoint += 2;
    return ret;
}

U32 ConsInit(U32 a, U32 d)
{
    U32 p = Cons();
    Rplaca(p, a);
    Rplacd(p, d);
    return p;
}

void Mark(U32 root)
{
    U32 current = root;
    U32 previous = 0;
    for (;;)
    {
        // follow cars
        while (current != 0 && !IsMarked(current))
        {
            SetMark(current);
            if (!IsAtom(current))
            {
                U32 next = Car(current);
                Rplaca(current, previous);
                previous = current;
                current = next;
            }
        }

        // retreat
        while (previous != 0 && IsFlagged(previous))
        {
            ClearFlag(previous);
            U32 next = Cdr(previous);
            Rplacd(previous, current);
            current = previous;
            previous = next;
        }

        if (previous == 0) break;

        // switch to cdrs
        SetFlag(previous);
        U32 next = Car(previous);
        Rplaca(previous, current);
        current = Cdr(previous);
        Rplacd(previous, next);
    }
}

int main()
{
    // nil
    sMemory[0] = 0;
    sMemory[1] = 0;

    // T
    sMemory[2] = 0;
    sMemory[3] = 0;
    
    U32 p;
    p = ConsInit(6, 8);
    p = ConsInit(0, 0);
    p = ConsInit(10, 0);
    p = ConsInit(12, 4);
    p = ConsInit(0, 0);
    
    Mark(4);

    DumpDot(4);

    return 0;
}
