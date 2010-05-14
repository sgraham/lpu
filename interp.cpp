#include <cstdio>
#include <cassert>
#include <cstdlib>
#include <cstring>

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
static U32 sMemory[MaxMemSize][2];
static int sAllocPoint = 2;
static const int StartOfMemory = 2;

void ResetMachine()
{
    memset(sMemory, 0, sizeof(sMemory));

    // nil is 0
    // T is 1

    // NOTE: StartOfMemory corresponds to after these constants
}



#if 0
    #define CHECK_ADDR(_c_) (assert((_c_) % 8 == 0 && (_c_) < MaxMemSize))
#else
    #define CHECK_ADDR(_c_)
#endif


U32 Car(U32 cell)
{
    CHECK_ADDR(cell);
    return sMemory[cell][0] & 0xffffff;
}

U32 Cdr(U32 cell)
{
    CHECK_ADDR(cell);
    return sMemory[cell][1] & 0xffffff;
}

bool IsMarked(U32 cell)
{
    CHECK_ADDR(cell);
    return (sMemory[cell][0] & 0x80000000) != 0;
}

void SetMark(U32 cell)
{
    CHECK_ADDR(cell);
    sMemory[cell][0] |= 0x80000000;
}

void ClearMark(U32 cell)
{
    CHECK_ADDR(cell);
    sMemory[cell][0] &= ~0x80000000;
}

bool IsCdrTraceInProgress(U32 cell)
{
    CHECK_ADDR(cell);
    return (sMemory[cell][1] & 0x80000000) != 0;
}

void SetCdrTraceInProgress(U32 cell)
{
    CHECK_ADDR(cell);
    sMemory[cell][1] |= 0x80000000;
}

void ClearCdrTraceInProgress(U32 cell)
{
    CHECK_ADDR(cell);
    sMemory[cell][1] &= ~0x80000000;
}

void Rplaca(U32 cell, U32 value)
{
    CHECK_ADDR(cell);
    sMemory[cell][0] = value;
}

void RplacaAndMark(U32 cell, U32 value)
{
    CHECK_ADDR(cell);
    sMemory[cell][0] = 0x80000000 | value;
}

void Rplacd(U32 cell, U32 value)
{
    CHECK_ADDR(cell);
    sMemory[cell][1] = value;
}

bool IsAtom(U32 cell)
{
    CHECK_ADDR(cell);
    return (sMemory[cell][0] & 0x40000000) != 0;
}

void DumpDot()
{
    FILE* f = fopen("tmp.dot", "wt");
    fprintf(f, "digraph nodes {\n");
    fprintf(f, "graph [\nrankdir = \"LR\"\n];\n");
    for (int i = StartOfMemory; i < sAllocPoint; ++i)
    {
        fprintf(f,
                "\"0x%x\" [\n  label = \"<addr> 0x%x (%s%s) | <car> car = ",
                i,
                i,
                IsMarked(i) ? "M" : "",
                IsCdrTraceInProgress(i) ? "T" : "");
        if (Car(i) == 0) fprintf(f, "(nil)");
        else fprintf(f, "0x%x", Car(i));
        fprintf(f, "| <cdr> cdr = ");
        if (Cdr(i) == 0) fprintf(f, "(nil)");
        else fprintf(f, "0x%x", Cdr(i));
        fprintf(f, "\"\n  shape = \"record\"\n];\n");
        if (Car(i) != 0)
            fprintf(f, "\"0x%x\":car -> \"0x%x\":addr;\n", i, Car(i));
        if (Cdr(i) != 0)
            fprintf(f, "\"0x%x\":cdr -> \"0x%x\":addr;\n", i, Cdr(i));
    }
    fprintf(f, "}\n");
    fclose(f);
    system("dot -Tpng tmp.dot -otmp.png && gnome-open tmp.png");
}

U32 Cons()
{
    U32 ret = sAllocPoint;
    sAllocPoint += 1;
    return ret;
}

U32 ConsInit(U32 a, U32 d)
{
    U32 p = Cons();
    Rplaca(p, a);
    Rplacd(p, d);
    return p;
}

// Walk down the cars, setting the mark bit, and replacing the car value with
// a pointer to the parent. When we get to the end of the car chain, restore
// car, and replace cdr with parent pointer, and set bit noting that we're on
// the right side, then continue down the left. if we're done the car and the
// cdr (both bits set), then restore the cdr and go back up to the saved
// parent and finish the rest of the graph above.
//
// (this is called Deutsh-Schorr-Waite, or simply "pointer reversal" for
// googling).
static void Mark(U32 root)
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
                RplacaAndMark(current, previous);
                previous = current;
                current = next;
            }
        }

        // retreat
        while (previous != 0 && IsCdrTraceInProgress(previous))
        {
            U32 next = Cdr(previous);
            Rplacd(previous, current); // clear of CdrTraceInProgress here
            current = previous;
            previous = next;
        }

        if (previous == 0) break;

        // switch to cdrs
        U32 next = Car(previous);
        RplacaAndMark(previous, current);
        current = Cdr(previous);
        Rplacd(previous, next);
        SetCdrTraceInProgress(previous);
    }
}

// Phase 1:
// Walk from the start of memory at the bottom until finding a non-used (mark
// bit not set) cell. Walk from the top of the allocations until we find a
// used (mark bit set) cell. Move the used into the unused. Write a pointer to
// the new location into the old used location. Continue until the two
// pointers meet.
//
// Phase 2:
// Iterate through all in use memory (start of memory until where phase 1
// terminated) and update all pointers: if they point past termination point
// then they were moved, so update the address with the one that was saved
// into the old location.
//
// This is called "two-finger" sweep (the top and bottom pointers are the
// fingers).
static void Sweep()
{
    U32 low = StartOfMemory;
    U32 high = sAllocPoint - 1;
    for (;;)
    {
        while (!IsMarked(high))
        {
            if (high == low) goto donePhase1;
            high -= 1;
        }
        while (IsMarked(low))
        {
            if (high == low) goto donePhase1;
            low += 1;
        }
        RplacaAndMark(low, Car(high));
        Rplacd(low, Cdr(high));
        Rplaca(high, low); // set broken heart; also clears mark
    }
donePhase1:

    assert(low == high);

    high += 1;

    for (U32 i = StartOfMemory; i < high; ++i)
    {
        if (Car(i) >= high)
        {
            Rplaca(i, Car(Car(i)));
        }
        if (Cdr(i) >= high)
        {
            Rplacd(i, Car(Cdr(i)));
        }

        // can't save mark data because otherwise the full tree won't be
        // traversed next time.
        ClearMark(i);
    }
    sAllocPoint = high;
}

void GC()
{
    Mark(StartOfMemory);
    Sweep();
}

static void UnitTests()
{
    ResetMachine();

    /*
        A -> B,C
        B -> nil,nil
        C -> D,nil
        D -> E,A
        E -> nil,nil
    */
    ConsInit(3, 4);
    ConsInit(0, 0);
    ConsInit(5, 0);
    ConsInit(6, 2);
    ConsInit(0, 0);

    GC();

    assert(5 + StartOfMemory == sAllocPoint);
    //DumpDot();


    // nuke C->D ptr which chops off D/E
    Rplaca(4, 0);

    GC();
    assert(3 + StartOfMemory == sAllocPoint);
    //DumpDot();
}

int main(int argc, char** argv)
{
    if (argc >= 2 && strcmp(argv[1], "--tests") == 0)
    {
        UnitTests(); // assert on fail
        return 0;
    }

    printf("nothing to do.\n");
    return 0;
}
