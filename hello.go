package main

import "fmt"

/*
    top bit of car is 'in use'
    top bit of cdr is for the gc
    next 7 bits of both are the Type
    last 24 bits are the data, sometimes immediate, else points to another node
*/

type Type int
const (
    t_pointer Type = iota
    t_fixnum
    t_symbol
    t_list
    t_variable
    t_procedure
    t_if
    t_if2
    t_combination
    t_moreArgs
    t_funcall
    t_cons
    t_car
    t_cdr
    t_atom
)

type TypedValue uint32

type Pair struct {
    car TypedValue
    cdr TypedValue
}

// todo; ugly as sin, and no assert? what's the better way?
func (t Type) String() string {
    switch (t) {
        case t_pointer: return "t_pointer"
        case t_fixnum: return "t_fixnum"
        case t_symbol: return "t_symbol"
        case t_list: return "t_list"
        case t_variable: return "t_variable"
        case t_procedure: return "t_procedure"
        case t_if: return "t_if"
        case t_if2: return "t_if2"
        case t_combination: return "t_combination"
        case t_moreArgs: return "t_moreArgs"
        case t_funcall: return "t_funcall"
        case t_cons: return "t_cons"
        case t_car: return "t_car"
        case t_cdr: return "t_cdr"
        case t_atom: return "t_atom"
    }
    panic("<unknown>")
}

func (tv TypedValue) GetSpecial() bool { return tv & 0x80000000 != 0 }
func (tv *TypedValue) SetSpecial(v bool) () { *tv |= 0x80000000 }
func (tv *TypedValue) ClrSpecial(v bool) () { *tv &^= 0x80000000 }

func (p *Pair) Car() TypedValue { return p.car }
func (p *Pair) Cdr() TypedValue { return p.cdr }

func (tv TypedValue) Data() uint32 { return uint32(tv & 0xffffff) }
func (tv TypedValue) Type() Type { return Type(tv & 0xff000000 >> 24) }

func (p Pair) String() string {
    return fmt.Sprintf("todo;")
/*
    return fmt.Sprintf("<InUse:%t, GC:%t, A~%s, D~%s, A=%06x (%d), D=%06x (%d)>",
            p.InUse(), p.CarBeingTraced(),
            p.CarType().String()[2:], p.CdrType().String()[2:],
            p.CarData(), p.CarData(),
            p.CdrData(), p.CdrData())
            */
}

const maxAddress = 0xffffff

type MachineData struct {
    memory [maxAddress]Pair

    // our registers
    exp Pair
    env Pair
    val Pair
    args Pair
    clink Pair

    pc uint
}

func main() {
    md := new(MachineData)

    Cons
    md.memory[0].Set(t_if, 1, t_pointer, 0, true)

    md.memory[1].Set(t_fixnum, 100, t_pointer, 2, true)

    md.memory[2].Set(t_fixnum, 200, t_fixnum, 300, true)


    // written in this crazy way because we're trying to pretend we're hardware
    // (no locals, no stack, 5 explicit registers)
    md.exp = md.memory[0]

    for {
        State_Eval:
            switch md.exp.CarType() {
            case t_fixnum, t_symbol, t_list: goto State_Self
            //case t_variable: goto State_Lookup
            //case t_procedure: goto State_Procedure
            case t_if: goto State_If1
            //case t_combination: goto State_EvComb
            }

        State_Self:
            md.val = md.exp
            goto State_Return

        State_If1:
            md.val = md.exp.Cdr()
            md.clink = Cons(md.env, md.clink)
            md.clink = TypedCons(t_if2, md.val, md.clink)
            md.exp = md.exp.Car()
            goto State_Eval

        State_Return:
            switch md.clink.CarType() {
            case t_if2: goto State_If2
            default: panic("unexpected clink return")
            }
    }
}
