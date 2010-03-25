package main

import "fmt"

/*
    top bit of car is 'in use'
    top bit of cdr is for the gc
    next 7 bits of both are the Type
    last 24 bits are the data, sometimes immediate, else points to another node
*/
type Pair struct {
    car uint32
    cdr uint32
}

type Type int
const (
    t_pointer = iota
    t_fixnum
    t_symbol
    t_list
    t_variable
    t_procedure
    t_if
    t_combination
    t_moreArgs
    t_funcall
    t_cons
    t_car
    t_cdr
    t_atom
)

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
        case t_combination: return "t_combination"
        case t_moreArgs: return "t_moreArgs"
        case t_funcall: return "t_funcall"
        case t_cons: return "t_cons"
        case t_car: return "t_car"
        case t_cdr: return "t_cdr"
        case t_atom: return "t_atom"
    }
    return "<unknown>"
}

func (p Pair) IsInUse() bool { return p.car & 0x80000000 != 0 }
func (p Pair) SetInUse() () { p.car |= 0x80000000 }
func (p Pair) ClrInUse() () { p.car &^= 0x80000000 }

func (p Pair) IsCarBeingTraced() bool { return p.cdr & 0x80000000 != 0 }
func (p Pair) SetCarBeingTraced() () { p.cdr |= 0x80000000 }
func (p Pair) ClrCarBeingTraced() () { p.cdr &^= 0x80000000 }

func (p Pair) CarType() Type { return Type((p.car & 0x7fffffff) >> 24) }
func (p Pair) CdrType() Type { return Type((p.cdr & 0x7fffffff) >> 24) }

func (p Pair) CarData() uint { return uint(p.car & 0xffffff) }
func (p Pair) CdrData() uint { return uint(p.cdr & 0xffffff) }

func (p Pair) String() string {
    return fmt.Sprintf("<InUse:%t, GC:%t, A is:%s, D is:%s, A:%0x, D:%0x>",
            p.IsInUse(), p.IsCarBeingTraced(),
            p.CarType().String()[2:], p.CdrType().String()[2:],
            p.CarData(), p.CdrData())
}


type MachineData struct {
    memory [0xffffff]Pair
}

func main() {
    md := new(MachineData)
    a := 5 + 5
    fmt.Printf("hello, world: %d %v\n", a, md.memory[0])
}
