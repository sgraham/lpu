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

func (p *Pair) InUse() bool { return p.car & 0x80000000 != 0 }
func (p *Pair) SetInUse() () { p.car |= 0x80000000 }
func (p *Pair) ClrInUse() () { p.car &^= 0x80000000 }

func (p *Pair) CarBeingTraced() bool { return p.cdr & 0x80000000 != 0 }
func (p *Pair) SetCarBeingTraced() () { p.cdr |= 0x80000000 }
func (p *Pair) ClrCarBeingTraced() () { p.cdr &^= 0x80000000 }

func (p *Pair) CarType() Type { return Type((p.car & 0x7fffffff) >> 24) }
func (p *Pair) SetCar(t Type, data uint32) { p.car = uint32(t << 24) | data }
func (p *Pair) SetCarNil() { p.SetCar(t_pointer, 0) }

func (p *Pair) CdrType() Type { return Type((p.cdr & 0x7fffffff) >> 24) }
func (p *Pair) SetCdr(t Type, data uint32) { p.cdr = uint32(t << 24) | data }
func (p *Pair) SetCdrNil() { p.SetCdr(t_pointer, 0) }

func (p *Pair) Set(carT Type, carData uint32, cdrT Type, cdrData uint32, inUse bool) {
    p.SetCar(carT, carData)
    p.SetCdr(cdrT, cdrData)
    if inUse {
        p.SetInUse()
    } else {
        p.ClrInUse()
    }
}

func (p *Pair) CarData() uint { return uint(p.car & 0xffffff) }
func (p *Pair) CdrData() uint { return uint(p.cdr & 0xffffff) }

func (p Pair) String() string {
    return fmt.Sprintf("<InUse:%t, GC:%t, A~%s, D~%s, A=%06x (%d), D=%06x (%d)>",
            p.InUse(), p.CarBeingTraced(),
            p.CarType().String()[2:], p.CdrType().String()[2:],
            p.CarData(), p.CarData(),
            p.CdrData(), p.CdrData())
}

const maxAddress = 0xffffff

type MachineData struct {
    memory [maxAddress]Pair
    exp Pair
    env Pair
    val Pair
    args Pair
    clink Pair

    pc uint
}

func main() {
    md := new(MachineData)

    md.memory[0].Set(t_if, 1, t_pointer, 0, true)

    md.memory[1].Set(t_fixnum, 100, t_pointer, 2, true)

    md.memory[2].Set(t_fixnum, 200, t_fixnum, 300, true)


    for {
        cur := md.memory[md.pc]
        switch cur.CarType() {
            case t_if:
                fmt.Printf("%06x: in t_if: %v\n", md.pc, cur)
            default:
                fmt.Printf("%06x: unknown (%v)\n", md.pc, cur)
        }
        md.pc += 1
        if md.pc > maxAddress {
            md.pc = 0
        }
    }
}
