# words at top of memory for GC & setup. must be set up in both hs
# GC0 = temp for GC
# GC1 = GC addr, tempIP (starts as loc of GC func, replaced by return to code while in GC)
# GC2 = _start of code, top_of_heap (to allow for storage of globals & code)
# locals? = 32 bytes, 16 16 bit words used for local evaluation
# 
# +--------------------------------------+---------------+--------+-----+
# | end of heap     ...    start of heap | _start (code) | locals |GC012|
# +--------------------------------------+---------------+--------+-----+
# 
# initially HEAP = cdr(GC1) via 'resetheap' instr
# on first startup, have to dec heap twice so that there's room for the two
# temps at beginning of GC (if every cell is used and live when we do a GC we
# wouldn't have any room for those two temporaries otherwise)


# runs garbage collection, switches active halfspace, and clobbers P with a
# new allocation (based on assumption we're called from a failed cons). old
# P is the cdr of newly allocated cell per normal cons behaviour.
.align
.public
GC:
    # store A/D/P to other halfspace
    # this is the values of the registers, not what they point at (we don't
    # know if they're pointers or not yet), but we need somewhere to store
    # before we use them here to run the collection. also makes the scan loop
    # more uniform.

    resetheap       # set HEAP/SCAN to cdr(GC1)
    togglehs        # now new halfspace
    cons            # store (0, P)
    cons            # and allocate another
    store           # and save A,D

    # set P to beginning of heap == cdr(GC1)
    # can't get it during resetheap because haven't saved P yet
    imm GC1         # point at GC1
    AtoP
    cdr             # load value
    DtoP            # and put it into P which we use as the "scan" register for GC

.align              # align so we can jump from the bottom
next_cell:
    
    # SCAN == car of GC0. When SCAN catches up with HEAP we've walked the
    # whole queue.
    imm GC0
    car
    AtoD
    freecells       # HEAPtoA
    xor             # eq: if they're the same, it'll be all clear bits
    not             #     then not to make that true
    jz not_done_scan
    # todo; restore regs, return

not_done_scan:

    ##
    ## handle the two fields of all objects in turn, car then cdr
    ## 

    # first, check the CAR of SCAN
    imm GC0
    AtoP
    car             # A = car(P) from new halfspace
    isatom
    jz not_an_atom  # if the value's not a pointer, it's already in the new HS and there's nothing to do
    j done_car      # branch is too far for jnz
not_an_atom:

    # otherwise, the car is a list pointer, load what it points at
    car             # reload A = car(P) where P is the cell we're scanning
    AtoP            # move that pointer into P
    AtoD            # save off the pointer in D for later
    togglehs        # and load what it points to in the old HS
    car
    togglehs        # and toggle back to new
    # if the object is marked as being already moved in the old HS, then we
    # just want to replace the data portion of the car of it with the value
    # in the old HS (which is the pointer to the moved object in the new HS).
    isbh
    jz car_not_already_copied

    # replace data in car of P in new HS with data part of pointer in old HS
    # pointer
    DtoP            # load pointer to old obj saved above
    togglehs        # load broken heart pointer value from old HS
    car
    togglehs
    shl4            # remove type from value loaded from old HS
    shr4
    # A is now the pointer value we want to store into the car of SCAN, and
    # we want to or it with what's already there
    # with the type that's already there.
    AtoD            # save pointer value to D
    imm GC0         # load SCAN again
    AtoP
    car
    AtoP            # load the car of the value that's currently in SCAN
    car             # A is now the old pointer value, with type
    shl12           # drop data from current value, but keep type
    shr12
    or              # or D (the pointer value) into the type in A
    rplaca          # and store the whole thing back into what we're scanning

    j done_car

car_not_already_copied:

    # otherwise, we allocate a new cons in the new HS, and copy the cell of
    # data that P's car points to (not replacing any pointer values so that we
    # still have the pointers to broken hearts in the old HS).
    # (note, old obj ptr saved to D above)
    cons            # allocate the new cell                                     # PAD = NC,  x, OO
    PtoA            # save newly allocated cell in A                            # PAD = NC, NC, OO
    DtoP            # restore P to point to the object in the old HS            # PAD = OO, NC, OO
    togglehs        # load cdr of that obj from old HS                          
    cdr                                                                         # PAD = OO, NC, DD
    togglehs        # and store to new HS
    swapAP          # swap A and P, so P is the new cell and we have old in A   # PAD = NC, OO, DD
    rplacd          # write the old cdr to the new HS
    # now cdr is copied, save newly allocated in D
    PtoD                                                                        # PAD = NC, OO, NC
    AtoP            # point back at old cell to load car                        # PAD = OO, OO, NC
    togglehs
    car             # load car from old HS                                      # PAD = OO, AD, NC
    togglehs
    swapDP          # D now old, P is new to store into                         # PAD = NC, AD, OO
    rplaca          # store previous car into new cell
    PtoA            # save new pointer into A                                   # PAD = NC, NC, OO
    DtoP            # and restore P to the old halfspace scan value             # PAD = OO, NC, OO
    # (whew!)

    # now, P is old ptr, A is new ptr

    # then, stomp a broken heart (tag and pointer) into oldHS
    AtoD                                                                        # PAD = OO, NC, NC
    immlo 0x8
    or                                                                          # PAD = OO,TNC, NC
    togglehs
    rplaca          # store new ptr with broken heart type into old HS
    togglehs

    # and, finally, set the car to the newly allocated pointer, leaving
    # type the same
    AtoD            # save NC to D                                              # PAD = OO, NC, NC
    imm GC0         # load SCAN again to update the car we're scanning          # PAD = OO,pSC, NC
    AtoP                                                                        # PAD =pSC,pSC, NC
    car                                                                         # PAD =pSC,SCN, NC
    AtoP                                                                        # PAD =SCN,SCN, NC
    DtoA                                                                        # PAD =SCN, NC, NC
    rplacadata      # write NC ptr to SCANs car


.align
done_car:


    # now do all that for the CDR of SCAN. not quite identical because the ops
    # ops is cdr rather than car, so we have to save/move differently



    # we're done this cell, move down to the next one
    decP
    imm next_cell
    j
