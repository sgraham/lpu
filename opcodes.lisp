;
; This file is definition for opcodes for documentation purposes, but also
; for the assembler, disassembler, and simulator.
;
; the register and memory operations are specified with bit ranges from high to low.
;
; user registers are A, D, P, all 16 bits
; inaccessible registers: HEAP (12 bits), HS (1 bit), PC (14 bits), IR (8 bits)
;
; see also comments in gc.asm for layout of heap which corresponds to some constants here
;
; jty?
; locals or 16b fixed size stack at top of mem?
; helpers for simple nth (for locals in a list)
;   pseudo elt == cdr*N car
;   pseudo setf elt == cdr*N rplaca

(defvar *name-to-opcode* (make-hash-table))

(defclass opdata ()
  ((name
     :initarg :name
     :reader name)
   (opcode
     :initarg :opcode
     :reader opcode)
   (body
     :initarg :body
     :reader body)))

(defmacro op (opcode mnemonic &body body)
  `(setf (gethash ,mnemonic *name-to-opcode*)
         (make-instance 'opdata 
                        :name ,mnemonic
                        :opcode ,opcode
                        :body ',body)))

(defmacro pseudo (mnemonic &body body)
  )

(defmacro reg-getter (reg func width)
  `(defun ,func (high low)
     (assert (>= high low))
     (let ((start (- (1- ,width) high))
           (end (1+ (- (1- ,width) low))))
       (subseq ,reg start end))))

(defmacro reg-setter (reg func width)
  `(defun ,func (high low val)
     (assert (>= high low))
     (assert (= (length val) (1+ (- high low))))
     (let ((start (- (1- ,width) high))
           (end (1+ (- (1- ,width) low))))
       (setf (subseq ,reg start end) val))))

(defmacro defregister (name width getter setter)
 `(progn
    (defvar ,name (make-array ,width :element-type 'bit))
    (reg-getter ,name ,getter ,width)
    (reg-setter ,name ,setter ,width)))

(defregister reg-a 16 getA setA)
(defregister reg-d 16 getD setD)
(defregister reg-p 16 getP setP)
(defregister reg-heap 12 getHEAP setHEAP)
(defregister reg-hs 1 getHS setHS)
(defregister reg-pc 14 getPC setPC)
(defregister reg-ir 8 getIR setIR)

(defregister reg-mem (* 8 4 4096 2) getMEM setMEM) ; 8 bits, 4 bytes per cell, 4k cells, 2 hs
(assert (= (length reg-mem) 262144)) ; just to be sure since that's what the sram is

(defun bits-for-offset (offset)
  (assert (and (>= offset 0)
               (< offset 4)))
  (cond ((= offset 0) #*00)
        ((= offset 1) #*01)
        ((= offset 2) #*10)
        ((= offset 3) #*11)))

(defun bits-to-integer (bits)
  (reduce #'+ (let ((n 1))
                (reverse
                  (map 'vector
                       #'(lambda (x)
                           (let ((ret (* x n)))
                             (setq n (* n 2))
                             ret))
                       (reverse bits))))))

(defun readram (offset)
  "always indexed by P + byte offset + halfspace offset, and in 8 bit chunks"
  (let* ((addr (bits-to-integer (concatenate 'vector (getHS 0 0) (getP #xB #x0) (bits-for-offset offset))))
         (bit-addr-low (* addr 8))
         (bit-addr-high (+ bit-addr-low 7)))
   (getMEM bit-addr-high bit-addr-low)))

(defun writeram (offset val)
 "always indexed by P + byte offset + halfspace offset, and in 8 bit chunks"
  (assert (and (>= offset 0)
               (< offset 4)))
  (let* ((addr (bits-to-integer (concatenate 'vector (getHS 0 0) (getP #xB #x0) (bits-for-offset offset))))
         (bit-addr-low (* addr 8))
         (bit-addr-high (+ bit-addr-low 7)))
   (setMEM bit-addr-high bit-addr-low val)))


;(maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) *name-to-opcode*)

(op #b00000000 'nop)
(op #b00000001 'car
  (setA #x7 #x0 (readram 0))
  (setA #xF #x8 (readram 1))
(op #b00000010 'cdr
  (setD #x7 #x0 (readram 2))
  (setD #xF #x8 (readram 3))
;(op #b00000011 unassigned)
(op #b00000100 'cons)
(op #b00000101 'rplaca
  (writeram 0 (getA #x7 #x0))
  (writeram 1 (getA #xF #x8)))
(op #b00000110 'rplacd
  (writeram 2 (getD #x7 #x0))
  (writeram 3 (getD #xF #x8)))
;(op #b00000111 unassigned)

(op #b00001000 'shl4
  (setA #xF #x4 (getA #xB #x0))
  (setA #x3 #x0 (getZero 4)))
(op #b00001001 'shr4
  (setA #xB #x0 (getA #xF #x4))
  (setA #xF #xC (getZero 4)))
(op #b00001010 'shl12
  (setA #xF #xC (getA #x3 #x0))
  (setA #xB #x0 (getZero 12)))
(op #b00001011 'shr12
  (setA #x3 #x0 (getA #xF #xC))
  (setA #xF #x4 (getZero 12)))
(op #b00001100 'shl
  (setA #xF #x1 (getA #xE #x0))
  (setA #x0 #x0 (getZero 1)))
(op #b00001101 'shr
  (setA #xE #x0 (getA #xF #x1))
  (setA #xF #xF (getZero 1)))
(op #b00001110 'shlnum
  (setA #xB #x1 (getA #xA #x0))
  (setA #x0 #x0 (getZero 1)))
(op #b00001111 'shrnum
  (setA #xA #x0 (getA #xB #x1))
  (setA #xB #xB (getZero 1)))

(op #b00010000 'add)
(op #b00010001 'sub)
(op #b00010010 'xor)
(op #b00010011 'not)
(op #b00010100 'and)
(op #b00010101 'or)
(op #b00010110 'inc)
(op #b00010111 'dec)

(op #b00011000 'AtoD)
(op #b00011001 'DtoA)
(op #b00011010 'AtoP)
(op #b00011011 'PtoA)
(op #b00011100 'DtoP)
(op #b00011101 'PtoD)
(op #b00011110 'swapAD)
(op #b00011111 'swapAP)

;(op #b00100000 unassigned)
;(op #b00100001 unassigned)
;(op #b00100010 unassigned)
;(op #b00100011 unassigned)
;(op #b00100100 unassigned)
(op #b00100101 'putc)
(op #b00100110 'lt)
(op #b00100111 'j)

; GC helpers
(op #b00101000 'togglehs)
(op #b00101001 'resetheap)
(op #b00101010 'freecells)
;(op #b00101011 unassigned)
;(op #b00101100 unassigned)
;(op #b00101101 unassigned)
;(op #b00101110 unassigned)
;(op #b00101111 unassigned)


; possibly a shift l/r by +-8 and remove shl12, etc?
;(op #b0011xxxx unassigned)

; todo; some of jz0/jz1/jz-1 don't make sense and could be reused
; one is a nop, could reuse 00000000 opcode
(loop for i from -32 to 31 do
      (op (logior #b01000000 (logand i #b111111))
        (intern (concatenate 'string (symbol-name 'jz) (write-to-string i)))))

(loop for i from 0 to 63 do
      (op (logior #b10000000 i)
        (intern (concatenate 'string (symbol-name 'immlo) (write-to-string i)))))

(loop for i from 0 to 63 do
      (op (logior #b11000000 i)
        (intern (concatenate 'string (symbol-name 'immhi) (write-to-string i)))))

(pseudo load ; load both car and cdr
  car
  cdr)

(pseudo isbh ; top bit (broken heart) is set?
  shr12
  shl
  shr4)

(pseudo islist ; second-from-top bit (is ptr) is set?
  shl
  shr12
  shl
  shr4)

(pseudo isatom ; second-from-top bit (is ptr) is clear?
  shl
  shr12
  shl
  shr4
  not)

; todo; would be nice to put these in hardware a little more somehow
; walk proper list in P to a constant depth and get element to A
(pseudo elt0 car)
(pseudo elt1 cdr car)
(pseudo elt2 cdr cdr car)
(pseudo elt3 cdr cdr cdr car)
(pseudo elt4 cdr cdr cdr cdr car)
(pseudo elt5 cdr cdr cdr cdr cdr car)

; setf eltN x
(pseudo setl0 rplaca)
(pseudo setl1 cdr rplaca)
(pseudo setl2 cdr cdr rplaca)
(pseudo setl3 cdr cdr cdr rplaca)
(pseudo setl4 cdr cdr cdr cdr rplaca)
(pseudo setl5 cdr cdr cdr cdr cdr rplaca)

#|
00000000 nop
00000001 car
    A[7-0] = MEM[P[b-0]*4+0][7-0]
    A[f-8] = MEM[P[b-0]*4+1][7-0]
00000010 cdr
    D[7-0] = MEM[P[b-0]*4+2][7-0]
    D[f-8] = MEM[P[b-0]*4+3][7-0]
00000011 load
    A[7-0] = MEM[P[b-0]*4+0][7-0]
    A[f-8] = MEM[P[b-0]*4+1][7-0]
    D[7-0] = MEM[P[b-0]*4+2][7-0]
    D[f-8] = MEM[P[b-0]*4+3][7-0]
00000100 cons
    if HEAP == 0:
        MEM[GC1+2][7-0] = IP[7-0]
        MEM[GC1+3][d-8] = IP[d-8]
        IP[7-0] = MEM[GC1+0][7-0]
        IP[d-8] = MEM[GC1+1][d-8]
    else:
        HEAP = HEAP - 1
        MEM[HEAP*4+0][7-0] = 0
        MEM[HEAP*4+1][f-8] = 0x40
        MEM[HEAP*4+2][7-0] = P[7-0]
        MEM[HEAP*4+3][f-8] = P[f-8]
        P = HEAP
00000101 rplaca
    MEM[P[b-0]*4+0][7-0] = A[7-0]
    MEM[P[b-0]*4+1][7-0] = A[f-8]
00000110 rplacd
    MEM[P[b-0]*4+2][7-0] = D[7-0]
    MEM[P[b-0]*4+3][7-0] = D[f-8]
00000111 rplacb
    MEM[P[b-0]*4+0][7-0] = A[7-0]
    MEM[P[b-0]*4+1][7-0] = A[f-8]
    MEM[P[b-0]*4+2][7-0] = D[7-0]
    MEM[P[b-0]*4+3][7-0] = D[f-8]

00001000 shl4
    A[b-0] = A[b-0] << 4
00001001 shr4
    A[b-0] = A[b-0] >> 4
00001010 shl12
    A[b-0] = A[b-0] << 12
00001011 shr12
    A[b-0] = A[b-0] >> 12
00001100 shl1
    A[b-0] = A[b-0] << 1
00001101 shr1
    A[b-0] = A[b-0] >> 1
00001110 ?
00001111 ?

00010000 add
    A[b-0] = A[b-0] + D[b-0]
00010001 sub
    A[b-0] = A[b-0] - D[b-0]
00010010 xor
    A[b-0] = A[b-0] ^ D[b-0]
00010011 not
    A[b-0] = ~A[b-0]
00010100 and
    A[b-0] = A[b-0] & D[b-0]
00010101 or
    A[b-0] = A[b-0] | D[b-0]
00010110 inc
    A[b-0] = A[b-0] + 1
00010111 dec
    A[b-0] = A[b-0] - 1

00011000 AtoD
    D[f-0] = A[f-0]
00011001 DtoA
    A[f-0] = D[f-0]
00011010 AtoP
    P[f-0] = A[f-0]
00011011 PtoA
    A[f-0] = P[f-0]
00011100 DtoP
    P[f-0] = D[f-0]
00011101 PtoD
    D[f-0] = P[f-0]
00011110 swapAD
    tmp = A[f-0]
    A[f-0] = D[f-0]
    D[f-0] = tmp
00011111 swapAP
    tmp = A[f-0]
    A[f-0] = P[f-0]
    P[f-0] = tmp

00100000 ?
00100001 ?
00100010 ?
00100011 ?
00100100 ?
    A[f-0] = ~A[f-f]
00100100 putc
00100110 lt
00100111 j
    IP = A[b-0]*4

# GC helpers
00101000 togglehs
    HS = HS ^ 1
00101001 resetheap
    HEAP = cdr(GC1)
00101010 freecells
    A[b-0] = HEAP
    A[f-c] = 0                      # 0 == fixnum tag
00101011 ?
00101100 ?
00101101 ?
00101110 ?
00101111 ?

# possibly a shift l/r by +-8?
0011xxxx ?

01xxxxxx jz L
    # todo; sign extension
    IP = IP + INSTR[5-0]

10xxxxxx immlo N
    A[5-0] = INSTR[5-0]
    A[b-6] = 0

11xxxxxx immhi N
    A[b-6] = INSTR[5-0]


pseudo isbh     # top bit is set?
    shr12
    shl
    shr4

pseudo islist   # second-from-top bit is set?
    shl
    shr12
    shl
    shr4

pseudo isatom   # second-from-top bit is clear?
    shl
    shr12
    shl
    shr4
    not

pseudo elt0     # walk proper list to a constant depth
    car

pseudo elt1
    cdr
    car

pseudo elt2
    cdr
    cdr
    car

pseudo elt3
    cdr
    cdr
    cdr
    car

pseudo elt4
    cdr
    cdr
    cdr
    cdr
    car

pseudo elt5
    cdr
    cdr
    cdr
    cdr
    cdr
    car

pseudo elt6
    cdr
    cdr
    cdr
    cdr
    cdr
    cdr
    car

pseudo elt7
    cdr
    cdr
    cdr
    cdr
    cdr
    cdr
    cdr
    car

pseudo setl0
    rplaca

pseudo setl1
    cdr
    rplaca

pseudo setl2
    cdr
    cdr
    rplaca

pseudo setl3
    cdr
    cdr
    cdr
    rplaca

pseudo setl4
    cdr
    cdr
    cdr
    cdr
    rplaca

pseudo setl5
    cdr
    cdr
    cdr
    cdr
    cdr
    rplaca

pseudo setl6
    cdr
    cdr
    cdr
    cdr
    cdr
    cdr
    rplaca

pseudo setl7
    cdr
    cdr
    cdr
    cdr
    cdr
    cdr
    cdr
    rplaca
|#


; vim: set lispwords+=op,pseudo:
