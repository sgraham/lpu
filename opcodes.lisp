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

(defvar *name-to-opdata* (make-hash-table))
(defvar *opcode-to-opdata* (make-hash-table))

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
  `(progn
     (let ((data (make-instance 'opdata 
                                :name ,mnemonic
                                :opcode ,opcode
                                :body '(progn ,@body))))
       (setf (gethash ,mnemonic *name-to-opdata*) data)
       (setf (gethash ,opcode *opcode-to-opdata*) data))))

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

(defun integer-to-bits (num width)
  (make-array width
              :initial-contents
              (reverse (loop repeat width
                             for i = 1 then (* i 2)
                             collecting (if (= i (logand i num)) 1 0)))))

(defun readram (offset)
  "always indexed by lower 12 of P + byte offset + halfspace offset, and in 8 bit chunks"
  (let* ((addr (bits-to-integer (concatenate 'vector (getHS 0 0) (getP #xB #x0) (bits-for-offset offset))))
         (bit-addr-low (* addr 8))
         (bit-addr-high (+ bit-addr-low 7)))
   (getMEM bit-addr-high bit-addr-low)))

(defun writeram (offset val)
 "always indexed by lower 12 of P + byte offset + halfspace offset, and in 8 bit chunks"
  (assert (and (>= offset 0)
               (< offset 4)))
  (let* ((addr (bits-to-integer (concatenate 'vector (getHS 0 0) (getP #xB #x0) (bits-for-offset offset))))
         (bit-addr-low (* addr 8))
         (bit-addr-high (+ bit-addr-low 7)))
   (setMEM bit-addr-high bit-addr-low val)))

(defun getZero (width) (make-array width :element-type 'bit :initial-element 0))

;(eval (body (gethash 'car *name-to-opdata*)))

;(maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) *name-to-opdata*)

; todo; is this a builtin? must be a better way to write at least
(defun flatten (l)
  (cond
    ((null l) nil)
    ((atom l) (list l))
    (t (concatenate 'list
                    (flatten (car l))
                    (flatten (cdr l))))))

(flatten '(1 2 (3 4) 3))

; todo; label extraction and alignment as a prepass
(defmacro lpuas (&body instrs)
  `(flatten (mapcar #'(lambda (x)
                        (if (listp x)
                          (cond ((eq (car x) 'imm)
                                 (list (opcode (gethash
                                                 (instrn 'immlo (logand (cadr x) #b111111))
                                                 *name-to-opdata*))
                                       (opcode (gethash
                                                 (instrn 'immhi (ash (logand (cadr x) #b111111000000) -6))
                                                 *name-to-opdata*))))
                                ((eq (car x) 'immlo)
                                 (opcode (gethash
                                           (instrn 'immlo (logand (cadr x) #b111111))
                                           *name-to-opdata*)))
                                ((eq (car x) 'immhi)
                                 (opcode (gethash
                                           (instrn 'immhi (ash (logand (cadr x) #b111111000000) -6))
                                           *name-to-opdata*))))
                          (opcode (gethash x *name-to-opdata*))))
                    ',instrs)))

(defun lpudis (instrs) (mapcar #'(lambda (x) (name (gethash x *opcode-to-opdata*))) instrs))

(defun lpurun (instrs)
  (mapc #'(lambda (x)
            (setIR #x7 #x0 (integer-to-bits x 8))
            (eval (body (gethash x *opcode-to-opdata*))))
        instrs)
  'halt)

(lpurun
  (lpuas
    (imm 72) ; H
    putc
    (imm 101) ; e
    putc
    (imm 108) ; l
    putc
    putc
    (imm 111) ; o
    putc
    (imm 33) ; !
    putc))


  

(op #b00000000 'nop)
(op #b00000001 'car
  (setA #x7 #x0 (readram 0))
  (setA #xF #x8 (readram 1)))
(op #b00000010 'cdr
  (setD #x7 #x0 (readram 2))
  (setD #xF #x8 (readram 3)))
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
(op #b00100101 'putc
  (princ (code-char (bits-to-integer (getA #x7 #x0)))))
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
(defun instrn (sym n)
  (intern (concatenate 'string (symbol-name sym) (write-to-string n))))

(loop for i from -32 to 31 do
      (op (logior #b01000000 (logand i #b111111))
       (instrn 'jz i)))

(loop for i from 0 to 63 do
      (op (logior #b10000000 i) (instrn 'immlo i)
       (setA #x5 #x0 (getIR #x5 #x0))
       (setA #xf #x6 (getZero 10))))

(loop for i from 0 to 63 do
      (op (logior #b11000000 i) (instrn 'immhi i)
        (setA #xb #x6 (getIR #x5 #x0))))

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


; vim: set lispwords+=op,pseudo:
