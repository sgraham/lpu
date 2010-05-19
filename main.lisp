#|

if
cons            alloc & store
car/cdr         load  a/d == low bit in address for offset
rplaca/rplacd   store
quote
lambda
<call>
? atom

probably need some more for gc to be written in code
cons-in-other-space, rplaca-in-other-space, etc.
plus flip space

registers:

exp: expression being evaluated
env: current environment
val: result of evaluation
args: evaluated arguments during call
clink: control stack for recursing 
alloc: current allocation location

32 bits cells is smallest unit of allocation
16 bit words are car/cdr
top 4 bits are type info with top-most == is pointer
remaining 12 bits allow address of 4k cells == 16k bytes
    32k sram -> halfspace, abusing the fact that relatively our ram is cheap & big and insanely fast


|#

(declaim (optimize (safety 3)) (optimize (debug 3)) (optimize (speed 0)))

(defparameter *type-self-eval-ptr*   #b0000000000000000)
(defparameter *type-if*              #b0001000000000000)
(defparameter *type-quote*           #b0010000000000000)

(defparameter *type-self-eval-immed* #b1000000000000000)

(defparameter *type-is-atom-mask*    #b1000000000000000)

(defparameter *type-mask*            #b1111000000000000)
(defparameter *data-mask*            #b0000111111111111)

(defparameter *cdr-mask*             #x0000ffff)
(defparameter *car-mask*             #xffff0000)


(defvar *reg-alloc* 0)
(defvar *memory* (make-array 4096))

(defun prim-atom? (exp)
  (not (= (logand exp *type-is-atom-mask*) 0)))

(defun prim-car (exp)
  (logand (prim-car-raw exp) *data-mask*))
(defun prim-car-raw (exp)
  (assert (not (prim-atom? exp)))
  (ash
    (logand
      (elt *memory* (logand exp *data-mask*))
      *car-mask*)
    -16))

(defun prim-cdr (exp)
  (logand (prim-cdr-raw exp) *data-mask*))

(defun prim-cdr-raw (exp)
  (assert (not (prim-atom? exp)))
  (logand
    (elt *memory* (logand exp *data-mask*))
    *cdr-mask*))

(defun prim-cons (kar kdr)
  (let ((loc *reg-alloc*))
    (setf (elt *memory* loc) (logior (ash kar 16) kdr))
    (incf *reg-alloc*)
    loc))

(defvar *prim-nil* (prim-cons 0 0))
(defvar *prim-t* (prim-cons 0 0))

(defun seval (sexp)
  (let ((val 0)
        (env 0)
        (args 0)
        (clink 0))
    (cond
      ((eq (logand *type-mask* sexp) *type-self-eval-immed*)
       (logand sexp *data-mask*))
      ((eq (logand *type-mask* sexp) *type-if*)
       (progn
         (setq val (prim-cdr sexp))
         (setq clink (prim-cons env clink))
         (setq clink (prim-typed-cons :if2 val clink))
         (setq sexp (prim-car sexp))))
      (t nil))))


(defun scompile (exp)
  "Convert regular lisp expression to simple expression to be evaluated by
  machine/seval"
  (if (atom exp)
    (cond ((typep exp 'fixnum)
           (logior *type-self-eval-immed* exp))
          ((null exp) *type-self-eval-ptr*))
    (cond ((eq (car exp) 'if)
           (let ((if-cond (cadr exp))
                 (if-then (caddr exp))
                 (if-else (cadddr exp)))
             (let ((tail (prim-cons
                           (scompile if-then)
                           (scompile if-else)))
                   (test (scompile if-cond)))
               (logior *type-if* (prim-cons test tail))))))))

(defun spprint (sexp)
  "convert simple machine expr into tagged list format, mostly for debugging"
  (cond
    ((eq (logand *type-mask* sexp) *type-self-eval-immed*)
     `(:int ,(logand sexp *data-mask*)))
    ((eq sexp 0)
     '(:nil))
    ((eq (logand *type-mask* sexp) *type-if*)
     `(:if ,(spprint (prim-car-raw sexp))
           ,(spprint (prim-car-raw (prim-cdr-raw sexp)))
           ,(spprint (prim-cdr-raw (prim-cdr-raw sexp)))))
    (t (error "unhandled case" sexp))))
   

