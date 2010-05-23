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

perhaps use data bits in primitives to indicate other half-space?
hmm, wait. all those primitive functions can be one type that
dispatch on more bits in data so we dont' have to try to cram into
the 3 bits?

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

(defparameter *type-is-ptr-mask*     #b1000000000000000)

(defparameter *type-self-eval-ptr*   #b1000000000000000)
(defparameter *type-if*              #b1001000000000000)
(defparameter *type-call*            #b1010000000000000)
(defparameter *type-lambda*          #b1100000000000000)

(defparameter *type-self-eval-immed* #b0000000000000000)
(defparameter *type-car*             #b0001000000000000)
(defparameter *type-cdr*             #b0010000000000000)
(defparameter *type-rplaca*          #b0011000000000000)
(defparameter *type-rplacd*          #b0100000000000000)
(defparameter *type-cons*            #b0101000000000000)
(defparameter *type-symbol*          #b0110000000000000)

; these need to have the high-bit set because they're or'd into
; tag bits on a cons cell stored in clink
(defparameter *type-retloc-if2*      #b1001000000000000)
(defparameter *type-retloc-evcomb3*  #b1010000000000000)

(defparameter *type-mask*            #b1111000000000000)
(defparameter *data-mask*            #b0000111111111111)
(defparameter *data-and-ptr-mask*    #b1000111111111111)

(defparameter *cdr-mask*             #x0000ffff)
(defparameter *car-mask*             #xffff0000)


(defvar *reg-alloc* 0)
(defvar *memory* (make-array 4096))

(defun prim-list? (p) (and
                        (= (logand p *type-is-ptr-mask*) *type-is-ptr-mask*)
                        (not (= p *type-self-eval-ptr*)))) ; nil
(defun prim-atom? (p) (not (prim-list? p)))
(defun prim-if? (p) (= (logand p *type-mask*) *type-if*))
(defun prim-lambda? (p) (= (logand p *type-mask*) *type-lambda*))
(defun prim-call? (p) (= (logand p *type-mask*) *type-call*))
(defun prim-car? (p) (= (logand p *type-mask*) *type-car*))
(defun prim-cdr? (p) (= (logand p *type-mask*) *type-cdr*))
(defun prim-rplaca? (p) (= (logand p *type-mask*) *type-rplaca*))
(defun prim-rplacd? (p) (= (logand p *type-mask*) *type-rplacd*))
(defun prim-cons? (p) (= (logand p *type-mask*) *type-cons*))
(defun prim-symbol? (p) (= (logand p *type-mask*) *type-symbol*))

(defun prim-get-data (p) (logand p *data-mask*))

(defun prim-car-data (exp)
  (logand (prim-car exp) *data-mask*))
(defun prim-car (exp)
  (assert (not (prim-atom? exp)))
  (ash
    (logand
      (elt *memory* (logand exp *data-mask*))
      *car-mask*)
    -16))

(defun prim-cdr-data (exp)
  (logand (prim-cdr exp) *data-mask*))
(defun prim-cdr (exp)
  (assert (not (prim-atom? exp)))
  (logand
    (elt *memory* (logand exp *data-mask*))
    *cdr-mask*))

(defun prim-cons (kar kdr)
  (let ((loc *reg-alloc*))
    (setf (elt *memory* loc) (logior (ash kar 16) kdr))
    (incf *reg-alloc*)
    (logior *type-self-eval-ptr* loc)))


(defparameter *prim-intern-hash* (make-hash-table :test #'equal))
(defvar *prim-intern-count* 0)
(defun prim-intern (sym)
  (let ((symname (symbol-name sym)))
    (multiple-value-bind (value present) (gethash symname *prim-intern-hash*)
      (if present
        value
        (setf (gethash symname *prim-intern-hash*)
              (incf *prim-intern-count*))))))
(defun prim-symbol-name (sym)
  (maphash #'(lambda (k v)
               (if (= v sym)
                 (return-from prim-symbol-name k)))
           *prim-intern-hash*))

; relies on load order; prim-nil has to be address 0
(defvar *prim-nil* (prim-cons 0 0))
(defvar *prim-t* (prim-cons 0 0))

(defvar *reg-exp* 0)
(defvar *reg-val* 0)
(defvar *reg-env* 0)
(defvar *reg-args* 0)
(defvar *reg-clink* 0)

(defun print-machine-state ()
 (format t "~%    exp = 0x~x~%    val = 0x~x~%    env = 0x~x~%    args = 0x~x~%    clink = 0x~x~%"
  *reg-exp*
  *reg-val*
  *reg-env*
  *reg-args*
  *reg-clink*))

(defvar *machine-logging* nil)
(defvar *machine-single-step* nil)

; would be nice to make this part of the state def too, but I couldn't figure
; out how, at least without writing a full code walker for the whole machine
; function.
(defmacro mach-debug (name)
  `(progn
     (if *machine-logging*
       (progn
         (princ (symbol-name ',name))
         (print-machine-state)))
     (if *machine-single-step*
       (progn
         (princ "... ret to step")
         (force-output)
         (read-line)))))


(defun run-machine ()
  "implementation of the machine. written in an attempted 'hardware'y fashion
  with no hidden host language control mechanisms"

  (tagbody

    :st-eval (mach-debug eval)
    (let ((exptype (logand *type-mask* *reg-exp*)))
      (cond ((= exptype *type-self-eval-immed*) (go :st-self))
            ((= exptype *type-self-eval-ptr*) (go :st-self))
            ((= exptype *type-if*) (go :st-if1))
            ((= exptype *type-retloc-if2*) (go :st-if2))
            (t (error "unhandled type in eval"))))


    :st-self (mach-debug self)
    (setq *reg-val* *reg-exp*)
    (go :st-return)


    :st-if1 (mach-debug if)
    ; save env, val (now the then/else), and return target (where
    ; we'll go after evaling the condition)
    (setq *reg-val* (prim-cdr *reg-exp*))
    (setq *reg-clink* (prim-cons *reg-env* *reg-clink*))
    (setq *reg-clink* (logior *type-retloc-if2* (prim-cons *reg-val* *reg-clink*)))

    ; set the expression to be evaluated to the condition, and
    ; 'recurse' (now that we've saved return info)
    (setq *reg-exp* (prim-car *reg-exp*))
    (go :st-eval)


    :st-if2 (mach-debug if2)
    ; after we evaluate the condition, we 'return' here to continue
    ; and evaluate either the 'then' or the 'else' of an if.

    ; first, 'pop' val (into exp) and env from our stack
    (setq *reg-exp* (prim-car *reg-clink*))
    (setq *reg-clink* (prim-cdr *reg-clink*))

    (setq *reg-env* (prim-car *reg-clink*))
    (setq *reg-clink* (prim-cdr *reg-clink*))

    (if (= *reg-val* *type-self-eval-ptr*) ; nil
      ; if recursive evaluation was nil, then walk to the else
      ; condition (todo; pack the else into the same cell)
      ; and then evaluate it
      (progn
        (setq *reg-exp* (prim-cdr *reg-exp*))
        (setq *reg-exp* (prim-car *reg-exp*))
        (go :st-eval))
      ; otherwise if it was non-nil, then get the consequent and eval
      (progn
        (setq *reg-exp* (prim-car *reg-exp*))
        (go :st-eval)))


    :st-evcomb3 (mach-debug evcomb3)


    :st-return (mach-debug return)
    (let ((exptype (logand *type-mask* *reg-clink*)))
      (cond ((= exptype *type-retloc-if2*) (go :st-if2))
            ((= exptype *type-retloc-evcomb3*) (go :st-evcomb3))))
    ))

(defun seval (sexp)
  (setq *reg-exp* sexp)
  (setq *reg-val* 0)
  (setq *reg-env* 0)
  (setq *reg-args* 0)
  (setq *reg-clink* 0)
  (sdot-and-view sexp)
  (run-machine)
  *reg-val*)

;    (cond
;      ((eq (logand *type-mask* sexp) *type-self-eval-immed*)
;       (logand sexp *data-mask*))
;      ((eq (logand *type-mask* sexp) *type-if*)
;       (progn
;         (setq val (prim-cdr sexp))
;         (setq clink (prim-cons env clink))
;         (setq clink (prim-typed-cons :if2 val clink))
;         (setq sexp (prim-car sexp))))
;      (t nil))))

;(seval (scompile '(if 1 2)))

;(sdot-and-view (scompile '(lambda () 4)))

(defun list->prim-list (L &key (set-end *type-self-eval-ptr*))
  (if (null L)
    set-end
    (prim-cons (car L) (list->prim-list (cdr L) :set-end set-end))))

(defun scompile (exp)
  "Convert regular lisp expression to simple expression to be evaluated by
  machine/seval"

  (if (atom exp)
    (cond ((typep exp 'fixnum)
           (logior *type-self-eval-immed* exp))
          ((null exp) *type-self-eval-ptr*)
          ((typep exp 'symbol)
           (logior *type-symbol* (prim-intern exp)))
          (t (error "unhandled case")))
    (cond ((eq (car exp) 'if) (logior *type-if* (list->prim-list (mapcar #'scompile (cdr exp)))))
          ((eq (car exp) 'car) (logior *type-call* (prim-cons (scompile (cadr exp)) *type-car*)))
          ((eq (car exp) 'cdr) (logior *type-call* (prim-cons (scompile (cadr exp)) *type-cdr*)))
          ((eq (car exp) 'rplaca) (logior *type-call*
                                          (prim-cons (scompile (cadr exp))
                                                     (prim-cons (scompile (caddr exp))
                                                                *type-rplaca*))))
          ((eq (car exp) 'rplacd) (logior *type-call*
                                          (prim-cons (scompile (cadr exp))
                                                     (prim-cons (scompile (caddr exp))
                                                                *type-rplacd*))))
          ((eq (car exp) 'cons) (logior *type-call*
                                        (prim-cons (scompile (cadr exp))
                                                   (prim-cons (scompile (caddr exp))
                                                              *type-cons*))))
          ((eq (car exp) 'quote) (list->prim-list (mapcar #'scompile (cadr exp))))
          ((eq (car exp) 'lambda) (logior *type-lambda*
                                          (prim-cons (list->prim-list (mapcar #'scompile (cadr exp)))
                                                     (scompile (caddr exp)))))
          (t (logior *type-call* (list->prim-list (mapcar #'scompile (cdr exp))
                                                  :set-end (scompile (car exp))))))))

;(sdot-and-view (scompile 99))
;(sdot-and-view (scompile '(if 1 2 3)))
;(sdot-and-view (scompile '(if 1 2)))
;(sdot-and-view (scompile '(88 9 4 8)))

;(sdot-and-view (scompile '(cons 3 4)))

;(sdot-and-view (scompile '(lambda (a b c) (inc a b c))))

(defun spprint (sexp)
  "convert simple machine expr into tagged list format, mostly for debugging"
  (cond
    ((eq (logand *type-mask* sexp) *type-self-eval-immed*)
     `(:int ,(logand sexp *data-mask*)))
    ((eq sexp 0)
     '(:nil))
    ((eq (logand *type-mask* sexp) *type-if*)
     `(:if ,(spprint (prim-car sexp))
           ,(spprint (prim-car (prim-cdr sexp)))
           ,(spprint (prim-cdr (prim-cdr sexp)))))
    ((eq (logand *type-mask* sexp) *type-car*)
     `(:car ,(spprint (prim-car sexp))))
    ((eq (logand *type-mask* sexp) *type-cdr*)
     `(:cdr ,(spprint (prim-car sexp))))
    (t (error "unhandled case" sexp))))

(defun get-attached-nodes (p &optional ret)
  (if (prim-list? p)
    (get-attached-nodes (prim-cdr p)
                        (get-attached-nodes (prim-car p) (cons p ret)))
    ret))

(defun node-label (p)
  (cond ((eq p *type-self-eval-ptr*)
         "(nil)")
        ((prim-atom? p)
         (cond ((prim-car? p) "CAR")
               ((prim-cdr? p) "CDR")
               ((prim-rplaca? p) "RPLACA")
               ((prim-rplacd? p) "RPLACD")
               ((prim-cons? p) "CONS")
               ((prim-symbol? p) (format nil "SYMBOL (~a) = '~a'" p (prim-symbol-name (prim-get-data p))))
               (t (format nil "INT ~a" (prim-get-data p)))))
        ((prim-if? p)
         (format nil "IF (0x~x)" p))
        ((prim-lambda? p)
         (format nil "LAMBDA (0x~x)" p))
        ((prim-call? p)
         (format nil "CALL (0x~x)" p))
        (t (format nil "PTR 0x~x" p))))

(defun node->dot (p)
  (if (prim-atom? p)
    (node-label p)
    (format nil "\"0x~x\" [ label = \"<addr> @ 0x~x | <car> ~a | <cdr> ~a\" shape = \"record\" ];~%"
            p
            p
            (node-label (prim-car p))
            (node-label (prim-cdr p)))))

(defun node-connections (p)
  (let ((ret nil))
    (if (and (prim-list? p) (prim-list? (prim-car p)))
      (setq ret (cons (format nil "\"0x~x\":car -> \"0x~x\":addr;~%" p (prim-car p)) ret)))
    (if (and (prim-list? p) (prim-list? (prim-cdr p)))
      (setq ret (cons (format nil "\"0x~x\":cdr -> \"0x~x\":addr;~%" p (prim-cdr p)) ret)))
    (reduce #'(lambda (a b) (concatenate 'string a b)) ret :initial-value "")))

(defun sdot (sexp)
  "convert simple machine expr into graphviz .dot text format"
  (with-open-file (out "tmp.dot" :direction :output :if-exists :supersede)
    (format out "digraph nodes {~%")
    (format out "graph [~%rankdir = \"LR\"~%shape = \"record\"~%];~%")
    (let* ((nodelist (get-attached-nodes sexp))
           (nodes-as-string (mapcar #'node->dot nodelist))
           (connections (mapcar #'node-connections nodelist)))
      (loop for node in nodes-as-string do (princ node out))
      (loop for cnx in connections do (princ cnx out)))
    (format out "start [ label = \"~a\" ];~%" (node-label sexp))
    (if (prim-list? sexp)
      (format out "start -> \"0x~x\":addr;~%"  sexp))
    (format out "}~%")))

(defun sdot-and-view (sexp)
  "sdot and then run-program to generate and view png"
  (sdot sexp)
  (sb-ext:run-program "/bin/sh" '("-c" "dot -Tpng tmp.dot -otmp.png && gnome-open tmp.png")
                      :output t
                      :error :output))


