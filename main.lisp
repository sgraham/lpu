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
(defparameter *type-lambda*          #b1100000000000000) ; lambda is the source item that creates a
(defparameter *type-closure*         #b1101000000000000) ; closure when eval'd (which includes the env)

(defparameter *type-self-eval-immed* #b0000000000000000)
(defparameter *type-primcall*        #b0001000000000000)
(defparameter *type-car-call*        (logior *type-primcall* 1))
(defparameter *type-cdr-call*        (logior *type-primcall* 2))
(defparameter *type-rplaca-call*     (logior *type-primcall* 3))
(defparameter *type-rplacd-call*     (logior *type-primcall* 4))
(defparameter *type-cons-call*       (logior *type-primcall* 5))
(defparameter *type-fun-call*        (logior *type-primcall* 6))
(defparameter *type-symbol*          #b0010000000000000)

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
                        (not (prim-null? p))))
(defun prim-null? (p) (= p *type-self-eval-ptr*))
(defun prim-atom? (p) (not (prim-list? p)))
(defun prim-if? (p) (= (logand p *type-mask*) *type-if*))
(defun prim-lambda? (p) (= (logand p *type-mask*) *type-lambda*))
(defun prim-call? (p) (= (logand p *type-mask*) *type-call*))
(defun prim-car-call? (p) (= p *type-car-call*))
(defun prim-cdr-call? (p) (= p *type-cdr-call*))
(defun prim-rplaca-call? (p) (= p *type-rplaca-call*))
(defun prim-rplacd-call? (p) (= p *type-rplacd-call*))
(defun prim-cons-call? (p) (= p *type-cons-call*))
(defun prim-fun-call? (p) (= p *type-fun-call*))
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

(defun prim-rplacd (kons obj)
  (setf (elt *memory* (logand *data-mask* kons)) (logior (ash (prim-car kons) 16) obj)))

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

(defparameter *machine-logging* nil)
(defparameter *machine-single-step* nil)

; would be nice to make this part of the state def too, but I couldn't figure
; out how, at least without writing a full code walker for the whole machine
; function.
(defmacro STATE-DEBUG (name)
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

    ; ------------------------------------------
    :st-eval (STATE-DEBUG eval)
    (let ((exptype (logand *type-mask* *reg-exp*)))
      (cond ((= exptype *type-self-eval-immed*) (go :st-self))
            ((= exptype *type-self-eval-ptr*) (go :st-self))
            ((= exptype *type-symbol*) (go :st-self))
            ((= exptype *type-if*) (go :st-if1))
            ((= exptype *type-lambda*) (go :st-lambda))
            ;((= exptype *type-call*) (go :st-))
            (t (error "unhandled type in eval"))))


    ; ------------------------------------------
    :st-self (STATE-DEBUG self)
    (setq *reg-val* *reg-exp*)
    (go :st-return)


    ; ------------------------------------------
    :st-lambda (STATE-DEBUG lambda)
    (setq *reg-val* (logior *type-closure* (prim-cons *reg-exp* *reg-env*)))
    (go :st-return)


    ; ------------------------------------------
    :st-if1 (STATE-DEBUG if)
    ; save env, val (now the then/else), and return target (where
    ; we'll go after evaling the condition)
    (setq *reg-val* (prim-cdr *reg-exp*))
    (setq *reg-clink* (prim-cons *reg-env* *reg-clink*))
    (setq *reg-clink* (logior *type-retloc-if2* (prim-cons *reg-val* *reg-clink*)))

    ; set the expression to be evaluated to the condition, and
    ; 'recurse' (now that we've saved return info)
    (setq *reg-exp* (prim-car *reg-exp*))
    (go :st-eval)


    ; ------------------------------------------
    :st-if2 (STATE-DEBUG if2)
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


    ; ------------------------------------------
    :st-evcomb3 (STATE-DEBUG evcomb3)


    ; ------------------------------------------
    :st-return (STATE-DEBUG return)
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
  ;(sdot-and-view sexp)
  (run-machine)
  *reg-val*)

;(seval (scompile '((lambda () 1))))
;(seval (scompile '(a)))

;(sdot-and-view (scompile '(lambda () 4)))

(defun list->prim-list (L)
  (if (null L)
    *type-self-eval-ptr*
    (prim-cons (car L) (list->prim-list (cdr L)))))

(defun list->prim-call (L)
  (labels ((rplacd-last-as-call-func (prim-L func)
                                     (if (prim-null? (prim-cdr prim-L))
                                       (prim-rplacd prim-L func)
                                       (rplacd-last-as-call-func (prim-cdr prim-L) func))
                                     prim-L))
    (let* ((args (cdr L))
           (func (car L)))
      (rplacd-last-as-call-func (logior *type-call* (list->prim-list args))
                                func))))

;(sdot-and-view (list->prim-call `(,*type-car-call* ,(scompile 1) ,(scompile 'a))))

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
          ((eq (car exp) 'car) (list->prim-call `(,*type-car-call* ,(scompile (cadr exp)))))
          ((eq (car exp) 'cdr) (list->prim-call `(,*type-cdr-call* ,(scompile (cadr exp)))))
          ((eq (car exp) 'rplaca) (list->prim-call `(,*type-rplaca-call*
                                                 ,(scompile (cadr exp))
                                                 ,(scompile (caddr exp)))))
          ((eq (car exp) 'rplacd) (list->prim-call `(,*type-rplacd-call*
                                                 ,(scompile (cadr exp))
                                                 ,(scompile (caddr exp)))))
          ((eq (car exp) 'cons) (list->prim-call `(,*type-cons-call*
                                                    ,(scompile (cadr exp))
                                                    ,(scompile (caddr exp)))))
          ((eq (car exp) 'quote) (list->prim-list (mapcar #'scompile (cadr exp))))
          ((eq (car exp) 'lambda) (logior *type-lambda*
                                          (prim-cons (list->prim-list (mapcar #'scompile (cadr exp)))
                                                     (scompile (caddr exp)))))
          (t (list->prim-call `(,*type-fun-call*
                                 ,@(mapcar #'scompile (cdr exp))
                                 ,(scompile (car exp))))))))

;(sdot-and-view (scompile 99))
;(sdot-and-view (scompile '(if 1 2 3)))
;(sdot-and-view (scompile '(if 1 2)))
;(sdot-and-view (scompile '(88 9 4 8)))

;(sdot-and-view (scompile '(cons 3 4)))

;(sdot-and-view (scompile '(a 2 3)))

(defun get-attached-nodes (p &optional ret)
  (if (prim-list? p)
    (get-attached-nodes (prim-cdr p)
                        (get-attached-nodes (prim-car p) (cons p ret)))
    ret))

(defun node-label (p &optional (addr? t))
  (let ((addr (if addr? (format nil " (0x~x)" p) "")))
    (cond ((eq p *type-self-eval-ptr*)
           "(nil)")
          ((prim-atom? p)
           (cond ((prim-car-call? p) "CAR")
                 ((prim-cdr-call? p) "CDR")
                 ((prim-rplaca-call? p) "RPLACA")
                 ((prim-rplacd-call? p) "RPLACD")
                 ((prim-cons-call? p) "CONS")
                 ((prim-fun-call? p) "FUNCALL")
                 ((prim-symbol? p) (format nil "SYMBOL~a = '~a'" addr (prim-symbol-name (prim-get-data p))))
                 (t (format nil "INT ~a" (prim-get-data p)))))
          ((prim-if? p)
           (format nil "IF~a" addr))
          ((prim-lambda? p)
           (format nil "LAMBDA~a" addr))
          ((prim-call? p)
           (format nil "CALL~a" addr))
          (t (format nil "PTR~a" addr)))))

(defun node->dot (p)
  (if (prim-atom? p)
    (node-label p)
    (format nil "\"0x~x\" [ label = \"<addr> @ 0x~x | <car> ~a | <cdr> ~a\" shape = \"record\" ];~%"
            p
            p
            (node-label (prim-car p))
            (node-label (prim-cdr p)))))

(defun node-dot-connections (p)
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
           (connections (mapcar #'node-dot-connections nodelist)))
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


(defun spprint-node (p)
  (let ((self (format nil "~a" (node-label p nil))))
    (if (prim-atom? p)
      self
      `(,self (,(spprint-node (prim-car p))
                ,(spprint-node (prim-cdr p)))))))

(defun spprint (p &optional (output-stream nil))
  "convert simple machine expr into (sorta) readable string. for debugging and unit tests."
  (if output-stream
    (pprint (spprint-node p) output-stream)
    (spprint-node p)))
  
