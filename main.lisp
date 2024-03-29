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

perhaps use data bits in primitives to indicate other halfspace?
hmm, wait. all those primitive functions can be one type that
dispatch on more bits in data so we dont' have to try to cram into
the 3 bits?

registers:

exp: expression being evaluated
env: current environment
val: result of evaluation
args: evaluated arguments during call
stack: control stack for recursing 
alloc: current allocation location

32 bits cells is smallest unit of allocation
16 bit words are car/cdr
top 4 bits are type info with top-most == is pointer
remaining 12 bits allow address of 4k cells == 16k bytes
    32k sram -> halfspace, abusing the fact that relatively our ram is cheap & big and insanely fast


|#

; we know these warnings aren't a problem, so to avoid changing the code
; as distributed, just muffle the warning spam
(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(load "lisp-unit.lisp")
(declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))

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
(defparameter *type-add-call*        (logior *type-primcall* 7))
(defparameter *type-inc-call*        (logior *type-primcall* 8))
(defparameter *type-and-call*        (logior *type-primcall* 9))
(defparameter *type-ior-call*        (logior *type-primcall* 10))
(defparameter *type-xor-call*        (logior *type-primcall* 11))
(defparameter *type-not-call*        (logior *type-primcall* 12))
(defparameter *type-srl-call*        (logior *type-primcall* 13))
(defparameter *type-zerop-call*      (logior *type-primcall* 14))
(defparameter *type-blit-call*       (logior *type-primcall* 15))
(defparameter *type-symbol*          #b0010000000000000)
(defparameter *type-variable*        #b0011000000000000)
(defparameter *type-broken-heart*    #b0100000000000000) ; todo; this is sort of a pointer, but not really as it has no actual type

; these need to have the high-bit set because they're or'd into
; tag bits on a cons cell stored in stack
(defparameter *type-retloc-if2*      #b1001000000000000)
(defparameter *type-retloc-call-3*   #b1010000000000000)

(defparameter *type-mask*            #b1111000000000000)
(defparameter *data-mask*            #b0000111111111111)
(defparameter *data-and-ptr-mask*    #b1000111111111111)

(defparameter *cdr-mask*             #x0000ffff)
(defparameter *car-mask*             #xffff0000)

;;;
;;;
;;; data for machine
;;;
;;;
(defvar *prim-nil* #xbad)
(defvar *prim-t* #xbad)

; for debugging, to make sure we're tracking all references properly
(defparameter *machine-gc-on-every-cons* nil)

; the current allocation pointer
(defvar *reg-alloc* 0)

; memory, split into two halfspaces
(defvar *reg-cur-halfspace* 0) ; only 1 bit
(defvar *memory* nil)

(defvar *reg-exp* 0)
(defvar *reg-val* 0)
(defvar *reg-env* 0)
(defvar *reg-args* 0)
(defvar *reg-stack* 0)

(defun print-machine-state ()
 (format t "~%    exp = 0x~x~%    val = 0x~x~%    env = 0x~x~%    args = 0x~x~%    stack = 0x~x~%"
  *reg-exp*
  *reg-val*
  *reg-env*
  *reg-args*
  *reg-stack*))

(defparameter *machine-logging* nil)
(defparameter *machine-single-step* nil)


(defun prim-cons (kar kdr &key (other 0))
  ; if our debug flag is set, and we're not allocating in the other halfspace
  ; (i.e. doing a collection, the trigger a collection here)
  (if (and *machine-gc-on-every-cons* (= other 0))
    (host-gc))
  (if (>= *reg-alloc* 4096)
    (error 'out-of-memory))
    ;(progn
      ;(assert (= other 0))
      ;(host-gc)))
  (let ((loc *reg-alloc*))
    (setf (elt (elt *memory* (logxor *reg-cur-halfspace* other)) loc) (logior (ash kar 16) kdr))
    (incf *reg-alloc*)
    (logior *type-self-eval-ptr* loc)))


(defun reset-machine ()
  (setq *memory* (make-array 2))
  (setf (elt *memory* 0) (make-array 4096))
  (setf (elt *memory* 1) (make-array 4096))
  (setq *reg-alloc* 0)
  (setq *reg-cur-halfspace* 0)

  ; must be in slot 0
  (setq *prim-nil* (prim-cons 0 0))
  (setq *prim-t* (prim-cons 0 0))

  (setq *reg-exp* *prim-nil*)
  (setq *reg-val* *prim-nil*)
  (setq *reg-env* *prim-nil*)
  (setq *reg-args* *prim-nil*)
  (setq *reg-stack* *prim-nil*))
(reset-machine)

(defun prim-null? (p) (= p *type-self-eval-ptr*))
(defun prim-list? (p) (and
                        (= (logand p *type-is-ptr-mask*) *type-is-ptr-mask*)
                        (not (prim-null? p))))
(defun prim-atom? (p) (not (prim-list? p)))
(defun prim-if? (p) (= (logand p *type-mask*) *type-if*))
(defun prim-lambda? (p) (= (logand p *type-mask*) *type-lambda*))
(defun prim-closure? (p) (= (logand p *type-mask*) *type-closure*))
(defun prim-call? (p) (= (logand p *type-mask*) *type-call*))
(defun prim-car-call? (p) (= p *type-car-call*))
(defun prim-cdr-call? (p) (= p *type-cdr-call*))
(defun prim-rplaca-call? (p) (= p *type-rplaca-call*))
(defun prim-rplacd-call? (p) (= p *type-rplacd-call*))
(defun prim-cons-call? (p) (= p *type-cons-call*))
(defun prim-fun-call? (p) (= p *type-fun-call*))
(defun prim-add-call? (p) (= p *type-add-call*))
(defun prim-inc-call? (p) (= p *type-inc-call*))
(defun prim-and-call? (p) (= p *type-and-call*))
(defun prim-ior-call? (p) (= p *type-ior-call*))
(defun prim-xor-call? (p) (= p *type-xor-call*))
(defun prim-not-call? (p) (= p *type-not-call*))
(defun prim-srl-call? (p) (= p *type-srl-call*))
(defun prim-zerop-call? (p) (= p *type-zerop-call*))
(defun prim-blit-call? (p) (= p *type-blit-call*))
(defun prim-symbol? (p) (= (logand p *type-mask*) *type-symbol*))
(defun prim-variable? (p) (= (logand p *type-mask*) *type-variable*))
(defun prim-broken-heart? (p) (= (logand p *type-mask*) *type-broken-heart*))

(defun prim-get-data (p) (logand p *data-mask*))

(defun prim-car (exp &key (other 0) (raw nil))
  (assert (or raw (not (prim-atom? exp))))
  (ash
    (logand
      (elt (elt *memory* (logxor *reg-cur-halfspace* other)) (logand exp *data-mask*))
      *car-mask*)
    -16))
(defun prim-car-data (exp)
  (logand (prim-car exp) *data-mask*))

(defun prim-cdr (exp &key (other 0) (raw nil))
  (assert (or raw (not (prim-atom? exp))))
  (logand
    (elt (elt *memory* (logxor *reg-cur-halfspace* other)) (logand exp *data-mask*))
    *cdr-mask*))
(defun prim-cdr-data (exp)
  (logand (prim-cdr exp) *data-mask*))


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

(defun prim-rplaca (kons obj &key (other 0))
  (setf (elt
          (elt *memory* (logxor *reg-cur-halfspace* other))
          (logand *data-mask* kons))
        (logior (ash obj 16) (prim-cdr kons :other other :raw t))))

(defun prim-rplacd (kons obj &key (other 0))
  (setf (elt
          (elt *memory* (logxor *reg-cur-halfspace* other))
          (logand *data-mask* kons))
        (logior (ash (prim-car kons :other other :raw t) 16) obj)))

;
; hardware alu, 12 bit no carry (yet?)
; 
(defun prim-add (v amt)
  (let ((ret (+ v amt)))
    (if (> ret #xfff)
      (- ret #x1000)
      ret)))
(defun prim-inc (v)
  (let ((ret (+ v 1)))
    (if (> ret #xfff)
      (- ret #x1000)
      ret)))
(defun prim-and (v with) (logand v with))
(defun prim-ior (v with) (logior v with))
(defun prim-xor (v with) (logxor v with))
(defun prim-not (v) (logand (lognot v) #xfff))
(defun prim-srl (v)
  "rotate 1 bit left"
  (logior (logand (ash v 1) #xfff)
          (ash (logand v #x800) -11)))
(defun prim-zerop (v)
  (if (= v 0)
    *prim-t*
    *prim-nil*))
(defun prim-blit ()
  (print "blitting"))

(defun node-label (p &optional (addr? t))
  "get a sensible name for a node"
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
                 ((prim-add-call? p) "+")
                 ((prim-inc-call? p) "INC")
                 ((prim-and-call? p) "LAND")
                 ((prim-ior-call? p) "LIOR")
                 ((prim-xor-call? p) "LXOR")
                 ((prim-not-call? p) "LNOT")
                 ((prim-srl-call? p) "SRL")
                 ((prim-zerop-call? p) "ZEROP")
                 ((prim-blit-call? p) "BLIT")
                 ((prim-variable? p) (format nil "VARIABLE @ ~a,~a" ; top six bits are where in chain, bottom 6 are index in bucket
                                             (ash (prim-get-data p) -6)
                                             (logand (prim-get-data p) #b111111)))
                 ((prim-symbol? p) (format nil "SYMBOL~a = '~a'" addr (prim-symbol-name (prim-get-data p))))
                 (t (format nil "INT ~a" (prim-get-data p)))))
          ((prim-if? p)
           (format nil "IF~a" addr))
          ((prim-lambda? p)
           (format nil "LAMBDA~a" addr))
          ((prim-closure? p)
           (format nil "CLOSURE~a" addr))
          ((prim-call? p)
           (format nil "CALL~a" addr))
          (t (format nil "PTR~a" addr)))))


;;;
;;; 
;;; converts a machine object to a graphviz .dot file for view/debugging
;;; 
;;;

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


(defun get-attached-nodes (p &optional ret)
  (if (prim-list? p)
    (get-attached-nodes (prim-cdr p)
                        (get-attached-nodes (prim-car p) (cons p ret)))
    ret))

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
  (run-program "/bin/sh" '("-c" "dot -Tpng tmp.dot -otmp.png && gnome-open tmp.png")
               :output t
               :error :output))

;;;
;;;
;;; another debugging method, outputs as semi-readable sexp. mostly used for
;;; unit tests
;;;
;;;
(defun spprint-node (p recname isrec visited)
  (let* ((have-vis (gethash p visited))
         (p-is-rec (gethash p isrec))
         (self (if have-vis
                 (format nil "#->~a" (write-to-string (gethash p recname)))
                 (format nil "~a~a"
                         (if p-is-rec
                           (concatenate 'string "#" (write-to-string (gethash p recname)) "=")
                           "")
                         (node-label p nil)))))
    (if (or (prim-atom? p) have-vis)
      self
      (progn
        (setf (gethash p visited) t)
        `(,self (,(spprint-node (prim-car p) recname isrec visited)
                  ,(spprint-node (prim-cdr p) recname isrec visited)))))))

(defvar *spprint-find-recursive-counter* 0)
(defun spprint-find-recursive (p name isrec)
  (let ((prev (gethash p name)))
    (if prev
      (setf (gethash p isrec) t)
      (if (prim-list? p)
        (progn
          (setf (gethash p name) (incf *spprint-find-recursive-counter*))
          (spprint-find-recursive (prim-car p) name isrec)
          (spprint-find-recursive (prim-cdr p) name isrec))))))

#|
(defun printhash (hash)
  (maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) hash))

(let* ((a (prim-cons *prim-nil* *prim-nil*))
       (b (prim-cons a *prim-nil*))
       (c (prim-cons b *prim-nil*))
       (name (make-hash-table))
       (isrec (make-hash-table)))
  (prim-rplaca a c)
  (setq *spprint-find-recursive-counter* 0)
  (spprint-find-recursive c name isrec)
  (printhash name)
  (printhash isrec)
  (print (spprint c)))
|#

(defun spprint (p &optional (output-stream nil))
  "convert simple machine expr into (sorta) readable string. for debugging and unit tests."
  (let ((recname (make-hash-table))
        (isrec (make-hash-table)))
    (setq *spprint-find-recursive-counter* 0)
    (spprint-find-recursive p recname isrec)
    (if output-stream
      (pprint (spprint-node p recname isrec (make-hash-table)) output-stream)
      (spprint-node p recname isrec (make-hash-table)))))

;;;
;;;
;;; helper macros to make run-machine a bit more readable, and inject debugging
;;; stuff from above
;;;
;;;
(defun generate-state-symbol (state)
  (intern (concatenate 'string "state-" (symbol-name state))))

(defun machine-state-debugging (state)
  `(progn
     (if *machine-logging*
       (progn
         (princ (symbol-name ',state))
         (print-machine-state)))
     (if *machine-single-step*
       (progn
         (princ "... ret to step, 'g' for graph from exp, 'v' for graph from val")
         (force-output)
         (let ((res (read-line)))
           (if (equal res "g")
             (sdot-and-view *reg-exp*)))))))

(defmacro state-machine (&body state-clauses)
  `(macrolet ((goto (state) `(go ,(generate-state-symbol state))))
     (tagbody 
       ,@(mapcan #'(lambda (clause)
                     (if (not (equal (car clause) 'state))
                       (error 'expecting-state-as-top-level-of-state-machine))
                     (let ((state-name (cadr clause)))
                       `(,(generate-state-symbol state-name)
                          ,(machine-state-debugging state-name)
                          ,@(cddr clause))))
                 state-clauses))))

  (macroexpand '(state-machine
     (state other
            (goto other))
     ))

    (seval (scompile '((lambda (mul)
                         (mul 9 8))
                       (lambda (a b)
                         ((lambda (x y-left total)
                            (if (zerop y-left)
                              total
                              (self x (+ y-left #xfff) (+ total x))))
                          a b 0)))))

;;;
;;;
;;; main machine, seval to set up and run
;;;
;;;
(defun run-machine ()
  "implementation of the machine. written in an attempted 'hardware'y fashion
  with no hidden host language control mechanisms or storage; only the five
  registers are used"

  (state-machine

    ; ------------------------------------------
    (state eval
           (let ((exptype (logand *type-mask* *reg-exp*)))
             (cond ((= exptype *type-self-eval-immed*) (goto self))
                   ((= exptype *type-self-eval-ptr*) (goto self))
                   ((= exptype *type-symbol*) (goto self))
                   ((= exptype *type-variable*) (goto variable))
                   ((= exptype *type-if*) (goto if1))
                   ((= exptype *type-lambda*) (goto lambda))
                   ((= exptype *type-call*) (goto call))
                   (t (error "unhandled type in eval")))))


    ; ------------------------------------------
    (state self
           (setq *reg-val* *reg-exp*)
           (goto return))


    ; ------------------------------------------
    (state variable
           (setq *reg-val* *reg-env*) ; set env to val for walking
           (goto walk-chain-to-bucket))


    ; ------------------------------------------
    (state walk-chain-to-bucket
           ; walk along the spine of the environment to find the right frame, at
           ; each step, decrementing the chain value in exp.
           ; jump to find-variable-in-bucket once we count down
           (if (= (logand #b111111000000 (prim-get-data *reg-exp*)) 0)
             (progn
               (setq *reg-val* (prim-car *reg-val*))
               (goto find-variable-in-bucket))
             (progn
               (setq *reg-val* (prim-cdr *reg-val*))
               (setq *reg-args* (logand #b000000111111 (prim-get-data *reg-exp*))) ; XXX TODO args used as temp. ok here?
               (setq *reg-exp* (prim-add (prim-get-data *reg-exp*) #xfc0)) ; there's no sub: 0xfc0 is (~64)+1 which is negation
               (setq *reg-exp* (logior *type-variable* *reg-exp* *reg-args*))
               (goto walk-chain-to-bucket))))

    ; ------------------------------------------
    (state find-variable-in-bucket
           ; now val is the list of variables in this bucket of the right frame
           ; walk down to get the right index in that list. we know the chain
           ; is always 0 if we get here, so no need to compare vs top bits.
           (if (= (prim-get-data *reg-exp*) 0)
             (progn
               (setq *reg-val* (prim-car *reg-val*))
               (goto return))
             (progn
               (setq *reg-val* (prim-cdr *reg-val*))
               (setq *reg-exp* (prim-add (prim-get-data *reg-exp*) #xfff)) ; there's no sub: 0xfff is -1
               (goto find-variable-in-bucket))))



    ; ------------------------------------------
    (state lambda
           ;(print "lambda->closure")
           ;(print (spprint *reg-exp*))
           ;(print (spprint *reg-env*))
           (setq *reg-val* (logior *type-closure* (prim-cons *reg-exp* *reg-env*)))
           (goto return))


    ; ------------------------------------------
    (state if1
           ; save env, val (now the then/else), and return target (where
           ; we'll go after evaling the condition)
           (setq *reg-val* (prim-cdr *reg-exp*))
           (setq *reg-stack* (prim-cons *reg-env* *reg-stack*))
           (setq *reg-stack* (logior *type-retloc-if2* (prim-cons *reg-val* *reg-stack*)))

           ; set the expression to be evaluated to the condition, and
           ; 'recurse' (now that we've saved return info)
           (setq *reg-exp* (prim-car *reg-exp*))
           (goto eval))


    ; ------------------------------------------
    (state if2
           ; after we evaluate the condition, we 'return' here to continue
           ; and evaluate either the 'then' or the 'else' of an if.

           ; first, 'pop' val (into exp) and env from our stack
           (setq *reg-exp* (prim-car *reg-stack*))
           (setq *reg-stack* (prim-cdr *reg-stack*))

           (setq *reg-env* (prim-car *reg-stack*))
           (setq *reg-stack* (prim-cdr *reg-stack*))

           (if (= *reg-val* *type-self-eval-ptr*) ; nil
             ; if recursive evaluation was nil, then walk to the else
             ; condition (todo; pack the else into the same cell)
             ; and then evaluate it
             (progn
               (setq *reg-exp* (prim-cdr *reg-exp*))
               (setq *reg-exp* (prim-car *reg-exp*))
               (goto eval))
             ; otherwise if it was non-nil, then get the consequent and eval
             (progn
               (setq *reg-exp* (prim-car *reg-exp*))
               (goto eval))))


    ; ------------------------------------------
    (state call
           ; initially, exp is the call expression that we want to evaluate. we want
           ; to build up the args register evaluating each of the arguments in turn.
           ; start by setting args to nil (the tail of the list)
           (setq *reg-args* *prim-nil*))

    ;; fall through


    ; ------------------------------------------
    (state call-1
           (let ((exptype (logand *type-mask* *reg-exp*)))
             (cond ((= exptype *type-primcall*)
                    (cond
                      ((= *reg-exp* *type-car-call*) (goto car))
                      ((= *reg-exp* *type-cdr-call*) (goto cdr))
                      ((= *reg-exp* *type-cons-call*) (goto cons))
                      ((= *reg-exp* *type-rplaca-call*) (goto rplaca))
                      ((= *reg-exp* *type-rplacd-call*) (goto rplacd))
                      ((= *reg-exp* *type-add-call*) (goto add))
                      ((= *reg-exp* *type-inc-call*) (goto inc))
                      ((= *reg-exp* *type-and-call*) (goto and))
                      ((= *reg-exp* *type-ior-call*) (goto ior))
                      ((= *reg-exp* *type-xor-call*) (goto xor))
                      ((= *reg-exp* *type-not-call*) (goto not))
                      ((= *reg-exp* *type-srl-call*) (goto srl))
                      ((= *reg-exp* *type-zerop-call*) (goto zerop))
                      ((= *reg-exp* *type-blit-call*) (goto blit))
                      ((= *reg-exp* *type-fun-call*) (goto fun-call))))
                   ((= exptype *type-call*) (goto call-2)))))

    ;; fall through if evaluating next argument


    ; ------------------------------------------
    (state call-2
           ; save the current environment, the current arguments
           ; that we've built up, and the next thing we're going
           ; to evaluate (using val as a temp).
           (setq *reg-stack* (prim-cons *reg-env* *reg-stack*))
           (setq *reg-stack* (prim-cons *reg-args* *reg-stack*))
           (setq *reg-val* (prim-cdr *reg-exp*))
           (setq *reg-stack* (logior *type-retloc-call-3* (prim-cons *reg-val* *reg-stack*)))

           ; then, set the thing to be evaluated to the argument
           ; we're working on and recurse
           (setq *reg-exp* (prim-car *reg-exp*))
           (goto eval))


    ; ------------------------------------------
    (state call-3
           ; when we're done evaluating the argument from above, we 'return' to here.
           ; we pop the current position in the parameters, the arguments and the
           ; environment off of the stack, add the just-evaluated result to args, and
           ; continue down the list of arguments
           (setq *reg-exp* (prim-car *reg-stack*))     ; pop remaining params (exp)
           (setq *reg-stack* (prim-cdr *reg-stack*))
           (setq *reg-args* (prim-car *reg-stack*))    ; pop saved earlier args
           (setq *reg-stack* (prim-cdr *reg-stack*))
           (setq *reg-env* (prim-car *reg-stack*))     ; pop saved environment
           (setq *reg-stack* (prim-cdr *reg-stack*))

           ; add the sub-evaluation to the list of args, and continue on
           (setq *reg-args* (prim-cons *reg-val* *reg-args*))
           (goto call-1))


    ; ------------------------------------------
    (state car
           (setq *reg-val* (prim-car *reg-val*))
           (goto return))


    ; ------------------------------------------
    (state cdr
           (setq *reg-val* (prim-cdr *reg-val*))
           (goto return))


    ; ------------------------------------------
    (state cons
           ; take cadr of args, and use the fact that we know the order of evaluation
           ; so val is already car of original args.
           (setq *reg-args* (prim-cdr *reg-args*))
           (setq *reg-args* (prim-car *reg-args*))
           (setq *reg-val* (prim-cons *reg-args* *reg-val*))
           (goto return))


    (state add
           (setq *reg-args* (prim-cdr *reg-args*))
           (setq *reg-args* (prim-car *reg-args*))
           (setq *reg-val* (prim-add *reg-args* *reg-val*))
           (goto return))
    (state inc
           (setq *reg-val* (prim-inc *reg-val*))
           (goto return))
    (state and
           (setq *reg-args* (prim-cdr *reg-args*))
           (setq *reg-args* (prim-car *reg-args*))
           (setq *reg-val* (prim-and *reg-args* *reg-val*))
           (goto return))
    (state ior
           (setq *reg-args* (prim-cdr *reg-args*))
           (setq *reg-args* (prim-car *reg-args*))
           (setq *reg-val* (prim-ior *reg-args* *reg-val*))
           (goto return))
    (state xor
           (setq *reg-args* (prim-cdr *reg-args*))
           (setq *reg-args* (prim-car *reg-args*))
           (setq *reg-val* (prim-xor *reg-args* *reg-val*))
           (goto return))
    (state not
           (setq *reg-val* (prim-not *reg-val*))
           (goto return))
    (state srl
           (setq *reg-val* (prim-srl *reg-val*))
           (goto return))
    (state zerop
           (setq *reg-val* (prim-zerop *reg-val*))
           (goto return))
    (state blit
           ; todo; something
           (setq *reg-val* 42)
           (goto return))

    ; ------------------------------------------
    (state rplaca
           ; same logic as cons, but rplaca
           (setq *reg-args* (prim-cdr *reg-args*))
           (setq *reg-args* (prim-car *reg-args*))
           (prim-rplaca *reg-args* *reg-val*)
           ; and set the evaluated value to the object we stomped
           (setq *reg-val* *reg-args*)
           (goto return))


    ; ------------------------------------------
    (state rplacd
           ; same logic as cons, but rplacd
           (setq *reg-args* (prim-cdr *reg-args*))
           (setq *reg-args* (prim-car *reg-args*))
           (prim-rplacd *reg-args* *reg-val*)
           ; and set the evaluated value to the object we stomped
           (setq *reg-val* *reg-args*)
           (goto return))


    ; ------------------------------------------
    (state fun-call
           ; args is all arguments to function, in reversed order
           ; val is already car of args, based on order of evaluation
           ; (which is the closure that we're calling)
           ;
           ; set args to the actual parameters to the function
           (setq *reg-args* (prim-cdr *reg-args*))
           ; and add the 'self' parameter to the front of the args (the original closure)
           (setq *reg-args* (prim-cons *reg-val* *reg-args*))

           ; car of closure is the original lambda
           (setq *reg-exp* (prim-car *reg-val*))


           ; car of the lambda is the body of the function, which is what we
           ; want to evaluate
           (setq *reg-exp* (prim-car *reg-exp*))

           ; the cdr of the closure is the environment. we cons the new arguments
           ; together with the closure's saved environment and store in env.
           (setq *reg-val* (prim-cdr *reg-val*))
           (setq *reg-env* (prim-cons *reg-args* *reg-val*))
           (goto eval))


    ; ------------------------------------------
    (state return
           (let ((exptype (logand *type-mask* *reg-stack*)))
             (cond ((= exptype *type-retloc-if2*) (goto if2))
                   ((= exptype *type-retloc-call-3*) (goto call-3)))))
    ))

(defun seval (sexp)
  (setq *reg-exp* sexp)
  (setq *reg-val* *prim-nil*)
  (setq *reg-env* (prim-cons (logior *type-symbol* (prim-intern 'END-OF-ENV)) *prim-nil*))
  (setq *reg-args* *prim-nil*)
  (setq *reg-stack* *prim-nil*)
  ;(sdot-and-view sexp)
  (run-machine)
  *reg-val*)


;;;;
;;;;
;;;;
;;;; compiler to the tagged machine representation
;;;;
;;;;
;;;;

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


; todo; make sure chain and pos fit in bits

(defun make-variable-ref-inner (var env chain)
  (let ((pos (position var (car env))))
    (if (null pos)
      (if (null (cdr env))
        (error 'could-not-find-variable-in-environment :var var)
        (make-variable-ref-inner var (cdr env) (+ chain 1)))
      (logior *type-variable* (ash chain 6) pos))))

(defun make-variable-ref (var env)
  (make-variable-ref-inner var env 0))
      
;(run-tests variable-lookup)

; todo should probably have some special business for 0 arg functions
;      blit is currently silly

(defparameter *one-arg-prim-funs* `((car ,*type-car-call*)
                                    (cdr ,*type-cdr-call*)
                                    (inc ,*type-inc-call*)
                                    (lnot ,*type-not-call*)
                                    (srl ,*type-srl-call*)
                                    (zerop ,*type-zerop-call*)))
(defparameter *two-arg-prim-funs* `((cons ,*type-cons-call*)
                                    (+ ,*type-add-call*)
                                    (land ,*type-and-call*)
                                    (lior ,*type-ior-call*)
                                    (lxor ,*type-xor-call*)
                                    (rplaca ,*type-rplaca-call*)
                                    (rplacd ,*type-rplacd-call*)))
(defun scompile-inner (exp env)
  (if (atom exp)
    (cond ((typep exp 'fixnum)
           (logior *type-self-eval-immed* (logand #xfff exp)))
          ((null exp) *type-self-eval-ptr*)
          ((typep exp 'symbol)
           (if (keywordp exp)
             (logior *type-symbol* (prim-intern exp))
             (make-variable-ref exp env)))
          (t (error "unhandled case")))
    (cond ((eq (car exp) 'if) (logior *type-if* (list->prim-list (mapcar #'(lambda (x) (scompile-inner x env)) (cdr exp)))))
          ((eq (car exp) 'blit)
           (list->prim-call `(,*type-blit-call*
                               ,*prim-nil*)))
          ((position (car exp) *one-arg-prim-funs* :key #'car)
           (list->prim-call `(,(second (find (car exp) *one-arg-prim-funs* :key #'car))
                               ,(scompile-inner (cadr exp) env))))
          ((position (car exp) *two-arg-prim-funs* :key #'car)
           (list->prim-call `(,(second (find (car exp) *two-arg-prim-funs* :key #'car))
                               ,(scompile-inner (cadr exp) env)
                               ,(scompile-inner (caddr exp) env))))
          ((eq (car exp) 'quote) (list->prim-list (mapcar #'(lambda (x) (scompile-inner x env)) (cadr exp))))
          ((eq (car exp) 'lambda) (logior *type-lambda*
                                          (prim-cons (scompile-inner (caddr exp) ; body
                                                                     (cons (cons 'self (cadr exp)) env)) ; eval env
                                                     ; this is say, docstring, or numargs or something
                                                     *prim-nil*)))
          (t (list->prim-call `(,*type-fun-call*
                                 ,@(reverse (mapcar #'(lambda (x) (scompile-inner x env)) (cdr exp)))
                                 ,(scompile-inner (car exp) env)))))))

(defun scompile (exp)
  "Convert regular lisp expression to simple expression to be evaluated by
  machine/seval"
  (scompile-inner exp nil))
  

;;;;
;;;;
;;;;
;;;; simple garbage collector, currently written using CL stuff (not directly
;;;; translatable to machine or user code)
;;;;
;;;;
;;;;
; scanning pointer used during gc. todo; is it really a register? how is it accessed?
(defvar *reg-scan* 0)

(defun copy-to-new-halfspace (p)
  ;(print "c-t-n-h")
  ;(print p)
  (let ((p-type (logand *type-mask* p)))
    (if (prim-atom? p)
      ; if it's an atom don't do anything
      p
      (let ((p-car (prim-car p)))
        ;(format nil "p=~x, car=~x~%" p p-car)
        (if (prim-broken-heart? p-car)
          ; if it's a broken heart, return the forwarded address
          (logior p-type (prim-get-data p-car))
          ; otherwise, allocate in the new halfspace and install
          ; a broken heart in the old location
          (let ((ret (prim-cons p-car (prim-cdr p) :other 1)))
            (prim-rplaca p (logior *type-broken-heart* (logand ret *data-mask*)))
            (logior p-type ret)))))))

(defun host-gc ()
  "scan current half space and copy live data to the other half space

  roots are the 5 main machine registers, and memory slot 2, which points to
  the garbage collector code itself."

  (setf *reg-scan* 2)
  (setf *reg-alloc* 2)

  (setf *reg-exp* (copy-to-new-halfspace *reg-exp*))
  (setf *reg-val* (copy-to-new-halfspace *reg-val*))
  (setf *reg-env* (copy-to-new-halfspace *reg-env*))
  (setf *reg-args* (copy-to-new-halfspace *reg-args*))
  (setf *reg-stack* (copy-to-new-halfspace *reg-stack*))

  ; copy registers if they're pointers
  ; while scan hasn't caught up with alloc
  ;     get value in car of scan in newHS = x
  ;     if x isn't ptr
  ;         done
  ;     get value at car of x in oldHS = y
  ;     if y is broken-heart,
  ;         replace data in car of scan in newHS with data in y
  ;     otherwise, alloc and copy whole x cell to newHS (not changing any ptr values)
  ;         stomp broken-heart into car(x) in oldHS
  ;         set car of scan to newlyallocated with same type as previously
  ;
  ;     repeat same for cdr
  ;
  ;     increment scan

  ;(print "GC")

  ;(handle-pointer 2)
  (tagbody
    :gc-next

    (if (= *reg-scan* *reg-alloc*)
      (go :gc-done))

    (let ((carval (prim-car *reg-scan* :other 1 :raw t)))
      (if (= (logand carval *type-is-ptr-mask*)
             *type-is-ptr-mask*)
        (prim-rplaca *reg-scan* (copy-to-new-halfspace carval) :other 1)))
    (let ((cdrval (prim-cdr *reg-scan* :other 1 :raw t)))
      (if (= (logand cdrval *type-is-ptr-mask*)
             *type-is-ptr-mask*)
        (prim-rplacd *reg-scan* (copy-to-new-halfspace cdrval) :other 1)))

    (incf *reg-scan*)
    (go :gc-next)

    :gc-done)

  (setf *reg-cur-halfspace* (logxor *reg-cur-halfspace* 1))
  )
