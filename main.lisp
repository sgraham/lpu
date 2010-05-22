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

(defparameter *type-is-ptr-mask*     #b1000000000000000)

(defparameter *type-self-eval-ptr*   #b1000000000000000)
(defparameter *type-if*              #b1001000000000000)
(defparameter *type-call*            #b1010000000000000)

(defparameter *type-self-eval-immed* #b0000000000000000)
(defparameter *type-car*             #b0001000000000000)
(defparameter *type-cdr*             #b0010000000000000)

(defparameter *type-mask*            #b1111000000000000)
(defparameter *data-mask*            #b0000111111111111)

(defparameter *cdr-mask*             #x0000ffff)
(defparameter *car-mask*             #xffff0000)


(defvar *reg-alloc* 0)
(defvar *memory* (make-array 4096))

(defun prim-list? (p) (and
                        (= (logand p *type-is-ptr-mask*) *type-is-ptr-mask*)
                        (not (= p *type-self-eval-ptr*)))) ; nil
(defun prim-atom? (p) (not (prim-list? p)))
(defun prim-if? (p) (= (logand p *type-mask*) *type-if*))
(defun prim-call? (p) (= (logand p *type-mask*) *type-call*))
(defun prim-car? (p) (= (logand p *type-mask*) *type-car*))
(defun prim-cdr? (p) (= (logand p *type-mask*) *type-cdr*))

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


(defun list->prim-list (L &key (set-end *type-self-eval-ptr*))
  (if (null L)
    set-end
    (prim-cons (car L) (list->prim-list (cdr L) :set-end set-end))))

;(sdot-and-view (list->prim-list '(1 2 3)))

(defun scompile (exp)
  "Convert regular lisp expression to simple expression to be evaluated by
  machine/seval"
  (if (atom exp)
    (cond ((typep exp 'fixnum)
           (logior *type-self-eval-immed* exp))
          ((null exp) *type-self-eval-ptr*)
          (t (error "unhandled case" exp)))
    (cond ((eq (car exp) 'if) (logior *type-if* (list->prim-list (mapcar #'scompile (cdr exp)))))
          ((eq (car exp) 'car) (logior *type-call* (prim-cons (scompile (cadr exp)) *type-car*)))
          ((eq (car exp) 'cdr) (logior *type-call* (prim-cons (scompile (cadr exp)) *type-cdr*)))
          (t (logior *type-call* (list->prim-list (mapcar #'scompile (cdr exp))
                                                  :set-end (scompile (car exp))))))))

;(sdot-and-view (scompile 99))
;(sdot-and-view (scompile '(if 1 2 3)))
;(sdot-and-view (scompile '(if 1 2)))
;(sdot-and-view (scompile '(88 9 4 8)))

;(sdot-and-view (scompile '(car 7)))

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
               (t (format nil "INT ~a" (prim-get-data p)))))
        ((prim-if? p)
         (format nil "IF (0x~x)" p))
        ((prim-call? p)
         (format nil "CALL (0x~x)" p))
        (t (format nil "UNK 0x~x" p))))

(defun node->dot (p)
  (if (prim-atom? p)
    (node-label p)
    (format nil "\"0x~x\" [ label = \"<addr> @ 0x~x | <car> ~a | <cdr> ~a\" shape = \"record\" ];~%"
            p
            p
            (if (prim-atom? (prim-car p))
              (node-label (prim-car p))
              (format nil "0x~x" (prim-car p)))
            (if (prim-atom? (prim-cdr p))
              (node-label (prim-cdr p))
              (format nil "0x~x" (prim-cdr p))))))

(defun node-connections (p)
  (let ((ret nil))
    (if (and (prim-list? p) (prim-list? (prim-car p)))
      (setq ret (cons (format nil "\"0x~x\":car -> \"0x~x\":addr;~%" p (prim-car p)) ret)))
    (if (and (prim-list? p) (prim-list? (prim-cdr p)))
      (setq ret (cons (format nil "\"0x~x\":cdr -> \"0x~x\":addr;~%" p (prim-cdr p)) ret)))
    (reduce (lambda (a b) (concatenate 'string a b)) ret :initial-value "")))

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


