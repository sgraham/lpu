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
(defparameter *type-car*             #b0011000000000000)
(defparameter *type-cdr*             #b0100000000000000)

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
          ((null exp) *type-self-eval-ptr*)
          (t (error "unhandled case" exp)))
    (cond ((eq (car exp) 'if)
           (let ((if-cond (cadr exp))
                 (if-then (caddr exp))
                 (if-else (cadddr exp)))
             (let ((tail (prim-cons
                           (scompile if-then)
                           (scompile if-else)))
                   (test (scompile if-cond)))
               (logior *type-if* (prim-cons test tail)))))
          ((eq (car exp) 'car)
           (logior *type-car* (scompile (cadr exp))))
          ((eq (car exp) 'cdr)
           (logior *type-cdr* (scompile (cadr exp))))
          ((eq (car exp) 'quote)
           (logior *type-self-eval-ptr* (scompile (cadr exp))))
          (t error "unhandled case" exp))))

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
    ((eq (logand *type-mask* sexp) *type-car*)
     `(:car ,(spprint (prim-car-raw sexp))))
    ((eq (logand *type-mask* sexp) *type-cdr*)
     `(:cdr ,(spprint (prim-car-raw sexp))))
    (t (error "unhandled case" sexp))))

(defun sdot (sexp)
  "convert simple machine expr into graphviz .dot text format"
  (with-open-file (out "tmp.dot" :direction :output :if-exists :supersede)
    (format out "digraph nodes {~%")
    (format out "graph [~%rankdir = \"LR\"~%shape = \"record\"~%];~%")
    (labels ((sdot-node (sexp)
                        (flet ((head (sexp)
                                     (format out "~x [~%" sexp))
                               (tail ()
                                     (format out "~%shape = \"record\"~%];~%")))
                          (head sexp)
                          (cond
                            ((eq (logand *type-mask* sexp) *type-self-eval-immed*)
                             (format out "label = \"<head> int ~d\"" (logand sexp *data-mask*)))
                            ((eq sexp 0)
                             (format out "label = \"<head> (nil)\""))
                            ((eq (logand *type-mask* sexp) *type-if*)
                             ; a bit of extra work for if because we walkt to the then-else too
                             (format out "label = \"<head> if ~x | <car> car = ~x | <cdr> cdr = ~x\""
                                     sexp
                                     (prim-car-raw sexp)
                                     (prim-cdr-raw sexp))
                             (tail)
                             (format out "~x:car -> ~x:head~%" sexp (prim-car-raw sexp))
                             (format out "~x:cdr -> ~x:head~%" sexp (prim-cdr-raw sexp))
                             (head (prim-cdr-raw sexp))
                             (let ((te-sexp (prim-cdr-raw sexp)))
                               (format out "label = \"<head> then-else ~x | <car> car = ~x | <cdr> cdr = ~x\""
                                       te-sexp
                                       (prim-car-raw te-sexp)
                                       (prim-cdr-raw te-sexp))
                               (tail)
                               (format out "~x:car -> ~x:head~%" te-sexp (prim-car-raw te-sexp))
                               (format out "~x:cdr -> ~x:head~%" te-sexp (prim-cdr-raw te-sexp)))
                             (sdot-node (prim-car-raw sexp))
                             (sdot-node (prim-car-raw (prim-cdr-raw sexp)))
                             (sdot-node (prim-cdr-raw (prim-cdr-raw sexp)))
                             (head sexp))
                            (t (error "unhandled case" sexp)))
                          (tail))))
      (sdot-node sexp))
    (format out "}~%")
    ))

(defun sdot-and-view (sexp)
  "sdot and then run-program to generate and view as png"
  (sdot sexp)
  (sb-ext:run-program "/bin/sh" '("-c" "dot -Tpng tmp.dot -otmp.png && gnome-open tmp.png")
                      :output t
                      :error :output))

(sdot-and-view (scompile 1))
(sdot-and-view (scompile '(if 1 2 3)))



#|
    FILE* f = fopen("tmp.dot", "wt");
    fprintf(f, "digraph nodes {\n");
    fprintf(f, "graph [\nrankdir = \"LR\"\n];\n");
    for (int i = StartOfMemory; i < sAllocPoint; ++i)
    {
        fprintf(f,
                "\"0x%x\" [\n  label = \"<addr> 0x%x (%s%s) | <car> car = ",
                i,
                i,
                IsMarked(i) ? "M" : "",
                IsCdrTraceInProgress(i) ? "T" : "");
        if (Car(i) == 0) fprintf(f, "(nil)");
        else fprintf(f, "0x%x", Car(i));
        fprintf(f, "| <cdr> cdr = ");
        if (Cdr(i) == 0) fprintf(f, "(nil)");
        else fprintf(f, "0x%x", Cdr(i));
        fprintf(f, "\"\n  shape = \"record\"\n];\n");
        if (Car(i) != 0)
            fprintf(f, "\"0x%x\":car -> \"0x%x\":addr;\n", i, Car(i));
        if (Cdr(i) != 0)
            fprintf(f, "\"0x%x\":cdr -> \"0x%x\":addr;\n", i, Cdr(i));
    }
    fprintf(f, "}\n");
    fclose(f);
    system("dot -Tpng tmp.dot -otmp.png && gnome-open tmp.png");
|#
