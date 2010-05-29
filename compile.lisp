;;;
;;;
;;; must be loaded after main. compiles a normal lisp sexp to the tagged
;;; machine format. the sexps that represent the code are allocated in runtime
;;; that's in main.
;;;
;;;

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

(defun scompile-inner (exp env)
  (if (atom exp)
    (cond ((typep exp 'fixnum)
           (logior *type-self-eval-immed* exp))
          ((null exp) *type-self-eval-ptr*)
          ((typep exp 'symbol)
           (if (keywordp exp)
             (logior *type-symbol* (prim-intern exp))
             (make-variable-ref exp env)))
          (t (error "unhandled case")))
    (cond ((eq (car exp) 'if) (logior *type-if* (list->prim-list (mapcar #'(lambda (x) (scompile-inner x env)) (cdr exp)))))
          ((eq (car exp) 'car) (list->prim-call `(,*type-car-call* ,(scompile-inner (cadr exp) env))))
          ((eq (car exp) 'cdr) (list->prim-call `(,*type-cdr-call* ,(scompile-inner (cadr exp) env))))
          ((eq (car exp) 'rplaca) (list->prim-call `(,*type-rplaca-call*
                                                      ,(scompile-inner (cadr exp) env)
                                                      ,(scompile-inner (caddr exp) env))))
          ((eq (car exp) 'rplacd) (list->prim-call `(,*type-rplacd-call*
                                                      ,(scompile-inner (cadr exp) env)
                                                      ,(scompile-inner (caddr exp) env))))
          ((eq (car exp) 'cons) (list->prim-call `(,*type-cons-call*
                                                    ,(scompile-inner (cadr exp) env)
                                                    ,(scompile-inner (caddr exp) env))))
          ((eq (car exp) 'quote) (list->prim-list (mapcar #'(lambda (x) (scompile-inner x env)) (cadr exp))))
          ((eq (car exp) 'lambda) (logior *type-lambda*
                                          (prim-cons (scompile-inner (caddr exp) ; body
                                                                     (cons (cadr exp) env)) ; eval env
                                                     ; this is say, docstring, or numargs or something
                                                     *prim-nil*)))
          (t (list->prim-call `(,*type-fun-call*
                                 ,@(reverse (mapcar #'(lambda (x) (scompile-inner x env)) (cdr exp)))
                                 ,(scompile-inner (car exp) env)))))))

(defun scompile (exp)
  "Convert regular lisp expression to simple expression to be evaluated by
  machine/seval"
  (scompile-inner exp nil))
  
