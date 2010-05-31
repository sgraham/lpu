(use-package :lisp-unit)

(define-test simple-eval-no-compile
  (assert-equal 1 (seval (logior *type-self-eval-immed* 1)))
  (assert-equal 15 (prim-car (prim-cons (logior *type-self-eval-immed* 15)
                                        (logior *type-self-eval-immed* 99))))
  (assert-equal (prim-cdr (prim-cons (logior *type-self-eval-immed* 15)
                                     (logior *type-self-eval-immed* 99)))
                99)
  )


(define-test simple-compile
  (assert-equal 13 (scompile 13))
  (assert-equal #x8000 (scompile '()))
  (assert-equal #x8000 (scompile nil))
  (assert-equal 0 (scompile 0))
  )

(define-test simple-compile-and-pprint
  (assert-equal
    "INT 1"
    (spprint (scompile 1)))
  (assert-equal
    '("IF" ("INT 848" "(nil)"))
    (spprint (scompile '(if 848))))
  (assert-equal
    '("IF" ("INT 1" ("PTR" ("INT 2" "(nil)"))))
    (spprint (scompile '(if 1 2))))
  (assert-equal
    '("IF"
      ("INT 1"
       ("PTR"
        (("IF" ("INT 0" ("PTR" ("INT 99" ("PTR" ("INT 88" "(nil)"))))))
         ("PTR" ("INT 444" "(nil)"))))))
    (spprint (scompile '(if 1 (if 0 99 88) 444))))
  (assert-equal
    '("CALL" ("INT 3" ("PTR" ("INT 2" ("PTR" ("INT 1" "FUNCALL"))))))
    (spprint (scompile '(1 2 3))))
  (assert-equal
    '("CALL" ("INT 3" ("PTR" ("INT 4" "CONS"))))
    (spprint (scompile '(cons 3 4))))
  (assert-equal
    "SYMBOL = 'X'"
    (spprint (scompile ':x)))
  (assert-equal
    "SYMBOL = 'BLORP'"
    (spprint (scompile ':blORp)))
  (assert-equal
    '("CALL" ("SYMBOL = 'STUFF'" "CAR"))
    (spprint (scompile '(car :stuff))))
  (assert-equal
    '("CALL" ("SYMBOL = 'STUFF'" "CDR"))
    (spprint (scompile '(cdr :stuff))))
  (assert-equal
    '("CALL"
      ("SYMBOL = 'STUFF'" ("PTR" ("SYMBOL = 'THINGS'" "CONS"))))
    (spprint (scompile '(cons :stuff :things))))
  (assert-equal
    '("CALL"
      ("SYMBOL = 'STUFF'" ("PTR" ("SYMBOL = 'THINGS'" "RPLACA"))))
    (spprint (scompile '(rplaca :stuff :things))))
  (assert-equal
    '("CALL"
      ("SYMBOL = 'STUFF'" ("PTR" ("SYMBOL = 'THINGS'" "RPLACD"))))
    (spprint (scompile '(rplacd :stuff :things))))
  (assert-equal
    '("LAMBDA" ("VARIABLE @ 0,0" "(nil)"))
    (spprint (scompile '(lambda (a) a))))
  (assert-equal
    '("LAMBDA" ("VARIABLE @ 0,1" "(nil)"))
    (spprint (scompile '(lambda (a b) b))))
  (assert-equal
    '("LAMBDA"
         (("IF"
           ("VARIABLE @ 0,0" ("PTR" ("INT 4" ("PTR" ("VARIABLE @ 0,1" "(nil)"))))))
          "(nil)"))
    (spprint (scompile '(lambda (a b) (if a 4 b)))))

  ; todo; actual quote tests, rather than :sym
  )

(define-test variable-lookup
  (assert-equal
    (logior *type-variable* 0 0)
    (make-variable-ref 'a '((a))))
  (assert-equal
    (logior *type-variable* 0 0)
    (make-variable-ref 'a '((a b))))
  (assert-equal
    (logior *type-variable* 0 1)
    (make-variable-ref 'b '((a b))))
  (assert-equal
    (logior *type-variable* 0 3)
    (make-variable-ref 'd '((a b c d e))))
  (assert-equal
    (logior *type-variable* (ash 1 6) 1)
    (make-variable-ref 'e '((a b c) (d e f))))
  (assert-equal
    (logior *type-variable* (ash 1 6) 0)
    (make-variable-ref 'd '((a b c) (d e f))))
  (assert-equal
    (logior *type-variable* (ash 1 6) 2)
    (make-variable-ref 'f '((a b c) (d e f))))
  (assert-equal
    (logior *type-variable* (ash 0 6) 0)
    (make-variable-ref 'a '((a b c) (d e a))))
  (assert-equal
    (logior *type-variable* (ash 1 6) 2)
    (make-variable-ref 'a '(() (d e a))))
  ; todo; figure out conditions
  ;(assert-error
    ;'could-not-find-variable-in-environment
    ;(make-variable-ref 'f '(() (d e a))))
  )



(define-test eval-call-prim-funcs
  (assert-equal
    42
    (seval (scompile '(car '(42 99)))))
  (assert-equal
    99
    (seval (scompile '(car (cdr '(42 99))))))
  (assert-equal
    555
    (seval (scompile '(car (cons 555 585)))))
  (assert-equal
    585
    (seval (scompile '(cdr (cons 555 585)))))
  (assert-equal
    999
    (seval (scompile '(car (cdr (cons 555 (cons 999 nil)))))))
  (assert-equal
    999
    (seval (scompile '(car
                        ((lambda (a) a)
                         (cons 999 998))))))
  (assert-equal
    998
    (seval (scompile '(cdr
                        ((lambda (a) a)
                         (cons 999 998))))))
  (assert-equal
    75
    (seval (scompile '(car
                        ((lambda (a) (rplaca a 75))
                         (cons 999 998))))))
  (assert-equal
    74
    (seval (scompile '(cdr
                        ((lambda (a) (rplacd a 74))
                         (cons 999 998))))))
  (assert-equal
    52
    (seval (scompile '(+ 25 27))))
  (assert-equal
    #xf0f
    (seval (scompile '(land #xfff #xf0f))))
  (assert-equal
    #xf0f
    (seval (scompile '(lior #xf00 #x00f))))
  (assert-equal
    #xf00
    (seval (scompile '(lxor #xf0f #x00f))))
  (assert-equal
    #x0f0
    (seval (scompile '(lnot #xf0f))))
  (assert-equal
    45
    (seval (scompile '(inc 44))))
  (assert-equal
    8
    (seval (scompile '(srl 4))))
  (assert-equal
    1
    (seval (scompile '(srl #x800))))
  )

(define-test eval-with-funcall
  (assert-equal
    444
    (seval (scompile '((lambda () 444)))))
  (assert-equal
    532
    (seval (scompile '((lambda (a) a) 532))))
  (assert-equal
    532
    (seval (scompile '((lambda (a b) a) 532 88))))
  (assert-equal
    88
    (seval (scompile '((lambda (a b) b) 532 88))))
  (assert-equal
    532
    (seval (scompile '((lambda (a b c) a) 532 88 46))))
  (assert-equal
    88
    (seval (scompile '((lambda (a b c) b) 532 88 46))))
  (assert-equal
    46
    (seval (scompile '((lambda (a b c) c) 532 88 46))))
  (assert-equal
    46
    (seval (scompile '((lambda (a b c) c) 532 88 46 999)))) ; todo; too many should error i guess
  (assert-equal
    532
    (seval (scompile '((lambda (a b c) a) 532 88)))) ; todo; too few should error i guess
  (assert-equal
    999
    (seval (scompile '((lambda (a)
                         a)
                       (car (cons 999 998))))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GC tests
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *gc-func* #'host-gc)

(define-test gc-basic
  ; just a simple list
  (reset-machine)
  (assert-equal 0 *reg-cur-halfspace*)
  (let* ((c (prim-cons *prim-nil* *prim-nil*))
         (b (prim-cons c *prim-nil*))
         (a (prim-cons b *prim-nil*))
         (before (spprint a)))
    (assert-equal 5 *reg-alloc*)
    (setq *reg-val* a)
    (funcall *gc-func*)
    (setq a *reg-val*)
    (let ((after (spprint a)))
      (assert-equal before after)
      (assert-equal 1 *reg-cur-halfspace*)
      (assert-equal 5 *reg-alloc*))))


(define-test gc-loop-car
  ; circular a->b->a->...
  (reset-machine)
  (assert-equal 0 *reg-cur-halfspace*)
  (let* ((b (prim-cons *prim-nil* *prim-nil*))
         (a (prim-cons b *prim-nil*)))
    (assert-equal 4 *reg-alloc*)
    (prim-rplaca b a)
    (let ((before (spprint a)))
      (setq *reg-val* a)
      (funcall *gc-func*)
      (setq a *reg-val*)
      (let ((after (spprint a)))
        (assert-equal before after)
        (assert-equal 1 *reg-cur-halfspace*)
        (assert-equal 4 *reg-alloc*)))))

(define-test gc-loop-cdr
  ; circular a->b->c->a->...
  (reset-machine)
  (assert-equal 0 *reg-cur-halfspace*)
  (let* ((c (prim-cons *prim-nil* *prim-nil*))
         (b (prim-cons *prim-nil* c))
         (a (prim-cons *prim-nil* b)))
    (assert-equal 5 *reg-alloc*)
    (prim-rplacd c a)
    (let ((before (spprint a)))
      (setq *reg-val* a)
      (funcall *gc-func*)
      (setq a *reg-val*)
      (let ((after (spprint a)))
        (assert-equal before after)
        (assert-equal 1 *reg-cur-halfspace*)
        (assert-equal 5 *reg-alloc*)))))

(define-test gc-loop-both-and-two-collections
  ; a->b,nil
  ; b->a,c
  ; c->d,nil
  ; d->b,nil
  (reset-machine)
  (assert-equal 0 *reg-cur-halfspace*)
  (let* ((b (prim-cons *prim-nil* *prim-nil*))
         (a (prim-cons b *prim-nil*))
         (d (prim-cons b *prim-nil*))
         (c (prim-cons d *prim-nil*)))
    (prim-rplaca b a)
    (prim-rplacd b c)
    (let ((before (spprint a)))
      (setq *reg-exp* a)
      (funcall *gc-func*)
      (setq a *reg-exp*)
      (let ((after1 (spprint a)))
        (assert-equal before after1)
        (assert-equal 1 *reg-cur-halfspace*)
        (setq *reg-val* a)
        (funcall *gc-func*)
        (setq a *reg-val*)
        (let ((after2 (spprint a)))
          (assert-equal before after2)
          (assert-equal after1 after2)
          (assert-equal 0 *reg-cur-halfspace*))))))

(define-test gc-code
  (reset-machine)
  (assert-equal 0 *reg-cur-halfspace*)
  (let* ((x (scompile '((lambda (a b c) c) 532 88 46)))
         (before (spprint x)))
    (setq *reg-val* x)
    (funcall *gc-func*)
    (setq x *reg-val*)
    (let ((after (spprint x)))
      (assert-equal 1 *reg-cur-halfspace*)
      (assert-equal before after))))

(run-tests)
