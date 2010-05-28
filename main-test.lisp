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


(run-tests)
