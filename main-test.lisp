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
    '("CALL" ("INT 2" ("PTR" ("INT 3" ("PTR" ("INT 1" "FUNCALL"))))))
    (spprint (scompile '(1 2 3))))
  (assert-equal
    '("CALL" ("INT 3" ("PTR" ("INT 4" "CONS"))))
    (spprint (scompile '(cons 3 4))))
  (assert-equal
    "SYMBOL = 'X'"
    (spprint (scompile 'x)))
  (assert-equal
    "SYMBOL = 'BLORP'"
    (spprint (scompile 'blORp)))
  (assert-equal
    '("CALL" ("SYMBOL = 'STUFF'" "CAR"))
    (spprint (scompile '(car stuff))))
  (assert-equal
    '("CALL" ("SYMBOL = 'STUFF'" "CDR"))
    (spprint (scompile '(cdr stuff))))
  (assert-equal
    '("CALL"
      ("SYMBOL = 'STUFF'" ("PTR" ("SYMBOL = 'THINGS'" "CONS"))))
    (spprint (scompile '(cons stuff things))))
  (assert-equal
    '("CALL"
      ("SYMBOL = 'STUFF'" ("PTR" ("SYMBOL = 'THINGS'" "RPLACA"))))
    (spprint (scompile '(rplaca stuff things))))
  (assert-equal
    '("CALL"
      ("SYMBOL = 'STUFF'" ("PTR" ("SYMBOL = 'THINGS'" "RPLACD"))))
    (spprint (scompile '(rplacd stuff things))))
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
  )


(spprint (scompile '(1 2 3)))
(run-tests)
