(use-package :lisp-unit)

(define-test simple-eval
  (assert-equal 1 (seval (logior *type-self-eval-immed* 1)))
  (assert-equal 15 (prim-car (prim-cons (logior *type-self-eval-immed* 15)
                                        (logior *type-self-eval-immed* 99))))
  (assert-equal (prim-cdr (prim-cons (logior *type-self-eval-immed* 15)
                                     (logior *type-self-eval-immed* 99)))
                99)
  )


(define-test simple-compile
  (assert-equal (+ #x8000 13) (scompile 13))
  (assert-equal 0 (scompile '()))
  (assert-equal 0 (scompile nil))
  (assert-equal #x8000 (scompile 0))
  )

(define-test simple-pprint
  (assert-equal
    '(:int 1)
    (spprint (scompile 1)))
  (assert-equal
    '(:IF (:INT 848) (:NIL) (:NIL))
    (spprint (scompile '(if 848))))
  (assert-equal
    '(:IF (:INT 1) (:INT 2) (:NIL))
    (spprint (scompile '(if 1 2))))
  )


(run-tests)
