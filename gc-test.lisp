(use-package :lisp-unit)

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


(define-test gc-loop
  ; circular a->b->c->a->...
  (reset-machine)
  (assert-equal 0 *reg-cur-halfspace*)
  (let* ((c (prim-cons *prim-nil* *prim-nil*))
         (b (prim-cons c *prim-nil*))
         (a (prim-cons b *prim-nil*)))
    (assert-equal 5 *reg-alloc*)
    (prim-rplaca c a)
    (let ((before (spprint a)))
      (setq *reg-val* a)
      (funcall *gc-func*)
      (setq a *reg-val*)
      (let ((after (spprint a)))
        (assert-equal before after)
        (assert-equal 1 *reg-cur-halfspace*)
        (assert-equal 5 *reg-alloc*)))))



(run-tests)
