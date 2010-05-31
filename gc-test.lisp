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

(run-tests)
