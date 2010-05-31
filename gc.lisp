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
