;;;
;;;
;;; the garbage collector for the machine. this is written is user-space, so
;;; needs to be compiled and installed into the code space.
;;; 
;;; when the allocator runs out of space, it makes a call to the car of memory
;;; cell 2, with cell 0 being nil, and cell 1 being 't'
;;;
;;; so, we compile the gc, and rplaca the lambda into 2.
;;;
;;; of course, this code must be written very carefully because it can't do
;;; anything crazy, number one on the crazy-list being causing allocations.
;;;
;;;

; scanning pointer used during gc. todo; is it really a register? how is it accessed?
(defvar *reg-scan* 0)

(defun copy-to-new-halfspace (p)
  (print p)
  (if (prim-atom? p)
    ; if it's an atom don't do anything
    p
    (if (prim-broken-heart? (prim-car p))
      ; if it's a broken heart, return the forwarded address
      (logior (logand *type-mask* p) (prim-get-data (prim-car p)))
      ; otherwise, allocate in the new halfspace and install
      ; a broken heart in the old location
      (let ((ret (prim-cons (prim-car p) (prim-cdr p) :other 1)))
        (prim-rplaca p (logior *type-broken-heart* ret))
        ret))))

(defun host-gc ()
  "scan current half space and copy live data to the other half space

  roots are the 5 main machine registers, and memory slot 2, which points to
  the garbage collector code itself."

  (setf *reg-scan* 0)
  (setf *reg-alloc* 0)

  (setf *reg-exp* (copy-to-new-halfspace *reg-exp*))
  (setf *reg-val* (copy-to-new-halfspace *reg-val*))
  (setf *reg-env* (copy-to-new-halfspace *reg-env*))
  (setf *reg-args* (copy-to-new-halfspace *reg-args*))
  (setf *reg-clink* (copy-to-new-halfspace *reg-clink*))

  ;(handle-pointer 2)
  (tagbody
    :gc-next

    (print "reg-scan")
    (print *reg-scan*)
    (print "reg-alloc")
    (print *reg-alloc*)

    (if (= *reg-scan* *reg-alloc*)
      (go :gc-done))

    (prim-rplaca *reg-scan* (copy-to-new-halfspace (prim-car *reg-scan*)) :other 1)
    (prim-rplacd *reg-scan* (copy-to-new-halfspace (prim-cdr *reg-scan*)) :other 1)
    (incf *reg-scan*)
    (go :gc-next)

    :gc-done)

  (setf *reg-cur-halfspace* (logxor *reg-cur-halfspace* 1))
  )
