(declaim (optimize (safety 3)) (optimize (debug 3)) (optimize (speed 0)))

(load "main.lisp")
(load "compile.lisp")
(load "gc.lisp")

; we know these warnings aren't a problem, so to avoid changing the code
; as distributed, just muffle the warning spam
(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(load "lisp-unit.lisp")
(declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))

(load "main-test.lisp")
(load "gc-test.lisp")
