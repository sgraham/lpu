(load "main.lisp")
(load "compile.lisp")
(load "gc.lisp")

; we know these warnings aren't a problem, so to avoid changing the code
; as distributed, just muffle the warning spam
(declaim (sb-ext:muffle-conditions style-warning))
(load "lisp-unit.lisp")
(declaim (sb-ext:unmuffle-conditions style-warning))

(load "main-test.lisp")
