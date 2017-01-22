(in-package #:clasp-ffi.tests)

#|
(defun load-file (name)
  (let ((filename (format nil "/opt/common-lisp/lang/clasp/src/clasp/src/tests/fli/~A" name)))
    (format *debug-io* "~&*** Loading ~a ... " filename)
    (load filename)
    (format *debug-io* "done.~&")))

(defun load-clasp-ffi-tests ()
  (load-file "defpackage.lisp")
  (load-file "bindings.lisp")
  (load-file "funcall.lisp")
  )
|#

(defun run-clasp-ffi-tests ()
  (rt:do-tests))
