#|
Build and load this module with (compile-file "ecl.lsp" :load t)
|#
;;
;; With this other statement, we import the C function sin(), which
;; operates on IEEE doubles. Notice that we include the C header to
;; get the full declaration.
;;
(defun c-sin (x)
  (ffi:clines "#include <math.h>")
  (ffi:c-inline (x) (:double) :double "sin(#0)" :one-liner t))
;;
;; We now use this function and compare with the lisp version.
;;
(format t "~%Lisp sin:~t~d~%C sin:~t~d~%Difference:~t~d"
	(sin 1.0d0) (c-sin 1.0d0) (- (sin 1.0d0) (c-sin 1.0d0)))
