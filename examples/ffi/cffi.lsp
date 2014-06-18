#|
Build and load this module with (compile-file "cffi.lsp" :load t)
|#
;;
;; This toplevel statement notifies the compiler that we will
;; need this shared library at runtime. We do not need this
;; statement in windows.
;;
#-(or ming32 windows)
(cffi:load-foreign-library #+darwin "/usr/lib/libm.dylib"
			   #-darwin "/usr/lib/libm.so")
;;
;; With this other statement, we import the C function sin(),
;; which operates on IEEE doubles.
;;
(cffi:defcfun ("sin" c-sin) :double :double)
;;
;; We now use this function and compare with the lisp version.
;;
(format t "~%Lisp sin:~t~d~%C sin:~t~d~%Difference:~t~d"
	(sin 1.0d0) (c-sin 1.0d0) (- (sin 1.0d0) (c-sin 1.0d0)))
;;
;; The following also works: no declaration!
;;
(let ((c-cos (cffi:foreign-funcall "cos" :double 1.0d0 :double)))
   (format t "~%Lisp cos:~t~d~%C cos:~t~d~%Difference:~t~d"
	(sin 1.0d0) c-sin (- (sin 1.0d0) c-sin)))
