;;; Copyright (c) 2006, Juan Jose Garcia Ripoll.
;;;
;;; ECL is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;;	See file '../Copyright' for full details.

;;;
;;; DESCRIPTION:
;;;
;;; This file uses a "Hellow world!" string which is in an another C
;;; file called hello_aux.c. Both hello.lisp and hello_aux.c are
;;; compiled and linked into either
;;;
;;;	1) a FASL file (see build_fasl.lisp)
;;;	2) a shared library (see build_dll.lisp)
;;;	3) or a standalone executable file. (build_exe.lisp)
;;;
;;; USE:
;;;
;;; Launch a copy of ECL and load this file in it
;;;
;;;	(load "readme.lisp")
;;;

(format t "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BUILDING A COMPOUND FASL FILE
;;;
")

;;;
;;; * We compile hello.lisp and hello_aux.c separately.
;;;
(compile-file "hello.lisp" :system-p t)

(c::compiler-cc "hello_aux.c" (compile-file-pathname "hello_aux.c" :type :object))

;;;
;;; * We combine both in a single FASL file
;;;

(defconstant +compound-fasl+ (compile-file-pathname "compound" :type :fasl))

(c::build-fasl +compound-fasl+
	       :lisp-files
	       (list (compile-file-pathname "hello.lisp" :type :object))
	       :ld-flags
	       (list (namestring (compile-file-pathname "hello_aux.c" :type :object))))

;;;
;;; * We load both files
;;;

(load +compound-fasl+)


(format t "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BUILDING A STANDALONE EXECUTABLE
;;;
")

;;
;; * Combine files in a standalone executable. We reuse the files
;;   from the previous example
;;

(defconstant +standalone-exe+ (compile-file-pathname "standalone" :type :program))

(c::build-program +standalone-exe+
		  :lisp-files
		  (list (compile-file-pathname "hello.lisp" :type :object))
		  :ld-flags
		  (list (namestring (compile-file-pathname "hello_aux.c" :type :object)))
		  :epilogue-code
		  '(si::quit))

;;
;; * Test the program
;;
(si::system (format nil "./~A" +standalone-exe+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLEAN UP
;;;

(delete-file (compile-file-pathname "hello.lisp" :type :object))
(delete-file (compile-file-pathname "hello_aux.c" :type :object))
(delete-file +compound-fasl+)
(delete-file +standalone-exe+)
