;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; BYTECMP Fake compiler which is used as a replacement when we do not
;;;;         want or can have the real native compiler.

(in-package "EXT")

(defpackage "C"
  (:use "CL")
  (:export
   "*SUPPRESS-COMPILER-WARNINGS*"
   "*SUPPRESS-COMPILER-NOTES*"))

(defun bc-disassemble (thing)
  (when (si::valid-function-name-p thing)
    (setq thing (fdefinition thing)))
  (cond ((null thing))
	((functionp thing)
	 (si::bc-disassemble thing))
	((and (consp thing)
              (member (car thing) '(LAMBDA 'EXT:LAMBDA-BLOCK)))
	 (disassemble (compile nil thing)))
	(t
	 (error 'simple-type-error
		:datum thing
		:expected-type '(OR FUNCTION (SATISFIES SI:VALID-FUNCTION-NAME-P))
		:format-control "DISASSEMBLE cannot accept ~A"
		:format-arguments (list thing))))
  nil)

(defun bc-compile (name &optional (def nil supplied-p) &aux form)
 (cond ((and supplied-p def)
        (when (functionp def)
	  (unless (function-lambda-expression def)
            (return-from bc-compile (values def nil nil)))
	  (setf def (function-lambda-expression def)))
        (setq form (if name
		     `(progn (setf (symbol-function ',name) #',def) ',name)
		     `(setq GAZONK #',def))))
       ((not (fboundp name))
	(error "Symbol ~s is unbound." name))
       ((typep (setf def (symbol-function name)) 'standard-generic-function)
	(warn "COMPILE can not compile generic functions yet")
	(return-from bc-compile (values def t nil)))
       ((null (setq form (function-lambda-expression def)))
	(warn "We have lost the original function definition for ~s. Compilation failed"
              name)
	(return-from bc-compile (values def t nil)))
       (t
	(setq form `(progn (setf (symbol-function ',name) #',form) ',name))))
 (values (eval form) nil nil))

(defun bc-compile-file-pathname (name &key (output-file name) (type :fasl)
				 verbose print c-file h-file data-file
				 shared-data-file system-p load external-format)
  (declare (ignore load c-file h-file data-file shared-data-file system-p verbose print))
  (let ((extension "fasc"))
    (case type
      ((:fasl :fas) (setf extension "fasc"))
      (t (error "In COMPILE-FILE-PATHNAME, the type ~A is unsupported." type)))
    (make-pathname :type extension :defaults output-file)))

(defun bc-compile-file (input
			&key
			((:verbose *compile-verbose*) *compile-verbose*)
			((:print *compile-print*) *compile-print*)
			(load nil)
			(external-format :default)
			(output-file nil output-file-p)
			&allow-other-keys &aux foo)
  (setf output-file (if output-file-p
                        (pathname output-file)
                        (bc-compile-file-pathname input)))
  (when *compile-verbose*
    (format t "~&;;; Compiling ~A" input))
  (cond ((not (streamp input))
         (let* ((ext:*source-location* (cons (truename input) 0))
                (*compile-file-pathname* (pathname (merge-pathnames input)))
                (*compile-file-truename* (truename input)))
           (with-open-file (sin input :direction :input)
             (bc-compile-file sin :output-file output-file))))
        ((not output-file-p)
         (error "COMPILE-FILE invoked with a stream input and no :OUTPUT-FILE"))
        (t
         (with-open-file (sout output-file :direction :output :if-exists :supersede
                               :if-does-not-exist :create
			       :external-format external-format)
	   (let ((binary (loop with *package* = *package*
			    with ext:*bytecodes-compiler* = t
			    for position = (file-position input)
			    for form = (read input nil :EOF)
			    until (eq form :EOF)
			    do (when ext::*source-location*
				 (rplacd ext:*source-location* position))
			    collect (si:eval-with-env form nil nil nil nil))))
	     (sys:with-ecl-io-syntax
		 (write binary :stream sout :circle t :escape t :readably t :pretty nil))
	     (terpri sout)))))
  (when load
    (load output-file :verbose *compile-verbose*))
  (values output-file nil nil))

(defun install-bytecodes-compiler ()
  (ext::package-lock (find-package :cl) nil)
  (pushnew :ecl-bytecmp *features*)
  (setf (fdefinition 'disassemble) #'bc-disassemble
        (fdefinition 'compile) #'bc-compile
        (fdefinition 'compile-file) #'bc-compile-file
        (fdefinition 'compile-file-pathname) #'bc-compile-file-pathname)
  (ext::package-lock (find-package :cl) t))

(defun install-c-compiler ()
  (require :cmp)
  (locally (declare (notinline install-c-compiler))
    (funcall 'install-c-compiler)))

#-ecl-min
(progn
#+(and dlopen (not windows))
(sys::autoload "SYS:cmp" 'compile-file 'compile 'compile-file-pathname 'disassemble)
#-(and dlopen (not windows))
(install-bytecodes-compiler)
)

(provide 'BYTECMP)

#-ecl-min
(package-lock "COMMON-LISP" t)
