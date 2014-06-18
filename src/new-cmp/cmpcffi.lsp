;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPCFFI --  Foreign functions interface for C/C++
;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; C/C++ DECLARATIONS AND HEADERS
;;;

(defun c1clines (destination args)
  (unless (every #'stringp args)
    (cmperr "The argument to CLINES, ~s, is not a list of strings." args))
  (setf *clines-string-list* (nconc *clines-string-list* (copy-list args)))
  (c1translate destination '(progn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; C/C++ INLINE CODE
;;;

(defun c1c-inline (destination args)
  ;; We are on the safe side by assuming that the form has side effects
  (destructuring-bind (arguments arg-types output-type c-expression
				 &rest rest
				 &key (side-effects t) one-liner
				 &aux output-rep-type)
      args
    (unless (= (length arguments) (length arg-types))
      (cmperr "In a C-INLINE form the number of declare arguments and the number of supplied ones do not match:~%~S"
	      `(C-INLINE ,@args)))
    ;; We cannot handle :cstrings as input arguments. :cstrings are
    ;; null-terminated strings, but not all of our lisp strings will
    ;; be null terminated. In particular, those with a fill pointer
    ;; will not.
    (let ((ndx (position :cstring arg-types)))
      (when ndx
	(let* ((var (gensym))
	       (value (elt arguments ndx)))
	  (setf (elt arguments ndx) var
		(elt arg-types ndx) :char*)
	  (return-from c1c-inline
	    (c1translate destination
	     `(ffi::with-cstring (,var ,value)
	       (c-inline ,arguments ,arg-types ,output-type ,c-expression
		,@rest)))))))
    ;; Find out the output types of the inline form. The syntax is rather relaxed
    ;; 	output-type = lisp-type | c-type | (values {lisp-type | c-type}*)
    (flet ((produce-type-pair (type)
	     (if (c-backend::lisp-type-p type)
		 (cons type (c-backend::lisp-type->rep-type type))
		 (cons (c-backend::rep-type->lisp-type type) type))))
      (cond ((eq output-type ':void)
	     (setf output-rep-type '()
		   output-type 'NIL))
	    ((equal output-type '(VALUES &REST t))
	     (setf output-rep-type '((VALUES &REST t))))
	    ((and (consp output-type) (eql (first output-type) 'VALUES))
	     (setf output-rep-type (mapcar #'cdr (mapcar #'produce-type-pair (rest output-type)))
		   output-type 'T))
	    (t
	     (let ((x (produce-type-pair output-type)))
	       (setf output-type (car x)
		     output-rep-type (list (cdr x)))))))
    (let* ((processed-arguments '()))
      (unless (and (listp arguments)
		   (listp arg-types)
		   (stringp c-expression))
	(cmperr "C-INLINE: wrong type of arguments ~S"
		arguments arg-types c-expression))
      (unless (= (length arguments)
                 (length arg-types))
        (cmperr "C-INLINE: mismatch between sizes of argument list and argument types."))
      (c1with-saved-values (prefix postfix temps arguments)
        (nconc prefix
               (c1c-inline-op output-type destination temps arg-types
                              output-rep-type c-expression side-effects
                              one-liner)
               postfix)))))
