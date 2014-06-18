;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPMAP  Map functions.

(in-package "COMPILER")

(defun expand-mapcar (whole)
  (when (< (length whole) 3)
    (si::signal-simple-error
     #'program-error nil "Too few arguments to function ~A in form: ~A"
     (firt whole) whole))
  (let ((which (first whole)))
    (when (eq which 'FUNCALL)
      (setf whole (rest whole)
	    which (first whole))
      (when (consp which)
	(if (eq (first which) 'FUNCTION)
	    (setf which (second which))
	    (return-from expand-mapcar whole))))
    (let* ((function (second whole))
	   (args (cddr whole))
	   iterators for-statements
	   (in-or-on :IN)
	   (do-or-collect :COLLECT)
	   (list-1-form nil)
	   (finally-form nil))
      (case which
	(MAPCAR)
	(MAPLIST (setf in-or-on :ON))
	(MAPC (setf do-or-collect :DO))
	(MAPL (setf in-or-on :ON do-or-collect :DO))
	(MAPCAN (setf do-or-collect 'NCONC))
	(MAPCON (setf in-or-on :ON do-or-collect 'NCONC)))
      (when (eq do-or-collect :DO)
	(let ((var (gensym)))
	  (setf list-1-form `(with ,var = ,(first args))
		args (list* var (rest args))
		finally-form `(finally (return ,var)))))
      (loop for arg in (reverse args)
	    do (let ((var (gensym)))
		 (setf iterators (cons var iterators)
		       for-statements (list* :for var in-or-on arg for-statements))))
      `(loop ,@list-1-form
	     ,@for-statements
	     ,do-or-collect (funcall ,function ,@iterators)
	     ,@finally-form))))

(define-compiler-macro mapcar (&whole whole &rest r)
  (expand-mapcar whole))

(define-compiler-macro mapc (&whole whole &rest r)
  (expand-mapcar whole))

(define-compiler-macro mapcan (&whole whole &rest r)
  (expand-mapcar whole))

(define-compiler-macro maplist (&whole whole &rest r)
  (expand-mapcar whole))

(define-compiler-macro mapl (&whole whole &rest r)
  (expand-mapcar whole))

(define-compiler-macro mapcon (&whole whole &rest r)
  (expand-mapcar whole))
