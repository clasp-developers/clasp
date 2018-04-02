;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;                    package routines

(in-package "SYSTEM")

(defun find-all-symbols (string-or-symbol)
  "Args: (string-designator)
Returns a list of all symbols that have the specified print name.
STRING-DESIGNATOR may be a symbol, in which case the print name of the symbol
is used."
  (let ((symbol-name (string string-or-symbol)))
    (mapcan #'(lambda (p)
		(multiple-value-bind (s i)
		    (find-symbol symbol-name p)
		  (if (or (eq i :internal) (eq i :external))
		      (list s)
		      nil)))
	    (list-all-packages))))

(defun packages-iterator (packages options maybe-list)
  (let ((all-symbols nil))
    (when (or (atom packages) (not maybe-list))
      (setq packages (list packages)))
    (dolist (p packages)
      (let ((package (si::coerce-to-package p)))
	(multiple-value-bind (hash-ext hash-int packages-used)
	    (si::package-hash-tables package)
	  (when (member :external options)
	    (push (list package :external hash-ext) all-symbols))
	  (when (member :internal options)
	    (push (list package :internal hash-int) all-symbols))
	  (when (member :inherited options)
	    (dolist (p packages-used)
	      (push (list package :inherited (si::package-hash-tables p))
		    all-symbols))))))
    (unless all-symbols
      (return-from packages-iterator #'(lambda () (values nil nil nil nil))))
    (let* ((current (pop all-symbols))
	   (package (first current))
	   (type (second current))
	   (iterator (si::hash-table-iterator (third current))))
      (flet ((iterate ()
	       (tagbody
		AGAIN
		  (multiple-value-bind (found key value)
		      (funcall iterator)
		    (declare (ignore key))
		    (cond 
		      (found
		       (when (eq type :inherited)
			 (multiple-value-bind (s access)
			     (find-symbol (symbol-name value) package)
			   (unless (and (eq s value) (eq access type))
			     (go AGAIN))))
		       (return-from iterate (values t value type package)))
		      ((null all-symbols)
		       (return-from iterate (values nil nil nil nil)))
		      (t
		       (setq current (pop all-symbols))
		       (setq package (first current)
			     type (second current)
			     iterator (si::hash-table-iterator (third current))
			     ))))
		  (go AGAIN))))
	#'iterate))))

(defmacro with-package-iterator ((iterator package-list &rest conditions)
				 &rest body)
  (if conditions
      (let ((aux (set-difference conditions '(:external :internal :inherited))))
	(when aux
	  (signal-simple-error 'simple-program-error nil "Clauses ~{~S~} are not allowed."
			       (list aux))))
      (signal-simple-error 'simple-program-error
			   nil
			   "Must supply at least one of :inherited, :external or :internal"
			   nil))
  `(let ((,iterator (packages-iterator ,package-list ',conditions t)))
    (macrolet ((,iterator () (list 'funcall ',iterator)))
      ,@body)))

(defun expand-do-symbols (var package result-form body options)
  (let* ((i (gensym))
	 (found (gensym))
	 declaration doc)
    (multiple-value-setq (declaration body doc)
      (find-declarations body nil))
    `(do* ((,i (packages-iterator ,package ',options t))
	   ,found ,var)
	  (nil)
       ,@declaration
       (multiple-value-setq (,found ,var) (funcall ,i))
       (unless ,found (return ,result-form))
       ,@body)))

(defmacro do-symbols ((var &optional (package '*package*) (result-form nil))
                      &rest body)
  "Syntax: (do-symbols (var [package [result]])
          {decl}* {tag | statement}*)
Executes STATEMENTs once for each symbol in PACKAGE (which defaults to the
current package), with VAR bound to the symbol.  Then evaluates RESULT (which
defaults to NIL) and returns all values."
  (expand-do-symbols var package result-form body '(:inherited :internal :external)))

(defmacro do-external-symbols
          ((var &optional (package '*package*) (result-form nil)) &rest body)
  "Syntax: (do-external-symbols (var [package [result]])
          {decl}* {tag | statement}*)
Establishes a NIL block and executes STATEMENTs once for each external symbol
in PACKAGE (which defaults to the current package), with VAR bound to the
variable.  Then evaluates RESULT (which defaults to NIL) and returns all
values."
  (expand-do-symbols var package result-form body '(:external)))

(defmacro do-all-symbols ((var &optional (result-form nil)) &rest body)
  "Syntax: (do-all-symbols (var [result]) {decl}* {tag | statement}*)
Establishes a NIL block and executes STATEMENTs once for each symbol in each
package, with VAR bound to the symbol.  Then evaluates RESULT (which defaults
to NIL) and returns all values."
  (expand-do-symbols var '(list-all-packages) result-form body '(:internal :external)))

(defun print-symbol-apropos (symbol)
  (prin1 symbol)
  (when (fboundp symbol)
        (if (special-operator-p symbol)
            (princ "  Special form")
            (if (macro-function symbol)
                (princ "  Macro")
                (princ "  Function"))))
  (when (boundp symbol)
        (if (constantp symbol)
            (princ "  Constant")
            (princ "  has value")))
  (terpri))


(defun apropos (string &optional package)
  "Args: (string &optional (package nil))
Prints those symbols whose print-names contain STRING as substring.  If
PACKAGE is non-NIL, then only the specified PACKAGE is searched."
  (setq string (string string))
  (mapc #'print-symbol-apropos (apropos-list string package))
  (values))

;; apropos-list function from stassats June 28, 2015
(defun apropos-list (string &optional package)
  "Args: (string &optional (package nil))
Returns a list of all symbols whose print-names contain STRING as substring.
If PACKAGE is non-NIL, then only the specified PACKAGE is searched."
  ;; Remove duplicates, since it's sorted, this is faster than delete-duplicates
  (loop with previous = 0
        for x in (sort (apropos-list-inner string package)
                       #'string-lessp)
        if (not (eq previous x))
        collect (setf previous x)))

(defun apropos-list-inner (string package &optional seen-packages)
  (when (member package seen-packages) (return-from apropos-list-inner nil))
  (setf seen-packages (cons package seen-packages))
  (let* ((list '())
	 (string (string string)))
    (cond (package
	   (dolist (p (package-use-list package))
	     (setf list (nconc (apropos-list-inner string p seen-packages) list)))
	   (do-symbols (symbol package)
	     (when (search string (string symbol) :test #'char-equal)
	       (setq list (cons symbol list)))))
	  (t
	   (do-all-symbols (symbol)
	     (when (search string (string symbol) :test #'char-equal)
	       (setq list (cons symbol list))))))
    list))
