;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: (DEFPACKAGE :COLON-MODE :EXTERNAL) -*-
;;;
;;;				 THE BOEING COMPANY
;;;			      BOEING COMPUTER SERVICES
;;;			       RESEARCH AND TECHNOLOGY
;;;				  COMPUTER SCIENCE
;;;			      P.O. BOX 24346, MS 7L-64
;;;			       SEATTLE, WA 98124-0346
;;;
;;;
;;; Copyright (c) 1990, 1991 The Boeing Company, All Rights Reserved.
;;;
;;; Permission is granted to any individual or institution to use,
;;; copy, modify, and distribute this software, provided that this
;;; complete copyright and permission notice is maintained, intact, in
;;; all copies and supporting documentation and that modifications are
;;; appropriately documented with date, author and description of the
;;; change.
;;;
;;; Stephen L. Nicoud (snicoud@boeing.com) provides this software "as
;;; is" without express or implied warranty by him or The Boeing
;;; Company.
;;;
;;; This software is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor accepts
;;; responsibility to anyone for the consequences of using it or for
;;; whether it serves any particular purpose or works at all.
;;;
;;;	Author:	Stephen L. Nicoud
;;;
;;; -----------------------------------------------------------------
;;;
;;;	Adapted for ECL by Giuseppe Attardi, 6/6/1994.
;;; 
;;; -----------------------------------------------------------------

(in-package "SYSTEM")

(defmacro DEFPACKAGE (name &rest options)
  (declare (type (or symbol string character) name))
  "DEFPACKAGE - DEFINED-PACKAGE-NAME {OPTION}*			[Macro]

   This creates a new package, or modifies an existing one, whose name is
   DEFINED-PACKAGE-NAME.  The DEFINED-PACKAGE-NAME may be a string or a 
   symbol; if it is a symbol, only its print name matters, and not what
   package, if any, the symbol happens to be in.  The newly created or 
   modified package is returned as the value of the DEFPACKAGE form.

   Each standard OPTION is a list of keyword (the name of the option)
   and associated arguments.  No part of a DEFPACKAGE form is evaluated.
   Except for the :SIZE and :DOCUMENTATION options, more than one option 
   of the same kind may occur within the same DEFPACKAGE form.

  Valid Options:
	(:documentation		string)
	(:size			integer)
	(:nicknames		{package-name}*)
	(:shadow		{symbol-name}*)
	(:shadowing-import-from	package-name {symbol-name}*)
	(:use			{package-name}*)
	(:import-from		package-name {symbol-name}*)
	(:intern		{symbol-name}*)
	(:export		{symbol-name}*)
	(:export-from		{package-name}*)

  [Note: :EXPORT-FROM is an extension to DEFPACKAGE.
	 If a symbol is interned in the package being created and
	 if a symbol with the same print name appears as an external
	 symbol of one of the packages in the :EXPORT-FROM option,
	 then the symbol is exported from the package being created.

	 :SIZE is used only in Genera and Allegro.]"

  (dolist (option options)
    (unless (member (first option)
		    '(:DOCUMENTATION :SIZE :NICKNAMES :SHADOW
		      :SHADOWING-IMPORT-FROM :USE :IMPORT-FROM :INTERN :EXPORT
		      :EXPORT-FROM) :test #'eq)
      (cerror "Proceed, ignoring this option."
	      "~s is not a valid DEFPACKAGE option." option)))
  (labels ((to-string (x) (if (numberp x) x (string x)))
	   (option-values-list (option options &aux output)
	     (dolist (o options)
	       (let ((o-option (first o)))
		 (when (string= o-option option)
		   (let* ((o-package (string (second o)))
			  (former-symbols (assoc o-package output))
			  (o-symbols (union (mapcar #'to-string (cddr o))
					    (cdr former-symbols)
					    :test #'equal)))
		     (if former-symbols
		       (setf (cdr former-symbols) o-symbols)
		       (setq output (acons o-package o-symbols output)))))))
	     output)
	   (option-values (option options &aux output)
	     (dolist (o options)
	       (let* ((o-option (first o))
		      (o-symbols (mapcar #'to-string (cdr o))))
		 (when (string= o-option option)
		   (setq output (union o-symbols output :test #'equal)))))
	     output))
    (dolist (option '(:SIZE :DOCUMENTATION))
      (when (<= 2 (count option options ':key #'car))
	(si::simple-program-error "DEFPACKAGE option ~s specified more than once."
				  option)))
    (setq name (string name))
    (let* ((nicknames (option-values ':nicknames options))
	   (documentation (option-values ':documentation options))
	   (shadowed-symbol-names (option-values ':shadow options))
	   (interned-symbol-names (option-values ':intern options))
	   (exported-symbol-names (option-values ':export options))
	   (shadowing-imported-from-symbol-names-list
	    (option-values-list ':shadowing-import-from options))
	   (imported-from-symbol-names-list
	    (option-values-list ':import-from options))
	   (exported-from-package-names (option-values ':export-from options)))
      (dolist (duplicate (find-duplicates shadowed-symbol-names
					  interned-symbol-names
					  (loop for list in shadowing-imported-from-symbol-names-list append (rest list))
					  (loop for list in imported-from-symbol-names-list append (rest list))))
	(si::simple-program-error
	 "The symbol ~s cannot coexist in these lists:~{ ~s~}"
	 (first duplicate)
	 (loop for num in (rest duplicate)
	       collect (case num
			 (1 ':SHADOW)
			 (2 ':INTERN)
			 (3 ':SHADOWING-IMPORT-FROM)
			 (4 ':IMPORT-FROM)))))
      (dolist (duplicate (find-duplicates exported-symbol-names
					  interned-symbol-names))
	(si::simple-program-error
	 "The symbol ~s cannot coexist in these lists:~{ ~s~}"
	 (first duplicate)
	 (loop for num in (rest duplicate) collect
	       (case num
		 (1 ':EXPORT)
		 (2 ':INTERN)))))
      `(eval-when (eval compile load)
        (si::dodefpackage
	,name
	',nicknames
	,(car documentation)
	',(if (assoc ':use options) (option-values ':use options) "CL")
	',shadowed-symbol-names
	',interned-symbol-names
	',exported-symbol-names
	',shadowing-imported-from-symbol-names-list
	',imported-from-symbol-names-list
	',exported-from-package-names)))))


(defun dodefpackage (name
		    nicknames
		    documentation
		    use
		    shadowed-symbol-names
		    interned-symbol-names
		    exported-symbol-names
		    shadowing-imported-from-symbol-names-list
		    imported-from-symbol-names-list
		    exported-from-package-names)
  (if (find-package name)
      (progn ; (rename-package name name)
        (when nicknames
          (rename-package name name nicknames))
        (when use
          (unuse-package (package-use-list (find-package name)) name)))
      (make-package name :use nil :nicknames nicknames))
  (let ((*package* (find-package name)))
    (when documentation
      (setf (package-documentation *package*) documentation))
    (shadow shadowed-symbol-names)
    (dolist (item shadowing-imported-from-symbol-names-list)
      (let ((package (find-package (first item))))
        (unless package
          (signal-simple-error 'simple-package-error "Create the package"
                               "The name ~s does not designate any package"
                               (list (first item))
                               :package (find-package name)))
	(dolist (name (rest item))
	  (shadowing-import (find-or-make-symbol name package)))))
    (use-package use)
    (dolist (item imported-from-symbol-names-list)
      (let ((package (find-package (first item))))
	(dolist (name (rest item))
	  ;; IMPORT can accept a list as argument, hence if we want to
	  ;; import symbol NIL, we have to enclose it in a list.
	  (import (or (find-or-make-symbol name package) (list NIL))))))
    (mapc #'intern interned-symbol-names)
    (export (mapcar #'intern exported-symbol-names))
    (dolist (package exported-from-package-names)
      (do-external-symbols (symbol (find-package package))
	(when (nth 1 (multiple-value-list
		      (find-symbol (string symbol))))
	  (export (list (intern (string symbol))))))))
  (find-package name))

(defun find-or-make-symbol (name package)
  (multiple-value-bind (symbol found)
      (find-symbol name package)
    (unless found
      (signal-simple-error 'simple-package-error "INTERN it."
			   "Cannot find symbol ~S in package ~S"
			   (list name package)
			   :package package)
      (setq symbol (intern name package)))
    symbol))

(defun find-duplicates (&rest lists)
  (let (results)
    (loop for list in lists
	  for more on (cdr lists)
	  for i from 1
	  do
	  (loop for elt in list
		as entry = (find elt results :key #'car
				 :test #'string=)
		unless (member i entry)
		do
		(loop for l2 in more
		      for j from (1+ i)
		      do
		      (if (member elt l2 :test #'string=)
			(if entry
			  (nconc entry (list j))
			  (setq entry (car (push (list elt i j)
						 results))))))))
    results))

;;;; ------------------------------------------------------------
;;;;	End of File
;;;; ------------------------------------------------------------
