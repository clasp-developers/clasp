;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;; CMPENV-PROCLAIM -- Proclamations for the compiler
;;;;
;;;; One implementation of PROCLAIM that uses symbol properties to
;;;; store the proclamations. This has the disadvantage that
;;;; proclamations can not be easily cleaned up.
;;;;
;;;; The following code is to be coordinated with that in sysfun.lsp
;;;; and proclamations.lsp
;;;;
 
(in-package #-ecl-new "COMPILER" #+ecl-new "C-ENV")

#-:CCL
(defun proclaim (decl &aux decl-name)
  (unless (listp decl)
	  (error "The proclamation specification ~s is not a list" decl))
  (case (setf decl-name (car decl))
    (SPECIAL
     (dolist (var (cdr decl))
       (if (symbolp var)
           (sys:*make-special var)
           (error "Syntax error in proclamation ~s" decl))))
    (OPTIMIZE
     (dolist (x (cdr decl))
       (when (symbolp x) (setq x (list x 3)))
       (if (or (not (consp x))
               (not (consp (cdr x)))
               (not (numberp (second x)))
               (not (<= 0 (second x) 3)))
           (warn "The OPTIMIZE proclamation ~s is illegal." x)
           (case (car x)
		 (DEBUG (setq *debug* (second x)))
                 (SAFETY (setq *safety* (second x)))
                 (SPACE (setq *space* (second x)))
                 (SPEED (setq *speed* (second x)))
                 (COMPILATION-SPEED (setq *speed* (- 3 (second x))))
                 (t (warn "The OPTIMIZE quality ~s is unknown." (car x)))))))
    (TYPE
     (if (consp (cdr decl))
         (proclaim-var (second decl) (cddr decl))
         (error "Syntax error in proclamation ~s" decl)))
    (FTYPE
     (if (atom (rest decl))
	 (error "Syntax error in proclamation ~a" decl)
	 (multiple-value-bind (type-name args)
	     (si::normalize-type (second decl))
	   (if (eq type-name 'FUNCTION)
	       (dolist (v (cddr decl))
		 (proclaim-function v args))
	       (error "In an FTYPE proclamation, found ~A which is not a function type."
		      (second decl))))))
    (INLINE
     (proclaim-inline (cdr decl)))
    (NOTINLINE
     (proclaim-notinline (cdr decl)))
    ((OBJECT IGNORE DYNAMIC-EXTENT IGNORABLE)
     ;; FIXME! IGNORED!
     (dolist (var (cdr decl))
       (unless (si::valid-function-name-p var)
	 (error "Not a valid function name ~s in ~s proclamation" var decl-name))))
    (DECLARATION
     (validate-alien-declaration (rest decl) #'error)
     (setf si::*alien-declarations*
           (append (rest decl) si:*alien-declarations*)))
    (SI::C-EXPORT-FNAME
     (dolist (x (cdr decl))
       (cond ((symbolp x)
	      (multiple-value-bind (found c-name)
		  (si::mangle-name x t)
		(if found
		    (warn "The function ~s is already in the runtime.~%C-EXPORT-FNAME declaration ignored." x)
		    (put-sysprop x 'Lfun c-name))))
	     ((consp x)
	      (destructuring-bind (c-name lisp-name) x
		(if (si::mangle-name lisp-name)
		    (warn "The funciton ~s is already in the runtime.~%C-EXPORT-FNAME declaration ignored." lisp-name)
		    (put-sysprop lisp-name 'Lfun c-name))))
	     (t
	      (error "Syntax error in proclamation ~s" decl)))))
    ((ARRAY ATOM BASE-CHAR BIGNUM BIT BIT-VECTOR CHARACTER COMPILED-FUNCTION
      COMPLEX CONS DOUBLE-FLOAT EXTENDED-CHAR FIXNUM FLOAT HASH-TABLE INTEGER KEYWORD LIST
      LONG-FLOAT NIL NULL NUMBER PACKAGE PATHNAME RANDOM-STATE RATIO RATIONAL
      READTABLE SEQUENCE SHORT-FLOAT SIMPLE-ARRAY SIMPLE-BIT-VECTOR
      SIMPLE-STRING SIMPLE-VECTOR SINGLE-FLOAT STANDARD-CHAR STREAM STRING
      SYMBOL T VECTOR SIGNED-BYTE UNSIGNED-BYTE FUNCTION)
     (proclaim-var decl-name (cdr decl)))
    (otherwise
     (cond ((member (car decl) si:*alien-declarations*))
	   ((multiple-value-bind (ok type)
		(valid-type-specifier decl-name)
	      (when ok
		(proclaim-var type (rest decl))
		t)))
           ((maybe-add-policy decl *cmp-env-root*))            
	   ((let ((proclaimer (get-sysprop (car decl) :proclaim)))
	      (when (functionp proclaimer)
		(mapc proclaimer (rest decl))
		t)))
	   (t
	    (warn "Unknown declaration specifier ~s" decl-name))))))

(defun proclaim-var (type vl)
  (dolist (var vl)
    (if (symbolp var)
	(let ((type1 (get-sysprop var 'CMP-TYPE)))
	  (setq type1 (if type1 (type-and type1 type) type))
	  (unless type1
	    (warn
	     "Inconsistent type declaration was found for the variable ~s."
	     var)
	    (setq type1 T))
	  (put-sysprop var 'CMP-TYPE type1))
	(warn "The variable name ~s is not a symbol." var))))

