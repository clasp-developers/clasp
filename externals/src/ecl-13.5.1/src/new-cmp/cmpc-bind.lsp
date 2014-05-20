;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPBIND  Variable Binding.
;;;;
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll.
;;;;
;;;;  This file is part of ECoLisp, herein referred to as ECL.
;;;;
;;;;    ECL is free software; you can redistribute it and/or modify it under
;;;;    the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;;;;    the Free Software Foundation; either version 2 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "C-BACKEND")

;;; bind must be called for each variable in a lambda or let, once the value
;;; to be bound has been placed in loc.
;;; bind takes care of setting var-loc.

(defun bind (loc var)
  ;; loc can be either (LCL n), 'VA-ARGS, (KEYVARS n), (CAR n),
  ;; a constant, or (VAR var) from a let binding. ; ccb
  (declare (type var var))
  (case (var-kind var)
    (CLOSURE
     (let ((var-loc (var-loc var)))
       (unless (sys:fixnump var-loc)
	 ;; first binding: assign location
	 (setq var-loc (next-env))
	 (setf (var-loc var) var-loc))
       (wt-nl "cl_object CLV" var-loc "=env" *env-lvl* "=CONS(")
       (wt-coerce-loc :object loc)
       (if (zerop var-loc)
           (wt ",Cnil);")
           (wt ",env" *env-lvl* ");"))
       (wt-comment (var-name var))))
    (LEXICAL
     (let ((var-loc (var-loc var)))
       (unless (consp var-loc)
	 ;; first binding: assign location
	 (setq var-loc (next-lex))
	 (setf (var-loc var) var-loc))
       (wt-nl) (wt-lex var-loc) (wt "= ")
       (wt-coerce-loc :object loc)
       (wt ";"))
       (wt-comment (var-name var)))
    ((SPECIAL GLOBAL)
     (bds-bind loc var))
    (t
     (cond ((not (eq (var-loc var) 'OBJECT))
	    ;; already has location (e.g. optional in lambda list)
	    ;; check they are not the same
	    (unless (equal (var-loc var) loc)
	      (wt-nl var "= ")
	      (wt-coerce-loc (var-rep-type var) loc)
	      (wt ";")))
	   ((and (consp loc) (eql (car loc) 'LCL))
	    ;; set location for lambda list requireds
	    (setf (var-loc var) loc))
	   (t
	    (baboon)))
	 )))

(defun bds-bind (loc var)
  ;; Optimize the case (let ((*special-var* *special-var*)) ...)
  (cond ((and (var-p loc)
	      (member (var-kind loc) '(global special))
	      (eq (var-name loc) (var-name var)))
	 (wt-nl "ecl_bds_push(cl_env_copy," (var-loc var) ");"))
	(t
	 (wt-nl "ecl_bds_bind(cl_env_copy," (var-loc var) ",")
	 (wt-coerce-loc :object loc)
	 (wt ");")))
  (wt-comment (var-name var)))
