;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPC-SET	Set locations

(in-package "C-BACKEND")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ASSIGNING TO LOCATIONS
;;;

(defun uses-values (loc)
  (and (consp loc)
       (or (member (car loc) '(CALL CALL-NORMAL CALL-INDIRECT) :test #'eq)
           (and (eq (car loc) 'C-INLINE)
                (eq (sixth loc) 'VALUES)))))

(defun set-loc (loc destination)
  (unless (eql destination loc)
    (cond ((var-p destination)
           (set-var loc destination))
          ((atom destination)
           (let ((fd (gethash destination +c2-set-loc-table+)))
             (cond (fd
                    (funcall fd loc))
                   ((setq fd (gethash destination +c2-wt-loc-table+))
                    (wt-nl) (funcall fd) (wt "= ")
                    (wt-coerce-loc (loc-representation-type destination) loc)
                    (wt ";"))
                   (t
                    (error "No known way to assign to location ~A"
                           destination)))))
          (t
           (let* ((name (first destination))
                  (fd (gethash name +c2-set-loc-table+)))
             (cond (fd
                    (apply fd loc (rest destination)))
                   ((setq fd (gethash name +c2-wt-loc-table+))
                    (wt-nl) (apply fd (rest destination)) (wt "= ")
                    (wt-coerce-loc (loc-representation-type destination) loc)
                    (wt ";"))
                   (t
                    (error "No known way to assign to location ~A"
                           destination))))))))

(defun set-values-loc (loc)
  (cond ((eq loc 'VALUES))
        ((uses-values loc)
         (wt-nl "cl_env_copy->values[0]=") (wt-coerce-loc :object loc) (wt ";"))
        (t
         (wt-nl "cl_env_copy->values[0]=") (wt-coerce-loc :object loc)
         (wt ";")
         (wt-nl "cl_env_copy->nvalues=1;"))))

(defun set-values+value0-loc (loc)
  (cond ((eq loc 'VALUES)
         (wt-nl "value0=cl_env_copy->values[0];"))
        ((uses-values loc)
         (wt-nl "value0=")(wt-coerce-loc :object loc) (wt ";"))
        (t
         (wt-nl "value0=") (wt-coerce-loc :object loc) (wt ";")
         (wt-nl "cl_env_copy->nvalues=1;"))))

(defun set-value0-loc (loc)
  (wt-nl "value0=") (wt-coerce-loc :object loc) (wt ";"))

(defun set-return-loc (loc)
  (set-values+value0-loc loc))

(defun set-actual-return-loc (loc)
  (set-loc loc 'VALUES+VALUE0)
  (wt-nl "return value0;"))

(defun set-trash-loc (loc)
  (when (loc-has-side-effects loc)
    (wt-nl loc ";")))

(defun set-the-loc (value type loc)
  (set-loc value loc))

(defun set-var (loc var &aux (var-loc (var-loc var))) ;  ccb
  (unless (var-p var)
    (baboon))
  (when (unused-variable-p var)
    (set-loc loc 'trash)
    (return-from set-var))
  (case (var-kind var)
    (DISCARDED
     (set-loc loc 'TRASH))
    (CLOSURE
     (wt-nl)(wt-env var-loc)(wt "= ")
     (wt-coerce-loc (var-rep-type var) loc)
     (wt #\;))
    (LEXICAL
     (wt-nl)(wt-lex var-loc)(wt "= ")
     (wt-coerce-loc (var-rep-type var) loc)
     (wt #\;))
    ((SPECIAL GLOBAL)
     (if (safe-compile)
         (wt-nl "cl_set(" var-loc ",")
         (wt-nl "ECL_SETQ(cl_env_copy," var-loc ","))
     (wt-coerce-loc (var-rep-type var) loc)
     (wt ");"))
    (t
     (wt-nl var-loc "= ")
     (wt-coerce-loc (var-rep-type var) loc)
     (wt #\;))))
