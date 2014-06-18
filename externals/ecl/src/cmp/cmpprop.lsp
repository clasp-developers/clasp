;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPPROP Type propagation.

(in-package "COMPILER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TYPE PROPAGATION LOOP
;;;

(eval-when (:execute :compile-toplevel)
  (defparameter *type-propagation-messages* nil)
  (defmacro prop-message (string &rest args)
    (when *type-propagation-messages*
      `(format *standard-output* ,string ,@args))))

(defun p1propagate (form assumptions)
  (unless form
    (return-from p1propagate (values 'null assumptions)))
  (when (c1form-p form)
    (let* ((*cmp-env* (c1form-env form))
	   (*compile-file-pathname* (c1form-file form))
	   (*compile-file-position* (c1form-file-position form))
	   (*current-form* (c1form-form form))
	   (*current-toplevel-form* (c1form-toplevel-form form))
           (name (c1form-name form))
           (propagator (gethash name *p1-dispatch-table*)))
      (when propagator
        (prop-message "~&;;; Entering type propagation for ~A" name)
        (multiple-value-bind (new-type assumptions)
            (apply propagator form assumptions (c1form-args form))
          (when assumptions
            (baboon :format-control "Non-empty assumptions found in P1PROPAGATE"))
          (prop-message "~&;;; Propagating ~A gives type ~A" name
                        new-type)
          (return-from p1propagate
	     (values (setf (c1form-type form)
			   (values-type-and (c1form-type form)
					    new-type))
		     assumptions))))))
  (cmpnote "Refusing to propagate ~A" form)
  (values (c1form-type form) assumptions))

(defun p1trivial (form assumptions &rest rest)
  (declare (ignore rest))
  (values (c1form-type form) assumptions))

(defun p1var (form assumptions var)
  (let* ((record (and (assoc var assumptions)
		      (baboon :format-control "Non empty assumptions found in P1VAR")))
	 ;; Use the type of C1FORM because it might have been
	 ;; coerced by a THE form.
	 (var-type (if record (cdr record) (var-type var)))
	 (type (type-and var-type (c1form-primary-type form))))
    (prop-message "~&;;; Querying variable ~A gives ~A" (var-name var) type)
    (values type assumptions)))

(defun p1values (form assumptions values)
  (loop for v in values
     collect (multiple-value-bind (type new-assumptions)
		 (p1propagate v assumptions)
	       (setf assumptions new-assumptions)
	       (values-type-primary-type type))
     into all-values
     finally (return (values `(values ,@all-values) assumptions))))

(defun p1propagate-list (list assumptions)
  (loop with final-type = t
     for f in list
     do (multiple-value-setq (final-type assumptions) (p1propagate f assumptions))
     finally (return (values final-type assumptions))))

(defun p1merge-branches (root chains)
  "ROOT is a list of assumptions, while CHAINS is list of extended versions of
ROOT. This function takes all those extensions and makes a final list in which
type assumptions have been merged, giving the variables the OR type of each
of the occurrences in those lists."
  (unless (and (null root)
	       (every #'null chains))
    (baboon :format-control "P1MERGE-BRANCHES got a non-empty list of assumptions")))

(defun revise-var-type (variable assumptions where-to-stop)
  (unless (and (null assumptions) (null where-to-stop))
    (baboon :format-control "REVISE-VAR-TYPE got a non-empty list of assumptions")))

(defun p1block (c1form assumptions blk body)
  (setf (blk-type blk) nil)
  (multiple-value-bind (normal-type assumptions)
      (p1propagate body assumptions)
    (let ((blk-type (blk-type blk)))
      (values (if blk-type (values-type-or blk-type normal-type) normal-type)
	      assumptions))))

(defun p1return-from (c1form assumptions blk return-type value variable-or-nil)
  (let* ((values-type (p1propagate value assumptions))
	 (blk-type (blk-type blk)))
    (setf (blk-type blk) (if blk-type
			     (values-type-or blk-type values-type)
			     values-type))
    (values values-type assumptions)))

(defun p1call-global (c1form assumptions fname args)
  (loop for v in args
     do (multiple-value-bind (arg-type local-ass)
            (p1propagate v assumptions)
          (setf assumptions local-ass))
     finally (let ((type (propagate-types fname args)))
               (prop-message "~&;;; Computing output of function ~A with args~&;;;  ~{ ~A~}~&;;; gives ~A, while before ~A"
                       fname (mapcar #'c1form-primary-type args)
		       type (c1form-type c1form))
               (return (values type assumptions)))))

(defun p1call-local (c1form assumptions fun args)
  (loop for v in args
     do (multiple-value-bind (arg-type local-ass)
            (p1propagate v assumptions)
          (setf assumptions local-ass))
     finally (return (values (fun-return-type fun)
                             assumptions))))

(defun p1catch (c1form assumptions tag body)
  (multiple-value-bind (tag-type assumptions)
      (p1propagate tag assumptions)
    (p1propagate body assumptions))
  (values t assumptions))

(defun p1throw (c1form assumptions catch-value output-value)
  (multiple-value-bind (type new-assumptions)
      (p1propagate catch-value assumptions)
    (p1propagate output-value new-assumptions))
  (values t assumptions))

(defun p1if (c1form assumptions fmla true-branch false-branch)
  (multiple-value-bind (fmla-type base-assumptions)
      (p1propagate fmla assumptions)
    (multiple-value-bind (t1 a1)
        (p1propagate true-branch base-assumptions)
      (multiple-value-bind (t2 a2)
          (p1propagate false-branch base-assumptions)
        (values (values-type-or t1 t2)
		(p1merge-branches base-assumptions (list a1 a2)))))))

(defun p1fmla-not (c1form assumptions form)
  (multiple-value-bind (type assumptions)
      (p1propagate form assumptions)
    (values '(member t nil) assumptions)))

(defun p1fmla-and (c1form orig-assumptions butlast last)
  (loop with type = t
     with assumptions = orig-assumptions
     for form in (append butlast (list last))
     collect (progn
	       (multiple-value-setq (type assumptions)
		 (p1propagate form assumptions))
	       assumptions)
     into assumptions-list
     finally (return (values (type-or 'null (values-type-primary-type type))
			     (p1merge-branches orig-assumptions
					       assumptions-list)))))

(defun p1fmla-or (c1form orig-assumptions butlast last)
  (loop with type
     with output-type = t
     with assumptions = orig-assumptions
     for form in (append butlast (list last))
     collect (progn
	       (multiple-value-setq (type assumptions)
		 (p1propagate form assumptions))
	       (setf output-type (type-or (values-type-primary-type type)
					  output-type))
	       assumptions)
     into assumptions-list
     finally (return (values output-type
			     (p1merge-branches orig-assumptions
					       assumptions-list)))))

(defun p1lambda (c1form assumptions lambda-list doc body &rest not-used)
  (prop-message "~&;;;~&;;; Propagating function~&;;;")
  (let ((type (p1propagate body assumptions)))
    (values type assumptions)))

(defun p1propagate-function (fun assumptions)
  (multiple-value-bind (output-type assumptions)
      (p1propagate (fun-lambda fun) assumptions)
    (values (setf (fun-return-type fun) output-type)
            assumptions)))

(defun p1let* (c1form base-assumptions vars forms body)
  (let ((assumptions base-assumptions))
    (loop with type
       for v in vars
       for f in forms
       unless (or (global-var-p v) (var-set-nodes v))
       do (progn
	    (multiple-value-setq (type assumptions) (p1propagate f assumptions))
	    (setf (var-type v) (type-and (values-type-primary-type type)
					 (var-type v)))
	    (prop-message "~&;;; Variable ~A assigned type ~A"
			  (var-name v) (var-type v))))
    (multiple-value-bind (type assumptions)
        (p1propagate body assumptions)
      (loop for v in vars
         do (revise-var-type v assumptions base-assumptions))
      (values type assumptions))))

(defun p1locals (c1form assumptions funs body labels)
  (loop for f in funs
     do (p1propagate-function f assumptions))
  (p1propagate body assumptions))

(defun p1multiple-value-bind (c1form assumptions vars-list init-c1form body)
  (multiple-value-bind (init-form-type assumptions)
      (p1propagate init-c1form assumptions)
    (loop for v in vars-list
       for type in (values-type-to-n-types init-form-type (length vars-list))
       unless (or (global-var-p v)
		  (var-set-nodes v))
       do (setf (var-type v) (type-and (var-type v) type)) and
       do (prop-message "~&;;; Variable ~A assigned type ~A"
			  (var-name v) (var-type v)))
    (p1propagate body assumptions)))

(defun p1multiple-value-setq (c1form assumptions vars-list value-c1form)
  (multiple-value-bind (init-form-type assumptions)
      (p1propagate value-c1form assumptions)
    (values init-form-type assumptions)))

(defun p1progn (c1form assumptions forms)
  (p1propagate-list forms assumptions))

(defun p1compiler-typecase (c1form assumptions variable expressions)
  (let ((var-type (var-type variable)))
    (loop with output-type = t
       for (a-type c1form) in expressions
       for c1form-type = (p1propagate c1form assumptions)
       when (or (member a-type '(t otherwise))
		(subtypep var-type a-type))
       do (setf output-type c1form-type)
       finally (return (values output-type assumptions)))))

(defun p1checked-value (c1form assumptions type value let-form)
  (let* ((value-type (p1propagate value assumptions))
	 (alt-type (p1propagate let-form assumptions)))
    (if (subtypep value-type type)
	value-type
	type)))

(defun p1progv (c1form assumptions variables values body)
  (let (type)
    (multiple-value-setq (type assumptions)
      (p1propagate variables assumptions))
    (multiple-value-setq (type assumptions)
      (p1propagate values assumptions))
    (p1propagate body assumptions)))

(defun p1setq (c1form assumptions var c1form)
  (multiple-value-bind (value-type assumptions)
      (p1propagate c1form assumptions)
    (values (type-and (var-type var) (values-type-primary-type value-type))
	    assumptions)))

(defun p1psetq (c1form assumptions vars c1forms)
  (loop for form in c1forms
     do (multiple-value-bind (new-type assumptions)
	  (p1propagate form assumptions)))
  (values 'null assumptions))

(defun p1with-stack (c1form assumptions body)
  (p1propagate body assumptions))

(defun p1stack-push-values (c1form assumptions form inline)
  (multiple-value-bind (form-type assumptions)
      (p1propagate form assumptions)
    (values nil assumptions)))

(defvar *tagbody-depth* -1
  "If n > 0, limit the number of passes to converge tagbody forms. If
-1, let the compiler do as many passes as it wishes. Complexity grows
as 2^*tagbody-limit* in the worst cases.")

(defun p1go (c1form assumptions tag-var return-type)
  (values t assumptions))

(defun filter-only-declarations (assumptions)
  (when assumptions
    (baboon :format-control "FILTER-ONLY-DECLARATIONS gets a non-empty assumption list"))
  nil)

(defun p1tagbody (c1form orig-assumptions tag-loc body)
  (prop-message "~&;;; P1TAGBODY-SIMPLE pass")
  (let* ((assumptions (filter-only-declarations orig-assumptions))
         (ass-list (p1tagbody-one-pass c1form assumptions tag-loc body)))
    (values 'null (append (p1merge-branches nil ass-list) orig-assumptions))))

(defun p1tagbody-one-pass (c1form assumptions tag-loc body)
  (declare (ignore tag-loc))
  (loop with local-ass = assumptions
     with ass-list = '()
     with aux
     for f in body
     do (if (tag-p f)
            (let ((diff (ldiff local-ass assumptions)))
              (when diff
                (push diff ass-list))
              (prop-message "~&;;; Label ~A found" (tag-name f))
              (setf local-ass assumptions))
            (multiple-value-setq (aux local-ass) (p1propagate f local-ass)))
     finally (return
               (let ((diff (ldiff local-ass assumptions)))
                 (if diff
                     (cons diff ass-list)
                     ass-list)))))

(defun p1unwind-protect (c1form assumptions form body)
  (declare (ignore c1form))
  (multiple-value-bind (output-type assumptions)
      (p1propagate form assumptions)
    (p1propagate body assumptions)
    (values output-type assumptions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun type-from-array-elt (array &aux name)
  "Input is a lisp type representing a valid subtype of ARRAY. Output is
either the array element type or NIL, denoting that we are not able to
compute it. This version only handles the simplest cases."
  (values (cond ((eq array 'string)
                 'character)
                ((eq array 'base-string)
                 'base-char)
                ((member (setf array (expand-deftype array))
                         '(array vector simple-array))
                 t)
                ((atom array)
                 (setf array 'array)
                 t)
                ((eq (setf name (first array)) 'OR)
                 `(OR ,@(mapcar #'type-from-array-elt (rest array))))
                ((eq (setf name (first array)) 'AND)
                 `(AND ,@(mapcar #'type-from-array-elt (rest array))))
                ((not (member (first array) 
                              '(array vector simple-array)))
                 (setf array 'array)
		 t)
                ((null (rest array))
                 t)
                (t
		 (let ((x (second array)))
		   (if (eq x '*) t x))))
          array))

(def-type-propagator si::aset (fname array-type &rest indices-and-object)
  (multiple-value-bind (elt-type array-type)
      (type-from-array-elt array-type)
    (values (cons array-type
		  (nconc (make-list (1- (length indices-and-object))
				    :initial-element 'si::index)
			 (list elt-type)))
            elt-type)))

(def-type-propagator aref (fname array-type &rest indices)
  (multiple-value-bind (elt-type array-type)
      (type-from-array-elt array-type)
    (values (list* array-type (make-list (length indices)
                                         :initial-element 'si::index))
            elt-type)))

(def-type-propagator si::row-major-aset (fname array-type index obj)
  (multiple-value-bind (elt-type array-type)
      (type-from-array-elt array-type)
    (values (list array-type 'si::index elt-type)
            elt-type)))

(def-type-propagator row-major-aref (fname array-type index)
  (multiple-value-bind (elt-type array-type)
      (type-from-array-elt array-type)
    (values (list array-type 'si::index) elt-type)))

