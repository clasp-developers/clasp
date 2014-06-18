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

;;;; CMPIF  Conditionals.

(in-package "COMPILER")

(defun c1if (args)
  (check-args-number 'IF args 2 3)
  (let ((test (c1expr (car args))))
    ;; Resolve IF expressions with constant arguments
    (multiple-value-bind (constant-p value)
	(c1form-constant-p test)
      (when constant-p
	(return-from c1if
	  (if value (second args) (third args)))))
    ;; Otherwise, normal IF form
    (let* ((true-branch (c1expr (second args)))
           (false-branch (c1expr (third args))))
      (make-c1form* 'IF
                    :type (values-type-or (c1form-type true-branch)
					  (c1form-type false-branch))
                    :args test true-branch false-branch))))

(defun c1not (args)
  (check-args-number 'NOT args 1 1)
  (let* ((value (c1expr (first args))))
    ;; When the argument is constant, we can just return
    ;; a constant as well.
    (multiple-value-bind (constant-p value)
	(c1form-constant-p value)
      (when constant-p
	(return-from c1not (not value))))
    (make-c1form* 'FMLA-NOT
                  :type '(member t nil)
                  :args value)))

(defun c1and (args)
  ;; (AND) => T
  (if (null args)
      (c1t)
      (let* ((values (c1args* args))
             (last (first (last values)))
             (butlast (nbutlast values)))
        ;; (AND x) => x
        (if butlast
            (make-c1form* 'FMLA-AND
                          :type (type-or 'null (c1form-primary-type last))
                          :args butlast last)
            last))))

(defun c1or (args)
  ;; (OR) => T
  (if (null args)
      (c1nil)
      (let* ((values (c1args* args))
	     (last (first (last values)))
	     (butlast (butlast values)))
        ;; (OR x) => x
	(if butlast
	    (make-c1form* 'FMLA-OR
			 :type (reduce #'type-or butlast
                                       :key #'c1form-primary-type
                                       :initial-value (c1form-primary-type last))
			 :args butlast last)
	    last))))

(defun c2if (c1form fmla form1 form2)
  (declare (ignore c1form))
  ;; FIXME! Optimize when FORM1 or FORM2 are constants
  (cond ((and (eq *destination* 'TRASH)
	      (eq (c1form-name form2) 'LOCATION))
	 ;; Optimize (IF condition true-branch) or a situation in which
	 ;; the false branch can be discarded.
	 (with-optional-exit-label (false-label)
	   (let ((*destination* `(JUMP-FALSE ,false-label)))
	     (c2expr* fmla))
	   (c2expr form1)))
	((and (eq *destination* 'TRASH)
	      (eq (c1form-name form1) 'LOCATION))
	 ;; Optimize (IF condition useless-value false-branch) when
	 ;; the true branch can be discarded.
	 (with-optional-exit-label (true-label)
	   (let ((*destination* `(JUMP-TRUE ,true-label)))
	     (c2expr* fmla))
	   (c2expr form2)))
	(t
	 (with-exit-label (false-label)
	   (let ((*destination* `(JUMP-FALSE ,false-label)))
	     (c2expr* fmla))
	   (c2expr form1))
	 (c2expr form2))))

(defun negate-argument (inlined-arg dest-loc)
  (let* ((loc (second inlined-arg))
         (rep-type (loc-representation-type loc)))
    (apply #'produce-inline-loc
           (list inlined-arg)
           (if (eq (loc-representation-type dest-loc) :bool)
               (case rep-type
                 (:bool '((:bool) (:bool) "(#0)==ECL_NIL" nil t))
                 (:object '((:object) (:bool) "(#0)!=ECL_NIL" nil t))
                 (otherwise (return-from negate-argument nil)))
               (case rep-type
                 (:bool '((:bool) (:object) "(#0)?ECL_NIL:ECL_T" nil t))
                 (:object '((:object) (:object) "Null(#0)?ECL_T:ECL_NIL" nil t))
                 (otherwise (return-from negate-argument nil)))))))

(defun c2fmla-not (c1form arg)
  (declare (ignore c1form))
  (let ((dest *destination*))
    (cond ((jump-true-destination-p dest)
           (let ((*destination* `(JUMP-FALSE ,@(cdr dest))))
             (c2expr arg)))
          ((jump-false-destination-p dest)
           (let ((*destination* `(JUMP-TRUE ,@(cdr dest))))
             (c2expr arg)))
          (t
           (let ((*inline-blocks* 0)
                 (*temp* *temp*))
             (unwind-exit (negate-argument
                           (emit-inline-form arg nil)
                           dest))
             (close-inline-blocks))))))

(defun jump-true-destination-p (dest)
  (and (consp dest) (eq (si:cons-car dest) 'JUMP-TRUE)))

(defun jump-false-destination-p (dest)
  (and (consp dest) (eq (si:cons-car dest) 'JUMP-FALSE)))

(defun c2fmla-and (c1form butlast last)
  (declare (ignore c1form))
  (if (jump-false-destination-p *destination*)
      (progn
	(mapc #'c2expr* butlast)
	(c2expr last))
      (with-exit-label (normal-exit)
	(with-exit-label (false-label)
	  (let ((*destination* `(JUMP-FALSE ,false-label)))
	    (mapc #'c2expr* butlast))
	  (c2expr last))
	(unwind-exit nil))))

(defun c2fmla-or (c1form butlast last)
  (declare (ignore c1form))
  (cond ((jump-true-destination-p *destination*)
	 (mapc #'c2expr* butlast)
	 (c2expr last))
	((jump-false-destination-p *destination*)
	 (with-exit-label (true-label)
	   (let ((*destination* `(JUMP-TRUE ,true-label)))
	     (mapc #'c2expr* butlast))
	   (c2expr last))
	 (unwind-exit t))
        (t
         (with-exit-label (common-exit)
           (with-exit-label (normal-exit)
             (dolist (f butlast)
               (let ((*destination* 'VALUE0))
                 (c2expr* f))
               (set-jump-true 'VALUE0 normal-exit))
             (c2expr last))
           (unwind-exit 'VALUE0)))))

(defun set-jump-true (loc label)
  (multiple-value-bind (constantp value)
      (loc-immediate-value-p loc)
    (cond ((not constantp)
           (cond ((eq (loc-representation-type loc) :bool)
                  (wt-nl "if (" loc ") {"))
                 (t
                  (wt-nl "if ((")
                  (wt-coerce-loc :object loc)
                  (wt ")!=ECL_NIL) {")))
           (cond ((unwind-no-exit label)
		  (incf *opened-c-braces*)
		  (wt-nl) (wt-go label)
		  (wt-nl-close-brace))
		 (t
		  (wt " ") (wt-go label) (wt " }"))))
          ((null value))
          (t
           (unwind-no-exit label)
           (wt-nl) (wt-go label)))))

(defun set-jump-false (loc label)
  (multiple-value-bind (constantp value)
      (loc-immediate-value-p loc)
    (cond ((not constantp)
           (cond ((eq (loc-representation-type loc) :bool)
                  (wt-nl "if (!(" loc ")) {"))
                 (t
                  (wt-nl "if (Null(")
                  (wt-coerce-loc :object loc)
                  (wt ")) {")))
	   (cond ((unwind-no-exit label)
		  (incf *opened-c-braces*)
		  (wt-nl) (wt-go label)
		  (wt-nl-close-brace))
		 (t
		  (wt " ") (wt-go label) (wt " }"))))
          (value)
          (t
           (unwind-no-exit label)
           (wt-nl) (wt-go label)))))
