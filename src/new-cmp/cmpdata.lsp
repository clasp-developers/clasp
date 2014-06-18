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

;;;; CMPDATA     Collect data used in lisp code

(in-package "C-DATA")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DATA DATABASE
;;;
;;; Each lisp compiled file consists on code and a data section. Among
;;; other optimizations Whenever an #'in-package toplevel form is
;;; found, a read-time evaluated expression is inserted in the data
;;; section which changes the current package for the rest of it. This
;;; way it is possible to save some space by writing the symbol's
;;; package only when it does not belong to the current package.

(defun data-permanent-storage-size ()
  (length *permanent-objects*))

(defun data-temporary-storage-size ()
  (length *temporary-objects*))

(defun data-size ()
  (+ (data-permanent-storage-size)
     (data-temporary-storage-size)))

(defun data-init (&optional filename)
  (if (and filename (probe-file filename))
      (with-open-file (s filename :direction :input)
	(setf *permanent-objects* (read s)
	      *temporary-objects* (read s)))
      (setf *permanent-objects* (make-array 128 :adjustable t :fill-pointer 0)
	    *temporary-objects* (make-array 128 :adjustable t :fill-pointer 0))))

(defun data-get-all-objects ()
  ;; We collect all objects that are to be externalized, but filter out
  ;; those which will be created by a lisp form.
  (loop for i in (nconc (map 'list #'first *permanent-objects*)
			(map 'list #'first *temporary-objects*))
	collect (if (gethash i *load-objects*)
		    0
		    i)))

(defun data-empty-loc ()
  (add-object 0 :duplicate t :permanent t))

(defun load-form-data-place-p (data-record)
  (typep (first data-record) 'fixnum))

(defun add-load-form (object location)
  (when (clos::need-to-make-load-form-p object *cmp-env*)
    (if (not (eq *compiler-phase* 't1))
	(error "Unable to internalize complex object ~A in ~a phase"
               object *compiler-phase*)
	(multiple-value-bind (make-form init-form) (make-load-form object)
	  (setf (gethash object *load-objects*) location)
          (setf *make-forms*
                (nconc *make-forms*
                       (and make-form (c1translate location make-form))
                       (and init-form (c1translate location init-form))))))))

(defun add-object (object &key (duplicate nil)
		   (permanent (or (symbolp object) *permanent-data*)))
  ;; FIXME! Currently we have two data vectors and, when compiling
  ;; files, it may happen that a constant is duplicated and stored
  ;; both in VV and VVtemp. This would not be a problem if the
  ;; constant were readable, but due to using MAKE-LOAD-FORM we may
  ;; end up having two non-EQ objects created for the same value.
  (let* ((test (if *compiler-constants* 'eq 'equal))
	 (array (if permanent *permanent-objects* *temporary-objects*))
	 (vv (if permanent 'VV 'VV-temp))
	 (x (or (and (not permanent)
		     (find object *permanent-objects* :test test
			   :key #'first))
		(find object array :test test :key #'first)))
	 (next-ndx (length array))
	 found)
    (cond ((and x duplicate)
	   (setq x (list* vv next-ndx (if (eq 0 object) nil (list object))))
	   (vector-push-extend (list object x next-ndx) array)
	   x)
	  (x
	   (second x))
	  ((and (not duplicate)
		(symbolp object)
		(multiple-value-setq (found x) (si::mangle-name object)))
	   x)
	  (t
	   (setq x (list* vv next-ndx (if (eq 0 object) nil (list object))))
	   (vector-push-extend (list object x next-ndx) array)
	   (unless *compiler-constants*
	     (add-load-form object x))
	   x))))

(defun add-symbol (symbol)
  (add-object symbol :duplicate nil :permanent t))

(defun add-keywords (keywords)
  ;; We have to build, in the vector VV[], a sequence with all
  ;; the keywords that this function uses. It does not matter
  ;; whether each keyword has appeared separately before, because
  ;; cl_parse_key() needs the whole list. However, we can reuse
  ;; keywords lists from other functions when they coincide with ours.
  ;; We search for keyword lists that are similar. However, the list
  ;; *OBJECTS* contains elements in decreasing order!!!
  (let ((x (search keywords *permanent-objects*
		   :test #'(lambda (k record) (eq k (first record))))))
    (if x
        (second (elt *permanent-objects* x))
	(prog1
	    (add-object (pop keywords) :duplicate t :permanent t)
	  (dolist (k keywords)
	    (add-object k :duplicate t :permanent t))))))
