;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "CLOS")

;;; ----------------------------------------------------------------------
;;; Load forms
;;;
;;; Clasp extends the ANSI specification by allowing to use
;;; MAKE-LOAD-FORM on almost any kind of lisp object.
;;; But it doesn't necessarily use those methods in the compiler. see cmpliteral.lisp
;;;

(defun make-load-form-saving-slots (object &key slot-names environment)
  ;; The ALLOCATE-INSTANCE form here is treated magically by the file
  ;; compiler; see cmp/cmpliteral.lisp ALLOCATE-INSTANCE-FORM-P
  (declare (ignore environment))
  (do* ((class (class-of object))
	(initialization (list object 'load-instance))
	(slots (class-slots class) (cdr slots)))
      ((endp slots)
       (values `(allocate-instance ,class) (nreverse initialization)))
    (let* ((slot (first slots))
	   (slot-name (slot-definition-name slot)))
      (when (or (and (null slot-names)
		     (eq (slot-definition-allocation slot) :instance))
		(member slot-name slot-names))
        (when (slot-boundp object slot-name)
          (push `',slot-name initialization)
          (push `',(slot-value object slot-name) initialization))))))

;;; This function basically exists so that cmpliteral can handle
;;; make-load-form-saving-slots forms without compiling them recursively.
;;; We used to use a progn of setf slot-values, but that's more complex.
(defun load-instance (instance &rest slot-names-values)
  (loop for (name value) on slot-names-values by #'cddr
        do (setf (slot-value instance name) value))
  (values))

(defun need-to-make-load-form-p (object env)
  "Return T if the object cannot be externalized using the lisp
printer and we should rather use MAKE-LOAD-FORM."
  (declare (ignore env))
  (let ((*load-form-cache* nil))
    (declare (special *load-form-cache*))
    (labels ((recursive-test (object)
	       (loop
		;; For simple, atomic objects we just return NIL. There is no need to
		;; call MAKE-LOAD-FORM on them
		(when (typep object '(or character number symbol pathname string bit-vector))
		  (return nil))
		;; For complex objects we set up a cache and run through the
		;; objects content looking for data that might require
		;; MAKE-LOAD-FORM to be externalized.  The cache is used to
		;; solve the problem of circularity and of EQ references.
		(unless *load-form-cache*
		  (setf *load-form-cache* (make-hash-table :size 128 :test #'eq)))
		(when (gethash object *load-form-cache*)
		  (return nil))
		(setf (gethash object *load-form-cache*) t)
		(cond ((arrayp object)
		       (unless (subtypep (array-element-type object) '(or character number))
			 (dotimes (i (array-total-size object))
			   (recursive-test (row-major-aref object i))))
		       (return nil))
		      ((consp object)
		       (recursive-test (car object))
		       (setf object (rest object)))
		      (t
		       (throw 'need-to-make-load-form t))))))
      (catch 'need-to-make-load-form
	(recursive-test object)
	nil))))

(defmethod make-load-form ((object t) &optional env)
  (flet ((maybe-quote (object)
	   (if (or (consp object) (symbolp object))
	       (list 'quote object)
	       object)))
    (unless (need-to-make-load-form-p object env)
      (return-from make-load-form (maybe-quote object)))
    (typecase object
      (array
       (let ((init-forms '()))
	 (values `(make-array ',(array-dimensions object)
		   :element-type ',(array-element-type object)
		   :adjustable ',(adjustable-array-p object)
		   :initial-contents
		   ',(loop for i from 0 below (array-total-size object)
			   collect (let ((x (row-major-aref object i)))
				     (if (need-to-make-load-form-p x env)
					 (progn (push `(setf (row-major-aref ,object ,i) ',x)
						      init-forms)
						0)
					 x))))
		 (and init-forms `(progn ,@init-forms)))))
      (cons
       (values `(cons ,(maybe-quote (car object)) nil)
	       (and (rest object) `(rplacd ,(maybe-quote object)
					   ,(maybe-quote (cdr object))))))
      (t
       (no-make-load-form object)))))

(defmethod make-load-form ((object standard-object) &optional environment)
  (declare (ignore environment))
  (no-make-load-form object))

(defmethod make-load-form ((object structure-object) &optional environment)
  (declare (ignore environment))
  (no-make-load-form object))

(defmethod make-load-form ((object condition) &optional environment)
  (declare (ignore environment))
  (no-make-load-form object))

(defun no-make-load-form (object)
  #+(or)(declare (optimize (debug 3)))
  (error "No adequate specialization of MAKE-LOAD-FORM for an object of type ~a"
	 (type-of object)))

(defmethod make-load-form ((class class) &optional environment)
  ;; The find-class form here is treated magically by the file compiler-
  ;; see cmp/cmpliteral.lisp FIND-CLASS-FORM-P
  (declare (ignore environment))
  (let ((name (class-name class)))
    (if (and name (eq (find-class name) class))
	`(find-class ',name)
	(error "Cannot externalize anonymous class ~A" class))))

(defmethod make-load-form ((package package) &optional environment)
  (declare (ignore environment))
  `(find-package ,(package-name package)))

;;; Extension. (Allowed per CLHS 3.2.4.3.)
;;; This is required for a lot of satiation.lisp to function.
(defmethod make-load-form ((method method) &optional environment)
  (declare (ignore environment))
  ;; FIXME: Should spruce up cmpliteral so it doesn't compile calls with
  ;; all constant arguments.
  `(load-method
    ',(generic-function-name (method-generic-function method))
    ',(method-qualifiers method)
    ',(method-specializers method)))

;;; Also an extension, to support the above.
(defmethod make-load-form ((spec eql-specializer) &optional environment)
  (declare (ignore environment))
  `(intern-eql-specializer ',(eql-specializer-object spec)))

(defun class-slotd-form (slot-name class &optional earlyp)
  (let ((form
          `(or (find ',slot-name (class-slots ,class) :key #'slot-definition-name)
               (error "Probably a BUG: slot ~a in ~a stopped existing between compile and load"
                      ',slot-name ,class))))
    (if earlyp
        `(with-early-accessors (+standard-class-slots+ +slot-definition-slots+)
           (flet ((slot-definition-name (sd) (slot-definition-name sd))) ; macro, so.
             ,form))
        form)))

(defmethod make-load-form ((method effective-reader-method)
                           &optional environment)
  (declare (ignore environment))
  (let ((orig (effective-accessor-method-original method)))
    `(,(if (eq (class-of orig) (find-class 'standard-reader-method))
           'early-intern-effective-reader
           'intern-effective-reader)
      ',orig
      ',(effective-accessor-method-location method))))

(defmethod make-load-form ((method effective-writer-method)
                           &optional environment)
  (declare (ignore environment))
  (let ((orig (effective-accessor-method-original method)))
    `(,(if (eq (class-of orig) (find-class 'standard-writer-method))
           'early-intern-effective-writer
           'intern-effective-writer)
      ',orig
      ',(effective-accessor-method-location method))))

;;; ----------------------------------------------------------------------
;;; Printing
;;; ----------------------------------------------------------------------

;;; General method was moved up to fixup.lisp 
#+(or)
(defmethod print-object ((instance t) stream)
  (print-unreadable-object (instance stream)
    (let ((*package* (find-package "CL")))
      (format stream "~S"
	      (class-name (si:instance-class instance)))))
  instance)

(defmethod print-object ((class class) stream)
  (print-unreadable-object (class stream :type t)
    (write (class-name class) :stream stream))
  class)

(defmethod print-object ((gf standard-generic-function) stream)
  (print-unreadable-object (gf stream :type t)
    (write (generic-function-name gf) :stream stream))
  gf)

(defmethod print-object ((m standard-method) stream)
  (print-unreadable-object (m stream :type t)
    (format stream "~A ~{~S ~}~S"
	    (let ((gf (method-generic-function m)))
	      (if gf
		  (generic-function-name gf)
		  'UNNAMED))
            (method-qualifiers m)
	    (loop for spec in (method-specializers m)
                  collect (cond ((and (classp spec)
                                      (class-name spec)))
                                ((typep spec 'eql-specializer)
                                 `(eql ,(eql-specializer-object spec)))
                                (t spec)))))
  m)

(defmethod print-object ((s slot-definition) stream)
  (print-unreadable-object (s stream :type t)
    (write (slot-definition-name s) :stream stream))
  s)

(defmethod print-object ((mc method-combination) stream)
  (print-unreadable-object (mc stream :type t)
    (write (method-combination-name mc) :stream stream))
  mc)

(defmethod print-object ((es eql-specializer) stream)
  (print-unreadable-object (es stream :type t)
    (write (eql-specializer-object es) :stream stream))
  es)

(defmethod print-object ((obj structure-object) stream)
  (let* ((class (si:instance-class obj))
	 (slotds (class-slots class)))
    (when (and ;; to fix ansi-tests PRINT-LEVEL.8 & PRINT-LEVEL.9
           ;; printing a struct w/o slots
           ;; don't test for slotds
           ;; *p-readably* effectively disables *p-level*
           (not *print-readably*)
           *print-level*
           (zerop *print-level*))
      (write-string "#" stream)
      (return-from print-object obj))
    (write-string "#S(" stream)
    (prin1 (class-name class) stream)
    (do ((scan slotds (cdr scan))
	 (i 0 (1+ i))
	 (limit (or *print-length* most-positive-fixnum))
	 (sv))
	((null scan))
      (declare (fixnum i))
      (when (>= i limit)
	(write-string " ..." stream)
	(return))
      (setq sv (si:instance-ref obj i))
      ;; fix bug where symbols like :FOO::BAR are printed
      (write-string " " stream)
      (let ((kw (intern (symbol-name (slot-definition-name (car scan)))
                        (load-time-value (find-package "KEYWORD")))))
        (prin1 kw stream))
      (write-string " " stream)
      (if *print-level*
          (let ((*print-level* (1- *print-level*)))
            (prin1 sv stream))
          (prin1 sv stream)))
    (write-string ")" stream)
    obj))

(defmethod print-object ((object standard-object) stream)
  (print-unreadable-object (object stream :type t :identity t))
  object)

#-staging
(defmethod print-object ((l cons) stream)
  (if (cdr l)
      (case (first l)
        (eclector.reader:quasiquote
         (write-char #\` stream)
         (core:write-object (second l) stream))
        (eclector.reader:unquote
         (write-char #\, stream)
         (core:write-object (second l) stream))
        (eclector.reader:unquote-splicing
         (write-string ",@" stream)
         (core:write-object (second l) stream))
        (otherwise
         (call-next-method)))
      (call-next-method)))

(defun ext::float-nan-string (x)
  (when *print-readably*
    (error 'print-not-readable :object x))
  (cdr (assoc (type-of x)
              '((single-float . "#<single-float quiet NaN>")
                (double-float . "#<double-float quiet NaN>")
                (long-float . "#<long-float quiet NaN>")
                (short-float . "#<short-float quiet NaN>")))))

(defun ext::float-infinity-string (x)
  (when (and *print-readably* (null *read-eval*))
    (error 'print-not-readable :object x))
  (let* ((negative-infinities '((single-float .
                                 "#.ext:single-float-negative-infinity")
                                (double-float .
                                 "#.ext:double-float-negative-infinity")
                                (long-float .
                                 "#.ext:long-float-negative-infinity")
                                (short-float .
                                 "#.ext:short-float-negative-infinity")))
         (positive-infinities '((single-float .
                                 "#.ext:single-float-positive-infinity")
                                (double-float .
                                 "#.ext:double-float-positive-infinity")
                                (long-float .
                                 "#.ext:long-float-positive-infinity")
                                (short-float .
                                 "#.ext:short-float-positive-infinity")))
         (record (assoc (type-of x)
                        (if (plusp x) positive-infinities negative-infinities))))
    (unless record
      (error "Not an infinity"))
    (cdr record)))

;;; ----------------------------------------------------------------------
;;; Variable source info
;;; ----------------------------------------------------------------------
;;; Cooperates with code in lsp/evalmacros.lisp because of bootstrapping.
;;; (Specifically, being able to dump SPIs with make-load-form.)
;;; FIXME: Move if possible, export?

(defun core::variable-source-info-saver (var cspi)
  `((setf (gethash ',var core::*variable-source-infos*) ',cspi)))

(defun core::variable-source-info (var)
  (gethash var core::*variable-source-infos*))

;;; ----------------------------------------------------------------------
;;; Describe
;;; ----------------------------------------------------------------------

(defmethod describe-object ((obj t) (stream t))
  (format stream "~%~S is an instance of class ~S"
          obj (class-name (class-of obj)))
  obj)

;;; ----------------------------------------------------------------------
;;; Clasp specific methods

(defmethod cl:print-object ((object core:general) stream)
  (if (and *print-readably* (core:fieldsp object))
      (progn
        (write-string "#i" stream)
        (write (cons (class-name (class-of object)) (core:encode object)) :stream stream))
      (call-next-method)))

(in-package :core)
 
(defmacro field (node name slot-access)
  (let ((valgs (gensym)))
  `(case (core:record-stage ,node)
     ((:initializing :saving)
      (core:field-write ,node ,name ,slot-access))
     (:reading
      (let ((,valgs (core:field-read ,node ,name )))
        (setf ,slot-access ,valgs)))
     (:patching
      (let ((,valgs (core:field-patch ,node ,name ,slot-access)))
        (setf ,slot-access ,valgs))))))

(defmacro with-record-serialize-slots ((node) &rest name-slot-access-pairs)
  (let ((valgs (gensym)))
  `(case (core:record-stage ,node)
     ((:initializing :saving)
      ,@(loop for entry in name-slot-access-pairs
              for name = (car entry)
              for slot-access = (cadr entry)
              collect `(core:field-write ,node ,name ,slot-access)))
     (:reading
      ,@(loop for entry in name-slot-access-pairs
              for name = (car entry)
              for slot-access = (cadr entry)
              collect `(let ((,valgs (core:field-read ,node ,name )))
                         (setf ,slot-access ,valgs))))
     (:patching
      ,@(loop for entry in name-slot-access-pairs
              for name = (car entry)
              for slot-access = (cadr entry)
              collect `(let ((,valgs (core:field-patch ,node ,name ,slot-access)))
                         (setf ,slot-access ,valgs)))))))
(export '(field with-record-serialize-slots))

(in-package :clos)

(defun print-object-readably-with-slots (obj stream skip-slot-names)
  (format stream "#$(~s " (class-name (class-of obj)))
  (loop for slot in (clos:class-slots (class-of obj))
        for slot-name = (clos:slot-definition-name slot)
        for initargs = (clos:slot-definition-initargs slot)
        if (and (car initargs)
                (not (position slot-name skip-slot-names))
                (slot-boundp obj slot-name))
          do (format stream "~s ~s " (car initargs) (slot-value obj slot-name)))
  (format stream ") "))

(export '(print-object-readably-with-slots))
