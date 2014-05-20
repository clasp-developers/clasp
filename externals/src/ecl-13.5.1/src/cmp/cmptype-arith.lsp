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

;;;; CMPTYPE-ARITH -- Operations upon and among types

(in-package #-new-cmp "COMPILER" #+new-cmp "C-TYPES")

;;; CL-TYPE is any valid type specification of Common Lisp.
;;;
;;; TYPE is a representation type used by ECL.  TYPE is one of:
;;;
;;;				T(BOOLEAN)
;;;
;;;	FIXNUM  CHARACTER  SINGLE-FLOAT  DOUBLE-FLOAT
;;;	(VECTOR T)  STRING  BIT-VECTOR  (VECTOR FIXNUM)
;;;	(VECTOR SINGLE-FLOAT)  (VECTOR DOUBLE-FLOAT)
;;;	(ARRAY T)  (ARRAY BASE-CHAR)  (ARRAY BIT)
;;;	(ARRAY FIXNUM)
;;;	(ARRAY SINGLE-FLOAT)  (ARRAY DOUBLE-FLOAT)
;;;	STANDARD-OBJECT STRUCTURE-OBJECT
;;;	SYMBOL
;;;	UNKNOWN
;;;
;;;				NIL
;;;
;;;
;;; immediate-type:
;;;	FIXNUM		int
;;;	CHARACTER	char
;;;	SINGLE-FLOAT	float
;;;	DOUBLE-FLOAT	double

(deftype any () 't)

(defun member-type (type disjoint-supertypes)
  (member type disjoint-supertypes :test #'subtypep))

;;; Check if THING is an object of the type TYPE.
;;; Depends on the implementation of TYPE-OF.
;;; (only used for saving constants?)
#-new-cmp
(defun object-type (thing)
  (let ((type (if thing (type-of thing) 'SYMBOL)))
    (case type
      ((FIXNUM SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT SYMBOL NULL) type)
      ((BASE-CHAR STANDARD-CHAR CHARACTER EXTENDED-CHAR) 'CHARACTER)
      ((STRING BASE-STRING BIT-VECTOR) type)
      (VECTOR (list 'VECTOR (array-element-type thing)))
      (ARRAY (list 'ARRAY (array-element-type thing)))
      #+clos
      (STANDARD-OBJECT 'STANDARD-OBJECT)
      #+clos
      (STRUCTURE-OBJECT 'STRUCTURE-OBJECT)
      #+sse2
      ((EXT:SSE-PACK EXT:INT-SSE-PACK EXT:FLOAT-SSE-PACK EXT:DOUBLE-SSE-PACK) type)
      (t t))))

(defun valid-type-specifier (type)
  (handler-case
     (if (subtypep type 'T)
	 (values t type)
         (values nil nil))
    (error (c) (values nil nil))))

(defun known-type-p (type)
  (subtypep type T))

(defun trivial-type-p (type)
  (subtypep T type))

(defun-equal-cached type-and (t1 t2)
  ;; FIXME! Should we allow "*" as type name???
  (when (or (eq t1 t2) (eq t2 '*))
    (return-from type-and t1))
  (when (eq t1 '*)
    (return-from type-and t2))
  (let* ((si::*highest-type-tag* si::*highest-type-tag*)
	 (si::*save-types-database* t)
	 (si::*member-types* si::*member-types*)
	 (si::*elementary-types* si::*elementary-types*)
	 (tag1 (si::safe-canonical-type t1))
	 (tag2 (si::safe-canonical-type t2)))
    (cond ((and (numberp tag1) (numberp tag2))
	   (setf tag1 (si::safe-canonical-type t1)
		 tag2 (si::safe-canonical-type t2))
	   (cond ((zerop (logand tag1 tag2)) ; '(AND t1 t2) = NIL
		  NIL)
		 ((zerop (logandc2 tag1 tag2)) ; t1 <= t2
		  t1)
		 ((zerop (logandc2 tag2 tag1)) ; t2 <= t1
		  t2)
		 (t
		  `(AND ,t1 ,t2))))
	  ((eq tag1 'CONS)
	   (cmpwarn "Unsupported CONS type ~S. Replacing it with T." t1)
	   t2)
	  ((eq tag2 'CONS)
	   (cmpwarn "Unsupported CONS type ~S. Replacing it with T." t2)
	   t1)
	  ((null tag1)
           ;(setf c::*compiler-break-enable* t) (break)
	   (cmpnote "Unknown type ~S. Assuming it is T." t1)
	   t2)
	  (t
           ;(setf c::*compiler-break-enable* t) (break)
	   (cmpnote "Unknown type ~S. Assuming it is T." t2)
	   t1))))

(defun values-number-from-type (type)
  (cond ((or (eq type 'T) (eq type '*))
         (values 0 MULTIPLE-VALUES-LIMIT))
        ((or (atom type) (not (eq (first type) 'VALUES)))
         (values 1 1))
        ((or (member '&rest type) (member '&optional type))
         (values 0 MULTIPLE-VALUES-LIMIT))
        (t
         (let ((l (1- (length type))))
           (values l l)))))

(defun-equal-cached values-type-primary-type (type)
  ;; Extract the type of the first value returned by this form. We are
  ;; pragmatic and thus (VALUES) => NULL  [CHECKME!]
  (when (and (consp type) (eq (first type) 'VALUES))
    (if (null (rest type))
	(setf type 'null)
	(let ((subtype (second type)))
	  (when (or (eq subtype '&optional) (eq subtype '&rest))
	    (setf type (cddr type))
	    (when (or (null type)
		      (eq (setf subtype (first type)) '&optional)
		      (eq subtype '&rest))
	      (cmperr "Syntax error in type expression ~S" type))
	    ;; An &optional or &rest output value might be missing
	    ;; If this is the case, the the value will be NIL.
	    (setf subtype (type-or 'null subtype)))
	  (setf type subtype))))
  type)

(defun-equal-cached values-type-to-n-types (type length)
  (if (or (atom type) (not (eql (first type) 'values)))
      (and (plusp length)
           (list* type (make-list (1- length) :initial-element 'NULL)))
      (do* ((l (rest type))
            (output '())
            (n length (1- n)))
           ((or (null l) (zerop n)) (nreverse output))
        (let ((type (pop l)))
          (case type
            (&optional
             (when (null l)
               (cmperr "Syntax error in type expression ~S" type))
             (setf type (pop l)))
            (&rest
             (when (null l)
               (cmperr "Syntax error in type expression ~S" type))
             (return-from values-type-to-n-types
               (nreconc output (make-list n :initial-element (first l))))))
          (push type output)))))

(defun split-values-type (type)
  (if (or (atom type) (not (eq (first type) 'VALUES)))
      (values (list type) nil nil)
      (let ((rest (member '&rest type))
            (opt (member '&optional type)))
        (values (ldiff (rest type) (or rest opt))
                (ldiff (rest (member '&optional type)) rest)
                (rest (member '&rest type))))))

(defun-equal-cached values-type-or (t1 t2)
  (when (or (eq t2 'T) (equalp t2 '(VALUES &REST T)))
    (return-from values-type-or t2))
  (when (or (eq t1 'T) (equalp t1 '(VALUES &REST T)))
    (return-from values-type-or t1))
  (unless t1
    (return-from values-type-or t2))
  (unless t2
    (return-from values-type-or t1))
  (multiple-value-bind (req1 opt1 rest1)
      (split-values-type t1)
    (multiple-value-bind (req2 opt2 rest2)
        (split-values-type t2)
      (let ((req '())
            (opt '())
            (rest '()))
        (loop for t1 in req1
           do (cond (req2
                     (push (type-or t1 (pop req2)) req))
                    (opt2
                     (push (type-or t1 (pop opt2)) opt))
                    (rest2
                     (push (type-or t1 (first rest2)) opt))
                    (t
                     (push t1 opt))))
        (loop for t1 in opt1
           do (cond (req2
                     (push (type-or t1 (pop req2)) opt))
                    (opt2
                     (push (type-or t1 (pop opt2)) opt))
                    (rest2
                     (push (type-or t1 (first rest2)) opt))
                    (t
                     (push t1 opt))))
        (let ((t1 (if rest1 (first rest1) t)))
          (loop for t2 in req2
             do (push (type-or t1 t2) opt))
          (loop for t2 in opt2
             do (push (type-or t1 t2) opt))
          (if rest2
              (setf rest (list (type-or t1 (first rest2))))
              (setf rest rest1)))
        `(VALUES ,@(nreverse req)
                 ,@(and opt (cons '&optional (nreverse opt)))
                 ,@(and rest (cons '&optional rest)))))))

(defun-equal-cached values-type-and (t1 t2)
  (when (or (eq t2 'T) (equalp t2 '(VALUES &REST T)))
    (return-from values-type-and t1))
  (when (or (eq t1 'T) (equalp t1 '(VALUES &REST T)))
    (return-from values-type-and t2))
  (when (or (null t1) (null t2))
    (return-from values-type-and nil))
  (multiple-value-bind (req1 opt1 rest1)
      (split-values-type t1)
    (multiple-value-bind (req2 opt2 rest2)
        (split-values-type t2)
      (let ((req '())
            (opt '())
            (rest '()))
        (loop for t1 in req1
           do (cond (req2 (push (type-and t1 (pop req2)) req))
                    (opt2 (push (type-and t1 (pop opt2)) req))
                    (rest2 (push (type-and t1 (first rest2)) req))
                    (t (setf opt1 nil rest1 nil) (return))))
        (loop for t1 in opt1
           do (cond (req2 (push (type-and t1 (pop req2)) req))
                    (opt2 (push (type-and t1 (pop opt2)) opt))
                    (rest2 (push (type-and t1 (first rest2)) opt))
                    (t (setf opt1 nil rest1 nil) (return))))
        (when rest
          (let ((t1 (first rest)))
            (loop for t2 in req2
               do (push (type-and t1 t2) req))
            (loop for t2 in opt2
               do (push (type-and t1 t2) opt))
            (when rest2
              (setf rest (list (type-and t1 (first rest2)))))))
        `(VALUES ,@(nreverse req)
                 ,@(and opt (cons '&optional (nreverse opt)))
                 ,@(and rest (cons '&optional rest)))))))

(defun-equal-cached type-or (t1 t2)
  ;; FIXME! Should we allow "*" as type name???
  (when (or (eq t1 t2) (eq t2 '*))
    (return-from type-or t1))
  (when (eq t1 '*)
    (return-from type-or t2))
  (let* ((si::*highest-type-tag* si::*highest-type-tag*)
	 (si::*save-types-database* t)
	 (si::*member-types* si::*member-types*)
	 (si::*elementary-types* si::*elementary-types*)
	 (tag1 (si::safe-canonical-type t1))
	 (tag2 (si::safe-canonical-type t2)))
    (cond ((and (numberp tag1) (numberp tag2))
	   (setf tag1 (si::safe-canonical-type t1)
		 tag2 (si::safe-canonical-type t2))
	   (cond ((zerop (logandc2 tag1 tag2)) ; t1 <= t2
		  t2)
		 ((zerop (logandc2 tag2 tag1)) ; t2 <= t1
		  t1)
		 (t
		  `(OR ,t1 ,t2))))
	  ((eq tag1 'CONS)
	   (cmpwarn "Unsupported CONS type ~S. Replacing it with T." t1)
	   T)
	  ((eq tag2 'CONS)
	   (cmpwarn "Unsupported CONS type ~S. Replacing it with T." t2)
	   T)
	  ((null tag1)
	   ;(break)
	   (cmpnote "Unknown type ~S" t1)
	   T)
	  (t
	   ;(break)
	   (cmpnote "Unknown type ~S" t2)
	   T))))

(defun type>= (type1 type2)
  (subtypep type2 type1))

