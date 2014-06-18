;-*- Mode:     Lisp -*-
;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Sat Apr 23 10:18:00 CEST 2012
;;;; Contains: Metaobject Protocol tests

(in-package :cl-test)

;;; Date: 23/04/2012
;;; Description:
;;;
;;;	COMPUTE-APPLICABLE-METHODS-USING-CLASSES works with one and
;;;	two methods and no EQL.
;;;
(deftest mop-c-a-m-u-c-two-methods
    (progn
      (fmakunbound 'mop-fn)
      (defgeneric mop-fn (a)
	(:method ((a number)) (cos a))
	(:method ((a symbol)) a))
      (let ((m1 (compute-applicable-methods #'mop-fn (list 1.0)))
	    (m2 (compute-applicable-methods #'mop-fn (list 'a))))
	(flet ((f (class)
		 (multiple-value-list (clos:compute-applicable-methods-using-classes
				       #'mop-fn (list (find-class class))))))
	  (and (equalp (f 'number) (list m1 t))
	       (equalp (f 'real) (list m1 t))
	       (equalp (f 'symbol) (list m2 t))
	       (equalp (f 'cons) '(nil t))
	       t))))
  t)

;;; Date: 23/04/2012
;;; Description:
;;;
;;;	COMPUTE-APPLICABLE-METHODS-USING-CLASSES fails with EQL specializers
;;;	when one of the specializers is covered by the classes.
;;;
(deftest mop-c-a-m-u-c-fails-with-eql
    (progn
      (fmakunbound 'mop-fn)
      (defgeneric mop-fn (a)
	(:method ((a (eql 1))) 1)
	(:method ((a (eql 'a))) 2)
	(:method ((a float)) 3))
      (let ((m1 (compute-applicable-methods #'mop-fn (list 1)))
	    (m2 (compute-applicable-methods #'mop-fn (list 'a)))
	    (m3 (compute-applicable-methods #'mop-fn (list 1.0))))
	(flet ((f (class)
		 (multiple-value-list (clos:compute-applicable-methods-using-classes
				       #'mop-fn (list (find-class class))))))
	  (and (equalp (f 'integer) (list nil nil))
	       (equalp (f 'number) (list nil nil))
	       (equalp (f 'symbol) (list nil nil))
	       (equalp (f 'float) (list m3 t))
	       (= (length m1) 1)
	       (= (length m2) 1)
	       (= (length m3) 1)
	       t))))
  t)

;;; Date: 24/04/2012
;;; Description:
;;;
;;;	COMPUTE-DISCRIMINATING-FUNCTION is invoked and honored by ECL.
;;;
(deftest mop-discriminator
    (progn
      (fmakunbound 'foo)
      (defclass my-generic-function (standard-generic-function)
	())
      (defmethod clos:compute-discriminating-function ((gf my-generic-function))
	;; We compute the invocaions of c-d-f. Note that it is invoked
	;; quite often -- we could probably optimize this.
	#'(lambda (&rest args)
	    args))
      (defgeneric foo (a)
	(:generic-function-class my-generic-function))
      (unwind-protect
	   (foo 2)
	(fmakunbound 'foo)))
  (2))

;;; Date: 24/04/2012
;;; Description:
;;;
;;;	COMPUTE-DISCRIMINATING-FUNCTION is invoked on ADD-METHOD, REMOVE-METHOD,
;;;	DEFGENERIC, INITIALIZE-INSTANCE and REINITIALIZE-INSTANCE acting on
;;;	generic functions.
;;;
(deftest mop-discriminator-recomputation
    (progn
      (defparameter *mop-discriminator-recomputation* 0)
      (fmakunbound 'foo)
      (defclass my-generic-function (standard-generic-function)
	())
      (defmethod clos:compute-discriminating-function ((gf my-generic-function))
	;; We compute the invocaions of c-d-f. Note that it is invoked
	;; quite often -- we could probably optimize this.
	(incf *mop-discriminator-recomputation*)
	(call-next-method))
      (and (progn
	     (setf *mop-discriminator-recomputation* 0)
	     (eval '(defgeneric foo (a)
		     (:generic-function-class my-generic-function)))
	     (plusp *mop-discriminator-recomputation* ))
	   (typep #'foo 'my-generic-function)
	   (progn
	     (setf *mop-discriminator-recomputation* 0)
	     (eval '(defmethod foo ((a number)) (print a)))
	     (plusp *mop-discriminator-recomputation*))
	   (progn
	     (setf *mop-discriminator-recomputation* 0)
	     (eval '(remove-method #'foo (first (compute-applicable-methods
						 #'foo
						 (list 1.0)))))
	     (plusp *mop-discriminator-recomputation*))
	   t))
  t)

;;; Date: 24/04/2012
;;; Description:
;;;
;;;	Verify ECL calls COMPUTE-APPLICABLE-METHODS-USING-CLASSES for
;;;	user-defined generic function classes.
;;;
(deftest mop-compute-applicable-methods-using-classes-is-honored
    (progn
      (defparameter *mop-dispatch-used* 0)
      (fmakunbound 'foo)
      (defclass my-generic-function (standard-generic-function)
	())
      (defmethod clos:compute-applicable-methods-using-classes
	  ((gf my-generic-function) classes)
	(incf *mop-dispatch-used*)
	(call-next-method))
      (defgeneric foo (a)
	(:generic-function-class my-generic-function)
	(:method ((a number)) (cos 1.0)))
      (and (zerop *mop-dispatch-used*)
	   (progn (foo 1.0) (plusp *mop-dispatch-used*))))
  t)

;;; Date: 24/04/2012
;;; Description:
;;;
;;;	Verify ECL calls COMPUTE-APPLICABLE-METHODS for
;;;	user-defined generic function classes.
;;;
(deftest mop-compute-applicable-methods-is-honored
    (progn
      (defparameter *mop-dispatch-used* 0)
      (fmakunbound 'foo)
      (defclass my-generic-function (standard-generic-function)
	())
      (defmethod clos:compute-applicable-methods-using-classes
	  ((gf my-generic-function) classes)
	(incf *mop-dispatch-used*)
	(values nil nil))
      (defmethod compute-applicable-methods
	  ((gf my-generic-function) args)
	(incf *mop-dispatch-used*)
	(call-next-method))
      (defgeneric foo (a)
	(:generic-function-class my-generic-function)
	(:method ((a number)) (cos 1.0)))
      (and (zerop *mop-dispatch-used*)
	   (progn (foo 1.0) (= *mop-dispatch-used* 2))))
  t)

