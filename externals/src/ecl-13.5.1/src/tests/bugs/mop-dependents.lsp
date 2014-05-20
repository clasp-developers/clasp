;-*- Mode:     Lisp -*-
;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Sat Apr 23 09:02:03 CEST 2012
;;;; Contains: Metaobject Protocol tests

(in-package :cl-test)

(defclass mop-dependent-object ()
  ((log :initform nil :initarg :log :accessor mop-dependent-object-log)))

(defmethod update-dependent ((object t) (dep mop-dependent-object) &rest initargs)
  (push (list* object initargs) (mop-dependent-object-log dep)))

;;; Date: 23/04/2012
;;; Description:
;;;
;;;	ADD-DEPENDENT uses pushnew
;;;
(deftest mop-gf-add-non-redundant
    (let* ((dep (make-instance 'mop-dependent-object))
	   l1 l2)
      (fmakunbound 'mop-gf-add/remove-dependent)
      (defgeneric mop-gf-add/remove-dependent (a))
      (let ((f #'mop-gf-add/remove-dependent))
	(clos:add-dependent f dep)
	(setf l1 (clos::generic-function-dependents f))
	(clos:add-dependent f dep)
	(setf l2 (clos::generic-function-dependents f))
	(and (eq l1 l2)
	     (equalp l1 (list dep))
	     t)))
  t)

;;; Date: 23/04/2012
;;; Description:
;;;
;;;	Generic functions have dependents and are activated
;;;
(deftest mop-gf-add/remove-dependent
    (let* ((dep (make-instance 'mop-dependent-object))
	   l1 l2 l3 l4 l5 l6)
      (fmakunbound 'mop-gf-add/remove-dependent)
      (defgeneric mop-gf-add/remove-dependent (a))
      (let ((f #'mop-gf-add/remove-dependent)
	    m1 m2)
	;;
	;; * ADD-DEPENDENT registers the object with the function
	;;
	(clos:add-dependent f dep)
	(setf l1 (clos::generic-function-dependents f))
	;;
	;; * ADD-METHOD invokes UPDATE-DEPENDENT
	;;
	(defmethod mop-gf-add/remove-dependent ((a number)) (cos a))
	(setf l2 (mop-dependent-object-log dep))
	;;
	;; * REMOVE-METHOD invokes UPDATE-DEPENDENT
	;;
	(setf m1 (first (compute-applicable-methods f (list 1.0))))
	(remove-method f m1)
	(setf l3 (mop-dependent-object-log dep))
	;;
	;; * REMOVE-DEPENDENT eliminates all dependencies
	;;
	(clos:remove-dependent f dep)
	(setf l4 (clos::generic-function-dependents f))
	;;
	;; * ADD-METHOD invokes UPDATE-DEPENDENT but has no effect
	;;
	(defmethod mop-gf-add/remove-dependent ((a symbol)) a)
	(setf l5 (mop-dependent-object-log dep))
	;;
	;; * REMOVE-METHOD invokes UPDATE-DEPENDENT but has no effect
	;;
	(setf m2 (first (compute-applicable-methods f (list 'a))))
	(setf l6 (mop-dependent-object-log dep))
	;; the first call to defmethod adds two entries: one for the
	;; add-method and another one for a reinitialize-instance with
	;; the name of the function
	(and (equalp l1 (list dep))
	     (eq l2 (rest l3))
	     (equalp l3
		     (list (list f 'remove-method m1)
			   (list f 'add-method m1)
			   (list f :name 'mop-gf-add/remove-dependent)))
	     (null l4)
	     (eq l5 l3)
	     (eq l6 l3)
	     t)))
  t)

;;; Date: 23/04/2012
;;; Description:
;;;
;;;	ADD-DEPENDENT does not duplicate elements
;;;
(deftest mop-class-add/remove-dependent
    (let* ((dep (make-instance 'mop-dependent-object))
	   l1 l2)
      (when (find-class 'mop-class-add/remove-dependent nil)
	(setf (class-name (find-class 'mop-class-add/remove-dependent)) nil))
      (defclass mop-class-add/remove-dependent () ())
      (let ((f (find-class 'mop-class-add/remove-dependent)))
	(clos:add-dependent f dep)
	(setf l1 (clos::class-dependents f))
	(clos:add-dependent f dep)
	(setf l2 (clos::class-dependents f))
	(and (eq l1 l2)
	     (equalp l1 (list dep))
	     t)))
  t)

;;; Date: 23/04/2012
;;; Description:
;;;
;;;	Standard classes have dependents and are activated
;;;
(deftest mop-class-add/remove-dependent
    (let* ((dep (make-instance 'mop-dependent-object))
	   l1 l2 l3 l4 l5)
      (when (find-class 'mop-class-add/remove-dependent nil)
	(setf (class-name (find-class 'mop-class-add/remove-dependent)) nil))
      (defclass mop-class-add/remove-dependent () ())
      (let ((f (find-class 'mop-class-add/remove-dependent)))
	;;
	;; * ADD-DEPENDENT registers the object with the class
	;;
	(clos:add-dependent f dep)
	(setf l1 (clos::class-dependents f))
	;;
	;; * SHARED-INITIALIZE invokes UPDATE-DEPENDENT
	;;
	(defclass mop-class-add/remove-dependent () (a))
	(setf l2 (clos::class-dependents f))
	(setf l3 (mop-dependent-object-log dep))
	;;
	;; * REMOVE-DEPENDENT eliminates object from list
	;;
	(clos:remove-dependent f dep)
	(setf l4 (clos::class-dependents f))
	;;
	;; * SHARED-INITIALIZE invokes UPDATE-DEPENDENT without effect
	;;
	(defclass mop-class-add/remove-dependent () ())
	(setf l5 (mop-dependent-object-log dep))
	;;
	;; the first call to defclass adds one entry with the reinitialization
	;; of the class both in name and list of slots
	(and (equalp l1 (list dep))
	      (eq l1 l2)
	      (equalp l3
		      (list (list f :name 'mop-class-add/remove-dependent
				  :direct-superclasses nil
				  :direct-slots '((:name a)))))
	      (null l4)
	      (eq l5 l3)
	      t)))
  t)

