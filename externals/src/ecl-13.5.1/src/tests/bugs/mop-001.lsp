;-*- Mode:     Lisp -*-
;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Metaobject Protocol tests

(in-package :cl-test)

(use-package :clos)

(defun delete-class (&rest class-names)
  ;;; do nothing. We will figure out later what to do.
  (values))

;;; Fixed: 14/04/2006 (juanjo)
;;; Description:
;;;
;;;	The slot definitions from some classes did not get converted.
;;;	Besides, metaobject CLASS had the same list for direct and effective
;;;	slots.
;;;
(deftest mop-0001-fixup
    (block top
      (labels ((test-class (class-object)
		 (let ((x (find-if-not #'(lambda (x)
					   (typep x 'standard-direct-slot-definition))
				       (class-direct-slots class-object))))
		   (when x
		     (format t "Class ~a has as direct slot ~a" class-object x)
		     (return-from top (class-name class-object))))
		 (let ((x (find-if-not #'(lambda (x)
					   (typep x 'standard-effective-slot-definition))
				       (class-slots class-object))))
		   (when x
		     (format t "Class ~a has as effective slot ~a" class-object x)
		     (return-from top (class-name class-object))))
		 (mapc #'test-class (clos::class-direct-subclasses class-object))))
	(test-class (find-class 't))
	nil))
  nil)

;;; Date: 13/02/2006
;;; From: Dan Debertin
;;; Fixed: 24-02-2006 (juanjo)
;;; Description:
;;;
;;;	Subclasses of STANDARD-CLASS would not inherit all their slots
;;;	and thus would cause runtime errors when creating instances.
;;;

(deftest mop-0002-metaclasses
    (eval '(progn
	    (defclass foo-metaclass (standard-class) ())
	    (defclass faa () ((a :initform 2 :initarg :a)) (:metaclass foo-metaclass))
	    (prog1 (slot-value (make-instance 'faa :a 3) 'a)
	      (cl-test::delete-class 'foo-metaclass 'faa))))
  3)

;;; Date: 02/03/2006
;;; From: Pascal Costanza
;;; Fixed: 07/03/2006 (juanjo)
;;; Description:
;;;
;;;	CLOS should export the symbols from the AMOP.
;;;


(defconstant +mop-symbols+ '("DIRECT-SLOT-DEFINITION"
"EFFECTIVE-SLOT-DEFINITION" "EQL-SPECIALIZER" "FORWARD-REFERENCED-CLASS"
"FUNCALLABLE-STANDARD-CLASS" "FUNCALLABLE-STANDARD-OBJECT" "METAOBJECT"
"SLOT-DEFINITION" "SPECIALIZER" "STANDARD-ACCESSOR-METHOD"
"STANDARD-DIRECT-SLOT-DEFINITION" "STANDARD-EFFECTIVE-SLOT-DEFINITION"
"STANDARD-READER-METHOD" "STANDARD-SLOT-DEFINITION" "STANDARD-WRITER-METHOD"
"ACCESSOR-METHOD-SLOT-DEFINITION" "ADD-DEPENDENT" "ADD-DIRECT-METHOD"
"ADD-DIRECT-SUBCLASS" "CLASS-DEFAULT-INITARGS"
"CLASS-DIRECT-DEFAULT-INITARGS" "CLASS-DIRECT-SLOTS"
"CLASS-DIRECT-SUBCLASSES" "CLASS-DIRECT-SUPERCLASSES" "CLASS-FINALIZED-P"
"CLASS-PRECEDENCE-LIST" "CLASS-PROTOTYPE" "CLASS-SLOTS"
"COMPUTE-APPLICABLE-METHODS-USING-CLASSES" "COMPUTE-CLASS-PRECEDENCE-LIST"
"COMPUTE-DEFAULT-INITARGS" "COMPUTE-DISCRIMINATING-FUNCTION"
"COMPUTE-EFFECTIVE-METHOD" "COMPUTE-EFFECTIVE-SLOT-DEFINITION"
"COMPUTE-SLOTS" "DIRECT-SLOT-DEFINITION-CLASS"
"EFFECTIVE-SLOT-DEFINITION-CLASS" "ENSURE-CLASS" "ENSURE-CLASS-USING-CLASS"
"ENSURE-GENERIC-FUNCTION-USING-CLASS" "EQL-SPECIALIZER-OBJECT"
"EXTRACT-LAMBDA-LIST" "EXTRACT-SPECIALIZER-NAMES" "FINALIZE-INHERITANCE"
"FIND-METHOD-COMBINATION" "FUNCALLABLE-STANDARD-INSTANCE-ACCESS"
"GENERIC-FUNCTION-ARGUMENT-PRECEDENCE-ORDER"
"GENERIC-FUNCTION-DECLARATIONS" "GENERIC-FUNCTION-LAMBDA-LIST"
"GENERIC-FUNCTION-METHOD-CLASS" "GENERIC-FUNCTION-METHOD-COMBINATION"
"GENERIC-FUNCTION-METHODS" "GENERIC-FUNCTION-NAME" "INTERN-EQL-SPECIALIZER"
"MAKE-METHOD-LAMBDA" "MAP-DEPENDENTS" "METHOD-FUNCTION"
"METHOD-GENERIC-FUNCTION" "METHOD-LAMBDA-LIST" "METHOD-SPECIALIZERS"
"READER-METHOD-CLASS" "REMOVE-DEPENDENT" "REMOVE-DIRECT-METHOD"
"REMOVE-DIRECT-SUBCLASS" "SET-FUNCALLABLE-INSTANCE-FUNCTION"
"SLOT-BOUNDP-USING-CLASS" "SLOT-DEFINITION-ALLOCATION"
"SLOT-DEFINITION-INITARGS" "SLOT-DEFINITION-INITFORM"
"SLOT-DEFINITION-INITFUNCTION" "SLOT-DEFINITION-LOCATION"
"SLOT-DEFINITION-NAME" "SLOT-DEFINITION-READERS" "SLOT-DEFINITION-WRITERS"
"SLOT-DEFINITION-TYPE" "SLOT-MAKUNBOUND-USING-CLASS"
"SLOT-VALUE-USING-CLASS" "SPECIALIZER-DIRECT-GENERIC-FUNCTIONS"
"SPECIALIZER-DIRECT-METHODS" "STANDARD-INSTANCE-ACCESS" "UPDATE-DEPENDENT"
"VALIDATE-SUPERCLASS" "WRITER-METHOD-CLASS"))

(deftest mop-0003-symbols
    (let ((*package* (find-package "CLOS")))
      (and (remove-if #'(lambda (x)
			  (multiple-value-bind (s t)
			      (find-symbol x *package*)
			    (and s (eq t :external))))
		      +mop-symbols+)
	   t))
  nil)

;;; Date: 02/03/2006
;;; From: Dank Corkill
;;; Fixed: 02-03-2006 (Dan Corkill)
;;; Description:
;;;
;;;	DEFCLASS allows additional options which should be handled by the
;;;	metaclass.
;;;

(deftest mop-0004-defclass-options
    (eval '(let ((*aux* 5))
	    (declare (special *aux*))
	    (defclass foo-metaclass (standard-class) ())
	    (defmethod shared-initialize ((class foo-metaclass) slot-names
					   &rest initargs &key option)
	      (prog1 (call-next-method)
		(setf *aux* option)))
	    (defclass faa ()
	      ((a :initform *aux* :initarg :a))
	      (:metaclass foo-metaclass)
	      (:option t))
	    (prog1 (slot-value (make-instance 'faa) 'a)
	      (cl-test::delete-class 'foo-metaclass 'faa))))
  (T))

;;; Date: 02/03/2006
;;; From: Dank Corkill
;;; Fixed: 02-03-2006 (Dan Corkill)
;;; Description:
;;;
;;;	Readers and writers for slot documentation.
;;;

(deftest mop-0004b-slot-documentation
    (eval '(progn
	    (defclass fee ()
	      ((a :initform *aux* :initarg :a)))
	    (setf (documentation (first (clos:class-slots (find-class 'fee))) t)
	     #1="hola")
	    (documentation (first (clos:class-slots (find-class 'fee))) t)))
  #1#)

;;; Date: 25/03/2006
;;; From: Pascal Costanza
;;; Fixed: 03/04/2006 (juanjo)
;;; Description:
;;;
;;;	The default slot setter methods had the first argument
;;;	(i.e. the new value) specialized to NIL. This makes it
;;;	impossible to write further specializations.
;;;

(deftest mop-0005-setf-specializer
    (progn
      (defclass fee ()
	((a :accessor fee-a)))
      (prog1
	  (list
	   (mapcar #'class-name
		   (method-specializers (first (generic-function-methods #'(setf fee-a)))))
	   (mapcar #'class-name
		   (method-specializers (first (generic-function-methods #'fee-a)))))
	(delete-class 'fee)))
  ((t fee) (fee)))

;;; Date: 06/04/2006
;;; From: Pascal Costanza
;;; Fixed: ---
;;; Description:
;;;
;;;	When a required argument in a method is not explicitely given
;;;	an specializer, the specializer should be T. Thus
;;;		(defmethod foo (a))
;;;	is equivalent to
;;;		(defmethod foo ((a t)))
;;;

(deftest mop-0006-method-specializer
    (progn
      (defmethod mop-0006-foo (a))
      (prog1
	  (method-specializers (first (generic-function-methods #'mop-0006-foo)))
	(fmakunbound 'mop-0006-foo)))
  (#.(find-class t)))

;;; Date: 22/04/2006
;;; From: M. Goffioul
;;; Fixed: 23/04/2006 (juanjo)
;;; Description:
;;;
;;;	When a class inherits from two other classes which have a slot
;;;	with the same name, the new class should inherit the accessors
;;;	from both classes.
;;;

(deftest mop-0007-slot-inheritance
    (progn
      (defclass fee-1 ()
	((slot-0 :initform 0 :reader slot-0)
	 (slot-1 :initform 1 :reader slot-1)))
      (defclass fee-2 ()
	((slot-0 :initform 2 :reader slot-2)))
      (defclass fee-3 (fee-1 fee-2)
	((slot-0 :initform 3 :accessor c-slot-0)))
      (flet ((accessors (class)
	       (list (class-name class)
		     (mapcar #'slot-definition-readers (class-slots class))
		     (mapcar #'slot-definition-readers (class-slots class)))))
	(prog1
	    (list (accessors (find-class 'fee-1))
		  (accessors (find-class 'fee-2))
		  (accessors (find-class 'fee-3))
		  (mapcar #'(lambda (o)
			      (mapcar #'(lambda (method)
					  (handler-case (funcall method o)
					    (error (c) nil)))
				      '(slot-0 slot-2 c-slot-0)))
			  (mapcar #'make-instance '(fee-1 fee-2 fee-3))))
	  (delete-class 'fee-1 'fee-2 'fee-3))))
  ((fee-1 ((slot-0) (slot-1)) ((slot-0) (slot-1)))
   (fee-2 ((slot-2)) ((slot-2)))
   (fee-3 ((c-slot-0 slot-0 slot-2) (slot-1))
	  ((c-slot-0 slot-0 slot-2) (slot-1)))
   ((0 nil nil)
    (nil 2 nil)
    (3 3 3))))


;;; Date: 28/04/2006
;;; From: P. Costanza
;;; Fixed: 05/05/2006 (P. Costanza)
;;; Description:
;;;
;;;	Option names from classes and generic functions which are not
;;;	in the keyword package should be quoted. This test is
;;;	essentially like mop-0004-... because our DEFGENERIC does not
;;;	support non-keyword options.
;;;

(deftest mop-0008-defclass-option-quote
    (eval '(let ((*aux* 5))
	    (declare (special *aux*))
	    (defclass foo-metaclass (standard-class) ())
	    (defmethod shared-initialize ((class foo-metaclass) slot-names
					  &rest initargs &key ((cl-user::option option)))
	      (prog1 (call-next-method)
		(setf *aux* option)))
	    (defclass faa ()
	      ((a :initform *aux* :initarg :a))
	      (:metaclass foo-metaclass)
	      (cl-user::option t))
	    (prog1 (slot-value (make-instance 'faa) 'a)
	      (cl-test::delete-class 'foo-metaclass 'faa))))
  (t))


;;; Date: 05/10/2006
;;; From: Rick Taube
;;; Fixed: 10/10/2006 (juanjo)
;;; Description:
;;;
;;;	:INITFORM arguments do not get properly expanded when the form
;;;	is a constant variable.
;;;
;;;	(defclass a () ((a :initform most-positive-fixnum)))
;;;	(slot-value (make-instance a) 'a) => most-positive-fixnum
;;;

(deftest mop-0009-defclass-initform
    (loop for quoting in '(nil t)
	  collect
	  (loop for f in '(most-positive-fixnum #1=#.(lambda () 1) 12 "hola" :a t nil)
		collect (prog1 (eval `(progn
				       (defclass foo () ((a :initform ,(if quoting (list 'quote f) f))))
				       (slot-value (make-instance 'foo) 'a)))
			  (cl-test::delete-class 'foo))))
  ((#.most-positive-fixnum #1# 12 "hola" :a t nil)
   (most-positive-fixnum #1# 12 "hola" :a t nil)))
