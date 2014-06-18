;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.o
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;
;;; COMMON LISP CLASSES HIERARCHY
;;;
;;; The following set of constants describe the slots, the names of
;;; the classes and their relation, including both standard Commmon Lisp
;;; and the MetaObject Protocol. This information is only loaded when
;;; bootstrapping and compiling ECL.
;;;

(in-package "CLOS")

;;; ----------------------------------------------------------------------
;;; Class SPECIALIZER

(eval-when (:compile-toplevel :execute)
  (defparameter +specializer-slots+
    '((flag :initform nil :accessor eql-specializer-flag)
      (direct-methods :initform nil :accessor specializer-direct-methods)
      (direct-generic-functions :initform nil :accessor specializer-direct-generic-functions))))

(eval-when (:compile-toplevel :execute)
  (defparameter +eql-specializer-slots+
    '((flag :initform t :accessor eql-specializer-flag)
      (direct-methods :initform nil :accessor specializer-direct-methods)
      (direct-generic-functions :initform nil :accessor specializer-direct-generic-functions)
      (object :initarg :object :accessor eql-specializer-object))))

;;; ----------------------------------------------------------------------
;;; Class METHOD-COMBINATION

(eval-when (:compile-toplevel :execute)
  (defparameter +method-combination-slots+
    `((name :initarg :name :accessor method-combination-name)
      (compiler :initarg :compiler :accessor method-combination-compiler)
      (options :initarg :options :accessor method-combination-options))))

;;; ----------------------------------------------------------------------
;;; Class CLASS

(eval-when (:compile-toplevel :execute)
  (defparameter +class-slots+
    `(,@+specializer-slots+
      (name :initarg :name :initform nil :accessor class-id)
      (direct-superclasses :initarg :direct-superclasses
       :accessor class-direct-superclasses)
      (direct-subclasses :initform nil :accessor class-direct-subclasses)
      (slots :accessor class-slots)
      (precedence-list :accessor class-precedence-list)
      (direct-slots :initarg :direct-slots :accessor class-direct-slots)
      (direct-default-initargs :initarg :direct-default-initargs
       :initform nil :accessor class-direct-default-initargs)
      (default-initargs :accessor class-default-initargs)
      (finalized :initform nil :accessor class-finalized-p)
      (docstring :initarg :documentation :initform nil)
      (size :accessor class-size)
      (sealedp :initarg :sealedp :initform nil :accessor class-sealedp)
      (prototype)
      (dependents :initform nil :accessor class-dependents)
      (valid-initargs :accessor class-valid-initargs)
      (slot-table :accessor slot-table)
      (location-table :initform nil :accessor class-location-table)
      ))

  (defconstant +class-name-ndx+
    (position 'name +class-slots+ :key #'first))
  (defconstant +class-precedence-list-ndx+
    (position 'precedence-list +class-slots+ :key #'first)))

;;; ----------------------------------------------------------------------
;;; STANDARD-CLASS

(eval-when (:compile-toplevel :execute)
  (defparameter +standard-class-slots+
    (append +class-slots+
	    '((optimize-slot-access)
	      (forward)))))

;;; ----------------------------------------------------------------------
;;; STRUCTURE-CLASS

(eval-when (:compile-toplevel :execute)
  (defparameter +structure-class-slots+
    (append +class-slots+
	    '((slot-descriptions)
	      (initial-offset)
	      (defstruct-form)
	      (constructors)
	      (documentation)
	      (copier)
	      (predicate)
	      (print-function)))))

;;; ----------------------------------------------------------------------
;;; STANDARD-GENERIC-FUNCTION

(eval-when (:compile-toplevel :execute)
  (defparameter +standard-generic-function-slots+
    '((name :initarg :name :initform nil
       :reader generic-function-name)
      (spec-list :initform nil :accessor generic-function-spec-list)
      (method-combination 
       :initarg :method-combination :initform (find-method-combination (class-prototype (find-class 'standard-generic-function)) 'standard nil)
       :accessor generic-function-method-combination)
      (lambda-list :initarg :lambda-list
       :accessor generic-function-lambda-list)
      (argument-precedence-order 
       :initarg :argument-precedence-order
       :initform nil
       :accessor generic-function-argument-precedence-order)
      (method-class
       :initarg :method-class
       :initform (find-class 'standard-method))
      (docstring :initarg :documentation :initform nil)
      (methods :initform nil :accessor generic-function-methods)
      (a-p-o-function :initform nil :accessor generic-function-a-p-o-function)
      (declarations
       :initarg :declarations
       :initform nil
       :accessor generic-function-declarations)
      (dependents :initform nil :accessor generic-function-dependents))))

;;; ----------------------------------------------------------------------
;;; STANDARD-METHOD

(eval-when (:compile-toplevel :execute)
  (defparameter +standard-method-slots+
    '((the-generic-function :initarg :generic-function :initform nil
       :accessor method-generic-function)
      (lambda-list :initarg :lambda-list
       :accessor method-lambda-list)
      (specializers :initarg :specializers :accessor method-specializers)
      (qualifiers :initform nil :initarg :qualifiers :accessor method-qualifiers)
      (the-function :initarg :function :accessor method-function)
      (docstring :initarg :documentation :initform nil)
      (plist :initform nil :initarg :plist :accessor method-plist)
      (keywords :initform nil :accessor method-keywords)))

  (defparameter +standard-accessor-method-slots+
    (append +standard-method-slots+
	    '((slot-definition :initarg :slot-definition
		    :initform nil 
	       ;; FIXME! Should be a :reader
		    :accessor accessor-method-slot-definition)))))

;;; ----------------------------------------------------------------------
;;; SLOT-DEFINITION
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +slot-definition-slots+
    '((name :initarg :name :initform nil :accessor slot-definition-name)
      (initform :initarg :initform :initform +initform-unsupplied+ :accessor slot-definition-initform)
      (initfunction :initarg :initfunction :initform nil :accessor slot-definition-initfunction)
      (declared-type :initarg :type :initform t :accessor slot-definition-type)
      (allocation :initarg :allocation :initform :instance :accessor slot-definition-allocation)
      (initargs :initarg :initargs :initform nil :accessor slot-definition-initargs)
      (readers :initarg :readers :initform nil :accessor slot-definition-readers)
      (writers :initarg :writers :initform nil :accessor slot-definition-writers)
      (docstring :initarg :documentation :initform nil :accessor slot-definition-documentation)
      (location :initarg :location :initform nil :accessor slot-definition-location)
      )))

;;; ----------------------------------------------------------------------
(eval-when (:compile-toplevel :execute)
  ;;
  ;; All changes to this are connected to the changes in 
  ;; the code of cl_class_of() in src/instance.d
  ;;
  (defconstant +builtin-classes-list+
	 '(;(t object)
	    (sequence)
	      (list sequence)
	        (cons list)
	    (array)
	      (vector array sequence)
	        (string vector)
                #+unicode
	        (base-string string vector)
	        (bit-vector vector)
	    (stream)
	      (ext:ansi-stream stream)
		(file-stream ext:ansi-stream)
		(echo-stream ext:ansi-stream)
		(string-stream ext:ansi-stream)
		(two-way-stream ext:ansi-stream)
		(synonym-stream ext:ansi-stream)
		(broadcast-stream ext:ansi-stream)
		(concatenated-stream ext:ansi-stream)
		(ext:sequence-stream ext:ansi-stream)
	    (character)
	    (number)
	      (real number)
	        (rational real)
		  (integer rational)
		    (fixnum integer)
		    (bignum integer)
		  (ratio rational)
	        (float real)
	      (complex number)
	    (symbol)
	      (null symbol list)
	      (keyword symbol)
	    (package)
	    (function)
	    (pathname)
	      (logical-pathname pathname)
	    (hash-table)
	    (random-state)
	    (readtable)
            (si::code-block)
	    (si::foreign-data)
	    (si::frame)
	    (si::weak-pointer)
	    #+threads (mp::process)
	    #+threads (mp::lock)
	    #+threads (mp::rwlock)
	    #+threads (mp::condition-variable)
	    #+threads (mp::semaphore)
	    #+threads (mp::barrier)
	    #+threads (mp::mailbox)
	    #+sse2 (ext::sse-pack))))

;;; FROM AMOP:
;;;
;;;	Metaobject Class		Direct Superclasses
;;; 	standard-object			(t)
;;; 	funcallable-standard-object	(standard-object function)
;;; *	metaobject			(standard-object)
;;; *	generic-function		(metaobject funcallable-standard-object)
;;; 	standard-generic-function	(generic-function)
;;; *	method				(metaobject)
;;; 	standard-method			(method)
;;; *	standard-accessor-method	(standard-method)
;;; 	standard-reader-method		(standard-accessor-method)
;;; 	standard-writer-method		(standard-accessor-method)
;;; *	method-combination		(metaobject)
;;; *	slot-definition			(metaobject)
;;; *	direct-slot-definition		(slot-definition)
;;; *	effective-slot-definition	(slot-definition)
;;; *	standard-slot-definition	(slot-definition)
;;; 	standard-direct-slot-definition	(standard-slot-definition direct-slot-definition)
;;; 	standard-effective-slot-definition	(standard-slot-definition effective-slot-definition)
;;; *	specializer			(metaobject)
;;; 	eql-specializer			(specializer)
;;; *	class				(specializer)
;;; 	built-in-class			(class)
;;; 	forward-referenced-class	(class)
;;; 	standard-class			(class)
;;; 	funcallable-standard-class	(class)
;;;
(eval-when (eval)
  (defconstant +class-hierarchy+
    `((standard-class)
      (standard-effective-slot-definition)
      (standard-direct-slot-definition)
      (standard-class
       :metaclass nil ; Special-cased in boot.lsp
       :direct-slots #.+standard-class-slots+)
      (standard-direct-slot-definition
       :direct-slots #3=#.+slot-definition-slots+)
      (standard-effective-slot-definition
       :direct-slots #3#)
      (t
       :index 0)
      (standard-object
       :direct-superclasses (t))
      (metaobject
       :direct-superclasses (standard-object))
      (slot-definition
       :direct-superclasses (metaobject)
       :direct-slots #3#)
      (standard-slot-definition
       :direct-superclasses (slot-definition)
       :direct-slots #3#)
      (direct-slot-definition
       :direct-superclasses (slot-definition)
       :direct-slots #3#)
      (effective-slot-definition
       :direct-superclasses (slot-definition)
       :direct-slots #3#)
      (standard-direct-slot-definition
       :direct-superclasses (standard-slot-definition direct-slot-definition)
       :direct-slots #3#)
      (standard-effective-slot-definition
       :direct-superclasses (standard-slot-definition effective-slot-definition)
       :direct-slots #3#)
      (method-combination
       :direct-superclasses (metaobject)
       :direct-slots #.+method-combination-slots+)
      (specializer
       :direct-superclasses (metaobject)
       :direct-slots #.+specializer-slots+)
      (eql-specializer
       :direct-superclasses (specializer)
       :direct-slots #.+eql-specializer-slots+)
      (class
       :direct-superclasses (specializer)
       :direct-slots #.+class-slots+)
      (forward-referenced-class
       :direct-superclasses (class)
       :direct-slots #.+class-slots+)
      (built-in-class
       :direct-superclasses (class)
       :direct-slots #1=#.+standard-class-slots+)
      (std-class
       :direct-superclasses (class)
       :direct-slots #1#)
      (standard-class
       :direct-superclasses (std-class)
       :direct-slots #1#
       :metaclass standard-class)
      (funcallable-standard-class
       :direct-superclasses (std-class)
       :direct-slots #1#)
      ,@(loop for (name . rest) in +builtin-classes-list+
	   for index from 1
	   collect (list name :metaclass 'built-in-class
			 :index index
			 :direct-superclasses (or rest '(t))))
      (funcallable-standard-object
       :direct-superclasses (standard-object function))
      (generic-function
       :metaclass funcallable-standard-class
       :direct-superclasses (metaobject funcallable-standard-object))
      (standard-generic-function
       :direct-superclasses (generic-function)
       :direct-slots #.+standard-generic-function-slots+
       :metaclass funcallable-standard-class)
      (method
       :direct-superclasses (metaobject))
      (standard-method
       :direct-superclasses (method)
       :direct-slots #.+standard-method-slots+)
      (standard-accessor-method
       :direct-superclasses (standard-method)
       :direct-slots #2=#.+standard-accessor-method-slots+)
      (standard-reader-method
       :direct-superclasses (standard-accessor-method)
       :direct-slots #2#)
      (standard-writer-method
       :direct-superclasses (standard-accessor-method)
       :direct-slots #2#)
      (standard-optimized-reader-method
       :direct-superclasses (standard-reader-method)
       :direct-slots #2#)
      (standard-optimized-writer-method
       :direct-superclasses (standard-writer-method)
       :direct-slots #2#)
      (structure-class
       :direct-superclasses (class)
       :direct-slots #.+structure-class-slots+)
      (structure-object
       :metaclass structure-class
       :direct-superclasses (t))
      )))

