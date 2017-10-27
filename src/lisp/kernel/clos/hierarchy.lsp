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

#+(or)
(eval-when (:compile-toplevel :execute :load-toplevel)
  (push :mlog *features*)
  (defmacro mlog (fmt &rest fmtargs)
    `(core:bformat *debug-io* ,fmt ,@fmtargs)))
;;#+(or)
(defmacro mlog (fmt &rest fmtargs) nil)


#+clasp(defvar *optimize-slot-access* t
	 "In ECL this is in ecl/src/c/symbol_table.h")

;;; ----------------------------------------------------------------------
;;; Class SPECIALIZER

(eval-when (:compile-toplevel :execute #+clasp :load-toplevel)
  (defparameter +specializer-slots+
    '((flag :initform nil :accessor eql-specializer-flag)
      (direct-methods :initform nil :accessor specializer-direct-methods)
      (direct-generic-functions :initform nil :accessor specializer-direct-generic-functions)
      (call-history-generic-functions :initform nil :accessor specializer-call-history-generic-functions)
      (specializer-mutex :initform (mp:make-shared-mutex 'call-history-generic-functions-mutex)
       :accessor specializer-mutex))))

(eval-when (:compile-toplevel :execute #+clasp :load-toplevel)
  (defparameter +eql-specializer-slots+
    '((flag :initform t :accessor eql-specializer-flag)
      (direct-methods :initform nil :accessor specializer-direct-methods)
      (direct-generic-functions :initform nil :accessor specializer-direct-generic-functions)
      (object :initarg :object :accessor eql-specializer-object))))

;;; ----------------------------------------------------------------------
;;; Class METHOD-COMBINATION

(eval-when (:compile-toplevel :execute #+clasp :load-toplevel)
  (defparameter +method-combination-slots+
    `((name :initarg :name :accessor method-combination-name)
      (compiler :initarg :compiler :accessor method-combination-compiler)
      (options :initarg :options :accessor method-combination-options))))

;;; ----------------------------------------------------------------------
;;; Class CLASS

(eval-when (:compile-toplevel :execute #+clasp :load-toplevel)
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
      (stamp-for-instances :accessor stamp-for-instances)
      (creator :accessor creator)
      ))

  #-clasp
  (defconstant +class-name-ndx+
    (position 'name +class-slots+ :key #'first))
  #-clasp
  (defconstant +class-precedence-list-ndx+
    (position 'precedence-list +class-slots+ :key #'first)))

#+clasp
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defparameter *class-name-ndx*
    #.(position 'name +class-slots+ :key #'first))
  (defparameter *class-precedence-list-ndx*
    #.(position 'precedence-list +class-slots+ :key #'first)))

;;; ----------------------------------------------------------------------
;;; STANDARD-CLASS

(eval-when (:compile-toplevel :execute #+clasp :load-toplevel)
  (defparameter +standard-class-slots+
    (append +class-slots+
	    '((optimize-slot-access)
	      (forward)
              (source-position :initform nil :initarg :source-position :accessor class-source-position)))))

;;; ----------------------------------------------------------------------
;;; STRUCTURE-CLASS

(eval-when (:compile-toplevel :execute  #+clasp :load-toplevel)
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

(eval-when (:compile-toplevel :execute  #+clasp :load-toplevel)
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

(eval-when (:compile-toplevel :execute  #+clasp :load-toplevel)
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

(eval-when (:compile-toplevel :execute  #+clasp :load-toplevel)
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
(eval-when (:compile-toplevel :execute #+clasp :load-toplevel )
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
           #+clasp(simple-vector vector)
           #+clasp(core:simple-vector-byte8-t vector)
           #+clasp(core:simple-vector-byte16-t vector)
           #+clasp(core:simple-vector-byte32-t vector)
           #+clasp(core:simple-vector-byte64-t vector)
           #+clasp(core:simple-vector-int8-t vector)
           #+clasp(core:simple-vector-int16-t vector)
           #+clasp(core:simple-vector-int32-t vector)
           #+clasp(core:simple-vector-int64-t vector)
           #+clasp(core:simple-vector-size-t vector)
           #+clasp(core:simple-vector-fixnum vector)
           #+clasp(core:simple-vector-double vector)
           #+clasp(core:simple-vector-float vector)
           #+clasp(core:MDARRAY-BASE-CHAR array)
           #+clasp(core:MDARRAY-BIT array)
           #+clasp(core:MDARRAY-BYTE16-T array)
           #+clasp(core:MDARRAY-BYTE32-T array)
           #+clasp(core:MDARRAY-BYTE64-T array)
           #+clasp(core:MDARRAY-BYTE8-T array)
           #+clasp(core:MDARRAY-CHARACTER array)
           #+clasp(core:MDARRAY-DOUBLE array)
           #+clasp(core:MDARRAY-DUMP  Function array)
           #+clasp(core:MDARRAY-FIXNUM array)
           #+clasp(core:MDARRAY-FLOAT array)
           #+clasp(core:MDARRAY-INT16-T array)
           #+clasp(core:MDARRAY-INT32-T array)
           #+clasp(core:MDARRAY-INT64-T array)
           #+clasp(core:MDARRAY-INT8-T array)
           #+clasp(core:MDARRAY-SIZE-T array)
           #+clasp(core:MDARRAY-T array)
           #+clasp(core:SIMPLE-MDARRAY-BASE-CHAR array)
           #+clasp(core:SIMPLE-MDARRAY-BIT array)
           #+clasp(core:SIMPLE-MDARRAY-BYTE16-T array)
           #+clasp(core:SIMPLE-MDARRAY-BYTE32-T array)
           #+clasp(core:SIMPLE-MDARRAY-BYTE64-T array)
           #+clasp(core:SIMPLE-MDARRAY-BYTE8-T array)
           #+clasp(core:SIMPLE-MDARRAY-CHARACTER array)
           #+clasp(core:SIMPLE-MDARRAY-DOUBLE array)
           #+clasp(core:SIMPLE-MDARRAY-FIXNUM array)
           #+clasp(core:SIMPLE-MDARRAY-FLOAT array)
           #+clasp(core:SIMPLE-MDARRAY-INT16-T array)
           #+clasp(core:SIMPLE-MDARRAY-INT32-T array)
           #+clasp(core:SIMPLE-MDARRAY-INT64-T array)
           #+clasp(core:SIMPLE-MDARRAY-INT8-T array)
           #+clasp(core:SIMPLE-MDARRAY-SIZE-T array)
           #+clasp(core:SIMPLE-MDARRAY-T array)
	        (string vector)
                #+(or unicode clasp)
	   (base-string string vector)
           #+clasp(simple-base-string base-string)
           #+clasp(core:str8ns base-string)
           #+clasp(string vector)
           #+clasp(core:simple-character-string string)
           #+clasp(core:str-wns string)
           (bit-vector vector)
           #+clasp(cl:simple-bit-vector bit-vector)
           #+clasp(core:bit-vector-ns bit-vector)
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
;;#+clasp		    (fixnum integer)
;;#+clasp		    (bignum integer)
		  (ratio rational)
	        (float real)
	      (complex number)
	    (symbol)
	      (null symbol list)
              #-clasp(keyword symbol)   ;; Clasp doesn't use a keyword class
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
	   #+clasp(si::external-object)
	   #+clasp(si::iterator)
	     #+clasp(si::directory-iterator si::iterator)
	     #+clasp(si::recursive-directory-iterator si::iterator)
	    #+threads (mp::process)
	    #+threads (mp::lock)
	    #+threads (mp::rwlock)
	    #+threads (mp::condition-variable)
	    #+(and ecl threads) (mp::semaphore)
	    #+(and ecl threads) (mp::barrier)
	    #+(and ecl threads) (mp::mailbox)
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
;;;#+cclasp
#+(or)(eval-when (:compile-toplevel :execute :load-toplevel)
        (setq clasp-cleavir:*use-type-inference* nil))

;;; Some classes are in here multiple times because they are created then recreated (in boot.lsp)

(eval-when (eval #+clasp :compile-toplevel #+clasp :load-toplevel  )
  (locally (declare (optimize (debug 0)))
    (defconstant +class-hierarchy+
      `((standard-class
         #+clasp :creates-classes #+clasp t)
        #+clasp
        (built-in-class
         #+clasp :creates-classes #+clasp t)
        (standard-effective-slot-definition)
        (standard-direct-slot-definition)
        (standard-class
         :metaclass nil                 ; Special-cased in boot.lsp
         :direct-slots #.+standard-class-slots+
         #+clasp :creates-classes #+clasp t
         )
        #+clasp
        (built-in-class
         :metaclass nil                 ; Special-cased in boot.lsp
         :direct-slots #.+standard-class-slots+
         #+clasp :creates-classes #+clasp t
         )
        (standard-direct-slot-definition
         :direct-slots #3=#.+slot-definition-slots+)
        (standard-effective-slot-definition
         :direct-slots #3#)
        (t
         :index 0)
        #+clasp(class
                :direct-slots #.+class-slots+
                #+clasp :creates-classes #+clasp t
                )
        (standard-object
         :direct-superclasses (t))
        #+clasp
        (core:cxx-object
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
         :direct-slots #.+class-slots+
         #+clasp :creates-classes #+clasp t
         )
        (forward-referenced-class
         :direct-superclasses (class)
         :direct-slots #.+class-slots+
         #+clasp :creates-classes #+clasp t
         )
        (built-in-class
         :direct-superclasses (class)
         :direct-slots #1=#.+standard-class-slots+
         #+clasp :creates-classes #+clasp t)
        #+clasp(core:cxx-class
                :direct-superclasses (class)
                :direct-slots #1#
                #+clasp :creates-classes #+clasp t
                )
        #+clasp(clbind:class-rep
                :direct-superclasses (class)
                :direct-slots #1#
                #+clasp :creates-classes #+clasp t
                )
        (std-class
         :direct-superclasses (class)
         :direct-slots #1#
         #+clasp :creates-classes #+clasp t
         )
        (standard-class
         :direct-superclasses (std-class)
         :direct-slots #1#
         :metaclass standard-class
         #+clasp :creates-classes #+clasp t
         )
        (funcallable-standard-class
         :direct-superclasses (std-class)
         :direct-slots #1#
         #+clasp :creates-classes #+clasp t
         )
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
        (structure-class
         :direct-superclasses (class)
         :direct-slots #.+structure-class-slots+)
        (structure-object
         :metaclass structure-class
         :direct-superclasses (t))
        #+clasp(core:derivable-cxx-class
                :direct-superclasses (class)
                :direct-slots #.+standard-class-slots+
                :creates-classes t)
        #+clasp(derivable-cxx-object
                :metaclass core:derivable-cxx-class
                :direct-superclasses (t))
        ))))

;;;#+cclasp
#+(or)(eval-when (:compile-toplevel :execute :load-toplevel)
  (setq clasp-cleavir:*use-type-inference* t))

#+clasp
(eval-when (:compile-toplevel :execute)
  (let ((sanity (core:class-slot-sanity-check)))
    (dolist (name-slot (core:class-slot-sanity-check))
      (let* ((name (car name-slot))
             (core-slot-index (cdr name-slot))
             (clos-slot-index (position name +standard-class-slots+ :key #'car)))
        (if clos-slot-index
            (unless (= core-slot-index clos-slot-index)
              (format t "There is a mismatch between what clasp thinks the ~a class slot index should be (~a) and where clos says the class slot index is (~a) - update metaClass.h~%" name core-slot-index clos-slot-index)
              (error "There is a mismatch between what clasp thinks the ~a class slot index should be (~a) and where clos says the class slot index is (~a) - update metaClass.h~%" name core-slot-index clos-slot-index))
            (cond
              ((eq name 'number-of-slots-in-standard-class)
               (unless (= core-slot-index (length +standard-class-slots+))
                 (error "There is a mismatch between what clasp thinks should be the number of standard-class slots (~a) and what clos says it is (~a) - update metaClass.h" core-slot-index (length +standard-class-slots+))))
              ((eq name 'number-of-slots-in-structure-class)
               (unless (= core-slot-index (length +structure-class-slots+))
                 (error "There is a mismatch between what clasp thinks should be the number of structure-class slots (~a) and what clos says it is (~a) - update metaClass.h" core-slot-index (length +structure-class-slots+))))
              (t (error "The class-slot-sanity-check ~a could not be verified against clos - fix the sanity check at the end of hierarchy.lsp" name-slot))))))))
        
            
