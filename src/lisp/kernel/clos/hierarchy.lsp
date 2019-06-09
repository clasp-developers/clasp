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
    `(core:bformat *error-output* ,fmt ,@fmtargs)))
;;#+(or)
(defmacro mlog (fmt &rest fmtargs) nil)

;;; ----------------------------------------------------------------------
;;; Class SPECIALIZER

(eval-when (:compile-toplevel :execute #+clasp :load-toplevel)
  (defparameter +specializer-slots+
    ;; The number of specializer slots is fixed in instance.h.
    ;; A change in the number of slots here needs to be reflected there.
    ;; The slots marked with a :location are also fixed in instance.h.
    ;; They need to have those locations, even in user subclasses of this class.
    ;; Also note that boot.lsp ignores these locations for effective slots, just
    ;; using the position in the list here; so that must match the :location.
    '((flag :initform nil :accessor eql-specializer-flag)
      (direct-methods :initform nil :accessor specializer-direct-methods)
      (direct-generic-functions :initform nil :accessor specializer-direct-generic-functions)
      (call-history-generic-functions :initform nil :accessor specializer-call-history-generic-functions
                                      :location 3)
      (specializer-mutex :initform (mp:make-shared-mutex 'call-history-generic-functions-mutex)
                         :accessor specializer-mutex :location 4)
      ;;; Any changes to the slots above need to be reflected in instance.h
      )))

(eval-when (:compile-toplevel :execute #+clasp :load-toplevel)
  (defparameter +eql-specializer-slots+
    ;; We don't splice in +specializer-slots+ because we need the :initform t for flag.
    ;; Nonetheless, these slots should match those of specializer, plus the slot for the object.
    '((flag :initform t :accessor eql-specializer-flag)
      (direct-methods :initform nil :accessor specializer-direct-methods)
      (direct-generic-functions :initform nil :accessor specializer-direct-generic-functions)
      (call-history-generic-functions :initform nil :accessor specializer-call-history-generic-functions
                                      :location 3)
      (specializer-mutex :initform (mp:make-shared-mutex 'call-history-generic-functions-mutex)
                         :accessor specializer-mutex :location 4)
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
    ;; Any changes involving adding, removing, rearranging slots below need to be reflected in instance.h.
    ;; See comment in +specializer-slots+ about locations.
    `(,@+specializer-slots+
      (name :initarg :name :initform nil :accessor class-id :location 5)
      (direct-superclasses :initarg :direct-superclasses :initform nil
			   :accessor class-direct-superclasses :location 6)
      (direct-subclasses :initform nil :accessor class-direct-subclasses :location 7)
      (slots :accessor class-slots :location 8)
      (precedence-list :accessor class-precedence-list :location 9)
      (direct-slots :initarg :direct-slots :accessor class-direct-slots :location 10)
      (direct-default-initargs :initarg :direct-default-initargs :location 11
			       :initform nil :accessor class-direct-default-initargs)
      (default-initargs :accessor class-default-initargs :location 12)
      ;; FIXME: is the writer supposed to be exported? I don't think so!
      (finalized :initform nil :accessor class-finalized-p :location 13)
      (docstring :initarg :documentation :initform nil :location 14)
      (size :accessor class-size)
      (prototype)
      (dependents :initform nil :accessor class-dependents :location 17)
      (valid-initargs :accessor class-valid-initargs)
      (slot-table :accessor slot-table)
      (location-table :initform nil :accessor class-location-table :location 20)
      (stamp-for-instances :accessor stamp-for-instances :location 21)
      (creator :accessor creator :location 22)
      (source-position :initform nil :initarg :source-position :accessor class-source-position)
      ;;; Any changes to the slots above need to be reflected in instance.h and metaClass.h
      )))

;;; ----------------------------------------------------------------------
;;; STANDARD-CLASS

(eval-when (:compile-toplevel :execute #+clasp :load-toplevel)
  (defparameter +standard-class-slots+ +class-slots+))

;;; ----------------------------------------------------------------------
;;; STRUCTURE-CLASS

(eval-when (:compile-toplevel :execute  #+clasp :load-toplevel)
  (defparameter +structure-class-slots+
    ;; Note that we don't need some of the class-slots, e.g. initargs, so we could
    ;; hypothetically reorganize things.
    ;; We also don't really need any of these slots, but it might be good to have
    ;; some kind of structure to represent descriptions of structures later.
    (append +class-slots+
	    '((slot-descriptions)
	      (initial-offset)
	      (constructors)))))

;;; ----------------------------------------------------------------------
;;; STANDARD-GENERIC-FUNCTION

(eval-when (:compile-toplevel :execute  #+clasp :load-toplevel)
  (defparameter +standard-generic-function-slots+
    ;; Any changes involving adding, removing, rearranging slots below
    ;; need to be reflected in funcallableInstance.h. See +specializer-slots+ comment.
    '((name :initarg :name :initform nil
            :reader generic-function-name :location 0)
      (spec-list :initform nil :accessor generic-function-spec-list :location 1)
      (method-combination
       :initarg :method-combination
       :initform (find-method-combination (class-prototype (find-class 'standard-generic-function))
                  'standard nil)
       :accessor generic-function-method-combination)
      (lambda-list :initarg :lambda-list :location 3
                   :accessor generic-function-lambda-list)
      ;; Any changes to the slots above need to be reflected in funcallableInstance.h
      ;; (except method-combination)
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
      ;; these are the precomputed results of cl:function-keywords.
      (keywords :initform nil :initarg :keywords :accessor method-keywords)
      (aok-p :initform nil :initarg :aok-p :accessor method-allows-other-keys-p)
      ;; leaf-method-p is T if the method form doesn't call call-next-method or next-method-p
      ;; our custom initargs are internal symbols, as per MOP "The defmethod macros"
      (leaf-method-p :initform nil :initarg leaf-method-p :reader leaf-method-p)
      (fast-method-function :initform nil :initarg fast-method-function :reader fast-method-function)))

  (defparameter +standard-accessor-method-slots+
    (append +standard-method-slots+
	    '((slot-definition :initarg :slot-definition
                               :initform nil
                               :reader accessor-method-slot-definition)))))

;;; ----------------------------------------------------------------------
;;; SLOT-DEFINITION
;;;

(eval-when (:compile-toplevel :execute  #+clasp :load-toplevel)
  (core:defconstant-equal +slot-definition-slots+
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
  (core:defconstant-equal +builtin-classes-list+
	 '(;(t object)
	    (sequence)
	      (list sequence)
	        (cons list)
	    (array)
           (vector array sequence)
           (core:abstract-simple-vector vector)
           (core:complex-vector vector)
           (core:mdarray array)
           (core:simple-mdarray core:mdarray)
           (string vector)
                #+(or unicode clasp)
	   (base-string string vector)
           (simple-base-string core:abstract-simple-vector base-string)
           (core:str8ns core:complex-vector base-string)
           (string vector)
           (core:simple-character-string core:abstract-simple-vector string)
           (core:str-wns core:complex-vector string)
           (bit-vector vector)
           (cl:simple-bit-vector core:abstract-simple-vector bit-vector)
           (core:bit-vector-ns core:complex-vector bit-vector)
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
            ;;; (hash-table)  ;;No longer inherits from (core:general) 
	    (random-state)
	    (readtable)
            (si::code-block)
	    (si::foreign-data)
	    (si::frame)
	    (si::weak-pointer)
	   (si::external-object)
	   (si::iterator)
	     (si::directory-iterator si::iterator)
	     (si::recursive-directory-iterator si::iterator)
	    #+threads (mp::process)
	    #+threads (mp::lock)
	    #+threads (mp::rwlock)
	    #+threads (mp::condition-variable)
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
    (core:defconstant-equal +class-hierarchy+
      `((standard-class)
        (built-in-class)
        (standard-effective-slot-definition)
        (standard-direct-slot-definition)
        (standard-class
         :metaclass nil                 ; Special-cased in boot.lsp
         :direct-slots #.+standard-class-slots+)
        (built-in-class
         :metaclass nil                 ; Special-cased in boot.lsp
         :direct-slots #.+standard-class-slots+)
        (standard-direct-slot-definition
         :direct-slots #3=#.+slot-definition-slots+)
        (standard-effective-slot-definition
         :direct-slots #3#)
        (t)
        (class :direct-slots #.+class-slots+)
        (standard-object
         :direct-superclasses (t))
        (core:general
         :direct-superclasses (t))
        (core:cxx-object
         :direct-superclasses (core:general))
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
        (core:cxx-class
         :direct-superclasses (class)
         :direct-slots #1#)
        #+(or)(clbind:class-rep
               :direct-superclasses (class)
               :direct-slots #1#)
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
                           :direct-superclasses (or rest '(core:general))))
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
        (core:clbind-cxx-class
         :direct-superclasses (class)
         :direct-slots #.+standard-class-slots+)
        (core:derivable-cxx-class
         :direct-superclasses (class)
         :direct-slots #.+standard-class-slots+)
        (derivable-cxx-object
         :metaclass core:derivable-cxx-class
         :direct-superclasses (standard-object))
        ))))

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
                 (format t "There is a mismatch between what clasp thinks should be the number of standard-class slots (~a) and what clos says it is (~a) - update metaClass.h~%" core-slot-index (length +standard-class-slots+))
                 (error "There is a mismatch between what clasp thinks should be the number of standard-class slots (~a) and what clos says it is (~a) - update metaClass.h" core-slot-index (length +standard-class-slots+))))
              ((eq name 'number-of-slots-in-structure-class)
               (unless (= core-slot-index (length +structure-class-slots+))
                 (format t "There is a mismatch between what clasp thinks should be the number of structure-class slots (~a) and what clos says it is (~a) - update metaClass.h~%" core-slot-index (length +structure-class-slots+))
                 (error "There is a mismatch between what clasp thinks should be the number of structure-class slots (~a) and what clos says it is (~a) - update metaClass.h" core-slot-index (length +structure-class-slots+))))
              (t (format t "The class-slot-sanity-check ~a could not be verified against clos - fix the sanity check at the end of hierarchy.lsp" name-slot)
                 (error "The class-slot-sanity-check ~a could not be verified against clos - fix the sanity check at the end of hierarchy.lsp" name-slot))))))))
        
            
