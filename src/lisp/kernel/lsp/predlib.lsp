;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;                              predicate routines

(in-package "SYSTEM")

(defun constantly-t (&rest foo)
  (declare (ignore foo))
  t)

(defun constantly-nil (&rest foo)
  (declare (ignore foo))
  nil)

(declaim (inline constantly))
(defun constantly (n)
  "Args: (n)
Builds a new function which accepts any number of arguments but always outputs N."
  (case n
    ((nil) #'constantly-nil)
    ((t) #'constantly-t)
    (t #'(lambda (&rest x) (declare (ignore x)) n))))

(defparameter *subtypep-cache* (core:make-simple-vector-t 256 nil nil))

(defparameter *upgraded-array-element-type-cache* (core:make-simple-vector-t 128 nil nil))

(defun subtypep-clear-cache ()
  (fill-array-with-elt *subtypep-cache* nil 0 nil)
  (fill-array-with-elt *upgraded-array-element-type-cache* nil 0 nil))

(defun create-type-name (name)
  (when (member name *alien-declarations*)
    (error "Symbol ~s is a declaration specifier and cannot be used to name a new type" name)))
(export 'create-type-name)

(defvar *type-expanders* (make-hash-table :test #'eq :thread-safe t))

(export 'ext::type-expander "EXT")
(defun ext:type-expander (name)
  (values (gethash name *type-expanders*)))

(defun (setf ext:type-expander) (function name)
  (unless (symbolp name)
    (error "~s is not a valid type specifier" name))
  (create-type-name name)
  (core:hash-table-setf-gethash *type-expanders* name function)
  (subtypep-clear-cache)
  function)


;;; DEFTYPE macro.
(defmacro deftype (name lambda-list &rest body &environment env)
  "Syntax: \(deftype name lambda-list {decl | doc}* {form}*)
Defines a new type-specifier abbreviation in terms of an 'expansion' function
	(lambda lambda-list1 {DECL}* {FORM}*)
where LAMBDA-LIST1 is identical to LAMBDA-LIST except that all optional
parameters with no default value specified in LAMBDA-LIST defaults to the
symbol '*', but not to NIL.  When the type system of ECL encounters a type
specifier (NAME arg1 ... argn), it calls the expansion function with the
arguments ARG1 ... ARGn, and uses the returned value instead of the original
type specifier.  When the symbol NAME is used as a type specifier, the
expansion function is called with no argument.
The doc-string DOC, if supplied, is saved as a TYPE doc and can be retrieved
by (documentation 'NAME 'type)."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (funcall #'(setf ext:type-expander)
              ,(ext:parse-deftype name lambda-list body env)
              ',name)
     ',name))

;;; Some DEFTYPE definitions.

(deftype boolean ()
  "A BOOLEAN is an object which is either NIL or T."
  '(member nil t))

(deftype index ()
  '(INTEGER 0 #.(1- array-dimension-limit)))

(deftype fixnum ()
  "A FIXNUM is an integer between MOST-NEGATIVE-FIXNUM (= - 2^29 in ECL) and
MOST-POSITIVE-FIXNUM (= 2^29 - 1 in ECL) inclusive.  Other integers are
bignums."
  '(INTEGER #.most-negative-fixnum #.most-positive-fixnum))
(deftype bignum ()
  '(OR (INTEGER * (#.most-negative-fixnum)) (INTEGER (#.most-positive-fixnum) *)))

(deftype general-number ()
  "The type of numbers that are not immediates. Clasp-specific, internal."
  '(and number (not fixnum) (not single-float)))

(deftype eq-incomparable ()
  "The type of objects that have different results for EQL and EQ.
Clasp-specific, internal. Used in optimizations of EQL."
  'general-number)

(deftype ext:byte2 () '(unsigned-byte 2))
(deftype ext:integer2 () '(signed-byte 2))
(deftype ext:byte4 () '(unsigned-byte 4))
(deftype ext:integer4 () '(signed-byte 4))

(deftype ext:byte8 () '(INTEGER 0 255))
(deftype ext:integer8 () '(INTEGER -128 127))
(deftype ext:byte16 () '(INTEGER 0 #xFFFF))
(deftype ext:integer16 () '(INTEGER #x-8000 #x7FFF))
(deftype ext:byte32 () '(INTEGER 0 #xFFFFFFFF))
(deftype ext:integer32 () '(INTEGER #x-80000000 #x7FFFFFFF))
(deftype ext:byte64 () '(INTEGER 0 #xFFFFFFFFFFFFFFFF))
(deftype ext:integer64 () '(INTEGER #x-8000000000000000 #x7FFFFFFFFFFFFFFF))

(deftype real (&optional (start '* start-p) (end '*))
  (if start-p
      (let (rat-start
	    real-start
	    rat-end
	    real-end)
	(cond ((consp start)
	       (setq start (first start)
		     rat-start (list (rational start))
		     real-start (list (float start))))
	      ((numberp start)
	       (setq rat-start (rational start)
		     real-start (float start)))
	      (t
	       (setq rat-start start
		     real-start start)))
	(cond ((consp end)
	       (setq end (first end)
		     rat-end (list (rational end))
		     real-end (list (float end))))
	      ((numberp end)
	       (setq rat-end (rational end)
		     real-end (float end)))
	      (t
	       (setq rat-end end
		     real-end end)))
	`(OR (RATIONAL ,rat-start ,rat-end) (FLOAT ,real-start ,real-end)))
      '(OR RATIONAL FLOAT)))

#-short-float
(deftype short-float (&rest args)
  (if args
      `(single-float ,@args)
      'single-float))

#-long-float
(deftype long-float (&rest args)
  (if args
      `(double-float ,@args)
      'double-float))

(deftype bit ()
  "A BIT is either integer 0 or 1."
  '(INTEGER 0 1))

(deftype mod (n)
  `(INTEGER 0 ,(1- n)))

(deftype signed-byte (&optional s)
  "As a type specifier, (SIGNED-BYTE n) specifies those integers that can be
represented with N bits in 2's complement representation."
  (if (or (null s) (eq s '*))
      '(INTEGER * *)
      `(INTEGER ,(- (expt 2 (1- s))) ,(1- (expt 2 (1- s))))))

(deftype unsigned-byte (&optional s)
  "As a type specifier, (UNSIGNED-BYTE n) specifies non-negative integers that
can be represented with N bits."
  (if (or (null s) (eq s '*))
      '(INTEGER 0 *)
      `(INTEGER 0 ,(1- (expt 2 s)))))

(deftype null ()
  "The type to which only NIL belongs."
  '(MEMBER NIL))

(deftype sequence ()
  "A sequence is either a list or a vector."
  '(OR CONS NULL (ARRAY * (*))))

(deftype list ()
  "As a type specifier, LIST is used to specify the type consisting of NIL and
cons objects.  In our ordinary life with Lisp, however, a list is either NIL
or a cons whose cdr is a list, and is notated by its elements surrounded with
parentheses.
The backquote macro is sometimes useful to construct a complicated list
structure.  When evaluating `(...)
	,form embeds the value of FORM,
	,@form and ,.form embed all elements of the list value of FORM,
	and other things embed itself
into the structure at their position.  For example,
	`(a b ,c d e) expands to (list* 'a 'b c '(d e))
	`(a b ,@c d e) expands to (list* 'a 'b (append c '(d e)))
	`(a b ,.c d e) expands to (list* 'a 'b (nconc c '(d e)))"
  '(OR CONS NULL))

(deftype atom ()
  "An ATOM is an object that is not a CONS."
  '(NOT CONS))

(deftype vector (&optional (element-type '*) (size '*))
  "A vector is a one-dimensional array.  Strings and bit-vectors are kinds of
vectors.  Other vectors are called general vectors and are notated as
	#(elem ... elem)
Some vectors may be displaced to another array, may have a fill-pointer, or
may be adjustable.  Other vectors are called simple-vectors."
  `(array ,element-type (,size)))

(deftype extended-char ()
  "A character which is not of type BASE-CHAR."
  '(and character (not base-char)))

;;; Several of the below conditionalize on *; this is presumably so that
;;; the resulting types can be EQ constants rather than newly consed.
;;; Also, wrt strings: Iff :UNICODE is a feature, base-char and character
;;; are distinct types.
(deftype string (&optional size)
  "A string is a vector of characters.  A string is notated by surrounding the
characters with double quotes.  Some strings may be displaced to another
string, may have a fill-pointer, or may be adjustable.  Other strings are
called simple-strings."
  #-unicode
  (if (eq size '*)
      '(array character (*))
      `(array character (,size)))
  #+unicode
  (if (eq size '*)
      '(or (array base-char (*)) (array character (*)))
      `(or (array base-char (,size))
	   (array character (,size)))))

(deftype base-string (&optional (size '*))
  "A string which is made of BASE-CHAR."
  (if (eq size '*) '(array base-char (*)) `(array base-char (,size))))

(deftype extended-string (&optional (size '*))
  "A string which is not a base string."
  #-unicode
  NIL
  #+unicode
  (if (eq size '*) '(array character (*)) `(array character (,size))))

(deftype bit-vector (&optional (size '*))
  "A bit-vector is a vector of bits.  A bit-vector is notated by '#*' followed
by its elements (0 or 1).  Bit-vectors may be displaced to another array, may
have a fill-pointer, or may be adjustable.  Other bit-vectors are called
simple-bit-vectors.  Only simple-bit-vectors can be input in the above format
using '#*'."
  (if (eq size '*) '(array bit (*)) `(array bit (,size))))

(deftype simple-vector (&optional (size '*))
  "A simple-vector is a vector that is not displaced to another array, has no
fill-pointer, is not adjustable, and can hold any objects."
  (if (eq size '*) '(simple-array t (*)) `(simple-array t (,size))))

(deftype simple-string (&optional size)
  "A simple-string is a string that is not displaced to another array, has no
fill-pointer, and is not adjustable."
  #-unicode
  (if size
    `(simple-array character (,size))
    '(simple-array character (*)))
  #+unicode
  (if size
      `(or (simple-array base-char (,size))
	   (simple-array character (,size)))
      '(or (simple-array base-char (*)) (simple-array character (*)))))

(deftype simple-base-string (&optional size)
  "A base-string which cannot be adjusted nor displaced."
  (if size `(simple-array base-char (,size)) '(simple-array base-char (*))))

(deftype simple-bit-vector (&optional size)
  "A bit-vector that is not displaced to another array, has no fill-pointer,
and is not adjustable."
  (if size `(simple-array bit (,size)) '(simple-array bit (*))))

(deftype ext:array-index ()
  '(integer 0 #.(1- array-dimension-limit)))

;;************************************************************
;;			TYPEP
;;************************************************************

(defun simple-array-p (x)
  (and (arrayp x)
       (not (adjustable-array-p x))
       (not (array-has-fill-pointer-p x))
       (not (array-displacement x))))

(defun complex-array-p (x)
  (and (arrayp x)
       (or (adjustable-array-p x)
	   (array-has-fill-pointer-p x)
	   (array-displacement x))))

(defun simple-type-predicate (name)
  ;; For some built in types, returns the name of an indicator function.
  ;; That is, (typep object name) = (funcall (simple-type-predicate name) object)
  ;; Otherwise NIL.
  (case name
    (ARRAY 'ARRAYP)
    (ATOM 'ATOM)
    #-unicode
    (EXTENDED-CHAR 'CONSTANTLY-NIL)
    (BASE-CHAR 'BASE-CHAR-P)
    (BASE-STRING 'BASE-STRING-P)
    (BIT-VECTOR 'BIT-VECTOR-P)
    (CHARACTER 'CHARACTERP)
    (COMPILED-FUNCTION 'COMPILED-FUNCTION-P)
    (COMPLEX 'COMPLEXP)
    (COMPLEX-ARRAY 'COMPLEX-ARRAY-P)
    (CONS 'CONSP)
    (DOUBLE-FLOAT 'CORE:DOUBLE-FLOAT-P)
    (FLOAT 'FLOATP)
    (FUNCTION 'FUNCTIONP)
    (HASH-TABLE 'HASH-TABLE-P)
    (INTEGER 'INTEGERP)
    (FIXNUM 'SI::FIXNUMP)
    (KEYWORD 'KEYWORDP)
    (LIST 'LISTP)
    (LOGICAL-PATHNAME 'LOGICAL-PATHNAME-P)
    ((NIL) 'CONSTANTLY-NIL)
    (NULL 'NULL)
    (NUMBER 'NUMBERP)
    (PACKAGE 'PACKAGEP)
    (RANDOM-STATE 'RANDOM-STATE-P)
    (RATIONAL 'RATIONALP)
    (PATHNAME 'PATHNAMEP)
    (READTABLE 'READTABLEP)
    (REAL 'REALP)
    (SIMPLE-ARRAY 'SIMPLE-ARRAY-P)
    (SIMPLE-BIT-VECTOR 'SIMPLE-BIT-VECTOR-P)
    (SIMPLE-STRING 'SIMPLE-STRING-P)
    (SIMPLE-VECTOR 'SIMPLE-VECTOR-P)
    (SINGLE-FLOAT 'CORE:SINGLE-FLOAT-P)
    (STREAM 'STREAMP)
    (STRING 'STRINGP)
    (SYMBOL 'SYMBOLP)
    ((T) 'CONSTANTLY-T)
    (VECTOR 'VECTORP)
    (t nil)))

(defconstant-equal +upgraded-array-element-types+
  '#.(append '(NIL BASE-CHAR #+unicode CHARACTER BIT)
             '(ext:byte2 ext:integer2)
             '(ext:byte4 ext:integer4)
             '(EXT:BYTE8 EXT:INTEGER8)
             '(EXT:BYTE16 EXT:INTEGER16)
             '(EXT:BYTE32 EXT:INTEGER32)
             '(fixnum)
             '(EXT:BYTE64 EXT:INTEGER64)
             '(SINGLE-FLOAT DOUBLE-FLOAT T)))

(defun upgraded-array-element-type (element-type &optional env)
  (declare (ignore env))
  (let* ((hash (logand 127 (si:hash-eql element-type)))
	 (record (aref *upgraded-array-element-type-cache* hash)))
    (declare (type (integer 0 127) hash))
    (if (and record (eq (car record) element-type))
	(cdr record)
	(let ((answer (if (member element-type +upgraded-array-element-types+
				  :test #'eq)
			  element-type
			  (dolist (v +upgraded-array-element-types+ 'T)
			    (when (subtypep element-type v)
			      (return v))))))
	  (row-major-aset *upgraded-array-element-type-cache* hash
                          (cons element-type answer))
	  answer))))

(defun upgraded-complex-part-type (real-type &optional env)
  (declare (ignore env))
  ;; ECL does not have specialized complex types. If we had them, the
  ;; code would look as follows
  ;;   (dolist (v '(INTEGER RATIO RATIONAL SINGLE-FLOAT DOUBLE-FLOAT FLOAT REAL)
  ;; 	   (error "~S is not a valid part type for a complex." real-type))
  ;;     (when (subtypep real-type v)
  ;;       (return v))))
  (unless (subtypep real-type 'REAL)
    (error "~S is not a valid part type for a complex." real-type))
  'REAL)

(defun in-interval-p (x interval)
  (let* (low high)
    (if (endp interval)
        (setq low '* high '*)
        (if (endp (cdr interval))
            (setq low (car interval) high '*)
            (setq low (car interval) high (second interval))))
    (cond ((eq low '*))
          ((consp low)
           (when (<= x (car low)) (return-from in-interval-p nil)))
          ((when (< x low) (return-from in-interval-p nil))))
    (cond ((eq high '*))
          ((consp high)
           (when (>= x (car high)) (return-from in-interval-p nil)))
          ((when (> x high) (return-from in-interval-p nil))))
    (return-from in-interval-p t)))

(defun error-type-specifier (type)
  (error "~S is not a valid type specifier." type))

(defun match-dimensions (array pat)
  (or (eq pat '*)
      (let ((rank (array-rank array)))
	(cond ((numberp pat) (= rank pat))
	      ((listp pat)
	       (dotimes (i rank (null pat))
		 (unless (and (consp pat)
			      (or (eq (car pat) '*)
				  (eql (array-dimension array i) (car pat))))
		   (return nil))
		 (setq pat (cdr pat))))
	      ((atom pat)
	       (error "~S does not describe array dimensions." pat))))))

;; Actually used way later in CLOS.
;; Inlining doesn't work here for bootstrap reasons, so we just use
;; subclassp directly within this file.
(declaim (inline of-class-p))
(defun of-class-p (object class)
  (si::subclassp (class-of object) class))

(defun typep (object type &optional env &aux tp i c)
  "Args: (object type)
Returns T if X belongs to TYPE; NIL otherwise."
  (cond ((symbolp type)
	 (let ((f (simple-type-predicate type)))
	   (cond (f (return-from typep (funcall f object)))
		 ((eq (type-of object) type) (return-from typep t))
		 (t (setq tp type i nil)))))
	((consp type)
	 (setq tp (car type) i (cdr type)))
	#+clos
	((clos::classp type)
	 (return-from typep (si:subclassp (class-of object) type)))
	(t
	 (error-type-specifier type)))
  (case tp
    ((EQL MEMBER) (and (member object i) t))
    (NOT (not (typep object (car i))))
    (OR (dolist (e i)
	  (when (typep object e) (return t))))
    (AND (dolist (e i t)
	   (unless (typep object e) (return nil))))
    (SATISFIES (funcall (car i) object))
    ((T) t)
    ((NIL) nil)
    (BIGNUM (and (integerp object) (not (si::fixnump object))))
    (RATIO (eq (type-of object) 'RATIO))
    (STANDARD-CHAR
     (and (characterp object) (standard-char-p object)))
    (INTEGER
     (and (integerp object) (in-interval-p object i)))
    (RATIONAL
     (and (rationalp object) (in-interval-p object i)))
    (FLOAT
     (and (floatp object) (in-interval-p object i)))
    (REAL
     (and (or (rationalp object) (floatp object)) (in-interval-p object i)))
    ((SINGLE-FLOAT #-short-float SHORT-FLOAT)
     (and (eq (type-of object) 'SINGLE-FLOAT) (in-interval-p object i)))
    ((DOUBLE-FLOAT #-long-float LONG-FLOAT)
     (and (eq (type-of object) 'DOUBLE-FLOAT) (in-interval-p object i)))
    #+long-float
    (LONG-FLOAT
     (and (eq (type-of object) 'LONG-FLOAT) (in-interval-p object i)))
    ;; OK, important conformance note. CLHS says TYPEP for a complex checks
    ;; the real and imaginary parts against the complex element type.
    ;; However, this seems to conflict with the idea of upgraded complex types
    ;; and the operation of subtypep... so, no.
    ;; Note that if you decide to change this, you'll need to alter the
    ;; compiler macro (in cmp/opt-type.lsp) as well.
    (COMPLEX (complexp object))
    (SEQUENCE (or (listp object) (vectorp object)))
    (CONS (and (consp object)
	       (or (endp i)
		   (let ((car-type (first i)))
		     (or (eq car-type '*) (typep (car object) car-type))))
	       (or (endp (cdr i))
		   (let ((cdr-type (second i)))
		     (or (eq cdr-type '*) (typep (cdr object) cdr-type))))))
    (BASE-STRING
     (and (base-string-p object)
          (or (null i) (match-dimensions object i))))
    (STRING
     (and (stringp object)
          (or (null i) (match-dimensions object i))))
    (BIT-VECTOR
     (and (bit-vector-p object)
          (or (null i) (match-dimensions object i))))
    (SIMPLE-BASE-STRING
     (and (base-string-p object)
          (simple-string-p object)
	  (or (null i) (match-dimensions object i))))
    (SIMPLE-STRING
     (and (simple-string-p object)
          (or (null i) (match-dimensions object i))))
    (SIMPLE-BIT-VECTOR
     (and (simple-bit-vector-p object)
          (or (null i) (match-dimensions object i))))
    (SIMPLE-VECTOR
     (and (simple-vector-p object)
          (or (null i) (match-dimensions object i))))
    (COMPLEX-ARRAY
     (and (complex-array-p object)
          (or (endp i) (eq (car i) '*)
	      ;; (car i) needs expansion
	      (eq (array-element-type object)
		  (upgraded-array-element-type (car i))))
          (or (endp (cdr i)) (match-dimensions object (second i)))))
    (SIMPLE-ARRAY
     (and (simple-array-p object)
          (or (endp i) (eq (car i) '*)
	      ;; (car i) needs expansion
	      (eq (array-element-type object)
		  (upgraded-array-element-type (car i))))
          (or (endp (cdr i)) (match-dimensions object (second i)))))
    (ARRAY
     (and (arrayp object)
          (or (endp i) (eq (car i) '*)
              ;; Or the element type of object should be EQUAL to (car i).
              ;; Is this too strict?
              (eq (array-element-type object)
		  (upgraded-array-element-type (car i))))
          (or (endp (cdr i)) (match-dimensions object (second i)))))
    (t
     (cond
           ((ext:type-expander tp)
            (typep object (funcall (ext:type-expander tp) type env)))
	   ((consp i)
	    (error-type-specifier type))
	   ((setq c (find-class type nil))
	    ;; Follow the inheritance chain
            (si:subclassp (class-of object) c))
	   (t
	    (error-type-specifier type))))))

;;************************************************************
;;			NORMALIZE-TYPE
;;************************************************************
;; NORMALIZE-TYPE normalizes the type using the DEFTYPE definitions.
;; The result is a pair of values
;;  VALUE-1 = normalized type name or object
;;  VALUE-2 = normalized type arguments or nil
(defun normalize-type (type &optional env &aux tp i fd)
  ;; Loops until the car of type has no DEFTYPE definition.
  (cond ((symbolp type)
	 (if (setq fd (ext:type-expander type))
             (normalize-type (funcall fd type env))
             (values type nil)))
	#+clos
	((clos::classp type) (values type nil))
	((atom type)
	 (error-type-specifier type))
	((progn
	   (setq tp (car type) i (cdr type))
	   (setq fd (ext:type-expander tp)))
	 (normalize-type (funcall fd type env)))
	((and (eq tp 'INTEGER) (consp (cadr i)))
	 (values tp (list (car i) (1- (caadr i)))))
	(t (values tp i))))

;;************************************************************
;;			COERCE
;;************************************************************

(defun error-coerce (object type)
  (error "Cannot coerce ~S to type ~S." object type))

(declaim (inline character))
(defun character (character-designator)
  (if (characterp character-designator)
      character-designator
      (let ((s (string character-designator)))
        (if (= (length s) 1)
            (char s 0)
            (error 'simple-type-error
                   :datum character-designator
                   :expected-type '(or character (string 1) symbol)
                   :format-control "~s is not a character designator."
                   :format-arguments (list character-designator))))))

(defun coerce (object type &aux aux)
  "Args: (x type)
Coerces X to an object of the specified type, if possible.  Signals an error
if not possible."
  (when (typep object type)
    ;; Just return as it is.
    (return-from coerce object))
  ;; We use the original type for error messages, and for concatenate,
  ;; the latter because we have weird cases like STRING. STRING means
  ;; (vector character) in this context, rather than its actual expansion,
  ;; which includes multiple element types (base-char and character on clasp)
  ;; This means deftypes will be expanded more than once, which is inefficient,
  ;; but any function that takes a type specifier is always going to be kind of
  ;; inefficient, and we have a coerce compiler macro later to avoid it.
  (multiple-value-bind (head tail)
      (normalize-type type)
    (case head
      ((t) object)
      ((character base-char) (character object))
      ((float) (float object))
      ((short-float) (float object 0.0s0))
      ((single-float) (float object 0.0f0))
      ((double-float) (float object 0.0d0))
      ((long-float) (float 0.0l0))
      ((function) (coerce-to-function object))
      ((complex)
       (destructuring-bind (&optional (realt t) (imagt t))
           tail
         (complex (coerce (realpart object) realt)
                  (coerce (imagpart object) imagt))))
      ((and) ; clasp extension
       (dolist (type tail)
         (setq aux (coerce object type)))
       (unless (typep aux type)
         (error-coerce object type)))
      (t (if (typep object 'sequence)
             (concatenate type object)
             (error-coerce object type))))))

;;************************************************************
;;			SUBTYPEP
;;************************************************************
;;
;; TYPES LATTICE (Following Henry Baker's paper)
;;
;; The algorithm works as follows. Types are identified with sets. Some sets
;; are elementary, in the sense that other types may be expressed as
;; combination of them. We partition these sets into FAMILIES
;;
;;	Built-in objects --- Hash tables, etc
;;	Intervals --- (INTEGER a b), (REAL a b), etc
;;	Arrays --- (ARRAY * (2)), etc
;;	Classes
;;
;; When passed a type specifier, ECL canonicalizes it: it decomposes the
;; type into the most elementary sets, assigns a unique bit pattern (TAG) to
;; each of these sets, and builds a composite tag for the type by LOGIOR.
;; Operations between these sets reduce to logical operations between these
;; bit patterns. Given types T1, T2 and a function which produces tags f(T)
;;
;;	f((AND T1 T2)) = (LOGIAND f(T1) f(T2))
;;	f((OR T1 T2)) = (LOGIOR f(T1) f(T2))
;;	f((NOT T1)) = (LOGNOT f(T2))
;;
;; However, tags are not permanent: whenever a new type is registered, the
;; tag associated to a type may be changed (for instance, because new
;; elementary sets are discovered, which also belong to existing types).

(defparameter *save-types-database* nil)

(defparameter *highest-type-tag* #B1)

(defparameter *member-types* ())

(defparameter *intervals-mask* #B1)

(defparameter *elementary-types* '())

(defun new-type-tag ()
  (prog1 *highest-type-tag*
    (setq *highest-type-tag* (ash *highest-type-tag* 1))))

;; Find out the tag for a certain type, if it has been already registered.
;;
(defun find-registered-tag (type &optional (test #'equal))
  (let* ((pos (assoc type *elementary-types* :test test)))
    (and pos (cdr pos))))

;; We are going to make changes in the types database. Save a copy if this
;; will cause trouble.
;;
(defun maybe-save-types ()
  (when *save-types-database*
    (setq *save-types-database* nil
	  *elementary-types* (copy-tree *elementary-types*)
	  *member-types* (copy-tree *member-types*))))

;; We have created and tagged a new type (NEW-TAG). However, there are
;; composite and synonym types registered around which are supertypes of
;; this type and need to be tagged. TYPE-MASK is a bit pattern which helps
;; us in recognizing these supertypes.
;;
(defun update-types (type-mask new-tag)
  (maybe-save-types)
  (dolist (i *elementary-types*)
    (unless (zerop (logand (cdr i) type-mask))
      (setf (cdr i) (logior new-tag (cdr i))))))

;; FIND-TYPE-BOUNDS => (VALUES TAG-SUPER TAG-SUB)
;;
;; This function outputs two values: TAG-SUB, the tag for the union-type of all
;; types which are subtypes of the supplied one; and TAG-SUPER, which is either
;; the tag for the union-type of all types which a supertype of the supplied
;; one (MINIMIZE-SUPER = NIL) or the tag for the smallest type which is a
;; supertype of the given one (MINIMIZE-SUPER = TRUE). The search process is
;; restricted to types in the same family class.
;;
;; A value of MINIMIZE-SUPER = TRUE only makes sense for some families (such
;; as semi-infinite intervals), for which (SUBTYPEP T1 T2) = T and (SUBTYPEP T1
;; T3) = T implies either (SUBTYPEP T2 T3) = T or (SUBTYPEP T3 T2) = T.
;;
(defun find-type-bounds (type in-our-family-p type-<= minimize-super)
  (declare (optimize (safety 0))
	   (function in-our-family-p type-<=)) 
  (let* ((subtype-tag 0)
	 (disjoint-tag 0)
	 (supertype-tag (if minimize-super -1 0)))
    (dolist (i *elementary-types*)
      (declare (cons i))
      (let ((other-type (car i))
	    (other-tag (cdr i)))
	(when (funcall in-our-family-p other-type)
	  (cond ((funcall type-<= type other-type)
		 (if minimize-super
		     (when (zerop (logandc2 other-tag supertype-tag))
		       (setq supertype-tag other-tag))
		     (setq supertype-tag (logior other-tag supertype-tag))))
		((funcall type-<= other-type type)
		 (setq subtype-tag (logior other-tag subtype-tag)))
		(t
		 (setq disjoint-tag (logior disjoint-tag other-tag)))))))
    (values (if (= supertype-tag -1) 0
		(logandc2 supertype-tag (logior disjoint-tag subtype-tag)))
	    subtype-tag)))

;; A new type is to be registered, which is not simply a composition of
;; previous types. A new tag has to be created, and all supertypes are to be
;; tagged. Here we have to distinguish two possibilities: first, a supertype
;; may belong to the same family (intervals, arrays, etc); second, some
;; supertypes may be basic types (NUMBER is a supertype for (INTEGER 0 2),
;; for instance). The first possibility is detected with the comparison
;; procedure, TYPE-<=; the second possibility is detected by means of tags.
;;
(defun register-type (type in-our-family-p type-<=)
  (declare (optimize (safety 0))
	   (function in-our-family-p type-<=))
  (or (find-registered-tag type)
      (multiple-value-bind (tag-super tag-sub)
	  (find-type-bounds type in-our-family-p type-<= nil)
	(let ((tag (new-type-tag)))
	  (update-types (logandc2 tag-super tag-sub) tag)
	  (setq tag (logior tag tag-sub))
	  (push-type type tag)))))

;;----------------------------------------------------------------------
;; MEMBER types. We register this object in a separate list, *MEMBER-TYPES*,
;; and tag all types to which it belongs. We need to treat three cases
;; separately
;;	- Ordinary types, via simple-member-type, check the objects
;;	  against all pre-registered types, adding their tags.
;;	- Ordinary numbers, are translated into intervals.
;;	- Floating point zeros, have to be treated separately. This
;;	  is done by assigning a special tag to -0.0 and translating
;;	  (MEMBER 0.0) = (AND (float-type 0.0 0.0) (NOT (MEMBER -0.0)))
;;
(defun register-member-type (object)
  (let ((pos (assoc object *member-types*)))
    (cond ((and pos (cdr pos)))
	  ((not (realp object))
	   (simple-member-type object))
	  ((and (floatp object) (zerop object))
	   #.(if (eql (- 0.0) 0.0)
		 '(number-member-type object)
		 '(if (minusp (float-sign object))
		      (simple-member-type object)
		      (logandc2 (number-member-type object)
			        (register-member-type (- object))))))
	  (t
	   (number-member-type object)))))

(defun simple-member-type (object)
  (let* ((tag (new-type-tag)))
    (maybe-save-types)
    (setq *member-types* (acons object tag *member-types*))
    (dolist (i *elementary-types*)
      (let ((type (car i)))
	(when (typep object type)
	  (cons-setf-cdr i (logior tag (cdr i))))))
    tag))

;; We convert number into intervals, so that (AND INTEGER (NOT (EQL
;; 10))) is detected as a subtype of (OR (INTEGER * 9) (INTEGER 11
;; *)).
(defun number-member-type (object)
  (let* ((base-type (if (integerp object) 'INTEGER (type-of object)))
	 (type (list base-type object object)))
    (or (find-registered-tag type)
	(register-interval-type type))))


(defun push-type (type tag)
  (dolist (i *member-types*)
    (declare (cons i))
    (when (typep (car i) type)
      (setq tag (logior tag (cdr i)))))
  (push (cons type tag) *elementary-types*)
  tag)

;;----------------------------------------------------------------------
;; SATISFIES types. Here we should signal some error which is caught
;; somewhere up, to denote failure of the decision procedure.
;;
(defun register-satisfies-type (type)
  (declare (ignore type))
  (throw '+canonical-type-failure+ 'satisfies))

;;----------------------------------------------------------------------
;; CLOS classes and structures.
;;
#+clos(defun register-class (class)
  (declare (notinline class-name))
  (or (find-registered-tag class)
      ;; We do not need to register classes which belong to the core type
      ;; system of LISP (ARRAY, NUMBER, etc).
      (let* ((name (class-name class)))
	(and name
	     (eq class (find-class name 'nil))
	     (or (find-registered-tag name)
		 (find-built-in-tag name))))
      (and (not (clos::class-finalized-p class))
           (throw '+canonical-type-failure+ nil))
      (register-type class
		     #'(lambda (c) (or (clos::classp c) (symbolp c)))
		     #'(lambda (c1 c2)
			 (when (symbolp c1)
			   (setq c1 (find-class c1 nil)))
			 (when (symbolp c2)
			   (setq c2 (find-class c2 nil)))
			 (and c1 c2 (si::subclassp c1 c2))))))

;;----------------------------------------------------------------------
;; ARRAY types.
;;
(defun register-array-type (type)
  (multiple-value-bind (array-class elt-type dimensions)
      (parse-array-type type)
    (cond ((eq elt-type '*)
	   (canonical-type `(OR ,@(mapcar #'(lambda (type) `(,array-class ,type ,dimensions))
					  +upgraded-array-element-types+))))
	  ((find-registered-tag (setq type (list array-class elt-type dimensions))))
	  (t
	   (register-type type #'array-type-p #'array-type-<=)))))

;;
;; We look for the most specialized type which is capable of containing
;; this object. LIST always contains 'T, so that this procedure never
;; fails. It is faster than UPGRADED-... because we use the tags of types
;; that have been already registered.
;;
(defun fast-upgraded-array-element-type (type)
  (cond ((eql type '*) '*)
	((member type +upgraded-array-element-types+ :test #'eq)
	 type)
	(t
	 (dolist (other-type +upgraded-array-element-types+ 'T)
	   (when (fast-subtypep type other-type)
	     (return other-type))))))

;;
;; This canonicalizes the array type into the form
;;	({COMPLEX-ARRAY | SIMPLE-ARRAY} {elt-type | '*} {'* | (['*]*)})
;;
;; ELT-TYPE is the upgraded element type of the input.
;;
(defun parse-array-type (input)
  (let* ((type input)
	 (name (pop type))
	 (elt-type (fast-upgraded-array-element-type (if type (pop type) '*)))
	 (dims (if type (pop type) '*)))
    (when type
      (error "Wrong array type designator ~S." input))
    (cond ((numberp dims)
	   (unless (< -1 dims array-rank-limit)
	     (error "Wrong rank size array type ~S." input))
	   (setq dims (nthcdr (- array-rank-limit dims)
			      '#.(make-list array-rank-limit :initial-element '*))))
	  ((consp dims)
	   (dolist (i dims)
	     (unless (or (eq i '*)
			 (and (integerp i) (< -1 i array-dimension-limit)))
	       (error "Wrong dimension size in array type ~S." input)))))
    (values name elt-type dims)))

;;
;; This function checks whether the array type T1 is a subtype of the array
;; type T2.
;;
(defun array-type-<= (t1 t2)
  (unless (and (eq (first t1) (first t2))
	       (eq (second t1) (second t2)))
    (return-from array-type-<= nil))
  (let ((dim (third t1))
	(pat (third t2)))
    (cond ((eq pat '*) t)
	  ((eq dim '*) nil)
	  (t (do ((a dim (cdr a))
		  (b pat (cdr b)))
		 ((or (endp a)
		      (endp b)
		      (not (or (eq (car b) '*)
			       (eql (car b) (car a)))))
		  (and (null a) (null b)))
	       )))))

(defun array-type-p (type)
  (and (consp type)
       (member (first type) '(COMPLEX-ARRAY SIMPLE-ARRAY))))

;;----------------------------------------------------------------------
;; INTERVALS:
;;
;; Arbitrary intervals may be defined as the union or intersection of
;; semi-infinite intervals, of the form (number-type b *), where B is
;; either a real number, a list with one real number or *.
;; Any other interval, may be defined using these. For instance
;;  (INTEGER 0 2) = (AND (INTEGER 0 *) (NOT (INTEGER (2) *)))
;;  (SHORT-FLOAT (0.2) (2)) = (AND (SHORT-FLOAT (0.2) *) (NOT (SHORT-FLOAT 2 *)))

(defun register-elementary-interval (type b)
  (setq type (list type b))
  (or (find-registered-tag type #'equalp)
      (multiple-value-bind (tag-super tag-sub)
	  (find-type-bounds type
			    #'(lambda (other-type)
				(and (consp other-type)
				     (null (cddr other-type))))
			    #'(lambda (i1 i2)
				(and (eq (first i1) (first i2))
				     (bounds-<= (second i2) (second i1))))
			    t)
	(let ((tag (new-type-tag)))
	  (update-types (logandc2 tag-super tag-sub) tag)
	  (setq tag (logior tag tag-sub))
	  (push-type type tag)))))

(defun register-interval-type (interval)
  (let* ((i interval)
	 (type (pop i))
	 (low (if i (pop i) '*))
	 (high (if i (pop i) '*))
	 (tag-high (cond ((eq high '*)
			  0)
			 ((eq type 'INTEGER)
			  (setq high (if (consp high)
					 (ceiling (first high))
					 (floor (1+ high))))
			  (register-elementary-interval type high))
			 ((consp high)
			  (register-elementary-interval type (first high)))
			 (t
			  (register-elementary-interval type (list high)))))
	 (tag-low (register-elementary-interval type
		    (cond ((or (eq '* low) (not (eq type 'INTEGER)) (integerp low))
			   low)
			  ((consp low)
			   (floor (1+ (first low))))
			  (t
			   (ceiling low)))))
	 (tag (logandc2 tag-low tag-high)))
    (unless (eq high '*)
      (push-type interval tag))
    tag))

;; All comparisons between intervals operations may be defined in terms of
;;
;;	(BOUNDS-<= b1 b2)	and	(BOUNDS-< b1 b2)
;;
;; The first one checks whether (REAL b2 *) is contained in (REAL b1 *). The
;; second one checks whether (REAL b2 *) is strictly contained in (REAL b1 *)
;; (i.e., (AND (REAL b1 *) (NOT (REAL b2 *))) is not empty).
;;
(defun bounds-<= (b1 b2)
  (cond ((eq b1 '*) t)
	((eq b2 '*) nil)
	((consp b1)
	 (if (consp b2)
	     (<= (first b1) (first b2))
	     (< (first b1) b2)))
	((consp b2)
	 (<= b1 (first b2)))
	(t
	 (<= b1 b2))))

(defun bounds-< (b1 b2)
  (cond ((eq b1 '*) (not (eq b2 '*)))
	((eq b2 '*) nil)
	((consp b1)
	 (if (consp b2)
	     (< (first b1) (first b2))
	     (< (first b1) b2)))
	((consp b2)
	 (<= b1 (first b2)))
	(t
	 (< b1 b2))))

;;----------------------------------------------------------------------
;; COMPLEX types. We do not need to register anything, because all
;; possibilities have been covered by the definitions above. We only have to
;; bring the type to canonical form, which is a union of all specialized
;; complex types that can store an element of the corresponding type.
;;
(defun canonical-complex-type (real-type)
  ;; UPGRADE-COMPLEX-PART-TYPE will signal an error if REAL-TYPE
  ;; is not a subtype of REAL.
  (unless (eq real-type '*)
    (upgraded-complex-part-type real-type))
  (or (find-registered-tag '(COMPLEX REAL))
      (let ((tag (new-type-tag)))
	(push-type '(COMPLEX REAL) tag))))

;;----------------------------------------------------------------------
;; CONS types. Only (CONS T T) and variants, as well as (CONS NIL *), etc
;; are strictly supported.
;;
(defun register-cons-type (&optional (car-type '*) (cdr-type '*))
  ;; The problem with the code below is that it does not suport infinite
  ;; recursion. Instead we just canonicalize everything to CONS, irrespective
  ;; of whether the arguments are valid types or not!
  (let ((car-tag (if (eq car-type '*) -1 (canonical-type car-type)))
	(cdr-tag (if (eq cdr-type '*) -1 (canonical-type cdr-type))))
    (cond ((or (zerop car-tag) (zerop cdr-tag))
	   0)
	  ((and (= car-tag -1) (= cdr-tag -1))
	   (canonical-type 'CONS))
	  (t
	   (throw '+canonical-type-failure+ 'CONS)))))

;;----------------------------------------------------------------------
;; FIND-BUILT-IN-TAG
;;
;; This function computes the tags for all builtin types. We used to
;; do this computation and save it. However, for most cases it seems
;; faster if we just repeat it every time we need it, because the list of
;; *elementary-types* is kept smaller and *highest-type-tag* may be just
;; a fixnum.
;;
;; Note 1: There is some redundancy between this and the built-in
;; classes definitions. REGISTER-CLASS knows this and calls
;; FIND-BUILT-IN-TAG, which has priority. This is because some built-in
;; classes are also interpreted as intervals, arrays, etc.
;;
;; Note 2: All built in types listed here have to be symbols.
;;
(defconstant-equal +built-in-type-list+
    '((SYMBOL)
      (KEYWORD NIL SYMBOL)
      (PACKAGE)
      (COMPILED-FUNCTION)
      (FUNCTION (OR COMPILED-FUNCTION GENERIC-FUNCTION))
      ;;; backported from ecl
      (FIXNUM (INTEGER #.most-negative-fixnum #.most-positive-fixnum))
      (BIGNUM (OR (INTEGER * (#.most-negative-fixnum)) (INTEGER (#.most-positive-fixnum) *)))
      
      (INTEGER (INTEGER * *))
      ;;; the type need to exist, even if short-float is equal to single-float
      (SHORT-FLOAT (SHORT-FLOAT * *))
      (SINGLE-FLOAT (SINGLE-FLOAT * *))
      (DOUBLE-FLOAT (DOUBLE-FLOAT * *))
      ;;; the type need to exist, even if long-float is equal to double-float
      (LONG-FLOAT (LONG-FLOAT * *))
      (RATIO (RATIO * *))
      
      (RATIONAL (OR INTEGER RATIO))
      (FLOAT (OR SINGLE-FLOAT DOUBLE-FLOAT
              #+long-float LONG-FLOAT))
      (REAL (OR INTEGER
             #+short-float SHORT-FLOAT
             SINGLE-FLOAT
             DOUBLE-FLOAT
             #+long-float LONG-FLOAT
             RATIO))
      (COMPLEX (COMPLEX REAL))

      (NUMBER (OR REAL COMPLEX))

      (CHARACTER)
      #-unicode
      (BASE-CHAR CHARACTER)
      #+unicode
      (BASE-CHAR NIL CHARACTER)
      (STANDARD-CHAR NIL BASE-CHAR)

      (CONS)
      (NULL (MEMBER NIL))
      (LIST (OR CONS (MEMBER NIL)))

      (ARRAY (ARRAY * *))
      (SIMPLE-ARRAY (SIMPLE-ARRAY * *))
      (SIMPLE-VECTOR (SIMPLE-ARRAY T (*)))
      (SIMPLE-BIT-VECTOR (SIMPLE-ARRAY BIT (*)))
      (VECTOR (ARRAY * (*)))

      ;;; FIXME?: The simple-mdarray types are not exact -
      ;;; simple-mdarrays are never vectors.
      ;;; (mdarrays are superclasses of complex vectors, tho.)
      (core:simple-vector-byte2-t (simple-array ext:byte2 (*)))
      (core:simple-vector-byte4-t (simple-array ext:byte4 (*)))
      (core:simple-vector-byte8-t (simple-array ext:byte8 (*)))
      (core:simple-vector-byte16-t (simple-array ext:byte16 (*)))
      (core:simple-vector-byte32-t (simple-array ext:byte32 (*)))
      (core:simple-vector-byte64-t (simple-array ext:byte64 (*)))
      (core:simple-vector-int2-t (simple-array ext:integer2 (*)))
      (core:simple-vector-int4-t (simple-array ext:integer4 (*)))
      (core:simple-vector-int8-t (simple-array ext:integer8 (*)))
      (core:simple-vector-int16-t (simple-array ext:integer16 (*)))
      (core:simple-vector-int32-t (simple-array ext:integer32 (*)))
      (core:simple-vector-int64-t (simple-array ext:integer64 (*)))
      (core:simple-vector-fixnum (simple-array fixnum (*)))
      (core:simple-vector-double (simple-array double-float (*)))
      (core:simple-vector-float (simple-array single-float (*)))
      (core:complex-vector-byte2-t (complex-array ext:byte2 (*)))
      (core:complex-vector-byte4-t (complex-array ext:byte4 (*)))
      (core:complex-vector-byte8-t (complex-array ext:byte8 (*)))
      (core:complex-vector-byte16-t (complex-array ext:byte16 (*)))
      (core:complex-vector-byte32-t (complex-array ext:byte32 (*)))
      (core:complex-vector-byte64-t (complex-array ext:byte64 (*)))
      (core:complex-vector-int2-t (complex-array ext:integer2 (*)))
      (core:complex-vector-int4-t (complex-array ext:integer4 (*)))
      (core:complex-vector-int8-t (complex-array ext:integer8 (*)))
      (core:complex-vector-int16-t (complex-array ext:integer16 (*)))
      (core:complex-vector-int32-t (complex-array ext:integer32 (*)))
      (core:complex-vector-int64-t (complex-array ext:integer64 (*)))
      (core:complex-vector-fixnum (complex-array fixnum (*)))
      (core:complex-vector-double (complex-array double-float (*)))
      (core:complex-vector-float (complex-array single-float (*)))
      (core:MDARRAY-BASE-CHAR (array base-char *))
      (core:MDARRAY-BIT (array bit *))
      (core:mdarray-byte2-t (array ext:byte2 *))
      (core:mdarray-byte4-t (array ext:byte4 *))
      (core:mdarray-byte8-t (array ext:byte8 *))
      (core:MDARRAY-BYTE16-T (array ext:BYTE16 *))
      (core:MDARRAY-BYTE32-T (array ext:BYTE32 *))
      (core:MDARRAY-BYTE64-T (array ext:BYTE64 *))
      (core:MDARRAY-CHARACTER (array character *))
      (core:MDARRAY-DOUBLE (array double-float *))
      (core:MDARRAY-FIXNUM (array fixnum *))
      (core:MDARRAY-FLOAT (array single-float *))
      (core:mdarray-int2-t (array ext:integer2 *))
      (core:mdarray-int4-t (array ext:integer4 *))
      (core:mdarray-int8-t (array ext:integer8 *))
      (core:MDARRAY-INT16-T (array ext:integer16 *))
      (core:MDARRAY-INT32-T (array ext:integer32 *))
      (core:MDARRAY-INT64-T (array ext:integer64 *))
      (core:MDARRAY-T (array T *))
      (core:SIMPLE-MDARRAY-BASE-CHAR (simple-array base-char *))
      (core:SIMPLE-MDARRAY-BIT (simple-array bit *))
      (core:simple-mdarray-byte2-t (simple-array ext:byte2 *))
      (core:simple-mdarray-byte4-t (simple-array ext:byte4 *))
      (core:SIMPLE-MDARRAY-BYTE8-T (simple-array ext:BYTE8 *))
      (core:SIMPLE-MDARRAY-BYTE16-T (simple-array ext:byte16 *))
      (core:SIMPLE-MDARRAY-BYTE32-T (simple-array ext:BYTE32 *))
      (core:SIMPLE-MDARRAY-BYTE64-T (simple-array ext:BYTE64 *))
      (core:SIMPLE-MDARRAY-CHARACTER (simple-array CHARACTER *))
      (core:SIMPLE-MDARRAY-DOUBLE (simple-array DOUBLE-FLOAT *))
      (core:SIMPLE-MDARRAY-FIXNUM (simple-array fixnum *))
      (core:SIMPLE-MDARRAY-FLOAT (simple-array SINGLE-FLOAT *))
      (core:simple-mdarray-int2-t (simple-array  ext:integer2 *))
      (core:simple-mdarray-int4-t (simple-array  ext:integer4 *))
      (core:SIMPLE-MDARRAY-INT8-T (simple-array  ext:INTEGER8 *))
      (core:SIMPLE-MDARRAY-INT16-T (simple-array ext:INTEGER16 *))
      (core:SIMPLE-MDARRAY-INT32-T (simple-array ext:INTEGER32 *))
      (core:SIMPLE-MDARRAY-INT64-T (simple-array ext:INTEGER64 *))
      (core:SIMPLE-MDARRAY-T (simple-array T *))

      (STRING (ARRAY CHARACTER (*)))
      #+unicode
      (BASE-STRING (ARRAY BASE-CHAR (*)))
      (SIMPLE-STRING (SIMPLE-ARRAY CHARACTER (*)))
      #+unicode
      (SIMPLE-BASE-STRING (SIMPLE-ARRAY BASE-CHAR (*)))
      (BIT-VECTOR (ARRAY BIT (*)))

      (SEQUENCE (OR CONS (MEMBER NIL) (ARRAY * (*))))

      (HASH-TABLE)
      (PATHNAME)
      (LOGICAL-PATHNAME NIL PATHNAME)

      (BROADCAST-STREAM)
      (CONCATENATED-STREAM)
      (ECHO-STREAM)
      (FILE-STREAM)
      (STRING-STREAM)
      (SYNONYM-STREAM)
      (TWO-WAY-STREAM)
      (EXT:SEQUENCE-STREAM)
      ;;; backported from ecl
      (EXT:ANSI-STREAM (OR BROADCAST-STREAM CONCATENATED-STREAM ECHO-STREAM
                        FILE-STREAM STRING-STREAM SYNONYM-STREAM TWO-WAY-STREAM
                        EXT:SEQUENCE-STREAM))
      #+clos-streams (GRAY:FUNDAMENTAL-STREAM)
      (STREAM (OR EXT:ANSI-STREAM #+clos-streams GRAY:FUNDAMENTAL-STREAM))

      (READTABLE)
      #+threads (MP::PROCESS)
      #+threads (MP::LOCK)
      #+threads (MP::RWLOCK)
      #+threads (MP::CONDITION-VARIABLE)
      #+threads (MP::SEMAPHORE)
      #+threads (MP::BARRIER)
      #+threads (MP::MAILBOX)
      #+ffi (FOREIGN-DATA)
      #+sse2 (EXT:SSE-PACK (OR EXT:INT-SSE-PACK
                            EXT:FLOAT-SSE-PACK
                            EXT:DOUBLE-SSE-PACK))
      #+sse2 (EXT:INT-SSE-PACK)
      #+sse2 (EXT:FLOAT-SSE-PACK)
      #+sse2 (EXT:DOUBLE-SSE-PACK)
      (CODE-BLOCK)
      ))

(defun hash-table-fill (ht values)
  (dolist (pair values)
    (let ((key (car pair))
	  (value (cdr pair)))
      (core::hash-table-setf-gethash ht key value)))
  ht)

(defconstant-eqx +built-in-types+
  (hash-table-fill
   (make-hash-table :test 'eq :size 128)
   '#.sys::+built-in-type-list+)
  equalp)

(defun find-built-in-tag (name)
  (let (record)
    (cond ((eq name T)
	   -1)
	  ((eq (setq record (gethash name +built-in-types+ name))
		    name)
	   nil)
	  (t
	   (let* ((alias (pop record))
		  tag)
	     (if alias
		 (setq tag (canonical-type alias))
		 (let* ((strict-supertype (or (first record) 'T))
			(strict-supertype-tag (canonical-type strict-supertype)))
		   (setq tag (new-type-tag))
		   (unless (eq strict-supertype 't)
		     (extend-type-tag tag strict-supertype-tag))))
	     (push-type name tag))))))

(defun extend-type-tag (tag minimal-supertype-tag)
  (dolist (type *elementary-types*)
    (let ((other-tag (cdr type)))
      (when (zerop (logandc2 minimal-supertype-tag other-tag))
	(cons-setf-cdr type (logior tag other-tag))))))

;;----------------------------------------------------------------------
;; (CANONICAL-TYPE TYPE)
;;
;; This function registers all types mentioned in the given expression,
;; and outputs a code corresponding to the represented type. This
;; function has side effects: it destructively modifies the content of
;; *ELEMENTARY-TYPES* and *MEMBER-TYPES*.
;;
(defun canonical-type (type)
  (declare (notinline clos::classp))
  (cond ((find-registered-tag type))
	((eq type 'T) -1)
	((eq type 'NIL) 0)
        ((symbolp type)
	 (let ((expander (ext:type-expander type)))
	   (cond (expander
		  (canonical-type (funcall expander type nil)))
		 ((find-built-in-tag type))
		 (t (let ((class (find-class type nil)))
		      (if class
			  (progn
			    (register-class class))
			  (progn
			    (throw '+canonical-type-failure+ nil))
			  ))))))
	((consp type)
	 (case (first type)
	   (AND (apply #'logand (mapcar #'canonical-type (rest type))))
	   (OR (apply #'logior (mapcar #'canonical-type (rest type))))
	   (NOT (lognot (canonical-type (second type))))
	   ((EQL MEMBER) (apply #'logior (mapcar #'register-member-type (rest type))))
	   (SATISFIES (register-satisfies-type type))
	   ((INTEGER SINGLE-FLOAT DOUBLE-FLOAT RATIO #+long-float LONG-FLOAT)
	    (register-interval-type type))
	   ((FLOAT)
	    (canonical-type `(OR (SINGLE-FLOAT ,@(rest type))
				 (DOUBLE-FLOAT ,@(rest type))
				 #+long-float
				 (LONG-FLOAT ,@(rest type)))))
	   ((REAL)
	    (canonical-type `(OR (INTEGER ,@(rest type))
				 (RATIO ,@(rest type))
				 (SINGLE-FLOAT ,@(rest type))
				 (DOUBLE-FLOAT ,@(rest type))
				 #+long-float
				 (LONG-FLOAT ,@(rest type)))))
	   ((RATIONAL)
	    (canonical-type `(OR (INTEGER ,@(rest type))
				 (RATIO ,@(rest type)))))
	   (COMPLEX
	    (or (find-built-in-tag type)
		(canonical-complex-type (second type))))
	   (CONS (apply #'register-cons-type (rest type)))
	   (ARRAY (logior (register-array-type `(COMPLEX-ARRAY ,@(rest type)))
			  (register-array-type `(SIMPLE-ARRAY ,@(rest type)))))
	   ((COMPLEX-ARRAY SIMPLE-ARRAY) (register-array-type type))
	   (FUNCTION (canonical-type 'FUNCTION))
	   (t (let ((expander (ext:type-expander (first type))))
		(if expander
		    (canonical-type (funcall expander type nil))
		    (unless (assoc (first type) *elementary-types*)
		      (throw '+canonical-type-failure+ nil)))))))
	((clos::classp type)
	 (register-class type))
	(t
	 (error-type-specifier type))))

(defun safe-canonical-type (type)
  (catch '+canonical-type-failure+
    (canonical-type type)))

(defun fast-subtypep (t1 t2)
  (when (eq t1 t2)
    (return-from fast-subtypep (values t t)))
  (let* ((tag1 (safe-canonical-type t1))
	 (tag2 (safe-canonical-type t2)))
    (cond ((and (numberp tag1) (numberp tag2))
	   (values (zerop (logandc2 (safe-canonical-type t1)
				    (safe-canonical-type t2)))
		   t))
	  (t
	   (values nil nil)))))

(defun subtypep (t1 t2 &optional env)
  (declare (ignore env))
  ;; One easy case: types are equal
  (when (eq t1 t2)
    (return-from subtypep (values t t)))
  ;; Another easy case: types are classes.
  (when (and (clos::classp t1) (clos::classp t2))
    (return-from subtypep (values (subclassp t1 t2) t)))
  ;; Finally, cached results.
  (let* ((cache *subtypep-cache*)
         (hash (the (integer 0 255) (logand (hash-eql t1 t2) 255)))
         (elt (aref cache hash)))
    (when (and elt (eq (caar elt) t1) (eq (cdar elt) t2))
      (setq elt (cdr elt))
      (return-from subtypep (values (car elt) (cdr elt))))
    (let* ((*highest-type-tag* *highest-type-tag*)
	   (*save-types-database* t)
	   (*member-types* *member-types*)
	   (*elementary-types* *elementary-types*))
      (multiple-value-bind (test confident)
	  (fast-subtypep t1 t2)
	(row-major-aset cache hash (cons (cons t1 t2) (cons test confident)))
	(values test confident)))))

(defun fast-type= (t1 t2)
  (when (eq t1 t2)
    (return-from fast-type= (values t t)))
  (let* ((tag1 (safe-canonical-type t1))
	 (tag2 (safe-canonical-type t2)))
    (cond ((and (numberp tag1) (numberp tag2))
	   (values (= (safe-canonical-type t1) (safe-canonical-type t2))
		   t))
	  (t
	   (values nil nil)))))

(defun type= (t1 t2)
  (let ((*highest-type-tag* *highest-type-tag*)
	(*save-types-database* t)
	(*member-types* *member-types*)
	(*elementary-types* *elementary-types*))
    (fast-type= t1 t2)))
