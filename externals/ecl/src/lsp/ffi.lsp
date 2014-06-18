;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 2001, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; FFI	Symbols used in the foreign function interface

(in-package "FFI")

(import 'si:null-pointer-p)
(export 'si:null-pointer-p)

#-ecl-min
(clines "#include <string.h>")

(defmacro def-constant (name value &key (export nil))
  "Macro to define a constant and to export it"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defconstant ,name ,value)
     ,(when export (list 'export `(quote ,name)))
    ',name))

;;;----------------------------------------------------------------------
;;; FOREIGN TYPES
;;;

(defparameter *ffi-types* (make-hash-table :size 128))

(defparameter *use-dffi* t)

(defmacro def-foreign-type (name definition)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name ffi::*ffi-types*) ',definition)))

(defmacro def-type (name definition)
  (declare (ignore definition))
  `(deftype ,name () t))

(defun %convert-to-ffi-type (type &optional context)
  (if (atom type)
    (if (member type context)
      type
      (multiple-value-bind (value present-p) (gethash type *ffi-types* type)
        (if present-p
          (%convert-to-ffi-type value (cons type context))
          value)))
    (cons (%convert-to-ffi-type (first type) context)
          (%convert-to-ffi-type (rest type) context))))

(defmacro %align-data (data align)
  `(setf ,data (* (ceiling (/ ,data ,align)) ,align)))

(defun size-of-foreign-type (name)
  (let* ((size 0) align
	 (type (%convert-to-ffi-type name)))
    (unless type
      (error "Incomplete or unknown foreign type ~A" name))
    (cond ((symbolp type)
	   (setf size (si:size-of-foreign-elt-type type)
                 align (si:alignment-of-foreign-elt-type type)))
	  ((atom type)
	   (error "~A is not a valid foreign type identifier" name))
	  ((eq (setf name (first type)) :struct)
	   (setf size (slot-position type nil)
                 align (apply #'max (mapcar #'(lambda (field)
	                                        (multiple-value-bind (field-size field-align)
                                                    (size-of-foreign-type (second field))
						  (declare (ignore field-size))
						  field-align))
	                                    (rest type))))
	   (%align-data size align))
	  ((eq name :array)
	   (unless (and (setf size (third type)) (realp size))
	     (error "Incomplete foreign type: ~S" type))
	   (multiple-value-bind (elt-size elt-align)
	     (size-of-foreign-type (second type))
	     (setf size (* size elt-size)
                   align elt-align)))
	  ((eq name :union)
	   (dolist (field (rest type))
	     (multiple-value-bind (field-size field-align)
	       (size-of-foreign-type (second field))
	       (when (> field-size size)
	         (setf size field-size))
	       (when (or (null align) (> field-align align))
	         (setf align field-align)))))
	  ((eq name '*)
	   (setf size (si:size-of-foreign-elt-type :pointer-void)
                 align (si:alignment-of-foreign-elt-type :pointer-void)))
          ((eq name 'quote)
           (return-from size-of-foreign-type
             (size-of-foreign-type (second type))))
	  (t
	   (error "~A does not denote a foreign type" name)))
    (values size (or align 0))))

(defun allocate-foreign-object (type &optional (size 0 size-flag))
  (let ((type-size (size-of-foreign-type type)))
    (cond ((null size-flag)
	   (si::allocate-foreign-data type type-size))
	  ((and (typep size 'fixnum) (>= size 0))
	   (let ((bytes (* size type-size)))
	     (si::allocate-foreign-data `(:array ,type ,size) bytes)))
	  (t
	   (error "~A is not a valid array dimension size" size)))))

(defun free-foreign-object (ptr)
  (si::free-foreign-data ptr))

;;;----------------------------------------------------------------------
;;; ENUMERATION TYPES
;;;

(defmacro def-enum (name values-list &key (separator-string "#"))
  (let ((constants '())
	(value -1)
	field
	forms)
    (setf #| name (string name) |#
	  separator-string (string separator-string))
    (dolist (item values-list)
      (cond ((symbolp item)
	     (setf field item)
	     (incf value))
	    ((and (consp item)
		  (symbolp (setf field (first item)))
		  (integerp (setf value (second item)))
		  (endp (cddr item))))
	    (t
	     (error "Not a valid argument to DEF-ENUM~%~a" values-list)))
      (setf field (concatenate 'string
			       (symbol-name name)
			       separator-string
			       (string field)))
      (push `(defconstant ,(intern field (symbol-package name))
	       ',value)
	    forms))
    `(progn
       (def-foreign-type ,name :int)
       ,@forms)))


;;;----------------------------------------------------------------------
;;; STRUCTURE TYPES
;;;
;;; The structure type is represented by the following list:
;;;
;;;	(STRUCT (SLOT-NAME1 . SLOT-TYPE1)*)
;;;
;;; FIXME! We do not care about slot alignment!
;;;

(defmacro def-struct (name &rest slots)
  (let ((struct-type (list :struct))
	field
	type)
    (dolist (item (subst `(* ,name) :pointer-self slots))
      (if (and (consp item)
	       (= (length item) 2)
	       (symbolp (setf field (first item))))
	(setf type (second item))
	(error "Not a valid DEF-STRUCT slot ~A" item))
      (push (list field type) struct-type))
    `(def-foreign-type ,name ,(nreverse struct-type))))

(defun slot-position (type field)
  (setf type (%convert-to-ffi-type type))
  (let ((ndx 0)
	(is-union nil))
    (cond ((atom type)
	   (error "~A is not a foreign STRUCT or UNION type" type))
	  ((eq (first type) :struct))
	  ((eq (first type) :union)
	   (setf is-union t))
	  (t
	   (error "~A is not a foreign STRUCT or UNION type" type)))
    (dolist (slot (rest type))
      (let* ((slot-name (car slot))
	     (slot-type (cadr slot)))
	(multiple-value-bind (slot-size slot-align)
	  (size-of-foreign-type slot-type)
	  (%align-data ndx slot-align)
	  (when (eq slot-name field)
	    (return-from slot-position (values ndx slot-type slot-size)))
	  (unless is-union
	    (incf ndx slot-size)))))
    (values ndx nil nil)))

(defun get-slot-value (object struct-type field)
  (multiple-value-bind (slot-ndx slot-type slot-size)
      (slot-position struct-type field)
    (unless slot-size
      (error "~A is not a field of the type ~A" field struct-type))
    (%foreign-data-ref object slot-ndx slot-type slot-size)))

(defun (setf get-slot-value) (value object struct-type field)
  (multiple-value-bind (slot-ndx slot-type slot-size)
      (slot-position struct-type field)
    (unless slot-size
      (error "~A is not a field of the type ~A" field struct-type))
    (%foreign-data-set object slot-ndx slot-type value)))

(defun get-slot-pointer (object struct-type field)
  (multiple-value-bind (slot-ndx slot-type slot-size)
      (slot-position struct-type field)
    (unless slot-size
      (error "~A is not a field of the type ~A" field struct-type))
    (si::foreign-data-pointer object slot-ndx slot-size slot-type)))


;;;----------------------------------------------------------------------
;;; ARRAYS
;;;

(defmacro def-array-pointer (name element-type)
  `(def-foreign-type ,name (* ,element-type)))

(defun deref-array (array array-type position)
  (setf array-type (%convert-to-ffi-type array-type))
  (let* ((element-type (second array-type))
	 (element-size (size-of-foreign-type element-type))
	 (ndx (* position element-size))
	 (length (or (third array-type) '*)))
    (unless (or (eq length '*)
		(> length position -1))
      (error "Out of bounds when accessing array ~A." array))
    (%foreign-data-ref (si::foreign-data-recast array (+ ndx element-size) array-type) ndx element-type element-size)))

(defun (setf deref-array) (value array array-type position)
  (setf array-type (%convert-to-ffi-type array-type))
  (let* ((element-type (second array-type))
	 (element-size (size-of-foreign-type element-type))
	 (ndx (* position element-size))
	 (length (or (third array-type) '*)))
    (unless (or (eq length '*)
		(> length position -1))
      (error "Out of bounds when accessing array ~A." array))
    (%foreign-data-set (si::foreign-data-recast array (+ ndx element-size) array-type) ndx element-type value)))

(defun %foreign-data-set (obj ndx type value)
  (cond ((foreign-elt-type-p type)
         (si::foreign-data-set-elt obj ndx type value))
	((atom type)
	 (error "Unknown foreign primitive type: ~A" type))
	((eq (first type) '*)
	 (si::foreign-data-set-elt obj ndx :pointer-void value))
	(t
	 (si::foreign-data-set obj ndx value))))

(defun %foreign-data-ref (obj ndx type &optional (size 0 size-p))
  (cond ((foreign-elt-type-p type)
         (si::foreign-data-ref-elt obj ndx type))
	((atom type)
	 (error "Unknown foreign primitive type: ~A" type))
	((eq (first type) '*)
	 (si::foreign-data-recast (si::foreign-data-ref-elt obj ndx :pointer-void)
	                          (size-of-foreign-type (second type))
				  type))
	(t
	 (si::foreign-data-ref obj ndx (if size-p size (size-of-foreign-type type)) type))))

;;;----------------------------------------------------------------------
;;; UNIONS
;;;

(defmacro def-union (name &rest slots)
  (let ((struct-type (list :union))
	field
	type)
    (dolist (item (subst `(* ,struct-type) :pointer-self slots))
      (unless (and (consp item)
		   (= (length item) 2)
		   (symbolp (setf field (first item))))
	(error "Not a valid DEF-UNION slot ~A" item))
      (setf type (second item))
      (push (list field type) struct-type))
    `(def-foreign-type ,name ,(nreverse struct-type))))

;;;----------------------------------------------------------------------
;;; POINTERS
;;;

(defparameter +null-cstring-pointer+ (si:allocate-foreign-data :pointer-void 0))

(defun pointer-address (ptr)
  (si::foreign-data-address ptr))

(defun deref-pointer (ptr type)
  ;; FIXME! No checking!
  (setf type (%convert-to-ffi-type type))
  (cond ((foreign-elt-type-p type)
         (si::foreign-data-ref-elt ptr 0 type))
	((atom type)
	 (error "Unknown foreign primitive type: ~A" type))
	((eq (first type) '*)
	 (si::foreign-data-recast (si::foreign-data-ref-elt ptr 0 :pointer-void)
	                          (size-of-foreign-type (second type))
				  (second type)))
	(t
	 (error "Cannot dereference pointer to foreign data, ~A" ptr))
  ))

(defun (setf deref-pointer) (value ptr type)
  ;; FIXME! No checking!
  (setf type (%convert-to-ffi-type type))
  (if (foreign-elt-type-p type)
      (si::foreign-data-set-elt ptr 0 type value)
      (si::foreign-data-set ptr 0 value)))

(defun make-null-pointer (type)
  ;(setf type (%convert-to-ffi-type type))
  (si::allocate-foreign-data type 0))

(defun make-pointer (addr type)
  (c-inline (type (size-of-foreign-type type) addr) (:object :unsigned-long :unsigned-long) :object
            "ecl_make_foreign_data(#0, #1, (void*)#2)"
	    :side-effects t
	    :one-liner t))

#+(OR) ;; Already defined in core
(defun null-pointer-p (object)
  (si::null-pointer-p object))


;;;----------------------------------------------------------------------
;;; CHARACTERS AND STRINGS
;;;
;;; ECL always returns characters when dereferencing (:array * :char)
;;;

(defun null-char-p (char)
  (eq char #.(code-char 0)))

(defun ensure-char-character (char)
  (cond ((characterp char) char)
	((integerp char) (code-char char))
	(t (error "~a cannot be coerced to type CHARACTER" char))))

(defun ensure-char-integer (char)
  (cond ((characterp char) (char-code char))
	((integerp char) char)
	(t (error "~a cannot be coerced to type INTEGER" char))))

(defun ensure-char-storable (char)
  char)

(defun char-array-to-pointer (obj)
  (si::foreign-data-pointer obj 0 1 '(* :unsigned-char)))

(defmacro convert-from-cstring (object)
  object)

(defmacro convert-to-cstring (object)
  ;; This enforces that the string contains only as many characters as the
  ;; fill-pointer determines Since ECL always sets a 0 character after the
  ;; last element of a string, this way, the string is always zero-terminated
  `(si:copy-to-simple-base-string ,object))

(defmacro free-cstring (object)
  object)

(defmacro with-cstring ((cstring string) &body body)
  `(let ((,cstring (convert-to-cstring ,string))) ,@body))

(defmacro with-cstrings (bindings &rest body)
  (if bindings
    `(with-cstring ,(car bindings)
      (with-cstrings ,(cdr bindings)
        ,@body))
    `(progn ,@body)))

(defun foreign-string-length (foreign-string)
  (c-inline (foreign-string) (t) :int
	    "strlen((#0)->foreign.data)"
	    :side-effects nil
	    :one-liner t))

(defun convert-from-foreign-string (foreign-string
				    &key length (null-terminated-p t))
  (cond ((and (not length) null-terminated-p)
	 (setf length (foreign-string-length foreign-string)))
	((not (integerp length))
	 (error "~A is not a valid string length" length)))
  (c-inline (foreign-string length) (t fixnum) string
       "{
	cl_index length = #1;
	cl_object output = ecl_alloc_simple_base_string(length);
	memcpy(output->base_string.self, (#0)->foreign.data, length);
	@(return) = output;
	}"
       :one-liner nil
       :side-effects t))

(defun convert-to-foreign-string (string-designator)
  (let ((lisp-string (string string-designator)))
    (c-inline (lisp-string) (t) t
       "{
	cl_object lisp_string = #0;
	cl_index size = lisp_string->base_string.fillp;
	cl_object output = ecl_allocate_foreign_data(@(* :char), size+1);
	memcpy(output->foreign.data, lisp_string->base_string.self, size);
	output->foreign.data[size] = '\\0';
	@(return) = output;
	}"
	:one-liner nil
	:side-effects t)
    ))

(defun allocate-foreign-string (size &key unsigned)
  (si::allocate-foreign-data `(* ,(if unsigned :unsigned-char :char))
			     (1+ size)))

(defmacro with-foreign-string ((foreign-string lisp-string) &rest body)
  (let ((result (gensym)))
    `(let* ((,foreign-string (convert-to-foreign-string ,lisp-string))
            (,result (progn ,@body)))
       (free-foreign-object ,foreign-string)
       ,result)))

(defmacro with-foreign-strings (bindings &rest body)
  (if bindings
    `(with-foreign-string ,(car bindings)
      (with-foreign-strings ,(cdr bindings)
        ,@body))
    `(progn ,@body)))

;;;----------------------------------------------------------------------
;;; MACROLOGY
;;;

(defmacro with-foreign-object ((var type) &body body)
  `(let ((,var (allocate-foreign-object ,type)))
     (unwind-protect
	 (progn ,@body)
       (free-foreign-object ,var))))

(defmacro with-foreign-objects (bindings &rest body)
  (if bindings
    `(with-foreign-object ,(car bindings)
      (with-foreign-objects ,(cdr bindings)
        ,@body))
    `(progn ,@body)))

(defmacro with-cast-pointer (bind &body body)
  (let (binding-name ptr type)
    (case (length bind)
      (2 (setf binding-name (first bind)
	       ptr binding-name
	       type (second bind)))
      (3 (setf binding-name (first bind)
	       ptr (second bind)
	       type (third bind)))
      (otherwise (error "Arguments missing in WITH-CAST-POINTER")))
    `(let ((,binding-name (si::foreign-data-pointer (si::foreign-data-recast ,ptr (size-of-foreign-type ',type) :void) 0
						    (size-of-foreign-type ',type)
						    ',type)))
       ,@body)))

;;;----------------------------------------------------------------------
;;; INTERFACE TO C FUNCTIONS AND VARIABLES
;;;

(defun lisp-to-c-name (name)
  (cond ((or (stringp name)
             (symbolp name))
	 (values name (intern (string-upcase (substitute #\- #\_ (string name))))))
	((and (consp name)
	      (= (length name) 2))
	 (values (first name) (second name)))))

(defun %convert-to-arg-type (type)
  (let ((type (%convert-to-ffi-type type)))
    (cond ((atom type) type)
          ((eq (first type) '*) :pointer-void)
	  ((eq (first type) :array) :pointer-void)
	  (t (error "Unsupported argument type: ~A" type))
    )))

(defun %convert-to-return-type (type)
  (let ((type (%convert-to-ffi-type type)))
    (cond ((atom type) type)
          ((eq (first type) '*) (second type))
	  (t type))))

(defun produce-function-call (c-name nargs)
  (declare (si::c-local))
  (format nil "~a(~a)" c-name
	  (subseq "#0,#1,#2,#3,#4,#5,#6,#7,#8,#9,#a,#b,#c,#d,#e,#f,#g,#h,#i,#j,#k,#l,#m,#n,#o,#p,#q,#r,#s,#t,#u,#v,#w,#x,#y,#z"
		  0 (max 0 (1- (* nargs 3))))))

;;; FIXME! We should turn this into a closure generator that produces no code.
#+DFFI
(defmacro def-lib-function (name args &key returning module (call :cdecl))
  (multiple-value-bind (c-name lisp-name) (lisp-to-c-name name)
    (let* ((return-type (ffi::%convert-to-return-type returning))
	   (return-required (not (eq return-type :void)))
	   (argtypes (mapcar #'(lambda (a) (ffi::%convert-to-arg-type (second a))) args)))
      `(let ((c-fun (si::find-foreign-symbol ',c-name ,module :pointer-void 0)))
	(defun ,lisp-name ,(mapcar #'first args)
	  (si::call-cfun c-fun ',return-type ',argtypes (list ,@(mapcar #'first args)) ,call))))))

(defmacro def-function (name args &key module (returning :void) (call :cdecl))
  #+DFFI 
  (when (and module *use-dffi*)
    (return-from def-function
      `(def-lib-function ,name ,args :returning ,returning :module ,module :call ,call)))
  (multiple-value-bind (c-name lisp-name)
      (lisp-to-c-name name)
    (let* ((arguments (mapcar #'first args))
	   (arg-types (mapcar #'(lambda (type) (%convert-to-arg-type (second type))) args))
	   (return-type (%convert-to-return-type returning))
	   (nargs (length arguments))
	   (c-string (produce-function-call c-name nargs))
	   (casting-required (not (or (member return-type '(:void :cstring))
				      (foreign-elt-type-p return-type))))
	   (inline-form `(c-inline ,arguments ,arg-types
				   ,(if casting-required :pointer-void return-type)
				   ,c-string
				   :one-liner t
				   :side-effects t)))
      (when casting-required
	(setf inline-form
	      `(si::foreign-data-recast ,inline-form
					(size-of-foreign-type ',return-type)
					',return-type)))
      (when (> nargs 36)
	(error "FFI can only handle C functions with up to 36 arguments"))
      `(defun ,lisp-name (,@arguments)
	 ,inline-form)
      )))

(defmacro def-foreign-var (name type module)
  ;(declare (ignore module))
  (multiple-value-bind (c-name lisp-name)
      (lisp-to-c-name name)
    (let* ((ffi-type (%convert-to-ffi-type type))
           (can-deref (or (foreign-elt-type-p ffi-type)
                          (and (consp ffi-type)
                               (member (first ffi-type) '(* :array)))))
	   (inline-form (cond #+dffi
                              ((and module *use-dffi*)
			       `(si::find-foreign-symbol ,c-name ,module ',type ,(size-of-foreign-type type)))
			      (t
			       `(c-inline () () :object
					  ,(format nil "ecl_make_foreign_data(@~S, ~A, &~A)"
						   type (size-of-foreign-type type) c-name)
					  :side-effects t :one-liner t)))))
      (if can-deref
          `(progn
             (put-sysprop ',lisp-name 'ffi-foreign-var ,inline-form)
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (define-symbol-macro ,lisp-name
                 (ffi:deref-pointer (get-sysprop ',lisp-name 'ffi-foreign-var) ',type)
                 )))
          `(defparameter ,lisp-name ,inline-form))
      )))

(defun find-foreign-library (names directories &key drive-letters types)
  (unless (listp names)
    (setq names (list names)))
  (unless (listp directories)
    (setq directories (list directories)))
  (unless types
    (setq types #+win32 '("lib")
                #-win32 '("so" "a")))
  (unless (listp types)
    (setq types (list types)))
  (unless (listp drive-letters)
    (setq drive-letters (list drive-letters)))
  #-msvc
  (setq drive-letters '(nil))
  #+msvc
  (unless drive-letters
    (setq drive-letters '(nil)))
  (dolist (d drive-letters)
    (dolist (p directories)
      (dolist (n names)
        (dolist (e types)
	  (let ((full-path (probe-file (make-pathname
					  :device d
	                                  :directory (etypecase p
					               (pathname (pathname-directory p))
						       (string (pathname-directory (parse-namestring p)))
						       (list p))
					  :name n
					  :type e))))
	    (when full-path
	      (return-from find-foreign-library full-path))
	  )))))
  nil)

(defparameter +loaded-libraries+ nil)

(defun do-load-foreign-library (tmp &optional system-library)
 (let* ((path (cond ((pathnamep tmp) tmp)
                    ((probe-file (setf tmp (string tmp))) tmp)
                    (t (compile-file-pathname tmp :type #+msvc :lib #-msvc :dll))))
        (filename (namestring path))
        (pack (find-package "COMPILER"))
        (flag (if system-library
		  (concatenate 'string "-l" tmp)
		  filename)))
   (unless (find filename ffi::+loaded-libraries+ :test #'string-equal)
     (setf (symbol-value (intern "*LD-FLAGS*" pack))
	   (concatenate 'string (symbol-value (intern "*LD-FLAGS*" pack)) " " flag))
     (setf (symbol-value (intern "*LD-BUNDLE-FLAGS*" pack))
	   (concatenate 'string (symbol-value (intern "*LD-BUNDLE-FLAGS*" pack))
			" " flag))
     (setf (symbol-value (intern "*LD-SHARED-FLAGS*" pack))
	   (concatenate 'string (symbol-value (intern "*LD-SHARED-FLAGS*" pack))
			" " flag))
     (push filename ffi::+loaded-libraries+))
   t))

(defmacro load-foreign-library (filename &key module supporting-libraries force-load
				system-library &environment env)
 (declare (ignore module force-load supporting-libraries))
 (let ((compile-form (and (constantp filename env)
                          `((eval-when (:compile-toplevel)
                              (do-load-foreign-library ,filename
				,(ext:constant-form-value system-library))))))
       (dyn-form #+dffi (when (and (not system-library) *use-dffi*)
                          `((si:load-foreign-module ,filename)))
                 #-dffi nil))
   `(progn ,@compile-form ,@dyn-form)))

;;;----------------------------------------------------------------------
;;; CALLBACKS
;;;

#-dffi
(defmacro defcallback (&rest args)
  (error "DEFCALLBACK cannot be used in interpreted forms"))

#+dffi
(defmacro defcallback (name ret-type arg-desc &body body)
  (if *use-dffi*
      (multiple-value-bind (name call-type) (if (consp name)
                                                (values-list name)
                                                (values name :cdecl))
        (let ((arg-types (mapcar #'second arg-desc))
              (arg-names (mapcar #'first arg-desc)))
          `(si::make-dynamic-callback
            #'(ext::lambda-block ,name ,arg-names ,@body)
            ',name ',ret-type ',arg-types ,call-type)))
      (error "DEFCALLBACK cannot be used in interpreted forms when DFFI is disabled.")))

(defun callback (name)
  (let ((x (si::get-sysprop name :callback)))
    (unless x
      (error "There is no callback with name ~a" name))
    (first x)))

;;;----------------------------------------------------------------------
;;; COMPATIBILITY WITH OLDER FFI
;;;

(defun clines (&rest args)
  (error "The special form clines cannot be used in the interpreter: ~A"
	 args))

(eval-when (:load-toplevel :execute)
  (defmacro c-inline (args arg-types ret-type &body others)
    `(error "The special form c-inline cannot be used in the interpreter: ~A"
      (list (list ,@args) ',arg-types ',ret-type ,@others)))
  (defmacro c-progn (&rest body)
    '(error "The special form c-progn cannot be used in the interpreter.")))

(defmacro definline (fun arg-types type code)
"Syntax: (definline symbol (&rest arg-types) result-type &body body) " "

DEFINLINE behaves like a DEFCBODY (see), but also instructs the LISP compiler
to expand inline any call to function SYMBOL into code corresponding
to the C language expression BODY, whenever it can determine that
the actual arguments are of the specified type."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
              ;; defCbody must go first, because it clears symbol-plist of fun
              (defCbody ,fun ,arg-types ,type ,code)
              (declaim (ftype (function ,arg-types ,type) ,fun))
	      (c::def-inline ,fun :always ,arg-types ,type ,code)))

(defmacro defla (&rest body)
"Syntax: (defla name lambda-list &body body)" "

Used to DEFine Lisp Alternative.  For the interpreter, DEFLA is equivalent to
DEFUN, but the compiler ignores this form."
  `(eval-when (:execute)
     (defun ,@body)))

(defmacro defcbody (name arg-types result-type C-expr)
"Syntax: (defcbody symbol (&rest arg-types) result-type &body body)" "

The compiler defines a Lisp function named by SYMBOL whose body consists of the
C code of the string BODY. In the BODY one can reference the arguments of the
function as \"#0\", \"#1\", etc.
The interpreter ignores this form.  ARG-TYPEs are argument types of the
defined Lisp function and VALUE-TYPE is its the return type."
  (let ((args (mapcar #'(lambda (x) (gensym)) arg-types)))
  `(defun ,name ,args
     (c-inline ,args ,arg-types ,result-type
	       ,C-expr :one-liner t))))

(defmacro defentry (name arg-types c-name &key no-interrupts)
"Syntax: (defentry symbol (&rest arg-types*) (result-type function-name))

The compiler defines a Lisp function named by SYMBOL whose body consists of a
calling sequence to the C language function named by FUNCTION-NAME.  The
interpreter ignores this form.  ARG-TYPEs are argument types of the C function
and VALUE-TYPE is the return type of the C function.  Symbols OBJECT, INT,
CHAR, CHAR*, FLOAT, DOUBLE are allowed for these types."
  (let ((output-type :object)
	(args (mapcar #'(lambda (x) (gensym)) arg-types)))
    (if (consp c-name)
	(setf output-type (first c-name)
	      c-name (second c-name)))
    (let* ((call (produce-function-call (string c-name) (length arg-types)))
	   (full-text (if no-interrupts
			  (concatenate 'string
				       "ecl_disable_interrupts();@(return)="
				       call
				       ";ecl_enable_interrupts();")
			  call)))
      `(defun ,name ,args
	 (c-inline ,args ,arg-types ,output-type
		   ,full-text
		   :one-liner ,(not no-interrupts))))))

