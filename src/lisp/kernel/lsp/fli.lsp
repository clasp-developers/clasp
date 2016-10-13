;; ===========================================================================
;;;                  F L I    I M P L E M E N T A T I O N
;;; ===========================================================================
;;; -- IMPLEMEMTATION NOTES ---
;;;
;;; The complete FLI is comprised of the following files:
;;; .../src/core/fli.cc            - corresponding .cc file
;;; .../include/clasp/core/fli.h   - corresponding .h file
;;; .../src/lisp/kernel/fli.lsp    - this file
;;;
;;; --- END OF IMPLEMEMTATION NOTES ---

;;;----------------------------------------------------------------------------
;;; TODO
;;;
;;; 001 Implement Foreign Function Calling
;;; 002 Implement Callback Support
;;;
;;;----------------------------------------------------------------------------

;;;----------------------------------------------------------------------------
;;; ISSUES / KNOWN PROBLEMS
;;;
;;;
;;;----------------------------------------------------------------------------

(in-package "CLASP-FFI")

;;; ===========================================================================
;;; FLI LISP SIDE IMPLEMENTATION

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; MACROS

(defmacro with-foreign-object ((var type) &body body)
  `(let ((,var (%allocate-foreign-object ,type)))
     (unwind-protect
          (progn ,@body)
       (%free-foreign-object ,var))))

(defmacro with-foreign-objects (bindings &rest body)
  (if bindings
      `(with-foreign-object ,(car bindings)
         (with-foreign-objects ,(cdr bindings)
           ,@body))
      `(progn ,@body)))

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------

(declaim (inline ensure-core-pointer))
(defun ensure-core-pointer (ptr)
  (cond
    ((not ptr) (error "#'ensure-core-ptr *** Illegal argument value: PTR may not be NIL!"))
    ((eql (type-of ptr) 'CLASP-FFI::FOREIGN-DATA) (%core-pointer-from-foreign-data ptr ))
    ((eql (type-of ptr) 'CORE::POINTER) ptr)
    (t (error "#'ensure-core-pointer *** Illegal type ~S of ptr. Cannot convert to core::pointer." (type-of ptr)))))


;;; === B U I L T - I N   F O R E I G N   T Y P E S ===

;;; Implemented directly in C++.
;;; Also: See vector *foreign-type-spec-table* => Vector with all foreign type
;;; specs, consisting each of:
;;; - Lisp symbol
;;; - Lisp name
;;; - size
;;; - alignment
;;; - C++ name
;;; Adding built-in foreign types requires adding a type pec to this table!

(defgeneric %lisp-type->lisp-name (type))

(defmacro generate-type-spec-accessor-functions ()
  `(progn
     ,@(loop for spec across *foreign-type-spec-table*
          for idx from 0 to (1- (length *foreign-type-spec-table*))
          when spec
	  collect
	    `(defmethod %lisp-type->type-spec ((type (eql ',(%lisp-symbol spec))))
               (elt *foreign-type-spec-table* ,idx)))
     ))

;;; === B U I LT - I N   O P E R A T I O N S ===

;;; Implemented directly in C++:
;;; - %foreign-type-size
;;; - %foreign-type-alignment

;;; === P O I N T E R   O P E R A T I O N S ===

;;; Implemented directly in C++:
;;; - %pointerp
;;; - %null-pointer
;;; - %null-pointer-p
;;; - %make-pointer
;;; - %inc-pointer

;;; === F O R E I G N - A L L O C / - F R E E ===

(declaim (inline %foreign-alloc))
(defun %foreign-alloc (size)
  (%allocate-foreign-data size))

(declaim (inline %foreign-free))
(defun %foreign-free (ptr)
  (%free-foreign-data ptr))

;;; These code lines were debugged with the help of drneister, stassats and
;;; Shinmera on 2016-09-22 as a joint effort on IRC channel #clasp. Thx again!!

;;; === M E M - R E F ===

(defgeneric %mem-ref (ptr type &optional offset))

(defmacro generate-mem-ref-accessor-functions ()
  `(progn
     ,@(loop for spec across *foreign-type-spec-table*
          when spec
	  collect
	    `(defmethod %mem-ref (ptr (type (eql ',(%lisp-symbol spec))) &optional (offset 0))
	       (funcall ',(intern (concatenate 'string "%MEM-REF-" (string (%lisp-symbol spec))) 'clasp-ffi)
                        (%offset-address-as-integer ptr offset))))))

;;; === M E M - S E T ===

(defgeneric %mem-set (ptr type value &optional offset))

(defmacro generate-mem-set-accessor-functions ()
  `(progn
     ,@(loop for spec across *foreign-type-spec-table*
          when spec
	  collect
	    `(defmethod %mem-set (ptr (type (eql ',(%lisp-symbol spec))) value &optional (offset 0))
	       (funcall ',(intern (concatenate 'string "%MEM-SET-" (string (%lisp-symbol spec))) 'clasp-ffi)
                        (%offset-address-as-integer ptr offset) value)))))

;;; === F O R E I G N   F U N C T I O N  C A L L I N G ===

(declaim (inline foreign-funcall-transform-args))
(defun foreign-funcall-transform-args (args)
  "Return two values: lists of transformed args and result type."
  (let ((to-object-fn-name$ nil))
    (loop for (type arg) on args by #'cddr
       if arg
       collect (core::foreign-call (string-downcase (concatenate 'string "FROM_OBJECT_" #| (%lisp-name (%lisp-type->type-spec type)) |# "INT")) arg) into transformed-args
       else
       do (setf to-object-fn-name$ (string-downcase (concatenate 'string "TO_OBJECT_" #|(%lisp-name (%lisp-type->type-spec type))|# "INT")))
       finally (return (values transformed-args to-object-fn-name$)))))


(defmacro %foreign-funcall-pointer (ptr &rest args)
  "Funcall a pointer to a foreign function."
  `(multiple-value-bind (from-object-args to-object-fn-name$)
       (foreign-funcall-transform-args (list ,@args))
     (if to-object-fn-name$
	 `(core::foreign-call ,to-object-fn-name$
			      (core::foreign-call-pointer (ensure-core-pointer ,,ptr)
							  ,@from-object-args))
	 `(core::foreign-call-pointer (ensure-core-pointer ,,ptr)
				      ,@from-object-args)
	 )))

(defmacro %foreign-funcall (name &rest args)
  "Funcall a froeign function \"name\"."
  (let ((g-ptr (gensym)))
    `(let ((,g-ptr (%core-pointer-from-foreign-data (%dlsym ,name))))
       (if ,g-ptr
	   (%foreign-funcall-pointer ,g-ptr ,@args)
	   (error "Cannot call foreign function ~S - foreign function not found." ,name)))))

;;; === F O R E I G N   L I B R A R Y   H A N D L I N G ===

(declaim (inline %load-foreign-library))
(defun %load-foreign-library (name path)
  "Load a foreign library to be foudn at path. (name is ignored"
  (%dlopen path))

(declaim (inline %close-foreign-library))
(defun %close-foreign-library (ptr)
  "Close a foreign library."
  (%dlclose ptr))

;;; === F O R E I G N   G L O B A L S ===

(declaim (inline %foreign-symbol-name))
(defun %foreign-symbol-pointer (name)
  (%dlsym name))

;;;----------------------------------------------------------------------------
;;;
;;; F L I   I N I T I A L I Z A T I O N

(eval-when (:load-toplevel :execute :compile-toplevel)
  (generate-type-spec-accessor-functions)
  (generate-mem-ref-accessor-functions)
  (generate-mem-set-accessor-functions)
  (values))

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; EXPORTS

(export '(with-foreign-object
          with-foreign-objects
          %mem-ref
          %mem-set))
