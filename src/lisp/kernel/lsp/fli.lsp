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
;;; ISSUES / KNOWN PROBLEMS
;;;
;;;  None so far :-D
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
     ;; type -> type spec
     ,@(loop for spec across *foreign-type-spec-table*
          for idx from 0 to (1- (length *foreign-type-spec-table*))
          when spec
	  collect
	    `(defmethod %lisp-type->type-spec ((type (eql ',(%lisp-symbol spec))))
               (elt *foreign-type-spec-table* ,idx)))
     ))

(defmethod %lisp-type->type-spec (type)
  (error "Unknown FLI lisp type ~S - cannot determine type spec." type))

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

(defmethod %mem-ref (ptr type &optional offset)
  (declare (ignore ptr offset))
  (error "Unknown lisp type ~S for %mem-ref." type))

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

(defmethod %mem-set (ptr type value &optional offset)
  (declare (ignore ptr offset value))
  (error "Unknown lisp type ~S for %mem-set." type))

;;; === F O R E I G N   F U N C T I O N  C A L L I N G ===

;;; This code has been invented on-the-fly by drmeister on 2016-10-13 ...
;;; I still marvel at how drmeister comes up with simple code... Thx!

(defun split-list (list)
  (do ((list list (rest (rest list)))
       (left '() (list* (first list) left))
       (right '() (if (endp (rest list)) right (list* (second list) right))))
      ((endp list) (list (nreverse left) (nreverse right)))))

(defun translator-name (prefix type)
  (format nil "~a_object_~a"
          prefix
          (string-downcase (string (%lisp-name (%lisp-type->type-spec type))))))

(defun process-arguments (arguments)
  (let* ((splits (split-list (car arguments)))
         (types-and-maybe-return-type (car splits))
         (args (cadr splits))
         (explicit-return-type (> (length types-and-maybe-return-type) (length args)))
         (return-type (if explicit-return-type
                          (car (last types-and-maybe-return-type))
                          :void))
         (types (if explicit-return-type
                    (butlast types-and-maybe-return-type 1)
                    types-and-maybe-return-type)))

    #|
    (format *debug-io* "arguments = ~S~&" arguments)
    (format *debug-io* "splits = ~S~&" splits)
    (format *debug-io* "types-and-maybe-return-type = ~S~&" types-and-maybe-return-type)
    (format *debug-io* "args = ~S~&" args)
    (format *debug-io* "explicit-return-type = ~S~&" explicit-return-type)
    (format *debug-io* "return-type = ~S~&" return-type)
    (format *debug-io* "types = ~S~&" types)
    |#

    (values (loop for type in types
               for arg in args
               collect `(core:foreign-call
                         ,(translator-name "from" type)
                         ,arg))
            return-type)))

(defmacro %foreign-funcall (name &rest arguments)
  (multiple-value-bind (args return-type)
      (process-arguments arguments)
    (if (eq return-type :void)
        `(core:foreign-call-pointer (ensure-core-pointer (%dlsym ,name)) ,@args)
        `(core:foreign-call ,(translator-name "to" return-type)
                            (core:foreign-call-pointer (ensure-core-pointer (%dlsym ,name)) ,@args)))))

(defmacro %foreign-funcall-pointer (ptr &rest arguments)
  (multiple-value-bind (args return-type)
      (process-arguments arguments)
    (if (eq return-type :void)
        `(core:foreign-call-pointer (ensure-core-pointer, ptr) ,@args)
        `(core:foreign-call ,(translator-name "to" return-type)
                            (core:foreign-call-pointer (ensure-core-pointer ,ptr) ,@args)))))

;;; === F O R E I G N   L I B R A R Y   H A N D L I N G ===

(declaim (inline %load-foreign-library))
(defun %load-foreign-library (name path)
  "Load a foreign library to be foudn at path. (name is ignored"
  (declare (ignore name))
  (%dlopen path))

(declaim (inline %close-foreign-library))
(defun %close-foreign-library (ptr)
  "Close a foreign library."
  (%dlclose ptr))

;;; === F O R E I G N   G L O B A L S ===

(declaim (inline %foreign-symbol-pointer))
(defun %foreign-symbol-pointer (name)
  "Return a pointer (of type ForeignData_sp / FOREIGN_DATA to a foreign symbol."
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
;;; E X P O R T S

(eval-when (:load-toplevel :execute :compile-toplevel)
  (export '(with-foreign-object
            with-foreign-objects
            %foreign-alloc
            %foreign-free
            %mem-ref
            %mem-set
            %foreign-funcall
            %static-foreign-funcall
            %foreign-funcall-pointer
            %load-foreign-library
            %close-foreign-library
            %foreign-symbol-pointer)))

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; T E S T I N G

(defun test-ffi ()

  (format *debug-io* "*** TEST 1: %MEM-SET and %MEM-REF ...~&")
  (format *debug-io* "    Expected results: a = 47111, b = 47112.~&")

  (let* ((*v1* (%allocate-foreign-object :uint32))
         (a nil)
         (b nil)
         (fa (%foreign-data-address *v1*)))

    ;; --- WITH LOW-LEVEL CALLS ---
    (%mem-set-uint32 fa 47111)
    (setq a (%mem-ref-uint32 fa))

    ;; --- WITH HIGH-LEVEL CALLS ---
    (%mem-set fa :uint32 47112)
    (setq b (%mem-ref fa :uint32))

    (format *debug-io* "*** a = ~S, b = ~S.~&" a b))

  (format *debug-io* "*** TEST 2: %FOREIGN-FUNCALL ...~&")
  (format *debug-io* " => (%foreign-funcall \"fli_test_add\" (:int 2 :short 45 :int)) -> ~S~&" (%foreign-funcall "fli_test_add" (:int 2 :short 45 :int)))

  (format *debug-io* "*** TEST 3: %FOREIGN-FUNCALL-POINTER ...~&")
  (let ((fn-addr (%dlsym "fli_test_mul_uint32")))
    (format *debug-io* " => for function fli_test_mul2: (%foreign-funcall-pointer ~S (:uint32 45 :uint32)) -> ~S~&" fn-addr (%foreign-funcall-pointer fn-addr (:uint32 45 :uint32))))

  (format *debug-io* "*** TEST 4: %FOREIGN-FUNCALL-POINTER ...~&")
  (let ((fn-addr (%dlsym "fli_test_mul2_long")))
    (format *debug-io* " => for function fli_test_mul2_long: (%foreign-funcall-pointer ~S (:long 45 :long)) -> ~S~&" fn-addr (%foreign-funcall-pointer fn-addr (:long 45 :long))))

  )

(defun dbg-numeric-limits ()
  (%foreign-funcall "info_numeric_limits" ))
