;;; ===========================================================================
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

#+(or)
(eval-when (:execute)
	(format t "!~%!~%!~%!~%!~%In fli.lsp !~%")
	(setq core:*echo-repl-read* t))

(in-package "CLASP-FFI")

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; Debugging MACROS

;;; (pushnew :clasp-ffi.debug cl:*features*)

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
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

;;; See vector *foreign-type-spec-table* => Vector with all foreign type
;;; specs, consisting each of:
;;; - Lisp symbol
;;; - Lisp name
;;; - size
;;; - alignment
;;; - C++ name
;;; Adding built-in foreign types requires adding a type spec to this table!

(defun dbg-print-*foreign-type-spec-table* ()
  (loop
     for spec across *foreign-type-spec-table*
     for idx from 0 to (1- (length *foreign-type-spec-table*))
     when spec
     do
       (format *debug-io* "~d: ~S~&" idx spec)))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defgeneric %lisp-type->lisp-name (lisp-type-kw))

  (defmacro generate-type-spec-accessor-functions ()
    `(progn
       ;; type -> type spec
       ,@(loop for spec across *foreign-type-spec-table*
            for idx from 0 to (1- (length *foreign-type-spec-table*))
            when spec
            collect
              `(progn
;;;                 (core:bformat t "Defining %lisp-type->type-spec for %s\n" ,(%lisp-symbol spec))
                 (defmethod %lisp-type->type-spec ((lisp-type-kw (eql ',(%lisp-symbol spec))))
                   (elt *foreign-type-spec-table* ,idx))))
       ))

  (defmethod %lisp-type->type-spec (lisp-type-kw)
    (error "Unknown FLI lisp type ~S - cannot determine type spec." lisp-type-kw))
  ) ;; eval-when

;;; %FOREIGN-TYPE-SIZE
(eval-when (:compile-toplevel :load-toplevel :execute)

  (defgeneric %foreign-type-size (lisp-type-kw))

  (defmacro generate-foreign-type-size-functions ()
    `(progn
       ;; type -> type spec
       ,@(loop for spec across *foreign-type-spec-table*
            for idx from 0 to (1- (length *foreign-type-spec-table*))
            when spec
            collect
              `(defmethod %foreign-type-size ((lisp-type-kw (eql ',(%lisp-symbol spec))))
                 (%size (elt *foreign-type-spec-table* ,idx))))
       ))

  (defmethod %foreign-type-size (lisp-type-kw)
    (error "Unknown FLI lisp type ~S - cannot determine foreign size." lisp-type-kw))
  ) ;; eval-when

;;; %FOREIGN-TYPE-ALIGNMENT
(eval-when (:compile-toplevel :load-toplevel :execute)

  (defgeneric %foreign-type-alignment (lisp-type-kw))

  (defmacro generate-foreign-type-alignment-functions ()
    `(progn
       ;; type -> type spec
       ,@(loop for spec across *foreign-type-spec-table*
            for idx from 0 to (1- (length *foreign-type-spec-table*))
            when spec
            collect
              `(defmethod %foreign-type-alignment ((lisp-type-kw (eql ',(%lisp-symbol spec))))
                 (%alignment (elt *foreign-type-spec-table* ,idx))))
       ))

  (defmethod %foreign-type-alignment (lisp-type-kw)
    (error "Unknown FLI lisp type ~S - cannot determine foreign alignment." lisp-type-kw))
  ) ;; eval-when

;;; === T R A N S L A T O R    S U P O R T ===

(defgeneric %lisp-type->llvm-type-symbol-fn (lisp-type-kw))

(defmacro generate-llvm-type-symbol-fn-accessor-functions ()
  `(progn
     ;; type -> llvm type symbol
     ,@(loop for spec across *foreign-type-spec-table*
          for idx from 0 to (1- (length *foreign-type-spec-table*))
          when spec
	  collect
	    `(defmethod %lisp-type->llvm-type-symbol-fn ((lisp-type-kw (eql ',(%lisp-symbol spec))))
               (%llvm-type-symbol-fn (elt *foreign-type-spec-table* ,idx))))
     ))

(defmethod %lisp-type->llvm-type-symbol-fn (lisp-type-kw)
  (error "Unknown FLI lisp type ~S - cannot determine llvm type symbol function." lisp-type-kw))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun init-translators ()

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :short) (lambda () cmp::%i16%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :unsigned-short) (lambda () cmp::%i16%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :ushort) (lambda () cmp::%i16%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :int) (lambda () cmp::%i32%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :unsigned-int) (lambda () cmp::%i32%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :uint) (lambda () cmp::%i32%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :long) (lambda () cmp::%i64%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :unsigned-long) (lambda () cmp::%i64%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :ulong) (lambda () cmp::%i64%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :long-long) (lambda () cmp::%i64%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :llong) (lambda () cmp::%i64%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :unsigned-long-long) (lambda () cmp::%i64%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :ullong) (lambda () cmp::%i64%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :int8) (lambda () cmp::%i8%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :uint8) (lambda () cmp::%i8%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :int16) (lambda () cmp::%i16%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :uint16) (lambda () cmp::%i16%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :int32) (lambda () cmp::%i32%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :uint32) (lambda () cmp::%i32%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :int64) (lambda () cmp::%i64%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :uint64) (lambda () cmp::%i64%))

    #+int128 (%set-llvm-type-symbol-fn
              (%lisp-type->type-spec :int128) (lambda () cmp::%i128%))

    #+int128 (%set-llvm-type-symbol-fn
              (%lisp-type->type-spec :uint128) (lambda () cmp::%i128%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :size) (lambda () cmp::%size_t%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :ssize) (lambda () cmp::%size_t%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :single-float) (lambda () cmp::%float%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :float) (lambda () cmp::%float%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :double) (lambda () cmp::%double%))

    #+long-float (%set-llvm-type-symbol-fn
                  (%lisp-type->type-spec :long-float) (lambda () cmp::%long-float%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :pointer) (lambda () cmp::%i64*%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :void) (lambda () cmp::%void%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :char) (lambda () cmp::%i8%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :unsigned-char) (lambda () cmp::%i8%))

    (%set-llvm-type-symbol-fn
     (%lisp-type->type-spec :uchar) (lambda () cmp::%i8%))

    ;; TODO: CHECK & IMPLEMEMT !
    ;; (%set-llvm-type-symbol-fn (%lisp-type->type-spec :time) (lambda () cmp::+time_t+))
    ;; (%set-llvm-type-symbol-fn (%lisp-type->type-spec :ptrdiff) (lambda () cmp::+ptrdiff_t+))

    )

  (defun safe-translator-type (lisp-type-kw)
    (funcall (%lisp-type->llvm-type-symbol-fn lisp-type-kw)))

  (defun safe-translator-to-object-name (lisp-type-kw)
    (%to-object-fn-name (%lisp-type->type-spec lisp-type-kw)))

  (defun safe-translator-from-object-name (lisp-type-kw)
    (%from-object-fn-name (%lisp-type->type-spec lisp-type-kw)))

  ) ;; eval-when

;;; === B U I LT - I N   O P E R A T I O N S ===

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
  (let ((result (%allocate-foreign-data size)))
    #+clasp-ffi.debug (format *debug-io* "*** clasp-ffi::%foreign-alloc - size = ~S, allocated ptr = ~S.~%" size result)
    result))

(declaim (inline %foreign-free))
(defun %foreign-free (ptr)
  #+clasp-ffi.debug (format *debug-io* "*** clasp-ffi::%foreign-free - freeing ~S.~%" ptr)
  (%free-foreign-data ptr)
  nil)

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

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun translator-name (prefix direction type)
    (format nil "~a~a_object_~a"
            prefix direction
            (string-downcase (string (%lisp-name (%lisp-type->type-spec type))))))

  (defun to-translator-name (type)
    (translator-name "" "to" type))

  (defun from-translator-name (type)
    (translator-name "" "from" type))

  (defun split-list (list)
    (do ((list list (rest (rest list)))
         (left '() (list* (first list) left))
         (right '() (if (endp (rest list)) right (list* (second list) right))))
        ((endp list) (list (nreverse left) (nreverse right)))))

  (defun extract-signature (arguments)
    "Converts (:float 16.0 :int 3 :float) -> (values '(:float (:float :int)) (16.0 3))"
    (let* ((types-args (clasp-ffi::split-list arguments))
           (types (car types-args))
           (args (cadr types-args))
           (arg-types (butlast types))
           (last (car (last types))))
      (values (list last arg-types) args)))

  (defun split-arguments (arguments)
    (let* ((splits (split-list arguments))
           (types-and-maybe-return-type (car splits))
           (args (cadr splits))
           (explicit-return-type (> (length types-and-maybe-return-type) (length args)))
           (return-type (if explicit-return-type
                            (car (last types-and-maybe-return-type))
                            :void))
           (types (if explicit-return-type
                      (butlast types-and-maybe-return-type 1)
                      types-and-maybe-return-type)))
      (values (loop for type in types
                 for arg in args
                 collect `(core:foreign-call
                           ,(from-translator-name type)
                           ,arg))
              return-type)))

  (defun process-arguments (arguments)
    (cond

      ((null arguments)
       (values nil :void))

      ((and (listp arguments)
            (not (listp (car arguments)))
            (> (length arguments) 1))
       (split-arguments arguments))

      ((and (listp arguments)
            (not (listp (car arguments)))
            (= (length arguments) 1))
       (values nil (car arguments)))

      ((and (listp arguments)
            (listp (car arguments))
            (= (length arguments) 1))
       (split-arguments (car arguments)))

      (t (error "%foreign-funcall/process-arguments: Malformed arguments for foreign funcall: ~S" arguments))))

  ) ;; EVAL-WHEN

(defmacro %foreign-funcall (name &rest arguments)
  (multiple-value-bind (signature args)
      (extract-signature arguments)
    `(core:foreign-call-pointer ,signature (ensure-core-pointer (core:dlsym :rtld-default ,name)) ,@args)))

(defmacro %foreign-funcall-pointer (ptr &rest arguments)
  (multiple-value-bind (signature args)
      (extract-signature arguments)
    `(core:foreign-call-pointer ,signature (ensure-core-pointer ,ptr) ,@args)))

;;; === F O R E I G N   L I B R A R Y   H A N D L I N G ===

(declaim (inline %load-foreign-library))
(defun %load-foreign-library (name path)
  "Load a foreign library to be found at path. (name is ignored)"
  (declare (ignore name))
  (multiple-value-bind (handle error)
      (%dlopen path)
    (if (not handle)
        (error "~A" error)
        handle)))

(declaim (inline %close-foreign-library))
(defun %close-foreign-library (ptr)
  "Close a foreign library."
  (%dlclose ptr))

;;; === F O R E I G N   G L O B A L S ===

(declaim (inline %foreign-symbol-pointer))
(defun %foreign-symbol-pointer (name module)
  "Return a pointer (of type ForeignData_sp / FOREIGN_DATA to a foreign symbol."
  (declare (ignore module))
  (%dlsym name))

;;;----------------------------------------------------------------------------
;;;
;;; F L I   I N I T I A L I Z A T I O N

(eval-when (:load-toplevel :execute :compile-toplevel)
;;  (print (macroexpand '(generate-type-spec-accessor-functions)))
  (generate-type-spec-accessor-functions)
  (init-translators)
  (generate-llvm-type-symbol-fn-accessor-functions)
  (generate-mem-ref-accessor-functions)
  (generate-mem-set-accessor-functions)
  (generate-foreign-type-size-functions)
  (generate-foreign-type-alignment-functions)
  (values))

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; C A L L B A C K  S U P P O R T

;;; Agsin, drmeister did most of the work. This was adapted to run in the
;;; CLASP-FFI package.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mangled-callback-name (name)
    (format nil "clasp_ffi_cb_~a" name)))

(defun %expand-callback-definition (name-and-options return-type-kw argument-symbols argument-type-kws body)

  (multiple-value-bind (function-name convention)
      (if (consp name-and-options)
          (destructuring-bind (name &key convention)
              name-and-options
            (values name convention))
          (values name-and-options :cdecl))
    ;; Convert type keywords into llvm types ie: :int -> %i32%
    (let* ((body-form `(lambda ,argument-symbols ,@body))
           (argument-names (mapcar (lambda (s)
                                     (string s))
                                   argument-symbols))
           (mangled-function-name (mangled-callback-name function-name))
           (return-type (safe-translator-type return-type-kw ))
           (argument-types (mapcar (lambda (type-kw)
                                     (safe-translator-type type-kw))
                                   argument-type-kws))
           ;; Get the type of the function
           (function-type (llvm-sys:function-type-get return-type argument-types))
           ;; Create a new llvm function in the current llvm Module cmp:*the-module*
           (new-func (llvm-sys:function-create function-type
                                               'llvm-sys:external-linkage
                                               mangled-function-name
                                               cmp::*the-module*))
           (cmp::*current-function* new-func)
           (cmp::*current-function-name* mangled-function-name)
           ;; Create an IRBuilder - a helper for adding instructions to func
           (irbuilder-cur (llvm-sys:make-irbuilder cmp::*llvm-context*)))
      ;;(format t "Created function ~a in ~a~%" (llvm-sys:get-name new-func) cmp:*the-module*)
      ;; Create the entry basic block in the current function
      (let ((bb (cmp::irc-basic-block-create "entry" cmp::*current-function*)))
        (cmp::irc-set-insert-point-basic-block bb irbuilder-cur)
        ;; Start generating instructions
        (cmp::with-irbuilder (irbuilder-cur)
          ;; (1) Call the translators for every argument returning a value in a llvm register
          ;;       Get the c-args from the function argument list
          (let* ((c-args (mapcar #'(lambda (arg argname)
                                     (llvm-sys:set-name arg argname)
                                     arg)
                                 (llvm-sys:get-argument-list new-func) argument-names))
                 ;; Call a translator for each c-arg and accumulate a list of cl-args in registers
                 (cl-args (mapcar (lambda (c-arg arg-type-kw arg-name)
                                    (let* ((to-object-name (to-translator-name arg-type-kw))
                                           (trans-arg-name (format nil "translated-~a" arg-name))
                                           ;; Create the function declaration on the fly
                                           (to-object-func (cmp::get-or-declare-function-or-error cmp::*the-module* to-object-name)))
                                      (cmp::irc-call-or-invoke
                                       to-object-func
                                       (list c-arg)
                                       cmp::*current-unwind-landing-pad-dest*
                                       trans-arg-name)))
                                  c-args argument-type-kws argument-names)))
            ;; (2) Call the closure with the arguments in registers
            (let* ((real-args (if (< (length cl-args) core:+number-of-fixed-arguments+)
                                  (append cl-args (make-list (- core:+number-of-fixed-arguments+ (length cl-args)) :initial-element (cmp::null-t-ptr)))
                                  cl-args))
                   (function-object (if core:*use-cleavir-compiler*
                                        (funcall (find-symbol "COMPILE-LAMBDA-FORM-TO-LLVM-FUNCTION" :clasp-cleavir) body-form)
                                        (cmp:compile-lambda-function body-form)))
                   (invoke-fn (cmp::get-or-declare-function-or-error cmp::*the-module* "cc_call_callback"))
                   (fptr (cmp:irc-bit-cast function-object cmp:%t*% "fptr-t*"))
                   (cl-result (cmp::irc-call-or-invoke
                               invoke-fn
                               (list* fptr #| (cmp::null-t-ptr) not used in new call-conv |#
                                      (cmp:jit-constant-size_t (length cl-args)) real-args)
                               cmp::*current-unwind-landing-pad-dest*
                               "cl-result")))
              ;; (3) Call the translator for the return value
              (if (eq return-type-kw :void)
                  ;; Return with void
                  (llvm-sys:create-ret-void cmp::*irbuilder*)
                  ;; Return the result
                  (let* ((from-object-name (from-translator-name return-type-kw))
                         (from-object-func (cmp::get-or-declare-function-or-error cmp::*the-module* from-object-name))
                         (c-result (cmp::irc-call-or-invoke 
                                    from-object-func
                                    (list (llvm-sys:create-extract-value cmp::*irbuilder* cl-result (list 0) "val0"))
                                    cmp::*current-unwind-landing-pad-dest*
                                    "cl-result")))
                    (llvm-sys:create-ret cmp::*irbuilder* c-result))))))))
    `',function-name))

(defmacro %defcallback (name-and-options return-type-kw argument-symbols argument-type-kws &rest body)
  (%expand-callback-definition name-and-options return-type-kw argument-symbols argument-type-kws body))

(defmacro %callback (sym)
  `(%get-callback ',sym))

(defun %get-callback (sym-name)
  (if sym-name
      (%dlsym (mangled-callback-name sym-name))
      nil))

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
            %foreign-funcall-pointer
            %load-foreign-library
            %close-foreign-library
            %foreign-symbol-pointer
            %foreign-type-size
            %foreign-type-alignment
            %defcallback
            %callback
            %get-callback)))
