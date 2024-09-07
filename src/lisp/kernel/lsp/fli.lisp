;;; ===========================================================================
;;;                  F L I    I M P L E M E N T A T I O N
;;; ===========================================================================
;;; -- IMPLEMEMTATION NOTES ---
;;;
;;; The complete FLI is comprised of the following files:
;;; .../src/core/fli.cc            - corresponding .cc file
;;; .../include/clasp/core/fli.h   - corresponding .h file
;;; .../src/lisp/kernel/fli.lisp    - this file
;;;
;;; --- END OF IMPLEMEMTATION NOTES ---

#+(or)
(eval-when (:execute)
	(format t "!~%!~%!~%!~%!~%In fli.lisp !~%")
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
(defun ensure-core-pointer (ptr fn info)
  (cond
    ((not ptr)
     (format t "#'ensure-core-pointer *** Illegal argument value: PTR may not be NIL - fn = ~a info = ~a!~%" fn info)
     (gctools:wait-for-user-signal "Bad ensure-core-pointer Send SIGUSR1 to continue")
     (error "#'ensure-core-ptr *** Illegal argument value: PTR may not be NIL!"))
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
       (format *error-output* "~d: ~S~&" idx spec)))

;;; %LISP-TYPE->TYPE-SPEC
(defgeneric %lisp-type->type-spec (lisp-type-kw))

(defmacro generate-type-spec-accessor-functions ()
  `(progn
     ;; type -> type spec
     ,@(loop for spec across *foreign-type-spec-table*
             for idx from 0 to (1- (length *foreign-type-spec-table*))
             when spec
               collect
             `(progn
;;;             (core:fmt t "Defining lisp-type->type-spec for {}%N" ,(%lisp-symbol spec))
                (defmethod %lisp-type->type-spec ((lisp-type-kw (eql ',(%lisp-symbol spec))))
                  (elt *foreign-type-spec-table* ,idx))))))

(defmethod %lisp-type->type-spec (lisp-type-kw)
  (error "Unknown FLI lisp type ~S - cannot determine type spec." lisp-type-kw))

;;; %FOREIGN-TYPE-SIZE
(defgeneric %foreign-type-size (lisp-type-kw))

(defmacro generate-foreign-type-size-functions ()
  `(progn
     ;; type -> type spec
     ,@(loop for spec across *foreign-type-spec-table*
             for idx from 0 to (1- (length *foreign-type-spec-table*))
             when spec
               collect
             `(defmethod %foreign-type-size ((lisp-type-kw (eql ',(%lisp-symbol spec))))
                (%size (elt *foreign-type-spec-table* ,idx))))))

(defmethod %foreign-type-size (lisp-type-kw)
  (error "Unknown FLI lisp type ~S - cannot determine foreign size." lisp-type-kw))

;;; %FOREIGN-TYPE-ALIGNMENT
(defgeneric %foreign-type-alignment (lisp-type-kw))

(defmacro generate-foreign-type-alignment-functions ()
  `(progn
     ;; type -> type spec
     ,@(loop for spec across *foreign-type-spec-table*
             for idx from 0 to (1- (length *foreign-type-spec-table*))
             when spec
               collect
             `(defmethod %foreign-type-alignment ((lisp-type-kw (eql ',(%lisp-symbol spec))))
                (%alignment (elt *foreign-type-spec-table* ,idx))))))

(defmethod %foreign-type-alignment (lisp-type-kw)
  (error "Unknown FLI lisp type ~S - cannot determine foreign alignment." lisp-type-kw))

;;; === T R A N S L A T O R    S U P P O R T ===

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

(defun init-translators ()
  (macrolet ((set1 (keyword llvm)
               `(%set-llvm-type-symbol-fn
                 (%lisp-type->type-spec ,keyword) (lambda () ,llvm)))
             (setall (&rest pairs)
               `(progn
                  ,@(loop for (key ll) on pairs by #'cddr
                          collecting `(set1 ,key ,ll)))))
    (setall :short cmp::%i16%
            :unsigned-short cmp::%i16%
            :ushort cmp::%i16%
            :int cmp::%i32%
            :unsigned-int cmp::%i32%
            :uint cmp::%i32%
            :long cmp::%i64%
            :unsigned-long cmp::%i64%
            :ulong cmp::%i64%
            :long-long cmp::%i64%
            :llong cmp::%i64%
            :unsigned-long-long cmp::%i64%
            :ullong cmp::%i64%
            :int8 cmp::%i8%
            :uint8 cmp::%i8%
            :int16 cmp::%i16%
            :uint16 cmp::%i16%
            :int32 cmp::%i32%
            :uint32 cmp::%i32%
            :int64 cmp::%i64%
            :uint64 cmp::%i64%
            #+int128 :int128 #+int128 cmp::%i128%
            #+int128 :uint128 #+int128 cmp::%i128%
            :size cmp::%size_t%
            :ssize cmp::%size_t%
            ;#+short-float :short-float #+short-float cmp::%short-float%
            :single-float cmp::%float%
            :float cmp::%float%
            :double cmp::%double%
            ;#+long-float :long-float #+long-float cmp::%long-float%
            :pointer cmp::%i64*%
            :void cmp::%void%
            :char cmp::%i8%
            :unsigned-char cmp::%i8%
            :uchar cmp::%i8%
            ;; TODO: CHECK & IMPLEMEMT !
            ;; :time cmp::+time_t+
            ;; :ptrdiff cmp::+ptrdiff_t+
            )))

(defun safe-translator-type (lisp-type-kw)
  (funcall (%lisp-type->llvm-type-symbol-fn lisp-type-kw)))

(defun safe-translator-to-object-name (lisp-type-kw)
  (%to-object-fn-name (%lisp-type->type-spec lisp-type-kw)))

(defun safe-translator-from-object-name (lisp-type-kw)
  (%from-object-fn-name (%lisp-type->type-spec lisp-type-kw)))

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
    #+clasp-ffi.debug (format *error-output* "*** clasp-ffi::%foreign-alloc - size = ~S, allocated ptr = ~S.~%" size result)
    result))

(declaim (inline %foreign-free))
(defun %foreign-free (ptr)
  #+clasp-ffi.debug (format *error-output* "*** clasp-ffi::%foreign-free - freeing ~S.~%" ptr)
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

;;; === S A T I A T I O N ===
(defmacro generate-satiation ()
  (let ((to-satiate
          (loop for spec across *foreign-type-spec-table*
                when spec
                  collect `'((eql ,(%lisp-symbol spec))))))
    `(eval-when (:load-toplevel) ; don't need to do it while loading as source
       (clos:satiate #'%lisp-type->type-spec ,@to-satiate)
       (clos:satiate #'%foreign-type-size ,@to-satiate)
       (clos:satiate #'%foreign-type-alignment ,@to-satiate)
       (clos:satiate #'%lisp-type->llvm-type-symbol-fn ,@to-satiate))))

;;; === F O R E I G N   F U N C T I O N  C A L L I N G ===

;;; This code has been invented on-the-fly by drmeister on 2016-10-13 ...
;;; I still marvel at how drmeister comes up with simple code... Thx!

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

(defmacro %foreign-funcall (name &rest arguments)
  (multiple-value-bind (signature args)
      (extract-signature arguments)
    `(core:foreign-call-pointer ,signature (ensure-core-pointer (core:dlsym :rtld-default ,name) "%foreign-funcall" ,name) ,@args)))

(defmacro %foreign-funcall-pointer (ptr &rest arguments)
  (multiple-value-bind (signature args)
      (extract-signature arguments)
    `(core:foreign-call-pointer ,signature (ensure-core-pointer ,ptr "%foreign-funcall-pointer" ,ptr) ,@args)))

;;; We'd like for the bytecode to be able to handle foreign calls, but it
;;; doesn't make sense for it to handle them directly. So, we provide a
;;; %%foreign-funcall function, and a definition of foreign-call-pointer
;;; as a macro using it. clasp-cleavir will compile foreign-call-pointer as
;;; a special form, but the bytecode will resort to calling the function, which
;;; will in turn compile something to use.

;;; TODO: Set up Cleavir to lower %%foreign-funcall calls into actual foreign
;;; calls ("inline" the foreign-caller). That should make BTB CFFI efficient.
;;; Also set it up so that in the usual case where the foreign function is named,
;;; the lookup is done before runtime.

;;; Cache table from foreign-call signatures to caller functions.
;;; A caller takes a function pointer and its arguments as arguments.
(defvar *foreign-callers* (make-hash-table :test 'equal))

(defun ensure-foreign-caller (signature)
  (or (gethash signature *foreign-callers*)
      (setf (gethash signature *foreign-callers*)
            (clasp-cleavir::make-foreign-caller signature))))

(defun %%foreign-funcall (signature function-pointer &rest arguments)
  (apply (ensure-foreign-caller signature)
         (ensure-core-pointer function-pointer "%%foreign-funcall" function-pointer)
         arguments))

(defmacro core:foreign-call-pointer (signature pointer &rest arguments)
  `(%%foreign-funcall ',signature ,pointer ,@arguments))

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

(defun close-foreign-libraries-on-save ()
  (format t "Trying to close foreign libraries~%")
  (let ((cffi-package (find-package "CFFI")))
    (when cffi-package
      (let ((list-foreign-libraries-symbol (find-symbol "LIST-FOREIGN-LIBRARIES" cffi-package))
            (close-foreign-library-symbol (find-symbol "CLOSE-FOREIGN-LIBRARY" cffi-package)))
        (when (and list-foreign-libraries-symbol
                   close-foreign-library-symbol)
          (format t "Closing foreign libraries~%")
          (loop for lib in (funcall list-foreign-libraries-symbol)
                do (format t "Closing foreign library: ~s~%" lib)
                do (funcall close-foreign-library-symbol lib))))))
  (format t "Finished closing foreign libraries~%"))

(eval-when (:load-toplevel :execute)
  (cmp:register-save-hook 'close-foreign-libraries-on-save))

;;; === F O R E I G N   G L O B A L S ===

(declaim (inline %foreign-symbol-pointer))
(defun %foreign-symbol-pointer (name module)
  "Return a pointer (of type ForeignData_sp / FOREIGN_DATA to a foreign symbol."
  (declare (ignore module))
  (%dlsym name))

;;;----------------------------------------------------------------------------
;;;
;;; F L I   I N I T I A L I Z A T I O N

(generate-type-spec-accessor-functions)
(generate-llvm-type-symbol-fn-accessor-functions)
(generate-mem-ref-accessor-functions)
(generate-mem-set-accessor-functions)
(generate-foreign-type-size-functions)
(generate-foreign-type-alignment-functions)
(generate-satiation)
(init-translators)

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; C A L L B A C K  S U P P O R T

;;; Mapping from (Lisp-function . signature) to callbacks (pointers)
;;; Besides its importance for caching, this is also important for ensuring
;;; that Lisp functions with C callbacks are not collected by GC.
(defvar *callbacks* (make-hash-table :test 'eq))

;;; Mapping from callback names to callbacks (pointers)
(defvar *callbacks-by-name* (make-hash-table :test 'equal))

(defun signature-return-type (signature) (first signature))
(defun signature-argument-types (signature) (rest signature))

(defun codegen-callback (signature var &key (c-name "callback"))
  (let* ((rett-kw (signature-return-type signature))
         (rett (clasp-ffi:safe-translator-type rett-kw))
         (args-kws (signature-argument-types signature))
         (argsts (mapcar #'clasp-ffi:safe-translator-type args-kws))
         (type (llvm-sys:function-type-get rett argsts))
         (llfun (cmp:irc-function-create type 'llvm-sys:external-linkage
                                         c-name cmp:*the-module*))
         (cmp:*current-function* llfun)
         (cmp:*current-function-name* c-name)
         ;; FIXME
         (c-argument-names (mapcar #'symbol-name args-kws)))
    ;; Generate code.
    (cmp:with-irbuilder ((llvm-sys:make-irbuilder (cmp:thread-local-llvm-context)))
      (let ((bb (cmp:irc-basic-block-create "entry")))
        (cmp:irc-set-insert-point-basic-block bb)
        (let* ((c-args (llvm-sys:get-argument-list llfun))
               ;; Generate code to translate the arguments.
               (cl-args (mapcar (lambda (c-arg c-arg-name c-type)
                                  (cmp:irc-intrinsic-call-or-invoke
                                   (clasp-ffi::to-translator-name c-type)
                                   (list c-arg)
                                   (format nil "translated-~a" c-arg-name)))
                                c-args c-argument-names args-kws))
               ;; Grab the function.
               (closure-to-call (cmp:irc-t*-load var))
               ;; And call.
               ;; results-in-registers keeps things in the basic tmv format,
               ;; which is all we need here, as the C function only uses
               ;; the primary value.
               ;; TODO: We can probably skip the XEP in most cases.
               (cl-result (cmp:irc-funcall-results-in-registers
                           closure-to-call cl-args
                           (core:fmt nil "{}_closure" c-name))))
          ;; Slap names on the arguments.
          (mapc (lambda (arg argname) (llvm-sys:set-name arg argname))
                c-args c-argument-names)
          ;; Translate the return value back to C if applicable.
          (if (llvm-sys:type-equal rett cmp:%void%)
              (cmp:irc-ret-void)
              (cmp:irc-ret (cmp:irc-intrinsic-call-or-invoke
                            (clasp-ffi::from-translator-name rett-kw)
                            (list (cmp:irc-tmv-primary cl-result))
                            "c-result"))))))))

(defun make-callback (signature function)
  (declare (ignorable function))
  (let* ((module (cmp::create-run-time-module-for-compile))
         (id (cmp::next-jit-compile-counter))
         (varname (format nil "callback-lisp-function-~d" id))
         (callback-name (format nil "callback-~d" id)))
    (cmp::with-module (:module module :optimize nil)
      (let ((var (llvm-sys:make-global-variable module cmp:%t*% nil
                                                'llvm-sys:external-linkage
                                                (llvm-sys:undef-value-get
                                                 cmp:%t*%)
                                                varname)))
        (codegen-callback signature var :c-name callback-name))
      ;; Some variables required by parseLinkGraph.
      ;; We don't actually use them - this is a stupid sham.
      ;; Initializers need to be put in to ensure the symbol actually
      ;; exist in the lib - without initializers these are just declarations.
      (llvm-sys:make-global-variable module cmp:%t*[0]% nil
                                     'llvm-sys:external-linkage
                                     (llvm-sys:constant-array-get
                                      cmp:%t*[0]% nil)
                                     (format nil "__clasp_literals_~a"
                                             callback-name))
      ;;(llvm-sys:dump-module module)
      ;;(cmp:irc-verify-module-safe module)
      (let ((dylib (llvm-sys:jit-module-to-dylib module callback-name)))
        (setf (llvm-sys:jit-lookup-t dylib varname) function)
        (llvm-sys:jit-lookup dylib callback-name)))))

(defun %ensure-callback (signature function)
  (let ((key (cons function signature)))
    (or (gethash key *callbacks*)
        (setf (gethash key *callbacks*)
              (make-callback signature function)))))

(defmacro ensure-callback (signature function)
  `(%ensure-callback ',signature ,function))

(defun %get-callback (name)
  (or (gethash name *callbacks-by-name*)
      (error "No callback named ~a" name)))

(defmacro %callback (name) `(%get-callback ',name))

(defmacro %defcallback (name-and-options return-type-kw lambda-list argument-type-kws &body body)
  (multiple-value-bind (function-name convention)
      (if (consp name-and-options)
          (destructuring-bind (name &key convention) name-and-options
            (values name convention))
          (values name-and-options :cdecl))
    (declare (ignore convention)) ; FIXME
    `(progn
       (setf (gethash ',function-name *callbacks-by-name*)
             (ensure-callback (,return-type-kw ,@argument-type-kws)
                              (lambda ,lambda-list ,@body)))
       ',function-name)))

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
            %get-callback
            safe-translator-type)))
