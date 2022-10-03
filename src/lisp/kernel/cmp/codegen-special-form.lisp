(in-package #:cmp)

;;;; bclasp compilation of special forms.

(export 'llvm-inline)

;;; PROGV

(defmacro progv (symbols values &body forms)
  `(core:progv-function ,symbols ,values
                        (lambda ()
                          (declare (core:lambda-name core::progv-lambda))
                          (progn ,@forms))))

;;; CORE::VECTOR-LENGTH

(defun gen-vector-length-untagged (vector)
  (let* ((type (llvm-sys:type-get-pointer-to (simple-vector-llvm-type 't)))
         (cast (irc-bit-cast vector type)) ; treat the vector as a vector
         ;; find the location of the length
         (length-address
           (irc-typed-in-bounds-gep (simple-vector-llvm-type 't)
                                    cast
                                    (list 0 +simple-vector-length-slot+)
                                    "vector-length-address")))
    (irc-typed-load %i64% length-address "vector-length")))

(defun gen-vector-length (vector)
  (irc-tag-fixnum (gen-vector-length-untagged vector) "vector-length"))

;;; CORE::%ARRAY-DIMENSION

(defun gen-%array-dimension (array axisn)
  (let* ((untagged-axisn (irc-untag-fixnum axisn %i64% "untagged-axisn"))
         (untagged-dim (irc-array-dimension array untagged-axisn)))
    (irc-tag-fixnum untagged-dim "array-dimension")))

;;; FENCE

(defun order-spec->order (order-spec)
  (case order-spec
    ((:unordered) 'llvm-sys:unordered)
    ((:relaxed) 'llvm-sys:monotonic)
    ((:acquire) 'llvm-sys:acquire)
    ((:release) 'llvm-sys:release)
    ((:acquire-release) 'llvm-sys:acquire-release)
    ((:sequentially-consistent) 'llvm-sys:sequentially-consistent)
    (t (error "BUG: Unknown atomic order specifier ~a" order-spec))))

(defun gen-fence (order-spec)
  (when (or (eq order-spec :unordered) (eq order-spec :relaxed))
    (error "Can't generate a fence with ~a ordering" order-spec))
  (irc-fence (order-spec->order order-spec)))

;;; CLEAVIR-PRIMOP:CAR, CLEAVIR-PRIMOP:CDR

(defun gen-memref-address (tpointer offset)
  (irc-bit-cast
   ;; memref/set use byte addressing, so treat these as i8 arrays
   (irc-typed-gep-variable %i8%
                     (irc-bit-cast tpointer %i8*%)
                     ;; llvm doesn't actually have signed types,
                     ;; so the u is a misnomer - don't sweat it.
                     (list (cmp:make-uintptr_t offset)))
   %t**% "memref-set-addr"))

;;; CORE:VASLIST-LENGTH
;;; Get the count of remaining args in a vaslist.

(defun gen-vaslist-length (vaslist)
  (irc-tag-fixnum
   (irc-typed-load %size_t% (c++-field-ptr info.%vaslist% vaslist :nargs "vaslist-nargs"))))

;;; CORE:INSTANCE-REF

(defun gen-instance-ref (instance index)
  (irc-read-slot instance (irc-untag-fixnum index %size_t% "slot-location")))

;;; CORE:INSTANCE-SET

(defun gen-instance-set (instance index value)
  (irc-write-slot instance (irc-untag-fixnum index %size_t% "slot-location") value))

;;; CORE:INSTANCE-CAS

(defun gen-instance-cas (instance index old new)
  (irc-cmpxchg
   (irc-instance-slot-address
    instance (irc-untag-fixnum index %size_t% "slot-location"))
   old new))

;;; CORE:INSTANCE-RACK

(defun gen-instance-rack (instance) (irc-rack instance))

;;; CORE:INSTANCE-RACK-SET

(defun gen-instance-rack-set (instance rack)
  (irc-rack-set instance rack)
  rack)

;;; CORE:RACK-REF

(defun gen-rack-ref (rack index &key (order 'llvm-sys:monotonic))
  (irc-rack-read rack (irc-untag-fixnum index %size_t% "slot-location")
                 :order order))

;;; CORE:RACK-SET

(defun gen-rack-set (rack index value &key (order 'llvm-sys:monotonic))
  (irc-rack-write rack (irc-untag-fixnum index %size_t% "slot-location") value
                  :order order)
  value)

;;; DBG-i32

(defparameter *nexti* 10000)

#+(or)
(defmacro blog (fmt &rest fargs)
  `(core:fmt *error-output* ,fmt ,@fargs))
(defmacro blog (fmt &rest fargs)
  (declare (ignore fmt fargs))
  nil)

;;; FOREIGN-CALL, FOREIGN-CALL-POINTER

(defun function-type-create-on-the-fly (foreign-types)
  (let ((arg-types (mapcar (lambda (type)
                             (clasp-ffi::safe-translator-type type))
                           (second foreign-types)))
        (varargs nil))
    (llvm-sys:function-type-get (clasp-ffi::safe-translator-type (first foreign-types)) arg-types varargs)))

;;; DEFCALLBACK

;;; shared with cleavir.
;;; What we're doing here is defining a C function
;;; that calls the translators on its arguments, passes those translated arguments
;;; to a Lisp closure, then translates the primary return value of that function
;;; back to C and returns it (or if the C function is return type void, doesn't).
(defun gen-defcallback (c-name convention
                        return-type-name return-translator-name
                        argument-type-names argument-translator-names
                        parameters place-holder closure-value)
  (declare (ignore convention place-holder))         ; FIXME
  ;; parameters should be a list of symbols, i.e. lambda list with only required.
  (unless (= (length argument-type-names) (length parameters) (length argument-translator-names))
    (error "BUG: Callback function parameters and types have a length mismatch"))
;;; Generate a variable and put the closure in it.
  (let* ((closure-literal-slot-index (literal:new-table-index))
         (closure-var-name (core:fmt nil "{}_closure_var" c-name)))
    #+(or)(progn
            (format t "gen-defcallback - the (literal::literal-machine-table-index literal::*literal-machine*) -> ~d~%" (literal::literal-machine-table-index literal::*literal-machine*))
            (format t "gen-defcallback cmp:*load-time-value-holder-global-var* -> ~a~%" cmp:*load-time-value-holder-global-var*)
            (format t "gen-defcallback closure-value -> ~a~%" closure-value)
            (format t "gen-defcallback closure-literal-slot-index -> ~a~%" closure-literal-slot-index))
    (irc-t*-result closure-value (literal:constants-table-reference closure-literal-slot-index))
    ;; Now generate the C function.
    ;; We don't actually "do" anything with it- just leave it there to be linked/used like a C function.
    (with-landing-pad nil ; Since we're in a new function (which should never be an unwind dest)
      (let* ((c-argument-names (mapcar #'string parameters))
             (return-type (clasp-ffi:safe-translator-type return-type-name))
             (argument-types (mapcar #'clasp-ffi:safe-translator-type argument-type-names))
             (c-function-type (llvm-sys:function-type-get return-type argument-types))
             (new-func (llvm-sys:function-create c-function-type
                                                 'llvm-sys:external-linkage
                                                 c-name
                                                 *the-module*))
             (*current-function* new-func)
             (*current-function-name* c-name))
        (unless (llvm-sys:llvmcontext-equal (llvm-sys:get-context *the-module*)
                                            (llvm-sys:get-context new-func))
          (error "The llvm-context for the~%module ~s~%the thread LLVMContext is ~s~% doesn't match the one for the new-func ~s~%the c-function-type context is ~s~% The function return-type context is: ~s~% The argument types are ~s~%"
                 (llvm-sys:get-context *the-module*)
                 (cmp:thread-local-llvm-context)
                 (llvm-sys:get-context new-func)
                 (llvm-sys:get-context c-function-type)
                 (llvm-sys:get-context return-type)
                 (mapcar #'llvm-sys:get-context argument-types)))
        (with-irbuilder ((llvm-sys:make-irbuilder (thread-local-llvm-context)))
          (let ((bb (irc-basic-block-create "entry" new-func)))
            (irc-set-insert-point-basic-block bb)
            (let* ((c-args (mapcar (lambda (arg argname)
                                     (llvm-sys:set-name arg argname)
                                     arg)
                                   (llvm-sys:get-argument-list new-func)
                                   c-argument-names))
                   ;; Generate code to translate the arguments.
                   (cl-args (mapcar (lambda (c-arg c-arg-name translator)
                                      (irc-intrinsic-call
                                       translator
                                       (list c-arg)
                                       (format nil "translated-~a" c-arg-name)))
                                    c-args c-argument-names argument-translator-names))
                   ;; Generate code to get the closure from the global variable from earlier.
                   (closure-to-call (irc-t*-load (literal:constants-table-reference closure-literal-slot-index) closure-var-name))
                   ;; Generate the code to actually call the lisp function.
                   ;; results-in-registers keeps things in the basic tmv format, because
                   ;; here we don't need the store/load values dance.
                   ;; (The C function only gets/needs/wants the primary value.)
                   (cl-result (irc-funcall-results-in-registers
                               closure-to-call cl-args (core:fmt nil "{}_closure" c-name))))
              ;; Now generate a call the translator for the return value if applicable, then return.
              ;; NOTE: (eq return-type %void%) doesn't seem to work - and it's sketchy because it's a symbol macro
              (if (string= return-translator-name "from_object_void")
                  (irc-ret-void)
                  (let ((c-result (irc-intrinsic-call
                                   return-translator-name
                                   ;; get the 0th value.
                                   (list (irc-tmv-primary cl-result))
                                   "c-result")))
                    (irc-ret c-result)))
              )))))))
