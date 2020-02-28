#+(or)(eval-when (:compile-toplevel :execute :load-toplevel)
  (setq *echo-repl-read* t)
  (setq cmp::*debug-compile-file* t)
  (format t "About to compile-file ir.lsp~%"))

(in-package :clasp-cleavir)
  
(defun %literal-ref (value &optional read-only-p)
  (multiple-value-bind (index in-array)
      (literal:reference-literal value read-only-p)
    (unless in-array
      (error "%literal-ref of immediate value ~s is illegal" value))
    (let* ((literal-label (bformat nil "values-table[%d]" index))
           (gep (llvm-sys:create-const-gep2-64 cmp:*irbuilder*
                                               (literal:ltv-global)
                                               0 index
                                               literal-label)))
      gep)))

(defun %literal-value (value &optional (label "literal"))
  (cmp:irc-load (%literal-ref value)))

#+(or)
(defun %closurette-index (lambda-name function source-info-handle
                          filepos lineno column)
  (literal::reference-closure lambda-name function source-info-handle
                              filepos lineno column))

#+(or)
(defun %closurette-ref (lambda-name function source-info-handle
                        filepos lineno column)
  (let* ((index (%closurette-index lambda-name function source-info-handle
                                   filepos lineno column))
         (gep (llvm-sys:create-const-gep2-64 cmp:*irbuilder*
                                             (literal:ltv-global)
                                             0 index
                                             (bformat nil "values-table[%d]" index))))
    gep))

#+(or)
(defun %closurette-value (lambda-name function source-info-handle
                          filepos lineno column)
  (cmp:irc-load (%closurette-ref lambda-name function source-info-handle
                                 filepos lineno column)))

(defun %i1 (num)
  (cmp:jit-constant-i1 num))

(defun %i8 (num)
  (cmp:jit-constant-i8 num))

(defun %i32 (num)
  (cmp:jit-constant-i32 num))

(defun %i64 (num)
  (cmp:jit-constant-i64 num))

(defun %size_t (num)
  (cmp:jit-constant-size_t num))

(defun %uintptr_t (num)
  (cmp:make-uintptr_t num))

(defgeneric %default-int-type (abi))
(defmethod %default-int-type ((abi abi-x86-64)) cmp:%i64%)
(defmethod %default-int-type ((abi abi-x86-32)) cmp:%i32%)

(defun %nil ()
  "A nil in a T*"
  (%literal-value nil))


(defun instruction-llvm-function (instr)
  (llvm-sys:get-parent (llvm-sys:get-parent instr)))

(defun %intrinsic-call (function-name args &optional (label ""))
  (let* ((info (gethash function-name (cmp::get-primitives)))
         (does-not-throw (getf (cmp::primitive-properties info) :does-not-throw)))
    (when (and (null does-not-throw)                     ; it throws
               (not (string= function-name "cc_unwind")) ; it's not cc_unwind
               (not (string= function-name "cc_throw"))) ; it's not cc_throw
      ;; If we are using llvm CALL to call the intrinsic but it can
      ;; + throw an exception then print a warning - it should be
      ;; + called with %intrinsic-invoke-if-landing-pad-or-call
      ;; + ... unless its cc_unwind or cc_throw
      (warn "%intrinsic-call is being used for ~a when this intrinsic has been declared with the unwind property - meaning that it can throw an exception and %intrinsic-invoke-if-landing-pad-or-call should be used" function-name)))
  (cmp:irc-intrinsic-call function-name args label))

(defun %intrinsic-invoke-if-landing-pad-or-call (function-name args &optional (label "") (maybe-landing-pad cmp::*current-unwind-landing-pad-dest*))
  (cmp::irc-intrinsic-invoke-if-landing-pad-or-call function-name args label maybe-landing-pad)
  ;; FIXME:   If the current function has a landing pad - then use INVOKE
  #+(or)(if maybe-landing-pad
            (cmp:irc-intrinsic-invoke function-name args maybe-landing-pad label)
            (cmp:irc-intrinsic-call function-name args label)))

(defgeneric %sadd.with-overflow (x y abi))
(defmethod %sadd.with-overflow (x y (abi abi-x86-64))
  (%intrinsic-call "llvm.sadd.with.overflow.i64" (list x y)))
(defmethod %sadd.with-overflow (x y (abi abi-x86-32))
  (%intrinsic-call "llvm.sadd.with.overflow.i32" (list x y)))

(defgeneric %ssub.with-overflow (x y abi))
(defmethod %ssub.with-overflow (x y (abi abi-x86-64))
  (%intrinsic-call "llvm.ssub.with.overflow.i64" (list x y)))
(defmethod %ssub.with-overflow (x y (abi abi-x86-32))
  (%intrinsic-call "llvm.ssub.with.overflow.i32" (list x y)))

(defun %fadd (x y &optional (label "") fast-math-flags)
  (llvm-sys:create-fadd cmp:*irbuilder* x y label fast-math-flags))
(defun %fsub (x y &optional (label "") fast-math-flags)
  (llvm-sys:create-fsub cmp:*irbuilder* x y label fast-math-flags))
(defun %fmul (x y &optional (label "") fast-math-flags)
  (llvm-sys:create-fmul cmp:*irbuilder* x y label fast-math-flags))
(defun %fdiv (x y &optional (label "") fast-math-flags)
  (llvm-sys:create-fdiv cmp:*irbuilder* x y label fast-math-flags))

(defun %fcmp-olt (x y &optional (label "") fast-math-flags)
  (llvm-sys:create-fcmp-olt cmp:*irbuilder* x y label fast-math-flags))
(defun %fcmp-ole (x y &optional (label "") fast-math-flags)
  (llvm-sys:create-fcmp-ole cmp:*irbuilder* x y label fast-math-flags))
(defun %fcmp-oeq (x y &optional (label "") fast-math-flags)
  (llvm-sys:create-fcmp-oeq cmp:*irbuilder* x y label fast-math-flags))

(defun %fpext (value type &optional (label ""))
  (llvm-sys:create-fpext cmp:*irbuilder* value type label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; with-entry-basic-block
;;;
;;; All contained LLVM-IR gets written into the function alloca block.
;;;

(defmacro with-entry-ir-builder (&rest body)
  `(let ((cmp:*irbuilder* cmp:*irbuilder-function-alloca*))
     ,@body))


(defun %gep (type object indices &optional (label "gep"))
  "Check the type against the object type and if they match return the GEP.
And convert everything to JIT constants."
  (unless (llvm-sys:type-equal type (llvm-sys:get-type object))
    (error "%gep expected object of type ~a but got ~a of type ~a"
           type object (llvm-sys:get-type object)))
  (let ((converted-indices (mapcar (lambda (x) (%i32 x)) indices)))
    (cmp:irc-gep-variable object converted-indices label)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MULTIPLE-VALUE-ARRAY-ADDRESS
;;;
;;; Return the address of the multiple-value-array structure
;;; If it hasn't been determined in this function then stick the
;;; code to look it up into the entry block of the current function
;;;

(defvar *function-current-multiple-value-array-address*)
(defun multiple-value-array-address ()
  (unless *function-current-multiple-value-array-address*
    (with-entry-ir-builder
	(setq *function-current-multiple-value-array-address*
	      (%intrinsic-call "cc_multipleValuesArrayAddress" nil))))
  *function-current-multiple-value-array-address*)

(defvar +pointers-returned-in-registers+ 1)
;; Only one pointer and one integer can be returned in registers (from X86 System V ABI)
;; so we return one pointer (value) and the number of returned values.

;;; The "return-value" we use throughout translate is actually a list (nreg register...)
;;; Using scalars helps LLVM, in that mem2reg can eliminate a structure alloca.
;;; We have to return something here that's compatible (through load-return-value)
;;; with %return-type%.
;;; FIXME: An alternate scheme would be to actually alloca a return value, but load it as
;;; a whole and use extractvalue/insertvalue. Previously we allocad it but used geps to get
;;; pointers to the fields; llvm then chokes during mem2reg, leaving the structure in memory.

;;; We have nret before the primary, which is backwards from %return-type%, in case we want
;;; to add more registers at some point.
(defun alloca-return ()
  (list (cmp:alloca-size_t "nret")
        ;; only one register
        (cmp:alloca-t* "primary-return-value")))

;;; given the above "return-value", generate code to make an actual return-value,
;;; and return that LLVM::Value. (Note the type is tmv, not tmv*.)
(defun load-return-value (return-value)
  (let ((nret (cmp:irc-load (first return-value)))
        (val0 (cmp:irc-load (second return-value))))
    (cmp:irc-make-tmv nret val0)))

;;; given a tmv, store it in a "return-value".
(defun store-tmv (tmv return-value)
  (cmp:irc-store (cmp:irc-tmv-nret tmv) (first return-value))
  (cmp:irc-store (cmp:irc-tmv-primary tmv) (second return-value)))

(defun return-value-elt (return-regs idx)
  (if (< idx +pointers-returned-in-registers+)
      (elt return-regs idx)
      (let ((multiple-value-pointer (multiple-value-array-address)))
        (%gep cmp:%t*[0]*% multiple-value-pointer (list 0 idx)))))

;;; Given an above return-value, bind nret and ret-regs to the actual pointers.
;;; (ABI is currently ignored.)
(defmacro with-return-values ((return-value abi nret ret-regs) &body body)
  (declare (ignore abi))
  (let ((rvs (gensym "RETURN-VALUE")))
    `(let* ((,rvs ,return-value)
            (,nret (first ,rvs))
            (,ret-regs (rest ,rvs)))
       ,@body)))

;;; These functions are like cc_{save,restore}MultipleValue0
;; FIXME: we don't really need intrinsics for these - they're easy
(defun save-multiple-value-0 (return-value)
  (%intrinsic-call "cc_saveMultipleValue0" (list (load-return-value return-value))))
(defun restore-multiple-value-0 (return-value)
  (store-tmv (%intrinsic-call "cc_restoreMultipleValue0" nil) return-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; APPLY-CC-CALLING-CONVENTION
;;;
;;; Simplifies interaction with the calling convention.
;;; Arguments are passed in registers and in the multiple-value-array
;;;

(defun closure-call-or-invoke (closure return-value arguments &key (label ""))
  (let* ((entry-point (cmp:irc-calculate-entry closure))
         (real-args (cmp:irc-calculate-real-args arguments))
         (args (list* closure
                      (%size_t (length arguments))
                      real-args))
         (result-in-registers
           (if cmp::*current-unwind-landing-pad-dest*
               (cmp:irc-create-invoke entry-point args cmp::*current-unwind-landing-pad-dest* label)
               (cmp:irc-create-call entry-point args label))))
    (store-tmv result-in-registers return-value)))

(defun unsafe-multiple-value-foreign-call (intrinsic-name return-value args abi &key (label ""))
  (let* ((func (or (llvm-sys:get-function cmp:*the-module* intrinsic-name)
                   (let ((arg-types (make-list (length args) :initial-element cmp:%t*%))
                         (varargs nil))
                     (cmp:irc-function-create
                      (llvm-sys:function-type-get cmp:%return-type% arg-types varargs)
                      'llvm-sys::external-linkage
                      intrinsic-name
                      cmp:*the-module*))))
         (result-in-registers
           (if cmp::*current-unwind-landing-pad-dest*
               (cmp::irc-create-invoke func args cmp::*current-unwind-landing-pad-dest*)
               (cmp::irc-create-call func args))))
    (store-tmv result-in-registers return-value)))

(defun unsafe-foreign-call (call-or-invoke foreign-types foreign-name args abi &key (label ""))
  ;; Write excess arguments into the multiple-value array
  (let* ((arguments (mapcar (lambda (type arg)
                              (%intrinsic-invoke-if-landing-pad-or-call
                               (clasp-ffi::from-translator-name type) (list arg)))
                            (second foreign-types) args))
         (func (or (llvm-sys:get-function cmp:*the-module* foreign-name)
                   (cmp:irc-function-create
                    (cmp:function-type-create-on-the-fly foreign-types)
                    'llvm-sys::External-linkage
                    foreign-name
                    cmp:*the-module*))))
    ;;; FIXME: Do these calls also need an INVOKE version if landing-pad is set????
    (if (eq :void (first foreign-types))
        (progn
          (cmp::irc-call-or-invoke func arguments)
          (%intrinsic-invoke-if-landing-pad-or-call (clasp-ffi::to-translator-name (first foreign-types)) nil))
        (let ((foreign-result (cmp::irc-call-or-invoke func arguments)))
          (%intrinsic-invoke-if-landing-pad-or-call (clasp-ffi::to-translator-name (first foreign-types)) (list foreign-result))))))

(defun unsafe-foreign-call-pointer (call-or-invoke foreign-types pointer args abi &key (label ""))
  ;; Write excess arguments into the multiple-value array
  (let* ((arguments (mapcar (lambda (type arg)
                              (%intrinsic-invoke-if-landing-pad-or-call
                               (clasp-ffi::from-translator-name type) (list arg)))
                            (second foreign-types) args))
         (function-type (cmp:function-type-create-on-the-fly foreign-types))
         (function-pointer-type (llvm-sys:type-get-pointer-to function-type))
         (pointer-t* pointer)
         (function-pointer
           (cmp:irc-bit-cast
            (%intrinsic-call "cc_getPointer" (list pointer-t*)) function-pointer-type
            "cast-function-pointer")))
    ;;; FIXME: Do these calls also need an INVOKE version if landing-pad is set????
    (if (eq :void (first foreign-types))
        (progn
          (cmp::irc-call-or-invoke function-pointer arguments)
          (%intrinsic-invoke-if-landing-pad-or-call (clasp-ffi::to-translator-name (first foreign-types)) nil))
        (let ((result-in-t* (cmp::irc-call-or-invoke function-pointer arguments)))
          (%intrinsic-invoke-if-landing-pad-or-call (clasp-ffi::to-translator-name (first foreign-types)) (list result-in-t*))))))
