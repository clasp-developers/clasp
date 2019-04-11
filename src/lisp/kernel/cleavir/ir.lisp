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

(defgeneric make-return-nret (return-value abi))

(defmethod make-return-nret (return-value (abi abi-x86-64))
  (cmp:irc-gep-variable return-value (list (%i32 0) (%i32 1)) "ret-nvals"))

(defgeneric make-return-regs (return-value abi))

(defmethod make-return-regs (return-value (abi abi-x86-64))
  (list ; only one register.
   (cmp:irc-gep-variable return-value (list (%i32 0) (%i32 0)) "reg-regs")))

(defun return-value-elt (return-regs idx)
  (if (< idx +pointers-returned-in-registers+)
      (elt return-regs idx)
      (let ((multiple-value-pointer (multiple-value-array-address)))
        (%gep cmp::%t*[0]*% multiple-value-pointer (list 0 idx)))))

(defmacro with-return-values ((return-value abi nret ret-regs) &body body)
  (let ((rvs (gensym "RETURN-VALUE")) (abis (gensym "ABI")))
    `(let* ((,rvs ,return-value) (,abis ,abi)
            (,nret (make-return-nret ,rvs ,abis))
            (,ret-regs (make-return-regs ,rvs ,abis)))
       (declare (ignorable ,nret ,ret-regs))
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; APPLY-CC-CALLING-CONVENTION
;;;
;;; Simplifies interaction with the calling convention.
;;; Arguments are passed in registers and in the multiple-value-array
;;;

(defun closure-call-or-invoke (closure return-value arguments &key (label ""))
  (let* ((entry-point (cmp::irc-calculate-entry closure)) ;; intrinsic-name "cc_call")
         (real-args (if (< (length arguments) core:+number-of-fixed-arguments+)
                        (append arguments (make-list (- core:+number-of-fixed-arguments+ (length arguments)) :initial-element (cmp:null-t-ptr)))
                        arguments)))
    (let ((args (list*
                 closure
                 ;;                   (cmp:null-t-ptr)
                 (%size_t (length arguments))
                 real-args)))
      (let* ((result-in-registers
               (if cmp::*current-unwind-landing-pad-dest*
                   (cmp:irc-create-invoke entry-point args cmp::*current-unwind-landing-pad-dest* label)
                   (cmp:irc-create-call entry-point args label))))
        (cmp:irc-store result-in-registers return-value)))))

(defun unsafe-multiple-value-foreign-call (intrinsic-name return-value args abi &key (label ""))
  (let* ((func (or (llvm-sys:get-function cmp:*the-module* intrinsic-name)
                   (let ((arg-types (make-list (length args) :initial-element cmp:%t*%))
                         (varargs nil))
                     (cmp:irc-function-create
                      (llvm-sys:function-type-get cmp:%return_type% arg-types varargs)
                      'llvm-sys::External-linkage
                      intrinsic-name
                      cmp:*the-module*))))
         (result-in-registers
           (if cmp::*current-unwind-landing-pad-dest*
               (cmp::irc-create-invoke func args cmp::*current-unwind-landing-pad-dest*)
               (cmp::irc-create-call func args))))
    (cmp:irc-store result-in-registers return-value)))

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
