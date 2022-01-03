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
  (declare (ignore label))
  (cmp:irc-load (%literal-ref value)))

(defun %closurette-index (function)
  (unless (cmp:xep-group-p function)
    (error "The first argument to %closurette-index must be a xep-group - instead it is a ~s of class ~s" function (class-name (class-of function))))
  (literal::reference-closure function))

(defun %closurette-ref (function)
  (let* ((index (%closurette-index function))
         (gep (llvm-sys:create-const-gep2-64 cmp:*irbuilder*
                                             (literal:ltv-global)
                                             0 index
                                             (bformat nil "values-table[%d]" index))))
    gep))

(defun %closurette-value (function)
  (cmp:irc-load (%closurette-ref function)))

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
  (let* ((info (or (gethash function-name (cmp::get-primitives))
                   (error "BUG: Not a primitive: ~a" function-name)))
         (does-not-throw (getf (cmp::primitive-properties info) :does-not-throw)))
    (when (and (null does-not-throw)) ; it's not cc_throw
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
(defun %frem (x y &optional (label "") fast-math-flags)
  (llvm-sys:create-frem cmp:*irbuilder* x y label fast-math-flags))

(defun %fcmp-olt (x y &optional (label "") fast-math-flags)
  (llvm-sys:create-fcmp-olt cmp:*irbuilder* x y label fast-math-flags))
(defun %fcmp-ole (x y &optional (label "") fast-math-flags)
  (llvm-sys:create-fcmp-ole cmp:*irbuilder* x y label fast-math-flags))
(defun %fcmp-oeq (x y &optional (label "") fast-math-flags)
  (llvm-sys:create-fcmp-oeq cmp:*irbuilder* x y label fast-math-flags))

(defun %fneg (value &optional (label "") fast-math-flags)
  (llvm-sys:create-fneg cmp:*irbuilder* value label fast-math-flags))

(defun %fpext (value type &optional (label ""))
  (llvm-sys:create-fpext cmp:*irbuilder* value type label))
(defun %fptrunc (value type &optional (label ""))
  (llvm-sys:create-fptrunc cmp:*irbuilder* value type label))

(defun %sitofp (value type &optional (label ""))
  (llvm-sys:create-sito-fp cmp:*irbuilder* value type label))

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

(defun return-value-elt (idx)
  (assert (>= idx +pointers-returned-in-registers+))
  (let ((multiple-value-pointer (multiple-value-array-address)))
    (%gep cmp:%t*[0]*% multiple-value-pointer (list 0 idx))))

;;; These functions are like cc_{save,restore}MultipleValue0
;; FIXME: we don't really need intrinsics for these - they're easy
(defun save-multiple-value-0 (tmv)
  (%intrinsic-call "cc_saveMultipleValue0" (list tmv)))
(defun restore-multiple-value-0 ()
  (%intrinsic-call "cc_restoreMultipleValue0" nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; APPLY-CC-CALLING-CONVENTION
;;;
;;; Simplifies interaction with the calling convention.
;;; Arguments are passed in registers and in the multiple-value-array
;;;

(defun closure-call-or-invoke (closure arguments &key (label ""))
  (cmp:irc-funcall-results-in-registers closure arguments label)
  #+(or)
  (let ((call-info (cmp:irc-calculate-call-info closure arguments)))
    (cmp::irc-call-or-invoke (cmp:call-info-function-type call-info)
                             (call-info-entry-point call-info)
                             (call-info-real-args call-info)
                             cmp::*current-unwind-landing-pad-dest*
                             label)))

(defun unsafe-multiple-value-foreign-call (intrinsic-name args abi
                                           &key (label ""))
  (declare (ignore abi))
  (let* ((func (or (llvm-sys:get-function cmp:*the-module* intrinsic-name)
                   (let ((arg-types (make-list (length args) :initial-element cmp:%t*%))
                         (varargs nil))
                     (cmp:irc-function-create
                      (llvm-sys:function-type-get cmp:%return-type% arg-types varargs)
                      'llvm-sys::external-linkage
                      intrinsic-name
                      cmp:*the-module*))))
         (function-type (llvm-sys:get-function-type func)))
    (cmp::irc-call-or-invoke function-type func args
                             cmp::*current-unwind-landing-pad-dest*
                             label)))

(defun unsafe-foreign-call (call-or-invoke foreign-types foreign-name args abi &key (label ""))
  (declare (ignore call-or-invoke abi label))
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
                    cmp:*the-module*)))
         (function-type (llvm-sys:get-function-type func)))
    ;;; FIXME: Do these calls also need an INVOKE version if landing-pad is set????
    (if (eq :void (first foreign-types))
        (progn
          (cmp::irc-call-or-invoke function-type func arguments)
          (%intrinsic-invoke-if-landing-pad-or-call (clasp-ffi::to-translator-name (first foreign-types)) nil))
        (let ((foreign-result (cmp::irc-call-or-invoke function-type func arguments)))
          (%intrinsic-invoke-if-landing-pad-or-call (clasp-ffi::to-translator-name (first foreign-types)) (list foreign-result))))))

(defun unsafe-foreign-call-pointer (call-or-invoke foreign-types pointer args abi)
  (declare (ignore call-or-invoke abi))
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
          (cmp::irc-call-or-invoke function-type function-pointer arguments)
          (%intrinsic-invoke-if-landing-pad-or-call (clasp-ffi::to-translator-name (first foreign-types)) nil))
        (let ((result-in-t* (cmp::irc-call-or-invoke function-type function-pointer arguments)))
          (%intrinsic-invoke-if-landing-pad-or-call (clasp-ffi::to-translator-name (first foreign-types)) (list result-in-t*))))))
