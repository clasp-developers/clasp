#+(or)(eval-when (:compile-toplevel :execute :load-toplevel)
  (setq *echo-repl-read* t)
  (setq cmp::*debug-compile-file* t)
  (format t "About to compile-file ir.lsp~%"))

(in-package :clasp-cleavir)

(defgeneric literal-label (literal))

(defclass literal ()
  ((%value :initarg :value :reader literal-value)))

(defclass immediate-literal (literal)
  ((%tagged-value :initarg :tagged-value :reader immediate-literal-tagged-value)))

(defmethod literal-label ((literal immediate-literal))
  (format nil "~a" (immediate-literal-tagged-value literal)))

(defmethod make-load-form ((thing clasp-cleavir::immediate-literal) &optional environment)
  (make-load-form-saving-slots thing :environment environment))
  
(defmethod print-object ((object immediate-literal) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream ":value ~s" (literal-value object))))

(defclass arrayed-literal (literal)
  ((%index :initarg :index :reader arrayed-literal-index)
   (%literal-name :initarg :literal-name :reader arrayed-literal-literal-name)))

(defmethod literal-label ((literal arrayed-literal))
  (if (arrayed-literal-literal-name literal)
      (format nil "~a/~a" (arrayed-literal-index literal) (arrayed-literal-literal-name literal))
      (format nil "~a" (arrayed-literal-index literal))))

(defmethod print-object ((object arrayed-literal) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream ":value ~s :index ~s :literal-name ~s" (literal-value object)
            (arrayed-literal-index object)
            (arrayed-literal-literal-name object))))


(defun %literal-index (value &optional read-only-p)
  (let ((*debug-cleavir* *debug-cleavir-literals*))
    (multiple-value-bind (data in-array literal-name)
        (literal:reference-literal value read-only-p)
      (if in-array
          (make-instance 'arrayed-literal :value value :index data :literal-name literal-name)
          (make-instance 'immediate-literal :value value :tagged-value data)))))

  
(defun %literal-ref (value &optional read-only-p)
  (let ((literal (%literal-index value read-only-p)))
    (if (typep literal 'arrayed-literal)
        (let* ((index (arrayed-literal-index literal))
               (literal-label (if (arrayed-literal-literal-name literal)
                                  (bformat nil "values-table[%d]/%s" index (arrayed-literal-literal-name literal))
                                  (bformat nil "values-table[%d]" index)))
               (gep (llvm-sys:create-const-gep2-64 cmp:*irbuilder*
                                                   (cmp:ltv-global)
                                                   0 index
                                                   literal-label)))
          gep)
        (error "%literal-ref of immediate value ~s is illegal" value))))

(defun %literal-value (value &optional (label "literal"))
  (cmp:irc-load (%literal-ref value)))

(defun %closurette-index (lambda-name function source-info-handle
                          filepos lineno column)
  (literal::reference-closure lambda-name function source-info-handle
                              filepos lineno column))

(defun %closurette-ref (lambda-name function source-info-handle
                        filepos lineno column)
  (let* ((index (%closurette-index lambda-name function source-info-handle
                                   filepos lineno column))
         (gep (llvm-sys:create-const-gep2-64 cmp:*irbuilder*
                                             (cmp:ltv-global)
                                             0 index
                                             (bformat nil "values-table[%d]" index))))
    gep))

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

(defun %extract (val index &optional (label "extract"))
  (llvm-sys:create-extract-value cmp:*irbuilder* val (list index) label))

(defun %nil ()
  "A nil in a T*"
  (%literal-value nil))

(defun alloca (type size &optional (label ""))
  (llvm-sys:create-alloca cmp:*irbuilder-function-alloca* type (%i32 size) label))

(defun alloca-vaslist (&optional (label "vaslist"))
  (cmp:irc-alloca-vaslist :label label))

(defun alloca-va_list (&optional (label "vaslist"))
  (cmp:irc-alloca-va_list :label label))

(defun alloca-invocation-history-frame (&optional (label "ihf"))
  (cmp:irc-alloca-invocation-history-frame :label label))

(defun alloca-register-save-area (&optional (label "ihf"))
  (cmp:irc-alloca-register-save-area :label label))

(defun alloca-size_t (&optional (label "var"))
  (cmp:irc-alloca-size_t :label label))

(defun alloca-i32 (&optional (label "var"))
  (alloca cmp:%i32% 1 label))

(defun alloca-i8* (&optional (label "var"))
  (alloca cmp:%i8*% 1 label))

(defun alloca-i8 (num &optional (label "var"))
  "Allocate a block of memory in the stack frame"
  (alloca cmp:%i8% num label))

(defun alloca-return_type (&optional (label "return-value"))
  (alloca cmp:%return_type% 1 label))

(defun alloca-t* (&optional (label "var"))
  (let ((instr (alloca cmp:%t*% 1 label)))
    #+(or)(cc-dbg-when *debug-log*
		       (format *debug-log* "          alloca-t*   cmp:*irbuilder-function-alloca* = ~a~%" cmp:*irbuilder-function-alloca*)
		       (format *debug-log* "          Wrote ALLOCA ~a into function ~a~%" instr (llvm-sys:get-name (instruction-llvm-function instr))))
    instr))

(defun alloca-mv-struct (&optional (label "V"))
  (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
    (llvm-sys:create-alloca cmp:*irbuilder* cmp:%mv-struct% (%i32 1) label)))


(defun %load-or-null (obj)
  (if obj
      (cmp:irc-load obj)
      (llvm-sys:constant-pointer-null-get cmp:%t*%)))


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

(defun %load (place &optional (label ""))
  (cmp:irc-load place label))

(defun %store (val target &optional label)
  (let* ((instr (cmp:irc-store val target)))
    (when (typep target 'llvm-sys::instruction)
      (let ((store-fn (llvm-sys:get-name (instruction-llvm-function instr)))
	    (target-fn (llvm-sys:get-name (instruction-llvm-function target))))
	(unless (string= store-fn target-fn)
	  (error "Mismatch in store function vs target function - you are attempting to store a value in a target where the store instruction is in a different LLVM function(~a) from the target value(~a)" store-fn target-fn))))
    instr))

(defun %bit-cast (val type &optional (label ""))
  (llvm-sys:create-bit-cast cmp:*irbuilder* val type label))

(defun %pointer-cast (from totype &optional (label ""))
  (cmp:irc-pointer-cast from totype label))

(defun %ptrtoint (val type &optional (label ""))
  (llvm-sys:create-ptr-to-int cmp:*irbuilder* val type label))

(defun %inttoptr (val type &optional (label ""))
  (llvm-sys:create-int-to-ptr cmp:*irbuilder* val type label))

(defun %and (x y &optional (label ""))
  (llvm-sys:create-and-value-value cmp:*irbuilder* x y label))
(defun %or (x y &optional (label ""))
  (llvm-sys:create-or-value-value cmp:*irbuilder* x y label))

(defun %add (x y &optional (label ""))
  (llvm-sys:create-add cmp:*irbuilder* x y label))
(defun %add-nsw (x y &optional (label ""))
  (llvm-sys:create-nswadd cmp:*irbuilder* x y label))
(defun %add-nuw (x y &optional (label ""))
  (llvm-sys:create-nuwadd cmp:*irbuilder* x y label))

(defun %icmp-eq (x y &optional (label ""))
  (llvm-sys:create-icmp-eq cmp:*irbuilder* x y label))
(defun %icmp-ne (x y &optional (label ""))
  (llvm-sys:create-icmp-ne cmp:*irbuilder* x y label))
(defun %icmp-slt (x y &optional (label ""))
  (llvm-sys:create-icmp-slt cmp:*irbuilder* x y label))
(defun %icmp-sle (x y &optional (label ""))
  (llvm-sys:create-icmp-sle cmp:*irbuilder* x y label))
(defun %icmp-sgt (x y &optional (label ""))
  (llvm-sys:create-icmp-sgt cmp:*irbuilder* x y label))
(defun %icmp-sge (x y &optional (label ""))
  (llvm-sys:create-icmp-sge cmp:*irbuilder* x y label))

(defun %cond-br (test true-branch false-branch &key likely-true likely-false)
  (llvm-sys:create-cond-br cmp:*irbuilder* test true-branch false-branch nil))

(defun %ret (val)
  (cmp:irc-ret val))

(defun %sub (minuend subtrahend &key (label "") nuw nsw)
  (llvm-sys:create-sub cmp:*irbuilder* minuend subtrahend label nuw nsw))

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

(defun %shl (value shift &key (label "") nuw nsw)
  "If shift is an integer, generate shl with a constant uint64.
Otherwise do a variable shift."
  (if (integerp shift)
      (llvm-sys:create-shl-value-uint64
       cmp:*irbuilder* value shift label nuw nsw)
      (llvm-sys:create-shl-value-value
       cmp:*irbuilder* value shift label nuw nsw)))

(defun %lshr (value shift &key (label "") exact)
  "If shift is an integer, generate lshr with a constant uint64.
Otherwise do a variable shift."
  (if (integerp shift)
      (llvm-sys:create-lshr-value-uint64
       cmp:*irbuilder* value shift label exact)
      (llvm-sys:create-lshr-value-value
       cmp:*irbuilder* value shift label exact)))

(defun %udiv (dividend divisor &key (label "") exact)
  (llvm-sys:create-udiv cmp:*irbuilder* dividend divisor label exact))
(defun %urem (dividend divisor &key (label ""))
  (llvm-sys:create-urem cmp:*irbuilder* dividend divisor label))

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


(defun %gep-variable (object indices &optional (label "gep"))
  (llvm-sys:create-in-bounds-gep cmp:*irbuilder* object indices label))

(defun %gep (type object indices &optional (label "gep"))
  "Check the type against the object type and if they match return the GEP.
And convert everything to JIT constants."
  (unless (equal type (llvm-sys:get-type object))
    (error "%gep expected object of type ~a but got ~a of type ~a"
           type object (llvm-sys:get-type object)))
  (let ((converted-indices (mapcar (lambda (x) (%i32 x)) indices)))
    (%gep-variable object converted-indices label)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; RETURN-VALUES
;;;
;;; Simplifies access to the return values stored
;;; in registers and in the multiple-value-array
;;; %numvals - returns a Value that stores the number of return values
;;; %return-registers - a list of the first (register-return-size) return values
;;;                     returned in registers (X86 System V ABI says 1pointer + 1integer)
;;; %multiple-value-array-address - The an llvm::Value address of the multiple-value array
;;;                                 It is only set if the array is needed.
;;;
(defclass return-values ()
  ((%sret-arg :initarg :sret-arg :accessor sret-arg)
   (%numvals :initarg :numvals :accessor number-of-return-values)
   (%return-registers :initarg :return-registers :accessor return-registers)))


(defvar +pointers-returned-in-registers+ 1)
;; Only one pointer and one integer can be returned in registers return value returned in a register

(defgeneric make-return-values (return-sret-arg abi))

(defmethod make-return-values (return-sret-arg (abi abi-x86-64))
  (make-instance 'return-values
		 :sret-arg return-sret-arg
		 :numvals (llvm-sys:create-in-bounds-gep cmp:*irbuilder* return-sret-arg (list (%i32 0) (%i32 1)) "ret-nvals")
		 :return-registers (list (llvm-sys:create-in-bounds-gep cmp:*irbuilder* return-sret-arg (list (%i32 0) (%i32 0)) "ret-regs"))))


(defun return-values-num (return-vals)
  (number-of-return-values return-vals))

(defun return-value-elt (return-vals idx)
  (if (< idx +pointers-returned-in-registers+)
      (elt (return-registers return-vals) idx)
      (let ((multiple-value-pointer (multiple-value-array-address)))
        (%gep cmp::%t*[0]*% multiple-value-pointer (list 0 idx)))))

(defmacro with-return-values ((return-vals return-value abi) &body body)
  `(let* ((,return-vals (make-return-values ,return-value ,abi)))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; APPLY-CC-CALLING-CONVENTION
;;;
;;; Simplifies interaction with the calling convention.
;;; Arguments are passed in registers and in the multiple-value-array
;;;

(defun closure-call-or-invoke (closure return-value arg-allocas abi &key (label ""))
  (let* ((entry-point (cmp::irc-calculate-entry (%load closure))) ;; intrinsic-name "cc_call")
         (arguments (mapcar (lambda (x) (%load x)) arg-allocas))
         (real-args (if (< (length arguments) core:+number-of-fixed-arguments+)
                        (append arguments (make-list (- core:+number-of-fixed-arguments+ (length arguments)) :initial-element (cmp:null-t-ptr)))
                        arguments)))
    (with-return-values (return-vals return-value abi)
      (let ((args (list*
                   (cmp:irc-load closure)
                   ;;                   (cmp:null-t-ptr)
                   (%size_t (length arguments))
                   real-args)))
        (let* ((result-in-registers
                (if cmp::*current-unwind-landing-pad-dest*
                    (cmp:irc-create-invoke entry-point args cmp::*current-unwind-landing-pad-dest* label)
                    (cmp:irc-create-call entry-point args label))))
          (%store result-in-registers return-value))))))


(defun unsafe-multiple-value-foreign-call (intrinsic-name return-value arg-allocas abi &key (label ""))
  (with-return-values (return-vals return-value abi)
    (let* ((args (mapcar (lambda (x) (%load x)) arg-allocas))
           (func (or (llvm-sys:get-function cmp:*the-module* intrinsic-name)
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
      (%store result-in-registers return-value))))

(defun unsafe-foreign-call (call-or-invoke foreign-types foreign-name output arg-allocas abi &key (label ""))
  ;; Write excess arguments into the multiple-value array
  (let* ((arguments (mapcar (lambda (type arg)
                              (%intrinsic-invoke-if-landing-pad-or-call (clasp-ffi::from-translator-name type) (list (%load arg))))
                            (second foreign-types) arg-allocas))
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
          (%store (%intrinsic-invoke-if-landing-pad-or-call (clasp-ffi::to-translator-name (first foreign-types)) nil) output))
        (let ((foreign-result (cmp::irc-call-or-invoke func arguments)))
          (%store (%intrinsic-invoke-if-landing-pad-or-call (clasp-ffi::to-translator-name (first foreign-types)) (list foreign-result)) output)))))

(defun unsafe-foreign-call-pointer (call-or-invoke foreign-types pointer output arg-allocas abi &key (label ""))
  ;; Write excess arguments into the multiple-value array
  (let* ((arguments (mapcar (lambda (type arg)
                              (%intrinsic-invoke-if-landing-pad-or-call (clasp-ffi::from-translator-name type) (list (%load arg))))
                            (second foreign-types) arg-allocas))
         (function-type (cmp:function-type-create-on-the-fly foreign-types))
         (function-pointer-type (llvm-sys:type-get-pointer-to function-type))
         (pointer-t* pointer)
         (function-pointer (%bit-cast (%intrinsic-call "cc_getPointer" (list pointer-t*)) function-pointer-type "cast-function-pointer")))
    ;;; FIXME: Do these calls also need an INVOKE version if landing-pad is set????
    (if (eq :void (first foreign-types))
        (progn
          (cmp::irc-call-or-invoke function-pointer arguments)
          (%store (%intrinsic-invoke-if-landing-pad-or-call (clasp-ffi::to-translator-name (first foreign-types)) nil) output))
        (let ((result-in-t* (cmp::irc-call-or-invoke function-pointer arguments)))
          (%store (%intrinsic-invoke-if-landing-pad-or-call (clasp-ffi::to-translator-name (first foreign-types)) (list result-in-t*)) output)))))
