(in-package :clasp-cleavir)

(defun %i32 (num)
  (cmp:jit-constant-i32 num))

(defun %i64 (num)
  (cmp:jit-constant-i64 num))

(defun %size_t (num)
  (cmp:jit-constant-size_t num))

(defun %nil ()
  "A nil in a T*"
  (llvm-sys:create-int-to-ptr cmp:*irbuilder* (cmp:jit-constant-size_t cmp:+nil-value+) cmp:+t*+ "nil"))

(defun %load-or-null (obj)
  (if obj
      (cmp:irc-load obj)
      (llvm-sys:constant-pointer-null-get cmp:+t*+)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; with-entry-basic-block
;;;
;;; All contained LLVM-IR gets written into the clasp-cleavir:*current-function-entry-basic-block*
;;;

(defmacro with-entry-basic-block (&rest body)
  `(let ((cmp:*irbuilder* clasp-cleavir:*current-function-entry-basic-block*))
     ,@body))





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
    (with-entry-basic-block
	(setq *function-current-multiple-value-array-address* 
	      (irc-intrinsic "cc_multipleValuesArrayAddress" "mvpArray"))))
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
	(error "Finish implementing return-value-elt - you need to use gep to index into the array")
	#||(setf (multiple-value-array-address return-vals) multiple-value-pointer))
	(multiple-value-array-get multiple-value-pointer idx)||#)))




(defmacro with-return-values ((return-vals abi) &body body)
  (let ((args (gensym "args"))
	(return-sret-arg (gensym "retstruct")))
    `(let* ((,args (llvm-sys:get-argument-list cmp:*current-function*))
	    (,return-sret-arg (first ,args))
	    (,return-vals (make-return-values ,return-sret-arg ,abi)))
       ,@body)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; APPLY-CC-CALLING-CONVENTION
;;;
;;; Simplifies interaction with the calling convention.
;;; Arguments are passed in registers and in the multiple-value-array
;;;

(defun apply-closure (closure arguments abi)
  ;; Write excess arguments into the multiple-value array
  (unless (<= (length arguments) 5)
    (let ((mv-args (nthcdr 5 arguments)))
      (do ((idx 5 (1+ idx))
	   (cur-arg (nthcdr 5 arguments) (cdr cur-arg))
	   (arg (car cur-arg) (car cur-arg)))
	  ((null cur-arg) nil)
	(cmp:irc-store (cmp:irc-load arg) (multiple-value-elt idx)))))
  (with-return-values (return-vals abi)
    (cmp:irc-intrinsic "cc_apply"
		       (sret-arg return-vals)
		       (cmp:irc-load closure)
		       (%size_t (length arguments))
		       (%load-or-null (first arguments))
		       (%load-or-null (second arguments))
		       (%load-or-null (third arguments))
		       (%load-or-null (fourth arguments))
		       (%load-or-null (fifth arguments)))))

