
(in-package :clasp-cleavir-hir)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction NAMED-ENTER-INSTRUCTION
;;;
;;; This instruction is an ENTER-INSTRUCTION that keeps
;;; track of the lambda-name


(defclass named-enter-instruction (cleavir-ir:enter-instruction)
  ((%lambda-name :initarg :lambda-name :initform "lambda" :accessor lambda-name)))

(defun make-named-enter-instruction
    (lambda-list lambda-name &optional (successor nil successor-p))
  (let ((oe (if successor-p
		(cleavir-ir:make-enter-instruction lambda-list successor)
		(cleavir-ir:make-enter-instruction lambda-list))))
    (change-class oe 'named-enter-instruction :lambda-name lambda-name)))


(defmethod cleavir-ir-graphviz:label ((instr named-enter-instruction))
  (with-output-to-string (s)
    (format s "named-enter(~a)" (lambda-name instr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction PRECALC-SYMBOL-INSTRUCTION.
;;;
;;; This instruction is used to lookup a precalculated value
;;; in a precalculated symbol or value vector
;;; Represented as a vector containing an entry for each
;;; precalculated value.  
;;;
;;; This instruction takes two inputs.  The first input is a simple vector
;;; that holds the precalculated values.  The second
;;; input is an immediate input containing a non-negative integer and
;;; which serves as an index into the vector.  This
;;; instruction has a single output, which is a dynamic lexical
;;; location.

(defclass precalc-symbol-instruction (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ((%original-object :initarg :original-object :accessor precalc-symbol-instruction-original-object)))

(defun make-precalc-symbol-instruction
    (index-input output &key successor vector original-object)
  (make-instance 'precalc-symbol-instruction
    :inputs (list index-input)
    :outputs (list output)
    :successors (if (null successor) nil (list successor))
    :vector vector
    :original-object original-object))


(defun escaped-string (str)
  (with-output-to-string (s) (loop for c across str do (when (member c '(#\\ #\")) (princ #\\ s)) (princ c s))))

(defmethod cleavir-ir-graphviz:label ((instr precalc-symbol-instruction))
  (with-output-to-string (s)
    (format s "precalc-symbol-ref ; ")
    (let ((original-object (escaped-string
			 (format nil "~s" (precalc-symbol-instruction-original-object instr)))))
      (if (> (length original-object) 30)
	  (format s "~a..." (subseq original-object 0 30))
	  (princ original-object s)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction PRECALC-VALUE-INSTRUCTION.
;;;
;;; This instruction is used to lookup a precalculated value
;;; in a precalculated symbol or value vector
;;; Represented as a vector containing an entry for each
;;; precalculated value.  
;;;
;;; This instruction takes two inputs.  The first input is a simple vector
;;; that holds the precalculated values.  The second
;;; input is an immediate input containing a non-negative integer and
;;; which serves as an index into the vector.  This
;;; instruction has a single output, which is a dynamic lexical
;;; location.

(defclass precalc-value-instruction (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ((%original-object :initarg :original-object :accessor precalc-value-instruction-original-object)))

(defun make-precalc-value-instruction
    (index-input output &key successor vector original-object)
  (make-instance 'precalc-value-instruction
    :inputs (list index-input)
    :outputs (list output)
    :successors (if (null successor) nil (list successor))
    :original-object original-object))


(defun escaped-string (str)
  (with-output-to-string (s) (loop for c across str do (when (member c '(#\\ #\")) (princ #\\ s)) (princ c s))))

(defmethod cleavir-ir-graphviz:label ((instr precalc-value-instruction))
  (with-output-to-string (s)
    (format s "precalc-value-ref ; ")
    (let ((original-object (escaped-string
			 (format nil "~s" (precalc-value-instruction-original-object instr)))))
      (if (> (length original-object) 30)
	  (format s "~a..." (subseq original-object 0 30))
	  (princ original-object s)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction MULTIPLE-VALUE-ONE-FORM-CALL-INSTRUCTION.
;;;
;;; The first input of this instruction is an ordinary lexical
;;; location.  The remaining inputs are of type VALUES-LOCATION, and
;;; each represents multiple values returned form the evaluation of
;;; some form.  This instruction has a single output, also of the type
;;; VALUES-LOCATION.

(defclass multiple-value-one-form-call-instruction (cleavir-ir:multiple-value-call-instruction)
  ())

(defun make-multiple-value-one-form-call-instruction
    (inputs output &optional (successor nil successor-p))
  (make-instance 'multiple-value-one-form-call-instruction
    :inputs inputs
    :outputs (list output)
    :successors (if successor-p (list successor) '())))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FDEFINITION-INSTRUCTION.

(defclass setf-fdefinition-instruction (cleavir-ir:fdefinition-instruction)
  ())

(defun make-setf-fdefinition-instruction
    (input output &optional (successor nil successor-p))
  (make-instance 'setf-fdefinition-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (if successor-p (list successor) '())))


(defmethod cleavir-ir-graphviz:label ((instruction setf-fdefinition-instruction)) "setf-fdefinition")
