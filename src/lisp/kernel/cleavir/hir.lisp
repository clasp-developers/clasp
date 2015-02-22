
(in-package :clasp-cleavir-hir)

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
  ((%vector :initarg :vector :accessor precalc-symbol-instruction-vector)
   (%original-object :initarg :original-object :accessor precalc-symbol-instruction-original-object)))

(defun make-precalc-symbol-instruction
    (vector-input index-input output &key successor vector original-object)
  (make-instance 'precalc-symbol-instruction
    :inputs (list vector-input index-input)
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
      (if (> (length original-object) 10)
	  (format s "~a..." (subseq original-object 0 10))
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
    (vector-input index-input output &key successor vector original-object)
  (make-instance 'precalc-value-instruction
    :inputs (list vector-input index-input)
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
      (if (> (length original-object) 10)
	  (format s "~a..." (subseq original-object 0 10))
	  (princ original-object s)))))
