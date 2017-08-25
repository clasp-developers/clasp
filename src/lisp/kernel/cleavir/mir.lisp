(in-package #:cc-mir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TYPED-LEXICAL-LOCATION
;;;
;;; Lexical locations for unboxed values. Know their LLVM type.
;;; Hypothetically, all lexical locations could be turned into
;;; these, with the vast majority having an LLVM type of t*.

(defclass typed-lexical-location (cleavir-ir:lexical-location)
  ((%type :initarg :type :accessor lexical-location-type)))

;;; Convenience
(defun insert-after (new old)
  (cleavir-ir:insert-instruction-after new old)
  new)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CHARACTERP-INSTRUCTION
;;;
;;; Generates code for characterp
;;;

(defclass characterp-instruction (cleavir-ir:instruction cleavir-ir:two-successors-mixin) ())

(defun make-characterp-instruction (input successors)
  (make-instance 'characterp-instruction
                 :inputs (list input)
                 :successors successors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SINGLE-FLOAT-P-INSTRUCTION
;;;
;;; Generates code for single-float-p
;;;

(defclass single-float-p-instruction (cleavir-ir:instruction cleavir-ir:two-successors-mixin) ())

(defun make-single-float-p-instruction (input successors)
  (make-instance 'single-float-p-instruction
                 :inputs (list input)
                 :successors successors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; HEADERQ-INSTRUCTION
;;;
;;; Branch based on the type header of an object.
;;;

(defclass headerq-instruction (cleavir-ir:instruction cleavir-ir:two-successors-mixin)
  ((%header-value-min-max :initarg :hvmm :accessor header-value-min-max)))

(defun make-headerq-instruction (header-value-min-max input successors)
  (make-instance 'headerq-instruction
                 :hvmm header-value-min-max
                 :inputs (list input)
                 :successors successors))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Assign a unique integer index to every instruction
;;; using the stealth mixin - see system.lisp
;;;

(defun assign-mir-instruction-datum-ids (top)
  (let ((id 1)
	(datums (make-hash-table)))
    (cleavir-ir:map-instructions 
     (lambda (instr)
       (setf (clasp-cleavir:instruction-gid instr) (incf id))
       (loop for datum in (append (cleavir-ir:inputs instr) (cleavir-ir:outputs instr))
	    do (unless (gethash datum datums)
		 (setf (gethash datum datums) t)
		 (setf (clasp-cleavir:datum-gid datum) (incf id)))))
     top)))


(defgeneric label-datum (datum))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum CONSTANT-INPUT.

(defmethod label-datum ((datum cleavir-ir:constant-input))
  (cleavir-ir:value datum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum LEXICAL-LOCATION.

(defmethod label-datum ((datum cleavir-ir:lexical-location))
  (cleavir-ir:name datum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum VALUES-LOCATION.

(defmethod label-datum ((datum cleavir-ir:values-location)) "V")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum IMMEDIATE-INPUT.

(defmethod label-datum ((datum cleavir-ir:immediate-input)) (cleavir-ir:value datum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum LOAD-TIME-VALUE-INPUT.

(defmethod label-datum ((datum cleavir-ir:load-time-value-input))
  "LTV")


(defun describe-mir (instr )
  (with-output-to-string (stream)
    (format stream "~a " (cleavir-ir-graphviz:label instr))
    (when (cleavir-ir:inputs instr)
      (format stream "(")
      (loop for datum in (cleavir-ir:inputs instr)
	 do (format stream "~a " (label-datum datum)))
      (format stream ")"))
    (when (or (cleavir-ir:inputs instr) (cleavir-ir:outputs instr))
      (format stream " -> "))
    (when (cleavir-ir:outputs instr)
      (format stream "(")
      (loop for datum in (cleavir-ir:outputs instr)
	 do (format stream "~a " (label-datum datum)))
      (format stream ")"))))
