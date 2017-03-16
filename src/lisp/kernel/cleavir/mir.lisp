(in-package #:cc-mir)

(defun insert-after (new old)
  (cleavir-ir:insert-instruction-after new old)
  new)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; STACK-ENCLOSE
;;;
;;; Stores a core::ClosureWithSlots* pointer on the stack
;;;

(defclass stack-enclose-instruction (cleavir-ir:enclose-instruction) ())

(defun make-stack-enclose-instruction (output successor code)
  (make-instance 'stack-enclose-instruction
                 :outputs (list output)
                 :successors (list successor)
                 :code code))

(defmethod cleavir-ir-graphviz:draw-instruction ((instruction stack-enclose-instruction) stream)
  (format stream "   ~a [label = \"stack-enclose\"];~%"
	  (cleavir-ir-graphviz::instruction-id instruction))
  (format stream "  ~a -> ~a [color = pink, style = dashed];~%"
	  (gethash (cleavir-ir:code instruction) cleavir-ir-graphviz::*instruction-table*)
	  (cleavir-ir-graphviz::instruction-id instruction)))


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
                 :successors (list successor)))

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
                 :successors (list successor)))

 
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
