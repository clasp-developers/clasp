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
;;; SAVE-FRAME-INSTRUCTION
;;;
;;; Allocate a marker for the frame so that it can be unwound to.

(defclass save-frame-instruction (cleavir-ir:instruction cleavir-ir:one-successor-mixin) ())

(defun make-save-frame-instruction (output &optional (successor nil successor-p))
  (make-instance 'save-frame-instruction
                 :outputs (list output)
                 :successors (if successor-p (list successor) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Like a CATCH-INSTRUCTION, but instead of making a continuation
;;; just returns the one it's input.
;;; The GO-INDEX is a small integer used to tell apart catches in
;;; the same function.
;;;
;;; FIXME: We don't represent the INVOKE alternates explicitly in the IR graph, instead relying
;;; on cmp:with-landing-pad (i.e. dynamic variables). This means we can't simply replace the
;;; CATCH with an assignment, because we need to ensure the abnormal branch is actually translated.
;;; This in turn means that even if the first successor of an (assign-)catch has only one predecessor,
;;; it will be the head of a spurious basic block.

(defclass assign-catch-instruction (cleavir-ir:catch-instruction)
  ((%go-index :initarg go-index :reader go-index)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Assign a unique integer index to every instruction
;;; using the stealth mixin - see system.lisp
;;;

#+stealth-gids
(defun assign-mir-instruction-datum-ids (top)
  (let ((id 1)
	(datums (make-hash-table)))
    (cleavir-ir:map-instructions 
     (lambda (instr)
       #+stealth-gids (setf (clasp-cleavir:instruction-gid instr) (incf id))
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
