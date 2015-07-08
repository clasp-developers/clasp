(in-package :clasp-cleavir-hir)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction DEBUG-MESSAGE-INSTRUCTION
;;;
;;; This instruction is an DEBUG-MESSAGE-INSTRUCTION that prints a message


(defclass debug-message-instruction (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ((%debug-message :initarg :debug-message :accessor debug-message)))


(defmethod cleavir-ir-graphviz:label ((instr debug-message-instruction))
  (with-output-to-string (s)
    (format s "debug-message(~a)" (debug-message instr))))



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
;;; Instruction LANDING-PAD-NAMED-ENTER-INSTRUCTION
;;;
;;; This instruction is an ENTER-INSTRUCTION that keeps
;;; track of the lambda-name


(defclass landing-pad-named-enter-instruction (clasp-cleavir-hir:named-enter-instruction)
  ((%landing-pad :initarg :landing-pad :accessor landing-pad)))

(defun make-landing-pad-named-enter-instruction
    (lambda-list lambda-name landing-pad &optional (successor nil successor-p))
  (let ((oe (if successor-p
		(cleavir-ir:make-enter-instruction lambda-list successor)
		(cleavir-ir:make-enter-instruction lambda-list))))
    (change-class oe 'landing-pad-named-enter-instruction :lambda-name lambda-name
		  :landing-pad landing-pad)))


(defmethod cleavir-ir-graphviz:label ((instr landing-pad-named-enter-instruction))
  (with-output-to-string (s)
    (format s "landing-pad-named-enter(~a)" (lambda-name instr))))



(defun frame-holder (enter)
  (or (typep enter 'landing-pad-named-enter-instruction) (error "~a does not have a frame holder"))
  ;; The frame holder is the last output
  (car (last (cleavir-ir:outputs enter))))

(defun (setf frame-holder) (frame-holder enter)
  (or (typep enter 'landing-pad-named-enter-instruction) (error "~a does not have a frame holder"))
  (setf (cleavir-ir:outputs enter) (append (cleavir-ir:outputs enter) (list frame-holder))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction LANDING-PAD-RETURN-INSTRUCTION
;;;
;;; This instruction is an RETURN-INSTRUCTION that keeps
;;; track of the landing-pad


(defclass landing-pad-return-instruction (cleavir-ir:return-instruction)
  ((%landing-pad :initarg :landing-pad :accessor landing-pad)))


(defmethod cleavir-ir-graphviz:label ((instr landing-pad-return-instruction))
  (with-output-to-string (s)
    (format s "landing-pad-return")))


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
    (index-input output &key successor original-object)
  (make-instance 'precalc-symbol-instruction
    :inputs (list index-input)
    :outputs (list output)
    :successors (if (null successor) nil (list successor))
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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction LANDING-PAD-INSTRUCTION.
;;;
#||
(defclass landing-pad-instruction (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ((%unwinds :initform nil :initarg :unwinds :accessor unwinds)
   (%basic-block :initarg :basic-block :accessor basic-block)
   (%frame :initarg :frame :accessor :frame)))

(defun make-landing-pad-instruction
    (output &key successor unwinds)
  (make-instance 'landing-pad-instruction
    :inputs nil
    :outputs (list output)
    :successors (if (null successor) nil (list successor))
    :unwinds unwinds))


(defmethod cleavir-ir-graphviz:label ((instr landing-pad-instruction))
  (with-output-to-string (str)
    (format str "landing-pad[")
    (dolist (unwind (unwinds instr))
      (format str "{~a-->~a};" (cc-mir:describe-mir unwind) (cc-mir:describe-mir (first (cleavir-ir:successors unwind)))))
    (format str "]")))
||#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INDEXED-UNWIND-INSTRUCTION
;;;
;;; UWIND instruction that takes an enclosed lexical variable as input
;;; and stores an integer jump-id to represent where to jump to in the
;;; landing-pad
(defclass indexed-unwind-instruction (cleavir-ir:unwind-instruction)
  ((%jump-id :initform nil :initarg :jump-id :accessor jump-id)))


#+(or)(defmethod cleavir-ir-graphviz:label ((instr indexed-unwind-instruction))
  (format t "Label for indexed-unwind-instruction~%")
  (with-output-to-string (stream)
    (format stream "indexed-unwind[~a]" (jump-id instr))))

(defmethod cleavir-ir-graphviz:draw-instruction ((instruction indexed-unwind-instruction) stream)
  (format stream "   ~a [label = \"indexed-unwind[~a]\"];~%"
	  (cleavir-ir-graphviz::instruction-id instruction) (jump-id instruction))
  (format stream "  ~a -> ~a [color = pink, style = dashed];~%"
	  (cleavir-ir-graphviz::instruction-id instruction)
	  (gethash (cleavir-ir:invocation instruction) cleavir-ir-graphviz::*instruction-table*)))


(defmethod cl:print-object ((instr indexed-unwind-instruction) stream)
  (format stream "#<indexed-unwind[~a]>" (jump-id instr)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; throw-instruction
;;;

(defclass throw-instruction (cleavir-ir:instruction cleavir-ir:no-successors-mixin)
  ((%throw-tag :initform nil :initarg :throw-tag :accessor throw-tag)))


(defun make-throw-instruction
    (throw-tag &key successor)
  (make-instance 'throw-instruction
    :inputs (list throw-tag)
    :outputs ()
    :successors (if (null successor) nil (list successor))))

(defmethod cleavir-ir-graphviz:label ((instr throw-instruction))
  (with-output-to-string (stream)
    (format stream "throw")))


(defmethod cl:print-object ((instr throw-instruction) stream)
  (format stream "#<throw>"))


