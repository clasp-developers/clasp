(in-package :clasp-cleavir-hir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction DEBUG-MESSAGE-INSTRUCTION
;;;
;;; This instruction is an DEBUG-MESSAGE-INSTRUCTION that prints a message at runtime.

(defclass debug-message-instruction (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ((%debug-message :initarg :debug-message :accessor debug-message)))

(defmethod cleavir-ir-graphviz:label ((instr debug-message-instruction))
  (with-output-to-string (s)
    (format s "debug-message(~a)" (debug-message instr))))

(defmethod cleavir-ir:clone-initargs append ((instruction debug-message-instruction))
  (list :debug-message (debug-message instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction DEBUG-BREAK-INSTRUCTION
;;;
;;; This instruction is an DEBUG-BREAK-INSTRUCTION that invokes the debugger

(defclass debug-break-instruction (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ())

(defmethod cleavir-ir-graphviz:label ((instr debug-break-instruction))
  (with-output-to-string (s)
    (format s "debug-break")))

(defmethod cleavir-ir:clone-initargs append ((instruction debug-break-instruction))
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction multiple-value-foreign-CALL-INSTRUCTION
;;;
;;; Calls a foreign function (designated by its name, a string) and receives its result as values.

(defclass multiple-value-foreign-call-instruction (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ((%function-name :initarg :function-name :accessor function-name)))

(defmethod cleavir-ir-graphviz:label ((instr multiple-value-foreign-call-instruction))
  (with-output-to-string (s)
    (format s "multiple-value-foreign-call(~a)" (function-name instr))))

(defmethod cleavir-ir:clone-initargs append ((instruction multiple-value-foreign-call-instruction))
  (list :function-name (function-name instruction)))

(defmethod make-multiple-value-foreign-call-instruction
    (function-name inputs outputs &optional (successor nil successor-p))
  (make-instance 'multiple-value-foreign-call-instruction
                 :function-name function-name
                 :inputs inputs
                 :outputs outputs
                 :successors (if successor-p (list successor) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FOREIGN-call-INSTRUCTION
;;;
;;; This instruction is an FOREIGN-call-INSTRUCTION that prints a message

(defclass foreign-call-instruction (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ((%foreign-types :initarg :foreign-types :accessor foreign-types)
   (%function-name :initarg :function-name :accessor function-name)))

(defmethod cleavir-ir-graphviz:label ((instr foreign-call-instruction))
  (with-output-to-string (s)
    (format s "foreign-call(~a)" (function-name instr))))

(defmethod cleavir-ir:clone-initargs append ((instruction foreign-call-instruction))
  (list :foreign-types (foreign-types instruction)
        :foreign-name (foreign-name instruction)))

(defmethod make-foreign-call-instruction
    (foreign-types function-name inputs outputs &optional (successor nil successor-p))
  (make-instance 'foreign-call-instruction
                 :function-name function-name
                 :foreign-types foreign-types
                 :inputs inputs
                 :outputs outputs
                 :successors (if successor-p (list successor) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction foreign-call-pointer-INSTRUCTION
;;;
;;; This instruction is an foreign-call-pointer-INSTRUCTION that prints a message

(defclass foreign-call-pointer-instruction (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ((%foreign-types :initarg :foreign-types :accessor foreign-types)))

(defmethod cleavir-ir-graphviz:label ((instr foreign-call-pointer-instruction))
  (with-output-to-string (s)
    (format s "foreign-call-pointer")))

(defmethod cleavir-ir:clone-initargs append ((instruction foreign-call-pointer-instruction))
  (list :foreign-types (foreign-types instruction)))

(defmethod make-foreign-call-pointer-instruction
    (foreign-types inputs outputs &optional (successor nil successor-p))
  (make-instance 'foreign-call-pointer-instruction
                 :foreign-types foreign-types
                 :inputs inputs
                 :outputs outputs
                 :successors (if successor-p (list successor) '())))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction VECTOR-LENGTH-INSTRUCTION
;;;
;;; This instruction gets the length of a vector, as CL:LENGTH.

(defclass vector-length-instruction (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ())

(defmethod cleavir-ir-graphviz:label ((instr vector-length-instruction))
  "vlength")

(defun make-vector-length-instruction (input output &optional (successor nil successor-p))
  (make-instance 'vector-length-instruction
                 :inputs (list input)
                 :outputs (list output)
                 :successors (if successor-p (list successor) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction DISPLACEMENT-INSTRUCTION
;;;
;;; Get the actual _Data of an mdarray.

(defclass displacement-instruction (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ())

(defmethod cleavir-ir-graphviz:label ((instr displacement-instruction))
  "displacement")

(defun make-displacement-instruction (input output &optional (successor nil successor-p))
  (make-instance 'displacement-instruction
                 :inputs (list input)
                 :outputs (list output)
                 :successors (if successor-p (list successor) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction DISPLACED-INDEX-OFFSET-INSTRUCTION
;;;
;;; Get the actual _DisplacedIndexOffset of an mdarray.

(defclass displaced-index-offset-instruction (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ())

(defmethod cleavir-ir-graphviz:label ((instr displaced-index-offset-instruction))
  "d-offset")

(defun make-displaced-index-offset-instruction (input output &optional (successor nil successor-p))
  (make-instance 'displaced-index-offset-instruction
                 :inputs (list input)
                 :outputs (list output)
                 :successors (if successor-p (list successor) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction ARRAY-TOTAL-SIZE-INSTRUCTION
;;;
;;; Get the _ArrayTotalSize of an mdarray.

(defclass array-total-size-instruction (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ())

(defmethod cleavir-ir-graphviz:label ((instr array-total-size-instruction))
  "ATS")

(defun make-array-total-size-instruction (input output &optional (successor nil successor-p))
  (make-instance 'array-total-size-instruction
                 :inputs (list input)
                 :outputs (list output)
                 :successors (if successor-p (list successor) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction ARRAY-RANK-INSTRUCTION
;;;
;;; Get the rank of an mdarray.

(defclass array-rank-instruction (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ())

(defmethod cleavir-ir-graphviz:label ((instr array-rank-instruction))
  "rank")

(defun make-array-rank-instruction (input output &optional (successor nil successor-p))
  (make-instance 'array-rank-instruction
                 :inputs (list input)
                 :outputs (list output)
                 :successors (if successor-p (list successor) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction ARRAY-DIMENSION-INSTRUCTION
;;;
;;; Get a dimension of an mdarray.

(defclass array-dimension-instruction (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ())

(defmethod cleavir-ir-graphviz:label ((instr array-dimension-instruction))
  "AD")

(defun make-array-dimension-instruction (mdarray axis output &optional (successor nil successor-p))
  (make-instance 'array-dimension-instruction
                 :inputs (list mdarray axis)
                 :outputs (list output)
                 :successors (if successor-p (list successor) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction BIND-VA-LIST-INSTRUCTION
;;;
;;; Sort of like destructuring-bind, but with a va-list
;;; instead of a list (thus why it's a special operator),
;;; and only allowing ordinary lambda lists.

(defclass bind-va-list-instruction (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ((%lambda-list :initarg :lambda-list :accessor cleavir-ir:lambda-list)))

(defmethod cleavir-ir-graphviz:label ((instr bind-va-list-instruction))
  )

(defmethod cleavir-ir:clone-initargs append ((instruction bind-va-list-instruction))
  (list :lambda-list (cleavir-ir:lambda-list instruction)))

(defun make-bind-va-list-instruction (lambda-list va-list &optional (successor nil successor-p))
  (make-instance 'bind-va-list-instruction
                 :lambda-list lambda-list
                 :inputs (list va-list)
                 ;; copied from cleavir-ir:make-enter-instruction
                 :outputs (loop for item in lambda-list
                                append (cond ((member item lambda-list-keywords) nil)
                                             ((consp item)
                                              (if (= (length item) 3)
                                                  (rest item)
                                                  item))
                                             (t (list item))))
                 :successors (if successor-p (list successor) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction NAMED-ENTER-INSTRUCTION
;;;
;;; This instruction is an ENTER-INSTRUCTION that keeps
;;; track of the lambda-name


(defclass named-enter-instruction (cleavir-ir:enter-instruction)
  ((%lambda-name :initarg :lambda-name :initform "lambda" :accessor lambda-name)
   (%original-lambda-list :initarg :original-lambda-list :initform nil :reader original-lambda-list)
   (%docstring :initarg :docstring :initform nil :reader docstring)))

(defun make-named-enter-instruction
    (lambda-list lambda-name &key (successor nil successor-p) origin original-lambda-list docstring)
  (let ((oe (if successor-p
		(cleavir-ir:make-enter-instruction lambda-list :successor successor :origin origin)
		(cleavir-ir:make-enter-instruction lambda-list :origin origin))))
    (change-class oe 'named-enter-instruction :lambda-name lambda-name
                                              :original-lambda-list original-lambda-list
                                              :docstring docstring)))

(defmethod cleavir-ir-graphviz:label ((instr named-enter-instruction))
  (with-output-to-string (s)
    (format s "named-enter(~a)" (lambda-name instr))))

(defmethod cleavir-ir:clone-initargs append ((instruction named-enter-instruction))
  (list :lambda-name (lambda-name instruction)
        :original-lambda-list (original-lambda-list instruction)
        :docstring (docstring instruction)))

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
  (assert (typep (cleavir-ir:value index-input) 'clasp-cleavir::literal))
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

(defmethod cleavir-ir:clone-initargs append ((instruction precalc-value-instruction))
  (list :original-object (precalc-value-instruction-original-object instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;
;;; Instruction MULTIPLE-VALUE-ONE-FORM-CALL-INSTRUCTION.
;;;
;;; The first input of this instruction is an ordinary lexical
;;; location.  The remaining inputs are of type VALUES-LOCATION, and
;;; each represents multiple values returned from the evaluation of
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; throw-instruction
;;;

(defclass throw-instruction (cleavir-ir:instruction cleavir-ir:no-successors-mixin)
  ((%throw-tag :initform nil :initarg :throw-tag :accessor throw-tag)))


(defun make-throw-instruction (throw-tag)
  (make-instance 'throw-instruction
    :inputs (list throw-tag)))

(defmethod cleavir-ir-graphviz:label ((instr throw-instruction))
  (with-output-to-string (stream)
    (format stream "throw")))

(defmethod cleavir-ir:clone-initargs append ((instruction throw-instruction))
  (list :throw-tag (throw-tag instruction)))

(defmethod cl:print-object ((instr throw-instruction) stream)
  (format stream "#<throw>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod cleavir-remove-useless-instructions:instruction-may-be-removed-p ((instruction debug-message-instruction)) nil)

(defmethod cleavir-remove-useless-instructions:instruction-may-be-removed-p ((instruction debug-break-instruction)) nil)

;(defmethod cleavir-remove-useless-instructions:instruction-may-be-removed-p ((instruction precalc-value-instruction)) t)

(defmethod cleavir-remove-useless-instructions:instruction-may-be-removed-p ((instruction multiple-value-one-form-call-instruction)) nil)

(defmethod cleavir-remove-useless-instructions:instruction-may-be-removed-p ((instruction setf-fdefinition-instruction)) nil)

(defmethod cleavir-remove-useless-instructions:instruction-may-be-removed-p ((instruction throw-instruction)) nil)


