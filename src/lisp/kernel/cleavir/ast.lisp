(in-package :clasp-cleavir-ast)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SETF-FDEFINITION-AST.
;;;
;;; This AST is generated from a reference to a global SETF function.

(defclass setf-fdefinition-ast (cleavir-ast:fdefinition-ast)
  ())

(defun make-setf-fdefinition-ast (name-ast info)
  (make-instance 'setf-fdefinition-ast :name-ast name-ast :info info))

(cleavir-io:define-save-info setf-fdefinition-ast)

(defmethod cleavir-ast:children ((ast setf-fdefinition-ast))
  (list (cleavir-ast:name-ast ast)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NAMED-FUNCTION-AST
;;;
;;; This AST is a subclass of FUNCTION-AST. It is used to pass the LAMBDA-NAME declaration
;;; down to the HIR->MIR.

(defclass named-function-ast (cleavir-ast:function-ast)
  ((%lambda-name :initarg :lambda-name :initform "lambda-ast" :reader lambda-name)))

(cleavir-io:define-save-info named-function-ast
    (:lambda-name lambda-name))

(defmethod cleavir-ast-graphviz::label ((ast named-function-ast))
  (with-output-to-string (s)
    (format s "named-function (~a)" (lambda-name ast))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class THROW-AST
;;;
;;; This AST is used to represent a THROW

(defclass throw-ast (cleavir-ast:ast)
  ((%tag-ast :initarg :tag-ast :reader tag-ast)
   (%result-ast :initarg :result-ast :reader result-ast)))

(defun make-throw-ast (tag-ast result-ast)
  (make-instance 'throw-ast
    :tag-ast tag-ast
    :result-ast result-ast))

(cleavir-io:define-save-info throw-ast
  (:tag-ast tag-ast)
  (:result-ast result-ast))

(defmethod cleavir-ast:children ((ast throw-ast))
  (list (tag-ast ast) (result-ast ast)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DEBUG-MESSAGE-AST
;;;
;;; This AST is used to represent a debugging message inserted into the generated code.

(defclass debug-message-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%debug-message :initarg :debug-message  :accessor debug-message)))

(cleavir-io:define-save-info debug-message-ast
    (:debug-message debug-message))

(defmethod cleavir-ast-graphviz::label ((ast debug-message-ast))
  (with-output-to-string (s)
    (format s "debug-message (~a)" (debug-message ast))))

(defmethod cleavir-ast:children ((ast debug-message-ast)) nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BASE-FOREIGN-CALL-AST
;;;
;;; This AST is used to represent a call to an intrinsic function inserted into the generated code.

(defclass base-foreign-call-ast (cleavir-ast:ast)
  ((%foreign-types :initarg :foreign-types :accessor foreign-types)
   (%argument-asts :initarg :argument-asts :reader argument-asts)))

(cleavir-io:define-save-info base-foreign-call-ast
  (:foreign-types foreign-types)
  (:argument-asts argument-asts))

(defmethod cleavir-ast-graphviz::label ((ast base-foreign-call-ast))
  (with-output-to-string (s)
    (format s "base-foreign-call ~a" (foreign-types ast))))

(defmethod cleavir-ast:children ((ast base-foreign-call-ast))
  (argument-asts ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class MULTIPLE-VALUE-FOREIGN-CALL-AST
;;;
;;; This AST is used to represent a call to an intrinsic function inserted into the generated code.

(defclass multiple-value-foreign-call-ast (base-foreign-call-ast)
  ((%function-name :initarg :function-name  :accessor function-name)))

(cleavir-io:define-save-info multiple-value-foreign-call-ast
    (:function-name function-name))

(defmethod cleavir-ast-graphviz::label ((ast multiple-value-foreign-call-ast))
  (with-output-to-string (s)
    (format s "multiple-value-foreign-call (~a)" (function-name ast))))

(defmethod cleavir-ast:children ((ast multiple-value-foreign-call-ast))
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FOREIGN-call-AST
;;;
;;; This AST is used to represent a call to a named foreign function
;;;   inserted into the generated code.

(defclass foreign-call-ast (base-foreign-call-ast cleavir-ast:one-value-ast-mixin)
  ((%function-name :initarg :function-name :accessor function-name)))

(cleavir-io:define-save-info foreign-call-ast
    (:function-name function-name))

(defmethod cleavir-ast-graphviz::label ((ast foreign-call-ast))
  (with-output-to-string (s)
    (format s "foreign-call (~a)" (function-name ast))))

(defmethod cleavir-ast:children ((ast foreign-call-ast))
  (argument-asts ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class foreign-call-pointer-AST
;;;
;;; This AST is used to represent a call to an pointer to a function inserted into the generated code.

(defclass foreign-call-pointer-ast (base-foreign-call-ast cleavir-ast:one-value-ast-mixin)
  ())

(cleavir-io:define-save-info foreign-call-pointer-ast )

(defmethod cleavir-ast-graphviz::label ((ast foreign-call-pointer-ast))
  (with-output-to-string (s)
    (format s "foreign-call-pointer")))

(defmethod cleavir-ast:children ((ast foreign-call-pointer-ast))
  (argument-asts ast))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class PRECALC-VECTOR-FUNCTION-AST
;;;
;;; This AST is a subclass of FUNCTION-AST. It is used when an AST
;;; is transformed by hoisting all the LOAD-TIME-VALUE-ASTs in the tree
;;; by turning them into PRECALC-VALUE-AST that are also required
;;; arguments of the PRECALC-VECTOR-FUNCTION-AST.
;;;
;;; This AST class supplies a slot that contains a list of the forms
;;; that were contained in the LOAD-TIME-VALUE-ASTs. In order to
;;; evaluate the original AST, the transformed AST must be called with
;;; two vectors that are filled by evaluating those forms and putting
;;; their results (symbols and values) into the presym-vector and preval-vector
;;; calc-value-vector and passing that as arguments.
;;


(defclass precalc-vector-function-ast (cleavir-ast:top-level-function-ast)
  ((%precalc-asts :initarg :precalc-asts :reader precalc-asts)))

(defun make-precalc-vector-function-ast (body-ast precalc-asts forms policy)
  (make-instance 'precalc-vector-function-ast
		 :body-ast body-ast
		 :lambda-list nil
		 :precalc-asts precalc-asts
		 :forms forms
                 :policy policy))

(cleavir-io:define-save-info precalc-vector-function-ast
    (:precalc-asts precalc-asts))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class PRECALC-VALUE-REFERENCE-AST
;;;
;;; This class represents a reference to a value that is precalculated
;;; at load-time (COMPILE-FILE) or compile-time (COMPILE) and placed into
;;; a LoadTimeValue object that is passed to the function.
;;;

(defclass precalc-value-reference-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin cleavir-ast:side-effect-free-ast-mixin) 
  ((%ref-index :initarg :index :accessor precalc-value-reference-index)
   (%original-object :initarg :original-object :accessor precalc-value-reference-ast-original-object)))


(cleavir-io:define-save-info precalc-value-reference-ast
  (:index precalc-value-reference-index)
  (:original-object precalc-value-reference-ast-original-object))

(defmethod cleavir-ast:children ((ast precalc-value-reference-ast))
  nil)


(defun escaped-string (str)
  (with-output-to-string (s) (loop for c across str do (when (member c '(#\\ #\")) (princ #\\ s)) (princ c s))))

(defmethod cleavir-ast-graphviz::label ((ast precalc-value-reference-ast))
  (with-output-to-string (s)
    (format s "precalc-val-ref(~a) ; " (precalc-value-reference-index ast))
    (let ((original-object (escaped-string (format nil "~s" (precalc-value-reference-ast-original-object ast)))))
      (if (> (length original-object) 10)
	  (format s "~a..." (subseq original-object 0 10))
	  (princ original-object s)))))




(defun generate-new-precalculated-value-index (form read-only-p)
  "Generates code for the form that places the result into a precalculated-vector and returns the precalculated-vector index.
If this form has already been precalculated then just return the precalculated-value index"
  (cond
    ((and (consp form) (eq (first form) 'QUOTE))
     (let* ((constant (cadr form))
            (constant-index (clasp-cleavir:%literal-index constant read-only-p)))
       constant-index))
    ((symbolp form) (clasp-cleavir:%literal-index form read-only-p))
    ((constantp form) (clasp-cleavir:%literal-index form read-only-p))
    (t
     ;; Currently read-only-p is ignored from here on
     ;; OPTIMIZE - an optimization would be to coalesce
     ;; the forms and their results
     (if (eq cleavir-generate-ast:*compiler* 'cl:compile-file)
         ;; COMPLE-FILE will generate a function for the form in the Module
         ;; and arrange for it's evaluation at load time
         ;; and to make its result available as a value
         #+(or)(let* ((index (literal:new-table-index))
                      (value (literal:with-ltv (literal:compile-load-time-value-thunk form))))
                 (literal:evaluate-function-into-load-time-value index ltv-func)
                 index)
         (literal:with-load-time-value (literal:compile-load-time-value-thunk form))
         ;; COMPILE on the other hand evaluates the form and puts its
         ;; value in the run-time environment
         (let ((value (eval form)))
           (cmp:codegen-rtv nil value))))))


(defun find-load-time-value-asts (ast)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((traverse (ast parent)
               (declare (core:lambda-name traverse))
	       (unless (gethash ast table)
		 (setf (gethash ast table) t)
		 (if (typep ast 'cleavir-ast:load-time-value-ast)
		     (list (list ast parent))
		     (let ((children (cleavir-ast:children ast)))
		       (reduce #'append
			       (mapcar (lambda (child) (funcall #'traverse child ast)) children)
			       :from-end t))))))
      (traverse ast nil))))

(defun hoist-load-time-value (ast)
  (let* ((load-time-value-asts (find-load-time-value-asts ast))
	 (forms (mapcar (lambda (ast-parent)
                          (cleavir-ast:form (first ast-parent)))
                        load-time-value-asts)))
    (loop for (ast parent) in load-time-value-asts
       do (change-class ast 'precalc-value-reference-ast ; 'cleavir-ast:t-aref-ast
                        :index (generate-new-precalculated-value-index
                                (cleavir-ast:form ast) (cleavir-ast:read-only-p ast))
                        :original-object (cleavir-ast:form ast)))
    (clasp-cleavir-ast:make-precalc-vector-function-ast
     ast (mapcar #'first load-time-value-asts) forms (cleavir-ast:policy ast))))




