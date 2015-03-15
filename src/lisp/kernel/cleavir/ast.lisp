(in-package :clasp-cleavir-ast)




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




(defmethod cleavir-ast-graphviz:label ((ast named-function-ast))
  (with-output-to-string (s)
    (format s "named-function (~a)" (lambda-name ast))))



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

(defun make-precalc-vector-function-ast (body-ast precalc-value-name precalc-asts forms)
  (make-instance 'precalc-vector-function-ast
		 :body-ast body-ast
		 :lambda-list (list precalc-value-name) ;; symbol, value vectors are args
		 :precalc-asts precalc-asts
		 :forms forms))

(cleavir-io:define-save-info precalc-vector-function-ast
    (:precalc-asts precalc-asts))


#||(defmethod clavir-ast-graphviz:label ((ast precalc-vector-function-ast))
  (with-output-to-string (s)
    (format s "precalc-vec-fn (~a ~a)" 
||#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class PRECALC-SYMBOL-REFERENCE-AST
;;;
;;; This class represents a reference to a symbol that is precalculated
;;; at load-time (COMPILE-FILE) or compile-time (COMPILE) and placed into
;;; a LoadTimeValue object that is passed to the function.
;;;

(defclass precalc-symbol-reference-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin cleavir-ast:side-effect-free-ast-mixin) 
  ((%precalc-vector-ast :initarg :precalc-vector-ast :accessor precalc-symbol-reference-vector-ast)
   (%ref-index :initarg :index :accessor precalc-symbol-reference-index)
   (%original-object :initarg :original-object :accessor precalc-symbol-reference-ast-original-object)))

(cleavir-io:define-save-info precalc-symbol-reference-ast
    (:precalc-vector-ast precalc-symbol-reference-vector-ast)
  (:index precalc-symbol-reference-index)
  (:original-object precalc-symbol-reference-ast-original-object))

  
(defmethod cleavir-ast:children ((ast precalc-symbol-reference-ast))
  (list (precalc-symbol-reference-vector-ast ast)))


(defun escaped-string (str)
  (with-output-to-string (s) (loop for c across str do (when (member c '(#\\ #\")) (princ #\\ s)) (princ c s))))

(defmethod cleavir-ast-graphviz:label ((ast precalc-symbol-reference-ast))
  (with-output-to-string (s)
    (format s "precalc-sym-ref(~a) ; " (precalc-symbol-reference-index ast))
    (let ((original-object (escaped-string (format nil "~s" (precalc-symbol-reference-ast-original-object ast)))))
      (if (> (length original-object) 10)
	  (format s "~a..." (subseq original-object 0 10))
	  (princ original-object s)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class PRECALC-VALUE-REFERENCE-AST
;;;
;;; This class represents a reference to a value that is precalculated
;;; at load-time (COMPILE-FILE) or compile-time (COMPILE) and placed into
;;; a LoadTimeValue object that is passed to the function.
;;;

(defclass precalc-value-reference-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin cleavir-ast:side-effect-free-ast-mixin) 
  ((%precalc-vector-ast :initarg :precalc-vector-ast :accessor precalc-value-reference-vector-ast)
   (%ref-index :initarg :index :accessor precalc-value-reference-index)
   (%original-object :initarg :original-object :accessor precalc-value-reference-ast-original-object)))


(cleavir-io:define-save-info precalc-symbol-reference-ast
    (:precalc-vector-ast precalc-value-reference-vector-ast)
  (:index precalc-value-reference-index)
  (:original-object precalc-value-reference-ast-original-object))

(defmethod cleavir-ast:children ((ast precalc-value-reference-ast))
  (list (precalc-value-reference-vector-ast ast)))


(defun escaped-string (str)
  (with-output-to-string (s) (loop for c across str do (when (member c '(#\\ #\")) (princ #\\ s)) (princ c s))))

(defmethod cleavir-ast-graphviz:label ((ast precalc-value-reference-ast))
  (with-output-to-string (s)
    (format s "precalc-val-ref(~a) ; " (precalc-value-reference-index ast))
    (let ((original-object (escaped-string (format nil "~s" (precalc-value-reference-ast-original-object ast)))))
      (if (> (length original-object) 10)
	  (format s "~a..." (subseq original-object 0 10))
	  (princ original-object s)))))





(defun generate-new-precalculated-symbol-index (ast)
  "Generates code for the form that places the result into a precalculated-vector and returns the precalculated-vector index.
If this form has already been precalculated then just return the precalculated-value index"
  (let* ((form (cleavir-ast:form ast))
	 (read-only-p (cleavir-ast:read-only-p ast))
	 (symbol (second form))
	 (symbol-index (cmp:codegen-symbol nil symbol)))
;;    (format t "    generate-new-precalculated-symbol-index for: ~s  index: ~a~%" symbol symbol-index)
    (when (< symbol-index 0)
      (error "There is a problem with the symbol index: ~a" symbol-index))
    symbol-index))

(defun generate-new-precalculated-value-index (ast)
  "Generates code for the form that places the result into a precalculated-vector and returns the precalculated-vector index.
If this form has already been precalculated then just return the precalculated-value index"
  (let ((form (cleavir-ast:form ast))
	(read-only-p (cleavir-ast:read-only-p ast)))
    (cond
      ((and (consp form) (eq (first form) 'QUOTE))
       (let* ((constant (cadr form))
	      (constant-index (cmp:codegen-literal nil constant)))
	 constant-index))
      ((symbolp form)
       (cmp:codegen-literal nil form))
      ((constantp form)
       (cmp:codegen-literal nil form))
      (t
       (multiple-value-bind (index fn)
	   (cmp:compile-ltv-thunk "ltv-literal" form nil)
	 index))
      )))


(defun find-load-time-value-asts (ast)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((traverse (ast parent)
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
	 (forms (mapcar (lambda (ast-parent) (cleavir-ast:form (first ast-parent))) load-time-value-asts))
	 (precalc-lexical-ast (cleavir-ast:make-lexical-ast (gensym "precalc"))))
    (loop for (ast parent) in load-time-value-asts
       do (if (typep parent '(or cleavir-ast:fdefinition-ast cleavir-ast:symbol-value-ast))
	      (change-class ast 'precalc-symbol-reference-ast ; 'cleavir-ast:t-aref-ast
			    :precalc-vector-ast precalc-lexical-ast
			    :index (generate-new-precalculated-symbol-index ast)
			    :original-object (cleavir-ast:form ast))
	      (change-class ast 'precalc-value-reference-ast ; 'cleavir-ast:t-aref-ast
			    :precalc-vector-ast precalc-lexical-ast
			    :index (generate-new-precalculated-value-index ast)
			    :original-object (cleavir-ast:form ast))))
    (clasp-cleavir-ast:make-precalc-vector-function-ast
     ast precalc-lexical-ast (mapcar #'first load-time-value-asts) forms)))





