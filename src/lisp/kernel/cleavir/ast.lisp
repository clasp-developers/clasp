
(in-package :clasp-cleavir-ast)


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

(defun make-precalc-vector-function-ast (body-ast precalc-symbol-vector-name precalc-value-vector-name precalc-asts forms)
  (make-instance 'precalc-vector-function-ast
		 :body-ast body-ast
		 :lambda-list (list precalc-symbol-vector-name precalc-value-vector-name) ;; symbol, value vectors are args
		 :precalc-asts precalc-asts
		 :forms forms))

(cleavir-io:define-save-info precalc-vector-function-ast
    (:precalc-value-asts precalc-value-asts))


#||(defmethod clavir-ast-graphviz:label ((ast precalc-vector-function-ast))
  (with-output-to-string (s)
    (format s "precalc-vec-fn (~a ~a)" 
||#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class PRECALC-REFERENCE-AST
;;;
;;; This class represents a reference to a symbol that is precalc
;;; at load-time (COMPILE-FILE) or compile-time (COMPILE) and placed into
;;; a PRECALC-SYMBOL-VECTOR that is passed to the function.
;;;

(defclass precalc-reference-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin cleavir-ast:side-effect-free-ast-mixin) 
  ((%precalc-vector-ast :initarg :precalc-vector-ast :accessor precalc-vector-ast)
   (%index :initarg :index :accessor precalc-reference-index)
   (%original-object :initarg :original-object :accessor precalc-reference-ast-original-object)))

(defmethod cleavir-ast:children ((ast precalc-reference-ast))
  (list (precalc-vector-ast ast)))

(defmethod cleavir-ast-graphviz:label ((ast precalc-reference-ast))
  (with-output-to-string (s)
    (format s "precalc-ref(~a) ; " (precalc-reference-index ast))
    (let ((original-object (with-output-to-string (dl)
			 (format dl "~s" (precalc-reference-ast-original-object ast)))))
      (if (> (length original-object) 10)
	  (format s "~a..." (subseq original-object 0 10))
	  (princ original-object s)))))






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


(defun generate-new-precalculated-symbol-index (form)
  "Generates code for the form that places the result into a precalculated-vector and returns the precalculated-vector index.
If this form has already been precalculated then just return the precalculated-value index"
  (warn "Finish implementing generate-new-precalculated-symbol-index")
  0)

(defun generate-new-precalculated-value-index (form)
  "Generates code for the form that places the result into a precalculated-vector and returns the precalculated-vector index.
If this form has already been precalculated then just return the precalculated-value index"
  (warn "Finish implementing generate-new-precalculated-value-index")
  0)

(defun hoist-load-time-value (ast)
  (let* ((load-time-value-asts (find-load-time-value-asts ast))
	 (forms (mapcar (lambda (ast-parent) (cleavir-ast:form (first ast-parent))) load-time-value-asts))
	 (precalc-symbols-lexical-ast (cleavir-ast:make-lexical-ast (gensym "presym")))
	 (precalc-values-lexical-ast (cleavir-ast:make-lexical-ast (gensym "preval"))))
    (loop for (ast parent) in load-time-value-asts
       do (if (typep parent '(or cleavir-ast:fdefinition-ast cleavir-ast:symbol-value-ast))
	      (change-class ast 'precalc-reference-ast ; 'cleavir-ast:t-aref-ast
			    :precalc-vector-ast precalc-symbols-lexical-ast
			    :index (generate-new-precalculated-symbol-index (cleavir-ast:form ast))
			    :original-object (cleavir-ast:form ast))
	      (change-class ast 'precalc-reference-ast ; 'cleavir-ast:t-aref-ast
			    :precalc-vector-ast precalc-values-lexical-ast
			    :index (generate-new-precalculated-value-index (cleavir-ast:form ast))
			    :original-object (cleavir-ast:form ast))))
    (clasp-cleavir-ast:make-precalc-vector-function-ast
     ast precalc-symbols-lexical-ast precalc-values-lexical-ast (mapcar #'first load-time-value-asts) forms)))



