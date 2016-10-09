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
;;; Class INTRINSIC-CALL-AST
;;;
;;; This AST is used to represent a call to an intrinsic function inserted into the generated code.

(defclass intrinsic-call-ast (cleavir-ast:ast)
  ((%function-name :initarg :function-name  :accessor function-name)
   (%argument-asts :initarg :argument-asts :reader argument-asts)))

(cleavir-io:define-save-info intrinsic-call-ast
    (:function-name function-name)
  (:argument-asts argument-asts))

(defmethod cleavir-ast-graphviz::label ((ast intrinsic-call-ast))
  (with-output-to-string (s)
    (format s "intrinsic-call (~a)" (function-name ast))))

(defmethod cleavir-ast:children ((ast intrinsic-call-ast))
  (argument-asts ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FOREIGN-FUNCALL-AST
;;;
;;; This AST is used to represent a call to a named foreign function
;;;   inserted into the generated code.

(defclass foreign-funcall-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%function-name :initarg :function-name :accessor function-name)
   (%argument-asts :initarg :argument-asts :reader argument-asts)))

(cleavir-io:define-save-info foreign-funcall-ast
    (:function-name function-name)
  (:argument-asts argument-asts))

(defmethod cleavir-ast-graphviz::label ((ast foreign-funcall-ast))
  (with-output-to-string (s)
    (format s "foreign-funcall (~a)" (function-name ast))))

(defmethod cleavir-ast:children ((ast foreign-funcall-ast))
  (argument-asts ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class foreign-funcall-pointer-AST
;;;
;;; This AST is used to represent a call to an pointer to a function inserted into the generated code.

(defclass foreign-funcall-pointer-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%argument-asts :initarg :argument-asts :reader argument-asts)))

(cleavir-io:define-save-info foreign-funcall-pointer-ast
  (:argument-asts argument-asts))

(defmethod cleavir-ast-graphviz::label ((ast foreign-funcall-pointer-ast))
  (with-output-to-string (s)
    (format s "foreign-funcall-pointer")))

(defmethod cleavir-ast:children ((ast foreign-funcall-pointer-ast))
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

(defun make-precalc-vector-function-ast (body-ast precalc-asts forms)
  (make-instance 'precalc-vector-function-ast
		 :body-ast body-ast
		 :lambda-list nil
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
  ((%ref-index :initarg :index :accessor precalc-symbol-reference-index)
   (%original-object :initarg :original-object :accessor precalc-symbol-reference-ast-original-object)))

(cleavir-io:define-save-info precalc-symbol-reference-ast
  (:index precalc-symbol-reference-index)
  (:original-object precalc-symbol-reference-ast-original-object))

  
(defmethod cleavir-ast:children ((ast precalc-symbol-reference-ast))
  nil)


(defun escaped-string (str)
  (with-output-to-string (s) (loop for c across str do (when (member c '(#\\ #\")) (princ #\\ s)) (princ c s))))

(defmethod cleavir-ast-graphviz::label ((ast precalc-symbol-reference-ast))
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
       (if (eq cleavir-generate-ast:*compiler* 'cl:compile-file)
           ;; COMPLE-FILE will generate a function for the form in the Module
           ;; and arrange for it's evaluation at load time
           (multiple-value-bind (index fn)
               (cmp:compile-ltv-thunk "ltv-literal" form nil)
             (let ()
               (cmp:with-ltv-function-codegen (result ltv-env)
                 (cmp:irc-intrinsic "invokeTopLevelFunction" 
                                    result 
                                    fn 
                                    (cmp:irc-renv ltv-env)
                                    (cmp:jit-constant-unique-string-ptr "top-level")
                                    cmp:*gv-source-file-info-handle*
                                    (cmp:irc-size_t-*current-source-pos-info*-filepos)
                                    (cmp:irc-size_t-*current-source-pos-info*-lineno)
                                    (cmp:irc-size_t-*current-source-pos-info*-column)
                                    cmp:*load-time-value-holder-global-var*
                                    )))
             index)
           ;; COMPILE on the other hand evaluates the form and puts its
           ;; value in the run-time environment
           (let ((value (eval form)))
             (cmp:codegen-rtv nil value nil))))
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
	 (forms (mapcar (lambda (ast-parent) (cleavir-ast:form (first ast-parent))) load-time-value-asts)))
    (loop for (ast parent) in load-time-value-asts
       do (cond
	    ((typep parent '(or cleavir-ast:fdefinition-ast setf-fdefinition-ast cleavir-ast:symbol-value-ast))
	     (change-class ast 'precalc-symbol-reference-ast ; 'cleavir-ast:t-aref-ast
			   :index (generate-new-precalculated-symbol-index ast)
			   :original-object (cleavir-ast:form ast)))
	    (t (change-class ast 'precalc-value-reference-ast ; 'cleavir-ast:t-aref-ast
			     :index (generate-new-precalculated-value-index ast)
			     :original-object (cleavir-ast:form ast)))))
    (clasp-cleavir-ast:make-precalc-vector-function-ast
     ast (mapcar #'first load-time-value-asts) forms)))




