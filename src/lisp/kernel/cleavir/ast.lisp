(in-package :clasp-cleavir-ast)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SETF-FDEFINITION-AST.
;;;
;;; This AST is generated from a reference to a global SETF function.

(defclass setf-fdefinition-ast (cleavir-ast:fdefinition-ast)
  ())

(defun make-setf-fdefinition-ast (name-ast)
  (make-instance 'setf-fdefinition-ast :name-ast name-ast))

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
  ((%lambda-name :initarg :lambda-name :initform "lambda-ast" :reader lambda-name)
   (%original-lambda-list :initarg :original-lambda-list :initform nil :reader original-lambda-list)
   (%docstring :initarg :docstring :initform nil :reader docstring)))

(cleavir-io:define-save-info named-function-ast
    (:lambda-name lambda-name)
  (:original-lambda-list original-lambda-list)
  (:docstring docstring))

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

(defun make-throw-ast (tag-ast result-ast &optional origin)
  (make-instance 'throw-ast
    :tag-ast tag-ast
    :result-ast result-ast
    :origin origin))

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
;;; Class DEBUG-BREAK-AST
;;;
;;; This AST is used to represent a debugging break inserted into the generated code.

(defclass debug-break-ast (cleavir-ast:ast cleavir-ast:no-value-ast-mixin)
  ())

(cleavir-io:define-save-info debug-break-ast
    ())

(defmethod cleavir-ast-graphviz::label ((ast debug-break-ast))
  (with-output-to-string (s)
    (format s "debug-break (~a)" (debug-break ast))))

(defmethod cleavir-ast:children ((ast debug-break-ast)) nil)


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

(defmethod cleavir-ast-graphviz::label ((ast foreign-call-pointer-ast))
  (with-output-to-string (s)
    (format s "foreign-call-pointer")))

(defmethod cleavir-ast:children ((ast foreign-call-pointer-ast))
  (argument-asts ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VECTOR-LENGTH-AST
;;;
;;; Represents an operation to get the length of a vector.
;;; If the vector has a fill pointer it returns that,
;;; as the length and fill pointer have the same offset.

(defclass vector-length-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%vector :initarg :vector :accessor vl-ast-vector)))

(cleavir-io:define-save-info vector-length-ast
    (:vector vl-ast-vector))

(defmethod cleavir-ast-graphviz::label ((ast vector-length-ast))
  "vlength")

(defmethod cleavir-ast:children ((ast vector-length-ast))
  (list (vl-ast-vector ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DISPLACEMENT-AST
;;;
;;; Gets the actual underlying array of any mdarray.

(defclass displacement-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%mdarray :initarg :mdarray :accessor displacement-ast-mdarray)))

(cleavir-io:define-save-info displacement-ast
    (:mdarray displacement-ast-mdarray))

(defmethod cleavir-ast-graphviz::label ((ast displacement-ast))
  "displacement")

(defmethod cleavir-ast:children ((ast displacement-ast))
  (list (displacement-ast-mdarray ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DISPLACED-INDEX-OFFSET-AST
;;;
;;; Gets the actual underlying DIO of any mdarray.

(defclass displaced-index-offset-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%mdarray :initarg :mdarray :accessor displaced-index-offset-ast-mdarray)))

(cleavir-io:define-save-info displaced-index-offset-ast
    (:mdarray displaced-index-offset-ast-mdarray))

(defmethod cleavir-ast-graphviz::label ((ast displaced-index-offset-ast))
  "d-offset")

(defmethod cleavir-ast:children ((ast displaced-index-offset-ast))
  (list (displaced-index-offset-ast-mdarray ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DISPLACED-INDEX-OFFSET-AST
;;;
;;; Gets the actual underlying DIO of any mdarray.

(defclass array-total-size-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%mdarray :initarg :mdarray :accessor array-total-size-ast-mdarray)))

(cleavir-io:define-save-info array-total-size-ast
    (:mdarray array-total-size-ast-mdarray))

(defmethod cleavir-ast-graphviz::label ((ast array-total-size-ast))
  "ATS")

(defmethod cleavir-ast:children ((ast array-total-size-ast))
  (list (array-total-size-ast-mdarray ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ARRAY-RANK-AST
;;;
;;; Gets the rank of any mdarray.

(defclass array-rank-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%mdarray :initarg :mdarray :accessor array-rank-ast-mdarray)))

(cleavir-io:define-save-info array-rank-ast
    (:mdarray array-rank-ast-mdarray))

(defmethod cleavir-ast-graphviz::label ((ast array-rank-ast))
  "rank")

(defmethod cleavir-ast:children ((ast array-rank-ast))
  (list (array-rank-ast-mdarray ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ARRAY-DIMENSION-AST
;;;
;;; Gets the dimensions of any mdarray.

(defclass array-dimension-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%mdarray :initarg :mdarray :accessor array-dimension-ast-mdarray)
   (%axis :initarg :axis :accessor array-dimension-ast-axis)))

(cleavir-io:define-save-info array-dimension-ast
    (:mdarray array-dimension-ast-mdarray)
  (:axis array-dimension-ast-axis))

(defmethod cleavir-ast-graphviz::label ((ast array-dimension-ast))
  "AD")

(defmethod cleavir-ast:children ((ast array-dimension-ast))
  (list (array-dimension-ast-mdarray ast)
        (array-dimension-ast-axis ast)))



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

(defun make-precalc-vector-function-ast (body-ast precalc-asts forms policy &key origin)
  (make-instance 'precalc-vector-function-ast
                 :body-ast body-ast
                 :lambda-list nil
                 :precalc-asts precalc-asts
                 :forms forms
                 :policy policy
                 :origin origin))

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
  ((%ref-index :initarg :index :accessor precalc-value-reference-ast-index)
   (%original-object :initarg :original-object :accessor precalc-value-reference-ast-original-object)))


(cleavir-io:define-save-info precalc-value-reference-ast
  (:index precalc-value-reference-ast-index)
  (:original-object precalc-value-reference-ast-original-object))

(defmethod cleavir-ast:children ((ast precalc-value-reference-ast))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BIND-VA-LIST-AST
;;;
;;; Bind variables according to an ordinary lambda list based on a va_list.
;;; A lot like a function-ast, but not actually one because it just binds.

(defclass bind-va-list-ast (cleavir-ast:ast)
  ((%lambda-list :initarg :lambda-list :reader cleavir-ast:lambda-list)
   (%va-list-ast :initarg :va-list :reader va-list-ast)
   (%body-ast :initarg :body-ast :reader cleavir-ast:body-ast)))

(defun make-bind-va-list-ast (lambda-list va-list-ast body-ast &key origin (policy cleavir-ast:*policy*))
  (make-instance 'bind-va-list-ast
    :origin origin :policy policy
    :va-list va-list-ast :body-ast body-ast :lambda-list lambda-list))

(cleavir-io:define-save-info bind-va-list-ast
    (:lambda-list cleavir-ast:lambda-list)
  (:va-list va-list-ast)
  (:body-ast cleavir-ast:body-ast))

(defmethod cleavir-ast:children ((ast bind-va-list-ast))
  (list* (va-list-ast ast)
         (cleavir-ast:body-ast ast)
         (loop for entry in (cleavir-ast:lambda-list ast)
               append (cond ((symbolp entry) '())
                            ((consp entry)
                             (if (= (length entry) 2)
                                 entry
                                 (cdr entry)))
                            (t (list entry))))))


(defun escaped-string (str)
  (with-output-to-string (s) (loop for c across str do (when (member c '(#\\ #\")) (princ #\\ s)) (princ c s))))

(defmethod cleavir-ast-graphviz::label ((ast precalc-value-reference-ast))
  (with-output-to-string (s)
    (format s "precalc-val-ref(~a) ; " (clasp-cleavir:literal-label (precalc-value-reference-ast-index ast)))
    (let ((original-object (escaped-string (format nil "~s" (precalc-value-reference-ast-original-object ast)))))
      (if (> (length original-object) 10)
	  (format s "~a..." (subseq original-object 0 10))
	  (princ original-object s)))))




(defun generate-new-precalculated-value-index (env form read-only-p)
  "Generates code for the form that places the result into a precalculated-vector and returns the precalculated-vector index.
If this form has already been precalculated then just return the precalculated-value index"
  (cond
    ((constantp form env)
     (clasp-cleavir:%literal-index (ext:constant-form-value form env) read-only-p))
    (t
     ;; Currently read-only-p is ignored from here on
     ;; OPTIMIZE - an optimization would be to coalesce
     ;; the forms and their results
     (if (eq cleavir-generate-ast:*compiler* 'cl:compile-file)
         ;; COMPLE-FILE will generate a function for the form in the Module
         ;; and arrange for it's evaluation at load time
         ;; and to make its result available as a value
         (let* ((index (literal:with-load-time-value-cleavir
                           (clasp-cleavir::compile-form form env)))
                (result (make-instance 'clasp-cleavir:arrayed-literal :value form :index index)))
           (check-type result clasp-cleavir:literal)
           result)
         ;; COMPILE on the other hand evaluates the form and puts its
         ;; value in the run-time environment.
         ;; We use cleavir-env:eval rather than cclasp-eval-with-env so that it works
         ;; more correctly with alternate global environments.
         (let* ((value (cleavir-env:eval form env env))
                (index (cmp:codegen-rtv nil value))
                (result (make-instance 'clasp-cleavir:arrayed-literal :value form :index index :literal-name "NIL")))
           (check-type result clasp-cleavir:literal)
           result)))))


(defun find-load-time-value-asts (ast)
  (let ((table (make-hash-table :test #'eq :rehash-size 4.0 :rehash-threshold 1.0)))
    (labels ((traverse (ast parent)
               (declare (core:lambda-name traverse))
	       (unless (gethash ast table)
		 (setf (gethash ast table) t)
		 (if (typep ast 'cleavir-ast:load-time-value-ast)
		     (list (list ast parent))
		     (let ((children (cleavir-ast:children ast)))
		       (reduce #'append
			       (mapcar (lambda (child)
                                         (declare (core:lambda-name traverse.lambda))
                                         (funcall #'traverse child ast))
                                       children)
			       :from-end t))))))
      (traverse ast nil))))

(defun hoist-load-time-value (ast env)
  (let* ((load-time-value-asts (find-load-time-value-asts ast))
         (forms (mapcar (lambda (ast-parent)
                          (cleavir-ast:form (first ast-parent)))
                        load-time-value-asts)))
    (loop for (ast parent) in load-time-value-asts
          for index = (let ((index (generate-new-precalculated-value-index env (cleavir-ast:form ast) (cleavir-ast:read-only-p ast))))
                        (check-type index clasp-cleavir:literal)
                        index)
       do (change-class ast 'precalc-value-reference-ast ; 'cleavir-ast:t-aref-ast
                        :index index
                        :original-object (cleavir-ast:form ast)))
    (clasp-cleavir-ast:make-precalc-vector-function-ast
     ast (mapcar #'first load-time-value-asts) forms (cleavir-ast:policy ast)
     :origin (cleavir-ast:origin ast))))




