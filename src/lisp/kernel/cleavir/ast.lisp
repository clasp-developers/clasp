(in-package :clasp-cleavir-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SETF-FDEFINITION-AST.
;;;
;;; This AST is generated from a reference to a global SETF function.

(defclass setf-fdefinition-ast (cleavir-ast:fdefinition-ast)
  ())

(defun make-setf-fdefinition-ast (name-ast &key origin)
  (make-instance 'setf-fdefinition-ast :name-ast name-ast :origin origin))

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
   (%docstring :initarg :docstring :initform nil :reader docstring)
   ;; We can avoid or dx-allocate the &rest list sometimes- controlled here,
   ;; and set up from declarations in convert-form.lisp.
   ;; NIL indicates the general case (i.e. full heap allocation).
   (%rest-alloc :initarg :rest-alloc :initform nil :reader rest-alloc
                     :type (member nil ignore dynamic-extent))))

(cleavir-io:define-save-info named-function-ast
    (:lambda-name lambda-name)
  (:original-lambda-list original-lambda-list)
  (:docstring docstring)
  (:rest-alloc rest-alloc))

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
    (format s "debug-break")))

(defmethod cleavir-ast:children ((ast debug-break-ast)) nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BASE-FOREIGN-CALL-AST
;;;
;;; This AST is used to represent a call to an intrinsic function inserted into the generated code.

(defclass base-foreign-call-ast (cleavir-ast:ast)
  ((%foreign-types :initarg :foreign-types :accessor foreign-types :initform nil)
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
;;; Class DEFCALLBACK-AST
;;;
;;; This AST is used to represent a callback definition.

(defclass defcallback-ast (cleavir-ast:ast cleavir-ast:no-value-ast-mixin)
  (;; None of these are evaluated and there's a ton of them
   ;; so why bother splitting them up
   (%args :initarg :args :reader defcallback-args)
   (%callee :initarg :callee :reader cleavir-ast:callee-ast)))

(defmethod cleavir-ast:children ((ast defcallback-ast))
  (list (cleavir-ast:callee-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class HEADER-STAMP-CASE-AST
;;;

(defclass header-stamp-case-ast (cleavir-ast:ast)
  ((%stamp-ast :initarg :stamp-ast :reader stamp-ast)
   (%derivable-ast :initarg :derivable-ast :reader derivable-ast)
   (%rack-ast :initarg :rack-ast :reader rack-ast)
   (%wrapped-ast :initarg :wrapped-ast :reader wrapped-ast)
   (%header-ast :initarg :header-ast :reader header-ast)))

(defmethod cleavir-ast:children ((ast header-stamp-case-ast))
  (list (stamp-ast ast) (derivable-ast ast) (rack-ast ast)
        (wrapped-ast ast) (header-ast ast)))

(defun make-header-stamp-case-ast (stamp derivable rack wrapped header &optional origin)
  (make-instance 'header-stamp-case-ast
                 :stamp-ast stamp :derivable-ast derivable :rack-ast rack
                 :wrapped-ast wrapped :header-ast header
                 :origin origin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VECTOR-LENGTH-AST
;;;
;;; Represents an operation to get the length of a vector.
;;; If the vector has a fill pointer it returns that,
;;; as the length and fill pointer have the same offset.

(defclass vector-length-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%vector :initarg :vector :accessor cleavir-ast:arg-ast)))

(cleavir-io:define-save-info vector-length-ast
    (:vector cleavir-ast:arg-ast))

(defmethod cleavir-ast-graphviz::label ((ast vector-length-ast))
  "vlength")

(defmethod cleavir-ast:children ((ast vector-length-ast))
  (list (cleavir-ast:arg-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DISPLACEMENT-AST
;;;
;;; Gets the actual underlying array of any mdarray.

(defclass displacement-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%mdarray :initarg :mdarray :accessor cleavir-ast:arg-ast)))

(cleavir-io:define-save-info displacement-ast
    (:mdarray cleavir-ast:arg-ast))

(defmethod cleavir-ast-graphviz::label ((ast displacement-ast))
  "displacement")

(defmethod cleavir-ast:children ((ast displacement-ast))
  (list (cleavir-ast:arg-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DISPLACED-INDEX-OFFSET-AST
;;;
;;; Gets the actual underlying DIO of any mdarray.

(defclass displaced-index-offset-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%mdarray :initarg :mdarray :accessor cleavir-ast:arg-ast)))

(cleavir-io:define-save-info displaced-index-offset-ast
    (:mdarray cleavir-ast:arg-ast))

(defmethod cleavir-ast-graphviz::label ((ast displaced-index-offset-ast))
  "d-offset")

(defmethod cleavir-ast:children ((ast displaced-index-offset-ast))
  (list (cleavir-ast:arg-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ARRAY-TOTAL-SIZE-AST
;;;
;;; Gets the total size of any mdarray.

(defclass array-total-size-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%mdarray :initarg :mdarray :accessor cleavir-ast:arg-ast)))

(cleavir-io:define-save-info array-total-size-ast
    (:mdarray cleavir-ast:arg-ast))

(defmethod cleavir-ast-graphviz::label ((ast array-total-size-ast))
  "ATS")

(defmethod cleavir-ast:children ((ast array-total-size-ast))
  (list (cleavir-ast:arg-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ARRAY-RANK-AST
;;;
;;; Gets the rank of any mdarray.

(defclass array-rank-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%mdarray :initarg :mdarray :accessor cleavir-ast:arg-ast)))

(cleavir-io:define-save-info array-rank-ast
    (:mdarray cleavir-ast:arg-ast))

(defmethod cleavir-ast-graphviz::label ((ast array-rank-ast))
  "rank")

(defmethod cleavir-ast:children ((ast array-rank-ast))
  (list (cleavir-ast:arg-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ARRAY-DIMENSION-AST
;;;
;;; Gets the dimensions of any mdarray.

(defclass array-dimension-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%mdarray :initarg :mdarray :accessor cleavir-ast:arg1-ast)
   (%axis :initarg :axis :accessor cleavir-ast:arg2-ast)))

(cleavir-io:define-save-info array-dimension-ast
    (:mdarray cleavir-ast:arg1-ast)
  (:axis cleavir-ast:arg2-ast))

(defmethod cleavir-ast-graphviz::label ((ast array-dimension-ast))
  "AD")

(defmethod cleavir-ast:children ((ast array-dimension-ast))
  (list (cleavir-ast:arg1-ast ast)
        (cleavir-ast:arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class HEADER-STAMP-AST

(defclass header-stamp-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%arg :initarg :arg :accessor cleavir-ast:arg-ast)))
(cleavir-io:define-save-info header-stamp-ast (:arg cleavir-ast:arg-ast))
(defmethod cleavir-ast-graphviz::label ((ast header-stamp-ast)) "header-stamp")
(defmethod cleavir-ast:children ((ast header-stamp-ast))
  (list (cleavir-ast:arg-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class RACK-STAMP-AST

(defclass rack-stamp-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%arg :initarg :arg :accessor cleavir-ast:arg-ast)))
(cleavir-io:define-save-info rack-stamp-ast (:arg cleavir-ast:arg-ast))
(defmethod cleavir-ast-graphviz::label ((ast rack-stamp-ast)) "rack-stamp")
(defmethod cleavir-ast:children ((ast rack-stamp-ast))
  (list (cleavir-ast:arg-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class WRAPPED-STAMP-AST

(defclass wrapped-stamp-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%arg :initarg :arg :accessor cleavir-ast:arg-ast)))
(cleavir-io:define-save-info wrapped-stamp-ast (:arg cleavir-ast:arg-ast))
(defmethod cleavir-ast-graphviz::label ((ast wrapped-stamp-ast)) "wrapped-stamp")
(defmethod cleavir-ast:children ((ast wrapped-stamp-ast))
  (list (cleavir-ast:arg-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DERIVABLE-STAMP-AST

(defclass derivable-stamp-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%arg :initarg :arg :accessor cleavir-ast:arg-ast)))
(cleavir-io:define-save-info derivable-stamp-ast (:arg cleavir-ast:arg-ast))
(defmethod cleavir-ast-graphviz::label ((ast derivable-stamp-ast)) "derivable-stamp")
(defmethod cleavir-ast:children ((ast derivable-stamp-ast))
  (list (cleavir-ast:arg-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VASLIST-POP-AST
;;;
;;; Pops an element off a valist.
;;; Doesn't necessarily check that there is an element.

(defclass vaslist-pop-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%arg-ast :initarg :vaslist :reader cleavir-ast:arg-ast)))

(cleavir-io:define-save-info vaslist-pop-ast
    (:vaslist cleavir-ast:arg-ast))

(defmethod cleavir-ast-graphviz::label ((ast vaslist-pop-ast))
  "vaslist-pop")

(defmethod cleavir-ast:children ((ast vaslist-pop-ast))
  (list (cleavir-ast:arg-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VASLIST-LENGTH-AST
;;;
;;; Gets the remaining number of arguments of a vaslist.

(defclass vaslist-length-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%arg-ast :initarg :vaslist :reader cleavir-ast:arg-ast)))

(cleavir-io:define-save-info vaslist-length-ast
    (:vaslist cleavir-ast:arg-ast))

(defmethod cleavir-ast-graphviz::label ((ast vaslist-length-ast))
  "vaslist-length")

(defmethod cleavir-ast:children ((ast vaslist-length-ast))
  (list (cleavir-ast:arg-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CAS-AST
;;;
;;; Abstract. Class for compare-and-swap ASTs.

(defclass cas-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  (;; The "old" value being compared to the loaded one.
   (%cmp-ast :initarg :cmp-ast :reader cmp-ast)
   ;; The "new" value that's maybe being stored.
   (%value-ast :initarg :value-ast :reader cleavir-ast:value-ast)))

(cleavir-io:define-save-info cas-ast
    (:cmp-ast cmp-ast) (:value-ast cleavir-ast:value-ast))

(defmethod cleavir-ast:children ((ast cas-ast))
  (list (cmp-ast ast) (cleavir-ast:value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CAS-CAR-AST
;;;
;;; Compare-and-swap a cons's car.
;;; NOTE: Could be a child of CAR-AST? Except for CHILDREN methods.

(defclass cas-car-ast (cas-ast)
  ((%cons-ast :initarg :cons-ast :reader cleavir-ast:cons-ast)))

(cleavir-io:define-save-info cas-car-ast
    (:cons-ast cleavir-ast:cons-ast))

(defmethod cleavir-ast-graphviz::label ((ast cas-car-ast))
  "cas-car")

(defmethod cleavir-ast:children ((ast cas-car-ast))
  (list* (cleavir-ast:cons-ast ast) (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CAS-CDR-AST
;;;
;;; Compare-and-swap a cons's cdr.

(defclass cas-cdr-ast (cas-ast)
  ((%cons-ast :initarg :cons-ast :reader cleavir-ast:cons-ast)))

(cleavir-io:define-save-info cas-cdr-ast
    (:cons-ast cleavir-ast:cons-ast))

(defmethod cleavir-ast-graphviz::label ((ast cas-cdr-ast))
  "cas-cdr")

(defmethod cleavir-ast:children ((ast cas-cdr-ast))
  (list* (cleavir-ast:cons-ast ast) (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SLOT-CAS-AST
;;;
;;; Compare-and-swap an instance slot.

(defclass slot-cas-ast (cas-ast)
  ((%object-ast :initarg :object-ast :reader cleavir-ast:object-ast)
   (%slot-number-ast :initarg :slot-number-ast :reader cleavir-ast:slot-number-ast)))

(cleavir-io:define-save-info slot-cas-ast
    (:object-ast cleavir-ast:object-ast)
  (:slot-number-ast cleavir-ast:slot-number-ast))

(defmethod cleavir-ast-graphviz::label ((ast slot-cas-ast))
  "slot-cas")

(defmethod cleavir-ast:children ((ast slot-cas-ast))
  (list* (cleavir-ast:object-ast ast)
         (cleavir-ast:slot-number-ast ast)
         (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ACAS-AST
;;;
;;; Compare-and-swap an array element.

(defclass acas-ast (cas-ast)
  ((%array-ast :initarg :array-ast :reader cleavir-ast:array-ast)
   (%index-ast :initarg :index-ast :reader cleavir-ast:index-ast)
   (%element-type :initarg :element-type :reader cleavir-ast:element-type)
   (%simple-p :initarg :simple-p :reader cleavir-ast:simple-p)
   (%boxed-p :initarg :boxed-p :reader cleavir-ast:boxed-p)))

(cleavir-io:define-save-info acas-ast
    (:array-ast cleavir-ast:array-ast)
  (:index-ast cleavir-ast:index-ast)
  (:element-type cleavir-ast:element-type)
  (:simple-p cleavir-ast:simple-p)
  (:boxed-p cleavir-ast:boxed-p))

(defmethod cleavir-ast-graphviz::label ((ast acas-ast)) "acas")

(defmethod cleavir-ast:children ((ast acas-ast))
  (list* (cleavir-ast:array-ast ast)
         (cleavir-ast:index-ast ast)
         (call-next-method)))

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

(defun make-precalc-vector-function-ast (body-ast precalc-asts forms dynenv policy
                                         &key origin)
  (make-instance 'precalc-vector-function-ast
                 :body-ast body-ast
                 :lambda-list nil
                 :precalc-asts precalc-asts
                 :forms forms
                 :dynamic-environment-out dynenv
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

(defun escaped-string (str)
  (with-output-to-string (s) (loop for c across str do (when (member c '(#\\ #\")) (princ #\\ s)) (princ c s))))

(defmethod cleavir-ast-graphviz::label ((ast precalc-value-reference-ast))
  (with-output-to-string (s)
    (format s "precalc-val-ref ; ")
    (let ((original-object (escaped-string
                            (format nil "~s" (precalc-value-reference-ast-original-object ast)))))
      (if (> (length original-object) 10)
	  (format s "~a..." (subseq original-object 0 10))
	  (princ original-object s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BIND-VA-LIST-AST
;;;
;;; Bind variables according to an ordinary lambda list based on a va_list.
;;; A lot like a function-ast, but not actually one because it just binds.

(defclass bind-va-list-ast (cleavir-ast:ast)
  ((%lambda-list :initarg :lambda-list :reader cleavir-ast:lambda-list)
   (%va-list-ast :initarg :va-list :reader va-list-ast)
   (%body-ast :initarg :body-ast :reader cleavir-ast:body-ast)
   ;; as for named-function-ast
   (%rest-alloc :initarg :rest-alloc :reader rest-alloc)))

(defun make-bind-va-list-ast (lambda-list va-list-ast body-ast rest-alloc
                              &key origin (policy cleavir-ast:*policy*))
  (make-instance 'bind-va-list-ast
    :origin origin :policy policy :rest-alloc rest-alloc
    :va-list va-list-ast :body-ast body-ast :lambda-list lambda-list))

(cleavir-io:define-save-info bind-va-list-ast
    (:lambda-list cleavir-ast:lambda-list)
  (:va-list va-list-ast)
  (:body-ast cleavir-ast:body-ast)
  (:rest-alloc rest-alloc))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Load-time-value hoisting, Clasp-style.

(defun process-ltv (env form read-only-p)
  "If the form is an immediate constant, returns it. If the form has previously been processed here,
returns the previous results. Otherwise, generates code for the form that places the result into a
precalculated-vector and returns the index."
  (cond
    ((constantp form env)
     (let* ((value (ext:constant-form-value form env))
            (immediate (core:create-tagged-immediate-value-or-nil value)))
       (if immediate
           (values immediate t)
           (multiple-value-bind (index indexp)
               (literal:reference-literal value t)
             ;; FIXME: Might not to reorganize things deeper.
             (unless indexp
               (error "BUG: create-tagged-immediate-value-or-nil is inconsistent with literal machinery."))
             (values index nil)))))
    ;; Currently read-only-p is ignored from here on.
    ;; But it might be possible to coalesce EQ forms or something.
    ;; COMPLE-FILE will generate a function for the form in the Module
    ;; and arrange for it's evaluation at load time
    ;; and to make its result available as a value
    ((eq cleavir-generate-ast:*compiler* 'cl:compile-file)
     (values (literal:with-load-time-value
                 (clasp-cleavir::compile-form form env))
             nil))
    ;; COMPILE on the other hand evaluates the form and puts its
    ;; value in the run-time environment.
    (t
     (let ((value (cleavir-env:eval form env env)))
       (multiple-value-bind (index-or-immediate index-p)
           (literal:codegen-rtv-cclasp value)
         (values index-or-immediate (not index-p)))))))

(defun hoist-load-time-value (ast dynenv env)
  (let ((ltvs nil)
        (forms nil))
    (cleavir-ast:map-ast-depth-first-preorder
     (lambda (ast)
       (when (typep ast 'cleavir-ast:load-time-value-ast)
         (push ast ltvs)))
     ast)
    (dolist (ltv ltvs)
      (let ((form (cleavir-ast:form ltv)))
        (multiple-value-bind (index-or-immediate immediatep literal-name)
            (process-ltv env form (cleavir-ast:read-only-p ltv))
          (if immediatep
              (change-class ltv 'cleavir-ast:immediate-ast
                            :value index-or-immediate)
              (change-class ltv 'precalc-value-reference-ast
                            :index index-or-immediate
                            :origin (clasp-cleavir::ensure-origin (cleavir-ast:origin ltv) 9999912)
                            :original-object form)))
        (push form forms)))
    (let ((cleavir-ast:*dynamic-environment* dynenv))
      (clasp-cleavir-ast:make-precalc-vector-function-ast
       ast ltvs forms dynenv (cleavir-ast:policy ast)
       :origin (cleavir-ast:origin ast)))))
