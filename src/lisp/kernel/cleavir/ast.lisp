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

(cleavir-ast:define-children setf-fdefinition-ast ())

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

(cleavir-ast:define-children throw-ast (tag-ast result-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DEBUG-MESSAGE-AST
;;;
;;; This AST is used to represent a debugging message inserted into the generated code.

(defclass debug-message-ast (cleavir-ast:one-value-ast-mixin cleavir-ast:ast)
  ((%debug-message :initarg :debug-message  :accessor debug-message)))

(cleavir-io:define-save-info debug-message-ast
    (:debug-message debug-message))

(defmethod cleavir-ast-graphviz::label ((ast debug-message-ast))
  (with-output-to-string (s)
    (format s "debug-message (~a)" (debug-message ast))))

(cleavir-ast:define-children debug-message-ast ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DEBUG-BREAK-AST
;;;
;;; This AST is used to represent a debugging break inserted into the generated code.

(defclass debug-break-ast (cleavir-ast:no-value-ast-mixin cleavir-ast:ast)
  ())

(cleavir-io:define-save-info debug-break-ast
    ())

(defmethod cleavir-ast-graphviz::label ((ast debug-break-ast))
  (with-output-to-string (s)
    (format s "debug-break")))

(cleavir-ast:define-children debug-break-ast ())


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

(cleavir-ast:define-children base-foreign-call-ast argument-asts)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FOREIGN-call-AST
;;;
;;; This AST is used to represent a call to a named foreign function
;;;   inserted into the generated code.

(defclass foreign-call-ast (cleavir-ast:one-value-ast-mixin base-foreign-call-ast)
  ((%function-name :initarg :function-name :accessor function-name)))

(cleavir-io:define-save-info foreign-call-ast
    (:function-name function-name))

(defmethod cleavir-ast-graphviz::label ((ast foreign-call-ast))
  (with-output-to-string (s)
    (format s "foreign-call (~a)" (function-name ast))))

(cleavir-ast:define-children foreign-call-ast argument-asts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class foreign-call-pointer-AST
;;;
;;; This AST is used to represent a call to an pointer to a function inserted into the generated code.

(defclass foreign-call-pointer-ast (cleavir-ast:one-value-ast-mixin base-foreign-call-ast)
  ())

(defmethod cleavir-ast-graphviz::label ((ast foreign-call-pointer-ast))
  (with-output-to-string (s)
    (format s "foreign-call-pointer")))

(cleavir-ast:define-children foreign-call-pointer-ast argument-asts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DEFCALLBACK-AST
;;;
;;; This AST is used to represent a callback definition.

(defclass defcallback-ast (cleavir-ast:no-value-ast-mixin cleavir-ast:ast)
  (;; None of these are evaluated and there's a ton of them
   ;; so why bother splitting them up
   (%args :initarg :args :reader defcallback-args)
   (%callee :initarg :callee :reader cleavir-ast:callee-ast)))

(cleavir-ast:define-children defcallback-ast (cleavir-ast:callee-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class HEADER-STAMP-CASE-AST
;;;

(defclass header-stamp-case-ast (cleavir-ast:ast)
  ((%stamp-ast :initarg :stamp-ast :reader stamp-ast)))

(cleavir-ast:define-children header-stamp-case-ast (stamp-ast))

(defun make-header-stamp-case-ast (stamp &optional origin)
  (make-instance 'header-stamp-case-ast
    :stamp-ast stamp :origin origin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class HEADER-STAMP-AST

(defclass header-stamp-ast (cleavir-ast:one-value-ast-mixin cleavir-ast:ast)
  ((%arg :initarg :arg :accessor cleavir-ast:arg-ast)))
(cleavir-io:define-save-info header-stamp-ast (:arg cleavir-ast:arg-ast))
(defmethod cleavir-ast-graphviz::label ((ast header-stamp-ast)) "header-stamp")
(cleavir-ast:define-children header-stamp-ast (cleavir-ast:arg-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class RACK-STAMP-AST

(defclass rack-stamp-ast (cleavir-ast:one-value-ast-mixin cleavir-ast:ast)
  ((%arg :initarg :arg :accessor cleavir-ast:arg-ast)))
(cleavir-io:define-save-info rack-stamp-ast (:arg cleavir-ast:arg-ast))
(defmethod cleavir-ast-graphviz::label ((ast rack-stamp-ast)) "rack-stamp")
(cleavir-ast:define-children rack-stamp-ast (cleavir-ast:arg-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class WRAPPED-STAMP-AST

(defclass wrapped-stamp-ast (cleavir-ast:one-value-ast-mixin cleavir-ast:ast)
  ((%arg :initarg :arg :accessor cleavir-ast:arg-ast)))
(cleavir-io:define-save-info wrapped-stamp-ast (:arg cleavir-ast:arg-ast))
(defmethod cleavir-ast-graphviz::label ((ast wrapped-stamp-ast)) "wrapped-stamp")
(cleavir-ast:define-children wrapped-stamp-ast (cleavir-ast:arg-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DERIVABLE-STAMP-AST

(defclass derivable-stamp-ast (cleavir-ast:one-value-ast-mixin cleavir-ast:ast)
  ((%arg :initarg :arg :accessor cleavir-ast:arg-ast)))
(cleavir-io:define-save-info derivable-stamp-ast (:arg cleavir-ast:arg-ast))
(defmethod cleavir-ast-graphviz::label ((ast derivable-stamp-ast)) "derivable-stamp")
(cleavir-ast:define-children derivable-stamp-ast (cleavir-ast:arg-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ATOMIC-AST
;;;
;;; Abstract. Superclass for atomic operations.

(defclass atomic-ast (cleavir-ast:ast)
  (;; The ordering.
   (%order :initarg :order :reader order
           :type (member :relaxed :acquire :release :acquire-release
                         :sequentially-consistent))))

(cleavir-io:define-save-info atomic-ast (:order order))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CAS-AST
;;;
;;; Abstract. Class for compare-and-swap ASTs.

(defclass cas-ast (cleavir-ast:one-value-ast-mixin atomic-ast)
  (;; The "old" value being compared to the loaded one.
   (%cmp-ast :initarg :cmp-ast :reader cmp-ast)
   ;; The "new" value that's maybe being stored.
   (%value-ast :initarg :value-ast :reader cleavir-ast:value-ast)))

(cleavir-io:define-save-info cas-ast
    (:cmp-ast cmp-ast) (:value-ast cleavir-ast:value-ast))

(cleavir-ast:define-children cas-ast (cmp-ast cleavir-ast:value-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes ATOMIC-CAR-AST, ATOMIC-CDR-AST, ATOMIC-RPLACA-AST, ATOMIC-RPLACD-AST
;;;

(defclass atomic-car-ast (atomic-ast cleavir-ast:car-ast) ())
(defclass atomic-cdr-ast (atomic-ast cleavir-ast:cdr-ast) ())
(defclass atomic-rplaca-ast (atomic-ast cleavir-ast:rplaca-ast) ())
(defclass atomic-rplacd-ast (atomic-ast cleavir-ast:rplacd-ast) ())

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

(defmethod cleavir-ast:map-children (function (ast cas-car-ast))
  (funcall function (cleavir-ast:cons-ast ast))
  (call-next-method))

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

(defmethod cleavir-ast:map-children (function (ast cas-cdr-ast))
  (funcall function (cleavir-ast:cons-ast ast))
  (call-next-method))

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

(defmethod cleavir-ast:map-children (function (ast slot-cas-ast))
  (funcall function (cleavir-ast:object-ast ast))
  (funcall function (cleavir-ast:slot-number-ast ast))
  (call-next-method))

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

(defmethod cleavir-ast:map-children (function (ast acas-ast))
  (funcall function (cleavir-ast:array-ast ast))
  (funcall function (cleavir-ast:index-ast ast))
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BIND-AST
;;;
;;; Represents a special variable binding.
;;;

(defclass bind-ast (cleavir-ast:ast)
  ((%name :initarg :name-ast :reader cleavir-ast:name-ast)
   (%value :initarg :value-ast :reader cleavir-ast:value-ast)
   (%body :initarg :body-ast :reader cleavir-ast:body-ast)))

(cleavir-io:define-save-info bind-ast
    (:name-ast cleavir-ast:name-ast)
  (:value-ast cleavir-ast:value-ast)
  (:body-ast cleavir-ast:body-ast))

(defmethod cleavir-ast-graphviz::label ((ast bind-ast)) "bind")

(cleavir-ast:define-children bind-ast
    (cleavir-ast:name-ast cleavir-ast:value-ast cleavir-ast:body-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class UNWIND-PROTECT-AST
;;;
;;; Represents CL:UNWIND-PROTECT.
;;;
;;; NOTE: The cleanup forms are stored as a thunk. This could be changed
;;; so that the actual code is run, avoiding the overhead of allocating a
;;; closure, and calling and so on. For now I'm assuming it's unimportant.
;;;

(defclass unwind-protect-ast (cleavir-ast:ast)
  ((%body :initarg :body-ast :reader cleavir-ast:body-ast)
   ;; This will be a FUNCTION-AST.
   (%cleanup :initarg :cleanup-ast :reader cleanup-ast)))

(cleavir-io:define-save-info unwind-protect-ast
    (:body-ast cleavir-ast:body-ast)
  (:cleanup-ast cleanup-ast))

(defmethod cleavir-ast-graphviz::label ((ast unwind-protect-ast))
  "unwind-protect")

(cleavir-ast:define-children unwind-protect-ast (cleavir-ast:body-ast cleanup-ast))

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
   ;; Either NIL, indicating normal allocation,
   ;; or DYNAMIC-EXTENT, indicating dynamic extent (stack) allocation,
   ;; or IGNORE, indicating no allocation.
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

(defmethod cleavir-ast:map-children (function (ast bind-va-list-ast))
  (funcall function (va-list-ast ast))
  (funcall function (cleavir-ast:body-ast ast))
  (dolist (entry (cleavir-ast:lambda-list ast))
    (cond ((symbolp entry))
          ((consp entry)
           (if (= (length entry) 2)
               (mapc function entry)
               (mapc function (cdr entry))))
          (t (funcall function entry)))))
