(in-package :clasp-cleavir-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class THROW-AST
;;;
;;; This AST is used to represent a THROW

(defclass throw-ast (ast:ast)
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

(ast:define-children throw-ast (tag-ast result-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BASE-FOREIGN-CALL-AST
;;;
;;; This AST is used to represent a call to an intrinsic function inserted into the generated code.

(defclass base-foreign-call-ast (ast:ast)
  ((%foreign-types :initarg :foreign-types :accessor foreign-types :initform nil)
   (%argument-asts :initarg :argument-asts :reader argument-asts)))

(cleavir-io:define-save-info base-foreign-call-ast
    (:foreign-types foreign-types)
  (:argument-asts argument-asts))

(defmethod cleavir-ast-graphviz::label ((ast base-foreign-call-ast))
  (with-output-to-string (s)
    (format s "base-foreign-call ~a" (foreign-types ast))))

(ast:define-children base-foreign-call-ast argument-asts)

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

(defclass foreign-call-ast (ast:one-value-ast-mixin base-foreign-call-ast)
  ((%function-name :initarg :function-name :accessor function-name)))

(cleavir-io:define-save-info foreign-call-ast
    (:function-name function-name))

(defmethod cleavir-ast-graphviz::label ((ast foreign-call-ast))
  (with-output-to-string (s)
    (format s "foreign-call (~a)" (function-name ast))))

(ast:define-children foreign-call-ast argument-asts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class foreign-call-pointer-AST
;;;
;;; This AST is used to represent a call to an pointer to a function inserted into the generated code.

(defclass foreign-call-pointer-ast (ast:one-value-ast-mixin
                                    base-foreign-call-ast)
  ())

(defmethod cleavir-ast-graphviz::label ((ast foreign-call-pointer-ast))
  (with-output-to-string (s)
    (format s "foreign-call-pointer")))

(ast:define-children foreign-call-pointer-ast argument-asts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DEFCALLBACK-AST
;;;
;;; This AST is used to represent a callback definition.

(defclass defcallback-ast (ast:no-value-ast-mixin ast:ast)
  (;; None of these are evaluated and there's a ton of them
   ;; so why bother splitting them up
   (%args :initarg :args :reader defcallback-args)
   (%callee :initarg :callee :reader ast:callee-ast)))

(ast:define-children defcallback-ast (ast:callee-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ATOMIC-AST
;;;
;;; Abstract. Superclass for atomic operations.

(defclass atomic-ast (ast:ast)
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

(defclass cas-ast (ast:one-value-ast-mixin atomic-ast)
  (;; The "old" value being compared to the loaded one.
   (%cmp-ast :initarg :cmp-ast :reader cmp-ast)
   ;; The "new" value that's maybe being stored.
   (%value-ast :initarg :value-ast :reader ast:value-ast)))

(cleavir-io:define-save-info cas-ast
    (:cmp-ast cmp-ast) (:value-ast ast:value-ast))

(ast:define-children cas-ast (cmp-ast ast:value-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ATOMIC-VREF-AST, ATOMIC-VSET-AST, VCAS-AST
;;;
;;; Atomic operations on an element of a (simple-array * (*))

(defclass vref-ast (ast:ast) ; abstract
  ((%element-type :initarg :element-type :reader ast:element-type)
   (%array-ast :initarg :array-ast :reader ast:array-ast)
   (%index-ast :initarg :index-ast :reader ast:index-ast)))

(cleavir-io:define-save-info vref-ast
    (:element-type ast:element-type)
  (:array-ast ast:array-ast)
  (:index-ast ast:index-ast))

(ast:define-children vref-ast
    (ast:array-ast ast:index-ast))

(defclass atomic-vref-ast (ast:one-value-ast-mixin atomic-ast vref-ast)
  ())

(defclass atomic-vset-ast (ast:no-value-ast-mixin atomic-ast vref-ast)
  ((%value-ast :initarg :value-ast :reader ast:value-ast)))
(cleavir-io:define-save-info atomic-vset-ast (:value-ast ast:value-ast))
(ast:define-children atomic-vset-ast (ast:value-ast))

(defclass vcas-ast (cas-ast vref-ast) ())
