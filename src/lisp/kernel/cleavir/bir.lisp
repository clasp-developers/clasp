(in-package #:cc-bir)

(defclass mv-foreign-call (bir:one-output bir:instruction)
  ((%function-name :initarg :function-name :reader function-name)))

(defmethod ast-to-bir:compile-ast
    ((ast cc-ast:multiple-value-foreign-call-ast) inserter system)
  (ast-to-bir:with-compiled-arguments (args (cc-ast:argument-asts ast)
                                            inserter system)
    (let ((output (make-instance 'bir:output)))
      (build:insert inserter 'mv-foreign-call
                    :function-name (cc-ast:function-name ast)
                    :inputs args :outputs (list output))
      (list output))))

(defclass foreign-call-pointer (bir:one-output bir:instruction)
  ((%foreign-types :initarg :foreign-types :accessor foreign-types)))

(defmethod ast-to-bir:compile-ast
    ((ast cc-ast:foreign-call-pointer-ast) inserter system)
  (ast-to-bir:with-compiled-arguments (args (cc-ast:argument-asts ast)
                                            inserter system)
    (let ((out (make-instance 'bir:output)))
      (build:insert inserter 'foreign-call-pointer
                    :foreign-types (cc-ast:foreign-types ast)
                    :inputs args :outputs (list out))
      (list out))))

;;; atomics

(defclass atomic (bir:instruction)
  ((%order :initarg :order :reader order :initform :relaxed
           :type (member :relaxed :acquire :release :acquire-release
                         :sequentially-consistent))))

;;;
;;; vaslist stuff

(defclass cc-vaslist:values-list (bir:one-input bir:one-output
                                 bir:instruction)
  ())
(defclass cc-vaslist:nth (bir:one-output bir:instruction) ())
(defclass cc-vaslist:nthcdr (bir:one-output bir:instruction) ())
(defclass cc-vaslist:last (bir:one-output bir:instruction) ())
(defclass cc-vaslist:butlast (bir:one-output bir:instruction) ())
(defclass cc-vaslist:length (bir:one-input bir:one-output bir:instruction) ())

;;; This is (complement #'endp) for vaslists - i.e. "Not ENDP"
(defclass cc-vaslist:nendp (bir:one-input bir:conditional-test) ())

;;;

(macrolet ((defprimop (name ninputs out &rest rtype-info)
             `(progn
                (cleavir-primop-info:defprimop ,name ,ninputs ,out)
                ,@(when rtype-info
                    `((setf (gethash (cleavir-primop-info:info ',name)
                                     *primop-rtypes*)
                            '(,@rtype-info))))
                (cleavir-cst-to-ast:defprimop ,name))))

  (defprimop core:instance-rack 1 :value)
  (defprimop core::instance-rack-set 2 :effect)

  (defprimop core:rack-ref 2 :value)
  (defprimop core::rack-set 3 :effect)

  (defprimop core:vaslist-pop 1 :value)
  (defprimop core:vaslist-length 1 :value))

;;; misc

;;; longjmping through values allocas is no problem.

(defmethod cleavir-bir-transformations:simple-dynenv-p
    ((dynenv bir:values-save) (dest bir:dynamic-environment)
     (system clasp-cleavir:clasp))
  (cleavir-bir-transformations:simple-dynenv-p
   (bir:parent dynenv) dest system))
(defmethod cleavir-bir-transformations:simple-dynenv-p
    ((dynenv bir:values-collect) (dest bir:dynamic-environment)
     (system clasp-cleavir:clasp))
  (cleavir-bir-transformations:simple-dynenv-p
   (bir:parent dynenv) dest system))
