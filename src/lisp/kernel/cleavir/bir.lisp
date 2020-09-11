(defpackage #:clasp-cleavir-bir
  (:use #:cl)
  (:nicknames #:cc-bir))

(in-package #:cc-bir)

(defclass precalc-value (cleavir-bir:constant cleavir-bir:computation)
  ((%index :initarg :index :accessor precalc-value-index))
  (:default-initargs :inputs nil :rtype :object))

(defmethod cleavir-ast-to-bir:compile-ast
    ((ast cc-ast:precalc-value-reference-ast) inserter rtype)
  (let* ((index (cc-ast:precalc-value-reference-ast-index ast))
         (inst (make-instance 'precalc-value
                              :index (cc-ast:precalc-value-reference-ast-index ast)
                              :value (cc-ast:precalc-value-reference-ast-original-object ast))))
    (cleavir-ast-to-bir:return-1 inserter inst rtype)))

(defmethod cleavir-ast-to-bir:compile-ast :around
    ((ast cleavir-ast:block-ast) inserter context)
  (case context
    ((:multiple-values :effect) (call-next-method))
    (t (multiple-value-bind (inst outs)
           ;; FIXME this interface
           (cleavir-bir:make-multiple-to-fixed nil context)
         (cleavir-ast-to-bir::before inserter inst)
         (setf (cleavir-bir:inputs inst)
               (call-next-method ast inserter :multiple-values))
         outs))))
