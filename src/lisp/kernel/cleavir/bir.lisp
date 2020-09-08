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
    (prog1 (cleavir-ast-to-bir::figure-1-value inserter inst rtype)
      (cleavir-ast-to-bir::before inserter inst))))
