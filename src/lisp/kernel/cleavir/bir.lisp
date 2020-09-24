(in-package #:cc-bir)

(defclass precalc-value (cleavir-bir::no-input cleavir-bir:computation)
  ((%index :initarg :index :accessor precalc-value-index)
   (%original-form :initarg :form
                   :accessor precalc-value-original-form)))
(defmethod cleavir-bir:rtype ((d precalc-value)) :object)

(defmethod cleavir-ast-to-bir:compile-ast
    ((ast cc-ast:precalc-value-reference-ast) inserter)
  (list
   (cleavir-ast-to-bir:insert
    inserter
    (make-instance 'precalc-value
      :index (cc-ast:precalc-value-reference-ast-index ast)
      :value (cc-ast:precalc-value-reference-ast-original-object ast)))))

(defclass unwind-protect (cleavir-bir:dynamic-environment
                          cleavir-bir::one-input cleavir-bir::no-output
                          cleavir-bir:terminator1 cleavir-bir:operation)
  ())

(defmethod cleavir-ast-to-bir:compile-ast ((ast cc-ast:unwind-protect-ast)
                                            inserter)
  (let ((fu (cleavir-ast-to-bir:compile-ast (cc-ast:cleanup-ast ast) inserter)))
    (if (eq fu :no-return)
        :no-return
        (let* ((uw (make-instance 'unwind-protect
                     :inputs (cleavir-ast-to-bir:adapt inserter fu '(:object))))
               (ode (cleavir-ast-to-bir:dynamic-environment inserter))
               (during (cleavir-ast-to-bir:make-iblock
                        inserter :dynamic-environment uw)))
          (setf (cleavir-bir:next uw) (list during))
          (cleavir-ast-to-bir:terminate inserter uw)
          (cleavir-ast-to-bir:begin inserter during)
          (let ((rv (cleavir-ast-to-bir:compile-ast (cleavir-ast:body-ast ast)
                                                     inserter)))
            (cond ((eq rv :no-return) :no-return)
                  (t
                   (let ((next (cleavir-ast-to-bir:make-iblock
                                inserter :dynamic-environment ode)))
                     (cleavir-ast-to-bir:terminate
                      inserter
                      (make-instance 'cleavir-bir:jump
                        :inputs () :outputs () :unwindp t :next (list next)))
                     (cleavir-ast-to-bir:begin inserter next))
                   rv)))))))

(defclass bind (cleavir-bir:dynamic-environment cleavir-bir:terminator1
                cleavir-bir::no-output cleavir-bir:operation)
  ())

(defmethod cleavir-ast-to-bir:compile-ast ((ast cc-ast:bind-ast) inserter)
  (let ((args (cleavir-ast-to-bir:compile-arguments
               (list (cleavir-ast:name-ast ast) (cleavir-ast:value-ast ast))
               inserter)))
    (when (eq args :no-return)
      (return-from cleavir-ast-to-bir:compile-ast args))
    (let* ((during (cleavir-ast-to-bir:make-iblock inserter))
           (ode (cleavir-ast-to-bir:dynamic-environment inserter))
           (bind (make-instance 'bind :inputs args :next (list during))))
      (setf (cleavir-bir:dynamic-environment during) bind)
      (cleavir-ast-to-bir:terminate inserter bind)
      (cleavir-ast-to-bir:begin inserter during)
      (let ((rv (cleavir-ast-to-bir:compile-ast (cleavir-ast:body-ast ast)
                                                 inserter)))
        (cond ((eq rv :no-return) :no-return)
              (t
               (let ((next (cleavir-ast-to-bir:make-iblock
                            inserter :dynamic-environment ode)))
                 (cleavir-ast-to-bir:terminate
                  inserter
                  (make-instance 'cleavir-bir:jump
                    :inputs () :outputs () :unwindp t :next (list next)))
                 (cleavir-ast-to-bir:begin inserter next))
               rv))))))

(defmethod cleavir-bir::disassemble-instruction ((inst bind))
  `(,(cleavir-bir::dis-label inst) ,@(mapcar #'cleavir-bir::disassemble-datum
                                             (cleavir-bir:inputs inst))
    ,(cleavir-bir::dis-iblock (first (cleavir-bir:next inst)))))

(defclass mv-foreign-call (cleavir-bir:computation)
  ((%function-name :initarg :function-name :reader function-name)))
(defmethod cleavir-bir:rtype ((d mv-foreign-call)) :multiple-values)

(defmethod cleavir-ast-to-bir:compile-ast
    ((ast cc-ast:multiple-value-foreign-call-ast) inserter)
  (let ((args (cleavir-ast-to-bir:compile-arguments
               (cc-ast:argument-asts ast) inserter)))
    (if (eq args :no-return)
        :no-return
        (cleavir-ast-to-bir:insert
         inserter
         (make-instance 'mv-foreign-call
           :function-name (cc-ast:function-name ast)
           :inputs args)))))

(defclass header-stamp-case (cleavir-bir::no-input cleavir-bir::no-output
                             cleavir-bir:terminator cleavir-bir:operation)
  ())

(defmethod cleavir-ast-to-bir:compile-test-ast
    ((ast cc-ast:header-stamp-case-ast) inserter)
  (let ((rv (cleavir-ast-to-bir:compile-ast (cc-ast:stamp-ast ast) inserter)))
    (when (eq rv :no-return) (return-from cleavir-ast-to-bir:compile-test-ast rv))
    (let ((hsc (make-instance 'header-stamp-case
                 :inputs (cleavir-ast-to-bir:adapt rv '(:object)))))
      (cleavir-ast-to-bir:terminate inserter hsc))))
