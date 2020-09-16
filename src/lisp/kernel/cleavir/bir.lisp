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

(defclass unwind-protect (cleavir-bir:dynamic-environment
                          cleavir-bir::one-input cleavir-bir::no-output
                          cleavir-bir:terminator1 cleavir-bir:operation)
  ())

(defmethod cleavir-ast-to-bir:compile-ast ((ast cc-ast:unwind-protect-ast)
                                           inserter context)
  (let* ((uw (make-instance 'unwind-protect))
         (lu (make-instance 'cleavir-bir:jump
                            :inputs () :outputs () :unwindp t
                            :next (list (cleavir-ast-to-bir::iblock inserter))))
         (during (cleavir-ast-to-bir::make-iblock inserter
                                                  :dynamic-environment uw))
        (pre (cleavir-ast-to-bir::make-iblock inserter)))
    (cleavir-ast-to-bir::finalize inserter)
    (cleavir-ast-to-bir::reset inserter during)
    (cleavir-ast-to-bir::terminate inserter lu)
    (prog1 (cleavir-ast-to-bir:compile-ast
            (cleavir-ast:body-ast ast)
            inserter context)
      (cleavir-ast-to-bir::finalize inserter)
      (let ((uw-start (cleavir-ast-to-bir::iblock inserter)))
        (setf (cleavir-bir:next uw) (list uw-start))
        (cleavir-ast-to-bir::reset inserter pre)
        (cleavir-ast-to-bir::terminate inserter uw)
        (setf (cleavir-bir:inputs uw)
              (cleavir-ast-to-bir:compile-ast
               (cc-ast:cleanup-ast ast) inserter '(:object)))))))

(defclass bind (cleavir-bir:dynamic-environment cleavir-bir:terminator1
                cleavir-bir::no-output cleavir-bir:operation)
  ())

(defmethod cleavir-ast-to-bir:compile-ast ((ast cc-ast:bind-ast) inserter context)
  (let* ((bind (make-instance 'bind))
         (lu (make-instance 'cleavir-bir:jump
                            :inputs () :outputs () :unwindp t
                            :next (list (cleavir-ast-to-bir::iblock inserter))))
         (during (cleavir-ast-to-bir::make-iblock inserter
                                                  :dynamic-environment bind))
         (pre (cleavir-ast-to-bir::make-iblock inserter)))
    (cleavir-ast-to-bir::finalize inserter)
    (cleavir-ast-to-bir::reset inserter during)
    (cleavir-ast-to-bir::terminate inserter lu)
    (prog1 (cleavir-ast-to-bir:compile-ast
            (cleavir-ast:body-ast ast)
            inserter context)
      (cleavir-ast-to-bir::finalize inserter)
      (let ((bind-start (cleavir-ast-to-bir::iblock inserter)))
        (setf (cleavir-bir:next bind) (list bind-start))
        (cleavir-ast-to-bir::reset inserter pre)
        (cleavir-ast-to-bir::terminate inserter bind)
        (setf (cleavir-bir:inputs bind)
              (cleavir-ast-to-bir::compile-arguments
               (list (cleavir-ast:name-ast ast)
                     (cleavir-ast:value-ast ast))
               inserter))))))

(defmethod cleavir-bir::disassemble-instruction ((inst bind))
  `(,(cleavir-bir::dis-label inst) ,@(mapcar #'cleavir-bir::disassemble-datum
                                             (cleavir-bir:inputs inst))
    ,(cleavir-bir::dis-iblock (first (cleavir-bir:next inst)))))

(defclass mv-foreign-call (cleavir-bir:computation)
  ((%function-name :initarg :function-name :reader function-name)))

(defmethod cleavir-ast-to-bir:compile-ast
    ((ast cc-ast:multiple-value-foreign-call-ast) inserter context)
  (let* ((mvcall (make-instance 'mv-foreign-call
                   :function-name (cc-ast:function-name ast)))
         (result (cleavir-ast-to-bir::figure-mvalues inserter mvcall context))
         (_ (cleavir-ast-to-bir::before inserter mvcall))
         (args (cc-ast:argument-asts ast))
         (argsvs (cleavir-ast-to-bir::compile-arguments args inserter)))
    (declare (ignore _))
    (setf (cleavir-bir:inputs mvcall) argsvs)
    result))
