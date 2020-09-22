(in-package #:cc-bir)

(defclass precalc-value (cleavir-bir::no-input cleavir-bir:computation)
  ((%index :initarg :index :accessor precalc-value-index)
   (%original-form :initarg :form
                   :accessor precalc-value-original-form)))
(defmethod cleavir-bir:rtype ((d precalc-value)) :object)

(defmethod cleavir-ast-to-bir:compile-ast
    ((ast cc-ast:precalc-value-reference-ast) inserter rtype)
  (let* ((index (cc-ast:precalc-value-reference-ast-index ast))
         (inst
           (make-instance 'precalc-value
             :index (cc-ast:precalc-value-reference-ast-index ast)
             :form (cc-ast:precalc-value-reference-ast-original-object ast))))
    (cleavir-ast-to-bir:return-1 inserter inst rtype)))

(defmethod cleavir-ast-to-bir2:compile-ast
    ((ast cc-ast:precalc-value-reference-ast) inserter)
  (list
   (cleavir-ast-to-bir2:insert
    inserter
    (make-instance 'precalc-value
      :index (cc-ast:precalc-value-reference-ast-index ast)
      :value (cc-ast:precalc-value-reference-ast-original-object ast)))))

(defmethod cleavir-ast-to-bir:compile-ast :around
    ((ast cleavir-ast:block-ast) inserter context)
  (case context
    ((:multiple-values :effect) (call-next-method))
    (t (let* ((outs (loop for rt in context
                          collect (make-instance 'cleavir-bir:output :rtype rt)))
              (inst
                (make-instance 'cleavir-bir:multiple-to-fixed :outputs outs)))
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

(defmethod cleavir-ast-to-bir2:compile-ast ((ast cc-ast:unwind-protect-ast)
                                            inserter)
  (let ((fu (cleavir-ast-to-bir2:compile-ast (cc-ast:cleanup-ast ast) inserter)))
    (if (eq fu :no-return)
        :no-return
        (let* ((uw (make-instance 'unwind-protect))
               (ode (cleavir-ast-to-bir2:dynamic-environment inserter))
               (during (cleavir-ast-to-bir2:make-iblock
                        inserter :dynamic-environment uw)))
          (setf (cleavir-bir:next uw) (list during))
          (cleavir-ast-to-bir2:terminate inserter uw)
          (cleavir-ast-to-bir2:begin inserter during)
          (let ((rv (cleavir-ast-to-bir2:compile-ast (cleavir-ast:body-ast ast)
                                                     inserter)))
            (cond ((eq rv :no-return) :no-return)
                  (t
                   (let ((next (cleavir-ast-to-bir2:make-iblock
                                inserter :dynamic-environment ode)))
                     (cleavir-ast-to-bir2:terminate
                      inserter
                      (make-instance 'cleavir-bir:jump
                        :inputs () :outputs () :unwindp t :next (list next)))
                     (cleavir-ast-to-bir2:begin inserter next))
                   rv)))))))

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

(defmethod cleavir-ast-to-bir2:compile-ast ((ast cc-ast:unwind-protect-ast)
                                            inserter)
  (let ((args (cleavir-ast-to-bir2:compile-arguments
               (list (cleavir-ast:name-ast ast) (cleavir-ast:value-ast ast))
               inserter)))
    (when (eq args :no-return)
      (return-from cleavir-ast-to-bir2:compile-ast args))
    (let* ((during (cleavir-ast-to-bir2:make-iblock inserter))
           (ode (cleavir-ast-to-bir2:dynamic-environment inserter))
           (bind (make-instance 'bind :inputs args :next (list during))))
      (setf (cleavir-bir:dynamic-environment during) bind)
      (cleavir-ast-to-bir2:terminate inserter bind)
      (cleavir-ast-to-bir2:begin inserter during)
      (let ((rv (cleavir-ast-to-bir2:compile-ast (cleavir-ast:body-ast ast)
                                                 inserter)))
        (cond ((eq rv :no-return) :no-return)
              (t
               (let ((next (cleavir-ast-to-bir2:make-iblock
                            inserter :dynamic-environment ode)))
                 (cleavir-ast-to-bir2:terminate
                  inserter
                  (make-instance 'cleavir-bir:jump
                    :inputs () :outputs () :unwindp t :next (list next)))
                 (cleavir-ast-to-bir2:begin inserter next))
               rv))))))

(defmethod cleavir-bir::disassemble-instruction ((inst bind))
  `(,(cleavir-bir::dis-label inst) ,@(mapcar #'cleavir-bir::disassemble-datum
                                             (cleavir-bir:inputs inst))
    ,(cleavir-bir::dis-iblock (first (cleavir-bir:next inst)))))

(defclass mv-foreign-call (cleavir-bir:computation)
  ((%function-name :initarg :function-name :reader function-name)))
(defmethod cleavir-bir:rtype ((d mv-foreign-call)) :multiple-values)

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

(defmethod cleavir-ast-to-bir2:compile-ast
    ((ast cc-ast:multiple-value-foreign-call-ast) inserter)
  (let ((args (cleavir-ast-to-bir2:compile-arguments
               (cc-ast:argument-asts ast) inserter)))
    (if (eq args :no-return)
        :no-return
        (cleavir-ast-to-bir2:insert
         inserter
         (make-instance 'mv-foreign-call
           :function-name (cc-ast:function-name ast)
           :inputs args)))))

(defclass header-stamp-case (cleavir-bir::no-input cleavir-bir::no-output
                             cleavir-bir:terminator cleavir-bir:operation)
  ())

(defmethod cleavir-ast-to-bir2:compile-test-ast
    ((ast cc-ast:header-stamp-case-ast) inserter)
  (let ((rv (cleavir-ast-to-bir2:compile-ast (cc-ast:stamp-ast ast) inserter)))
    (when (eq rv :no-return) (return-from cleavir-ast-to-bir2:compile-test-ast rv))
    (let ((hsc (make-instance 'header-stamp-case
                 :inputs (cleavir-ast-to-bir2:adapt rv '(:object)))))
      (cleavir-ast-to-bir2:terminate inserter hsc))))
