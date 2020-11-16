(in-package #:cc-bir)

(defclass unwind-protect (cleavir-bir:dynamic-environment
                          cleavir-bir::one-input cleavir-bir::no-output
                          cleavir-bir:terminator1 cleavir-bir:operation)
  ())

(defmethod cleavir-ast-to-bir:compile-ast ((ast cc-ast:unwind-protect-ast)
                                           inserter system)
  (cleavir-ast-to-bir:with-compiled-ast (fu (cc-ast:cleanup-ast ast)
                                            inserter system)
    (let* ((uw (make-instance 'unwind-protect :inputs fu))
           (ode (cleavir-ast-to-bir:dynamic-environment inserter))
           (during (cleavir-ast-to-bir:make-iblock
                    inserter :dynamic-environment uw)))
      (setf (cleavir-bir:next uw) (list during))
      (cleavir-ast-to-bir:terminate inserter uw)
      (cleavir-ast-to-bir:begin inserter during)
      (let ((rv (cleavir-ast-to-bir:compile-ast (cleavir-ast:body-ast ast)
                                                inserter system)))
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

(defclass bind (cleavir-bir:dynamic-environment cleavir-bir:terminator1
                cleavir-bir::no-output cleavir-bir:operation)
  ())

(defmethod cleavir-ast-to-bir:compile-ast ((ast cc-ast:bind-ast)
                                           inserter system)
  (cleavir-ast-to-bir:with-compiled-asts (args ((cleavir-ast:name-ast ast)
                                                (cleavir-ast:value-ast ast))
                                               inserter system
                                               (:object :object))
    (let* ((during (cleavir-ast-to-bir:make-iblock inserter))
           (ode (cleavir-ast-to-bir:dynamic-environment inserter))
           (bind (make-instance 'bind :inputs args :next (list during))))
      (setf (cleavir-bir:dynamic-environment during) bind)
      (cleavir-ast-to-bir:terminate inserter bind)
      (cleavir-ast-to-bir:begin inserter during)
      (let ((rv (cleavir-ast-to-bir:compile-ast (cleavir-ast:body-ast ast)
                                                inserter system)))
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

(defclass mv-foreign-call (cleavir-bir:computation)
  ((%function-name :initarg :function-name :reader function-name)))
(defmethod cleavir-bir:rtype ((d mv-foreign-call)) :multiple-values)

(defmethod cleavir-ast-to-bir:compile-ast
    ((ast cc-ast:multiple-value-foreign-call-ast) inserter system)
  (cleavir-ast-to-bir:with-compiled-arguments (args (cc-ast:argument-asts ast)
                                                    inserter system)
    (cleavir-ast-to-bir:insert
     inserter
     (make-instance 'mv-foreign-call
       :function-name (cc-ast:function-name ast)
       :inputs (mapcar #'first args)))))

(defclass foreign-call-pointer (cleavir-bir:computation)
  ((%foreign-types :initarg :foreign-types :accessor foreign-types)))
(defmethod cleavir-bir:rtype ((d foreign-call-pointer)) :object)

(defmethod cleavir-ast-to-bir:compile-ast
    ((ast cc-ast:foreign-call-pointer-ast) inserter system)
  (cleavir-ast-to-bir:with-compiled-arguments (args (cc-ast:argument-asts ast)
                                                    inserter system)
    (list
     (cleavir-ast-to-bir:insert
      inserter
      (make-instance 'foreign-call-pointer
        :foreign-types (cc-ast:foreign-types ast)
        :inputs (mapcar #'first args))))))

(defclass defcallback (cleavir-bir:operation)
  ((%args :initarg :args :reader defcallback-args)))

(defmethod cleavir-ast-to-bir:compile-ast ((ast cc-ast:defcallback-ast)
                                           inserter system)
  (cleavir-ast-to-bir:with-compiled-ast (rv (cleavir-ast:callee-ast ast)
                                            inserter system)
    (cleavir-ast-to-bir:insert
     inserter
     (make-instance 'defcallback
       :args (cc-ast:defcallback-args ast)
       :inputs rv :outputs ())))
  ())

(defclass header-stamp-case (cleavir-bir::one-input cleavir-bir::no-output
                             cleavir-bir:terminator cleavir-bir:operation)
  ())

(defmethod cleavir-ast-to-bir:compile-test-ast
    ((ast cc-ast:header-stamp-case-ast) inserter system)
  (cleavir-ast-to-bir:with-compiled-ast (rv (cc-ast:stamp-ast ast)
                                            inserter system)
    (let* ((ibs
             (loop repeat 4 collect (cleavir-ast-to-bir:make-iblock inserter)))
           (hsc (make-instance 'header-stamp-case
                  :next ibs :inputs rv)))
      (cleavir-ast-to-bir:terminate inserter hsc)
      (copy-list ibs))))

(defclass acas (cleavir-bir:computation)
  ((%element-type :initarg :element-type :reader element-type)
   (%simple-p :initarg :simple-p :reader simple-p)
   (%boxed-p :initarg :boxed-p :reader boxed-p)))
(defmethod cleavir-bir:rtype ((d acas))
  (case (element-type d)
    ((t) :object)
    (otherwise (error "BUG: CAS only of general vectors is supported as of yet, sorry!"))))

(defmethod cleavir-ast-to-bir:compile-ast ((ast cc-ast:acas-ast)
                                           inserter system)
  (let ((boxed-p (cleavir-ast:boxed-p ast)))
    (unless boxed-p
      (error "BUG: CAS of only vectors with boxed elements is supported as of yet, sorry!"))
    (cleavir-ast-to-bir:with-compiled-asts (args ((cleavir-ast:array-ast ast)
                                                  (cleavir-ast:index-ast ast)
                                                  (cc-ast:cmp-ast ast)
                                                  (cleavir-ast:value-ast ast))
                                                 inserter system
                                                 (:object :object
                                                  :object :object))
      (list
       (cleavir-ast-to-bir:insert
        inserter
        (make-instance 'acas
          :inputs args
          :element-type (cleavir-ast:element-type ast)
          :simple-p (cleavir-ast:simple-p ast)
          :boxed-p boxed-p))))))

;;;

(macrolet ((defprimop (name (&rest in) (&rest out) ast &rest readers)
             `(progn
                (cleavir-bir:defprimop ,name (,@in) (,@out))
                (cleavir-ast-to-bir:defprimop ,name ,ast ,@readers))))
  (defprimop setf-fdefinition (:object) (:object)
    cc-ast:setf-fdefinition-ast cleavir-ast:name-ast)
  
  (defprimop core::vector-length (:object) (:object)
    cc-ast:vector-length-ast cleavir-ast:arg-ast)
  (defprimop core::%displacement (:object) (:object)
    cc-ast:displacement-ast cleavir-ast:arg-ast)
  (defprimop core::%displaced-index-offset (:object) (:object)
    cc-ast:displaced-index-offset-ast cleavir-ast:arg-ast)
  (defprimop core::%array-total-size (:object) (:object)
    cc-ast:array-total-size-ast cleavir-ast:arg-ast)
  (defprimop core::%array-rank (:object) (:object)
    cc-ast:array-rank-ast cleavir-ast:arg-ast)
  (defprimop core::%array-dimension (:object :object) (:object)
    cc-ast:array-dimension-ast cleavir-ast:arg1-ast cleavir-ast:arg2-ast)

  (defprimop clos:standard-instance-access (:object :object) (:object)
    cleavir-ast:slot-read-ast
    cleavir-ast:object-ast cleavir-ast:slot-number-ast)
  (defprimop (setf clos:standard-instance-access) (:object :object :object) ()
    cleavir-ast:slot-write-ast
    cleavir-ast:object-ast cleavir-ast:slot-number-ast cleavir-ast:value-ast)
  
  (defprimop clos:funcallable-standard-instance-access
      (:object :object) (:object)
    cleavir-ast:funcallable-slot-read-ast
    cleavir-ast:object-ast cleavir-ast:slot-number-ast)
  (defprimop (setf clos:funcallable-standard-instance-access)
      (:object :object :object) ()
    cleavir-ast:funcallable-slot-write-ast
    cleavir-ast:object-ast cleavir-ast:slot-number-ast cleavir-ast:value-ast)
  (defprimop core::instance-cas (:object :object :object :object) (:object)
    cc-ast:slot-cas-ast
    cc-ast:cmp-ast cleavir-ast:value-ast cleavir-ast:object-ast
    cleavir-ast:slot-number-ast)

  (defprimop core:instance-rack (:object) (:object)
    cc-ast:instance-rack-ast cleavir-ast:object-ast)
  (defprimop core:instance-rack-set (:object :object) ()
    cc-ast:instance-rack-set-ast cleavir-ast:object-ast cleavir-ast:value-ast)

  (defprimop core:rack-ref (:object :object) (:object)
    cc-ast:rack-read-ast cleavir-ast:object-ast cleavir-ast:slot-number-ast)
  (defprimop core:rack-set (:object :object :object) ()
    cc-ast:rack-write-ast cleavir-ast:object-ast cleavir-ast:slot-number-ast
    cleavir-ast:value-ast)

  (defprimop core:vaslist-pop (:object) (:object)
    cc-ast:vaslist-pop-ast cleavir-ast:arg-ast)
  (defprimop core:vaslist-length (:object) (:object)
    cc-ast:vaslist-length-ast cleavir-ast:arg-ast)

  (defprimop core::header-stamp (:object) (:object)
    cc-ast:header-stamp-ast cleavir-ast:arg-ast)
  (defprimop core::derivable-stamp (:object) (:object)
    cc-ast:derivable-stamp-ast cleavir-ast:arg-ast)
  (defprimop core::wrapped-stamp (:object) (:object)
    cc-ast:wrapped-stamp-ast cleavir-ast:arg-ast)
  (defprimop core::rack-stamp (:object) (:object)
    cc-ast:rack-stamp-ast cleavir-ast:arg-ast))
