(in-package #:cc-bir)

(defclass unwind-protect (bir:dynamic-environment bir:one-input bir:no-output
                          bir:terminator1)
  ())

(defmethod ast-to-bir:compile-ast ((ast cc-ast:unwind-protect-ast)
                                   inserter system)
  (ast-to-bir:with-compiled-ast (fu (cc-ast:cleanup-ast ast) inserter system)
    (let* ((uw (make-instance 'unwind-protect :inputs fu))
           (ode (ast-to-bir:dynamic-environment inserter))
           (during (ast-to-bir:make-iblock
                    inserter :dynamic-environment uw)))
      (setf (bir:next uw) (list during))
      (ast-to-bir:terminate inserter uw)
      (ast-to-bir:begin inserter during)
      (let ((rv (ast-to-bir:compile-ast (cleavir-ast:body-ast ast)
                                        inserter system)))
        (if (eq rv :no-return)
            :no-return
            ;; We need to pass the values through a phi so that
            ;; unwind-dynenv can deal with them. KLUDGEy?
            (let* ((next (ast-to-bir:make-iblock
                          inserter :dynamic-environment ode))
                   (phi (make-instance 'bir:phi
                          :iblock next)))
              (setf (bir:inputs next) (list phi))
              (ast-to-bir:terminate
               inserter
               (make-instance 'bir:jump
                 :inputs rv :outputs (list phi) :next (list next)))
              (ast-to-bir:begin inserter next)
              (list phi)))))))

(defclass bind (bir:dynamic-environment bir:no-output bir:terminator1) ())

(defmethod ast-to-bir:compile-ast ((ast cc-ast:bind-ast) inserter system)
  (ast-to-bir:with-compiled-asts (args ((cleavir-ast:name-ast ast)
                                        (cleavir-ast:value-ast ast))
                                       inserter system)
    (let* ((during (ast-to-bir:make-iblock inserter))
           (ode (ast-to-bir:dynamic-environment inserter))
           (bind (make-instance 'bind :inputs args :next (list during))))
      (setf (bir:dynamic-environment during) bind)
      (ast-to-bir:terminate inserter bind)
      (ast-to-bir:begin inserter during)
      (let ((rv (ast-to-bir:compile-ast (cleavir-ast:body-ast ast)
                                        inserter system)))
        (cond ((eq rv :no-return) :no-return)
              (t
               (let ((next (ast-to-bir:make-iblock
                            inserter :dynamic-environment ode)))
                 (ast-to-bir:terminate
                  inserter
                  (make-instance 'bir:jump
                    :inputs () :outputs () :next (list next)))
                 (ast-to-bir:begin inserter next))
               rv))))))

(defclass mv-foreign-call (bir:one-output bir:instruction)
  ((%function-name :initarg :function-name :reader function-name)))

(defmethod ast-to-bir:compile-ast
    ((ast cc-ast:multiple-value-foreign-call-ast) inserter system)
  (ast-to-bir:with-compiled-arguments (args (cc-ast:argument-asts ast)
                                            inserter system)
    (let ((output (make-instance 'bir:output)))
      (ast-to-bir:insert
       inserter
       (make-instance 'mv-foreign-call
         :function-name (cc-ast:function-name ast)
         :inputs args :outputs (list output)))
      (list output))))

(defclass foreign-call-pointer (bir:one-output bir:instruction)
  ((%foreign-types :initarg :foreign-types :accessor foreign-types)))

(defmethod ast-to-bir:compile-ast
    ((ast cc-ast:foreign-call-pointer-ast) inserter system)
  (ast-to-bir:with-compiled-arguments (args (cc-ast:argument-asts ast)
                                            inserter system)
    (let ((out (make-instance 'bir:output)))
      (ast-to-bir:insert
       inserter
       (make-instance 'foreign-call-pointer
         :foreign-types (cc-ast:foreign-types ast)
         :inputs args :outputs (list out)))
      (list out))))

(defclass defcallback (bir:no-output bir:instruction)
  ((%args :initarg :args :reader defcallback-args)))

(defmethod ast-to-bir:compile-ast ((ast cc-ast:defcallback-ast)
                                   inserter system)
  (ast-to-bir:with-compiled-ast (rv (cleavir-ast:callee-ast ast)
                                    inserter system)
    (ast-to-bir:insert
     inserter
     (make-instance 'defcallback
       :args (cc-ast:defcallback-args ast)
       :inputs rv :outputs ())))
  ())

(defclass header-stamp-case (bir:one-input bir:no-output bir:terminator) ())

(defmethod ast-to-bir:compile-test-ast
    ((ast cc-ast:header-stamp-case-ast) inserter system)
  (ast-to-bir:with-compiled-ast (rv (cc-ast:stamp-ast ast)
                                    inserter system)
    (let* ((ibs
             (loop repeat 4 collect (ast-to-bir:make-iblock inserter)))
           (hsc (make-instance 'header-stamp-case
                  :next ibs :inputs rv)))
      (ast-to-bir:terminate inserter hsc)
      (copy-list ibs))))

;;; atomics

(defclass atomic (bir:instruction)
  ((%order :initarg :order :reader order :initform :relaxed
           :type (member :relaxed :acquire :release :acquire-release
                         :sequentially-consistent))))

(defclass fence (atomic bir:no-input bir:no-output bir:instruction) ())

;;; we just make the bmir directly for atomic car and cdr

(defmethod ast-to-bir:compile-ast ((ast cc-ast:fence-ast) inserter system)
  (declare (ignore system))
  (ast-to-bir:insert
   inserter
   (make-instance 'cc-bir:fence :order (cc-ast:order ast)))
  ())

(defmethod ast-to-bir:compile-ast ((ast cc-ast:atomic-car-ast) inserter system)
  (ast-to-bir:with-compiled-asts (args ((cleavir-ast:cons-ast ast))
                                       inserter system)
    (let ((mr2-out (make-instance 'bir:output))
          (out (make-instance 'bir:output)))
      (ast-to-bir:insert
       inserter
       (make-instance 'cc-bmir:memref2
         :inputs args :outputs (list mr2-out)
         :offset (- cmp:+cons-car-offset+ cmp:+cons-tag+)))
      (ast-to-bir:insert
       inserter
       (make-instance 'cc-bmir:load
         :order (cc-ast:order ast) :inputs (list mr2-out) :outputs (list out)))
      (list out))))
(defmethod ast-to-bir:compile-ast ((ast cc-ast:atomic-cdr-ast) inserter system)
  (ast-to-bir:with-compiled-asts (args ((cleavir-ast:cons-ast ast))
                                       inserter system)
    (let ((mr2-out (make-instance 'bir:output))
          (out (make-instance 'bir:output)))
      (ast-to-bir:insert
       inserter
       (make-instance 'cc-bmir:memref2
         :inputs args :outputs (list mr2-out)
         :offset (- cmp:+cons-cdr-offset+ cmp:+cons-tag+)))
      (ast-to-bir:insert
       inserter
       (make-instance 'cc-bmir:load
         :order (cc-ast:order ast) :inputs (list mr2-out) :outputs (list out)))
      (list out))))
(defmethod ast-to-bir:compile-ast ((ast cc-ast:atomic-rplaca-ast)
                                   inserter system)
  (ast-to-bir:with-compiled-asts (args ((cleavir-ast:object-ast ast)
                                        (cleavir-ast:cons-ast ast))
                                       inserter system)
    (let ((mr2-out (make-instance 'bir:output)))
      (ast-to-bir:insert
       inserter
       (make-instance 'cc-bmir:memref2
         :inputs (list (second args)) :outputs (list mr2-out)
         :offset (- cmp:+cons-car-offset+ cmp:+cons-tag+)))
      (ast-to-bir:insert
       inserter
       (make-instance 'cc-bmir:store
         :order (cc-ast:order ast)
         :inputs (list (first args) mr2-out)))))
  :no-value)
(defmethod ast-to-bir:compile-ast ((ast cc-ast:atomic-rplacd-ast)
                                   inserter system)
  (ast-to-bir:with-compiled-asts (args ((cleavir-ast:object-ast ast)
                                        (cleavir-ast:cons-ast ast))
                                       inserter system)
    (let ((mr2-out (make-instance 'bir:output)))
      (ast-to-bir:insert
       inserter
       (make-instance 'cc-bmir:memref2
         :inputs (list (second args)) :outputs (list mr2-out)
         :offset (- cmp:+cons-cdr-offset+ cmp:+cons-tag+)))
      (ast-to-bir:insert
       inserter
       (make-instance 'cc-bmir:store
         :order (cc-ast:order ast)
         :inputs (list (first args) mr2-out)))))
  :no-value)
(defmethod ast-to-bir:compile-ast ((ast cc-ast:cas-car-ast) inserter system)
  (ast-to-bir:with-compiled-asts (args ((cc-ast:cmp-ast ast)
                                        (cleavir-ast:value-ast ast)
                                        (cleavir-ast:cons-ast ast))
                                       inserter system)
    (let ((mr2-out (make-instance 'bir:output))
          (out (make-instance 'bir:output)))
      (ast-to-bir:insert
       inserter
       (make-instance 'cc-bmir:memref2
         :inputs (list (third args)) :outputs (list mr2-out)
         :offset (- cmp:+cons-car-offset+ cmp:+cons-tag+)))
      (ast-to-bir:insert
       inserter
       (make-instance 'cc-bmir:cas
         :order (cc-ast:order ast)
         :outputs (list out) :inputs (list mr2-out (first args) (second args))))
      (list out))))
(defmethod ast-to-bir:compile-ast ((ast cc-ast:cas-cdr-ast) inserter system)
  (ast-to-bir:with-compiled-asts (args ((cc-ast:cmp-ast ast)
                                        (cleavir-ast:value-ast ast)
                                        (cleavir-ast:cons-ast ast))
                                       inserter system)
    (let ((mr2-out (make-instance 'bir:output))
          (out (make-instance 'bir:output)))
      (ast-to-bir:insert
       inserter
       (make-instance 'cc-bmir:memref2
         :inputs (list (third args)) :outputs (list mr2-out)
         :offset (- cmp:+cons-cdr-offset+ cmp:+cons-tag+)))
      (ast-to-bir:insert
       inserter
       (make-instance 'cc-bmir:cas
         :order (cc-ast:order ast)
         :outputs (list out) :inputs (list mr2-out (first args) (second args))))
      (list out))))

(defclass atomic-rack-read (atomic bir:one-output bir:instruction) ())
(defclass atomic-rack-write (atomic bir:no-output bir:instruction) ())
(defclass cas-rack (atomic bir:one-output bir:instruction) ())

(defmethod ast-to-bir:compile-ast ((ast cc-ast:atomic-rack-read-ast)
                                   inserter system)
  (ast-to-bir:with-compiled-asts (args ((cc-ast:rack-ast ast)
                                        (cleavir-ast:slot-number-ast ast))
                                       inserter system)
    (let ((out (make-instance 'bir:output)))
      (ast-to-bir:insert
       inserter
       (make-instance 'atomic-rack-read
         :order (cc-ast:order ast) :inputs args :outputs (list out)))
      (list out))))
(defmethod ast-to-bir:compile-ast ((ast cc-ast:atomic-rack-write-ast)
                                   inserter system)
  (ast-to-bir:with-compiled-asts (args ((cleavir-ast:value-ast ast)
                                        (cc-ast:rack-ast ast)
                                        (cleavir-ast:slot-number-ast ast))
                                       inserter system)
    (ast-to-bir:insert
     inserter
     (make-instance 'atomic-rack-write :order (cc-ast:order ast) :inputs args)))
  :no-value)
(defmethod ast-to-bir:compile-ast ((ast cc-ast:cas-rack-ast) inserter system)
  (ast-to-bir:with-compiled-asts (args ((cc-ast:cmp-ast ast)
                                        (cleavir-ast:value-ast ast)
                                        (cc-ast:rack-ast ast)
                                        (cleavir-ast:slot-number-ast ast))
                                       inserter system)
    (let ((out (make-instance 'bir:output)))
      (ast-to-bir:insert
       inserter
       (make-instance 'cas-rack :order (cc-ast:order ast)
                      :inputs args :outputs (list out)))
      (list out))))

(defclass abstract-vref (bir:instruction)
  ((%element-type :initarg :element-type :reader element-type)))
(defclass vref (atomic bir:one-output abstract-vref) ())
(defclass vset (atomic bir:no-output abstract-vref) ())
(defclass vcas (atomic bir:one-output abstract-vref) ())

(defmethod ast-to-bir:compile-ast ((ast cc-ast:atomic-vref-ast) inserter system)
  (ast-to-bir:with-compiled-asts (args ((cleavir-ast:array-ast ast)
                                        (cleavir-ast:index-ast ast))
                                       inserter system)
    (let ((out (make-instance 'bir:output)))
      (ast-to-bir:insert
       inserter
       (make-instance 'vref
         :order (cc-ast:order ast) :inputs args :outputs (list out)
         :element-type (cleavir-ast:element-type ast)))
      (list out))))
(defmethod ast-to-bir:compile-ast ((ast cc-ast:atomic-vset-ast) inserter system)
  (ast-to-bir:with-compiled-asts (args ((cleavir-ast:value-ast ast)
                                        (cleavir-ast:array-ast ast)
                                        (cleavir-ast:index-ast ast))
                                       inserter system)
    (ast-to-bir:insert
     inserter
     (make-instance 'vset
       :order (cc-ast:order ast)
       :element-type (cleavir-ast:element-type ast) :inputs args)))
  :no-value)
(defmethod ast-to-bir:compile-ast ((ast cc-ast:vcas-ast) inserter system)
  (ast-to-bir:with-compiled-asts (args ((cc-ast:cmp-ast ast)
                                        (cleavir-ast:value-ast ast)
                                        (cleavir-ast:array-ast ast)
                                        (cleavir-ast:index-ast ast))
                                       inserter system)
    (let ((out (make-instance 'bir:output)))
      (ast-to-bir:insert
       inserter
       (make-instance 'vcas
         :order (cc-ast:order ast) :inputs args :outputs (list out)
         :element-type (cleavir-ast:element-type ast)))
      (list out))))

;;;

(macrolet ((defprimop (name ninputs out &rest rtype-info)
             `(progn
                (cleavir-primop-info:defprimop ,name ,ninputs ,out)
                ,@(when rtype-info
                    `((setf (gethash (cleavir-primop-info:info ',name)
                                     *primop-rtypes*)
                            '(,@rtype-info))))
                (cleavir-cst-to-ast:defprimop ,name))))
  (defprimop core::vector-length 1 :value)
  (defprimop core::%displacement 1 :value)
  (defprimop core::%displaced-index-offset 1 :value)
  (defprimop core::%array-total-size 1 :value)
  (defprimop core::%array-rank 1 :value)
  (defprimop core::%array-dimension 2 :value)

  (defprimop core:instance-rack 1 :value)
  (defprimop core:instance-rack-set 2 :effect)

  (defprimop core:rack-ref 2 :value)
  (defprimop core:rack-set 3 :effect)

  (defprimop core:vaslist-pop 1 :value)
  (defprimop core:vaslist-length 1 :value))

(macrolet ((defprimop (name ninputs out ast &rest readers)
             `(progn
                (cleavir-primop-info:defprimop ,name ,ninputs ,out)
                (ast-to-bir:defprimop ,name ,ast ,@readers))))
  (defprimop setf-fdefinition 1 :value
    cc-ast:setf-fdefinition-ast cleavir-ast:name-ast)
  
  (defprimop core::instance-cas 4 :value cc-ast:slot-cas-ast
    cc-ast:cmp-ast cleavir-ast:value-ast cleavir-ast:object-ast
    cleavir-ast:slot-number-ast)

  (defprimop core::header-stamp 1 :value
    cc-ast:header-stamp-ast cleavir-ast:arg-ast)
  (defprimop core::derivable-stamp 1 :value
    cc-ast:derivable-stamp-ast cleavir-ast:arg-ast)
  (defprimop core::wrapped-stamp 1 :value
    cc-ast:wrapped-stamp-ast cleavir-ast:arg-ast)
  (defprimop core::rack-stamp 1 :value
    cc-ast:rack-stamp-ast cleavir-ast:arg-ast))
