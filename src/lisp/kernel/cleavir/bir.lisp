(in-package #:cc-bir)

(defclass unwind-protect (cleavir-bir:dynamic-environment
                          cleavir-bir::one-input cleavir-bir::no-output
                          cleavir-bir:terminator1)
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
              ((listp rv)
               (let* ((next (cleavir-ast-to-bir:make-iblock
                             inserter :dynamic-environment ode)))
                 (cleavir-ast-to-bir:terminate
                  inserter
                  (make-instance 'cleavir-bir:jump
                    :inputs () :outputs () :next (list next)))
                 (cleavir-ast-to-bir:begin inserter next))
               rv)
              (t
               ;; We need to pass the values through a phi so that
               ;; unwind-dynenv can deal with them. KLUDGEy?
               (let* ((next (cleavir-ast-to-bir:make-iblock
                             inserter :dynamic-environment ode))
                      (phi (make-instance 'cleavir-bir:phi
                             :iblock next :rtype :multiple-values)))
                 (setf (cleavir-bir:inputs next) (list phi))
                 (cleavir-ast-to-bir:terminate
                  inserter
                  (make-instance 'cleavir-bir:jump
                    :inputs (cleavir-ast-to-bir:adapt
                             inserter rv :multiple-values)
                    :outputs (list phi) :next (list next)))
                 (cleavir-ast-to-bir:begin inserter next)
                 phi)))))))

(defclass bind (cleavir-bir:dynamic-environment cleavir-bir::no-output
                cleavir-bir:terminator1)
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
                    :inputs () :outputs () :next (list next)))
                 (cleavir-ast-to-bir:begin inserter next))
               rv))))))

(defclass mv-foreign-call (cleavir-bir::one-output cleavir-bir:instruction)
  ((%function-name :initarg :function-name :reader function-name)))

(defmethod cleavir-ast-to-bir:compile-ast
    ((ast cc-ast:multiple-value-foreign-call-ast) inserter system)
  (cleavir-ast-to-bir:with-compiled-arguments (args (cc-ast:argument-asts ast)
                                                    inserter system)
    (let ((output (make-instance 'cleavir-bir:output :rtype :multiple-values)))
      (cleavir-ast-to-bir:insert
       inserter
       (make-instance 'mv-foreign-call
         :function-name (cc-ast:function-name ast)
         :inputs (mapcar #'first args) :outputs (list output)))
      output)))

(defclass foreign-call-pointer (cleavir-bir::one-output cleavir-bir:instruction)
  ((%foreign-types :initarg :foreign-types :accessor foreign-types)))

(defmethod cleavir-ast-to-bir:compile-ast
    ((ast cc-ast:foreign-call-pointer-ast) inserter system)
  (cleavir-ast-to-bir:with-compiled-arguments (args (cc-ast:argument-asts ast)
                                                    inserter system)
    (let ((out (make-instance 'cleavir-bir:output)))
      (cleavir-ast-to-bir:insert
       inserter
       (make-instance 'foreign-call-pointer
         :foreign-types (cc-ast:foreign-types ast)
         :inputs (mapcar #'first args) :outputs (list out)))
      (list out))))

(defclass defcallback (cleavir-bir::no-output cleavir-bir:instruction)
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
                             cleavir-bir:terminator)
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

;;; atomics

(defclass atomic (cleavir-bir:instruction)
  ((%order :initarg :order :reader order :initform :relaxed
           :type (member :relaxed :acquire :release :acquire-release
                         :sequentially-consistent))))

(defclass fence (atomic cleavir-bir::no-input cleavir-bir::no-output
                 cleavir-bir:instruction)
  ())

;;; we just make the bmir directly for atomic car and cdr

(defmethod cleavir-ast-to-bir:compile-ast ((ast cc-ast:fence-ast)
                                           inserter system)
  (cleavir-ast-to-bir:insert
   inserter
   (make-instance 'cc-bir:fence :order (cc-ast:order ast)))
  ())

(defmethod cleavir-ast-to-bir:compile-ast ((ast cc-ast:atomic-car-ast)
                                           inserter system)
  (cleavir-ast-to-bir:with-compiled-asts (args ((cleavir-ast:cons-ast ast))
                                               inserter system
                                               (:object))
    (let* ((mr2-out (make-instance 'cleavir-bir:output :rtype :address))
           (memref2 (cleavir-ast-to-bir:insert
                     inserter
                     (make-instance 'cc-bmir:memref2
                       :inputs args :outputs (list mr2-out)
                       :offset (- cmp:+cons-car-offset+ cmp:+cons-tag+))))
           (out (make-instance 'cleavir-bir:output)))
      (cleavir-ast-to-bir:insert
       inserter
       (make-instance 'cc-bmir:load
         :order (cc-ast:order ast) :inputs (list mr2-out) :outputs (list out)))
      (list out))))
(defmethod cleavir-ast-to-bir:compile-ast ((ast cc-ast:atomic-cdr-ast)
                                           inserter system)
  (cleavir-ast-to-bir:with-compiled-asts (args ((cleavir-ast:cons-ast ast))
                                               inserter system
                                               (:object))
    (let* ((mr2-out (make-instance 'cleavir-bir:output :rtype :address))
           (memref2 (cleavir-ast-to-bir:insert
                     inserter
                     (make-instance 'cc-bmir:memref2
                       :inputs args :outputs (list mr2-out)
                       :offset (- cmp:+cons-cdr-offset+ cmp:+cons-tag+))))
           (out (make-instance 'cleavir-bir:output)))
      (cleavir-ast-to-bir:insert
       inserter
       (make-instance 'cc-bmir:load
         :order (cc-ast:order ast) :inputs (list mr2-out) :outputs (list out)))
      (list out))))
(defmethod cleavir-ast-to-bir:compile-ast ((ast cc-ast:atomic-rplaca-ast)
                                           inserter system)
  (cleavir-ast-to-bir:with-compiled-asts (args ((cleavir-ast:object-ast ast)
                                                (cleavir-ast:cons-ast ast))
                                               inserter system
                                               (:object :object))
    (let* ((mr2-out (make-instance 'cleavir-bir:output :rtype :address))
           (memref2 (cleavir-ast-to-bir:insert
                     inserter
                     (make-instance 'cc-bmir:memref2
                       :inputs (list (second args)) :outputs (list mr2-out)
                       :offset (- cmp:+cons-car-offset+ cmp:+cons-tag+)))))
      (cleavir-ast-to-bir:insert
       inserter
       (make-instance 'cc-bmir:store
         :order (cc-ast:order ast)
         :inputs (list (first args) mr2-out)))))
  ())
(defmethod cleavir-ast-to-bir:compile-ast ((ast cc-ast:atomic-rplacd-ast)
                                           inserter system)
  (cleavir-ast-to-bir:with-compiled-asts (args ((cleavir-ast:object-ast ast)
                                                (cleavir-ast:cons-ast ast))
                                               inserter system
                                               (:object :object))
    (let* ((mr2-out (make-instance 'cleavir-bir:output :rtype :address))
           (memref2 (cleavir-ast-to-bir:insert
                     inserter
                     (make-instance 'cc-bmir:memref2
                       :inputs (list (second args)) :outputs (list mr2-out)
                       :offset (- cmp:+cons-cdr-offset+ cmp:+cons-tag+)))))
      (cleavir-ast-to-bir:insert
       inserter
       (make-instance 'cc-bmir:store
         :order (cc-ast:order ast)
         :inputs (list (first args) mr2-out)))))
  ())
(defmethod cleavir-ast-to-bir:compile-ast ((ast cc-ast:cas-car-ast)
                                           inserter system)
  (cleavir-ast-to-bir:with-compiled-asts (args ((cc-ast:cmp-ast ast)
                                                (cleavir-ast:value-ast ast)
                                                (cleavir-ast:cons-ast ast))
                                               inserter system
                                               (:object :object :object))
    (let* ((mr2-out (make-instance 'cleavir-bir:output :rtype :address))
           (memref2 (cleavir-ast-to-bir:insert
                     inserter
                     (make-instance 'cc-bmir:memref2
                       :inputs (list (third args)) :outputs (list mr2-out)
                       :offset (- cmp:+cons-car-offset+ cmp:+cons-tag+))))
           (out (make-instance 'cleavir-bir:output)))
      (cleavir-ast-to-bir:insert
       inserter
       (make-instance 'cc-bmir:cas
         :order (cc-ast:order ast)
         :outputs (list out) :inputs (list mr2-out (first args) (second args))))
      (list out))))
(defmethod cleavir-ast-to-bir:compile-ast ((ast cc-ast:cas-cdr-ast)
                                           inserter system)
  (cleavir-ast-to-bir:with-compiled-asts (args ((cc-ast:cmp-ast ast)
                                                (cleavir-ast:value-ast ast)
                                                (cleavir-ast:cons-ast ast))
                                               inserter system
                                               (:object :object :object))
    (let ((mr2-out (make-instance 'cleavir-bir:output :rtype :address))
          (memref2 (cleavir-ast-to-bir:insert
                    inserter
                    (make-instance 'cc-bmir:memref2
                      :inputs (list (third args)) :outputs (list mr2-out)
                      :offset (- cmp:+cons-cdr-offset+ cmp:+cons-tag+))))
          (out (make-instance 'cleavir-bir:output)))
      (cleavir-ast-to-bir:insert
       inserter
       (make-instance 'cc-bmir:cas
         :order (cc-ast:order ast)
         :outputs (list out) :inputs (list mr2-out (first args) (second args))))
      (list out))))

(defclass atomic-rack-read (atomic cleavir-bir::one-output
                            cleavir-bir:instruction)
  ())
(defclass atomic-rack-write (atomic cleavir-bir::no-output
                             cleavir-bir:instruction)
  ())
(defclass cas-rack (atomic cleavir-bir::one-output cleavir-bir:instruction) ())

(defmethod cleavir-ast-to-bir:compile-ast ((ast cc-ast:atomic-rack-read-ast)
                                           inserter system)
  (cleavir-ast-to-bir:with-compiled-asts (args ((cc-ast:rack-ast ast)
                                                (cleavir-ast:slot-number-ast
                                                 ast))
                                               inserter system
                                               (:object :object))
    (let ((out (make-instance 'cleavir-bir:output)))
      (cleavir-ast-to-bir:insert
       inserter
       (make-instance 'atomic-rack-read
         :order (cc-ast:order ast) :inputs args :outputs (list out)))
      (list out))))
(defmethod cleavir-ast-to-bir:compile-ast ((ast cc-ast:atomic-rack-write-ast)
                                           inserter system)
  (cleavir-ast-to-bir:with-compiled-asts (args ((cleavir-ast:value-ast ast)
                                                (cc-ast:rack-ast ast)
                                                (cleavir-ast:slot-number-ast
                                                 ast))
                                               inserter system
                                               (:object :object :object))
    (cleavir-ast-to-bir:insert
     inserter
     (make-instance 'atomic-rack-write :order (cc-ast:order ast) :inputs args)))
  ())
(defmethod cleavir-ast-to-bir:compile-ast ((ast cc-ast:cas-rack-ast)
                                           inserter system)
  (cleavir-ast-to-bir:with-compiled-asts (args ((cc-ast:cmp-ast ast)
                                                (cleavir-ast:value-ast ast)
                                                (cc-ast:rack-ast ast)
                                                (cleavir-ast:slot-number-ast
                                                 ast))
                                               inserter system
                                               (:object :object
                                                :object :object))
    (let ((out (make-instance 'cleavir-bir:output)))
      (cleavir-ast-to-bir:insert
       inserter
       (make-instance 'cas-rack :order (cc-ast:order ast)
                      :inputs args :outputs (list out)))
      (list out))))

(defclass abstract-vref (cleavir-bir:instruction)
  ((%element-type :initarg :element-type :reader element-type)))
(defclass vref (atomic cleavir-bir::one-output abstract-vref) ())
(defclass vset (atomic cleavir-bir::no-output abstract-vref) ())
(defclass vcas (atomic cleavir-bir::one-output abstract-vref) ())

(defmethod cleavir-ast-to-bir:compile-ast ((ast cc-ast:atomic-vref-ast)
                                           inserter system)
  (cleavir-ast-to-bir:with-compiled-asts (args ((cleavir-ast:array-ast ast)
                                                (cleavir-ast:index-ast ast))
                                               inserter system
                                               (:object :object))
    (let ((out (make-instance 'cleavir-bir:output)))
      (cleavir-ast-to-bir:insert
       inserter
       (make-instance 'vref
         :order (cc-ast:order ast) :inputs args :outputs (list out)
         :element-type (cleavir-ast:element-type ast)))
      (list out))))
(defmethod cleavir-ast-to-bir:compile-ast ((ast cc-ast:atomic-vset-ast)
                                           inserter system)
  (cleavir-ast-to-bir:with-compiled-asts (args ((cleavir-ast:value-ast ast)
                                                (cleavir-ast:array-ast ast)
                                                (cleavir-ast:index-ast ast))
                                               inserter system
                                               (:object :object :object))
    (cleavir-ast-to-bir:insert
     inserter
     (make-instance 'vset
       :order (cc-ast:order ast)
       :element-type (cleavir-ast:element-type ast) :inputs args)))
  ())
(defmethod cleavir-ast-to-bir:compile-ast ((ast cc-ast:vcas-ast)
                                           inserter system)
  (cleavir-ast-to-bir:with-compiled-asts (args ((cc-ast:cmp-ast ast)
                                                (cleavir-ast:value-ast ast)
                                                (cleavir-ast:array-ast ast)
                                                (cleavir-ast:index-ast ast))
                                               inserter system
                                               (:object :object
                                                :object :object))
    (let ((out (make-instance 'cleavir-bir:output)))
      (cleavir-ast-to-bir:insert
       inserter
       (make-instance 'vcas
         :order (cc-ast:order ast) :inputs args :outputs (list out)
         :element-type (cleavir-ast:element-type ast)))
      (list out))))

;;;

(macrolet ((defprimop (name (&rest in) (&rest out))
             `(progn
                (cleavir-primop-info:defprimop ,name (,@in) (,@out))
                (cleavir-cst-to-ast:defprimop ,name))))
  (defprimop core::vector-length (:object) (:object))
  (defprimop core::%displacement (:object) (:object))
  (defprimop core::%displaced-index-offset (:object) (:object))
  (defprimop core::%array-total-size (:object) (:object))
  (defprimop core::%array-rank (:object) (:object))
  (defprimop core::%array-dimension (:object :object) (:object))

  (defprimop core:instance-rack (:object) (:object))
  (defprimop core:instance-rack-set (:object :object) ())

  (defprimop core:rack-ref (:object :object) (:object))
  (defprimop core:rack-set (:object :object :object) ())

  (defprimop core:vaslist-pop (:object) (:object))
  (defprimop core:vaslist-length (:object) (:object)))

(macrolet ((defprimop (name (&rest in) (&rest out) ast &rest readers)
             `(progn
                (cleavir-primop-info:defprimop ,name (,@in) (,@out))
                (cleavir-ast-to-bir:defprimop ,name ,ast ,@readers))))
  (defprimop setf-fdefinition (:object) (:object)
    cc-ast:setf-fdefinition-ast cleavir-ast:name-ast)
  
  (defprimop core::instance-cas (:object :object :object :object) (:object)
    cc-ast:slot-cas-ast
    cc-ast:cmp-ast cleavir-ast:value-ast cleavir-ast:object-ast
    cleavir-ast:slot-number-ast)

  (defprimop core::header-stamp (:object) (:object)
    cc-ast:header-stamp-ast cleavir-ast:arg-ast)
  (defprimop core::derivable-stamp (:object) (:object)
    cc-ast:derivable-stamp-ast cleavir-ast:arg-ast)
  (defprimop core::wrapped-stamp (:object) (:object)
    cc-ast:wrapped-stamp-ast cleavir-ast:arg-ast)
  (defprimop core::rack-stamp (:object) (:object)
    cc-ast:rack-stamp-ast cleavir-ast:arg-ast))
