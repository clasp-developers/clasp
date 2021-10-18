(in-package #:clasp-cleavir)

;;; A "primop" is something that can be "called" like a function (all its
;;; arguments are evaluated) but which is specially translated by the compiler.

;;; Called by translate-simple-instruction. Return value irrelevant.
(defgeneric translate-primop (opname instruction))
;;; Called by translate-conditional-test
(defgeneric translate-conditional-primop (opname instruction next)
  (:method (opname (instruction bir:vprimop) next)
    (declare (ignore opname))
    ;; Like the default method on translate-conditional-test, compare the output
    ;; against NIL.
    (cmp:irc-cond-br
     (cmp:irc-icmp-eq (in (first (bir:outputs instruction))) (%nil))
     (second next) (first next))))

;;; Hash table from primop infos to rtype info.
;;; An rtype info is just a list (return-rtype argument-rtypes...)
;;; If there is no entry in the table, it's assumed to return (:object)
;;; and take :object arguments.
;;; See bir-to-bmir for more information about rtypes.
(defvar *primop-rtypes* (make-hash-table :test #'eq))

(defun primop-rtype-info (primop-info)
  (or (gethash primop-info *primop-rtypes*)
      (list* '(:object)
             (make-list (cleavir-primop-info:ninputs primop-info)
                        :initial-element :object))))

;;; Define a primop that returns values.
;;; param-info is either (return-rtype param-rtypes...) or an integer; the
;;; latter is short for taking that many :objects and returning (:object).
;;; For example, (defvprimop foo 2)
;;;              = (defvprimop foo ((:object) :object :object))
;;; The BODY is used as a translate-primop method, where the call instruction
;;; is available bound to INSTPARAM.
(defmacro defvprimop (name param-info (instparam) &body body)
  (let ((param-info (if (integerp param-info)
                        (list* '(:object) (make-list param-info
                                                     :initial-element :object))
                        param-info))
        (nsym (gensym "NAME")))
    `(progn
       (cleavir-primop-info:defprimop ,name ,(length (rest param-info)) :value)
       (setf (gethash (cleavir-primop-info:info ',name) *primop-rtypes*)
             '(,@param-info))
       (defmethod translate-primop ((,nsym (eql ',name)) ,instparam)
         ,@body)
       ',name)))

;;; Like defvprimop for the case where the body is just an intrinsic.
(defmacro defvprimop-intrinsic (name param-info intrinsic)
  ;; TODO: Assert argument types? Or maybe that should be done at a lower level
  ;; in irc-intrinsic-etc.
  `(defvprimop ,name ,param-info (call)
     (%intrinsic-invoke-if-landing-pad-or-call
      ,intrinsic (mapcar #'in (rest (bir:inputs call))))))

;;; Define a primop called for effect.
;;; Here param-info is parameters only.
(defmacro defeprimop (name param-info (instparam) &body body)
  (let ((param-info
          (list* () (if (integerp param-info)
                        (make-list param-info :initial-element :object)
                        param-info)))
        (nsym (gensym "NAME")))
    `(progn
       (cleavir-primop-info:defprimop ,name ,(length (rest param-info)) :effect)
       (setf (gethash (cleavir-primop-info:info ',name) *primop-rtypes*)
             '(,@param-info))
       (defmethod translate-primop ((,nsym (eql ',name)) ,instparam)
         ,@body)
       ',name)))

;;; Define a primop used as a conditional test.
;;; Here param-info is parameters only.
;;; The body is used for translate-conditional-primop, which is expected to
;;; return an LLVM i1 Value.
(defmacro deftprimop (name param-info (instparam nextparam) &body body)
  (let ((param-info
          (list* '(:object)
                 (if (integerp param-info)
                     (make-list param-info :initial-element :object)
                     param-info)))
        (nsym (gensym "NAME")))
    `(progn
       (cleavir-primop-info:defprimop ,name ,(length (rest param-info)) :value)
       (setf (gethash (cleavir-primop-info:info ',name) *primop-rtypes*)
             '(,@param-info))
       (defmethod translate-primop ((,nsym (eql ',name)) ,instparam)
         (declare (ignore ,instparam)))
       (defmethod translate-conditional-primop ((,nsym (eql ',name))
                                                ,instparam ,nextparam)
         ,@body)
       ',name)))

;;;

(macrolet ((def-float-compare (sfname dfname op reversep)
             `(progn
                (deftprimop ,sfname (:single-float :single-float)
                  (inst next)
                  (assert (= (length (bir:inputs inst)) 2))
                  (let ((,(if reversep 'i2 'i1)
                          (in (first (bir:inputs inst))))
                        (,(if reversep 'i1 'i2)
                          (in (second (bir:inputs inst)))))
                    (assert (llvm-sys:type-equal (llvm-sys:get-type i1)
                                                 cmp:%float%))
                    (assert (llvm-sys:type-equal (llvm-sys:get-type i2)
                                                 cmp:%float%))
                    (cmp:irc-cond-br (,op i1 i2) (first next) (second next))))
                (deftprimop ,dfname (:double-float :double-float)
                  (inst next)
                  (assert (= (length (bir:inputs inst)) 2))
                  (let ((,(if reversep 'i2 'i1)
                          (in (first (bir:inputs inst))))
                        (,(if reversep 'i1 'i2)
                          (in (second (bir:inputs inst)))))
                    (assert (llvm-sys:type-equal (llvm-sys:get-type i1)
                                                 cmp:%double%))
                    (assert (llvm-sys:type-equal (llvm-sys:get-type i2)
                                                 cmp:%double%))
                    (cmp:irc-cond-br (,op i1 i2) (first next) (second next)))))))
  (def-float-compare core::two-arg-sf-=  core::two-arg-df-=  %fcmp-oeq nil)
  (def-float-compare core::two-arg-sf-<  core::two-arg-df-<  %fcmp-olt nil)
  (def-float-compare core::two-arg-sf-<= core::two-arg-df-<= %fcmp-ole nil)
  (def-float-compare core::two-arg-sf->  core::two-arg-df->  %fcmp-olt   t)
  (def-float-compare core::two-arg-sf->= core::two-arg-df->= %fcmp-ole   t))

(macrolet ((def-float-unop (sfname sfintrinsic dfname dfintrinsic)
             `(progn
                (defvprimop-intrinsic ,sfname ((:single-float) :single-float)
                  ,sfintrinsic)
                (defvprimop-intrinsic ,dfname ((:double-float) :double-float)
                  ,dfintrinsic))))
  (def-float-unop core::sf-abs   "llvm.fabs.f32" core::df-abs   "llvm.fabs.f64")
  (def-float-unop core::sf-sqrt  "llvm.sqrt.f32" core::df-sqrt  "llvm.sqrt.f64")
  (def-float-unop core::sf-exp   "llvm.exp.f32"  core::df-exp   "llvm.exp.f64")
  (def-float-unop core::sf-log   "llvm.log.f32"  core::df-log   "llvm.log.f64")
  (def-float-unop core::sf-cos   "llvm.cos.f32"  core::df-cos   "llvm.cos.f64")
  (def-float-unop core::sf-sin   "llvm.sin.f32"  core::df-sin   "llvm.sin.f64")
  (def-float-unop core::sf-tan   "tanf"          core::df-tan   "tan")
  (def-float-unop core::sf-acos  "acosf"         core::df-acos  "acos")
  (def-float-unop core::sf-asin  "asinf"         core::df-asin  "asin")
  (def-float-unop core::sf-cosh  "coshf"         core::df-cosh  "cosh")
  (def-float-unop core::sf-sinh  "sinhf"         core::df-sinh  "sinh")
  (def-float-unop core::sf-tanh  "tanhf"         core::df-tanh  "tanh")
  (def-float-unop core::sf-acosh "acoshf"        core::df-acosh "acosh")
  (def-float-unop core::sf-asinh "asinhf"        core::df-asinh "asinh")
  (def-float-unop core::sf-atanh "atanhf"        core::df-atanh "atanh"))

(macrolet ((def-float-binop-op (sfname dfname ircop)
             `(progn
                (defvprimop ,sfname ((:single-float) :single-float :single-float)
                  (inst)
                  (assert (= 2 (length (bir:inputs inst))))
                  (let ((i1 (in (first (bir:inputs inst))))
                        (i2 (in (second (bir:inputs inst)))))
                    (assert (llvm-sys:type-equal (llvm-sys:get-type i1)
                                                 cmp:%float%))
                    (assert (llvm-sys:type-equal (llvm-sys:get-type i2)
                                                 cmp:%float%))
                    (out (,ircop i1 i2) (first (bir:outputs inst)))))
                (defvprimop ,dfname ((:double-float) :double-float :double-float)
                  (inst)
                  (assert (= 2 (length (bir:inputs inst))))
                  (let ((i1 (in (first (bir:inputs inst))))
                        (i2 (in (second (bir:inputs inst)))))
                    (assert (llvm-sys:type-equal (llvm-sys:get-type i1)
                                                 cmp:%double%))
                    (assert (llvm-sys:type-equal (llvm-sys:get-type i2)
                                                 cmp:%double%))
                    (out (,ircop i1 i2) (first (bir:outputs inst)))))))
           (def-float-binop-i (sfname sfintrinsic dfname dfintrinsic)
             `(progn
                (defvprimop-intrinsic ,sfname
                    ((:single-float) :single-float :single-float)
                  ,sfintrinsic)
                (defvprimop-intrinsic ,dfname
                    ((:double-float) :double-float :double-float)
                  ,dfintrinsic))))
  (def-float-binop-op core::two-arg-sf-+ core::two-arg-df-+ %fadd)
  (def-float-binop-op core::two-arg-sf-- core::two-arg-df-- %fsub)
  (def-float-binop-op core::two-arg-sf-* core::two-arg-df-* %fmul)
  (def-float-binop-op core::two-arg-sf-/ core::two-arg-df-/ %fdiv)
  (def-float-binop-i core::sf-expt "llvm.pow.f32" core::df-expt "llvm.pow.f64"))

(defvprimop core::sf-ftruncate ((:single-float :single-float)
                                :single-float :single-float)
  (inst)
  (assert (= 2 (length (bir:inputs inst))))
  (let ((i1 (in (first (bir:inputs inst))))
        (i2 (in (second (bir:inputs inst)))))
    (assert (llvm-sys:type-equal (llvm-sys:get-type i1) cmp:%float%))
    (assert (llvm-sys:type-equal (llvm-sys:get-type i2) cmp:%float%))
    ;; I think this is the best instruction sequence, but I am not sure.
    (out (list (%intrinsic-call "llvm.trunc.f32" (list (%fdiv i1 i2)))
               (%frem i1 i2))
         (first (bir:outputs inst)))))
(defvprimop core::df-ftruncate ((:double-float :double-float)
                                :double-float :double-float)
  (inst)
  (assert (= 2 (length (bir:inputs inst))))
  (let ((i1 (in (first (bir:inputs inst))))
        (i2 (in (second (bir:inputs inst)))))
    (assert (llvm-sys:type-equal (llvm-sys:get-type i1) cmp:%double%))
    (assert (llvm-sys:type-equal (llvm-sys:get-type i2) cmp:%double%))
    (out (list (%intrinsic-call "llvm.trunc.f64" (list (%fdiv i1 i2)))
               (%frem i1 i2))
         (first (bir:outputs inst)))))

(defvprimop core::sf-negate ((:single-float) :single-float) (inst)
  (assert (= 1 (length (bir:inputs inst))))
  (let ((arg (in (first (bir:inputs inst)))))
    (assert (llvm-sys:type-equal (llvm-sys:get-type arg) cmp:%float%))
    (out (%fneg arg) (first (bir:outputs inst)))))
(defvprimop core::df-negate ((:double-float) :double-float) (inst)
  (assert (= 1 (length (bir:inputs inst))))
  (let ((arg (in (first (bir:inputs inst)))))
    (assert (llvm-sys:type-equal (llvm-sys:get-type arg) cmp:%double%))
    (out (%fneg arg) (first (bir:outputs inst)))))

(defvprimop core::single-to-double ((:double-float) :single-float) (inst)
  (assert (= 1 (length (bir:inputs inst))))
  (let ((arg (in (first (bir:inputs inst)))))
    (assert (llvm-sys:type-equal (llvm-sys:get-type arg) cmp:%float%))
    (out (%fpext arg cmp:%double%) (first (bir:outputs inst)))))
(defvprimop core::double-to-single ((:single-float) :double-float) (inst)
  (assert (= 1 (length (bir:inputs inst))))
  (let ((arg (in (first (bir:inputs inst)))))
    (assert (llvm-sys:type-equal (llvm-sys:get-type arg) cmp:%double%))
    (out (%fptrunc arg cmp:%float%) (first (bir:outputs inst)))))

(defvprimop core::sf-vref ((:single-float) :object :object) (inst)
  (out (%intrinsic-invoke-if-landing-pad-or-call
        "cc_simpleFloatVectorAref" (mapcar #'in (bir:inputs inst)))
       (first (bir:outputs inst))))
(defvprimop core::df-aref ((:double-float) :object :object) (inst)
  (out (%intrinsic-invoke-if-landing-pad-or-call
        "cc_simpleDoubleVectorAref" (mapcar #'in (bir:inputs inst)))
       (first (bir:outputs inst))))

;;; These return the new value because it's a bit involved to rewrite BIR to use
;;; a linear datum more than once.
(defvprimop core::sf-vset ((:single-float) :single-float :object :object) (inst)
  (let ((args (mapcar #'in (bir:inputs inst))))
    (assert (llvm-sys:type-equal (llvm-sys:get-type (first args)) cmp:%float%))
    (%intrinsic-invoke-if-landing-pad-or-call "cc_simpleFloatVectorAset" args)
    (out (first args) (first (bir:outputs inst)))))
(defvprimop core::df-vset ((:double-float) :double-float :object :object) (inst)
  (let ((args (mapcar #'in (bir:inputs inst))))
    (assert (llvm-sys:type-equal (llvm-sys:get-type (first args)) cmp:%double%))
    (%intrinsic-invoke-if-landing-pad-or-call "cc_simpleDoubleVectorAset" args)
    (out (first args) (first (bir:outputs inst)))))
