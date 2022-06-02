(defpackage #:clasp-simd
  (:use #:cl)
  (:shadow #:+ #:- #:* #:/)
  (:local-nicknames ;;(#:cc #:clasp-cleavir)
                    (#:pinfo #:cleavir-primop-info)
                    (#:ast #:cleavir-ast)
                    (#:cst-to-ast #:cleavir-cst-to-ast)
                    (#:ast-to-bir #:cleavir-ast-to-bir)
                    (#:bir #:cleavir-bir)
                    (#:bir-to-bmir #:cc-bir-to-bmir))
  (:export #:dvector #:svector
           #:+ #:- #:* #:/
           #:reduce+ #:reduce*))

(in-package #:clasp-simd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Runtime definitions. These are the functional versions of the API, used
;;; only when the compiler fails to optimize them into machine operations.

(defun dvector (&rest args)
  "Create a SIMD vector of double floats."
  (coerce args '(simple-array double-float (cl:*))))
(defun svector (&rest args)
  "Create a SIMD vector of single floats."
  (coerce args '(simple-array single-float (cl:*))))

(macrolet ((defarith (op two-arg underlying)
             `(progn
                (defun ,op (&rest args) (cl:reduce #',two-arg args))
                (define-compiler-macro ,op (&rest args)
                  (case (length args)
                    ((1) (first args))
                    ((2) (list ',two-arg (first args) (second args)))
                    (otherwise
                     (cl:reduce (lambda (f1 f2) (list ',two-arg f1 f2))
                                args))))
                (defun ,two-arg (vec1 vec2)
                  (cond ((and (typep vec1 '(simple-array double-float (cl:*)))
                              (typep vec2 '(simple-array double-float (cl:*)))
                              (= (length vec1) (length vec2)))
                         (map '(simple-array double-float (cl:*))
                              #',underlying vec1 vec2))
                        ((and (typep vec1 '(simple-array single-float (cl:*)))
                              (typep vec2 '(simple-array single-float (cl:*)))
                              (= (length vec1) (length vec2)))
                         (map '(simple-array single-float (cl:*))
                              #',underlying vec1 vec2))
                        (t (error "Bad operands to ~s: ~s ~s"
                                  ',op vec1 vec2)))))))
  (defarith + +/2 cl:+)
  (defarith - -/2 cl:-)
  (defarith * */2 cl:*)
  (defarith / //2 cl:/))

(macrolet ((defred (op underlying)
             `(defun ,op (initial-value vector)
                (cl:reduce #',underlying vector :initial-value initial-value))))
  (defred reduce+ cl:+)
  (defred reduce* cl:*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiler hooks
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Vector rtype

(defstruct (vector-rtype
            (:constructor make-vector-rtype (length value-rtype))
            (:constructor make-dvec-rtype
                (length &aux (value-rtype :double-float)))
            (:constructor make-svec-rtype
                (length &aux (value-rtype :single-float))))
  (length (error "length required") :read-only t :type (integer 1))
  (value-rtype (error "value-rtype required") :read-only t))

(defmethod bir-to-bmir::min-vrtype ((vrt1 vector-rtype) (vrt2 vector-rtype))
  (if (and (eql (vector-rtype-length vrt1) (vector-rtype-length vrt2))
           (eql (vector-rtype-value-rtype vrt1) (vector-rtype-value-rtype vrt2)))
      vrt1
      (call-next-method)))

(defmethod bir-to-bmir::max-vrtype ((vrt1 vector-rtype) (vrt2 vector-rtype))
  (if (and (eql (vector-rtype-length vrt1) (vector-rtype-length vrt2))
           (eql (vector-rtype-value-rtype vrt1) (vector-rtype-value-rtype vrt2)))
      vrt1
      (call-next-method)))

(defmethod cc::vrtype->llvm ((vrtype vector-rtype))
  (llvm-sys:vector-type-get
   (cc::vrtype->llvm (vector-rtype-value-rtype vrtype))
   (vector-rtype-length vrtype) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DVECTOR and SVECTOR IR

(defun convert-args (arguments-cst env system)
  (loop for remaining = arguments-cst then (cst:rest remaining)
        until (cst:null remaining)
        collect (cst-to-ast:convert (cst:first remaining) env system)))

(defmethod cst-to-ast:convert-special ((symbol (eql '%svector)) cst env system)
  (cst:db origin (op-cst . arguments-cst) cst
    (declare (ignore op-cst))
    (let ((args (convert-args arguments-cst env system)))
      (make-instance 'vector-ast
        :origin origin :rtype (make-svec-rtype (length args))
        :argument-asts args))))
(defmethod cst-to-ast:convert-special ((symbol (eql '%dvector)) cst env system)
  (cst:db origin (op-cst . arguments-cst) cst
    (declare (ignore op-cst))
    (let ((args (convert-args arguments-cst env system)))
      (make-instance 'vector-ast
        :origin origin :rtype (make-dvec-rtype (length args))
        :argument-asts args))))

(cc::define-to-be-special-operator %svector)
(cc::define-to-be-special-operator %dvector)

(defclass vector-ast (ast:ast)
  ((%rtype :initarg :rtype :reader rtype)
   (%argument-asts :initarg :argument-asts :reader ast:argument-asts)))

(defmethod ast-to-bir:compile-ast ((ast vector-ast) inserter system)
  (ast-to-bir:with-compiled-arguments (args (ast:argument-asts ast) inserter system)
    (let ((output (make-instance 'bir:output)))
      (ast-to-bir:insert inserter 'vectori
                         :rtype (rtype ast)
                         :inputs args :outputs (list output))
      (list output))))

(defclass vectori (bir:one-output bir:instruction)
  ((%rtype :initarg :rtype :reader rtype)))

(defun vrtype (inst) (vector-rtype-value-rtype (rtype inst)))

(defmethod cc::translate-simple-instruction ((inst vectori) abi)
  (declare (ignore abi))
  (let* ((inputs (bir:inputs inst))
         (length (length inputs))
         (ltype (cc::vrtype->llvm (vrtype inst)))
         (vec (llvm-sys:undef-value-get (llvm-sys:vector-type-get ltype length nil))))
    (loop for i from 0
          for li = (cc::%i32 i)
          for inp in inputs
          for in = (cc::in inp)
          do (assert (llvm-sys:type-equal (llvm-sys:get-type in) ltype))
             (setf vec (cmp:irc-insert-element vec in li)))
    (cc::out vec (bir:output inst))))

(defmethod bir-to-bmir::%use-rtype ((inst vectori) (datum bir:datum))
  (list (vrtype inst)))

(defmethod bir-to-bmir::%definition-rtype ((inst vectori) (datum bir:datum))
  (list (rtype inst)))

(defmethod bir-to-bmir::insert-casts ((inst vectori))
  (bir-to-bmir::cast-inputs inst (list (vrtype inst)))
  (bir-to-bmir::maybe-cast-after inst (bir:output inst) (list (rtype inst))))

;;; Chosen because this is the basics of what SSE2 supports.
(cc::deftransform dvector ((d1 double-float) (d2 double-float))
  '(cc::truly-the (simple-array double-float (2)) (%dvector d1 d2)))
(cc::deftransform svector ((s1 single-float) (s2 single-float)
                           (s3 single-float) (s4 single-float))
  '(cc::truly-the (simple-array single-float (4)) (%svector s1 s2 s3 s4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Arithmetic primops and transforms

(macrolet ((defprimop (name length vrtype op)
             `(let ((rtype (make-vector-rtype ,length ,vrtype)))
                (cleavir-primop-info:defprimop ,name 2 :value :flushable)
                (setf (gethash ',name cc::*primop-rtypes*)
                      (list* (list rtype) (make-list 2 :initial-element rtype)))
                (defmethod cc::translate-primop ((name (eql ',name)) inst)
                  (let ((inputs (bir:inputs inst)))
                    (assert (= 2 (length inputs)))
                    (let ((i1 (cc::in (first (bir:inputs inst))))
                          (i2 (cc::in (first (bir:inputs inst))))
                          (vtype (llvm-sys:vector-type-get
                                  (cc::vrtype->llvm ,vrtype) ,length nil)))
                      (assert (llvm-sys:type-equal (llvm-sys:get-type i1) vtype))
                      (assert (llvm-sys:type-equal (llvm-sys:get-type i2) vtype))
                      (cc::out (,op i1 i2) (first (bir:outputs inst))))))))
           (defprimops (&rest specs)
             `(progn ,@(loop for spec in specs collect `(defprimop ,@spec)))))
  (defprimops
      (%+.sf.4 4 :single-float cc::%fadd)
      (%-.sf.4 4 :single-float cc::%fsub)
    (%*.sf.4 4 :single-float cc::%fmul)
    (%*.sf.4 4 :single-float cc::%fdiv)
    (%+.df.2 2 :double-float cc::%fadd)
    (%-.df.2 2 :double-float cc::%fsub)
    (%*.df.2 2 :double-float cc::%fmul)
    (%/.df.2 2 :double-float cc::%fdiv)))

(macrolet ((deftransform (name type primop)
             `(cc::deftransform ,name ((v1 ,type) (v2 ,type))
                '(cc::truly-the ,type (core::primop ,primop v1 v2))))
           (defsf4 (name primop) `(deftransform ,name (simple-array single-float (4)) ,primop))
           (defdf2 (name primop) `(deftransform ,name (simple-array double-float (2)) ,primop)))
  (defsf4 +/2 %+.sf.4)
  (defsf4 -/2 %-.sf.4)
  (defsf4 */2 %*.sf.4)
  (defsf4 //2 %/.sf.4)
  (defdf2 +/2 %+.df.2)
  (defdf2 -/2 %-.df.2)
  (defdf2 */2 %*.df.2)
  (defdf2 //2 %/.df.2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reduction primops and transforms

(macrolet ((defprimop (name length vrtype intrinsic)
             `(let ((rtype (make-vector-rtype ,length ,vrtype)))
                (cleavir-primop-info:defprimop ,name 2 :value :flushable)
                (setf (gethash ',name cc::*primop-rtypes*) (list (list ,vrtype) ,vrtype rtype))
                (defmethod cc::translate-primop ((name (eql ',name)) inst)
                  (cc::out (cc::%intrinsic-call ,intrinsic
                                                (mapcar #'cc::in (bir:inputs inst)))
                           (first (bir:outputs inst)))))))
  (defprimop %reduce+.sf.4 4 :single-float "llvm.vector.reduce.fadd.v4f32")
  (defprimop %reduce*.sf.4 4 :single-float "llvm.vector.reduce.fmul.v4f32")
  (defprimop %reduce+.df.2 2 :double-float "llvm.vector.reduce.fadd.v2f64")
  (defprimop %reduce*.df.2 2 :double-float "llvm.vector.reduce.fmul.v2f64"))

(macrolet ((deftransform (name length vtype primop)
             `(cc::deftransform ,name ((init ,vtype) (vec (simple-array ,vtype (,length))))
                '(cc::truly-the ,vtype (core::primop ,primop init vec)))))
  (deftransform reduce+ 4 single-float %reduce+.sf.4)
  (deftransform reduce* 4 single-float %reduce*.sf.4)
  (deftransform reduce+ 2 double-float %reduce+.df.2)
  (deftransform reduce* 2 double-float %reduce*.df.2))
