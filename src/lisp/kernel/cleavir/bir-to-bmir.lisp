(in-package #:cc-bir-to-bmir)

;;; This file handles miscellaneous bir-to-bmir reductions.
;;; So far they all operate on the level of single instructions,
;;; though they may also add a few instructions in place.

;;; Transform an instruction. Called for effect.
(defgeneric reduce-instruction (instruction)
  ;; Default method: Do nothing.
  (:method ((instruction bir:instruction))))

(defmethod reduce-instruction ((inst bir:fixed-values-save))
  ;; Reduce to MTF.
  ;; We don't bother merging iblocks because we're done with optimizations
  ;; that would use that information anyway.
  (bir:insert-instruction-before
   (make-instance 'cc-bmir:mtf
     :origin (bir:origin inst) :policy (bir:policy inst)
     :nvalues (bir:nvalues inst)
     :inputs (bir:inputs inst) :outputs (bir:outputs inst))
   inst)
  (bir:replace-terminator
   (make-instance 'bir:jump
     :origin (bir:origin inst) :policy (bir:policy inst)
     :inputs () :outputs () :next (bir:next inst))
   inst))

;;; Given a values ctype, is the number of values fixed?
(defun fixed-values-type-p (argsct)
  (let ((sys clasp-cleavir:*clasp-system*))
    (and (null (cleavir-ctype:values-optional argsct sys))
         (cleavir-ctype:bottom-p (cleavir-ctype:values-rest argsct sys) sys))))

(defmethod reduce-instruction ((inst bir:mv-call))
  ;; Reduce to cc-bmir:fixed-mv-call if the arguments have fixed #s of values.
  ;; FIXME: Is it a good idea to use type information this late? It
  ;; ought to be harmless, but it's different.
  (let ((arg-types (mapcar #'bir:ctype (rest (bir:inputs inst)))))
    (when (every #'fixed-values-type-p arg-types)
      (let* ((sys clasp-cleavir:*clasp-system*)
             (nvalues
               (reduce #'+ arg-types
                       :key (lambda (vct)
                              (length
                               (cleavir-ctype:values-required vct sys))))))
        (change-class inst 'cc-bmir:fixed-mv-call :nvalues nvalues)))))

(defmethod reduce-instruction ((inst bir:mv-local-call))
  (let* ((args (second (bir:inputs inst)))
         (argsct (bir:ctype args))
         (sys clasp-cleavir:*clasp-system*)
         (req (cleavir-ctype:values-required argsct sys))
         (opt (cleavir-ctype:values-optional argsct sys))
         (rest (cleavir-ctype:values-rest argsct sys)))
    (when (and (cleavir-ctype:bottom-p rest clasp-cleavir:*clasp-system*)
               (null opt))
      (change-class inst 'cc-bmir:fixed-mv-local-call
                    :nvalues (length req)))))

(defstruct call-transform
  ;; Either:
  ;; a function that receives the return type and arg types and returns the
  ;;  primop, or
  ;; a primop name (symbol or cons)
  primop
  ;; a values ctype that the call return type must be a subtype of
  ;; for the transformation to apply
  return-type
  ;; a list of non-values ctypes that the arguments must be subtypes of
  argtypes
  ;; either NIL, or an extra function (receiving the types) to be an
  ;;  additional test for the transform to apply
  test
  ;; either T, meaning use the call's arguments as is, or a list of indices
  ;;  to permute a subset of the arguments for the primop
  permutation)

;;; Calls we can reduce to primops. We do this late so that the more high level inference
;;; steps don't need to concern themselves with primops.
;;; We do reductions based on the derived types of the arguments and of the
;;; return value. Using the return type makes it easy to e.g. use fixnum
;;; arithmetic when we know the result is a fixnum, without recapitulating the
;;; difficult logic in type.lisp for figuring that out.
;;; This is a hash table where the keys are function names.
;;; The values are alists (primop return-type . argtypes).
(defvar *call-to-primop* (make-hash-table :test #'equal))

(defun more-specific-transform-p (transform1 transform2)
  (let ((argtypes1 (call-transform-argtypes transform1))
        (argtypes2 (call-transform-argtypes transform2)))
    (and (= (length argtypes1) (length argtypes2))
         (every #'subtypep argtypes1 argtypes2)
         (cleavir-ctype:values-subtypep
          (call-transform-return-type transform1)
          (call-transform-return-type transform2)
          clasp-cleavir:*clasp-system*)
         (null (call-transform-test transform2)))))

(defun %deftransform (name primop-name return-type argtypes
                      &optional test (permutation t))
  (setf (gethash name *call-to-primop*)
        (merge 'list (list (make-call-transform
                            :primop primop-name
                            :return-type return-type
                            :argtypes argtypes
                            :test test :permutation permutation))
               (gethash name *call-to-primop*)
               #'more-specific-transform-p)))

(defmacro deftransform (name primop-name &rest argtypes)
  `(progn (%deftransform ',name ',primop-name
                         '(values &optional &rest t) ',argtypes)
          ',name))

;;; deftransform with return type. This is a separate operator because it's
;;; much less common. And it has the annotated name because deftransform was 1st.
(defmacro deftransform-wr (name primop-name return-type &rest argtypes)
  `(progn (%deftransform ',name ',primop-name
                         (cleavir-env:parse-values-type-specifier
                          ',return-type nil clasp-cleavir:*clasp-system*)
                         ',argtypes)
          ',name))

;;; full deftransform with evaluated primop (for functional computation),
;;; additional test, and permutation.
(defmacro deftransform-f (name primopf test permutation
                          return-type &rest argtypes)
  `(progn (%deftransform ',name ,primopf
                         (cleavir-env:parse-values-type-specifier
                          ',return-type nil clasp-cleavir:*clasp-system*)
                         ',argtypes
                         ,test ',permutation)))

#+(or)
(deftransform symbol-value symbol-value symbol)

(deftransform core:generalp core:generalp t)

(defun compute-headerp-primop (return-type object-type header-type)
  (declare (ignore return-type object-type))
  `(core::headerq ,(first (cleavir-ctype:member-members
                           clasp-cleavir:*clasp-system*
                           header-type))))

;; If CTYPE is an eql type (constant), return (constant t), else nil nil.
(defun ctype-constant-value (ctype)
  (let ((system clasp-cleavir:*clasp-system*))
    (if (cleavir-ctype:member-p system ctype)
        (let ((members (cleavir-ctype:member-members system ctype)))
          (if (= (length members) 1)
              (values (first members) t)
              (values nil nil)))
        (values nil nil))))

(defun headerp-test (return-type object-type header-type)
  (declare (ignore return-type object-type))
  (multiple-value-bind (h constantp)
      (ctype-constant-value header-type)
    (and constantp (gethash h core:+type-header-value-map+) t)))

(deftransform-f core::headerp #'compute-headerp-primop 'headerp-test (0)
  t t t)

(deftransform functionp (core::headerq function) t)

(deftransform symbolp (core::headerq symbol) t)
(deftransform packagep (core::headerq package) t)

(deftransform arrayp (core::headerq array) t)
(deftransform core:data-vector-p
    (core::headerq core:abstract-simple-vector) t)

(deftransform hash-table-p (core::headerq hash-table) t)

(deftransform pathnamep (core::headerq pathname) t)

(deftransform core:fixnump core:fixnump t)
(deftransform core:single-float-p core:single-float-p t)

(deftransform random-state-p (core::headerq random-state) t)

(deftransform core::etypecase-error core::etypecase-error t t)
;; These are written kinda stupidly. Please forgive me.
;; TODO: Better mechanism for &key parameters.
(deftransform-f error (constantly 'type-error) (constantly t)
  (2 4) t (eql type-error) (eql :datum) t (eql :expected-type) t)
(deftransform-f error (constantly 'type-error) (constantly t)
  (4 2) t (eql type-error) (eql :expected-type) t (eql :datum) t)

(deftransform core:to-single-float core::double-to-single double-float)
(deftransform core:to-single-float core::fixnum-to-single fixnum)
(deftransform core:to-double-float core::single-to-double single-float)
(deftransform core:to-double-float core::fixnum-to-double fixnum)

(macrolet ((define-two-arg-ff (name sf-primop df-primop)
             `(progn
                (deftransform ,name ,sf-primop single-float single-float)
                (deftransform ,name ,df-primop double-float double-float))))
  (define-two-arg-ff core:two-arg-+ core::two-arg-sf-+ core::two-arg-df-+)
  (define-two-arg-ff core:two-arg-- core::two-arg-sf-- core::two-arg-df--)
  (define-two-arg-ff core:two-arg-* core::two-arg-sf-* core::two-arg-df-*)
  (define-two-arg-ff core:two-arg-/ core::two-arg-sf-/ core::two-arg-df-/)
  (define-two-arg-ff expt           core::sf-expt      core::df-expt))

(macrolet ((define-float-conditional (name sf-primop df-primop)
             `(progn
                (deftransform ,name ,sf-primop single-float single-float)
                (deftransform ,name ,df-primop double-float double-float))))
  (define-float-conditional core:two-arg-= core::two-arg-sf-= core::two-arg-df-=)
  (define-float-conditional core:two-arg-< core::two-arg-sf-< core::two-arg-df-<)
  (define-float-conditional core:two-arg-<= core::two-arg-sf-<= core::two-arg-df-<=)
  (define-float-conditional core:two-arg-> core::two-arg-sf-> core::two-arg-df->)
  (define-float-conditional core:two-arg->= core::two-arg-sf->= core::two-arg-df->=))

(deftransform core:two-arg-=  core::two-arg-fixnum-=  fixnum fixnum)
(deftransform core:two-arg-<  core::two-arg-fixnum-<  fixnum fixnum)
(deftransform core:two-arg-<= core::two-arg-fixnum-<= fixnum fixnum)
(deftransform core:two-arg->  core::two-arg-fixnum->  fixnum fixnum)
(deftransform core:two-arg->= core::two-arg-fixnum->= fixnum fixnum)

(deftransform ftruncate core::sf-ftruncate single-float single-float)
(deftransform ftruncate core::df-ftruncate double-float double-float)
;; TODO: One-arg form

(macrolet ((define-floatf (name sf-primop df-primop)
             `(progn
                (deftransform ,name ,sf-primop single-float)
                (deftransform ,name ,df-primop double-float))))
  (define-floatf exp         core::sf-exp    core::df-exp)
  (define-floatf cos         core::sf-cos    core::df-cos)
  (define-floatf sin         core::sf-sin    core::df-sin)
  (define-floatf tan         core::sf-tan    core::df-tan)
  (define-floatf cosh        core::sf-cosh   core::df-cosh)
  (define-floatf sinh        core::sf-sinh   core::df-sinh)
  (define-floatf tanh        core::sf-tanh   core::df-tanh)
  (define-floatf asinh       core::sf-asinh  core::df-asinh)
  (define-floatf abs         core::sf-abs    core::df-abs)
  (define-floatf core:negate core::sf-negate core::df-negate))

(deftransform sqrt core::sf-sqrt (single-float 0f0))
(deftransform sqrt core::df-sqrt (double-float 0d0))

(deftransform log core::sf-log (single-float (0f0)))
(deftransform log core::df-log (double-float (0d0)))

(deftransform acos core::sf-acos (single-float -1f0 1f0))
(deftransform acos core::df-acos (double-float -1f0 1f0))
(deftransform asin core::sf-asin (single-float -1f0 1f0))
(deftransform asin core::df-asin (double-float -1d0 1d0))

(deftransform acosh core::sf-acosh (single-float 1f0))
(deftransform acosh core::df-acosh (double-float 1d0))
(deftransform atanh core::sf-atanh (single-float (-1f0) (1f0)))
(deftransform atanh core::df-atanh (double-float (-1d0) (1d0)))

;;; When both dividend and divisor are positive, MOD and REM coincide,
;;; as do FLOOR and TRUNCATE.
(deftransform truncate core::fixnum-truncate
  fixnum (integer 1 #.most-positive-fixnum))
(deftransform truncate core::fixnum-truncate
  ;; -2 because most-negative-fixnum/-1 would overflow
  fixnum (integer #.most-negative-fixnum -2))
(deftransform floor core::fixnum-truncate
  (integer 0 #.most-positive-fixnum) (integer 1 #.most-positive-fixnum))
(deftransform mod core::fixnum-rem
  (integer 0 #.most-positive-fixnum) (integer 1 #.most-positive-fixnum))
(deftransform rem core::fixnum-rem
  fixnum (integer 1 #.most-positive-fixnum))
(deftransform rem core::fixnum-rem
  fixnum (integer #.most-negative-fixnum -2))

(deftransform lognot core::fixnum-lognot fixnum)

(macrolet ((deflog2 (name primop)
             `(deftransform ,name ,primop fixnum fixnum)))
  (deflog2 core:logand-2op core::fixnum-logand)
  (deflog2 core:logior-2op core::fixnum-logior)
  (deflog2 core:logxor-2op core::fixnum-logxor))

(deftransform-wr core:two-arg-+ core::fixnum-add fixnum fixnum fixnum)
(deftransform-wr core:two-arg-- core::fixnum-sub fixnum fixnum fixnum)
(deftransform-wr core:two-arg-* core::fixnum-mul fixnum fixnum fixnum)

(deftransform core:two-arg-+ core::fixnum-add-over fixnum fixnum)
(deftransform core:two-arg-- core::fixnum-sub-over fixnum fixnum)

(deftransform logcount core::fixnum-positive-logcount (and fixnum unsigned-byte))

;;; 63 is magic - fixnums are i64 so llvm will shift them 63 at most.
;;; should be done more elegantly, FIXME
(deftransform-wr ash core::fixnum-shl fixnum fixnum (integer 0 63))
(deftransform-wr core:ash-left core::fixnum-shl fixnum fixnum (integer 0 63))
(deftransform core:ash-right core::fixnum-ashr fixnum (integer 0 63))
(deftransform core:ash-right core::fixnum-ashr-min
  fixnum (and fixnum (integer 0)))

;;(deftransform car cleavir-primop:car cons)
;;(deftransform cdr cleavir-primop:cdr cons)

;;; We can't use %DISPLACEMENT here because it will return the underlying
;;; simple array even when there isn't one as far as standard lisp is concerned,
;;; e.g. for a simple mdarray, or an undisplaced adjustable array.
#+(or)
(deftransform array-displacement core::%displacement (and array (not simple-array)))
(deftransform array-total-size core::%array-total-size
  (and array (not (simple-array * (*)))))
(deftransform array-rank core::%array-rank (and array (not (simple-array * (*)))))
;;; Can't use %array-dimension since it doesn't check the rank.

(deftransform core:check-bound core:check-bound
  t fixnum t)
;; These are unsafe - make sure we only use core:vref when we don't need a
;; (further) bounds check.
(defmacro define-vector-transforms (element-type)
  `(progn
     (deftransform core:vref (core:vref ,element-type)
       (simple-array ,element-type (*)) fixnum)
     (deftransform (setf core:vref) (core::vset ,element-type)
       ;; FIXME: we should probably check the new value's type?
       ;; ditto for atomic aref below.
       t (simple-array ,element-type (*)) fixnum)))
(define-vector-transforms t)
(define-vector-transforms single-float)
(define-vector-transforms double-float)
(define-vector-transforms base-char)
(define-vector-transforms character)

(deftransform array-total-size core::vector-length (simple-array * (*)))

(deftransform characterp characterp t)

(deftransform length core::vector-length (simple-array * (*)))

(deftransform consp consp t)
(deftransform car cleavir-primop:car cons)
(deftransform cdr cleavir-primop:cdr cons)
(deftransform rplaca cleavir-primop:rplaca cons t)
(deftransform rplacd cleavir-primop:rplacd cons t)

(deftransform core:set-breakstep core:set-breakstep)
(deftransform core:unset-breakstep core:unset-breakstep)

(deftransform mp:fence (mp:fence :sequentially-consistent)
  (eql :sequentially-consistent))
(deftransform mp:fence (mp:fence :acquire-release) (eql :acquire-release))
(deftransform mp:fence (mp:fence :acquire) (eql :acquire))
(deftransform mp:fence (mp:fence :release) (eql :release))

(defun atomic-aref-test* (array-type order-type system)
  ;; what we want to check is that we know the exact array element
  ;; type and atomic access order.
  (declare (ignore return-type index-types))
  (and (cleavir-ctype:arrayp array-type system)
       (member (cleavir-ctype:array-element-type array-type system)
               ;; FIXME: Expand this set
               '(t single-float double-float
                 base-char character))
       (cleavir-ctype:member-p system order-type)
       (member (ctype-constant-value order-type)
               '(:sequentially-consistent :relaxed
                 :acquire-release :acquire :release))))

(defun atomic-aref-test (return-type order-type array-type
                         &rest index-types)
  (declare (ignore return-type index-types))
  (atomic-aref-test* array-type order-type
                     clasp-cleavir:*clasp-system*))
(defun atomic-aset-test (return-type new-type order-type array-type
                         &rest index-types)
  (declare (ignore return-type new-type index-types))
  (atomic-aref-test* array-type order-type
                     clasp-cleavir:*clasp-system*))

(defun compute-atomic-aref-primop (return-type order-type array-type
                                   &rest index-types)
  (declare (ignore return-type index-types))
  (let ((sys clasp-cleavir:*clasp-system*))
    `(core:vref ,(cleavir-ctype:array-element-type array-type sys)
                ,(first
                  (cleavir-ctype:member-members sys order-type)))))
(defun compute-atomic-aset-primop (return-type new-type
                                   order-type array-type
                                   &rest index-types)
  (declare (ignore return-type new-type index-types))
  (let ((sys clasp-cleavir:*clasp-system*))
    `(core::vset ,(cleavir-ctype:array-element-type array-type sys)
                ,(first
                  (cleavir-ctype:member-members sys order-type)))))

(deftransform-f core:atomic-aref #'compute-atomic-aref-primop
  'atomic-aref-test (1 2) t t (simple-array * (*)) fixnum)
(deftransform-f (setf core:atomic-aref) #'compute-atomic-aset-primop
  'atomic-aset-test (0 2 3) t t t (simple-array * (*)) fixnum)

(deftransform core:acas (core:acas :sequentially-consistent t 1)
  (eql :sequentially-consistent) t t simple-vector fixnum)
(deftransform core:acas (core:acas :acquire-release t 1)
  (eql :acquire-release) t t simple-vector fixnum)
(deftransform core:acas (core:acas :acquire t 1)
  (eql :acquire) t t simple-vector fixnum)
(deftransform core:acas (core:acas :release t 1)
  (eql :release) t t simple-vector fixnum)
(deftransform core:acas (core:acas :relaxed t 1)
  (eql :relaxed) t t simple-vector fixnum)

;;; e.g. (permute '(1 0) '(7 18 29)) => (18 7)
(defun permute (permutation list)
  (loop for index in permutation
        collect (nth index list)))

(defun reduce-call-to-primop (call primop permutation)
  (let* ((callee (bir:callee call))
         (args (rest (bir:inputs call)))
         (primop-args
           (if (eq permutation t)
               args
               (permute permutation args))))
    (change-class call 'bir:primop :inputs primop-args
                                   :info (cleavir-primop-info:info primop))
    ;; Delete the callee in the usual case that it's an fdef lookup.
    ;; KLUDGEy since we don't delete more generally.
    (when (typep callee 'bir:output)
      (let ((fdef (bir:definition callee)))
        (when (typep fdef 'bir:constant-fdefinition)
          (bir:delete-instruction fdef)))))
  (values))

(defmethod reduce-instruction ((inst bir:call))
  (let ((ids (cleavir-attributes:identities (bir:attributes inst))))
    (when ids
      (let* ((sys clasp-cleavir:*clasp-system*)
             (args (rest (bir:inputs inst)))
             (output (bir:output inst))
             (prtype (bir:ctype output))
             (types (mapcar #'bir:ctype args))
             (ptypes (mapcar (lambda (vty) (cleavir-ctype:primary vty sys)) types))
             (lptypes (length ptypes)))
        (dolist (id ids)
          (loop for transform in (gethash id *call-to-primop*)
                for primopf = (call-transform-primop transform)
                when (let ((trtype (call-transform-return-type transform))
                           (ttypes (call-transform-argtypes transform))
                           (test (call-transform-test transform)))
                       (and (= lptypes (length ttypes))
                            (every #'subtypep ptypes ttypes)
                            (cleavir-ctype:values-subtypep
                             prtype trtype clasp-cleavir:*clasp-system*)
                            (or (not test)
                                (apply test prtype ptypes))))
                  do (let ((primop (if (functionp primopf)
                                       (apply primopf prtype ptypes)
                                       primopf))
                           (perm (call-transform-permutation transform)))
                       (reduce-call-to-primop inst primop perm))
                     (return-from reduce-instruction)))))))

(defun reduce-instructions (function)
  (bir:map-local-instructions #'reduce-instruction function))

(defun reduce-module-instructions (module)
  (cleavir-set:mapset nil #'reduce-instructions (bir:functions module)))
