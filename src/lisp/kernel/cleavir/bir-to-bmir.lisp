(in-package #:cc-bir-to-bmir)

;;; This file handles miscellaneous bir-to-bmir reductions.
;;; So far they all operate on the level of single instructions,
;;; though they may also add a few instructions in place.

;;; Transform an instruction. Called for effect.
(defgeneric reduce-instruction (instruction)
  ;; Default method: Do nothing.
  (:method ((instruction bir:instruction))))

(defun process-typeq-type (ts)
  ;; Undo some parsing. KLUDGE.
  (cond
    ((equal ts '(integer #.most-negative-fixnum #.most-positive-fixnum))
     'fixnum)
    ((equal ts '(or (integer * (#.most-negative-fixnum))
                 (integer (#.most-positive-fixnum) *)))
     'bignum)
    ((equal ts '(single-float * *)) 'single-float)
    ((equal ts '(double-float * *)) 'double-float)
    ((equal ts '(rational * *)) 'rational)
    ((equal ts '(real * *)) 'real)
    ((equal ts '(complex *)) 'complex)
    ((equal ts '(array * *)) 'array)
    ((equal ts '(cons t t)) 'cons)
    ;; simple-bit-array becomes (simple-array bit (*)), etc.
    ((and (consp ts) (eq (car ts) 'simple-array))
     (core::simple-vector-type (second ts)))    
    ((or (equal ts '(or (simple-array base-char (*))
                     (simple-array character (*))))
         (equal ts '(or (simple-array character (*))
                     (simple-array base-char (*)))))
     (setf ts 'simple-string))
    ((and (consp ts) (eq (car ts) 'function))
     ;; We should check that this does not specialize, because
     ;; obviously we can't check that.
     'function)
    (t ts)))

(defmethod reduce-instruction ((typeq bir:typeq-test))
  (let ((ts (process-typeq-type (bir:test-ctype typeq))))
    (case ts
      ((fixnum) (change-class typeq 'cc-bmir:fixnump))
      ((cons) (change-class typeq 'cc-bmir:consp))
      ((character) (change-class typeq 'cc-bmir:characterp))
      ((single-float) (change-class typeq 'cc-bmir:single-float-p))
      ((core:general) (change-class typeq 'cc-bmir:generalp))
      (t (let ((header-info (gethash ts core:+type-header-value-map+)))
           (cond (header-info
                  (check-type header-info (or integer cons)) ; sanity check
                  (change-class typeq 'cc-bmir:headerq :info header-info))
                 (t (error "BUG: Typeq for unknown type: ~a" ts))))))))

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

;;; Calls we can reduce to primops. We do this late so that the more high level inference
;;; steps don't need to concern themselves with primops.
;;; We do reductions based on the derived types of the arguments and of the
;;; return value. Using the return type makes it easy to e.g. use fixnum
;;; arithmetic when we know the result is a fixnum, without recapitulating the
;;; difficult logic in type.lisp for figuring that out.
;;; This is a hash table where the keys are function names.
;;; The values are alists (primop return-type . argtypes).
(defvar *call-to-primop* (make-hash-table :test #'equal))

(defun %deftransform (name primop-name return-type argtypes)
  (setf (gethash name *call-to-primop*)
        (merge 'list (list (list* primop-name return-type argtypes))
               (gethash name *call-to-primop*)
               (lambda (k1 k2)
                 (let ((rt1 (first k1)) (ts1 (rest k1))
                       (rt2 (first k2)) (ts2 (rest k2)))
                   (and (= (length ts1) (length ts2))
                        (every #'subtypep ts1 ts2)
                        (cleavir-ctype:values-subtypep
                         rt1 rt2 clasp-cleavir:*clasp-system*))))
               :key #'cdr)))

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

(deftransform symbol-value symbol-value symbol)

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

;;; This is a very KLUDGEy way to find additions of fixnums whose result is
;;; a fixnum as well. Plus it hardcodes the number of fixnum bits. FIXME
(deftransform-wr core:two-arg-+ core::fixnum-add fixnum fixnum fixnum)
(deftransform-wr core:two-arg-- core::fixnum-sub fixnum fixnum fixnum)
(deftransform-wr core:two-arg-* core::fixnum-mul fixnum fixnum fixnum)

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
(defmacro define-vector-transforms (element-type ref set)
  `(progn
     (deftransform core:vref ,ref (simple-array ,element-type (*)) fixnum)
     (deftransform (setf core:vref) ,set t (simple-array ,element-type (*)) fixnum)))
(define-vector-transforms t core::t-vref core::t-vset)
(define-vector-transforms single-float core::sf-vref core::sf-vset)
(define-vector-transforms double-float core::df-vref core::df-vset)
(define-vector-transforms base-char core::bc-vref core::bc-vset)
(define-vector-transforms character core::c-vref core::c-vset)

(deftransform array-total-size core::vector-length (simple-array * (*)))

(deftransform length core::vector-length (simple-array * (*)))

(deftransform car cleavir-primop:car cons)
(deftransform cdr cleavir-primop:cdr cons)
(deftransform rplaca cleavir-primop:rplaca cons t)
(deftransform rplacd cleavir-primop:rplacd cons t)

(defmethod reduce-instruction ((inst bir:call))
  (let ((ids (cleavir-attributes:identities (bir:attributes inst))))
    (when ids
      (let* ((sys clasp-cleavir:*clasp-system*)
             (callee (bir:callee inst))
             (args (rest (bir:inputs inst)))
             (output (bir:output inst))
             (prtype (bir:ctype output))
             (types (mapcar #'bir:ctype args))
             (ptypes (mapcar (lambda (vty) (cleavir-ctype:primary vty sys)) types))
             (lptypes (length ptypes)))
        (dolist (id ids)
          (loop for (primop trtype . ttypes) in (gethash id *call-to-primop*)
                when (and (= lptypes (length ttypes))
                          (every #'subtypep ptypes ttypes)
                          (cleavir-ctype:values-subtypep
                           prtype trtype clasp-cleavir:*clasp-system*))
                  ;; Do the reduction to primop.
                  ;; If the callee is an fdefinition primop, delete that.
                  ;; KLUDGEy as we don't do a fully usedness analysis.
                  do (when (typep callee 'bir:output)
                       (let ((fdef (bir:definition callee)))
                         (when (and (typep fdef 'bir:primop)
                                    (eq 'fdefinition (cleavir-primop-info:name
                                                      (bir:info fdef))))
                           (let ((sym (first (bir:inputs fdef))))
                             (bir:delete-instruction fdef)
                             (when (typep sym 'bir:output)
                               (let ((symdef (bir:definition sym)))
                                 (when (typep symdef 'bir:constant-reference)
                                   (bir:delete-instruction symdef))))))))
                     (change-class inst 'bir:primop :inputs args
                                                    :info (cleavir-primop-info:info primop))
                     (return-from reduce-instruction)))))))

(defun reduce-instructions (function)
  (bir:map-local-instructions #'reduce-instruction function))

(defun reduce-module-instructions (module)
  (cleavir-set:mapset nil #'reduce-instructions (bir:functions module)))
