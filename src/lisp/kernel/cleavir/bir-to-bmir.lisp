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
;;; This is a hash table where the keys are function names.
;;; The values are alists (primop . argtypes).
(defvar *call-to-primop* (make-hash-table :test #'equal))

(defmacro deftransform (name primop-name &rest argtypes)
  `(setf (gethash ',name *call-to-primop*)
         (merge 'list (list (cons ',primop-name ',argtypes))
                (gethash ',name *call-to-primop*)
                (lambda (types1 types2)
                  (and (= (length types1) (length types2))
                       (every #'subtypep types1 types2)))
                :key #'cdr)))

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
(deftransform core:two-arg-+ core::fixnum-add (signed-byte 60) (signed-byte 60))
(deftransform core:two-arg-- core::fixnum-sub (signed-byte 60) (signed-byte 60))

(deftransform logcount core::fixnum-positive-logcount (and fixnum unsigned-byte))

;;(deftransform car cleavir-primop:car cons)
;;(deftransform cdr cleavir-primop:cdr cons)

(deftransform aref core::sf-vref (simple-array single-float (*)) t)
(deftransform aref core::df-vref (simple-array double-float (*)) t)
(deftransform row-major-aref core::sf-vref (simple-array single-float (*)) t)
(deftransform row-major-aref core::df-vref (simple-array double-float (*)) t)

(deftransform (setf aref) core::sf-vset
  single-float (simple-array single-float (*)) t)
(deftransform (setf aref) core::df-vset
  double-float (simple-array double-float (*)) t)
(deftransform (setf row-major-aref) core::sf-vset
  single-float (simple-array single-float (*)) t)
(deftransform (setf row-major-aref) core::df-vset
  double-float (simple-array double-float (*)) t)

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
             (types (mapcar #'bir:ctype args))
             (ptypes (mapcar (lambda (vty) (cleavir-ctype:primary vty sys)) types))
             (lptypes (length ptypes)))
        (dolist (id ids)
          (loop for (primop . ttypes) in (gethash id *call-to-primop*)
                when (and (= lptypes (length ttypes)) (every #'subtypep ptypes ttypes))
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
