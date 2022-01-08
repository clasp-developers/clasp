(in-package #:cc-bir-to-bmir)

;;; This file handles miscellaneous bir-to-bmir reductions.
;;; So far they all operate on the level of single instructions,
;;; though they may also add a few instructions in place.

;;; Transform an instruction. Called for effect.
(defgeneric reduce-instruction (instruction)
  ;; Default instruction: Do nothing.
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

(defmethod reduce-instruction ((primop bir:primop))
  (case (cleavir-primop-info:name (bir:info primop))
    ((cleavir-primop:car)
     (let ((in (bir:inputs primop))
           (nout (make-instance 'bir:output)))
       (change-class primop 'cc-bmir:load :inputs ())
       (let ((mr (make-instance 'cc-bmir:memref2
                   :inputs in :outputs (list nout)
                   :offset (- cmp:+cons-car-offset+ cmp:+cons-tag+)
                   :origin (bir:origin primop)
                   :policy (bir:policy primop))))
         (bir:insert-instruction-before mr primop)
         (setf (bir:inputs primop) (list nout)))))
    ((cleavir-primop:cdr)
     (let ((in (bir:inputs primop))
           (nout (make-instance 'bir:output)))
       (change-class primop 'cc-bmir:load :inputs ())
       (let ((mr (make-instance 'cc-bmir:memref2
                   :inputs in :outputs (list nout)
                   :offset (- cmp:+cons-cdr-offset+ cmp:+cons-tag+)
                   :origin (bir:origin primop)
                   :policy (bir:policy primop))))
         (bir:insert-instruction-before mr primop)
         (setf (bir:inputs primop) (list nout)))))
    ((cleavir-primop:rplaca)
     (let ((in (bir:inputs primop))
           (nout (make-instance 'bir:output)))
       (change-class primop 'cc-bmir:store :inputs ())
       (let ((mr (make-instance 'cc-bmir:memref2
                   :inputs (list (first in)) :outputs (list nout)
                   :offset (- cmp:+cons-car-offset+ cmp:+cons-tag+)
                   :origin (bir:origin primop)
                   :policy (bir:policy primop))))
         (bir:insert-instruction-before mr primop)
         (setf (bir:inputs primop) (list (second in) nout)))))
    ((cleavir-primop:rplacd)
     (let ((in (bir:inputs primop))
           (nout (make-instance 'bir:output)))
       (change-class primop 'cc-bmir:store :inputs ())
       (let ((mr (make-instance 'cc-bmir:memref2
                   :inputs (list (first in)) :outputs (list nout)
                   :offset (- cmp:+cons-cdr-offset+ cmp:+cons-tag+)
                   :origin (bir:origin primop)
                   :policy (bir:policy primop))))
         (bir:insert-instruction-before mr primop)
         (setf (bir:inputs primop) (list (second in) nout)))))))

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

(defun reduce-instructions (function)
  (bir:map-local-instructions #'reduce-instruction function))

(defun reduce-module-instructions (module)
  (cleavir-set:mapset nil #'reduce-instructions (bir:functions module)))
