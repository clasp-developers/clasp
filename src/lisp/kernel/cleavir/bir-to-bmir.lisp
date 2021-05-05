(in-package #:cc-bir-to-bmir)

(defun replace-typeq (typeq)
  (let ((ts (bir:test-ctype typeq)))
    ;; Undo some parsing. KLUDGE.
    (cond
      ;; FIXNUM
      ((equal ts '(integer #.most-negative-fixnum #.most-positive-fixnum))
       (setf ts 'fixnum))
      ;; bignum
      ((equal ts '(or (integer * (#.most-negative-fixnum))
                   (integer (#.most-positive-fixnum) *)))
       (setf ts 'bignum))
      ;; simple-bit-array becomes (simple-array bit (*)), etc.
      ((and (consp ts) (eq (car ts) 'simple-array))
       (setf ts (core::simple-vector-type (second ts))))
      ;; simple-string
      ((or (equal ts '(or (simple-array base-char (*))
                       (simple-array character (*))))
           (equal ts '(or (simple-array character (*))
                       (simple-array base-char (*)))))
       (setf ts 'simple-string))
      ((and (consp ts) (eq (car ts) 'function))
       ;; We should check that this does not specialize, because
       ;; obviously we can't check that.
       (setf ts 'function)))
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

(defun reduce-local-typeqs (function)
  (bir:map-iblocks
   (lambda (ib)
     (let ((term (bir:end ib)))
       (when (typep term 'bir:ifi)
         (let ((test-out (bir:input term)))
           (when (typep test-out 'bir:output)
             (let ((test (bir:definition test-out)))
               (when (typep test 'bir:typeq-test)
                 (replace-typeq test))))))))
   function))

(defun reduce-module-typeqs (module)
  (cleavir-bir:map-functions #'reduce-local-typeqs module))

(defun maybe-replace-primop (primop)
  (case (cleavir-primop-info:name (bir:info primop))
    ((cleavir-primop:car)
     (let ((in (bir:inputs primop))
           (nout (make-instance 'bir:output)))
       (change-class primop 'cc-bmir:load :inputs ())
       (let ((mr (make-instance 'cc-bmir:memref2
                   :inputs in :outputs (list nout)
                   :offset (- cmp:+cons-car-offset+ cmp:+cons-tag+))))
         (bir:insert-instruction-before mr primop)
         (setf (bir:inputs primop) (list nout)))))
    ((cleavir-primop:cdr)
     (let ((in (bir:inputs primop))
           (nout (make-instance 'bir:output)))
       (change-class primop 'cc-bmir:load :inputs ())
       (let ((mr (make-instance 'cc-bmir:memref2
                   :inputs in :outputs (list nout)
                   :offset (- cmp:+cons-cdr-offset+ cmp:+cons-tag+))))
         (bir:insert-instruction-before mr primop)
         (setf (bir:inputs primop) (list nout)))))
    ((cleavir-primop:rplaca)
     (let ((in (bir:inputs primop))
           (nout (make-instance 'bir:output)))
       (change-class primop 'cc-bmir:store :inputs ())
       (let ((mr (make-instance 'cc-bmir:memref2
                   :inputs (list (first in)) :outputs (list nout)
                   :offset (- cmp:+cons-car-offset+ cmp:+cons-tag+))))
         (bir:insert-instruction-before mr primop)
         (setf (bir:inputs primop) (list (second in) nout)))))
    ((cleavir-primop:rplacd)
     (let ((in (bir:inputs primop))
           (nout (make-instance 'bir:output)))
       (change-class primop 'cc-bmir:store :inputs ())
       (let ((mr (make-instance 'cc-bmir:memref2
                   :inputs (list (first in)) :outputs (list nout)
                   :offset (- cmp:+cons-cdr-offset+ cmp:+cons-tag+))))
         (bir:insert-instruction-before mr primop)
         (setf (bir:inputs primop) (list (second in) nout)))))))

(defun reduce-primops (function)
  (bir:map-local-instructions
   (lambda (i)
     (when (typep i 'bir:primop)
       (maybe-replace-primop i)))
   function))

(defun reduce-module-primops (module)
  (cleavir-set:mapset nil #'reduce-primops (bir:functions module)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Representation types ("rtypes")
;;;
;;; An rtype describes how a value or values is represented in the runtime.
;;; So far the possible rtypes are :object, meaning one T_O*, and
;;; :multiple-values, meaning several T_O*s stored in the thread local multiple
;;; values vector. In the future there will probably be rtypes for unboxed
;;; values as well as fixed numbers of values.
;;; NIL is a pseudo-rtype indicating a datum's use has no preference for how it
;;; is represented, or if the datum is simply unused. A datum's definitions
;;; must necessarily have a preferred rtype, i.e. NIL is not permitted there.

;; Given an instruction, determine what rtype it outputs.
(defgeneric definition-rtype (instruction))
;; usually correct default
(defmethod definition-rtype ((inst bir:instruction)) :object)
(defmethod definition-rtype ((inst bir:abstract-call)) :multiple-values)
(defmethod definition-rtype ((inst bir:values-save)) :multiple-values)
(defmethod definition-rtype ((inst bir:values-collect)) :multiple-values)
(defmethod definition-rtype ((inst cc-bir:mv-foreign-call)) :multiple-values)
(defmethod definition-rtype ((inst bir:thei))
  ;; THEI really throws a wrench in some stuff.
  (let ((input (first (bir:inputs inst))))
    (maybe-assign-rtype input)
    (cc-bmir:rtype input)))
;; FIXME: will be removed (?)
(defmethod definition-rtype ((inst bir:fixed-to-multiple)) :multiple-values)

;;; Given a datum, determine what rtype its use requires.
(defgeneric use-rtype (datum))
;; Given a user (instruction) and a datum, determine the rtype required.
(defgeneric %use-rtype (instruction datum))
(defmethod %use-rtype ((inst bir:instruction) (datum bir:datum))
  ;; Having this as a default is mildly dicey but should work: instructions
  ;; that need multiple value inputs are a definite minority.
  :object)
(defmethod %use-rtype ((inst bir:mv-call) (datum bir:datum))
  (if (member datum (rest (bir:inputs inst)))
      :multiple-values :object))
(defmethod %use-rtype ((inst bir:mv-local-call) (datum bir:datum))
  (if (member datum (rest (bir:inputs inst)))
      :multiple-values :object))
(defmethod %use-rtype ((inst bir:returni) (datum bir:datum)) :multiple-values)
(defmethod %use-rtype ((inst bir:values-save) (datum bir:datum))
  :multiple-values)
(defmethod %use-rtype ((inst bir:values-collect) (datum bir:datum))
  :multiple-values)
(defmethod %use-rtype ((inst bir:unwind) (datum bir:datum))
  (error "BUG: transitive-rtype should make this impossible!"))
(defmethod %use-rtype ((inst bir:jump) (datum bir:datum))
  (error "BUG: transitive-rtype should make this impossible!"))
;; FIXME: multiple-to-fixed will be removed
(defmethod %use-rtype ((inst bir:multiple-to-fixed) (datum bir:datum))
  :multiple-values)
(defmethod %use-rtype ((inst bir:thei) (datum bir:datum))
  ;; actual type tests, which need multiple values, should have been turned
  ;; into mv calls by this point. but out of an abundance of caution,
  (if (symbolp (bir:type-check-function inst))
      (use-rtype (first (bir:outputs inst)))
      :multiple-values))
             
;; Determine the rtype a datum needs to end up as by chasing transitive use.
(defun transitive-rtype (datum)
  (loop (let ((use (bir:use datum)))
          (etypecase use
            (null (return :object)) ; unused, doesn't matter
            ((or bir:jump bir:unwind)
             (setf datum (nth (position datum (bir:inputs use))
                              (bir:outputs use))))
            (bir:instruction (return (%use-rtype use datum)))))))
(defmethod use-rtype ((datum bir:phi)) (transitive-rtype datum))
(defmethod use-rtype ((datum bir:output)) (transitive-rtype datum))
(defmethod use-rtype ((datum bir:argument)) (transitive-rtype datum))

;;; Given two rtypes, return the most preferable rtype.
(defun min-rtype (rt1 rt2)
  (ecase rt1
    ((:object)
     (assert (member rt2 '(nil :object :multiple-values)))
     :object)
    ((:multiple-values)
     (ecase rt2
       ((nil) :object)
       ((:object :multiple-values) rt2)))
    ((nil)
     (assert (member rt2 '(:object :multiple-values)))
     :object)))

(defun assign-output-rtype (datum)
  (let* ((source (definition-rtype (bir:definition datum)))
         (dest (use-rtype datum))
         (rtype (min-rtype source dest)))
    (change-class datum 'cc-bmir:output :rtype rtype)
    rtype))

(defun phi-rtype (datum)
  ;; PHIs are trickier. If the destination is single-value, the phi can be too.
  ;; If not, then the phi could still be single-value, but only if EVERY
  ;; definition is, and otherwise we need to use multiple values.
  (let ((rt :object) (dest (use-rtype datum)))
    (ecase dest
      ((:object nil) :object)
      ((:multiple-values)
       (cleavir-set:doset (def (bir:definitions datum) rt)
         (etypecase def
           ((or bir:jump bir:unwind)
            (let ((in (nth (position datum (bir:outputs def))
                           (bir:inputs def))))
              (maybe-assign-rtype in)
              (ecase (cc-bmir:rtype in)
                (:object)
                (:multiple-values (setf rt :multiple-values)))))))))))

(defun assign-phi-rtype (datum)
  (change-class datum 'cc-bmir:phi :rtype (phi-rtype datum)))

(defgeneric maybe-assign-rtype (datum))
(defmethod maybe-assign-rtype ((datum cc-bmir:output)))
(defmethod maybe-assign-rtype ((datum cc-bmir:phi)))
(defmethod maybe-assign-rtype ((datum bir:variable)))
(defmethod maybe-assign-rtype ((datum bir:argument)))
(defmethod maybe-assign-rtype ((datum bir:load-time-value)))
(defmethod maybe-assign-rtype ((datum bir:constant)))
(defmethod maybe-assign-rtype ((datum bir:output))
  (assign-output-rtype datum))
(defmethod maybe-assign-rtype ((datum bir:phi))
  (assign-phi-rtype datum))

(defun assign-instruction-rtypes (inst)
  (mapc #'maybe-assign-rtype (bir:outputs inst)))

(defun assign-function-rtypes (function)
  (bir:map-local-instructions #'assign-instruction-rtypes function))

(defun assign-module-rtypes (module)
  (bir:map-functions #'assign-function-rtypes module))

(defun insert-mtf (after datum)
  (let* ((fx (make-instance 'cc-bmir:output :rtype :object))
         (mtf (make-instance 'bir:multiple-to-fixed :outputs (list fx))))
    (bir:insert-instruction-after mtf after)
    (bir:replace-uses fx datum)
    (setf (bir:inputs mtf) (list datum)))
  (values))

(defun maybe-insert-mtf (after datum)
  (ecase (cc-bmir:rtype datum)
    ((:object) (insert-mtf after datum))
    ((:multiple-values))))

(defun insert-ftm (before datum)
  (let* ((mv (make-instance 'cc-bmir:output :rtype :multiple-values))
         (ftm (make-instance 'bir:fixed-to-multiple :outputs (list mv))))
    (bir:insert-instruction-before ftm before)
    (bir:replace-uses mv datum)
    (setf (bir:inputs ftm) (list datum))
    ftm))

(defun maybe-insert-ftm (before datum)
  (ecase (cc-bmir:rtype datum)
    ((:object) (insert-ftm before datum))
    ((:multiple-values))))

(defun maybe-insert-ftms (before data)
  (loop for dat in data do (maybe-insert-ftm before dat)))

(defgeneric insert-values-coercion (instruction))

(defun check-object-input (instruction input)
  (ecase (cc-bmir:rtype input)
    ((:object))
    ((:multiple-values)
     (error "BUG: MV input into non-MV instruction ~a" instruction))))

(defun check-object-inputs (instruction
                            &optional (inputs (bir:inputs instruction)))
  (loop for inp in inputs do (check-object-input instruction inp)))

(defmethod insert-values-coercion ((instruction bir:instruction))
  ;; Default method: Assume we need all :objects and output an :object.)
  (check-object-inputs instruction))

;;; Make sure we don't insert things infinitely
(defmethod insert-values-coercion ((instruction bir:multiple-to-fixed)))
(defmethod insert-values-coercion ((instruction bir:fixed-to-multiple)))
;;; Doesn't need to do anything, and might not have all :object inputs
(defmethod insert-values-coercion ((instruction bir:thei)))

(defun insert-jump-coercion (instruction)
  (loop for inp in (bir:inputs instruction)
        for outp in (bir:outputs instruction)
        do (ecase (cc-bmir:rtype inp)
             ((:object)
              (ecase (cc-bmir:rtype outp)
                ((:object))
                ((:multiple-values) (insert-ftm instruction inp))))
             ((:multiple-values)
              (ecase (cc-bmir:rtype outp)
                ((:object)
                 (error "BUG: MV input into non-MV jump output in ~a"
                        instruction))
                ((:multiple-values)))))))

(defmethod insert-values-coercion ((instruction bir:jump))
  (insert-jump-coercion instruction))
(defmethod insert-values-coercion ((instruction bir:unwind))
  (insert-jump-coercion instruction))

(defmethod insert-values-coercion ((instruction bir:call))
  (check-object-inputs instruction)
  (maybe-insert-mtf instruction (first (bir:outputs instruction))))
(defmethod insert-values-coercion ((instruction bir:mv-call))
  (check-object-input instruction (first (bir:inputs instruction)))
  (maybe-insert-ftms instruction (rest (bir:inputs instruction)))
  (maybe-insert-mtf instruction (first (bir:outputs instruction))))
(defmethod insert-values-coercion ((instruction bir:local-call))
  (check-object-inputs instruction (rest (bir:inputs instruction)))
  (maybe-insert-mtf instruction (first (bir:outputs instruction))))
(defmethod insert-values-coercion ((instruction bir:mv-local-call))
  (maybe-insert-ftms instruction (rest (bir:inputs instruction)))
  (maybe-insert-mtf instruction (first (bir:outputs instruction))))
(defmethod insert-values-coercion ((instruction cc-bir:mv-foreign-call))
  (check-object-inputs instruction)
  (maybe-insert-mtf instruction (first (bir:outputs instruction))))
;;; FIXME: Make these tolerate single value inputs - improve efficiency
(defmethod insert-values-coercion ((instruction bir:values-save))
  (maybe-insert-ftms instruction (bir:inputs instruction))
  (maybe-insert-mtf instruction (first (bir:outputs instruction))))
(defmethod insert-values-coercion ((instruction bir:values-collect))
  (maybe-insert-ftms instruction (bir:inputs instruction))
  (maybe-insert-mtf instruction (first (bir:outputs instruction))))
(defmethod insert-values-coercion ((instruction bir:returni))
  (maybe-insert-ftms instruction (bir:inputs instruction)))

(defun insert-values-coercion-into-function (function)
  (cleavir-bir:map-local-instructions #'insert-values-coercion function))

(defun insert-values-coercion-into-module (module)
  (bir:map-functions #'insert-values-coercion-into-function module))
