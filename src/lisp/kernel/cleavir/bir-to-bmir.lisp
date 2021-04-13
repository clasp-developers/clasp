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

;;;


;; Given an instruction, determine what rtype it outputs.
(defgeneric definition-rtype (instruction))
;; usually correct default
(defmethod definition-rtype ((inst bir:instruction)) :object)
(defmethod definition-rtype ((inst bir:abstract-call)) :multiple-values)
(defmethod definition-rtype ((inst bir:values-save)) :multiple-values)
(defmethod definition-rtype ((inst bir:values-collect)) :multiple-values)
(defmethod definition-rtype ((inst cc-bir:mv-foreign-call)) :multiple-values)
;; FIXME: will be removed (?)
(defmethod definition-rtype ((inst bir:fixed-to-multiple)) :multiple-values)

(defun insert-value-coercion (instruction)
  ;; TODO:
  ;; Insert FTMs before inputs that need multiple values,
  ;; and MTFs after outputs that need fixed values.
  ;; This is a very simple way to try to minimize the multiple values we have
  ;; alive at any given time.
  (cond
    ((and (bir:outputs instruction)
          (not (typep instruction '(or bir:jump bir:thei bir:unwind))))
     (let* ((out (first (bir:outputs instruction)))
            (source (definition-rtype instruction))
            (dest (cc-bmir:rtype out)))
       (assert (null (rest (bir:outputs instruction))))
       (cond ((not dest)) ; datum is unused, so no coercion is necessary
             ((eq source dest)) ; they already agree, no coercion
             ((eq dest :multiple-values)
              (assert (eq source :object))
              (let* ((mv (make-instance 'bir:output))
                     (ftm (make-instance 'bir:fixed-to-multiple
                            :outputs (list mv))))
                (bir:insert-instruction-after ftm instruction)
                (bir:replace-uses mv out)
                (setf (bir:inputs ftm) (list out))
                t))
             ((eq dest :object)
              (assert (eq source :multiple-values))
              (let* ((fx (make-instance 'bir:output))
                     (mtf (make-instance 'bir:multiple-to-fixed
                            :outputs (list fx))))
                (bir:insert-instruction-after mtf instruction)
                (bir:replace-uses fx out)
                (setf (bir:inputs mtf) (list out))
                t))
             (t (error "BUG: impossible dest ~a for ~a" dest out)))))
    ((typep instruction 'bir:catch)
     (loop for eblock in (rest (bir:next instruction))
           for starti = (bir:start eblock)
           do (loop for phi in (bir:inputs eblock)
                    when (eq (cc-bmir:rtype phi) :object)
                      do (let* ((fx (make-instance 'bir:output))
                                (mtf (make-instance 'bir:multiple-to-fixed
                                       :outputs (list fx))))
                           (bir:insert-instruction-before mtf starti)
                           (bir:replace-uses fx phi)
                           (setf (bir:inputs mtf) (list phi))))))))

(defun insert-argument-ftm (argument starti)
  (when (eq (cc-bmir:rtype argument) :multiple-values)
    (let* ((mv (make-instance 'bir:output))
           (ftm (make-instance 'bir:fixed-to-multiple
                  :outputs (list mv))))
      (bir:insert-instruction-before ftm starti)
      (bir:replace-uses mv argument)
      (setf (bir:inputs ftm) (list argument)))))

(defun insert-value-coercion-into-function (function)
  ;; Insert after instructions
  (cleavir-bir:map-local-instructions #'insert-value-coercion function)
  ;; Insert FTM after any arguments that need it
  ;; (do this second so that we don't useless consider the mtf outputs)
  (let ((starti (bir:start (bir:start function))))
    (bir:map-lambda-list
     (lambda (state item index)
       (declare (ignore state index))
       (cond ((not (listp item))
              (insert-argument-ftm item starti))
             ((= (length item) 2)
              (insert-argument-ftm (first item) starti)
              (insert-argument-ftm (second item) starti))
             ((= (length item) 3)
              (insert-argument-ftm (second item) starti)
              (insert-argument-ftm (third item) starti))))
     (bir:lambda-list function))))

(defun insert-value-coercion-into-module (module)
  (bir:map-functions #'insert-value-coercion-into-function module))
