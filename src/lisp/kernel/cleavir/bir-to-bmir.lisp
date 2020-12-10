(in-package #:cc-bir-to-bmir)

(defun replace-typeq (typeq)
  (let* ((ts (cleavir-bir:ctype typeq))
         (ifi (cleavir-bir:use typeq))
         (inputs (cleavir-bir:inputs typeq)))
    (setf (cleavir-bir:inputs ifi) '())
    (cleavir-bir:delete-computation typeq)
    (setf (cleavir-bir:inputs ifi) inputs)
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
       (setf ts 'simple-string)))
    (case ts
      ((fixnum) (change-class ifi 'cc-bmir:fixnump))
      ((cons) (change-class ifi 'cc-bmir:consp))
      ((character) (change-class ifi 'cc-bmir:characterp))
      ((single-float) (change-class ifi 'cc-bmir:single-float-p))
      ((core:general) (change-class ifi 'cc-bmir:generalp))
      (t (let ((header-info (gethash ts core:+type-header-value-map+)))
           (cond (header-info
                  (check-type header-info (or integer cons)) ; sanity check
                  (change-class ifi 'cc-bmir:headerq :info header-info))
                 (t (error "BUG: Typeq for unknown type: ~a" ts))))))))

(defun reduce-local-typeqs (function)
  (cleavir-bir:map-iblocks
   (lambda (ib)
     (let ((term (cleavir-bir:end ib)))
       (when (typep term 'cleavir-bir:ifi)
         (let ((test (first (cleavir-bir:inputs term))))
           (when (typep test 'cleavir-bir:typeq-test)
             (replace-typeq test))))))
   function))

(defun reduce-module-typeqs (module)
  (cleavir-set:mapset nil
                      #'reduce-local-typeqs
                      (cleavir-bir:functions module)))

(defun maybe-replace-primop (primop)
  (case (cleavir-primop-info:name (cleavir-bir:info primop))
    ((cleavir-primop:car)
     (let ((in (cleavir-bir:inputs primop)))
       (change-class primop 'cc-bmir:load :inputs ())
       (let ((mr (make-instance 'cc-bmir:memref2
                   :inputs in
                   :offset (- cmp:+cons-car-offset+ cmp:+cons-tag+))))
         (cleavir-bir:insert-instruction-before mr primop)
         (setf (cleavir-bir:inputs primop) (list mr)))))
    ((cleavir-primop:cdr)
     (let ((in (cleavir-bir:inputs primop)))
       (change-class primop 'cc-bmir:load :inputs ())
       (let ((mr (make-instance 'cc-bmir:memref2
                   :inputs in
                   :offset (- cmp:+cons-cdr-offset+ cmp:+cons-tag+))))
         (cleavir-bir:insert-instruction-before mr primop)
         (setf (cleavir-bir:inputs primop) (list mr)))))
    ((cleavir-primop:rplaca)
     (let ((in (cleavir-bir:inputs primop)))
       (change-class primop 'cc-bmir:store :inputs ())
       (let ((mr (make-instance 'cc-bmir:memref2
                   :inputs (list (first in))
                   :offset (- cmp:+cons-car-offset+ cmp:+cons-tag+))))
         (cleavir-bir:insert-instruction-before mr primop)
         (setf (cleavir-bir:inputs primop) (list (second in) mr)))))
    ((cleavir-primop:rplacd)
     (let ((in (cleavir-bir:inputs primop)))
       (change-class primop 'cc-bmir:store :inputs ())
       (let ((mr (make-instance 'cc-bmir:memref2
                   :inputs (list (first in))
                   :offset (- cmp:+cons-cdr-offset+ cmp:+cons-tag+))))
         (cleavir-bir:insert-instruction-before mr primop)
         (setf (cleavir-bir:inputs primop) (list (second in) mr)))))))

(defun reduce-primops (function)
  (cleavir-bir:map-local-instructions
   (lambda (i)
     (when (typep i 'cleavir-bir:primop)
       (maybe-replace-primop i)))
   function))

(defun reduce-module-primops (module)
  (cleavir-set:mapset nil #'reduce-primops (cleavir-bir:functions module)))
