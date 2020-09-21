(in-package #:cc-bir-to-bmir)

(defun replace-typeq (typeq)
  (let ((ts (cleavir-bir:type-specifier typeq)))
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

(defun reduce-typeqs (ir)
  (cleavir-bir:map-instructions
   (lambda (i)
     (when (typep i 'cleavir-bir:typeq)
       (replace-typeq i)))
   ir))

(defun maybe-replace-primop (primop)
  (case (cleavir-bir:name (cleavir-bir::info primop))
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

(defun reduce-primops (ir)
  (cleavir-bir:map-instructions
   (lambda (i)
     (when (typep i 'cleavir-bir:primop)
       (maybe-replace-primop i)))
   ir))
