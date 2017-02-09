(defun class-allocation-classes ()
  (let ((allocations (make-array (hash-table-count core::*class-name-hash-table*)))
        (idx 0))
    (maphash (lambda (k v)
               (setf (svref allocations idx) v)
               (incf idx))
             core::*class-name-hash-table*)
    allocations))

(defun class-allocation-counter-create (classes)
  (make-array (length classes) :initial-element 0))

(defun class-allocation-counter-fill (classes vec)
  (dotimes (idx (length classes))
    (setf (svref vec idx) (allocation-meter (svref classes idx)))))

(defun class-allocation-counter-zero (vec)
  (dotimes (idx (length vec))
    (setf (svref vec idx) 0)))

(defun class-allocation-counter-op (op x y)
  (let ((result (make-array (length x))))
    (dotimes (idx (length x))
      (setf (svref result idx) (funcall op (svref x idx) (svref y idx))))
    result))

(defun class-allocation-counter-accumulate (classes total y)
  (dotimes (idx (length classes))
    (setf (svref total idx) (+ (svref total idx) (- (allocation-meter (svref classes idx)) (svref y idx))))))

(defun class-allocation-counter-copy (orig)
  (let ((copy (make-array (length orig))))
    (setf (svref copy i) (svref orig i))
    copy))


(defun print-report (classes allocations)
  (let ((result (make-array (length classes))))
    (dotimes (idx (length classes))
      (setf (svref result idx) (cons (svref classes idx) (svref allocations idx))))
    (sort result #'< :key #'cdr)
    (format t "(quote (~%")
    (dotimes (idx (length result))
      (let ((clas (car (svref result idx)))
            (count (cdr (svref result idx))))
        (when (> count 0)
          (format t "( ~s ~s )~%" (class-name clas) count))))
    (format t "))~%")
    (finish-output)
    nil))



(defmacro count-class-allocations (form)
  (let ((class-vec (gensym))
        (vec-before (gensym))
	(vec-after (gensym))
        (result (gensym)))
    `(funcall #'(lambda ()
		  (let* ((,class-vec (class-allocation-classes))
                         (,vec-before (class-allocation-counter-create ,class-vec))
                         (,vec-after (class-allocation-counter-create ,class-vec)))
                    (class-allocation-counter-zero ,vec-after)
                    (class-allocation-counter-fill ,class-vec ,vec-before)
                    (let ((,result ,form))
                      (class-allocation-counter-accumulate ,class-vec ,vec-after ,vec-before)
                      (print-report ,class-vec ,vec-after)
                      nil))))))



#|
(defparameter *l* '(1 2 3 nil 4 5 6))
(defmethod foo ((x integer) y z) (+ x y z))
(let ((before (allocation-meter (find-class 'closure-with-slots))))
  (dotimes (i 1000) nil)
  (let ((after (allocation-meter (find-class 'closure-with-slots))))
    (list before after (- after before))))

|#
