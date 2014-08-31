
(defun b (x y z) )
(defun a () )

(defun ns-per-partial-apply (stage fn args)
  (let ((rate (core:partial-applys-per-second stage fn args)))
    (* (/ 1.0 rate) 1.0d9)))

(defparameter *parts* '(
                        "C++ call-by-val"
                        "fn lookup"
                        "length of args"
                        "alloc frame"
                        "fill frame"
                        "apply"))

(defun prof-apply (parts analyzer fn args)
  (gctools:garbage-collect)
  (let (rev-times)
    (dotimes (i (length parts))
      (push (funcall analyzer i fn args) rev-times))
    (let ((times (reverse rev-times)))
      (do* ((stage 0 (1+ stage))
            (prev-time nil (car time-cur))
            (time-cur times (cdr time-cur))
            (part-cur parts (cdr part-cur)))
           ((null time-cur))
        (format t "stage~d (~20a) ~6,1f ns" stage (car part-cur) (car time-cur))
        (when prev-time
          (format t "  delta ~6,1f ns" (- (car time-cur) prev-time)))
        (terpri)
        ))))

(defun test-b ()
  (format t "test-b~%")
  (prof-apply *parts* #'ns-per-partial-apply #'b (list 1 2 3)))



(defun time-operation (op)
  (gctools:garbage-collect)
  (let ((op0 (* (/ 1.0 (core:operations-per-second 0)) 1.0d9)))
    (multiple-value-bind (time-or-nil op-name)
        (core:operations-per-second op)
      (if time-or-nil
          (format nil "Operation ~d  ~@40a --> ~10,1f ns" op op-name (- (* (/ 1.0 time-or-nil) 1.0d9) op0))
          nil))))


(defun time-ops ()
  (do* ((i 1 (1+ i))
        (res (time-operation i) (time-operation i)))
       ((null res))
    (format t "~a~%" res)))



