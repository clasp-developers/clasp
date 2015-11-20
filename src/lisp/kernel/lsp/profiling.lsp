;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Do microprofiling to assess the GC speed under different stack conditions
;;
;; Use (micro-profile-ops) to generate timings for different low-level operations
;;
;; I added a special-operator to the compiler called cmp::gc-profiling
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (not (find-package "MICRO-PROFILING"))
      (make-package "MICRO-PROFILING" :use '("CORE" "CL"))))


(in-package "MICRO-PROFILING")

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

#+(or)(defun test-b ()
	(format t "test-b~%")
	(prof-apply *parts* #'ns-per-partial-apply #'b (list 1 2 3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Time a single operation.  op is an integer index that indicates
;; which operation is timed.  Check compiler.cc core::operations-per-second to
;; see which integer value corresponds to what operation.
;; 
(defun time-operation (op)
  (gctools:garbage-collect)
  (let ((op0 (* (/ 1.0 (core:operations-per-second 0)) 1.0d9)))
    (multiple-value-bind (time-or-nil op-name)
        (core:operations-per-second op)
      (if time-or-nil
          (let* ((total-op-time (* (/ 1.0 time-or-nil) 1.0d9))
                 (rel-op-time (- total-op-time op0)))
            (format nil "Operation ~2d  ~@40a --> ~6,1f ns  (uncorrected ~6,1f)" op op-name rel-op-time total-op-time ))
          nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Generate timings for all of the operations
;;
(defun micro-profile-ops ()
  (format t "Stack depth: ~a~%" (gctools:stack-depth))
  (do* ((i 1 (1+ i))
        (res (time-operation i) (time-operation i)))
       ((null res))
    (format t "~a~%" res)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fill the stack to a given recursive depth and then do the timings.
;; The MPS library slows down as the stack gets larger because there
;; are more pinned objects to deal with.  Boehm doesn't have this problem
;; because all objects are pinned all the time (non-moving GC).
;;
(defun deep-stack-micro-profile-ops (depth)
  (if (eql depth 0)
      (micro-profile-ops)
      (deep-stack-micro-profile-ops (1- depth))))

(export '(micro-profile-ops deep-stack-micro-profile-ops))

