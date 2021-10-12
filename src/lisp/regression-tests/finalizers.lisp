(defpackage "FINALIZE-TEST"
  (:use :cl :clasp-tests))

(defvar *a*)
(defparameter *count* 0)

;;; ----------------------------------------------------------------------
;;;
;;; Test finalizing CONS cells
;;;
;;; This is a separate function in a perhaps-futile effort to prevent
;;; compiler optimizations from keeping the cons "reachable" when it's
;;; not in the source.
;;; A better way to do this might involve weak pointers? FIXME
(defun finalized-cons (len ninc)
  (let ((a (make-list len))
        (count 0))
    (flet ((inc (a) (declare (ignore a)) (incf count)))
      (loop repeat ninc do (gctools:finalize a #'inc))
      (values a (lambda () count)))))
(declaim (notinline finalized-cons))

(test finalizers-cons
      (let ((count 0))
        (let ((s (make-list 5)))
          (flet ((inc (a) (declare (ignore a)) (incf count)))
            (loop repeat 5 do (gctools:finalize s #'inc))))
        ;; s is now unreachable
        (loop repeat 10 do (gctools:garbage-collect))
        count)
      (5)
      :description "Check if list of cons finalizers were executed")

(test finalizers-cons-remove
      (let ((count 0))
        (let ((s (make-list 5)))
          (flet ((inc (a) (declare (ignore a)) (incf count)))
            (loop repeat 5 do (gctools:finalize s #'inc)))
          (gctools:definalize s))
        (loop repeat 10 do (gctools:garbage-collect))
        count)
      (0)
      :description "Check if list of cons finalizers were discarded")

;;; ------------------------------------------------------------
;;;
;;; Test finalizing general objects
(test finalizers-general
      (let ((count 0))
        (let ((s (make-array 5)))
          (flet ((inc (a) (declare (ignore a)) (incf count)))
            (loop repeat 5 do (gctools:finalize s #'inc))))
        ;; S is now unreachable
        (loop repeat 10 do (gctools:garbage-collect))
        count)
      (5)
      :description "Check if list of general finalizers were executed")

(test finalizers-general-remove
      (let ((count 0))
        (let ((s (make-array 5)))
          (flet ((inc (a) (declare (ignore a)) (incf count)))
            (loop repeat 5 do (gctools:finalize s #'inc)))
          (gctools:definalize s))
        ;; S is now unreachable
        (loop repeat 10 do (gctools:garbage-collect))
        count)
      (0)
      :description "Check if list of general finalizers were discarded")
