(defpackage "FINALIZE-TEST"
  (:use :cl :clasp-tests))

(defvar *a*)
(defparameter *count* 0)

;;; ----------------------------------------------------------------------
;;;
;;; Test finalizing CONS cells
;;;
(defun create-*a* (n)
  (setq *a* (make-list n)))
(create-*a* 5)

(gctools:finalize *a* #'(lambda (a)(declare (ignore a)) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a)(declare (ignore a)) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a)(declare (ignore a)) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a)(declare (ignore a)) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a)(declare (ignore a)) (setq *count* (+ 1 *count*))))
(setq *a* nil)
(setq *a* 99999)
(make-list 10000)
(dotimes (i 10)
  (gctools:garbage-collect))
(format t "finalizers-cons *count* -> ~d - it should be 5~%" *count*)
(test finalizers-cons (= *count* 5) :description "Check if list of cons finalizers were executed")

(setq *count* 0)
(create-*a* 5)
(gctools:finalize *a* #'(lambda (a)(declare (ignore a)) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a)(declare (ignore a)) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a)(declare (ignore a)) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a)(declare (ignore a)) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a)(declare (ignore a)) (setq *count* (+ 1 *count*))))
(gctools:definalize *a*)
(setq *a* nil)
(dotimes (i 10)
  (gctools:garbage-collect))
(test finalizers-cons-remove (= *count* 0) :description "Check if list of cons finalizers were discarded")

;;; ------------------------------------------------------------
;;;
;;; Test finalizing general objects
(defun bar (n)
  (setq *a* (core:bformat nil "Hi there %s" n)))
(bar 5)
(format t "*a* -> ~a~%" *a*)
(setq *count* 0)
(gctools:finalize *a* #'(lambda (a)(declare (ignore a)) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a)(declare (ignore a)) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a)(declare (ignore a)) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a)(declare (ignore a)) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a)(declare (ignore a)) (setq *count* (+ 1 *count*))))
(setq *a* nil)
(dotimes (i 10)
  (gctools:garbage-collect))
(format t "*count* --> ~a - it should be 5~%" *count*)
(test finalizers-general (= *count* 5) :description "Check if list of general finalizers were executed")

(setq *count* 0)
(bar 5)
(gctools:finalize *a* #'(lambda (a)(declare (ignore a)) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a)(declare (ignore a)) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a)(declare (ignore a)) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a)(declare (ignore a)) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a)(declare (ignore a)) (setq *count* (+ 1 *count*))))
(gctools:definalize *a*)
(setq *a* nil)
(dotimes (i 10)
  (gctools:garbage-collect))
(format t "*count* --> ~a - it should be 0~%" *count*)
(test finalizers-general-remove (= *count* 0) :description "Check if list of general finalizers were discarded")
