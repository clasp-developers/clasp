(defpackage "FINALIZE-TEST"
  (:use :cl :clasp-tests))

(defvar *a*)

;;; ----------------------------------------------------------------------
;;;
;;; Test finalizing CONS cells
;;;
(defun foo (n) (defparameter *a* (make-list n)))
(foo 5)
(defparameter *count* 0)
(gctools:finalize *a* #'(lambda (a) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a) (setq *count* (+ 1 *count*))))
(setq *a* nil)
(gctools:garbage-collect)
(test finalizers-cons (= *count* 5) :description "Check if list of cons finalizers were executed")

(setq *count* 0)
(foo 5)
(gctools:finalize *a* #'(lambda (a) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a) (setq *count* (+ 1 *count*))))
(gctools:definalize *a*)
(setq *a* nil)
(gctools:garbage-collect)
(test finalizers-cons-remove (= *count* 0) :description "Check if list of cons finalizers were discarded")

;;; ------------------------------------------------------------
;;;
;;; Test finalizing general objects
(defun bar (n) (defparameter *a* (bformat nil "Hi there %s" n)))
(bar 5)
(format t "*a* -> ~a~%" *a*)
(defparameter *count* 0)
(gctools:finalize *a* #'(lambda (a) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a) (setq *count* (+ 1 *count*))))
(setq *a* nil)
(gctools:garbage-collect)
(format t "*count* --> ~a~%" *count*)
(test finalizers-general (= *count* 5) :description "Check if list of general finalizers were executed")

(setq *count* 0)
(bar 5)
(gctools:finalize *a* #'(lambda (a) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a) (setq *count* (+ 1 *count*))))
(gctools:finalize *a* #'(lambda (a) (setq *count* (+ 1 *count*))))
(gctools:definalize *a*)
(setq *a* nil)
(dotimes (i 100) (gctools:garbage-collect))
(test finalizers-general-remove (= *count* 0) :description "Check if list of general finalizers were discarded")
