(defpackage "FINALIZE-TEST"
  (:use :cl :clasp-tests))

(defvar *a*)
(defparameter *count* 0)

;;; ----------------------------------------------------------------------
;;;
;;; This is a separate function in a perhaps-futile effort to prevent
;;; compiler optimizations from keeping the cons "reachable" when it's
;;; not in the source.
;;; Note that while we can use weak pointers to see if an object is accessible,
;;; boehm won't actually run finalizers until some point after weak pointers
;;; are splatted (according to gc.h) and other garbage collectors really
;;; don't make many guarantees at all about if or when finalizers run.
;;; So these tests is inherently dicey.

(defun finalized-object (maker nfinalizers)
  (let ((object (funcall maker))
        (count 0))
    (flet ((inc (a) (declare (ignore a)) (incf count)))
      (loop repeat nfinalizers do (gctools:finalize object #'inc))
      (values (ext:make-weak-pointer object) (lambda () count)))))
(declaim (notinline finalized-object))
(defun test-finalizers (maker nfinalizers)
  (multiple-value-bind (wp counter)
      (finalized-object (lambda () (make-list 5)) 5)
    ;; GC until the object becomes unreachable, and then
    ;; try to force finalizers to be invoked for good measure.
    ;; Max 10 iterations so we don't hang if something goes wrong.
    (loop repeat 10
          do (gctools:garbage-collect)
          while (ext:weak-pointer-valid wp))
    (gctools:invoke-finalizers)
    (values (funcall counter) (ext:weak-pointer-valid wp))))

(test finalizers-cons
      (test-finalizers (lambda () (make-list 5)) 5)
      (5 nil)
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

(test finalizers-general
      (test-finalizers (lambda () (make-array 5)) 5)
      (5 nil)
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
